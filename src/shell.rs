// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::process::Stdio;

use anyhow::bail;
use anyhow::Result;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

use crate::commands::cd_command;
use crate::commands::cp_command;
use crate::commands::exit_command;
use crate::commands::mkdir_command;
use crate::commands::mv_command;
use crate::commands::rm_command;
use crate::commands::sleep_command;
use crate::environment::Environment;
use crate::environment::RealEnvironment;
use crate::parser::Command;
use crate::parser::Sequence;
use crate::parser::SequentialList;
use crate::parser::StringOrWord;
use crate::parser::StringPart;
use crate::shell_types::EnvChange;
use crate::shell_types::ExecuteResult;
use crate::shell_types::ShellPipe;
use crate::shell_types::ShellPipeSender;
use crate::shell_types::ShellState;
use crate::shell_types::SpawnedStep;

pub async fn execute(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: &Path,
) -> i32 {
  let environment = RealEnvironment::default();
  execute_with_environment(list, env_vars, cwd, environment).await
}

pub(crate) async fn execute_with_environment(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: &Path,
  environment: impl Environment,
) -> i32 {
  assert!(cwd.is_absolute());
  let (env_stdout_tx, env_stdout) = ShellPipe::channel();
  let state = ShellState::new(env_vars, cwd, env_stdout_tx.clone());

  // spawn a task to pipe env_stdout to the actual environment's stdout
  let env_stdout_output_handle = tokio::task::spawn({
    let environment = environment.clone();
    async move {
      env_stdout.pipe_to_writer(environment.async_stdout()).await;
    }
  });

  // spawn a sequential list and pipe its output to the environment
  let spawned_step = spawn_sequential_list(
    list,
    state,
    ShellPipe::InheritStdin,
    environment.clone(),
    AsyncCommandBehavior::Wait,
  );
  let result = spawned_step
    .wait_with_stdout(move |stdout| async move {
      stdout.pipe_to_sender(env_stdout_tx).await;
    })
    .await;

  // wait on the environment's stdout handle to finish draining
  env_stdout_output_handle.await.unwrap();

  match result {
    ExecuteResult::Exit(code, _) => code,
    ExecuteResult::Continue(exit_code, _, _) => exit_code,
  }
}

#[derive(Debug, PartialEq)]
enum AsyncCommandBehavior {
  Wait,
  Yield,
}

fn spawn_sequential_list(
  list: SequentialList,
  mut state: ShellState,
  mut stdin: ShellPipe,
  environment: impl Environment,
  async_command_behavior: AsyncCommandBehavior,
) -> SpawnedStep {
  let (stdout_tx, stdout) = ShellPipe::channel();
  SpawnedStep {
    stdout,
    task: async move {
      let mut final_exit_code = 0;
      let mut final_changes = Vec::new();
      let mut async_handles = Vec::new();
      for item in list.items {
        if item.is_async {
          let state = state.clone();
          let environment = environment.clone();
          async_handles.push(tokio::task::spawn(async move {
            // use the current shell's stdout_tx in order to avoid
            // blocking async out
            let shell_stdout_tx = state.shell_stdout_tx();
            let spawned_step = spawn_sequence(
              item.sequence,
              state,
              // todo(#2): not correct... should use provided stdin
              ShellPipe::InheritStdin,
              environment,
            )
            .await;
            let result = spawned_step
              .wait_with_stdout(move |stdout| async move {
                stdout.pipe_to_sender(shell_stdout_tx).await;
              })
              .await;
            futures::future::join_all(result.into_handles()).await;
          }));
        } else {
          // todo(#2): not correct... should share the provided stdin
          let stdin = std::mem::replace(&mut stdin, ShellPipe::InheritStdin);
          let command = spawn_sequence(
            item.sequence,
            state.clone(),
            stdin,
            environment.clone(),
          )
          .await;
          let stdout_tx = stdout_tx.clone();
          let result = command
            .wait_with_stdout(move |stdout| async {
              stdout.pipe_to_sender(stdout_tx).await;
            })
            .await;
          match result {
            ExecuteResult::Exit(_, _) => return result,
            ExecuteResult::Continue(exit_code, changes, handles) => {
              state.apply_changes(&changes);
              final_changes.extend(changes);
              async_handles.extend(handles);
              // use the final sequential item's exit code
              final_exit_code = exit_code;
            }
          }
        }
      }

      // wait for async commands to complete
      if async_command_behavior == AsyncCommandBehavior::Wait {
        futures::future::join_all(async_handles.drain(..)).await;
      }

      ExecuteResult::Continue(final_exit_code, final_changes, async_handles)
    }
    .boxed(),
  }
}

fn spawn_sequence(
  sequence: Sequence,
  mut state: ShellState,
  stdin: ShellPipe,
  environment: impl Environment,
) -> BoxFuture<'static, SpawnedStep> {
  // requires boxed async because of recursive async
  async move {
    match sequence {
      Sequence::EnvVar(var) => {
        SpawnedStep::from_result(ExecuteResult::Continue(
          0,
          vec![EnvChange::SetEnvVar(
            var.name,
            evaluate_string_or_word(var.value, &state, environment).await,
          )],
          Vec::new(),
        ))
      }
      Sequence::ShellVar(var) => {
        SpawnedStep::from_result(ExecuteResult::Continue(
          0,
          vec![EnvChange::SetShellVar(
            var.name,
            evaluate_string_or_word(var.value, &state, environment).await,
          )],
          Vec::new(),
        ))
      }
      Sequence::Command(command) => {
        start_command(command, &state, stdin, environment).await
      }
      Sequence::BooleanList(list) => {
        let (stdout_tx, stdout) = ShellPipe::channel();
        SpawnedStep {
          stdout,
          task: async move {
            let mut changes = vec![];
            let first_result = spawn_and_wait_sequence(
              list.current,
              state.clone(),
              stdin,
              stdout_tx.clone(),
              environment.clone(),
            )
            .await;
            let (exit_code, mut async_handles) = match first_result {
              ExecuteResult::Exit(_, _) => return first_result,
              ExecuteResult::Continue(
                exit_code,
                sub_changes,
                async_handles,
              ) => {
                state.apply_changes(&sub_changes);
                changes.extend(sub_changes);
                (exit_code, async_handles)
              }
            };

            let next = if list.op.moves_next_for_exit_code(exit_code) {
              Some(list.next)
            } else {
              let mut next = list.next;
              loop {
                // boolean lists always move right on the tree
                match next {
                  Sequence::BooleanList(list) => {
                    if list.op.moves_next_for_exit_code(exit_code) {
                      break Some(list.next);
                    }
                    next = list.next;
                  }
                  _ => break None,
                }
              }
            };
            if let Some(next) = next {
              let next_result = spawn_and_wait_sequence(
                next,
                state.clone(),
                // todo(#2): share stdin
                ShellPipe::InheritStdin,
                stdout_tx.clone(),
                environment,
              )
              .await;
              match next_result {
                ExecuteResult::Exit(code, sub_handles) => {
                  async_handles.extend(sub_handles);
                  ExecuteResult::Exit(code, async_handles)
                }
                ExecuteResult::Continue(
                  exit_code,
                  sub_changes,
                  sub_handles,
                ) => {
                  changes.extend(sub_changes);
                  async_handles.extend(sub_handles);
                  ExecuteResult::Continue(exit_code, changes, async_handles)
                }
              }
            } else {
              ExecuteResult::Continue(exit_code, changes, async_handles)
            }
          }
          .boxed(),
        }
      }
      Sequence::Pipeline(pipeline) => {
        let (stdout_tx, stdout) = ShellPipe::channel();
        SpawnedStep {
          stdout,
          task: async move {
            let sequences = pipeline.into_vec();
            let mut wait_tasks = vec![];
            let mut last_input = Some(stdin);
            for sequence in sequences.into_iter() {
              let spawned_sequence = spawn_sequence(
                sequence,
                state.clone(),
                last_input.take().unwrap(),
                environment.clone(),
              )
              .await;
              last_input = Some(spawned_sequence.stdout);
              wait_tasks.push(spawned_sequence.task);
            }
            let output_handle = tokio::task::spawn({
              async move {
                last_input.unwrap().pipe_to_sender(stdout_tx).await;
              }
            });
            let mut results = futures::future::join_all(wait_tasks).await;
            output_handle.await.unwrap();
            let last_result = results.pop().unwrap();
            let all_handles =
              results.into_iter().flat_map(|r| r.into_handles());
            match last_result {
              ExecuteResult::Exit(code, mut handles) => {
                handles.extend(all_handles);
                ExecuteResult::Continue(code, Vec::new(), handles)
              }
              ExecuteResult::Continue(code, _, mut handles) => {
                handles.extend(all_handles);
                ExecuteResult::Continue(code, Vec::new(), handles)
              }
            }
          }
          .boxed(),
        }
      }
      Sequence::Subshell(list) => {
        let (stdout_tx, stdout) = ShellPipe::channel();
        SpawnedStep {
          stdout,
          task: async move {
            let step = spawn_sequential_list(
              *list,
              state.clone(),
              stdin,
              environment,
              // yield async commands to the parent
              AsyncCommandBehavior::Yield,
            );
            let result = step
              .wait_with_stdout(move |stdout| async move {
                stdout.pipe_to_sender(stdout_tx).await;
              })
              .await;

            // sub shells do not cause an exit
            match result {
              ExecuteResult::Exit(code, handles) => {
                ExecuteResult::Continue(code, Vec::new(), handles)
              }
              ExecuteResult::Continue(_, _, _) => result,
            }
          }
          .boxed(),
        }
      }
    }
  }
  .boxed()
}

async fn spawn_and_wait_sequence(
  sequence: Sequence,
  state: ShellState,
  stdin: ShellPipe,
  sender: ShellPipeSender,
  environment: impl Environment,
) -> ExecuteResult {
  let step = spawn_sequence(sequence, state, stdin, environment).await;
  step
    .wait_with_stdout(move |stdout| async {
      stdout.pipe_to_sender(sender).await;
    })
    .await
}

async fn start_command(
  command: Command,
  state: &ShellState,
  stdin: ShellPipe,
  environment: impl Environment,
) -> SpawnedStep {
  let mut args = evaluate_args(command.args, state, environment.clone()).await;
  let command_name = if args.is_empty() {
    String::new()
  } else {
    args.remove(0)
  };
  if command_name == "cd" {
    let cwd = state.cwd().clone();
    SpawnedStep::from_fn(move || cd_command(&cwd, args, environment))
  } else if command_name == "exit" {
    SpawnedStep::from_future(async move { exit_command(args, environment) })
  } else if command_name == "pwd" {
    // ignores additional arguments
    SpawnedStep::with_stdout_text(format!("{}\n", state.cwd().display()))
  } else if command_name == "echo" {
    SpawnedStep::with_stdout_text(format!("{}\n", args.join(" ")))
  } else if command_name == "true" {
    // ignores additional arguments
    SpawnedStep::from_exit_code(0)
  } else if command_name == "false" {
    // ignores additional arguments
    SpawnedStep::from_exit_code(1)
  } else if command_name == "cp" {
    let cwd = state.cwd().clone();
    SpawnedStep::from_future(async move {
      cp_command(&cwd, args, environment).await
    })
  } else if command_name == "mkdir" {
    let cwd = state.cwd().clone();
    SpawnedStep::from_future(async move {
      mkdir_command(&cwd, args, environment).await
    })
  } else if command_name == "mv" {
    let cwd = state.cwd().clone();
    SpawnedStep::from_future(async move {
      mv_command(&cwd, args, environment).await
    })
  } else if command_name == "rm" {
    let cwd = state.cwd().clone();
    SpawnedStep::from_future(async move {
      rm_command(&cwd, args, environment).await
    })
  } else if command_name == "sleep" {
    SpawnedStep::from_future(
      async move { sleep_command(args, environment).await },
    )
  } else {
    let mut state = state.clone();
    for env_var in command.env_vars {
      state.apply_env_var(
        &env_var.name,
        &evaluate_string_or_word(env_var.value, &state, environment.clone())
          .await,
      );
    }

    let command_path = match resolve_command_path(&command_name, &state).await {
      Ok(command_path) => command_path,
      Err(err) => {
        environment.eprintln(&err.to_string());
        return SpawnedStep::from_result(ExecuteResult::Continue(
          1,
          Vec::new(),
          Vec::new(),
        ));
      }
    };
    let mut sub_command = tokio::process::Command::new(&command_path);
    let child = sub_command
      .current_dir(state.cwd())
      .args(&args)
      .env_clear()
      .envs(state.env_vars())
      .stdout(Stdio::piped())
      .stdin(match &stdin {
        ShellPipe::InheritStdin => Stdio::inherit(),
        ShellPipe::Channel(_) => Stdio::piped(),
      })
      .stderr(Stdio::inherit())
      .spawn();

    let mut child = match child {
      Ok(child) => child,
      Err(err) => {
        environment
          .eprintln(&format!("Error launching '{}': {}", command_name, err));
        return SpawnedStep::from_result(ExecuteResult::Continue(
          1,
          Vec::new(),
          Vec::new(),
        ));
      }
    };

    if let ShellPipe::Channel(mut channel) = stdin {
      // spawn a task to pipe the messages from the provided
      // channel to this process' stdin
      let mut child_stdin = child.stdin.take().unwrap();
      tokio::task::spawn(async move {
        while let Some(message) = channel.recv().await {
          if child_stdin.write_all(&message).await.is_err() {
            return;
          }
        }
      });
    }

    let mut child_stdout = child.stdout.take().unwrap();
    let (stdout_tx, stdout) = ShellPipe::channel();

    SpawnedStep {
      stdout,
      task: async move {
        // spawn a task to pipe the messages from the process' stdout to the channel
        tokio::task::spawn(async move {
          let mut buffer = [0; 512]; // todo: what is an appropriate buffer size?
          while let Ok(size) = child_stdout.read(&mut buffer).await {
            // It seems checking for size of `0` is the recommended
            // way to detect when to close stdout. I tried to do a signal
            // after the `child.wait().await` call below up to here, but
            // what happens is a spin loop for 1000+ iterations between
            // when the process exists and when the signal occurs
            if size == 0 {
              break;
            }
            if stdout_tx.send(buffer[..size].to_vec()).is_err() {
              break;
            }
          }
          drop(stdout_tx); // close stdout
        });

        match child.wait().await {
          Ok(status) => ExecuteResult::Continue(
            status.code().unwrap_or(1),
            Vec::new(),
            Vec::new(),
          ),
          Err(err) => {
            environment.eprintln(&format!("{}", err));
            ExecuteResult::Continue(1, Vec::new(), Vec::new())
          }
        }
      }
      .boxed(),
    }
  }
}

async fn resolve_command_path(
  command_name: &str,
  state: &ShellState,
) -> Result<PathBuf> {
  if command_name.is_empty() {
    bail!("command name was empty");
  }

  // check for absolute
  if PathBuf::from(command_name).is_absolute() {
    return Ok(PathBuf::from(command_name));
  }

  // then relative
  if command_name.contains('/')
    || (cfg!(windows) && command_name.contains('\\'))
  {
    return Ok(state.cwd().join(&command_name));
  }

  // now search based on the current environment state
  let mut search_dirs = vec![state.cwd().clone()];
  if let Some(path) = state.get_var("PATH") {
    for folder in path.split(if cfg!(windows) { ';' } else { ':' }) {
      search_dirs.push(PathBuf::from(folder));
    }
  }
  let path_exts = if cfg!(windows) {
    let path_ext = state
      .get_var("PATHEXT")
      .map(|s| s.as_str())
      .unwrap_or(".EXE;.CMD;.BAT;.COM");
    let command_exts = path_ext
      .split(';')
      .map(|s| s.to_string().to_uppercase())
      .collect::<Vec<_>>();
    if command_exts.iter().any(|ext| command_name.ends_with(ext)) {
      None // use the command name as-is
    } else {
      Some(command_exts)
    }
  } else {
    None
  };

  for search_dir in search_dirs {
    let paths = if let Some(path_exts) = &path_exts {
      let mut paths = Vec::new();
      for path_ext in path_exts {
        paths.push(search_dir.join(format!("{}{}", command_name, path_ext)))
      }
      paths
    } else {
      vec![search_dir.join(command_name)]
    };
    for path in paths {
      if let Ok(metadata) = tokio::fs::metadata(&path).await {
        if metadata.is_file() {
          return Ok(path);
        }
      }
    }
  }

  bail!("{}: command not found", command_name)
}

async fn evaluate_args(
  args: Vec<StringOrWord>,
  state: &ShellState,
  environment: impl Environment,
) -> Vec<String> {
  let mut result = Vec::new();
  for arg in args {
    match arg {
      StringOrWord::Word(parts) => {
        // todo(dsherret): maybe we should have this work like sh and I believe
        // reparse then continually re-evaluate until there's only strings left.
        let text =
          evaluate_string_parts(parts, state, environment.clone()).await;
        for part in text.split(' ') {
          let part = part.trim();
          if !part.is_empty() {
            result.push(part.to_string());
          }
        }
      }
      StringOrWord::String(parts) => {
        result
          .push(evaluate_string_parts(parts, state, environment.clone()).await);
      }
    }
  }
  result
}

async fn evaluate_string_or_word(
  string_or_word: StringOrWord,
  state: &ShellState,
  environment: impl Environment,
) -> String {
  evaluate_string_parts(string_or_word.into_parts(), state, environment).await
}

async fn evaluate_string_parts(
  parts: Vec<StringPart>,
  state: &ShellState,
  environment: impl Environment,
) -> String {
  let mut final_text = String::new();
  for part in parts {
    match part {
      StringPart::Text(text) => final_text.push_str(&text),
      StringPart::Variable(name) => {
        if let Some(value) = state.get_var(&name) {
          final_text.push_str(value);
        }
      }
      StringPart::Command(list) => final_text.push_str(
        &evaluate_command_substitution(list, state, environment.clone()).await,
      ),
    }
  }
  final_text
}

async fn evaluate_command_substitution(
  list: SequentialList,
  state: &ShellState,
  environment: impl Environment,
) -> String {
  let (shell_tx, shell_stdout) = ShellPipe::channel();
  let spawned_step = spawn_sequential_list(
    list,
    {
      // command substitution needs to capture even the async
      // output, so set the current state's shell_stdout_tx
      // in order to capture it
      let mut state = state.clone();
      state.set_shell_stdout_tx(shell_tx);
      state
    },
    // todo(#2): this is not correct. It should use the current stdin.
    ShellPipe::InheritStdin,
    environment,
    AsyncCommandBehavior::Wait,
  );
  let output_handle = tokio::task::spawn(async {
    let mut final_data = Vec::new();
    let mut shell_rx = shell_stdout.unwrap_channel();
    let mut spawned_step_rx = spawned_step.stdout.unwrap_channel();

    loop {
      tokio::select! {
        Some(data) = shell_rx.recv() => {
          final_data.extend(data);
        }
        Some(data) = spawned_step_rx.recv() => {
          final_data.extend(data);
        }
        else => {
          break;
        }
      }
    }

    final_data
  });
  let _ = spawned_step.task.await;
  let data = output_handle.await.unwrap();
  let text = String::from_utf8_lossy(&data);

  // Remove the trailing newline and then replace inner newlines with a space
  // This seems to be what sh does, but I'm not entirely sure:
  //
  // > echo $(echo 1 && echo -e "\n2\n")
  // 1 2
  text
    .strip_suffix("\r\n")
    .or_else(|| text.strip_suffix('\n'))
    .unwrap_or(&text)
    .replace("\r\n", " ")
    .replace('\n', " ")
}
