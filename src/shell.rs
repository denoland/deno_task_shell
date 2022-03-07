// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::Duration;

use anyhow::bail;
use anyhow::Result;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;

use crate::fs_util;
use crate::parser::Command;
use crate::parser::Sequence;
use crate::parser::SequentialList;
use crate::parser::StringOrWord;
use crate::parser::StringPart;
use crate::shell_types::EnvChange;
use crate::shell_types::EnvState;
use crate::shell_types::ExecuteResult;
use crate::shell_types::ExecutedTask;
use crate::shell_types::ShellPipe;
use crate::shell_types::ShellPipeSender;

pub async fn execute(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: PathBuf,
  additional_cli_args: Vec<String>,
) -> Result<i32> {
  assert!(cwd.is_absolute());
  let list = append_cli_args(list, additional_cli_args)?;
  let state = EnvState::new(env_vars, cwd);
  let executed = execute_sequential_list(list, state, ShellPipe::InheritStdin);
  // todo: something better at outputting to stdout?
  let output_task = tokio::task::spawn(async move {
    executed.stdout.pipe_to_stdout().await;
  });
  let result = executed.task.await;
  output_task.await.unwrap();

  Ok(match result {
    ExecuteResult::Exit => 1,
    ExecuteResult::Continue(exit_code, _) => exit_code,
  })
}

/// When a user calls `deno task <task-name> -- <args>`, we want
/// to append those CLI arguments to the last command.
fn append_cli_args(
  mut list: SequentialList,
  args: Vec<String>,
) -> Result<SequentialList> {
  if args.is_empty() {
    return Ok(list);
  }

  // todo(THIS PR): this part and remove this clippy
  #[allow(clippy::redundant_pattern_matching)]
  if let Some(_) = list.items.last_mut() {
    todo!();
  }

  Ok(list)
}

fn execute_sequential_list(
  list: SequentialList,
  mut state: EnvState,
  mut stdin: ShellPipe,
) -> ExecutedTask {
  let (stdout_tx, stdout) = ShellPipe::channel();
  ExecutedTask {
    stdout,
    task: async move {
      let mut final_exit_code = 0;
      let mut final_changes = Vec::new();
      let mut async_handles = Vec::new();
      for item in list.items {
        if item.is_async {
          let state = state.clone();
          let stdout_tx = stdout_tx.clone();
          async_handles.push(tokio::task::spawn(async move {
            let command = execute_sequence(
              item.sequence,
              state,
              // todo: not correct... should use provided stdin
              ShellPipe::InheritStdin,
            )
            .await;
            // todo: better code
            let output_task = tokio::task::spawn(async move {
              command.stdout.pipe_to_sender(stdout_tx).await;
            });
            let result = command.task.await;
            output_task.await.unwrap();
            result
          }));
        } else {
          // todo: this doesn't seem correct. I believe all these commands
          // should use the same stdin and not inherit from the process after
          // the first item in the sequential list.
          let stdin = std::mem::replace(&mut stdin, ShellPipe::InheritStdin);
          let command =
            execute_sequence(item.sequence, state.clone(), stdin).await;
          // todo: something better?
          let output_task = tokio::task::spawn({
            let stdout_tx = stdout_tx.clone();
            async move {
              command.stdout.pipe_to_sender(stdout_tx).await;
            }
          });
          let result = command.task.await;
          output_task.await.unwrap();
          match result {
            ExecuteResult::Exit => return ExecuteResult::Exit,
            ExecuteResult::Continue(exit_code, changes) => {
              state.apply_changes(&changes);
              final_changes.extend(changes);
              // use the final sequential item's exit code
              final_exit_code = exit_code;
            }
          }
        }
      }

      // wait for async commands to complete
      futures::future::join_all(async_handles).await;

      ExecuteResult::Continue(final_exit_code, final_changes)
    }
    .boxed(),
  }
}

// todo(THIS PR): clean up this function
fn execute_sequence(
  sequence: Sequence,
  mut state: EnvState,
  stdin: ShellPipe,
) -> BoxFuture<'static, ExecutedTask> {
  // requires boxed async because of recursive async
  async move {
    match sequence {
      Sequence::EnvVar(var) => {
        ExecutedTask::from_result(ExecuteResult::Continue(
          0,
          vec![EnvChange::SetEnvVar(
            var.name,
            evaluate_string_or_word(var.value, &state).await,
          )],
        ))
      }
      Sequence::ShellVar(var) => {
        ExecutedTask::from_result(ExecuteResult::Continue(
          0,
          vec![EnvChange::SetShellVar(
            var.name,
            evaluate_string_or_word(var.value, &state).await,
          )],
        ))
      }
      Sequence::Command(command) => start_command(command, &state, stdin).await,
      Sequence::BooleanList(list) => {
        let (stdout_tx, stdout) = ShellPipe::channel();
        ExecutedTask {
          stdout,
          task: async move {
            // todo(THIS PR): clean this up
            let mut changes = vec![];
            let first_result = execute_and_wait_sequence(
              list.current,
              state.clone(),
              stdin,
              stdout_tx.clone(),
            )
            .await;
            let exit_code = match first_result {
              ExecuteResult::Exit => return ExecuteResult::Exit,
              ExecuteResult::Continue(exit_code, sub_changes) => {
                state.apply_changes(&sub_changes);
                changes.extend(sub_changes);
                exit_code
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
              let next_result = execute_and_wait_sequence(
                next,
                state.clone(),
                // seems suspect, but good enough for now
                ShellPipe::InheritStdin,
                stdout_tx.clone(),
              )
              .await;
              match next_result {
                ExecuteResult::Exit => ExecuteResult::Exit,
                ExecuteResult::Continue(exit_code, sub_changes) => {
                  changes.extend(sub_changes);
                  ExecuteResult::Continue(exit_code, changes)
                }
              }
            } else {
              ExecuteResult::Continue(exit_code, changes)
            }
          }
          .boxed(),
        }
      }
      Sequence::Pipeline(pipeline) => {
        let (stdout_tx, stdout) = ShellPipe::channel();
        ExecutedTask {
          stdout,
          task: async move {
            let sequences = pipeline.into_vec();
            let mut wait_tasks = vec![];
            let mut last_input = Some(stdin);
            for sequence in sequences.into_iter() {
              let executed_sequence = execute_sequence(
                sequence,
                state.clone(),
                last_input.take().unwrap(),
              )
              .await;
              last_input = Some(executed_sequence.stdout);
              wait_tasks.push(executed_sequence.task);
            }
            // todo: something better
            let output_task = tokio::task::spawn({
              async move {
                last_input.unwrap().pipe_to_sender(stdout_tx).await;
              }
            });
            let mut results = futures::future::join_all(wait_tasks).await;
            output_task.await.unwrap();
            let last_result = results.pop().unwrap();
            match last_result {
              ExecuteResult::Exit => ExecuteResult::Continue(1, Vec::new()),
              ExecuteResult::Continue(exit_code, _) => {
                ExecuteResult::Continue(exit_code, Vec::new())
              }
            }
          }
          .boxed(),
        }
      }
    }
  }
  .boxed()
}

async fn execute_and_wait_sequence(
  sequence: Sequence,
  state: EnvState,
  stdin: ShellPipe,
  sender: ShellPipeSender,
) -> ExecuteResult {
  let command = execute_sequence(sequence, state, stdin).await;
  // todo: something better
  let output_task = tokio::task::spawn({
    async move {
      command.stdout.pipe_to_sender(sender).await;
    }
  });
  let result = command.task.await;
  output_task.await.unwrap();
  result
}

async fn start_command(
  command: Command,
  state: &EnvState,
  stdin: ShellPipe,
) -> ExecutedTask {
  let mut args = evaluate_args(command.args, state).await;
  let command_name = if args.is_empty() {
    String::new()
  } else {
    args.remove(0)
  };
  if command_name == "cd" {
    let cwd = state.cwd().clone();
    let (tx, stdout) = ShellPipe::channel();
    ExecutedTask {
      stdout,
      task: async move {
        drop(tx); // close stdout
        if args.len() != 1 {
          eprintln!("cd is expected to have 1 argument.");
          ExecuteResult::Continue(1, Vec::new())
        } else {
          // affects the parent state
          let new_dir = cwd.join(&args[0]);
          match fs_util::canonicalize_path(&new_dir) {
            Ok(new_dir) => {
              ExecuteResult::Continue(0, vec![EnvChange::Cd(new_dir)])
            }
            Err(err) => {
              eprintln!("Could not cd to {}.\n\n{}", new_dir.display(), err);
              ExecuteResult::Continue(1, Vec::new())
            }
          }
        }
      }
      .boxed(),
    }
  } else if command_name == "exit" {
    let (tx, stdout) = ShellPipe::channel();
    ExecutedTask {
      stdout,
      task: async move {
        drop(tx); // close stdout
        if !args.is_empty() {
          eprintln!("exit had too many arguments.");
          ExecuteResult::Continue(1, Vec::new())
        } else {
          ExecuteResult::Exit
        }
      }
      .boxed(),
    }
  } else if command_name == "pwd" {
    // ignores additional arguments
    ExecutedTask::with_stdout_text(format!("{}\n", state.cwd().display()))
  } else if command_name == "echo" {
    ExecutedTask::with_stdout_text(format!("{}\n", args.join(" ")))
  } else if command_name == "true" {
    // ignores additional arguments
    ExecutedTask::from_exit_code(0)
  } else if command_name == "false" {
    // ignores additional arguments
    ExecutedTask::from_exit_code(1)
  } else if command_name == "sleep" {
    let (tx, stdout) = ShellPipe::channel();
    ExecutedTask {
      stdout,
      task: async move {
        // the time to sleep is the sum of all the arguments
        let mut total_time_ms = 0;
        for arg in args.iter() {
          match arg.parse::<f64>() {
            Ok(value_s) => {
              let ms = (value_s * 1000f64) as u64;
              total_time_ms += ms;
            }
            Err(err) => {
              eprintln!("Error parsing sleep argument to number: {}", err);
              return ExecuteResult::Continue(1, Vec::new());
            }
          }
        }
        tokio::time::sleep(Duration::from_millis(total_time_ms)).await;
        drop(tx); // close stdout
        ExecuteResult::Continue(0, Vec::new())
      }
      .boxed(),
    }
  } else {
    let mut state = state.clone();
    for env_var in command.env_vars {
      state.apply_env_var(
        &env_var.name,
        &evaluate_string_or_word(env_var.value, &state).await,
      );
    }

    let command_path = match resolve_command_path(&command_name, &state).await {
      Ok(command_path) => command_path,
      Err(err) => {
        eprintln!("Error launching '{}': {}", command_name, err);
        return ExecutedTask::from_result(ExecuteResult::Continue(
          1,
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
        eprintln!("Error launching '{}': {}", command_name, err);
        return ExecutedTask::from_result(ExecuteResult::Continue(
          1,
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

    ExecutedTask {
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
          Ok(status) => {
            // TODO(THIS PR): Is unwrapping to 1 ok here?
            ExecuteResult::Continue(status.code().unwrap_or(1), Vec::new())
          }
          Err(err) => {
            eprintln!("{}", err);
            ExecuteResult::Continue(1, Vec::new())
          }
        }
      }
      .boxed(),
    }
  }
}

async fn resolve_command_path(
  command_name: &str,
  state: &EnvState,
) -> Result<PathBuf> {
  if command_name.is_empty() {
    bail!("Command name was empty.");
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

  bail!("Command not found.")
}

async fn evaluate_args(
  args: Vec<StringOrWord>,
  state: &EnvState,
) -> Vec<String> {
  let mut result = Vec::new();
  for arg in args {
    match arg {
      StringOrWord::Word(parts) => {
        // todo: maybe we should have this work like sh and I believe
        // reparse then continually re-evaluate until there's only strings left
        let text = evaluate_string_parts(parts, state).await;
        for part in text.split(' ') {
          let part = part.trim();
          if !part.is_empty() {
            result.push(part.to_string());
          }
        }
      }
      StringOrWord::String(parts) => {
        result.push(evaluate_string_parts(parts, state).await);
      }
    }
  }
  result
}

async fn evaluate_string_or_word(
  string_or_word: StringOrWord,
  state: &EnvState,
) -> String {
  evaluate_string_parts(string_or_word.into_parts(), state).await
}

async fn evaluate_string_parts(
  parts: Vec<StringPart>,
  state: &EnvState,
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
      StringPart::SubShell(list) => {
        final_text.push_str(&evaluate_string_part_subshell(list, state).await)
      }
    }
  }
  final_text
}

async fn evaluate_string_part_subshell(
  list: SequentialList,
  state: &EnvState,
) -> String {
  let task = execute_sequential_list(
    list,
    state.clone(),
    // todo: this is not correct. It should use the current stdin.
    ShellPipe::InheritStdin,
  );
  // todo: something better?
  let output_task = tokio::task::spawn(async {
    let mut final_data = Vec::new();
    match task.stdout {
      ShellPipe::InheritStdin => unreachable!(),
      ShellPipe::Channel(mut rx) => {
        while let Some(data) = rx.recv().await {
          final_data.extend(data);
        }
      }
    }
    final_data
  });
  let _ = task.task.await;
  let data = output_task.await.unwrap();
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
