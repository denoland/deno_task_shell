// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;

use crate::parser::Command;
use crate::parser::CommandInner;
use crate::parser::PipeSequence;
use crate::parser::PipeSequenceOperator;
use crate::parser::Pipeline;
use crate::parser::PipelineInner;
use crate::parser::Redirect;
use crate::parser::RedirectFd;
use crate::parser::RedirectOp;
use crate::parser::Sequence;
use crate::parser::SequentialList;
use crate::parser::SimpleCommand;
use crate::parser::Word;
use crate::parser::WordPart;
use crate::shell::commands::cat_command;
use crate::shell::commands::cd_command;
use crate::shell::commands::cp_command;
use crate::shell::commands::exit_command;
use crate::shell::commands::mkdir_command;
use crate::shell::commands::mv_command;
use crate::shell::commands::pwd_command;
use crate::shell::commands::rm_command;
use crate::shell::commands::sleep_command;
use crate::shell::commands::xargs_collect_args;
use crate::shell::types::pipe;
use crate::shell::types::EnvChange;
use crate::shell::types::ExecuteResult;
use crate::shell::types::FutureExecuteResult;
use crate::shell::types::ShellPipeReader;
use crate::shell::types::ShellPipeWriter;
use crate::shell::types::ShellState;

use self::types::CANCELLATION_EXIT_CODE;

mod commands;
mod fs_util;
mod types;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_builder;

pub async fn execute(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: &Path,
) -> i32 {
  execute_with_pipes(
    list,
    env_vars,
    cwd,
    ShellPipeReader::stdin(),
    ShellPipeWriter::stdout(),
    ShellPipeWriter::stderr(),
  )
  .await
}

pub(crate) async fn execute_with_pipes(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: &Path,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> i32 {
  assert!(cwd.is_absolute());
  let state = ShellState::new(env_vars, cwd);

  // spawn a sequential list and pipe its output to the environment
  let result = execute_sequential_list(
    list,
    state,
    stdin,
    stdout,
    stderr,
    AsyncCommandBehavior::Wait,
  )
  .await;

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

fn execute_sequential_list(
  list: SequentialList,
  mut state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
  async_command_behavior: AsyncCommandBehavior,
) -> FutureExecuteResult {
  async move {
    let mut final_exit_code = 0;
    let mut final_changes = Vec::new();
    let mut async_handles = Vec::new();
    let mut was_exit = false;
    for item in list.items {
      if item.is_async {
        let state = state.clone();
        let stdin = stdin.clone();
        let stdout = stdout.clone();
        let stderr = stderr.clone();
        async_handles.push(tokio::task::spawn_local(async move {
          let main_token = state.token();
          let result =
            execute_sequence(item.sequence, state, stdin, stdout, stderr).await;
          let (exit_code, handles) = result.into_exit_code_and_handles();
          wait_handles(exit_code, handles, main_token).await
        }));
      } else {
        let result = execute_sequence(
          item.sequence,
          state.clone(),
          stdin.clone(),
          stdout.clone(),
          stderr.clone(),
        )
        .await;
        match result {
          ExecuteResult::Exit(exit_code, handles) => {
            async_handles.extend(handles);
            final_exit_code = exit_code;
            was_exit = true;
            break;
          }
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
      final_exit_code = wait_handles(
        final_exit_code,
        async_handles.drain(..).collect(),
        state.token(),
      )
      .await;
    }

    if was_exit {
      ExecuteResult::Exit(final_exit_code, async_handles)
    } else {
      ExecuteResult::Continue(final_exit_code, final_changes, async_handles)
    }
  }
  .boxed_local()
}

async fn wait_handles(
  mut exit_code: i32,
  mut handles: Vec<JoinHandle<i32>>,
  token: CancellationToken,
) -> i32 {
  if exit_code != 0 {
    token.cancel();
  }
  while !handles.is_empty() {
    let result = futures::future::select_all(handles).await;

    // prefer the first non-zero then non-cancellation exit code
    let new_exit_code = result.0.unwrap();
    if matches!(exit_code, 0 | CANCELLATION_EXIT_CODE) && new_exit_code != 0 {
      exit_code = new_exit_code;
    }

    handles = result.2;
  }
  exit_code
}

fn execute_sequence(
  sequence: Sequence,
  mut state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> FutureExecuteResult {
  // requires boxed async because of recursive async
  async move {
    match sequence {
      Sequence::ShellVar(var) => ExecuteResult::Continue(
        0,
        vec![EnvChange::SetShellVar(
          var.name,
          evaluate_word(var.value, &state, stdin, stderr).await,
        )],
        Vec::new(),
      ),
      Sequence::BooleanList(list) => {
        let mut changes = vec![];
        let first_result = execute_sequence(
          list.current,
          state.clone(),
          stdin.clone(),
          stdout.clone(),
          stderr.clone(),
        )
        .await;
        let (exit_code, mut async_handles) = match first_result {
          ExecuteResult::Exit(_, _) => return first_result,
          ExecuteResult::Continue(exit_code, sub_changes, async_handles) => {
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
          let next_result =
            execute_sequence(next, state, stdin, stdout, stderr).await;
          match next_result {
            ExecuteResult::Exit(code, sub_handles) => {
              async_handles.extend(sub_handles);
              ExecuteResult::Exit(code, async_handles)
            }
            ExecuteResult::Continue(exit_code, sub_changes, sub_handles) => {
              changes.extend(sub_changes);
              async_handles.extend(sub_handles);
              ExecuteResult::Continue(exit_code, changes, async_handles)
            }
          }
        } else {
          ExecuteResult::Continue(exit_code, changes, async_handles)
        }
      }
      Sequence::Pipeline(pipeline) => {
        execute_pipeline(pipeline, state, stdin, stdout, stderr).await
      }
    }
  }
  .boxed_local()
}

async fn execute_pipeline(
  pipeline: Pipeline,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> ExecuteResult {
  let result =
    execute_pipeline_inner(pipeline.inner, state, stdin, stdout, stderr).await;
  if pipeline.negated {
    match result {
      ExecuteResult::Exit(code, handles) => ExecuteResult::Exit(code, handles),
      ExecuteResult::Continue(code, changes, handles) => {
        let new_code = if code == 0 { 1 } else { 0 };
        ExecuteResult::Continue(new_code, changes, handles)
      }
    }
  } else {
    result
  }
}

async fn execute_pipeline_inner(
  pipeline: PipelineInner,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> ExecuteResult {
  match pipeline {
    PipelineInner::Command(command) => {
      execute_command(command, state, stdin, stdout, stderr).await
    }
    PipelineInner::PipeSequence(pipe_sequence) => {
      execute_pipe_sequence(*pipe_sequence, state, stdin, stdout, stderr).await
    }
  }
}

async fn resolve_redirect_pipe(
  redirect: &Redirect,
  state: &ShellState,
  stdin: &ShellPipeReader,
  stderr: &mut ShellPipeWriter,
) -> Result<ShellPipeWriter, ExecuteResult> {
  let words = evaluate_word_parts(
    redirect.io_file.clone().into_parts(),
    state,
    stdin.clone(),
    stderr.clone(),
  )
  .await;
  // edge case that's not supported
  if words.is_empty() {
    let _ = stderr.write_line("redirect path must be 1 argument, but found 0");
    return Err(ExecuteResult::from_exit_code(1));
  } else if words.len() > 1 {
    let _ = stderr.write_line(&format!(
      concat!(
        "redirect path must be 1 argument, but found {0} ({1}). ",
        "Did you mean to quote it (ex. \"{1}\")?"
      ),
      words.len(),
      words.join(" ")
    ));
    return Err(ExecuteResult::from_exit_code(1));
  }
  let output_path = &words[0];

  // cross platform suppress output
  if output_path == "/dev/null" {
    return Ok(ShellPipeWriter::null());
  }

  let output_path = state.cwd().join(output_path);
  let std_file_result = std::fs::OpenOptions::new()
    .write(true)
    .create(true)
    .append(redirect.op == RedirectOp::Append)
    .truncate(redirect.op != RedirectOp::Append)
    .open(&output_path);
  match std_file_result {
    Ok(std_file) => Ok(ShellPipeWriter::from_std(std_file)),
    Err(err) => {
      let _ = stderr.write_line(&format!(
        "error opening file for redirect ({}). {:#}",
        output_path.display(),
        err
      ));
      Err(ExecuteResult::from_exit_code(1))
    }
  }
}

async fn execute_command(
  command: Command,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  let (stdout, stderr) = if let Some(redirect) = &command.redirect {
    let pipe = match resolve_redirect_pipe(
      redirect,
      &state,
      &stdin,
      &mut stderr,
    )
    .await
    {
      Ok(value) => value,
      Err(value) => return value,
    };

    match redirect.maybe_fd {
      Some(RedirectFd::Fd(2)) => (stdout, pipe),
      Some(RedirectFd::Fd(1)) | None => (pipe, stderr),
      Some(RedirectFd::Fd(_)) => {
        let _ = stderr.write_line(
          "only redirecting to stdout (1) and stderr (2) is supported",
        );
        return ExecuteResult::from_exit_code(1);
      }
      Some(RedirectFd::StdoutStderr) => (pipe.clone(), pipe),
    }
  } else {
    (stdout, stderr)
  };
  match command.inner {
    CommandInner::Simple(command) => {
      execute_simple_command(command, state, stdin, stdout, stderr).await
    }
    CommandInner::Subshell(list) => {
      execute_subshell(list, state, stdin, stdout, stderr).await
    }
  }
}

async fn execute_pipe_sequence(
  pipe_sequence: PipeSequence,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> ExecuteResult {
  let mut wait_tasks = vec![];
  let mut last_output = Some(stdin);
  let mut next_inner: Option<PipelineInner> = Some(pipe_sequence.into());
  while let Some(sequence) = next_inner.take() {
    let (output_reader, output_writer) = pipe();
    let (stderr, command) = match sequence {
      PipelineInner::PipeSequence(pipe_sequence) => {
        next_inner = Some(pipe_sequence.next);
        (
          match pipe_sequence.op {
            PipeSequenceOperator::Stdout => stderr.clone(),
            PipeSequenceOperator::StdoutStderr => output_writer.clone(),
          },
          pipe_sequence.current,
        )
      }
      PipelineInner::Command(command) => (stderr.clone(), command),
    };
    wait_tasks.push(execute_command(
      command,
      state.clone(),
      last_output.take().unwrap(),
      output_writer.clone(),
      stderr.clone(),
    ));
    last_output = Some(output_reader);
  }
  let output_handle = tokio::task::spawn_blocking(|| {
    last_output.unwrap().pipe_to_sender(stdout).unwrap();
  });
  let mut results = futures::future::join_all(wait_tasks).await;
  output_handle.await.unwrap();
  let last_result = results.pop().unwrap();
  let all_handles = results.into_iter().flat_map(|r| r.into_handles());
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

async fn execute_subshell(
  list: Box<SequentialList>,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> ExecuteResult {
  let result = execute_sequential_list(
    *list,
    state,
    stdin,
    stdout,
    stderr,
    // yield async commands to the parent
    AsyncCommandBehavior::Yield,
  )
  .await;

  // sub shells do not cause an exit
  match result {
    ExecuteResult::Exit(code, handles) => {
      ExecuteResult::Continue(code, Vec::new(), handles)
    }
    ExecuteResult::Continue(_, _, _) => result,
  }
}

async fn execute_simple_command(
  command: SimpleCommand,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> ExecuteResult {
  let args =
    evaluate_args(command.args, &state, stdin.clone(), stderr.clone()).await;
  let mut state = state.clone();
  for env_var in command.env_vars {
    state.apply_env_var(
      &env_var.name,
      &evaluate_word(env_var.value, &state, stdin.clone(), stderr.clone())
        .await,
    );
  }
  execute_command_args(args, state, stdin, stdout, stderr).await
}

fn execute_command_args(
  mut args: Vec<String>,
  state: ShellState,
  stdin: ShellPipeReader,
  mut stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> FutureExecuteResult {
  macro_rules! execute_with_cancellation {
    ($result_expr:expr, $token:ident) => {
      tokio::select! {
        result = $result_expr => {
          result
        },
        _ = $token.cancelled() => {
          ExecuteResult::for_cancellation()
        }
      }
    };
  }

  // requires boxing because of recursive async
  async move {
    let command_name = if args.is_empty() {
      String::new()
    } else {
      args.remove(0)
    };
    let token = state.token();
    if token.is_cancelled() {
      return ExecuteResult::for_cancellation();
    }
    if let Some(stripped_name) = command_name.strip_prefix('!') {
      let _ = stderr.write_line(
        &format!(concat!(
          "History expansion is not supported:\n",
          "  {}\n",
          "  ~\n\n",
          "Perhaps you meant to add a space after the exclamation point to negate the command?\n",
          "  ! {}",
        ), command_name, stripped_name)
      );
      ExecuteResult::from_exit_code(1)
    } else if command_name == "cd" {
      let cwd = state.cwd().clone();
      cd_command(&cwd, args, stderr)
    } else if command_name == "exit" {
      exit_command(args, stderr)
    } else if command_name == "pwd" {
      pwd_command(state.cwd(), args, stdout, stderr)
    } else if command_name == "echo" {
      let _ = stdout.write_line(&args.join(" "));
      ExecuteResult::from_exit_code(0)
    } else if command_name == "true" {
      // ignores additional arguments
      ExecuteResult::from_exit_code(0)
    } else if command_name == "false" {
      // ignores additional arguments
      ExecuteResult::from_exit_code(1)
    } else if command_name == "cp" {
      let cwd = state.cwd().clone();
      execute_with_cancellation!(cp_command(&cwd, args, stderr), token)
    } else if command_name == "mkdir" {
      let cwd = state.cwd().clone();
      execute_with_cancellation!(mkdir_command(&cwd, args, stderr), token)
    } else if command_name == "cat" {
      let cwd = state.cwd().clone();
      cat_command(&cwd, args, stdin, stdout, stderr, token)
    } else if command_name == "mv" {
      let cwd = state.cwd().clone();
      execute_with_cancellation!(mv_command(&cwd, args, stderr), token)
    } else if command_name == "rm" {
      let cwd = state.cwd().clone();
      execute_with_cancellation!(rm_command(&cwd, args, stderr), token)
    } else if command_name == "sleep" {
      execute_with_cancellation!(sleep_command(args, stderr), token)
    } else if command_name == "xargs" {
      match xargs_collect_args(args, stdin.clone()) {
        Ok(args) => {
          // don't select on cancellation here as that will occur at a lower level
          execute_command_args(args, state, stdin, stdout, stderr).await
        }
        Err(err) => {
          let _ = stderr.write_line(&format!("xargs: {err}"));
          ExecuteResult::from_exit_code(1)
        }
      }
    } else if command_name == "export" {
      evaluate_export_command(args)
    } else {
      let command_path = match resolve_command_path(&command_name, &state, || {
        Ok(std::env::current_exe()?)
      })
      {
        Ok(command_path) => command_path,
        Err(err) => {
          stderr.write_line(&err.to_string()).unwrap();
          return ExecuteResult::Continue(1, Vec::new(), Vec::new());
        }
      };

      let mut sub_command = tokio::process::Command::new(&command_path);
      let child = sub_command
        .current_dir(state.cwd())
        .args(&args)
        .env_clear()
        .envs(state.env_vars())
        .stdout(stdout.into_stdio())
        .stdin(stdin.into_stdio())
        .stderr(stderr.clone().into_stdio())
        .spawn();

      let mut child = match child {
        Ok(child) => child,
        Err(err) => {
          stderr
            .write_line(&format!("Error launching '{command_name}': {err}"))
            .unwrap();
          return ExecuteResult::Continue(1, Vec::new(), Vec::new());
        }
      };

      // avoid deadlock since this is holding onto the pipes
      drop(sub_command);

      tokio::select! {
        result = child.wait() => match result {
          Ok(status) => ExecuteResult::Continue(
            status.code().unwrap_or(1),
            Vec::new(),
            Vec::new(),
          ),
          Err(err) => {
            stderr.write_line(&format!("{err}")).unwrap();
            ExecuteResult::Continue(1, Vec::new(), Vec::new())
          }
        },
        _ = token.cancelled() => {
          let _ = child.kill().await;
          ExecuteResult::for_cancellation()
        }
      }
    }
  }.boxed_local()
}

fn evaluate_export_command(args: Vec<String>) -> ExecuteResult {
  let mut changes = Vec::new();
  for arg in args {
    // ignore if it doesn't contain an equals
    if let Some(equals_index) = arg.find('=') {
      let arg_name = &arg[..equals_index];
      let arg_value = &arg[equals_index + 1..];
      changes.push(EnvChange::SetEnvVar(
        arg_name.to_string(),
        arg_value.to_string(),
      ));
    }
  }
  ExecuteResult::Continue(0, changes, Vec::new())
}

fn resolve_command_path(
  command_name: &str,
  state: &ShellState,
  current_exe: impl FnOnce() -> Result<PathBuf>,
) -> Result<PathBuf> {
  if command_name.is_empty() {
    bail!("command name was empty");
  }

  // Special handling to use the current executable for deno.
  // This is to ensure deno tasks that use deno work in environments
  // that don't have deno on the path and to ensure it use the current
  // version of deno being executed rather than the one on the path,
  // which has caused some confusion.
  if command_name == "deno" {
    if let Ok(exe_path) = current_exe() {
      // this condition exists to make the tests pass because it's not
      // using the deno as the current executable
      let file_stem = exe_path.file_stem().map(|s| s.to_string_lossy());
      if file_stem.map(|s| s.to_string()) == Some("deno".to_string()) {
        return Ok(exe_path);
      }
    }
  }

  // check for absolute
  if PathBuf::from(command_name).is_absolute() {
    return Ok(PathBuf::from(command_name));
  }

  // then relative
  if command_name.contains('/')
    || (cfg!(windows) && command_name.contains('\\'))
  {
    return Ok(state.cwd().join(command_name));
  }

  // now search based on the current environment state
  let mut search_dirs = vec![state.cwd().clone()];
  if let Some(path) = state.get_var("PATH") {
    for folder in path.split(if cfg!(windows) { ';' } else { ':' }) {
      search_dirs.push(PathBuf::from(folder));
    }
  }
  let path_exts = if cfg!(windows) {
    let uc_command_name = command_name.to_uppercase();
    let path_ext = state
      .get_var("PATHEXT")
      .map(|s| s.as_str())
      .unwrap_or(".EXE;.CMD;.BAT;.COM");
    let command_exts = path_ext
      .split(';')
      .map(|s| s.trim().to_uppercase())
      .filter(|s| !s.is_empty())
      .collect::<Vec<_>>();
    if command_exts.is_empty()
      || command_exts
        .iter()
        .any(|ext| uc_command_name.ends_with(ext))
    {
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
        paths.push(search_dir.join(format!("{command_name}{path_ext}")))
      }
      paths
    } else {
      vec![search_dir.join(command_name)]
    };
    for path in paths {
      // don't use tokio::fs::metadata here as it was never returning
      // in some circumstances for some reason
      if let Ok(metadata) = std::fs::metadata(&path) {
        if metadata.is_file() {
          return Ok(path);
        }
      }
    }
  }

  bail!("{}: command not found", command_name)
}

async fn evaluate_args(
  args: Vec<Word>,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> Vec<String> {
  let mut result = Vec::new();
  for arg in args {
    let parts = evaluate_word_parts(
      arg.into_parts(),
      state,
      stdin.clone(),
      stderr.clone(),
    )
    .await;
    result.extend(parts);
  }
  result
}

async fn evaluate_word(
  word: Word,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> String {
  evaluate_word_parts(word.into_parts(), state, stdin, stderr)
    .await
    .join(" ")
}

fn evaluate_word_parts(
  parts: Vec<WordPart>,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> LocalBoxFuture<Vec<String>> {
  // recursive async, so requires boxing
  async move {
    let mut result = Vec::new();
    let mut current_text = String::new();
    for part in parts {
      let evaluation_result_text = match part {
        WordPart::Text(text) => {
          current_text.push_str(&text);
          None
        }
        WordPart::Variable(name) => state.get_var(&name).map(|v| v.to_string()),
        WordPart::Command(list) => Some(
          evaluate_command_substitution(
            list,
            // contain cancellation to the command substitution
            &state.with_child_token(),
            stdin.clone(),
            stderr.clone(),
          )
          .await,
        ),
        WordPart::Quoted(parts) => {
          let text =
            evaluate_word_parts(parts, state, stdin.clone(), stderr.clone())
              .await
              .join(" ");
          current_text.push_str(&text);
          continue;
        }
      };

      // This text needs to be turned into a vector of strings.
      // For now we do a very basic string split on whitespace, but in the future
      // we should continue to improve this functionality.
      if let Some(text) = evaluation_result_text {
        let mut parts = text
          .split(' ')
          .map(|p| p.trim())
          .filter(|p| !p.is_empty())
          .collect::<Vec<_>>();

        if !parts.is_empty() {
          // append the first part to the current text
          let first_part = parts.remove(0);
          current_text.push_str(first_part);

          // store the current text
          result.push(current_text);

          // store all the parts
          result.extend(parts.into_iter().map(|p| p.to_string()));

          // use the last part as the current text so it maybe
          // gets appended to in the future
          current_text = result.pop().unwrap();
        }
      }
    }
    if !current_text.is_empty() {
      result.push(current_text);
    }
    result
  }
  .boxed_local()
}

async fn evaluate_command_substitution(
  list: SequentialList,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> String {
  let text = execute_with_stdout_as_text(|shell_stdout_writer| {
    execute_sequential_list(
      list,
      state.clone(),
      stdin,
      shell_stdout_writer,
      stderr,
      AsyncCommandBehavior::Wait,
    )
  })
  .await;

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

async fn execute_with_stdout_as_text(
  execute: impl FnOnce(ShellPipeWriter) -> FutureExecuteResult,
) -> String {
  let (shell_stdout_reader, shell_stdout_writer) = pipe();
  let spawned_output = execute(shell_stdout_writer);
  let output_handle = tokio::task::spawn_blocking(move || {
    let mut final_data = Vec::new();
    shell_stdout_reader.pipe_to(&mut final_data).unwrap();
    final_data
  });
  let _ = spawned_output.await;
  let data = output_handle.await.unwrap();
  String::from_utf8_lossy(&data).to_string()
}

#[cfg(test)]
mod local_test {
  use super::*;

  #[test]
  fn should_resolve_current_exe_path_for_deno() {
    let state =
      ShellState::new(Default::default(), &std::env::current_dir().unwrap());
    let path =
      resolve_command_path("deno", &state, || Ok(PathBuf::from("/bin/deno")))
        .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno"));

    let path = resolve_command_path("deno", &state, || {
      Ok(PathBuf::from("/bin/deno.exe"))
    })
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno.exe"));
  }
}
