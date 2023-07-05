// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::rc::Rc;

use futures::future;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;

pub use crate::shell::commands::ExecutableCommand;
pub use crate::shell::commands::ExecuteCommandArgsContext;
pub use crate::shell::commands::ShellCommand;
pub use crate::shell::commands::ShellCommandContext;
pub use crate::shell::types::EnvChange;
pub use crate::shell::types::ExecuteResult;
pub use crate::shell::types::FutureExecuteResult;
pub use crate::shell::types::ShellPipeReader;
pub use crate::shell::types::ShellPipeWriter;
pub use crate::shell::types::ShellState;

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

use self::types::CANCELLATION_EXIT_CODE;

mod commands;
mod fs_util;
mod types;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_builder;

/// Executes a `SequentialList` of commands in a deno_task_shell environment.
///
/// This function accepts a list of commands, a map of environment variables, the current working directory,
/// and a map of custom shell commands. It sets up the shell state and then calls `execute_with_pipes`
/// with the standard input, output, and error streams.
///
/// # Arguments
/// * `list` - A `SequentialList` of commands to execute.
/// * `env_vars` - A map of environment variables which are set in the shell.
/// * `cwd` - The current working directory.
/// * `custom_commands` - A map of custom shell commands and there ShellCommand implementation.
///
/// # Returns
/// The exit code of the command execution.
pub async fn execute(
  list: SequentialList,
  env_vars: HashMap<String, String>,
  cwd: &Path,
  custom_commands: HashMap<String, Rc<dyn ShellCommand>>,
) -> i32 {
  let state = ShellState::new(env_vars, cwd, custom_commands);
  execute_with_pipes(
    list,
    state,
    ShellPipeReader::stdin(),
    ShellPipeWriter::stdout(),
    ShellPipeWriter::stderr(),
  )
  .await
}

/// Executes a `SequentialList` of commands with specified input and output pipes.
///
/// This function accepts a list of commands, a shell state, and pipes for standard input, output, and error.
/// This function allows the user to retrive the data outputted by the execution and act on it using code.
/// This is made public for the use-case of running tests with shell execution in application depending on the library.
///
/// # Arguments
///
/// * `list` - A `SequentialList` of commands to execute.
/// * `state` - The current state of the shell, including environment variables and the current directory.
/// * `stdin` - A reader for the standard input stream.
/// * `stdout` - A writer for the standard output stream.
/// * `stderr` - A writer for the standard error stream.
///
/// # Returns
///
/// The exit code of the command execution.
pub async fn execute_with_pipes(
  list: SequentialList,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  stderr: ShellPipeWriter,
) -> i32 {
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
          let main_token = state.token().clone();
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
        state.token().clone(),
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
  mut stderr: ShellPipeWriter,
) -> FutureExecuteResult {
  // requires boxed async because of recursive async
  async move {
    match sequence {
      Sequence::ShellVar(var) => ExecuteResult::Continue(
        0,
        vec![EnvChange::SetShellVar(
          var.name,
          match evaluate_word(var.value, &state, stdin, stderr.clone()).await {
            Ok(value) => value,
            Err(err) => {
              return err.into_exit_code(&mut stderr);
            }
          },
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
  let words = match words {
    Ok(word) => word,
    Err(err) => {
      return Err(err.into_exit_code(stderr));
    }
  };
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
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  let args =
    evaluate_args(command.args, &state, stdin.clone(), stderr.clone()).await;
  let args = match args {
    Ok(args) => args,
    Err(err) => {
      return err.into_exit_code(&mut stderr);
    }
  };
  let mut state = state.clone();
  for env_var in command.env_vars {
    let value =
      evaluate_word(env_var.value, &state, stdin.clone(), stderr.clone()).await;
    let value = match value {
      Ok(value) => value,
      Err(err) => {
        return err.into_exit_code(&mut stderr);
      }
    };
    state.apply_env_var(&env_var.name, &value);
  }
  execute_command_args(args, state, stdin, stdout, stderr).await
}

fn execute_command_args(
  mut args: Vec<String>,
  state: ShellState,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> FutureExecuteResult {
  let command_name = if args.is_empty() {
    String::new()
  } else {
    args.remove(0)
  };
  if state.token().is_cancelled() {
    Box::pin(future::ready(ExecuteResult::for_cancellation()))
  } else if let Some(stripped_name) = command_name.strip_prefix('!') {
    let _ = stderr.write_line(
        &format!(concat!(
          "History expansion is not supported:\n",
          "  {}\n",
          "  ~\n\n",
          "Perhaps you meant to add a space after the exclamation point to negate the command?\n",
          "  ! {}",
        ), command_name, stripped_name)
      );
    Box::pin(future::ready(ExecuteResult::from_exit_code(1)))
  } else {
    let command = state.resolve_command(&command_name).unwrap_or_else(|| {
      Rc::new(ExecutableCommand::new(command_name)) as Rc<dyn ShellCommand>
    });
    command.execute(ShellCommandContext {
      args,
      state,
      stdin,
      stdout,
      stderr,
      execute_command_args: Box::new(move |context| {
        execute_command_args(
          context.args,
          context.state,
          context.stdin,
          context.stdout,
          context.stderr,
        )
      }),
    })
  }
}

async fn evaluate_args(
  args: Vec<Word>,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> Result<Vec<String>, EvaluateWordTextError> {
  let mut result = Vec::new();
  for arg in args {
    let parts = evaluate_word_parts(
      arg.into_parts(),
      state,
      stdin.clone(),
      stderr.clone(),
    )
    .await?;
    result.extend(parts);
  }
  Ok(result)
}

async fn evaluate_word(
  word: Word,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> Result<String, EvaluateWordTextError> {
  Ok(
    evaluate_word_parts(word.into_parts(), state, stdin, stderr)
      .await?
      .join(" "),
  )
}

enum EvaluateWordTextError {
  InvalidPattern {
    pattern: String,
    err: glob::PatternError,
  },
  NoFilesMatched {
    pattern: String,
  },
}

impl EvaluateWordTextError {
  pub fn message(&self) -> String {
    match self {
      EvaluateWordTextError::InvalidPattern { pattern, err } => {
        format!("glob: no matches found '{pattern}'. {err}")
      }
      EvaluateWordTextError::NoFilesMatched { pattern } => {
        format!("glob: no matches found '{pattern}'")
      }
    }
  }

  pub fn into_exit_code(self, stderr: &mut ShellPipeWriter) -> ExecuteResult {
    let _ = stderr.write_line(&self.message());
    ExecuteResult::from_exit_code(1)
  }
}

fn evaluate_word_parts(
  parts: Vec<WordPart>,
  state: &ShellState,
  stdin: ShellPipeReader,
  stderr: ShellPipeWriter,
) -> LocalBoxFuture<Result<Vec<String>, EvaluateWordTextError>> {
  enum TextPart {
    Quoted(String),
    Text(String),
  }

  impl TextPart {
    pub fn as_str(&self) -> &str {
      match self {
        TextPart::Quoted(text) => text,
        TextPart::Text(text) => text,
      }
    }
  }

  fn text_parts_to_string(parts: Vec<TextPart>) -> String {
    let mut result =
      String::with_capacity(parts.iter().map(|p| p.as_str().len()).sum());
    for part in parts {
      result.push_str(part.as_str());
    }
    result
  }

  fn evaluate_word_text(
    state: &ShellState,
    text_parts: Vec<TextPart>,
    is_quoted: bool,
  ) -> Result<Vec<String>, EvaluateWordTextError> {
    if !is_quoted
      && text_parts
        .iter()
        .filter_map(|p| match p {
          TextPart::Quoted(_) => None,
          TextPart::Text(text) => Some(text.as_str()),
        })
        .any(|text| text.chars().any(|c| matches!(c, '?' | '*' | '[')))
    {
      let mut current_text = String::new();
      for text_part in text_parts {
        match text_part {
          TextPart::Quoted(text) => {
            for c in text.chars() {
              match c {
                '?' | '*' | '[' | ']' => {
                  // escape because it was quoted
                  current_text.push('[');
                  current_text.push(c);
                  current_text.push(']');
                }
                _ => current_text.push(c),
              }
            }
          }
          TextPart::Text(text) => {
            current_text.push_str(&text);
          }
        }
      }
      let is_absolute = std::path::PathBuf::from(&current_text).is_absolute();
      let cwd = state.cwd();
      let pattern = if is_absolute {
        current_text
      } else {
        format!("{}/{}", cwd.display(), current_text)
      };
      let result = glob::glob_with(
        &pattern,
        glob::MatchOptions {
          // false because it should work the same way on case insensitive file systems
          case_sensitive: false,
          // true because it copies what sh does
          require_literal_separator: true,
          // true because it copies with sh does—these files are considered "hidden"
          require_literal_leading_dot: true,
        },
      );
      match result {
        Ok(paths) => {
          let paths =
            paths.into_iter().filter_map(|p| p.ok()).collect::<Vec<_>>();
          if paths.is_empty() {
            Err(EvaluateWordTextError::NoFilesMatched { pattern })
          } else {
            let paths = if is_absolute {
              paths
                .into_iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>()
            } else {
              paths
                .into_iter()
                .map(|p| {
                  let path = p.strip_prefix(cwd).unwrap();
                  path.display().to_string()
                })
                .collect::<Vec<_>>()
            };
            Ok(paths)
          }
        }
        Err(err) => Err(EvaluateWordTextError::InvalidPattern { pattern, err }),
      }
    } else {
      Ok(vec![text_parts_to_string(text_parts)])
    }
  }

  fn evaluate_word_parts_inner(
    parts: Vec<WordPart>,
    is_quoted: bool,
    state: &ShellState,
    stdin: ShellPipeReader,
    stderr: ShellPipeWriter,
  ) -> LocalBoxFuture<Result<Vec<String>, EvaluateWordTextError>> {
    // recursive async, so requires boxing
    async move {
      let mut result = Vec::new();
      let mut current_text = Vec::new();
      for part in parts {
        let evaluation_result_text = match part {
          WordPart::Text(text) => {
            current_text.push(TextPart::Text(text));
            None
          }
          WordPart::Variable(name) => {
            state.get_var(&name).map(|v| v.to_string())
          }
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
            let text = evaluate_word_parts_inner(
              parts,
              true,
              state,
              stdin.clone(),
              stderr.clone(),
            )
            .await?
            .join(" ");

            current_text.push(TextPart::Quoted(text));
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
            .map(|p| TextPart::Text(p.to_string()))
            .collect::<Vec<_>>();

          if !parts.is_empty() {
            // append the first part to the current text
            let first_part = parts.remove(0);
            current_text.push(first_part);

            if !parts.is_empty() {
              // evaluate and store the current text
              result.extend(evaluate_word_text(
                state,
                current_text,
                is_quoted,
              )?);

              // store all the parts except the last one
              for part in parts.drain(..parts.len() - 1) {
                result.extend(evaluate_word_text(
                  state,
                  vec![part],
                  is_quoted,
                )?);
              }

              // use the last part as the current text so it maybe
              // gets appended to in the future
              current_text = parts;
            }
          }
        }
      }
      if !current_text.is_empty() {
        result.extend(evaluate_word_text(state, current_text, is_quoted)?);
      }
      Ok(result)
    }
    .boxed_local()
  }

  evaluate_word_parts_inner(parts, false, state, stdin, stderr)
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

/// Used to communicate between commands.
pub fn pipe() -> (ShellPipeReader, ShellPipeWriter) {
  let (reader, writer) = os_pipe::pipe().unwrap();
  (ShellPipeReader(reader), ShellPipeWriter::OsPipe(writer))
}

/// Creates and returns a new `ShellPipeWriter` and its associated output handler.
///
/// The output handler reads from the writer, stores the data in a buffer, and converts that buffer to a `String`.
/// This function is useful when reading the output of an execution, in for instance tests.
///
/// # Returns
///
/// * A `ShellPipeWriter` for writing to the pipe.
/// * A `JoinHandle<String>` that will return the output when awaited.
pub fn get_output_writer_and_handle() -> (ShellPipeWriter, JoinHandle<String>) {
  let (stdout_reader, stdout_writer) = pipe();
  let stdout_handle = tokio::task::spawn_blocking(|| {
    let mut buf = Vec::new();
    stdout_reader.pipe_to(&mut buf).unwrap();
    String::from_utf8_lossy(&buf).to_string()
  });
  (stdout_writer, stdout_handle)
}
