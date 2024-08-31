// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::io::BufRead;
use std::io::BufReader;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

use crate::shell::types::ShellState;
use crate::ExecutableCommand;
use crate::ExecuteResult;
use crate::FutureExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use anyhow::Result;
use futures::FutureExt;
use thiserror::Error;

use super::which::CommandPathResolutionError;

#[derive(Debug, Clone)]
pub struct UnresolvedCommandName {
  pub name: String,
  pub base_dir: PathBuf,
}

pub fn execute_unresolved_command_name(
  command_name: UnresolvedCommandName,
  mut context: ShellCommandContext,
) -> FutureExecuteResult {
  async move {
    let command =
      match resolve_command(&command_name, &context, &context.args).await {
        Ok(command_path) => command_path,
        Err(ResolveCommandError::CommandPath(err)) => {
          let _ = context.stderr.write_line(&format!("{}", err));
          return ExecuteResult::Continue(
            err.exit_code(),
            Vec::new(),
            Vec::new(),
          );
        }
        Err(ResolveCommandError::FailedShebang(err)) => {
          let _ = context
            .stderr
            .write_line(&format!("{}: {}", command_name.name, err));
          return ExecuteResult::Continue(
            err.exit_code(),
            Vec::new(),
            Vec::new(),
          );
        }
      };
    match command.command_name {
      CommandName::Resolved(path) => {
        ExecutableCommand::new(command_name.name, path)
          .execute(context)
          .await
      }
      CommandName::Unresolved(command_name) => {
        context.args = command.args.into_owned();
        execute_unresolved_command_name(command_name, context).await
      }
    }
  }
  .boxed_local()
}

enum CommandName {
  Resolved(PathBuf),
  Unresolved(UnresolvedCommandName),
}

struct ResolvedCommand<'a> {
  command_name: CommandName,
  args: Cow<'a, Vec<String>>,
}

#[derive(Error, Debug)]
enum ResolveCommandError {
  #[error(transparent)]
  CommandPath(#[from] CommandPathResolutionError),
  #[error(transparent)]
  FailedShebang(#[from] FailedShebangError),
}

#[derive(Error, Debug)]
enum FailedShebangError {
  #[error(transparent)]
  CommandPath(#[from] CommandPathResolutionError),
  #[error(transparent)]
  Any(#[from] anyhow::Error),
}

impl FailedShebangError {
  pub fn exit_code(&self) -> i32 {
    match self {
      FailedShebangError::CommandPath(err) => err.exit_code(),
      FailedShebangError::Any(_) => 1,
    }
  }
}

async fn resolve_command<'a>(
  command_name: &UnresolvedCommandName,
  context: &ShellCommandContext,
  original_args: &'a Vec<String>,
) -> Result<ResolvedCommand<'a>, ResolveCommandError> {
  let command_path = match resolve_command_path(
    &command_name.name,
    &command_name.base_dir,
    &context.state,
  ) {
    Ok(command_path) => command_path,
    Err(err) => return Err(err.into()),
  };

  // only bother checking for a shebang when the path has a slash
  // in it because for global commands someone on Windows likely
  // won't have a script with a shebang in it on Windows
  if command_name.name.contains('/') {
    if let Some(shebang) = resolve_shebang(&command_path).map_err(|err| {
      ResolveCommandError::FailedShebang(FailedShebangError::Any(err.into()))
    })? {
      let (shebang_command_name, mut args) = if shebang.string_split {
        let mut args = parse_shebang_args(&shebang.command, context)
          .await
          .map_err(FailedShebangError::Any)?;
        args.push(command_path.to_string_lossy().to_string());
        (args.remove(0), args)
      } else {
        (
          shebang.command,
          vec![command_path.to_string_lossy().to_string()],
        )
      };
      args.extend(original_args.iter().cloned());
      return Ok(ResolvedCommand {
        command_name: CommandName::Unresolved(UnresolvedCommandName {
          name: shebang_command_name,
          base_dir: command_path.parent().unwrap().to_path_buf(),
        }),
        args: Cow::Owned(args),
      });
    }
  }

  return Ok(ResolvedCommand {
    command_name: CommandName::Resolved(command_path),
    args: Cow::Borrowed(original_args),
  });
}

async fn parse_shebang_args(
  text: &str,
  context: &ShellCommandContext,
) -> Result<Vec<String>> {
  fn err_unsupported(text: &str) -> Result<Vec<String>> {
    anyhow::bail!("unsupported shebang. Please report this as a bug (https://github.com/denoland/deno).\n\nShebang: {}", text)
  }

  let mut args = crate::parser::parse(text)?;
  if args.items.len() != 1 {
    return err_unsupported(text);
  }
  let item = args.items.remove(0);
  if item.is_async {
    return err_unsupported(text);
  }
  let pipeline = match item.sequence {
    crate::parser::Sequence::Pipeline(pipeline) => pipeline,
    _ => return err_unsupported(text),
  };
  if pipeline.negated {
    return err_unsupported(text);
  }
  let cmd = match pipeline.inner {
    crate::parser::PipelineInner::Command(cmd) => cmd,
    crate::parser::PipelineInner::PipeSequence(_) => {
      return err_unsupported(text)
    }
  };
  if cmd.redirect.is_some() {
    return err_unsupported(text);
  }
  let cmd = match cmd.inner {
    crate::parser::CommandInner::Simple(cmd) => cmd,
    crate::parser::CommandInner::Subshell(_) => return err_unsupported(text),
  };
  if !cmd.env_vars.is_empty() {
    return err_unsupported(text);
  }

  Ok(
    super::execute::evaluate_args(
      cmd.args,
      &context.state,
      context.stdin.clone(),
      context.stderr.clone(),
    )
    .await?,
  )
}

pub fn resolve_command_path(
  command_name: &str,
  base_dir: &Path,
  state: &ShellState,
) -> Result<PathBuf, CommandPathResolutionError> {
  super::which::resolve_command_path(
    command_name,
    base_dir,
    |name| state.get_var(name).map(|s| Cow::Borrowed(s.as_str())),
    std::env::current_exe,
  )
}

struct Shebang {
  string_split: bool,
  command: String,
}

fn resolve_shebang(
  file_path: &Path,
) -> Result<Option<Shebang>, std::io::Error> {
  let mut file = match std::fs::File::open(file_path) {
    Ok(file) => file,
    Err(err) if err.kind() == std::io::ErrorKind::NotFound => {
      return Ok(None);
    }
    Err(err) => return Err(err),
  };
  let text = b"#!/usr/bin/env ";
  let mut buffer = vec![0; text.len()];
  match file.read_exact(&mut buffer) {
    Ok(_) if buffer == text => (),
    _ => return Ok(None),
  }

  let mut reader = BufReader::new(file);
  let mut line = String::new();
  reader.read_line(&mut line)?;
  if line.is_empty() {
    return Ok(None);
  }
  let line = line.trim();

  Ok(Some(if let Some(command) = line.strip_prefix("-S ") {
    Shebang {
      string_split: true,
      command: command.to_string(),
    }
  } else {
    Shebang {
      string_split: false,
      command: line.to_string(),
    }
  }))
}
