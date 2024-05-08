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
  CommandPath(#[from] ResolveCommandPathError),
  #[error(transparent)]
  FailedShebang(#[from] FailedShebangError),
}

#[derive(Error, Debug)]
enum FailedShebangError {
  #[error(transparent)]
  CommandPath(#[from] ResolveCommandPathError),
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

/// Errors for executable commands.
#[derive(Error, Debug, PartialEq)]
pub enum ResolveCommandPathError {
  #[error("{}: command not found", .0)]
  CommandNotFound(String),
  #[error("command name was empty")]
  CommandEmpty,
}

impl ResolveCommandPathError {
  pub fn exit_code(&self) -> i32 {
    match self {
      // Use the Exit status that is used in bash: https://www.gnu.org/software/bash/manual/bash.html#Exit-Status
      ResolveCommandPathError::CommandNotFound(_) => 127,
      ResolveCommandPathError::CommandEmpty => 1,
    }
  }
}

pub fn resolve_command_path(
  command_name: &str,
  base_dir: &Path,
  state: &ShellState,
) -> Result<PathBuf, ResolveCommandPathError> {
  resolve_command_path_inner(command_name, base_dir, state, || {
    Ok(std::env::current_exe()?)
  })
}

fn resolve_command_path_inner(
  command_name: &str,
  base_dir: &Path,
  state: &ShellState,
  current_exe: impl FnOnce() -> Result<PathBuf>,
) -> Result<PathBuf, ResolveCommandPathError> {
  if command_name.is_empty() {
    return Err(ResolveCommandPathError::CommandEmpty);
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
    return Ok(base_dir.join(command_name));
  }

  // now search based on the current environment state
  let mut search_dirs = vec![base_dir.to_path_buf()];
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
  Err(ResolveCommandPathError::CommandNotFound(
    command_name.to_string(),
  ))
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

#[cfg(test)]
mod local_test {
  use super::*;

  #[test]
  fn should_resolve_current_exe_path_for_deno() {
    let cwd = std::env::current_dir().unwrap();
    let state = ShellState::new(
      Default::default(),
      &std::env::current_dir().unwrap(),
      Default::default(),
    );
    let path = resolve_command_path_inner("deno", &cwd, &state, || {
      Ok(PathBuf::from("/bin/deno"))
    })
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno"));

    let path = resolve_command_path_inner("deno", &cwd, &state, || {
      Ok(PathBuf::from("/bin/deno.exe"))
    })
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno.exe"));
  }

  #[test]
  fn should_error_on_unknown_command() {
    let cwd = std::env::current_dir().unwrap();
    let state = ShellState::new(Default::default(), &cwd, Default::default());
    // Command not found
    let result = resolve_command_path_inner("foobar", &cwd, &state, || {
      Ok(PathBuf::from("/bin/deno"))
    });
    assert_eq!(
      result,
      Err(ResolveCommandPathError::CommandNotFound(
        "foobar".to_string()
      ))
    );
    // Command empty
    let result = resolve_command_path_inner("", &cwd, &state, || {
      Ok(PathBuf::from("/bin/deno"))
    });
    assert_eq!(result, Err(ResolveCommandPathError::CommandEmpty));
  }
}
