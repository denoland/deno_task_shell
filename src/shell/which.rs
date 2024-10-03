// Copyright 2018-2024 the Deno authors. MIT license.

use std::borrow::Cow;
use std::path::Path;
use std::path::PathBuf;

use thiserror::Error;

/// Error when a command path could not be resolved.
#[derive(Error, Debug, PartialEq)]
pub enum CommandPathResolutionError {
  #[error("{}: command not found", .0)]
  CommandNotFound(String),
  #[error("command name was empty")]
  CommandEmpty,
}

impl CommandPathResolutionError {
  pub fn exit_code(&self) -> i32 {
    match self {
      // Use the Exit status that is used in bash: https://www.gnu.org/software/bash/manual/bash.html#Exit-Status
      CommandPathResolutionError::CommandNotFound(_) => 127,
      CommandPathResolutionError::CommandEmpty => 1,
    }
  }
}

/// Resolves a command name to an absolute path.
pub fn resolve_command_path<'a>(
  command_name: &str,
  base_dir: &Path,
  get_var: impl Fn(&str) -> Option<Cow<'a, str>>,
  current_exe: impl FnOnce() -> std::io::Result<PathBuf>,
) -> Result<PathBuf, CommandPathResolutionError> {
  if command_name.is_empty() {
    return Err(CommandPathResolutionError::CommandEmpty);
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
      if file_stem.map(|s| !s.starts_with("deno_task_shell-")).unwrap_or(true) {
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
  if let Some(path) = get_var("PATH") {
    for folder in path.split(if cfg!(windows) { ';' } else { ':' }) {
      search_dirs.push(PathBuf::from(folder));
    }
  }
  let path_exts = if cfg!(windows) {
    let uc_command_name = command_name.to_uppercase();
    let path_ext =
      get_var("PATHEXT").unwrap_or(Cow::Borrowed(".EXE;.CMD;.BAT;.COM"));
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
  Err(CommandPathResolutionError::CommandNotFound(
    command_name.to_string(),
  ))
}

#[cfg(test)]
mod local_test {
  use super::*;

  #[test]
  fn should_resolve_current_exe_path_for_deno() {
    let cwd = std::env::current_dir().unwrap();
    let path = resolve_command_path(
      "deno",
      &cwd,
      |_| None,
      || Ok(PathBuf::from("/bin/deno")),
    )
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno"));

    let path = resolve_command_path(
      "deno",
      &cwd,
      |_| None,
      || Ok(PathBuf::from("/bin/deno.exe")),
    )
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno.exe"));
  }

  #[test]
  fn should_error_on_unknown_command() {
    let cwd = std::env::current_dir().unwrap();
    // Command not found
    let result = resolve_command_path(
      "foobar",
      &cwd,
      |_| None,
      || Ok(PathBuf::from("/bin/deno")),
    );
    assert_eq!(
      result,
      Err(CommandPathResolutionError::CommandNotFound(
        "foobar".to_string()
      ))
    );
    // Command empty
    let result = resolve_command_path(
      "",
      &cwd,
      |_| None,
      || Ok(PathBuf::from("/bin/deno")),
    );
    assert_eq!(result, Err(CommandPathResolutionError::CommandEmpty));
  }
}
