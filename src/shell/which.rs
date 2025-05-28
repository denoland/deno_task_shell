// Copyright 2018-2024 the Deno authors. MIT license.

use std::ffi::OsStr;
use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;

use thiserror::Error;

use super::ShellState;

/// Error when a command path could not be resolved.
#[derive(Error, Debug, PartialEq)]
pub enum CommandPathResolutionError {
  #[error("{}: command not found", .0.to_string_lossy())]
  CommandNotFound(OsString),
  #[error("{}: failed canonicalizing", .0.to_string_lossy())]
  FailedCanonicalizing(OsString),
  #[error("command name was empty")]
  CommandEmpty,
}

impl CommandPathResolutionError {
  pub fn exit_code(&self) -> i32 {
    match self {
      // Use the Exit status that is used in bash: https://www.gnu.org/software/bash/manual/bash.html#Exit-Status
      CommandPathResolutionError::CommandNotFound(_) => 127,
      CommandPathResolutionError::CommandEmpty
      | CommandPathResolutionError::FailedCanonicalizing(_) => 1,
    }
  }
}

/// Resolves a command name to an absolute path.
pub fn resolve_command_path(
  command_name: &OsStr,
  base_dir: &Path,
  state: &ShellState,
) -> Result<PathBuf, CommandPathResolutionError> {
  if command_name.is_empty() {
    return Err(CommandPathResolutionError::CommandEmpty);
  }

  // check for absolute
  if PathBuf::from(command_name).is_absolute() {
    return Ok(PathBuf::from(command_name));
  }

  let result = which::WhichConfig::new_with_sys(state.clone())
    .binary_name(command_name.to_os_string())
    .custom_cwd(base_dir.to_path_buf())
    .first_result();
  result.map_err(|err| match err {
    which::Error::CannotFindBinaryPath
    | which::Error::CannotGetCurrentDirAndPathListEmpty => {
      CommandPathResolutionError::CommandNotFound(command_name.into())
    }
    which::Error::CannotCanonicalize => {
      CommandPathResolutionError::FailedCanonicalizing(command_name.into())
    }
  })
}

impl which::sys::Sys for ShellState {
  type ReadDirEntry = std::fs::DirEntry;

  type Metadata = std::fs::Metadata;

  fn is_windows(&self) -> bool {
    cfg!(windows)
  }

  fn current_dir(&self) -> std::io::Result<PathBuf> {
    Ok(self.cwd().to_path_buf())
  }

  fn home_dir(&self) -> Option<PathBuf> {
    // disable tilde expansion because that's handled by the shell
    None
  }

  fn env_split_paths(&self, paths: &OsStr) -> Vec<PathBuf> {
    std::env::split_paths(paths).collect()
  }

  fn env_var_os(&self, name: &OsStr) -> Option<OsString> {
    self.get_var(name).cloned()
  }

  fn metadata(&self, path: &Path) -> std::io::Result<Self::Metadata> {
    std::fs::metadata(path)
  }

  fn symlink_metadata(&self, path: &Path) -> std::io::Result<Self::Metadata> {
    std::fs::symlink_metadata(path)
  }

  fn read_dir(
    &self,
    path: &Path,
  ) -> std::io::Result<
    Box<dyn Iterator<Item = std::io::Result<Self::ReadDirEntry>>>,
  > {
    let iter = std::fs::read_dir(path)?;
    Ok(Box::new(iter))
  }

  fn is_valid_executable(
    &self,
    _path: &std::path::Path,
  ) -> std::io::Result<bool> {
    // we've considered everything to be executable so that cross platform
    // shebangs work and so that people don't need to bother marking an
    // item as executable in git, which is annoying for Windows users
    Ok(true)
  }
}
