// Copyright 2018-2024 the Deno authors. MIT license.

use std::ffi::OsStr;
use std::ffi::OsString;
use std::path::Path;
use std::path::PathBuf;

use thiserror::Error;

use super::ShellState;

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
    sys_traits::impls::real_home_dir_with_env(self)
  }

  fn env_split_paths(&self, paths: &OsStr) -> Vec<PathBuf> {
    std::env::split_paths(paths).collect()
  }

  fn env_var_os(&self, name: &OsStr) -> Option<OsString> {
    self.get_var(&OsString::from(name)).cloned()
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

  #[cfg(unix)]
  fn is_valid_executable(
    &self,
    path: &std::path::Path,
  ) -> std::io::Result<bool> {
    use rustix::fs as rfs;
    rfs::access(path, rfs::Access::EXEC_OK)
      .map(|_| true)
      .map_err(|e| io::Error::from_raw_os_error(e.raw_os_error()))
  }

  #[cfg(windows)]
  fn is_valid_executable(
    &self,
    path: &std::path::Path,
  ) -> std::io::Result<bool> {
    use std::os::windows::ffi::OsStrExt;

    let name = path
      .as_os_str()
      .encode_wide()
      .chain(Some(0))
      .collect::<Vec<u16>>();
    let mut bt: u32 = 0;
    // SAFETY: winapi call
    unsafe {
      Ok(
        windows_sys::Win32::Storage::FileSystem::GetBinaryTypeW(
          name.as_ptr(),
          &mut bt,
        ) != 0,
      )
    }
  }
}

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
