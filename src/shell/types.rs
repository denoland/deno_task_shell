// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use futures::future::BoxFuture;
use tokio::task::JoinHandle;

use crate::shell::fs_util;

#[derive(Clone)]
pub struct ShellState {
  /// Environment variables that should be passed down to sub commands
  /// and used when evaluating environment variables.
  env_vars: HashMap<String, String>,
  /// Variables that should be evaluated within the shell and
  /// not passed down to any sub commands.
  shell_vars: HashMap<String, String>,
  cwd: PathBuf,
}

impl ShellState {
  pub fn new(env_vars: HashMap<String, String>, cwd: &Path) -> Self {
    let mut result = Self {
      env_vars: Default::default(),
      shell_vars: Default::default(),
      cwd: PathBuf::new(),
    };
    // ensure the data is normalized
    for (name, value) in env_vars {
      result.apply_env_var(&name, &value);
    }
    result.set_cwd(cwd);
    result
  }

  pub fn cwd(&self) -> &PathBuf {
    &self.cwd
  }

  pub fn env_vars(&self) -> &HashMap<String, String> {
    &self.env_vars
  }

  pub fn get_var(&self, name: &str) -> Option<&String> {
    let name = if cfg!(windows) {
      Cow::Owned(name.to_uppercase())
    } else {
      Cow::Borrowed(name)
    };
    self
      .env_vars
      .get(name.as_ref())
      .or_else(|| self.shell_vars.get(name.as_ref()))
  }

  pub fn set_cwd(&mut self, cwd: &Path) {
    self.cwd = cwd.to_path_buf();
    // $PWD holds the current working directory, so we keep cwd and $PWD in sync
    self
      .env_vars
      .insert("PWD".to_string(), self.cwd.display().to_string());
  }

  pub fn apply_changes(&mut self, changes: &[EnvChange]) {
    for change in changes {
      self.apply_change(change);
    }
  }

  pub fn apply_change(&mut self, change: &EnvChange) {
    match change {
      EnvChange::SetEnvVar(name, value) => self.apply_env_var(name, value),
      EnvChange::SetShellVar(name, value) => {
        if self.env_vars.contains_key(name) {
          self.apply_env_var(name, value);
        } else {
          self.shell_vars.insert(name.to_string(), value.to_string());
        }
      }
      EnvChange::Cd(new_dir) => {
        self.cwd = new_dir.clone();
      }
    }
  }

  pub fn apply_env_var(&mut self, name: &str, value: &str) {
    let name = if cfg!(windows) {
      // environment variables are case insensitive on windows
      name.to_uppercase()
    } else {
      name.to_string()
    };
    if name == "PWD" {
      let cwd = PathBuf::from(value);
      if cwd.is_absolute() {
        if let Ok(cwd) = fs_util::canonicalize_path(&cwd) {
          // this will update the environment variable too
          self.set_cwd(&cwd);
        }
      }
    } else {
      self.shell_vars.remove(&name);
      if value.is_empty() {
        self.env_vars.remove(&name);
      } else {
        self.env_vars.insert(name, value.to_string());
      }
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EnvChange {
  // `export ENV_VAR=VALUE`
  SetEnvVar(String, String),
  // `ENV_VAR=VALUE`
  SetShellVar(String, String),
  Cd(PathBuf),
}

pub type FutureExecuteResult = BoxFuture<'static, ExecuteResult>;

#[derive(Debug)]
pub enum ExecuteResult {
  Exit(i32, Vec<JoinHandle<()>>),
  Continue(i32, Vec<EnvChange>, Vec<JoinHandle<()>>),
}

impl ExecuteResult {
  pub fn into_handles(self) -> Vec<JoinHandle<()>> {
    match self {
      ExecuteResult::Exit(_, handles) => handles,
      ExecuteResult::Continue(_, _, handles) => handles,
    }
  }

  pub fn from_exit_code(exit_code: i32) -> ExecuteResult {
    ExecuteResult::Continue(exit_code, Vec::new(), Vec::new())
  }
}

/// Reader side of a pipe.
pub struct ShellPipeReader(os_pipe::PipeReader);

impl Clone for ShellPipeReader {
  fn clone(&self) -> Self {
    Self(self.0.try_clone().unwrap())
  }
}

impl ShellPipeReader {
  pub fn stdin() -> ShellPipeReader {
    ShellPipeReader::from_raw(os_pipe::dup_stdin().unwrap())
  }

  pub fn from_raw(reader: os_pipe::PipeReader) -> Self {
    Self(reader)
  }

  pub fn into_stdio(self) -> std::process::Stdio {
    self.0.into()
  }

  /// Pipe everything to the specified writer
  pub fn pipe_to(mut self, writer: &mut dyn Write) -> Result<()> {
    loop {
      let mut buffer = [0; 512]; // todo: what is an appropriate buffer size?
      let size = self.0.read(&mut buffer)?;
      if size == 0 {
        break;
      }
      writer.write_all(&buffer[0..size])?;
    }
    Ok(())
  }

  /// Pipes this pipe to the specified sender.
  pub fn pipe_to_sender(self, mut sender: ShellPipeWriter) -> Result<()> {
    match &mut sender {
      ShellPipeWriter::OsPipe(pipe) => self.pipe_to(pipe),
      ShellPipeWriter::StdFile(file) => self.pipe_to(file),
      ShellPipeWriter::Null => Ok(()),
    }
  }
}

/// Writer side of a pipe.
///
/// Ensure that all of these are dropped when complete in order to
/// prevent deadlocks where the reader hangs waiting for a read.
pub enum ShellPipeWriter {
  OsPipe(os_pipe::PipeWriter),
  StdFile(std::fs::File),
  Null,
}

impl Clone for ShellPipeWriter {
  fn clone(&self) -> Self {
    match self {
      Self::OsPipe(pipe) => Self::OsPipe(pipe.try_clone().unwrap()),
      Self::StdFile(file) => Self::StdFile(file.try_clone().unwrap()),
      Self::Null => Self::Null,
    }
  }
}

impl ShellPipeWriter {
  pub fn stdout() -> ShellPipeWriter {
    ShellPipeWriter::from_raw(os_pipe::dup_stdout().unwrap())
  }

  pub fn stderr() -> ShellPipeWriter {
    ShellPipeWriter::from_raw(os_pipe::dup_stderr().unwrap())
  }

  pub fn null() -> ShellPipeWriter {
    ShellPipeWriter::Null
  }

  pub fn from_raw(writer: os_pipe::PipeWriter) -> Self {
    Self::OsPipe(writer)
  }

  pub fn from_std(std_file: std::fs::File) -> Self {
    Self::StdFile(std_file)
  }

  pub fn into_stdio(self) -> std::process::Stdio {
    match self {
      Self::OsPipe(pipe) => pipe.into(),
      Self::StdFile(file) => file.into(),
      Self::Null => std::process::Stdio::null(),
    }
  }

  pub fn write(&mut self, bytes: &[u8]) -> Result<()> {
    match self {
      Self::OsPipe(pipe) => pipe.write_all(bytes)?,
      Self::StdFile(file) => file.write_all(bytes)?,
      Self::Null => {}
    }
    Ok(())
  }

  pub fn write_line(&mut self, line: &str) -> Result<()> {
    let bytes = format!("{}\n", line);
    self.write(bytes.as_bytes())
  }
}

/// Used to communicate between commands.
pub fn pipe() -> (ShellPipeReader, ShellPipeWriter) {
  let (reader, writer) = os_pipe::pipe().unwrap();
  (ShellPipeReader(reader), ShellPipeWriter::OsPipe(writer))
}
