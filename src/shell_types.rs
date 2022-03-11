// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use futures::future::BoxFuture;
use tokio::task::JoinHandle;

use crate::fs_util;

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
    self
      .env_vars
      .get(name)
      .or_else(|| self.shell_vars.get(name))
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

#[derive(Debug, PartialEq)]
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

  pub fn with_stdout_text(
    mut stdout: ShellPipeWriter,
    text: String,
  ) -> ExecuteResult {
    let _ = stdout.write(&text.into_bytes());
    ExecuteResult::Continue(0, Vec::new(), Vec::new())
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

  pub fn into_raw(self) -> os_pipe::PipeReader {
    self.0
  }

  /// Write everything to the specified writer
  pub fn write_all(mut self, writer: &mut dyn Write) -> Result<()> {
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
    self.write_all(&mut sender.0)
  }
}

/// Writer side of a pipe.
///
/// Ensure that all of these are dropped when complete in order to
/// prevent deadlocks where the reader hangs waiting for a read.
pub struct ShellPipeWriter(os_pipe::PipeWriter);

impl Clone for ShellPipeWriter {
  fn clone(&self) -> Self {
    Self(self.0.try_clone().unwrap())
  }
}

impl ShellPipeWriter {
  pub fn stdout() -> ShellPipeWriter {
    ShellPipeWriter::from_raw(os_pipe::dup_stdout().unwrap())
  }

  pub fn stderr() -> ShellPipeWriter {
    ShellPipeWriter::from_raw(os_pipe::dup_stderr().unwrap())
  }

  pub fn from_raw(writer: os_pipe::PipeWriter) -> Self {
    Self(writer)
  }

  pub fn into_raw(self) -> os_pipe::PipeWriter {
    self.0
  }

  pub fn write(&mut self, bytes: &[u8]) -> Result<()> {
    Ok(self.0.write_all(bytes)?)
  }

  pub fn write_line(&mut self, line: &str) -> Result<()> {
    let bytes = format!("{}\n", line);
    self.write(bytes.as_bytes())
  }
}

/// Used to communicate between commands.
pub fn pipe() -> (ShellPipeReader, ShellPipeWriter) {
  let (reader, writer) = os_pipe::pipe().unwrap();
  (ShellPipeReader(reader), ShellPipeWriter(writer))
}
