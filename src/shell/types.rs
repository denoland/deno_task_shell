// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Arc;

use anyhow::Result;
use futures::future::LocalBoxFuture;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;

use crate::shell::fs_util;

use super::commands::builtin_commands;
use super::commands::ShellCommand;

#[derive(Clone)]
pub struct ShellState {
  /// Environment variables that should be passed down to sub commands
  /// and used when evaluating environment variables.
  env_vars: HashMap<String, String>,
  /// Variables that should be evaluated within the shell and
  /// not passed down to any sub commands.
  shell_vars: HashMap<String, String>,
  cwd: PathBuf,
  commands: Arc<HashMap<String, Rc<dyn ShellCommand>>>,
  /// Token to cancel execution.
  token: CancellationToken,
}

impl ShellState {
  pub fn new(
    env_vars: HashMap<String, String>,
    cwd: &Path,
    custom_commands: HashMap<String, Rc<dyn ShellCommand>>,
  ) -> Self {
    assert!(cwd.is_absolute());
    let mut commands = builtin_commands();
    commands.extend(custom_commands);
    let mut result = Self {
      env_vars: Default::default(),
      shell_vars: Default::default(),
      cwd: PathBuf::new(),
      commands: Arc::new(commands),
      token: CancellationToken::default(),
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
      EnvChange::UnsetVar(name) => {
        self.shell_vars.remove(name);
        self.env_vars.remove(name);
      }
      EnvChange::Cd(new_dir) => {
        self.set_cwd(new_dir);
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
      self.env_vars.insert(name, value.to_string());
    }
  }

  pub fn token(&self) -> &CancellationToken {
    &self.token
  }

  pub fn resolve_command(&self, name: &str) -> Option<Rc<dyn ShellCommand>> {
    // uses an Rc to allow resolving a command without borrowing from self
    self.commands.get(name).cloned()
  }

  pub fn with_child_token(&self) -> ShellState {
    let mut state = self.clone();
    state.token = self.token.child_token();
    state
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EnvChange {
  // `export ENV_VAR=VALUE`
  SetEnvVar(String, String),
  // `ENV_VAR=VALUE`
  SetShellVar(String, String),
  // `unset ENV_VAR`
  UnsetVar(String),
  Cd(PathBuf),
}

pub type FutureExecuteResult = LocalBoxFuture<'static, ExecuteResult>;

// https://unix.stackexchange.com/a/99117
// SIGINT (2) + 128
pub const CANCELLATION_EXIT_CODE: i32 = 130;

#[derive(Debug)]
pub enum ExecuteResult {
  Exit(i32, Vec<JoinHandle<i32>>),
  Continue(i32, Vec<EnvChange>, Vec<JoinHandle<i32>>),
}

impl ExecuteResult {
  pub fn for_cancellation() -> ExecuteResult {
    ExecuteResult::Exit(CANCELLATION_EXIT_CODE, Vec::new())
  }

  pub fn from_exit_code(exit_code: i32) -> ExecuteResult {
    ExecuteResult::Continue(exit_code, Vec::new(), Vec::new())
  }

  pub fn into_exit_code_and_handles(self) -> (i32, Vec<JoinHandle<i32>>) {
    match self {
      ExecuteResult::Exit(code, handles) => (code, handles),
      ExecuteResult::Continue(code, _, handles) => (code, handles),
    }
  }

  pub fn into_handles(self) -> Vec<JoinHandle<i32>> {
    self.into_exit_code_and_handles().1
  }
}

/// Reader side of a pipe.
pub struct ShellPipeReader(pub os_pipe::PipeReader);

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
  pub fn pipe_to(self, writer: &mut dyn Write) -> Result<()> {
    // don't bother flushing here because this won't ever be called
    // with a Rust wrapped stdout/stderr
    self.pipe_to_inner(writer, false)
  }

  fn pipe_to_with_flushing(self, writer: &mut dyn Write) -> Result<()> {
    self.pipe_to_inner(writer, true)
  }

  fn pipe_to_inner(
    mut self,
    writer: &mut dyn Write,
    flush: bool,
  ) -> Result<()> {
    loop {
      let mut buffer = [0; 512]; // todo: what is an appropriate buffer size?
      let size = self.0.read(&mut buffer)?;
      if size == 0 {
        break;
      }
      writer.write_all(&buffer[0..size])?;
      if flush {
        writer.flush()?;
      }
    }
    Ok(())
  }

  /// Pipes this pipe to the specified sender.
  pub fn pipe_to_sender(self, mut sender: ShellPipeWriter) -> Result<()> {
    match &mut sender {
      ShellPipeWriter::OsPipe(pipe) => self.pipe_to(pipe),
      ShellPipeWriter::StdFile(file) => self.pipe_to(file),
      // Don't lock stdout/stderr here because we want to release the lock
      // when reading from the sending pipe. Additionally, we want
      // to flush after every write because Rust's wrapper has an
      // internal buffer and Deno doesn't buffer stdout/stderr.
      ShellPipeWriter::Stdout => {
        self.pipe_to_with_flushing(&mut std::io::stdout())
      }
      ShellPipeWriter::Stderr => {
        self.pipe_to_with_flushing(&mut std::io::stderr())
      }
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
  // For stdout and stderr, instead of directly duplicating the raw pipes
  // and putting them in a ShellPipeWriter::OsPipe(...), we use Rust std's
  // stdout() and stderr() wrappers because it contains some code to solve
  // some encoding issues on Windows (ex. emojis). For more details, see
  // library/std/src/sys/windows/stdio.rs in Rust's source code.
  Stdout,
  Stderr,
  Null,
}

impl Clone for ShellPipeWriter {
  fn clone(&self) -> Self {
    match self {
      Self::OsPipe(pipe) => Self::OsPipe(pipe.try_clone().unwrap()),
      Self::StdFile(file) => Self::StdFile(file.try_clone().unwrap()),
      Self::Stdout => Self::Stdout,
      Self::Stderr => Self::Stderr,
      Self::Null => Self::Null,
    }
  }
}

impl ShellPipeWriter {
  pub fn stdout() -> Self {
    Self::Stdout
  }

  pub fn stderr() -> Self {
    Self::Stderr
  }

  pub fn null() -> Self {
    Self::Null
  }

  pub fn from_std(std_file: std::fs::File) -> Self {
    Self::StdFile(std_file)
  }

  pub fn into_stdio(self) -> std::process::Stdio {
    match self {
      Self::OsPipe(pipe) => pipe.into(),
      Self::StdFile(file) => file.into(),
      Self::Stdout => std::process::Stdio::inherit(),
      Self::Stderr => std::process::Stdio::inherit(),
      Self::Null => std::process::Stdio::null(),
    }
  }

  pub fn write_all(&mut self, bytes: &[u8]) -> Result<()> {
    match self {
      Self::OsPipe(pipe) => pipe.write_all(bytes)?,
      Self::StdFile(file) => file.write_all(bytes)?,
      // For both stdout & stderr, we want to flush after each
      // write in order to bypass Rust's internal buffer.
      Self::Stdout => {
        let mut stdout = std::io::stdout().lock();
        stdout.write_all(bytes)?;
        stdout.flush()?;
      }
      Self::Stderr => {
        let mut stderr = std::io::stderr().lock();
        stderr.write_all(bytes)?;
        stderr.flush()?;
      }
      Self::Null => {}
    }
    Ok(())
  }

  pub fn write_line(&mut self, line: &str) -> Result<()> {
    let bytes = format!("{line}\n");
    self.write_all(bytes.as_bytes())
  }
}
