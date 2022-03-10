// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use futures::future::BoxFuture;
use futures::Future;
use futures::FutureExt;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
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
  /// The current shell or subshell's stdout sender.
  /// This is used by async commands in order to avoid
  /// other steps waiting on the async commands to finish
  /// their output.
  shell_stdout_tx: ShellPipeSender,
}

impl ShellState {
  pub fn new(
    env_vars: HashMap<String, String>,
    cwd: &Path,
    shell_stdout_tx: ShellPipeSender,
  ) -> Self {
    let mut result = Self {
      env_vars: Default::default(),
      shell_vars: Default::default(),
      cwd: PathBuf::new(),
      shell_stdout_tx,
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

  pub fn shell_stdout_tx(&self) -> ShellPipeSender {
    self.shell_stdout_tx.clone()
  }

  pub fn set_shell_stdout_tx(&mut self, tx: ShellPipeSender) {
    self.shell_stdout_tx = tx;
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
}

pub struct SpawnedStep {
  pub stdout: ShellPipe,
  pub task: BoxFuture<'static, ExecuteResult>,
}

impl SpawnedStep {
  pub fn from_exit_code(exit_code: i32) -> Self {
    Self::from_result(ExecuteResult::Continue(
      exit_code,
      Vec::new(),
      Vec::new(),
    ))
  }

  pub fn from_result(execute_result: ExecuteResult) -> Self {
    let (tx, stdout) = ShellPipe::channel();
    Self {
      stdout,
      task: async move {
        drop(tx); // close stdout
        execute_result
      }
      .boxed(),
    }
  }

  pub fn with_stdout_text(text: String) -> Self {
    let (tx, stdout) = ShellPipe::channel();
    Self {
      stdout,
      task: async move {
        let _ = tx.send(text.into_bytes());
        drop(tx); // close stdout
        ExecuteResult::Continue(0, Vec::new(), Vec::new())
      }
      .boxed(),
    }
  }

  pub fn from_fn(
    func: impl FnOnce() -> ExecuteResult + Sync + Send + 'static,
  ) -> Self {
    Self::from_future(async move { func() })
  }

  pub fn from_future(
    future: impl Future<Output = ExecuteResult> + Send + 'static,
  ) -> Self {
    let (tx, stdout) = ShellPipe::channel();
    SpawnedStep {
      stdout,
      task: async move {
        let result = future.await;
        drop(tx); // close stdout
        result
      }
      .boxed(),
    }
  }

  pub async fn wait_with_stdout<T>(
    self,
    action: impl FnOnce(ShellPipe) -> T,
  ) -> ExecuteResult
  where
    T: Future<Output = ()> + Send + 'static,
  {
    let output_task = tokio::task::spawn(action(self.stdout));
    let result = self.task.await;
    output_task.await.unwrap();
    result
  }
}

pub type ShellPipeReceiver = UnboundedReceiver<Vec<u8>>;
pub type ShellPipeSender = UnboundedSender<Vec<u8>>;

/// Used to communicate between commands.
pub enum ShellPipe {
  /// Pull messages from stdin.
  InheritStdin,
  /// Receives pushed messages from a channel.
  Channel(ShellPipeReceiver),
}

impl ShellPipe {
  pub fn channel() -> (ShellPipeSender, ShellPipe) {
    let (data_tx, data_rx) = tokio::sync::mpsc::unbounded_channel();
    (data_tx, ShellPipe::Channel(data_rx))
  }

  /// Unwraps the pipe to a channel.
  pub fn unwrap_channel(self) -> ShellPipeReceiver {
    match self {
      ShellPipe::InheritStdin => unreachable!(),
      ShellPipe::Channel(rx) => rx,
    }
  }

  /// Write everything to the specified writer
  pub async fn write_all(
    self,
    mut writer: Box<dyn AsyncWrite + std::marker::Unpin + Send + Sync>,
  ) -> Result<()> {
    let mut rx = self.unwrap_channel();
    while let Some(data) = rx.recv().await {
      writer.write(&data).await?;
    }
    Ok(())
  }

  /// Pipes this pipe to the specified writer.
  pub async fn pipe_to_writer(
    self,
    writer: Box<dyn AsyncWrite + Unpin + Send + Sync>,
  ) {
    let _ = self.write_all(writer).await;
  }

  /// Pipes this pipe to the specified sender.
  pub async fn pipe_to_sender(self, sender: ShellPipeSender) {
    match self {
      ShellPipe::InheritStdin => unreachable!(),
      ShellPipe::Channel(mut rx) => {
        while let Some(data) = rx.recv().await {
          if sender.send(data).is_err() {
            break;
          }
        }
      }
    }
  }
}
