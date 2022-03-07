// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;
use futures::future::BoxFuture;
use futures::FutureExt;
use tokio::io::AsyncWrite;
use tokio::io::AsyncWriteExt;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;

use crate::fs_util;

#[derive(Clone)]
pub struct EnvState {
  /// Environment variables that should be passed down to sub commands
  /// and used when evaluating environment variables.
  pub env_vars: HashMap<String, String>,
  /// Variables that should be evaluated within the shell and
  /// not passed down to any sub commands.
  pub shell_vars: HashMap<String, String>,
  cwd: PathBuf,
}

impl EnvState {
  pub fn new(env_vars: HashMap<String, String>, cwd: PathBuf) -> Self {
    let mut result = Self {
      env_vars,
      shell_vars: Default::default(),
      cwd,
    };
    result.sync_cwd_with_pwd();
    result
  }

  /// $PWD holds the current working directory, so we keep cwd and $PWD in sync
  fn sync_cwd_with_pwd(&mut self) {
    self
      .env_vars
      .insert("PWD".to_string(), self.cwd.display().to_string());
  }

  pub fn cwd(&self) -> &PathBuf {
    &self.cwd
  }

  pub fn set_cwd(&mut self, cwd: &Path) {
    self.cwd = cwd.to_path_buf();
    self.sync_cwd_with_pwd();
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
    if name == "PWD" {
      let cwd = PathBuf::from(value);
      if cwd.is_absolute() {
        if let Ok(cwd) = fs_util::canonicalize_path(&cwd) {
          self.set_cwd(&cwd);
        }
      }
    } else {
      self.shell_vars.remove(name);
      if value.is_empty() {
        self.env_vars.remove(name);
      } else {
        self.env_vars.insert(name.to_string(), value.to_string());
      }
    }
  }
}

pub enum EnvChange {
  // `export ENV_VAR=VALUE`
  SetEnvVar(String, String),
  // `ENV_VAR=VALUE`
  SetShellVar(String, String),
  Cd(PathBuf),
}

pub enum ExecuteResult {
  Exit,
  Continue(i32, Vec<EnvChange>),
}

pub struct ExecutedTask {
  pub stdout: ShellPipe,
  pub task: BoxFuture<'static, ExecuteResult>,
}

impl ExecutedTask {
  pub fn from_exit_code(exit_code: i32) -> Self {
    Self::from_result(ExecuteResult::Continue(exit_code, Vec::new()))
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
        ExecuteResult::Continue(0, Vec::new())
      }
      .boxed(),
    }
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

  /// Write everything to the specified writer
  pub async fn write_all(
    self,
    mut writer: impl AsyncWrite + std::marker::Unpin,
  ) -> Result<()> {
    match self {
      ShellPipe::InheritStdin => unreachable!(),
      ShellPipe::Channel(mut rx) => {
        while let Some(data) = rx.recv().await {
          writer.write(&data).await?;
        }
      }
    }
    Ok(())
  }

  /// Pipes this pipe to the current process' stdout.
  pub async fn pipe_to_stdout(self) {
    let _ = self.write_all(tokio::io::stdout()).await;
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

// #[cfg(test)]
// mod test {
//   use crate::parse_string_parts;

//   use super::*;

//   #[test]
//   fn evaluate_string_parts_test() {
//     run_evaluate_sp_test("$Test", &[], &[], "");
//     run_evaluate_sp_test("$Test", &[("Test", "value")], &[], "value");
//     run_evaluate_sp_test("$Test", &[], &[("Test", "value")], "value");
//     run_evaluate_sp_test("$A a", &[], &[("A", "value")], "value a");
//     run_evaluate_sp_test("\\$A a", &[], &[("A", "value")], "$A a");
//   }

//   fn run_evaluate_sp_test(
//     text: &str,
//     shell_vars: &[(&str, &str)],
//     env_vars: &[(&str, &str)],
//     expected: &str,
//   ) {
//     let (_, parts) = parse_string_parts(|_| true)(text).unwrap();
//     let state = EnvState {
//       shell_vars: shell_vars
//         .iter()
//         .map(|(k, v)| (k.to_string(), v.to_string()))
//         .collect(),
//       env_vars: env_vars
//         .iter()
//         .map(|(k, v)| (k.to_string(), v.to_string()))
//         .collect(),
//       cwd: PathBuf::from("/current_dir"),
//     };
//     assert_eq!(state.evaluate_string_parts(&parts), expected)
//   }
// }
