// Copyright 2018-2024 the Deno authors. MIT license.

use std::path::PathBuf;

use crate::ExecuteResult;
use crate::FutureExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use futures::FutureExt;
use std::sync::Arc;
use tokio::sync::Mutex;

/// Command that resolves the command name and
/// executes it in a separate process.
pub struct ExecutableCommand {
  display_name: String,
  command_path: PathBuf,
}

impl ExecutableCommand {
  pub fn new(display_name: String, command_path: PathBuf) -> Self {
    Self {
      display_name,
      command_path,
    }
  }
}

impl ShellCommand for ExecutableCommand {
  fn execute(&self, context: ShellCommandContext) -> FutureExecuteResult {
    let display_name = self.display_name.clone();
    let command_name = self.command_path.clone();
    async move {
      let mut stderr = context.stderr;
      let mut sub_command = tokio::process::Command::new(&command_name);
      let child = sub_command
        .current_dir(context.state.cwd())
        .args(context.args)
        .env_clear()
        .envs(context.state.env_vars())
        .stdout(context.stdout.into_stdio())
        .stdin(context.stdin.into_stdio())
        .stderr(stderr.clone().into_stdio())
        .spawn();

      let child = match child {
        Ok(child) => Arc::new(Mutex::new(child)),
        Err(err) => {
          let _ = stderr.write_line(&format!(
            "Error launching '{}': {}",
            display_name, err
          ));
          return ExecuteResult::Continue(1, Vec::new(), Vec::new());
        }
      };

      // avoid deadlock since this is holding onto the pipes
      drop(sub_command);

      let child_clone = Arc::clone(&child);

      tokio::spawn(async move {
        tokio::signal::ctrl_c().await.unwrap();
        let mut child = child_clone.lock().await;
        if let Ok(None) = child.try_wait() {
          let _ = child.kill().await;
        }
      });

      let mut child_locked = child.lock().await;
      tokio::select! {
          result = child_locked.wait() => match result {
              Ok(status) => ExecuteResult::Continue(
                  status.code().unwrap_or(1),
                  Vec::new(),
                  Vec::new(),
              ),
              Err(err) => {
                  let _ = stderr.write_line(&format!("{}", err));
                  ExecuteResult::Continue(1, Vec::new(), Vec::new())
              }
          },
          _ = context.state.token().cancelled() => {
              let _ = child_locked.kill().await;
              ExecuteResult::for_cancellation()
          }
      }
    }
    .boxed_local()
  }
}
