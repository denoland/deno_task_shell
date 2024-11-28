// Copyright 2018-2024 the Deno authors. MIT license.

use std::path::PathBuf;

use crate::ExecuteResult;
use crate::FutureExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use futures::FutureExt;

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

      let mut child = match child {
        Ok(child) => child,
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

      loop {
        tokio::select! {
          result = child.wait() => match result {
            Ok(status) => return ExecuteResult::Continue(
              status.code().unwrap_or(1),
              Vec::new(),
              Vec::new(),
            ),
            Err(err) => {
              let _ = stderr.write_line(&format!("{}", err));
              return ExecuteResult::Continue(1, Vec::new(), Vec::new())
            }
          },
          signal = context.state.kill_signal().wait_any() => {
            if let Some(_id) = child.id() {
              #[cfg(unix)]
              kill(_id as i32, signal);

              if cfg!(not(unix)) {
                if signal.causes_abort() {
                  let _ = child.kill().await;
                  return ExecuteResult::Continue(signal.aborted_code(), Vec::new(), Vec::new());
                }
              }
            }
          }
        }
      }
    }
    .boxed_local()
  }
}

#[cfg(unix)]
pub fn kill(pid: i32, signal: crate::SignalKind) -> Option<()> {
  use nix::sys::signal::kill as unix_kill;
  use nix::sys::signal::Signal;
  use nix::unistd::Pid;
  let signo: i32 = signal.into();
  let sig = Signal::try_from(signo).ok()?;
  unix_kill(Pid::from_raw(pid), Some(sig)).ok()?;
  Some(())
}
