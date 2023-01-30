use std::path::PathBuf;

use crate::shell::types::ShellState;
use crate::ExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;

/// Command that resolves the command name and
/// executes it in a separate process.
pub struct ExecutableCommand {
  command_name: String,
}

impl ExecutableCommand {
  pub fn new(command_name: String) -> Self {
    Self { command_name }
  }
}

impl ShellCommand for ExecutableCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let command_name = self.command_name.clone();
    async move {
      let mut stderr = context.stderr;
      let command_path =
        match resolve_command_path(&command_name, &context.state, || {
          Ok(std::env::current_exe()?)
        }) {
          Ok(command_path) => command_path,
          Err(err) => {
            stderr.write_line(&err.to_string()).unwrap();
            return ExecuteResult::Continue(1, Vec::new(), Vec::new());
          }
        };

      let mut sub_command = tokio::process::Command::new(&command_path);
      let child = sub_command
        .current_dir(context.state.cwd())
        .args(&context.args)
        .env_clear()
        .envs(context.state.env_vars())
        .stdout(context.stdout.into_stdio())
        .stdin(context.stdin.into_stdio())
        .stderr(stderr.clone().into_stdio())
        .spawn();

      let mut child = match child {
        Ok(child) => child,
        Err(err) => {
          stderr
            .write_line(&format!("Error launching '{command_name}': {err}"))
            .unwrap();
          return ExecuteResult::Continue(1, Vec::new(), Vec::new());
        }
      };

      // avoid deadlock since this is holding onto the pipes
      drop(sub_command);

      tokio::select! {
        result = child.wait() => match result {
          Ok(status) => ExecuteResult::Continue(
            status.code().unwrap_or(1),
            Vec::new(),
            Vec::new(),
          ),
          Err(err) => {
            stderr.write_line(&format!("{err}")).unwrap();
            ExecuteResult::Continue(1, Vec::new(), Vec::new())
          }
        },
        _ = context.state.token().cancelled() => {
          let _ = child.kill().await;
          ExecuteResult::for_cancellation()
        }
      }
    }
    .boxed_local()
  }
}

fn resolve_command_path(
  command_name: &str,
  state: &ShellState,
  current_exe: impl FnOnce() -> Result<PathBuf>,
) -> Result<PathBuf> {
  if command_name.is_empty() {
    bail!("command name was empty");
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
      if file_stem.map(|s| s.to_string()) == Some("deno".to_string()) {
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
    return Ok(state.cwd().join(command_name));
  }

  // now search based on the current environment state
  let mut search_dirs = vec![state.cwd().clone()];
  if let Some(path) = state.get_var("PATH") {
    for folder in path.split(if cfg!(windows) { ';' } else { ':' }) {
      search_dirs.push(PathBuf::from(folder));
    }
  }
  let path_exts = if cfg!(windows) {
    let uc_command_name = command_name.to_uppercase();
    let path_ext = state
      .get_var("PATHEXT")
      .map(|s| s.as_str())
      .unwrap_or(".EXE;.CMD;.BAT;.COM");
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

  bail!("{}: command not found", command_name)
}

#[cfg(test)]
mod local_test {
  use super::*;

  #[test]
  fn should_resolve_current_exe_path_for_deno() {
    let state = ShellState::new(
      Default::default(),
      &std::env::current_dir().unwrap(),
      Default::default(),
    );
    let path =
      resolve_command_path("deno", &state, || Ok(PathBuf::from("/bin/deno")))
        .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno"));

    let path = resolve_command_path("deno", &state, || {
      Ok(PathBuf::from("/bin/deno.exe"))
    })
    .unwrap();
    assert_eq!(path, PathBuf::from("/bin/deno.exe"));
  }
}
