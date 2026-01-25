// Copyright 2018-2025 the Deno authors. MIT license.

use futures::future::LocalBoxFuture;

use crate::shell::types::EnvChange;
use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellOptions;

use super::ShellCommand;
use super::ShellCommandContext;

pub struct SetCommand;

impl ShellCommand for SetCommand {
  fn execute(
    &self,
    mut context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let result = execute_set(&mut context);
    Box::pin(futures::future::ready(result))
  }
}

fn execute_set(context: &mut ShellCommandContext) -> ExecuteResult {
  let mut changes = Vec::new();
  let args: Vec<String> = context
    .args
    .iter()
    .filter_map(|a| a.to_str().map(|s| s.to_string()))
    .collect();

  let mut i = 0;
  while i < args.len() {
    let arg = &args[i];
    if arg == "-o" || arg == "+o" {
      let enable = arg == "-o";
      if i + 1 < args.len() {
        let option_name = &args[i + 1];
        match option_name.as_str() {
          "pipefail" => {
            changes.push(EnvChange::SetOption(ShellOptions::PIPEFAIL, enable));
          }
          _ => {
            let _ = context
              .stderr
              .write_line(&format!("set: unknown option: {}", option_name));
            return ExecuteResult::from_exit_code(1);
          }
        }
        i += 2;
      } else {
        // No option name provided - in bash this would list options
        // For now, just return success
        i += 1;
      }
    } else {
      // unknown argument
      let _ = context
        .stderr
        .write_line(&format!("set: invalid option: {}", arg));
      return ExecuteResult::from_exit_code(1);
    }
  }

  ExecuteResult::Continue(0, changes, Vec::new())
}
