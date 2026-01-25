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
  let args: Vec<String> = context
    .args
    .iter()
    .filter_map(|a| a.to_str().map(|s| s.to_string()))
    .collect();

  // no arguments - in bash this would list all shell variables
  // for now, just return success
  if args.is_empty() {
    return ExecuteResult::from_exit_code(0);
  }

  // set -o (list options in human-readable format)
  if args.len() == 1 && args[0] == "-o" {
    let opts = context.state.shell_options();
    let _ = context.stdout.write_line(&format!(
      "pipefail\t{}",
      if opts.contains(ShellOptions::PIPEFAIL) {
        "on"
      } else {
        "off"
      }
    ));
    return ExecuteResult::from_exit_code(0);
  }

  // set +o (output commands to recreate current settings)
  if args.len() == 1 && args[0] == "+o" {
    let opts = context.state.shell_options();
    let _ = context.stdout.write_line(&format!(
      "set {} pipefail",
      if opts.contains(ShellOptions::PIPEFAIL) {
        "-o"
      } else {
        "+o"
      }
    ));
    return ExecuteResult::from_exit_code(0);
  }

  // parse option changes: set -o opt1 -o opt2 +o opt3 ...
  let mut changes = Vec::new();
  let mut i = 0;
  while i < args.len() {
    let arg = &args[i];
    if (arg == "-o" || arg == "+o") && i + 1 < args.len() {
      let enable = arg == "-o";
      let option_name = &args[i + 1];
      if let Some(option) = parse_option_name(option_name) {
        changes.push(EnvChange::SetOption(option, enable));
        i += 2;
      } else {
        let _ = context
          .stderr
          .write_line(&format!("set: unknown option: {}", option_name));
        return ExecuteResult::from_exit_code(1);
      }
    } else {
      let _ = context
        .stderr
        .write_line(&format!("set: invalid option: {}", arg));
      return ExecuteResult::from_exit_code(1);
    }
  }

  ExecuteResult::Continue(0, changes, Vec::new())
}

fn parse_option_name(name: &str) -> Option<ShellOptions> {
  match name {
    "pipefail" => Some(ShellOptions::PIPEFAIL),
    _ => None,
  }
}
