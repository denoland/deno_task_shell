// Copyright 2018-2024 the Deno authors. MIT license.

use futures::future::LocalBoxFuture;

use crate::shell::types::EnvChange;
use crate::shell::types::ExecuteResult;

use super::ShellCommand;
use super::ShellCommandContext;

pub struct ExportCommand;

impl ShellCommand for ExportCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let mut changes = Vec::new();
    for arg in context.args {
      // ignore if it doesn't contain an equals
      if let Some(equals_index) = arg.find('=') {
        let arg_name = &arg[..equals_index];
        let arg_value = &arg[equals_index + 1..];
        changes.push(EnvChange::SetEnvVar(
          arg_name.to_string(),
          arg_value.to_string(),
        ));
      }
    }
    let result = ExecuteResult::Continue(0, changes, Vec::new());
    Box::pin(futures::future::ready(result))
  }
}
