// Copyright 2018-2024 the Deno authors. MIT license.

use futures::future::LocalBoxFuture;

use crate::shell::types::ExecuteResult;

use super::ShellCommand;
use super::ShellCommandContext;

pub struct EchoCommand;

impl ShellCommand for EchoCommand {
  fn execute(
    &self,
    mut context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let _ = context.stdout.write_line(&context.args.join(" "));
    Box::pin(futures::future::ready(ExecuteResult::from_exit_code(0)))
  }
}
