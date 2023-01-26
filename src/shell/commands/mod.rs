// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

mod args;
mod cat;
mod cd;
mod cp_mv;
mod echo;
mod exit;
mod export;
mod mkdir;
mod pwd;
mod rm;
mod sleep;
mod xargs;

use std::collections::HashMap;
use std::path::PathBuf;

use futures::future::LocalBoxFuture;
use tokio_util::sync::CancellationToken;

use super::types::ExecuteResult;
use super::types::FutureExecuteResult;
use super::types::ShellPipeReader;
use super::types::ShellPipeWriter;

pub fn builtin_commands() -> HashMap<String, Box<dyn ShellCommand>> {
  HashMap::from([
    (
      "cat".to_string(),
      Box::new(cat::CatCommand) as Box<dyn ShellCommand>,
    ),
    (
      "cd".to_string(),
      Box::new(cd::CdCommand) as Box<dyn ShellCommand>,
    ),
    (
      "cp".to_string(),
      Box::new(cp_mv::CpCommand) as Box<dyn ShellCommand>,
    ),
    (
      "echo".to_string(),
      Box::new(echo::EchoCommand) as Box<dyn ShellCommand>,
    ),
    (
      "exit".to_string(),
      Box::new(exit::ExitCommand) as Box<dyn ShellCommand>,
    ),
    (
      "export".to_string(),
      Box::new(export::ExportCommand) as Box<dyn ShellCommand>,
    ),
    (
      "mkdir".to_string(),
      Box::new(mkdir::MkdirCommand) as Box<dyn ShellCommand>,
    ),
    (
      "mv".to_string(),
      Box::new(cp_mv::MvCommand) as Box<dyn ShellCommand>,
    ),
    (
      "pwd".to_string(),
      Box::new(pwd::PwdCommand) as Box<dyn ShellCommand>,
    ),
    (
      "rm".to_string(),
      Box::new(rm::RmCommand) as Box<dyn ShellCommand>,
    ),
    (
      "sleep".to_string(),
      Box::new(sleep::SleepCommand) as Box<dyn ShellCommand>,
    ),
    (
      "true".to_string(),
      Box::new(ExitCodeCommand(0)) as Box<dyn ShellCommand>,
    ),
    (
      "false".to_string(),
      Box::new(ExitCodeCommand(1)) as Box<dyn ShellCommand>,
    ),
    (
      "xargs".to_string(),
      Box::new(xargs::XargsCommand) as Box<dyn ShellCommand>,
    ),
  ])
}

pub struct ShellCommandContext {
  pub args: Vec<String>,
  pub cwd: PathBuf,
  pub stdin: ShellPipeReader,
  pub stdout: ShellPipeWriter,
  pub stderr: ShellPipeWriter,
  pub token: CancellationToken,
  pub execute_command_args: Box<
    dyn FnOnce(
      Vec<String>,
      ShellPipeReader,
      ShellPipeWriter,
      ShellPipeWriter,
    ) -> FutureExecuteResult,
  >,
}

pub trait ShellCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult>;
}

macro_rules! execute_with_cancellation {
  ($result_expr:expr, $token:expr) => {
    tokio::select! {
      result = $result_expr => {
        result
      },
      _ = $token.cancelled() => {
        ExecuteResult::for_cancellation()
      }
    }
  };
}

pub(super) use execute_with_cancellation;

struct ExitCodeCommand(i32);

impl ShellCommand for ExitCodeCommand {
  fn execute(
    &self,
    _context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    // ignores additional arguments
    Box::pin(futures::future::ready(ExecuteResult::from_exit_code(
      self.0,
    )))
  }
}
