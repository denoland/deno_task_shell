// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use crate::shell_types::ShellPipeReceiver;
use crate::shell_types::ShellPipeSender;

pub trait Environment: Clone + Send + Sync + 'static {
  fn stdout(&self) -> ShellPipeSender;
  fn stdin(&self) -> ShellPipeReceiver;
  fn eprintln(&self, text: &str);
}

#[derive(Clone, Default)]
pub struct RealEnvironment {}

impl Environment for RealEnvironment {
  fn stdout(&self) -> ShellPipeSender {
    ShellPipeSender::from_raw(os_pipe::dup_stdout().unwrap())
  }

  fn stdin(&self) -> ShellPipeReceiver {
    ShellPipeReceiver::from_raw(os_pipe::dup_stdin().unwrap())
  }

  fn eprintln(&self, text: &str) {
    eprintln!("{}", text);
  }
}
