// Copyright 2018-2024 the Deno authors. MIT license.

pub use commands::ExecutableCommand;
pub use commands::ExecuteCommandArgsContext;
pub use commands::ShellCommand;
pub use commands::ShellCommandContext;
pub use execute::execute;
pub use execute::execute_with_pipes;
pub use types::EnvChange;
pub use types::ExecuteResult;
pub use types::FutureExecuteResult;
pub use types::KillSignal;
pub use types::KillSignalDropGuard;
pub use types::ShellPipeReader;
pub use types::ShellPipeWriter;
pub use types::ShellState;
pub use types::SignalKind;
pub use types::pipe;

mod child_process_tracker;
mod command;
mod commands;
mod execute;
mod fs_util;
mod types;
pub mod which;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_builder;
