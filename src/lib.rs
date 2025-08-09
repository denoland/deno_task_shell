// Copyright 2018-2025 the Deno authors. MIT license.

#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]
#![deny(clippy::unused_async)]
#![allow(mismatched_lifetime_syntaxes)]

pub mod parser;

#[cfg(feature = "shell")]
mod shell;

#[cfg(feature = "shell")]
pub use shell::*;
