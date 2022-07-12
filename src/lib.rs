// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

pub mod parser;

#[cfg(feature = "shell")]
mod shell;

#[cfg(feature = "shell")]
pub use shell::*;
