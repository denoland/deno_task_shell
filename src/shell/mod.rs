// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

mod commands;
mod fs_util;

#[allow(clippy::module_inception)]
mod shell;
mod types;

pub use shell::*;

#[cfg(test)]
mod test;
#[cfg(test)]
mod test_builder;
