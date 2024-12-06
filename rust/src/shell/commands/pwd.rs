// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::Context;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use std::path::Path;

use crate::shell::fs_util;
use crate::shell::types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct PwdCommand;

impl ShellCommand for PwdCommand {
  fn execute(
    &self,
    mut context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let result = match execute_pwd(context.state.cwd(), context.args) {
      Ok(output) => {
        let _ = context.stdout.write_line(&output);
        ExecuteResult::from_exit_code(0)
      }
      Err(err) => {
        let _ = context.stderr.write_line(&format!("pwd: {err}"));
        ExecuteResult::from_exit_code(1)
      }
    };
    Box::pin(futures::future::ready(result))
  }
}

fn execute_pwd(cwd: &Path, args: Vec<String>) -> Result<String> {
  let flags = parse_args(args)?;
  let cwd = if flags.logical {
    fs_util::canonicalize_path(cwd)
      .with_context(|| format!("error canonicalizing: {}", cwd.display()))?
  } else {
    cwd.to_path_buf()
  };
  Ok(cwd.display().to_string())
}

#[derive(Debug, PartialEq)]
struct PwdFlags {
  logical: bool,
}

fn parse_args(args: Vec<String>) -> Result<PwdFlags> {
  let mut logical = false;
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::ShortFlag('L') => {
        logical = true;
      }
      ArgKind::ShortFlag('P') => {
        // ignore, this is the default
      }
      ArgKind::Arg(_) => {
        // args are ignored by pwd
      }
      _ => arg.bail_unsupported()?,
    }
  }

  Ok(PwdFlags { logical })
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses_args() {
    assert_eq!(parse_args(vec![]).unwrap(), PwdFlags { logical: false });
    assert_eq!(
      parse_args(vec!["-P".to_string()]).unwrap(),
      PwdFlags { logical: false }
    );
    assert_eq!(
      parse_args(vec!["-L".to_string()]).unwrap(),
      PwdFlags { logical: true }
    );
    assert!(parse_args(vec!["test".to_string()]).is_ok());
    assert_eq!(
      parse_args(vec!["--flag".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: --flag"
    );
    assert_eq!(
      parse_args(vec!["-t".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: -t"
    );
  }
}
