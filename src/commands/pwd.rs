// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;
use std::path::Path;

use crate::shell_types::ExecuteResult;
use crate::shell_types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub fn pwd_command(
  cwd: &Path,
  args: Vec<String>,
  mut stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match parse_args(args) {
    Ok(()) => {
      let _ = stdout.write_line(&cwd.display().to_string());
      ExecuteResult::from_exit_code(0)
    }
    Err(err) => {
      let _ = stderr.write_line(&format!("pwd: {}", err));
      ExecuteResult::from_exit_code(1)
    }
  }
}

fn parse_args(args: Vec<String>) -> Result<()> {
  if let Some(arg) = parse_arg_kinds(&args).into_iter().next() {
    match arg {
      ArgKind::LongFlag(flag) => {
        bail!("flags are currently not supported: --{}", flag)
      }
      ArgKind::ShortFlag(flag) => {
        bail!("flags are currently not supported: -{}", flag)
      }
      ArgKind::Arg(arg) => {
        bail!("args are currently not supported: {}", arg)
      }
    }
  }

  Ok(())
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses_args() {
    assert!(parse_args(vec![]).is_ok());
    assert_eq!(
      parse_args(vec!["test".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "args are currently not supported: test"
    );
    assert_eq!(
      parse_args(vec!["--flag".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "flags are currently not supported: --flag"
    );
    assert_eq!(
      parse_args(vec!["-t".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "flags are currently not supported: -t"
    );
  }
}
