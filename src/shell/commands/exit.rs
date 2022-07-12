// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub fn exit_command(
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_exit(args) {
    Ok(code) => ExecuteResult::Exit(code, Vec::new()),
    Err(err) => {
      // even if parsing args fails `exit` always exits
      stderr.write_line(&format!("exit: {}", err)).unwrap();
      ExecuteResult::Exit(1, Vec::new())
    }
  }
}

fn execute_exit(args: Vec<String>) -> Result<i32> {
  let exit_code = parse_args(args)?;

  Ok(if exit_code < 0 {
    let code = -exit_code % 256;
    256 - code
  } else {
    exit_code % 256
  })
}

fn parse_args(args: Vec<String>) -> Result<i32> {
  let args = parse_arg_kinds(&args);
  let mut paths = Vec::new();
  for arg in args {
    match arg {
      ArgKind::Arg(arg) => {
        paths.push(arg);
      }
      _ => arg.bail_unsupported()?,
    }
  }

  match paths.len() {
    0 => Ok(1),
    1 => {
      let arg = paths.remove(0).to_string();
      match arg.parse::<i32>() {
        Ok(value) => Ok(value),
        Err(_) => bail!("numeric argument required."),
      }
    }
    _ => {
      bail!("too many arguments")
    }
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn parses_args() {
    assert_eq!(parse_args(vec![]).unwrap(), 1);
    assert_eq!(parse_args(vec!["5".to_string()]).unwrap(), 5);
    assert_eq!(
      parse_args(vec!["test".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "numeric argument required."
    );
    assert_eq!(
      parse_args(vec!["1".to_string(), "2".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "too many arguments"
    );
    assert_eq!(
      parse_args(vec!["-a".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: -a"
    );
    assert_eq!(
      parse_args(vec!["--a".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: --a"
    );
  }

  #[test]
  fn executes_exit() {
    assert_eq!(execute_exit(vec![]).unwrap(), 1);
    assert_eq!(execute_exit(vec!["0".to_string()]).unwrap(), 0);
    assert_eq!(execute_exit(vec!["255".to_string()]).unwrap(), 255);
    assert_eq!(execute_exit(vec!["256".to_string()]).unwrap(), 0);
    assert_eq!(execute_exit(vec!["257".to_string()]).unwrap(), 1);
    assert_eq!(execute_exit(vec!["-1".to_string()]).unwrap(), 255);
  }
}
