// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::Result;
use futures::future::LocalBoxFuture;
use std::fs::File;
use std::io::Read;

use crate::shell::types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct CatCommand;

impl ShellCommand for CatCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let mut stderr = context.stderr.clone();
    let result = match execute_cat(context) {
      Ok(result) => result,
      Err(err) => {
        let _ = stderr.write_line(&format!("cat: {err}"));
        ExecuteResult::from_exit_code(1)
      }
    };
    Box::pin(futures::future::ready(result))
  }
}

fn execute_cat(mut context: ShellCommandContext) -> Result<ExecuteResult> {
  let flags = parse_args(context.args)?;
  let mut exit_code = 0;
  let mut buf = vec![0; 1024];
  for path in flags.paths {
    if path == "-" {
      context
        .stdin
        .clone()
        .pipe_to_sender(context.stdout.clone())?;
    } else {
      // buffered to prevent reading an entire file
      // in memory
      match File::open(context.state.cwd().join(&path)) {
        Ok(mut file) => loop {
          if context.state.token().is_cancelled() {
            return Ok(ExecuteResult::for_cancellation());
          }

          let size = file.read(&mut buf)?;
          if size == 0 {
            break;
          } else {
            context.stdout.write_all(&buf[..size])?;
          }
        },
        Err(err) => {
          context.stderr.write_line(&format!("cat: {path}: {err}"))?;
          exit_code = 1;
        }
      }
    }
  }

  Ok(ExecuteResult::from_exit_code(exit_code))
}

#[derive(Debug, PartialEq)]
struct CatFlags {
  paths: Vec<String>,
}

fn parse_args(args: Vec<String>) -> Result<CatFlags> {
  let mut paths = Vec::new();
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::Arg(file_name) => {
        paths.push(file_name.to_string());
      }
      // for now, we don't support any arguments
      _ => arg.bail_unsupported()?,
    }
  }

  if paths.is_empty() {
    paths.push("-".to_string());
  }

  Ok(CatFlags { paths })
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses_args() {
    assert_eq!(
      parse_args(vec![]).unwrap(),
      CatFlags {
        paths: vec!["-".to_string()]
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string()]).unwrap(),
      CatFlags {
        paths: vec!["path".to_string()]
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string(), "-".to_string()]).unwrap(),
      CatFlags {
        paths: vec!["path".to_string(), "-".to_string()]
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string(), "other-path".to_string()]).unwrap(),
      CatFlags {
        paths: vec!["path".to_string(), "other-path".to_string()]
      }
    );
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
