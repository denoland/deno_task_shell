// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::Result;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeReader;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub fn cat_command(
  cwd: &Path,
  args: Vec<String>,
  stdin: ShellPipeReader,
  stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_cat(cwd, args, stdin, stdout, stderr.clone()) {
    Ok(exit_code) => ExecuteResult::from_exit_code(exit_code),
    Err(err) => {
      let _ = stderr.write_line(&format!("cat: {}", err));
      ExecuteResult::from_exit_code(1)
    }
  }
}

fn execute_cat(
  cwd: &Path,
  args: Vec<String>,
  stdin: ShellPipeReader,
  mut stdout: ShellPipeWriter,
  mut stderr: ShellPipeWriter,
) -> Result<i32> {
  let flags = parse_args(args)?;
  let mut exit_code = 0;
  let mut buf = vec![0; 1024];
  for path in flags.paths {
    if path == "-" {
      stdin.clone().pipe_to_sender(stdout.clone())?;
    } else {
      // buffered to prevent reading an entire file
      // in memory
      match File::open(cwd.join(&path)) {
        Ok(mut file) => loop {
          let size = file.read(&mut buf)?;
          if size == 0 {
            break;
          } else {
            stdout.write(&buf[..size])?;
          }
        },
        Err(err) => {
          stderr.write_line(&format!("cat: {}: {}", path, err))?;
          exit_code = 1;
        }
      }
    }
  }

  Ok(exit_code)
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
