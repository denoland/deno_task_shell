// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use anyhow::Result;

use crate::fs_util;
use crate::shell_types::EnvChange;
use crate::shell_types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub fn cd_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  match execute_cd(cwd, args) {
    Ok(new_dir) => ExecuteResult::Continue(0, vec![EnvChange::Cd(new_dir)]),
    Err(err) => {
      eprintln!("cd: {}", err);
      ExecuteResult::Continue(1, Vec::new())
    }
  }
}

fn execute_cd(cwd: &Path, args: Vec<String>) -> Result<PathBuf> {
  let path = parse_args(args)?;
  let new_dir = cwd.join(&path);
  let new_dir = fs_util::canonicalize_path(&new_dir)?;
  if !new_dir.is_dir() {
    bail!("{}: Not a directory", path)
  }
  Ok(new_dir)
}

fn parse_args(args: Vec<String>) -> Result<String> {
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

  if paths.len() > 1 {
    bail!("too many arguments")
  } else if paths.is_empty() {
    // not the case in actual cd, but it is most likely
    // an error if someone does this in deno task
    bail!("expected at least 1 argument")
  }

  Ok(paths.remove(0).to_string())
}

#[cfg(test)]
mod test {
  use std::fs;
  use tempfile::tempdir;

  use super::*;

  #[test]
  fn parses_args() {
    assert_eq!(parse_args(vec!["test".to_string()]).unwrap(), "test");
    assert_eq!(
      parse_args(vec!["a".to_string(), "b".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "too many arguments"
    );
    assert_eq!(
      parse_args(vec![]).err().unwrap().to_string(),
      "expected at least 1 argument"
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
  fn gets_new_cd() {
    let dir = tempdir().unwrap();

    // non-existent
    assert_eq!(
      execute_cd(dir.path(), vec!["non-existent".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "The system cannot find the file specified. (os error 2)"
    );

    // existent file
    fs::write(dir.path().join("file.txt"), "").unwrap();
    assert_eq!(
      execute_cd(dir.path(), vec!["file.txt".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "file.txt: Not a directory"
    );

    // existent dir
    let sub_dir_path = dir.path().join("sub_dir");
    fs::create_dir(&sub_dir_path).unwrap();
    assert_eq!(
      execute_cd(dir.path(), vec!["sub_dir".to_string()]).unwrap(),
      sub_dir_path
    );
  }
}
