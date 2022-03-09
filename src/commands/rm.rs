// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;
use std::io::ErrorKind;
use std::path::Path;

use crate::shell_types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub async fn rm_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  match execute_remove(cwd, args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new()),
    Err(err) => {
      eprintln!("rm: {}", err);
      ExecuteResult::Continue(1, Vec::new())
    }
  }
}

async fn execute_remove(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_args(args)?;
  for specified_path in &flags.paths {
    let path = cwd.join(&specified_path);
    if flags.recursive {
      if path.is_dir() {
        if let Err(err) = tokio::fs::remove_dir_all(&path).await {
          if err.kind() != ErrorKind::NotFound || !flags.force {
            bail!("cannot remove '{}': {}", specified_path, err);
          }
        }
      } else {
        remove_file(&path, specified_path, &flags).await?;
      }
    } else {
      remove_file(&path, specified_path, &flags).await?;
    }
  }
  Ok(())
}

async fn remove_file(
  path: &Path,
  specified_path: &str,
  flags: &RmFlags,
) -> Result<()> {
  if let Err(err) = tokio::fs::remove_file(path).await {
    if err.kind() != ErrorKind::NotFound || !flags.force {
      bail!("cannot remove '{}': {}", specified_path, err);
    }
  }
  Ok(())
}

#[derive(Default, Debug, PartialEq)]
struct RmFlags {
  force: bool,
  recursive: bool,
  paths: Vec<String>,
}

fn parse_args(args: Vec<String>) -> Result<RmFlags> {
  let mut result = RmFlags::default();

  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::LongFlag("recursive")
      | ArgKind::ShortFlag('r')
      | ArgKind::ShortFlag('R') => {
        result.recursive = true;
      }
      ArgKind::LongFlag("force") | ArgKind::ShortFlag('f') => {
        result.force = true;
      }
      ArgKind::Arg(path) => {
        result.paths.push(path.to_string());
      }
      ArgKind::LongFlag(_) | ArgKind::ShortFlag(_) => arg.bail_unsupported()?,
    }
  }

  if result.paths.is_empty() {
    bail!("missing operand");
  }

  Ok(result)
}

#[cfg(test)]
mod test {
  use tempfile::tempdir;

  use super::*;
  use std::fs;

  #[test]
  fn parses_args() {
    assert_eq!(
      parse_args(vec![
        "--recursive".to_string(),
        "a".to_string(),
        "b".to_string(),
      ])
      .unwrap(),
      RmFlags {
        recursive: true,
        paths: vec!["a".to_string(), "b".to_string()],
        ..Default::default()
      }
    );
    assert_eq!(
      parse_args(vec!["-rf".to_string(), "a".to_string(), "b".to_string(),])
        .unwrap(),
      RmFlags {
        recursive: true,
        force: true,
        paths: vec!["a".to_string(), "b".to_string()],
      }
    );
    assert_eq!(
      parse_args(vec!["--recursive".to_string(), "-f".to_string(),])
        .err()
        .unwrap()
        .to_string(),
      "missing operand",
    );
    assert_eq!(
      parse_args(vec![
        "--recursive".to_string(),
        "-u".to_string(),
        "a".to_string(),
      ])
      .err()
      .unwrap()
      .to_string(),
      "unsupported flag: -u",
    );
    assert_eq!(
      parse_args(vec![
        "--recursive".to_string(),
        "--random-flag".to_string(),
        "a".to_string(),
      ])
      .err()
      .unwrap()
      .to_string(),
      "unsupported flag: --random-flag",
    );
  }

  #[tokio::test]
  async fn test_force() {
    let dir = tempdir().unwrap();
    let existent_file = dir.path().join("existent.txt");
    fs::write(&existent_file, "").unwrap();

    execute_remove(
      dir.path(),
      vec!["-f".to_string(), "non_existent.txt".to_string()],
    )
    .await
    .unwrap();

    let result =
      execute_remove(dir.path(), vec!["non_existent.txt".to_string()]).await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!(
        "cannot remove 'non_existent.txt': {}",
        no_such_file_error_text()
      )
    );

    assert!(existent_file.exists());
    execute_remove(dir.path(), vec!["existent.txt".to_string()])
      .await
      .unwrap();
    assert!(!existent_file.exists());
  }

  #[tokio::test]
  async fn test_recursive() {
    let dir = tempdir().unwrap();
    let existent_file = dir.path().join("existent.txt");
    fs::write(&existent_file, "").unwrap();

    let result = execute_remove(
      dir.path(),
      vec!["-r".to_string(), "non_existent.txt".to_string()],
    )
    .await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!(
        "cannot remove 'non_existent.txt': {}",
        no_such_file_error_text()
      )
    );

    // test on a file
    assert!(existent_file.exists());
    execute_remove(
      dir.path(),
      vec!["-r".to_string(), "existent.txt".to_string()],
    )
    .await
    .unwrap();
    assert!(!existent_file.exists());

    // test on a directory
    let sub_dir = dir.path().join("folder").join("sub");
    fs::create_dir_all(&sub_dir).unwrap();
    let sub_file = sub_dir.join("file.txt");
    fs::write(&sub_file, "test").unwrap();
    assert!(sub_file.exists());
    execute_remove(dir.path(), vec!["-r".to_string(), "folder".to_string()])
      .await
      .unwrap();
    assert!(!sub_file.exists());

    let result =
      execute_remove(dir.path(), vec!["-r".to_string(), "folder".to_string()])
        .await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!("cannot remove 'folder': {}", no_such_file_error_text())
    );
    execute_remove(dir.path(), vec!["-rf".to_string(), "folder".to_string()])
      .await
      .unwrap();
  }

  fn no_such_file_error_text() -> &'static str {
    if cfg!(windows) {
      "The system cannot find the file specified. (os error 2)"
    } else {
      "No such file or directory (os error 2)"
    }
  }
}