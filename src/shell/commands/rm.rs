// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use std::io::ErrorKind;
use std::path::Path;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::execute_with_cancellation;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct RmCommand;

impl ShellCommand for RmCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        rm_command(context.state.cwd(), context.args, context.stderr),
        context.state.token()
      )
    }
    .boxed_local()
  }
}

async fn rm_command(
  cwd: &Path,
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_remove(cwd, args).await {
    Ok(()) => ExecuteResult::from_exit_code(0),
    Err(err) => {
      let _ = stderr.write_line(&format!("rm: {err}"));
      ExecuteResult::from_exit_code(1)
    }
  }
}

async fn execute_remove(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_args(args)?;
  for specified_path in &flags.paths {
    let path = cwd.join(specified_path);
    let result = if flags.recursive {
      if path.is_dir() {
        tokio::fs::remove_dir_all(&path).await
      } else {
        remove_file_or_dir(&path, &flags).await
      }
    } else {
      remove_file_or_dir(&path, &flags).await
    };
    if let Err(err) = result {
      if err.kind() != ErrorKind::NotFound || !flags.force {
        bail!("cannot remove '{}': {}", specified_path, err);
      }
    }
  }

  Ok(())
}

async fn remove_file_or_dir(
  path: &Path,
  flags: &RmFlags,
) -> std::io::Result<()> {
  if flags.dir && path.is_dir() {
    tokio::fs::remove_dir(path).await
  } else {
    tokio::fs::remove_file(path).await
  }
}

#[derive(Default, Debug, PartialEq)]
struct RmFlags {
  force: bool,
  recursive: bool,
  dir: bool,
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
      ArgKind::LongFlag("dir") | ArgKind::ShortFlag('d') => {
        result.dir = true;
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
        "--dir".to_string(),
        "a".to_string(),
        "b".to_string(),
      ])
      .unwrap(),
      RmFlags {
        recursive: true,
        dir: true,
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
        dir: false,
        paths: vec!["a".to_string(), "b".to_string()],
      }
    );
    assert_eq!(
      parse_args(vec!["-d".to_string(), "a".to_string()]).unwrap(),
      RmFlags {
        recursive: false,
        force: false,
        dir: true,
        paths: vec!["a".to_string()],
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

  #[tokio::test]
  async fn test_dir() {
    let dir = tempdir().unwrap();
    let existent_file = dir.path().join("existent.txt");
    let existent_dir = dir.path().join("sub_dir");
    let existent_dir_files = dir.path().join("sub_dir_files");
    fs::write(&existent_file, "").unwrap();
    fs::create_dir(&existent_dir).unwrap();
    fs::create_dir(&existent_dir_files).unwrap();
    fs::write(existent_dir_files.join("file.txt"), "").unwrap();

    assert!(execute_remove(
      dir.path(),
      vec!["-d".to_string(), "existent.txt".to_string()],
    )
    .await
    .is_ok());

    assert!(execute_remove(
      dir.path(),
      vec!["-d".to_string(), "sub_dir".to_string()],
    )
    .await
    .is_ok());
    assert!(!existent_dir.exists());

    let result = execute_remove(
      dir.path(),
      vec!["-d".to_string(), "sub_dir_files".to_string()],
    )
    .await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!(
        "cannot remove 'sub_dir_files': {}",
        directory_not_empty_text()
      ),
    );
    assert!(existent_dir_files.exists());
  }

  fn no_such_file_error_text() -> &'static str {
    if cfg!(windows) {
      "The system cannot find the file specified. (os error 2)"
    } else {
      "No such file or directory (os error 2)"
    }
  }

  fn directory_not_empty_text() -> &'static str {
    if cfg!(windows) {
      "The directory is not empty. (os error 145)"
    } else if cfg!(target_os = "macos") {
      "Directory not empty (os error 66)"
    } else {
      "Directory not empty (os error 39)"
    }
  }
}
