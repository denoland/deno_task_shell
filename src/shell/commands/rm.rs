// Copyright 2018-2025 the Deno authors. MIT license.

use anyhow::Result;
use anyhow::bail;
use futures::FutureExt;
use futures::future::LocalBoxFuture;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::io::ErrorKind;
use std::path::Path;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::ShellCommand;
use super::ShellCommandContext;
use super::args::ArgKind;
use super::args::parse_arg_kinds;
use super::execute_with_cancellation;

pub struct RmCommand;

impl ShellCommand for RmCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        rm_command(context.state.cwd(), &context.args, context.stderr),
        context.state.kill_signal()
      )
    }
    .boxed_local()
  }
}

async fn rm_command(
  cwd: &Path,
  args: &[OsString],
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

async fn execute_remove(cwd: &Path, args: &[OsString]) -> Result<()> {
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
    if let Err(err) = result
      && (err.kind() != ErrorKind::NotFound || !flags.force)
    {
      bail!(
        "cannot remove '{}': {}",
        specified_path.to_string_lossy(),
        err
      );
    }
  }

  Ok(())
}

async fn remove_file_or_dir(
  path: &Path,
  flags: &RmFlags<'_>,
) -> std::io::Result<()> {
  if flags.dir && path.is_dir() {
    tokio::fs::remove_dir(path).await
  } else {
    tokio::fs::remove_file(path).await
  }
}

#[derive(Default, Debug, PartialEq)]
struct RmFlags<'a> {
  force: bool,
  recursive: bool,
  dir: bool,
  paths: Vec<&'a OsStr>,
}

fn parse_args(args: &[OsString]) -> Result<RmFlags> {
  let mut result = RmFlags::default();

  for arg in parse_arg_kinds(args) {
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
        result.paths.push(path);
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
      parse_args(&[
        "--recursive".into(),
        "--dir".into(),
        "a".into(),
        "b".into(),
      ])
      .unwrap(),
      RmFlags {
        recursive: true,
        dir: true,
        paths: vec![OsStr::new("a"), OsStr::new("b")],
        ..Default::default()
      }
    );
    assert_eq!(
      parse_args(&["-rf".into(), "a".into(), "b".into(),]).unwrap(),
      RmFlags {
        recursive: true,
        force: true,
        dir: false,
        paths: vec![OsStr::new("a"), OsStr::new("b")],
      }
    );
    assert_eq!(
      parse_args(&["-d".into(), "a".into()]).unwrap(),
      RmFlags {
        recursive: false,
        force: false,
        dir: true,
        paths: vec![OsStr::new("a")],
      }
    );
    assert_eq!(
      parse_args(&["--recursive".into(), "-f".into(),])
        .err()
        .unwrap()
        .to_string(),
      "missing operand",
    );
    assert_eq!(
      parse_args(&["--recursive".into(), "-u".into(), "a".into(),])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: -u",
    );
    assert_eq!(
      parse_args(&["--recursive".into(), "--random-flag".into(), "a".into(),])
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

    execute_remove(dir.path(), &["-f".into(), "non_existent.txt".into()])
      .await
      .unwrap();

    let result = execute_remove(dir.path(), &["non_existent.txt".into()]).await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!(
        "cannot remove 'non_existent.txt': {}",
        no_such_file_error_text()
      )
    );

    assert!(existent_file.exists());
    execute_remove(dir.path(), &["existent.txt".into()])
      .await
      .unwrap();
    assert!(!existent_file.exists());
  }

  #[tokio::test]
  async fn test_recursive() {
    let dir = tempdir().unwrap();
    let existent_file = dir.path().join("existent.txt");
    fs::write(&existent_file, "").unwrap();

    let result =
      execute_remove(dir.path(), &["-r".into(), "non_existent.txt".into()])
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
    execute_remove(dir.path(), &["-r".into(), "existent.txt".into()])
      .await
      .unwrap();
    assert!(!existent_file.exists());

    // test on a directory
    let sub_dir = dir.path().join("folder").join("sub");
    fs::create_dir_all(&sub_dir).unwrap();
    let sub_file = sub_dir.join("file.txt");
    fs::write(&sub_file, "test").unwrap();
    assert!(sub_file.exists());
    execute_remove(dir.path(), &["-r".into(), "folder".into()])
      .await
      .unwrap();
    assert!(!sub_file.exists());

    let result =
      execute_remove(dir.path(), &["-r".into(), "folder".into()]).await;
    assert_eq!(
      result.err().unwrap().to_string(),
      format!("cannot remove 'folder': {}", no_such_file_error_text())
    );
    execute_remove(dir.path(), &["-rf".into(), "folder".into()])
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

    assert!(
      execute_remove(dir.path(), &["-d".into(), "existent.txt".into()],)
        .await
        .is_ok()
    );

    assert!(
      execute_remove(dir.path(), &["-d".into(), "sub_dir".into()],)
        .await
        .is_ok()
    );
    assert!(!existent_dir.exists());

    let result =
      execute_remove(dir.path(), &["-d".into(), "sub_dir_files".into()]).await;
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
