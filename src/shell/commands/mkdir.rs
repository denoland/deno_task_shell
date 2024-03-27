// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;
use std::path::Path;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::execute_with_cancellation;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct MkdirCommand;

impl ShellCommand for MkdirCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        mkdir_command(context.state.cwd(), context.args, context.stderr),
        context.state.token()
      )
    }
    .boxed_local()
  }
}

async fn mkdir_command(
  cwd: &Path,
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_mkdir(cwd, args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new(), Vec::new()),
    Err(err) => {
      let _ = stderr.write_line(&format!("mkdir: {err}"));
      ExecuteResult::Continue(1, Vec::new(), Vec::new())
    }
  }
}

async fn execute_mkdir(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_args(args)?;
  for specified_path in &flags.paths {
    let path = cwd.join(specified_path);
    if path.is_file() || !flags.parents && path.is_dir() {
      bail!("cannot create directory '{}': File exists", specified_path);
    }
    if flags.parents {
      if let Err(err) = tokio::fs::create_dir_all(&path).await {
        bail!("cannot create directory '{}': {}", specified_path, err);
      }
    } else if let Err(err) = tokio::fs::create_dir(&path).await {
      bail!("cannot create directory '{}': {}", specified_path, err);
    }
  }
  Ok(())
}

#[derive(Default, Debug, PartialEq)]
struct MkdirFlags {
  parents: bool,
  paths: Vec<String>,
}

fn parse_args(args: Vec<String>) -> Result<MkdirFlags> {
  let mut result = MkdirFlags::default();

  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::LongFlag("parents") | ArgKind::ShortFlag('p') => {
        result.parents = true;
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
        "--parents".to_string(),
        "a".to_string(),
        "b".to_string(),
      ])
      .unwrap(),
      MkdirFlags {
        parents: true,
        paths: vec!["a".to_string(), "b".to_string()],
      }
    );
    assert_eq!(
      parse_args(vec!["-p".to_string(), "a".to_string(), "b".to_string(),])
        .unwrap(),
      MkdirFlags {
        parents: true,
        paths: vec!["a".to_string(), "b".to_string()],
      }
    );
    assert_eq!(
      parse_args(vec!["--parents".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "missing operand",
    );
    assert_eq!(
      parse_args(vec![
        "--parents".to_string(),
        "-p".to_string(),
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
        "--parents".to_string(),
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
  async fn test_creates() {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join("file.txt");
    let sub_dir_path = dir.path().join("folder");
    fs::write(&file_path, "").unwrap();
    fs::create_dir(sub_dir_path).unwrap();

    assert_eq!(
      execute_mkdir(dir.path(), vec!["file.txt".to_string()],)
        .await
        .err()
        .unwrap()
        .to_string(),
      "cannot create directory 'file.txt': File exists"
    );

    assert_eq!(execute_mkdir(
      dir.path(),
      vec!["-p".to_string(), "file.txt".to_string()],
    )
    .await
    .err()
    .unwrap().to_string(), "cannot create directory 'file.txt': File exists");

    assert_eq!(
      execute_mkdir(dir.path(), vec!["folder".to_string()],)
        .await
        .err()
        .unwrap()
        .to_string(),
      "cannot create directory 'folder': File exists"
    );

    // should work because of -p
    execute_mkdir(dir.path(), vec!["-p".to_string(), "folder".to_string()])
      .await
      .unwrap();

    execute_mkdir(dir.path(), vec!["other".to_string()])
      .await
      .unwrap();
    assert!(dir.path().join("other").exists());

    // sub folder
    assert_eq!(
      execute_mkdir(dir.path(), vec!["sub/folder".to_string()],)
        .await
        .err()
        .unwrap()
        .to_string(),
      format!(
        "cannot create directory 'sub/folder': {}",
        no_such_file_error_text()
      )
    );

    execute_mkdir(dir.path(), vec!["-p".to_string(), "sub/folder".to_string()])
      .await
      .unwrap();
    assert!(dir.path().join("sub").join("folder").exists());
  }

  fn no_such_file_error_text() -> &'static str {
    if cfg!(windows) {
      "The system cannot find the path specified. (os error 3)"
    } else {
      "No such file or directory (os error 2)"
    }
  }
}
