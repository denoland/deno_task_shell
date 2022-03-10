// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use anyhow::Result;

use crate::environment::Environment;
use crate::shell_types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub async fn cp_command(
  cwd: &Path,
  args: Vec<String>,
  environment: impl Environment,
) -> ExecuteResult {
  match execute_cp(cwd, args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new(), Vec::new()),
    Err(err) => {
      environment.eprintln(&format!("cp: {}", err));
      ExecuteResult::Continue(1, Vec::new(), Vec::new())
    }
  }
}

async fn execute_cp(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_args(cwd, args)?;
  for (from, to) in flags.operations {
    if let Err(err) = tokio::fs::copy(&from, &to).await {
      bail!(
        "could not copy {} to {}: {}",
        from.display(),
        to.display(),
        err
      );
    }
  }
  Ok(())
}

pub async fn mv_command(
  cwd: &Path,
  args: Vec<String>,
  environment: impl Environment,
) -> ExecuteResult {
  match execute_mv(cwd, args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new(), Vec::new()),
    Err(err) => {
      environment.eprintln(&format!("mv: {}", err));
      ExecuteResult::Continue(1, Vec::new(), Vec::new())
    }
  }
}

async fn execute_mv(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_args(cwd, args)?;
  for (from, to) in flags.operations {
    if let Err(err) = tokio::fs::rename(&from, &to).await {
      bail!(
        "could not move {} to {}: {}",
        from.display(),
        to.display(),
        err
      );
    }
  }
  Ok(())
}

struct CpMvFlags {
  operations: Vec<(PathBuf, PathBuf)>,
}

fn parse_args(cwd: &Path, args: Vec<String>) -> Result<CpMvFlags> {
  let mut paths = Vec::new();
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::Arg(arg) => {
        paths.push(arg);
      }
      _ => arg.bail_unsupported()?,
    }
  }
  if paths.is_empty() {
    bail!("missing file operand");
  } else if paths.len() == 1 {
    bail!("missing destination file operand after '{}'", paths[0]);
  }

  Ok(CpMvFlags {
    operations: get_copy_and_move_operations(cwd, paths)?,
  })
}

fn get_copy_and_move_operations(
  cwd: &Path,
  mut paths: Vec<&str>,
) -> Result<Vec<(PathBuf, PathBuf)>> {
  // copy and move share the same logic
  let specified_destination = paths.pop().unwrap();
  let destination = cwd.join(&specified_destination);
  let from_args = paths.into_iter().map(|a| cwd.join(a)).collect::<Vec<_>>();
  let mut operations = Vec::new();
  if from_args.len() > 1 {
    if !destination.is_dir() {
      bail!("target '{}' is not a directory", specified_destination);
    }
    for from in from_args {
      let to = destination.join(from.file_name().unwrap());
      operations.push((from, to));
    }
  } else {
    let to = if destination.is_dir() {
      destination.join(from_args[0].file_name().unwrap())
    } else {
      destination
    };
    operations.push((from_args[0].clone(), to));
  }
  Ok(operations)
}

#[cfg(test)]
mod test {
  use tempfile::tempdir;

  use super::*;
  use std::fs;

  #[tokio::test]
  async fn should_copy() {
    let dir = tempdir().unwrap();
    let file1 = dir.path().join("file1.txt");
    let file2 = dir.path().join("file2.txt");
    fs::write(&file1, "test").unwrap();
    execute_cp(
      dir.path(),
      vec!["file1.txt".to_string(), "file2.txt".to_string()],
    )
    .await
    .unwrap();
    assert!(file1.exists());
    assert!(file2.exists());

    let dest_dir = dir.path().join("dest");
    fs::create_dir(&dest_dir).unwrap();
    execute_cp(
      dir.path(),
      vec![
        "file1.txt".to_string(),
        "file2.txt".to_string(),
        "dest".to_string(),
      ],
    )
    .await
    .unwrap();
    assert!(file1.exists());
    assert!(file2.exists());
    assert!(dest_dir.join("file1.txt").exists());
    assert!(dest_dir.join("file2.txt").exists());

    let new_file = dir.path().join("new.txt");
    fs::write(&new_file, "test").unwrap();
    execute_cp(dir.path(), vec!["new.txt".to_string(), "dest".to_string()])
      .await
      .unwrap();
    assert!(dest_dir.is_dir());
    assert!(new_file.exists());
    assert!(dest_dir.join("new.txt").exists());

    let result = execute_cp(
      dir.path(),
      vec![
        "file1.txt".to_string(),
        "file2.txt".to_string(),
        "non-existent".to_string(),
      ],
    )
    .await
    .err()
    .unwrap();
    assert_eq!(
      result.to_string(),
      "target 'non-existent' is not a directory"
    );

    let result = execute_cp(dir.path(), vec![]).await.err().unwrap();
    assert_eq!(result.to_string(), "missing file operand");

    let result = execute_cp(dir.path(), vec!["file1.txt".to_string()])
      .await
      .err()
      .unwrap();
    assert_eq!(
      result.to_string(),
      "missing destination file operand after 'file1.txt'"
    );
  }

  #[tokio::test]
  async fn should_move() {
    let dir = tempdir().unwrap();
    let file1 = dir.path().join("file1.txt");
    let file2 = dir.path().join("file2.txt");
    fs::write(&file1, "test").unwrap();
    execute_mv(
      dir.path(),
      vec!["file1.txt".to_string(), "file2.txt".to_string()],
    )
    .await
    .unwrap();
    assert!(!file1.exists());
    assert!(file2.exists());

    let dest_dir = dir.path().join("dest");
    fs::write(&file1, "test").unwrap(); // recreate
    fs::create_dir(&dest_dir).unwrap();
    execute_mv(
      dir.path(),
      vec![
        "file1.txt".to_string(),
        "file2.txt".to_string(),
        "dest".to_string(),
      ],
    )
    .await
    .unwrap();
    assert!(!file1.exists());
    assert!(!file2.exists());
    assert!(dest_dir.join("file1.txt").exists());
    assert!(dest_dir.join("file2.txt").exists());

    let new_file = dir.path().join("new.txt");
    fs::write(&new_file, "test").unwrap();
    execute_mv(dir.path(), vec!["new.txt".to_string(), "dest".to_string()])
      .await
      .unwrap();
    assert!(dest_dir.is_dir());
    assert!(!new_file.exists());
    assert!(dest_dir.join("new.txt").exists());

    let result = execute_mv(
      dir.path(),
      vec![
        "file1.txt".to_string(),
        "file2.txt".to_string(),
        "non-existent".to_string(),
      ],
    )
    .await
    .err()
    .unwrap();
    assert_eq!(
      result.to_string(),
      "target 'non-existent' is not a directory"
    );

    let result = execute_mv(dir.path(), vec![]).await.err().unwrap();
    assert_eq!(result.to_string(), "missing file operand");

    let result = execute_mv(dir.path(), vec!["file1.txt".to_string()])
      .await
      .err()
      .unwrap();
    assert_eq!(
      result.to_string(),
      "missing destination file operand after 'file1.txt'"
    );
  }
}
