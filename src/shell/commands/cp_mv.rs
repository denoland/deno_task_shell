// Copyright 2018-2024 the Deno authors. MIT license.

use std::path::Path;
use std::path::PathBuf;

use anyhow::bail;
use anyhow::Context;
use anyhow::Result;
use futures::future::BoxFuture;
use futures::future::LocalBoxFuture;
use futures::FutureExt;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::execute_with_cancellation;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct CpCommand;

impl ShellCommand for CpCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        cp_command(context.state.cwd(), context.args, context.stderr),
        context.state.token()
      )
    }
    .boxed_local()
  }
}

async fn cp_command(
  cwd: &Path,
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_cp(cwd, args).await {
    Ok(()) => ExecuteResult::from_exit_code(0),
    Err(err) => {
      let _ = stderr.write_line(&format!("cp: {err}"));
      ExecuteResult::from_exit_code(1)
    }
  }
}

async fn execute_cp(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_cp_args(cwd, args)?;
  for (from, to) in &flags.operations {
    if let Err(err) = do_copy_operation(&flags, from, to).await {
      bail!(
        "could not copy {} to {}: {}",
        from.specified,
        to.specified,
        err
      );
    }
  }
  Ok(())
}

async fn do_copy_operation(
  flags: &CpFlags,
  from: &PathWithSpecified,
  to: &PathWithSpecified,
) -> Result<()> {
  // These are racy with the file system, but that's ok.
  // They only exists to give better error messages.
  if from.path.is_dir() {
    if flags.recursive {
      if to.path.exists() && to.path.is_file() {
        bail!("destination was a file");
      } else if to.path.is_symlink() {
        bail!("no support for copying to symlinks")
      } else if from.path.is_symlink() {
        bail!("no support for copying from symlinks")
      } else {
        copy_dir_recursively(from.path.clone(), to.path.clone()).await?;
      }
    } else {
      bail!("source was a directory; maybe specify -r")
    }
  } else {
    tokio::fs::copy(&from.path, &to.path).await?;
  }
  Ok(())
}

fn copy_dir_recursively(
  from: PathBuf,
  to: PathBuf,
) -> BoxFuture<'static, Result<()>> {
  // recursive, so box it
  async move {
    tokio::fs::create_dir_all(&to)
      .await
      .with_context(|| format!("Creating {}", to.display()))?;
    let mut read_dir = tokio::fs::read_dir(&from)
      .await
      .with_context(|| format!("Reading {}", from.display()))?;

    while let Some(entry) = read_dir.next_entry().await? {
      let file_type = entry.file_type().await?;
      let new_from = from.join(entry.file_name());
      let new_to = to.join(entry.file_name());

      if file_type.is_dir() {
        copy_dir_recursively(new_from.clone(), new_to.clone())
          .await
          .with_context(|| {
            format!("Dir {} to {}", new_from.display(), new_to.display())
          })?;
      } else if file_type.is_file() {
        tokio::fs::copy(&new_from, &new_to).await.with_context(|| {
          format!("Copying {} to {}", new_from.display(), new_to.display())
        })?;
      }
    }

    Ok(())
  }
  .boxed()
}

struct CpFlags {
  recursive: bool,
  operations: Vec<(PathWithSpecified, PathWithSpecified)>,
}

fn parse_cp_args(cwd: &Path, args: Vec<String>) -> Result<CpFlags> {
  let mut paths = Vec::new();
  let mut recursive = false;
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::Arg(arg) => {
        paths.push(arg);
      }
      ArgKind::LongFlag("recursive")
      | ArgKind::ShortFlag('r')
      | ArgKind::ShortFlag('R') => {
        recursive = true;
      }
      _ => arg.bail_unsupported()?,
    }
  }
  if paths.is_empty() {
    bail!("missing file operand");
  } else if paths.len() == 1 {
    bail!("missing destination file operand after '{}'", paths[0]);
  }

  Ok(CpFlags {
    recursive,
    operations: get_copy_and_move_operations(cwd, paths)?,
  })
}

pub struct MvCommand;

impl ShellCommand for MvCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        mv_command(context.state.cwd(), context.args, context.stderr),
        context.state.token()
      )
    }
    .boxed_local()
  }
}

async fn mv_command(
  cwd: &Path,
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_mv(cwd, args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new(), Vec::new()),
    Err(err) => {
      let _ = stderr.write_line(&format!("mv: {err}"));
      ExecuteResult::Continue(1, Vec::new(), Vec::new())
    }
  }
}

async fn execute_mv(cwd: &Path, args: Vec<String>) -> Result<()> {
  let flags = parse_mv_args(cwd, args)?;
  for (from, to) in flags.operations {
    if let Err(err) = tokio::fs::rename(&from.path, &to.path).await {
      bail!(
        "could not move {} to {}: {}",
        from.specified,
        to.specified,
        err
      );
    }
  }
  Ok(())
}

struct MvFlags {
  operations: Vec<(PathWithSpecified, PathWithSpecified)>,
}

fn parse_mv_args(cwd: &Path, args: Vec<String>) -> Result<MvFlags> {
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

  Ok(MvFlags {
    operations: get_copy_and_move_operations(cwd, paths)?,
  })
}

struct PathWithSpecified {
  path: PathBuf,
  specified: String,
}

fn get_copy_and_move_operations(
  cwd: &Path,
  mut paths: Vec<&str>,
) -> Result<Vec<(PathWithSpecified, PathWithSpecified)>> {
  // copy and move share the same logic
  let specified_destination = paths.pop().unwrap();
  let destination = cwd.join(specified_destination);
  let from_args = paths;
  let mut operations = Vec::new();
  if from_args.len() > 1 {
    if !destination.is_dir() {
      bail!("target '{}' is not a directory", specified_destination);
    }
    for from in from_args {
      let from_path = cwd.join(from);
      let to_path = destination.join(from_path.file_name().unwrap());
      operations.push((
        PathWithSpecified {
          specified: from.to_string(),
          path: from_path,
        },
        PathWithSpecified {
          specified: specified_destination.to_string(),
          path: to_path,
        },
      ));
    }
  } else {
    let from_path = cwd.join(from_args[0]);
    let to_path = if destination.is_dir() {
      destination.join(from_path.file_name().unwrap())
    } else {
      destination
    };
    operations.push((
      PathWithSpecified {
        specified: from_args[0].to_string(),
        path: from_path,
      },
      PathWithSpecified {
        specified: specified_destination.to_string(),
        path: to_path,
      },
    ));
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

    // test recursive flag
    fs::create_dir_all(dest_dir.join("sub_dir")).unwrap();
    fs::write(dest_dir.join("sub_dir").join("sub.txt"), "test").unwrap();
    let dest_dir2 = dir.path().join("dest2");

    let result =
      execute_cp(dir.path(), vec!["dest".to_string(), "dest2".to_string()])
        .await
        .err()
        .unwrap();
    assert_eq!(
      result.to_string(),
      "could not copy dest to dest2: source was a directory; maybe specify -r"
    );
    assert!(!dest_dir2.exists());

    execute_cp(
      dir.path(),
      vec!["-r".to_string(), "dest".to_string(), "dest2".to_string()],
    )
    .await
    .unwrap();
    assert!(dest_dir2.exists());
    assert!(dest_dir2.join("file1.txt").exists());
    assert!(dest_dir2.join("file2.txt").exists());
    assert!(dest_dir2.join("sub_dir").join("sub.txt").exists());

    // copy again
    execute_cp(
      dir.path(),
      vec!["-r".to_string(), "dest".to_string(), "dest2".to_string()],
    )
    .await
    .unwrap();

    // try copying to a file
    let result = execute_cp(
      dir.path(),
      vec![
        "-r".to_string(),
        "dest".to_string(),
        "dest2/file1.txt".to_string(),
      ],
    )
    .await
    .err()
    .unwrap();
    assert_eq!(
      result.to_string(),
      "could not copy dest to dest2/file1.txt: destination was a file"
    )
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
