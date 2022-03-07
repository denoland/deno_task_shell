// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

// todo: standardize and move all commands to this file

use std::io::ErrorKind;
use std::path::Path;
use std::path::PathBuf;
use std::time::Duration;

use crate::fs_util;
use crate::shell_types::EnvChange;
use crate::shell_types::ExecuteResult;

pub fn cd_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  if args.len() != 1 {
    eprintln!("cd expects 1 argument.");
    ExecuteResult::Continue(1, Vec::new())
  } else {
    // affects the parent state
    let new_dir = cwd.join(&args[0]);
    match fs_util::canonicalize_path(&new_dir) {
      Ok(new_dir) => {
        ExecuteResult::Continue(0, vec![EnvChange::Cd(new_dir)])
      }
      Err(err) => {
        eprintln!("Could not cd to {}.\n\n{}", new_dir.display(), err);
        ExecuteResult::Continue(1, Vec::new())
      }
    }
  }
}

pub async fn cp_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  // todo: support -R and some other flags (not all)
  if args.iter().any(|a| a.starts_with('-')) {
    eprintln!("Flags are not currently supported for cp.");
    ExecuteResult::Continue(1, Vec::new())
  } else if args.len() < 2 {
    eprintln!("cp expects at least 2 arguments.");
    ExecuteResult::Continue(1, Vec::new())
  } else {
    let operations = get_copy_and_move_operations(cwd, args);
    for (from, to) in operations {
      if let Err(err) = tokio::fs::copy(&from, &to).await {
        eprintln!("Could not copy {} to {}.\n\n{}", from.display(), to.display(), err);
        return ExecuteResult::Continue(1, Vec::new());
      }
    }
    ExecuteResult::Continue(0, Vec::new())
  }
}

pub async fn mv_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  // todo: support some other flags (not all)
  if args.iter().any(|a| a.starts_with('-')) {
    eprintln!("Flags are not currently supported for mv.");
    ExecuteResult::Continue(1, Vec::new())
  } else if args.len() < 2 {
    eprintln!("mv expects at least 2 arguments.");
    ExecuteResult::Continue(1, Vec::new())
  } else {
    let operations = get_copy_and_move_operations(cwd, args);
    for (from, to) in operations {
      if let Err(err) = tokio::fs::rename(&from, &to).await {
        eprintln!("Could not copy {} to {}.\n\n{}", from.display(), to.display(), err);
        return ExecuteResult::Continue(1, Vec::new());
      }
    }
    ExecuteResult::Continue(0, Vec::new())
  }
}

fn get_copy_and_move_operations(cwd: &Path, mut args: Vec<String>) -> Vec<(PathBuf, PathBuf)> {
  // copy and move share the same logic
  let destination = cwd.join(args.pop().unwrap());
  let from_args = args.into_iter().map(|a| cwd.join(a)).collect::<Vec<_>>();
  let mut operations = Vec::new();
  if from_args.len() > 1 {
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
  operations
}

pub async fn rm_command(cwd: &Path, args: Vec<String>) -> ExecuteResult {
  // todo: support some flags (not all)
  if args.iter().any(|a| a.starts_with('-')) {
    eprintln!("Flags are not currently supported for rm.");
    ExecuteResult::Continue(1, Vec::new())
  } else if args.len() < 2 {
    eprintln!("rm expects at least 2 arguments.");
    ExecuteResult::Continue(1, Vec::new())
  } else {
    for arg in args {
      let file_path = cwd.join(arg);
      if let Err(err) = tokio::fs::remove_file(&file_path).await {
        if err.kind() != ErrorKind::NotFound {
          eprintln!("Could not remove {}.\n\n{}", file_path.display(), err);
          return ExecuteResult::Continue(1, Vec::new());
        }
      }
    }
    ExecuteResult::Continue(0, Vec::new())
  }
}

pub async fn sleep_command(args: Vec<String>) -> ExecuteResult {
  // the time to sleep is the sum of all the arguments
  let mut total_time_ms = 0;
  for arg in args.iter() {
    match arg.parse::<f64>() {
      Ok(value_s) => {
        let ms = (value_s * 1000f64) as u64;
        total_time_ms += ms;
      }
      Err(err) => {
        eprintln!("Error parsing sleep argument to number: {}", err);
        return ExecuteResult::Continue(1, Vec::new());
      }
    }
  }
  tokio::time::sleep(Duration::from_millis(total_time_ms)).await;
  ExecuteResult::Continue(0, Vec::new())
}
