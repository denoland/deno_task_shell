// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;

use crate::shell::types::ExecuteResult;
use crate::EnvChange;

use super::ShellCommand;
use super::ShellCommandContext;

pub struct UnsetCommand;

impl ShellCommand for UnsetCommand {
  fn execute(
    &self,
    mut context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let result = match parse_names(context.args) {
      Ok(names) => ExecuteResult::Continue(
        0,
        names.into_iter().map(EnvChange::UnsetVar).collect(),
        Vec::new(),
      ),
      Err(err) => {
        let _ = context.stderr.write_line(&format!("unset: {err}"));
        ExecuteResult::Continue(1, Vec::new(), Vec::new())
      }
    };
    Box::pin(futures::future::ready(result))
  }
}

fn parse_names(mut args: Vec<String>) -> Result<Vec<String>> {
  match args.first() {
    None => {
      // Running the actual `unset` with no argument completes with success.
      Ok(args)
    }
    Some(flag) if flag == "-f" => bail!("unsupported flag: -f"),
    Some(flag) if flag == "-v" => {
      // It's fine to use `swap_remove` (instead of `remove`) because the order
      // of args doesn't matter for `unset` command.
      args.swap_remove(0);
      Ok(args)
    }
    Some(_) => Ok(args),
  }
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn parse_args() {
    assert_eq!(
      parse_names(vec!["VAR1".to_string()]).unwrap(),
      vec!["VAR1".to_string()]
    );
    assert_eq!(
      parse_names(vec!["VAR1".to_string(), "VAR2".to_string()]).unwrap(),
      vec!["VAR1".to_string(), "VAR2".to_string()]
    );
    assert!(parse_names(vec![]).unwrap().is_empty());
    assert_eq!(
      parse_names(vec![
        "-f".to_string(),
        "VAR1".to_string(),
        "VAR2".to_string()
      ])
      .err()
      .unwrap()
      .to_string(),
      "unsupported flag: -f".to_string()
    );
    assert_eq!(
      parse_names(vec![
        "-v".to_string(),
        "VAR1".to_string(),
        "VAR2".to_string()
      ])
      .unwrap(),
      vec!["VAR2".to_string(), "VAR1".to_string()]
    );
  }
}
