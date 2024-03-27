// Copyright 2018-2024 the Deno authors. MIT license.

use std::time::Duration;

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::execute_with_cancellation;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct SleepCommand;

impl ShellCommand for SleepCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      execute_with_cancellation!(
        sleep_command(context.args, context.stderr),
        context.state.token()
      )
    }
    .boxed_local()
  }
}

async fn sleep_command(
  args: Vec<String>,
  mut stderr: ShellPipeWriter,
) -> ExecuteResult {
  match execute_sleep(args).await {
    Ok(()) => ExecuteResult::from_exit_code(0),
    Err(err) => {
      let _ = stderr.write_line(&format!("sleep: {err}"));
      ExecuteResult::from_exit_code(1)
    }
  }
}

async fn execute_sleep(args: Vec<String>) -> Result<()> {
  let ms = parse_args(args)?;
  tokio::time::sleep(Duration::from_millis(ms)).await;
  Ok(())
}

fn parse_arg(arg: &str) -> Result<f64> {
  if let Some(t) = arg.strip_suffix('s') {
    return Ok(t.parse()?);
  }
  if let Some(t) = arg.strip_suffix('m') {
    return Ok(t.parse::<f64>()? * 60.);
  }
  if let Some(t) = arg.strip_suffix('h') {
    return Ok(t.parse::<f64>()? * 60. * 60.);
  }
  if let Some(t) = arg.strip_suffix('d') {
    return Ok(t.parse::<f64>()? * 60. * 60. * 24.);
  }

  Ok(arg.parse()?)
}

fn parse_args(args: Vec<String>) -> Result<u64> {
  // the time to sleep is the sum of all the arguments
  let mut total_time_ms = 0;
  let mut had_value = false;
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::Arg(arg) => match parse_arg(arg) {
        Ok(value_s) => {
          let ms = (value_s * 1000f64) as u64;
          total_time_ms += ms;
          had_value = true;
        }
        Err(err) => {
          bail!("error parsing argument '{}' to number: {}", arg, err);
        }
      },
      ArgKind::LongFlag(_) | ArgKind::ShortFlag(_) => arg.bail_unsupported()?,
    }
  }
  if !had_value {
    bail!("missing operand");
  }
  Ok(total_time_ms)
}

#[cfg(test)]
mod test {
  use std::time::Instant;

  use super::*;

  #[test]
  fn should_parse_arg() {
    assert_eq!(parse_arg("1").unwrap(), 1.);
    assert_eq!(parse_arg("1s").unwrap(), 1.);
    assert_eq!(parse_arg("1m").unwrap(), 1. * 60.);
    assert_eq!(parse_arg("1h").unwrap(), 1. * 60. * 60.);
    assert_eq!(parse_arg("1d").unwrap(), 1. * 60. * 60. * 24.);
    assert!(parse_arg("d").err().is_some());
  }

  #[test]
  fn should_parse_args() {
    let value = parse_args(vec![
      "0.5".to_string(),
      "1m".to_string(),
      "1.25".to_string(),
    ])
    .unwrap();
    assert_eq!(value, 500 + 1000 * 60 + 1250);

    let result = parse_args(vec![]).err().unwrap();
    assert_eq!(result.to_string(), "missing operand");

    let result = parse_args(vec!["test".to_string()]).err().unwrap();
    assert_eq!(
      result.to_string(),
      "error parsing argument 'test' to number: invalid float literal"
    );
  }

  #[tokio::test]
  async fn should_execute() {
    let time = Instant::now();
    execute_sleep(vec!["0.1".to_string()]).await.unwrap();
    assert!(time.elapsed().as_millis() >= 100);
  }
}
