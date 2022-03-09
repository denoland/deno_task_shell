// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::time::Duration;

use anyhow::bail;
use anyhow::Result;

use crate::environment::Environment;
use crate::shell_types::ExecuteResult;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub async fn sleep_command(
  args: Vec<String>,
  environment: impl Environment,
) -> ExecuteResult {
  match execute_sleep(args).await {
    Ok(()) => ExecuteResult::Continue(0, Vec::new()),
    Err(err) => {
      environment.eprintln(&format!("sleep: {}", err));
      ExecuteResult::Continue(1, Vec::new())
    }
  }
}

async fn execute_sleep(args: Vec<String>) -> Result<()> {
  let ms = parse_args(args)?;
  tokio::time::sleep(Duration::from_millis(ms)).await;
  Ok(())
}

fn parse_args(args: Vec<String>) -> Result<u64> {
  // the time to sleep is the sum of all the arguments
  let mut total_time_ms = 0;
  let mut had_value = false;
  for arg in parse_arg_kinds(&args) {
    match arg {
      ArgKind::Arg(arg) => match arg.parse::<f64>() {
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
  fn should_parse_args() {
    let value =
      parse_args(vec!["0.5".to_string(), "1".to_string(), "1.25".to_string()])
        .unwrap();
    assert_eq!(value, 500 + 1000 + 1250);

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
