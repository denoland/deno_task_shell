// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;

#[derive(Debug, PartialEq)]
pub enum ArgKind<'a> {
  ShortFlag(char),
  LongFlag(&'a str),
  Arg(&'a str),
}

impl<'a> ArgKind<'a> {
  pub fn bail_unsupported(&self) -> Result<()> {
    match self {
      ArgKind::Arg(arg) => {
        bail!("unsupported argument: {}", arg)
      }
      ArgKind::LongFlag(name) => {
        bail!("unsupported flag: --{}", name)
      }
      ArgKind::ShortFlag(name) => {
        bail!("unsupported flag: -{}", name)
      }
    }
  }
}

pub fn parse_arg_kinds(flags: &[String]) -> Vec<ArgKind> {
  let mut result = Vec::new();
  let mut had_dash_dash = false;
  for arg in flags {
    if had_dash_dash {
      result.push(ArgKind::Arg(arg));
    } else if arg == "--" {
      had_dash_dash = true;
    } else if let Some(flag) = arg.strip_prefix("--") {
      result.push(ArgKind::LongFlag(flag));
    } else if let Some(flags) = arg.strip_prefix('-') {
      for c in flags.chars() {
        result.push(ArgKind::ShortFlag(c));
      }
    } else {
      result.push(ArgKind::Arg(arg));
    }
  }
  result
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses() {
    let data = vec![
      "-f".to_string(),
      "-ab".to_string(),
      "--force".to_string(),
      "testing".to_string(),
      "other".to_string(),
      "--".to_string(),
      "--test".to_string(),
      "-t".to_string(),
    ];
    let args = parse_arg_kinds(&data);
    assert_eq!(
      args,
      vec![
        ArgKind::ShortFlag('f'),
        ArgKind::ShortFlag('a'),
        ArgKind::ShortFlag('b'),
        ArgKind::LongFlag("force"),
        ArgKind::Arg("testing"),
        ArgKind::Arg("other"),
        ArgKind::Arg("--test"),
        ArgKind::Arg("-t"),
      ]
    )
  }
}
