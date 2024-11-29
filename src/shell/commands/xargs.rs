// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use futures::FutureExt;

use crate::shell::types::ExecuteResult;
use crate::shell::types::ShellPipeReader;
use crate::ExecuteCommandArgsContext;

use super::args::parse_arg_kinds;
use super::args::ArgKind;
use super::ShellCommand;
use super::ShellCommandContext;

pub struct XargsCommand;

impl ShellCommand for XargsCommand {
  fn execute(
    &self,
    mut context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    async move {
      match xargs_collect_args(context.args, context.stdin.clone()) {
        Ok(args) => {
          // don't select on cancellation here as that will occur at a lower level
          (context.execute_command_args)(ExecuteCommandArgsContext {
            args,
            state: context.state,
            stdin: context.stdin,
            stdout: context.stdout,
            stderr: context.stderr,
          })
          .await
        }
        Err(err) => {
          let _ = context.stderr.write_line(&format!("xargs: {err}"));
          ExecuteResult::from_exit_code(1)
        }
      }
    }
    .boxed_local()
  }
}

fn xargs_collect_args(
  cli_args: Vec<String>,
  stdin: ShellPipeReader,
) -> Result<Vec<String>> {
  let flags = parse_args(cli_args)?;
  let mut buf = Vec::new();
  stdin.pipe_to(&mut buf)?;
  let text = String::from_utf8(buf)?;
  let mut args = flags.initial_args;

  if args.is_empty() {
    // defaults to echo
    args.push("echo".to_string());
  }

  if let Some(delim) = &flags.delimiter {
    // strip a single trailing newline (xargs seems to do this)
    let text = if *delim == '\n' {
      if let Some(text) = text.strip_suffix(&delim.to_string()) {
        text
      } else {
        &text
      }
    } else {
      &text
    };

    args.extend(text.split(*delim).map(|t| t.to_string()));
  } else if flags.is_null_delimited {
    args.extend(text.split('\0').map(|t| t.to_string()));
  } else {
    args.extend(delimit_blanks(&text)?);
  }

  Ok(args)
}

fn delimit_blanks(text: &str) -> Result<Vec<String>> {
  let mut chars = text.chars().peekable();
  let mut result = Vec::new();
  while chars.peek().is_some() {
    let mut current = String::new();
    while let Some(c) = chars.next() {
      match c {
        '\n' | '\t' | ' ' => break,
        '"' | '\'' => {
          const UNMATCHED_MESSAGE: &str = "unmatched quote; by default quotes are special to xargs unless you use the -0 option";
          let original_quote_char = c;
          while let Some(c) = chars.next() {
            if c == original_quote_char {
              break;
            }
            match c {
              '\n' => bail!("{}", UNMATCHED_MESSAGE),
              _ => current.push(c),
            }
            if chars.peek().is_none() {
              bail!("{}", UNMATCHED_MESSAGE)
            }
          }
        }
        '\\' => {
          if matches!(chars.peek(), Some('\n' | '\t' | ' ' | '"' | '\'')) {
            current.push(chars.next().unwrap());
          } else {
            current.push(c);
          }
        }
        _ => current.push(c),
      }
    }

    if !current.is_empty() {
      result.push(current);
    }
  }
  Ok(result)
}

#[derive(Debug, PartialEq)]
struct XargsFlags {
  initial_args: Vec<String>,
  delimiter: Option<char>,
  is_null_delimited: bool,
}

fn parse_args(args: Vec<String>) -> Result<XargsFlags> {
  fn parse_delimiter(arg: &str) -> Result<char> {
    let mut chars = arg.chars();
    if let Some(first_char) = chars.next() {
      let mut delimiter = first_char;
      if first_char == '\\' {
        delimiter = match chars.next() {
          // todo(dsherret): support more
          Some('n') => '\n',
          Some('r') => '\r',
          Some('t') => '\t',
          Some('\\') => '\\',
          Some('0') => '\0',
          None => bail!("expected character following escape"),
          _ => bail!("unsupported/not implemented escape character"),
        };
      }

      if chars.next().is_some() {
        bail!("expected a single byte char delimiter. Found: {}", arg);
      }

      Ok(delimiter)
    } else {
      bail!("expected non-empty delimiter");
    }
  }

  let mut initial_args = Vec::new();
  let mut delimiter = None;
  let mut iterator = parse_arg_kinds(&args).into_iter();
  let mut is_null_delimited = false;
  while let Some(arg) = iterator.next() {
    match arg {
      ArgKind::Arg(arg) => {
        if arg == "-0" {
          is_null_delimited = true;
        } else {
          initial_args.push(arg.to_string());
          // parse the remainder as arguments
          for arg in iterator.by_ref() {
            match arg {
              ArgKind::Arg(arg) => {
                initial_args.push(arg.to_string());
              }
              ArgKind::ShortFlag(f) => initial_args.push(format!("-{f}")),
              ArgKind::LongFlag(f) => initial_args.push(format!("--{f}")),
            }
          }
        }
      }
      ArgKind::LongFlag("null") => {
        is_null_delimited = true;
      }
      ArgKind::ShortFlag('d') => match iterator.next() {
        Some(ArgKind::Arg(arg)) => {
          delimiter = Some(parse_delimiter(arg)?);
        }
        _ => bail!("expected delimiter argument following -d"),
      },
      ArgKind::LongFlag(flag) => {
        if let Some(arg) = flag.strip_prefix("delimiter=") {
          delimiter = Some(parse_delimiter(arg)?);
        } else {
          arg.bail_unsupported()?
        }
      }
      _ => arg.bail_unsupported()?,
    }
  }

  if is_null_delimited && delimiter.is_some() {
    bail!("cannot specify both null and delimiter flag")
  }

  Ok(XargsFlags {
    initial_args,
    delimiter,
    is_null_delimited,
  })
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn parses_args() {
    assert_eq!(
      parse_args(vec![]).unwrap(),
      XargsFlags {
        initial_args: Vec::new(),
        delimiter: None,
        is_null_delimited: false,
      }
    );
    assert_eq!(
      parse_args(vec![
        "-0".to_string(),
        "echo".to_string(),
        "2".to_string(),
        "-d".to_string(),
        "--test=3".to_string()
      ])
      .unwrap(),
      XargsFlags {
        initial_args: vec![
          "echo".to_string(),
          "2".to_string(),
          "-d".to_string(),
          "--test=3".to_string()
        ],
        delimiter: None,
        is_null_delimited: true,
      }
    );
    assert_eq!(
      parse_args(vec![
        "-d".to_string(),
        "\\n".to_string(),
        "echo".to_string()
      ])
      .unwrap(),
      XargsFlags {
        initial_args: vec!["echo".to_string()],
        delimiter: Some('\n'),
        is_null_delimited: false,
      }
    );
    assert_eq!(
      parse_args(vec![
        "--delimiter=5".to_string(),
        "echo".to_string(),
        "-d".to_string()
      ])
      .unwrap(),
      XargsFlags {
        initial_args: vec!["echo".to_string(), "-d".to_string()],
        delimiter: Some('5'),
        is_null_delimited: false,
      }
    );
    assert_eq!(
      parse_args(vec!["-d".to_string(), "5".to_string(), "-t".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: -t",
    );
    assert_eq!(
      parse_args(vec!["-d".to_string(), "-t".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "expected delimiter argument following -d",
    );
    assert_eq!(
      parse_args(vec!["--delimiter=5".to_string(), "--null".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "cannot specify both null and delimiter flag",
    );
  }

  #[test]
  fn should_delimit_blanks() {
    assert_eq!(
      delimit_blanks("testing this\tout\nhere\n  \n\t\t test").unwrap(),
      vec!["testing", "this", "out", "here", "test",]
    );
    assert_eq!(
      delimit_blanks("testing 'this\tout  here  ' \"now double\"").unwrap(),
      vec!["testing", "this\tout  here  ", "now double"]
    );
    assert_eq!(
      delimit_blanks("testing 'this\nout  here  '").err().unwrap().to_string(),
      "unmatched quote; by default quotes are special to xargs unless you use the -0 option",
    );
  }
}
