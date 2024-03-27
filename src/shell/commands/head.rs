// Copyright 2018-2024 the Deno authors. MIT license.

use std::fs::File;
use std::io::Read;

use anyhow::bail;
use anyhow::Result;
use futures::future::LocalBoxFuture;
use tokio_util::sync::CancellationToken;

use crate::ExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use crate::ShellPipeWriter;

use super::args::parse_arg_kinds;
use super::args::ArgKind;

pub struct HeadCommand;

impl ShellCommand for HeadCommand {
  fn execute(
    &self,
    context: ShellCommandContext,
  ) -> LocalBoxFuture<'static, ExecuteResult> {
    let mut stderr = context.stderr.clone();
    let result = match execute_head(context) {
      Ok(result) => result,
      Err(err) => {
        let _ = stderr.write_line(&format!("head: {err}"));
        ExecuteResult::from_exit_code(1)
      }
    };
    Box::pin(futures::future::ready(result))
  }
}

fn copy_lines<F: FnMut(&mut [u8]) -> Result<usize>>(
  writer: &mut ShellPipeWriter,
  max_lines: u64,
  cancellation_token: &CancellationToken,
  mut read: F,
  buffer_size: usize,
) -> Result<ExecuteResult> {
  let mut written_lines = 0;
  let mut buffer = vec![0; buffer_size];
  while written_lines < max_lines {
    if cancellation_token.is_cancelled() {
      return Ok(ExecuteResult::for_cancellation());
    }
    let read_bytes = read(&mut buffer)?;
    if read_bytes == 0 {
      break;
    }

    if cancellation_token.is_cancelled() {
      return Ok(ExecuteResult::for_cancellation());
    }

    let mut written_bytes: usize = 0;
    let split_lines = buffer[..read_bytes].split(|&b| b == b'\n');
    for line in split_lines {
      if written_lines >= max_lines
        || (written_bytes + line.len()) >= read_bytes
      {
        break;
      }
      writer.write_all(line)?;
      writer.write_all(b"\n")?;
      written_bytes += line.len() + 1;
      written_lines += 1;
    }

    if written_lines < max_lines && written_bytes < read_bytes {
      writer.write_all(&buffer[written_bytes..read_bytes])?;
    }
  }

  Ok(ExecuteResult::from_exit_code(0))
}

fn execute_head(mut context: ShellCommandContext) -> Result<ExecuteResult> {
  let flags = parse_args(context.args)?;
  if flags.path == "-" {
    copy_lines(
      &mut context.stdout,
      flags.lines,
      context.state.token(),
      |buf| context.stdin.read(buf),
      512,
    )
  } else {
    let path = flags.path;
    match File::open(context.state.cwd().join(&path)) {
      Ok(mut file) => copy_lines(
        &mut context.stdout,
        flags.lines,
        context.state.token(),
        |buf| file.read(buf).map_err(Into::into),
        512,
      ),
      Err(err) => {
        context.stderr.write_line(&format!("head: {path}: {err}"))?;
        Ok(ExecuteResult::from_exit_code(1))
      }
    }
  }
}

#[derive(Debug, PartialEq)]
struct HeadFlags {
  path: String,
  lines: u64,
}

fn parse_args(args: Vec<String>) -> Result<HeadFlags> {
  let mut path: Option<String> = None;
  let mut lines: Option<u64> = None;
  let mut iterator = parse_arg_kinds(&args).into_iter();
  while let Some(arg) = iterator.next() {
    match arg {
      ArgKind::Arg(file_name) => {
        if path.is_none() {
          path = Some(file_name.to_string());
          continue;
        }

        // for now, we only support one file
        // TODO: support multiple files
        bail!("only one file is supported for now");
      }
      ArgKind::ShortFlag('n') => match iterator.next() {
        Some(ArgKind::Arg(arg)) => {
          lines = Some(arg.parse::<u64>()?);
        }
        _ => bail!("expected a value following -n"),
      },
      ArgKind::LongFlag(flag) => {
        if flag == "lines" || flag == "lines=" {
          bail!("expected a value for --lines");
        } else if let Some(arg) = flag.strip_prefix("lines=") {
          lines = Some(arg.parse::<u64>()?);
        } else {
          arg.bail_unsupported()?
        }
      }
      _ => arg.bail_unsupported()?,
    }
  }

  Ok(HeadFlags {
    path: path.unwrap_or("-".to_string()),
    lines: lines.unwrap_or(10),
  })
}

#[cfg(test)]
mod test {
  use crate::pipe;
  use std::cmp::min;

  use super::*;
  use pretty_assertions::assert_eq;

  async fn copies_lines(
    // #[case]
    buffer_size: usize,
  ) {
    let (reader, mut writer) = pipe();
    let reader_handle = reader.pipe_to_string_handle();
    let data = b"foo\nbar\nbaz\nqux\n";
    let data_length = data.len();
    let mut offset = 0;
    let result = copy_lines(
      &mut writer,
      2,
      &CancellationToken::new(),
      |buffer| {
        if offset >= data.len() {
          return Ok(0);
        }
        let buffer_length = buffer.len();
        let read_length = min(buffer_length, data_length);
        buffer[..read_length]
          .copy_from_slice(&data[offset..(offset + read_length)]);
        offset += read_length;
        Ok(read_length)
      },
      buffer_size,
    );
    drop(writer); // Drop the writer ahead of the reader to prevent a deadlock.
    assert_eq!(reader_handle.await.unwrap(), "foo\nbar\n");
    assert_eq!(result.unwrap().into_exit_code_and_handles().0, 0);
  }

  #[tokio::test]
  async fn copies_lines_with_shorter_buffer_size() {
    copies_lines(2).await;
  }

  #[tokio::test]
  async fn copies_lines_with_buffer_size_to_match_each_line_length() {
    copies_lines(4).await;
  }

  #[tokio::test]
  async fn copies_lines_with_buffer_of_one_and_half_times_of_each_line_length()
  {
    copies_lines(6).await;
  }

  #[tokio::test]
  async fn copies_lines_with_long_buffer_size() {
    copies_lines(512).await;
  }

  #[test]
  fn parses_args() {
    assert_eq!(
      parse_args(vec![]).unwrap(),
      HeadFlags {
        path: "-".to_string(),
        lines: 10
      }
    );
    assert_eq!(
      parse_args(vec!["-n".to_string(), "5".to_string()]).unwrap(),
      HeadFlags {
        path: "-".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["--lines=5".to_string()]).unwrap(),
      HeadFlags {
        path: "-".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string()]).unwrap(),
      HeadFlags {
        path: "path".to_string(),
        lines: 10
      }
    );
    assert_eq!(
      parse_args(vec!["-n".to_string(), "5".to_string(), "path".to_string()])
        .unwrap(),
      HeadFlags {
        path: "path".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["--lines=5".to_string(), "path".to_string()]).unwrap(),
      HeadFlags {
        path: "path".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string(), "-n".to_string(), "5".to_string()])
        .unwrap(),
      HeadFlags {
        path: "path".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["path".to_string(), "--lines=5".to_string()]).unwrap(),
      HeadFlags {
        path: "path".to_string(),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(vec!["-n".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "expected a value following -n"
    );
    assert_eq!(
      parse_args(vec!["--lines".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "expected a value for --lines"
    );
    assert_eq!(
      parse_args(vec!["--lines=".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "expected a value for --lines"
    );
    assert_eq!(
      parse_args(vec!["--flag".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: --flag"
    );
    assert_eq!(
      parse_args(vec!["-t".to_string()])
        .err()
        .unwrap()
        .to_string(),
      "unsupported flag: -t"
    );
  }
}
