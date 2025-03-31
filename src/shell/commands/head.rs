// Copyright 2018-2024 the Deno authors. MIT license.

use std::ffi::OsStr;
use std::ffi::OsString;
use std::fs::File;
use std::io::Read;

use anyhow::Result;
use anyhow::bail;
use futures::future::LocalBoxFuture;

use crate::ExecuteResult;
use crate::ShellCommand;
use crate::ShellCommandContext;
use crate::ShellPipeWriter;
use crate::shell::KillSignal;

use super::args::ArgKind;
use super::args::parse_arg_kinds;

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
  kill_signal: &KillSignal,
  mut read: F,
  buffer_size: usize,
) -> Result<ExecuteResult> {
  let mut written_lines = 0;
  let mut buffer = vec![0; buffer_size];
  while written_lines < max_lines {
    if let Some(exit_code) = kill_signal.aborted_code() {
      return Ok(ExecuteResult::from_exit_code(exit_code));
    }
    let read_bytes = read(&mut buffer)?;
    if read_bytes == 0 {
      break;
    }

    if let Some(exit_code) = kill_signal.aborted_code() {
      return Ok(ExecuteResult::from_exit_code(exit_code));
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
  let flags = parse_args(&context.args)?;
  if flags.path == "-" {
    copy_lines(
      &mut context.stdout,
      flags.lines,
      context.state.kill_signal(),
      |buf| context.stdin.read(buf),
      512,
    )
  } else {
    let path = flags.path;
    match File::open(context.state.cwd().join(path)) {
      Ok(mut file) => copy_lines(
        &mut context.stdout,
        flags.lines,
        context.state.kill_signal(),
        |buf| file.read(buf).map_err(Into::into),
        512,
      ),
      Err(err) => {
        context.stderr.write_line(&format!(
          "head: {}: {}",
          path.to_string_lossy(),
          err
        ))?;
        Ok(ExecuteResult::from_exit_code(1))
      }
    }
  }
}

#[derive(Debug, PartialEq)]
struct HeadFlags<'a> {
  path: &'a OsStr,
  lines: u64,
}

fn parse_args<'a>(args: &'a [OsString]) -> Result<HeadFlags<'a>> {
  let mut path: Option<&'a OsStr> = None;
  let mut lines: Option<u64> = None;
  let mut iterator = parse_arg_kinds(args).into_iter();
  while let Some(arg) = iterator.next() {
    match arg {
      ArgKind::Arg(file_name) => {
        if path.is_none() {
          path = Some(file_name);
          continue;
        }

        // for now, we only support one file
        // TODO: support multiple files
        bail!("only one file is supported for now");
      }
      ArgKind::ShortFlag('n') => match iterator.next() {
        Some(ArgKind::Arg(arg)) => {
          let num = arg.to_str().and_then(|a| a.parse::<u64>().ok());
          if let Some(num) = num {
            lines = Some(num);
          } else {
            bail!("expected a numeric value following -n")
          }
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
    path: path.unwrap_or(OsStr::new("-")),
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
      &KillSignal::default(),
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
      parse_args(&[]).unwrap(),
      HeadFlags {
        path: OsStr::new("-"),
        lines: 10
      }
    );
    assert_eq!(
      parse_args(&["-n".into(), "5".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("-"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["--lines=5".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("-"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["path".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("path"),
        lines: 10
      }
    );
    assert_eq!(
      parse_args(&["-n".into(), "5".into(), "path".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("path"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["--lines=5".into(), "path".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("path"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["path".into(), "-n".into(), "5".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("path"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["path".into(), "--lines=5".into()]).unwrap(),
      HeadFlags {
        path: OsStr::new("path"),
        lines: 5
      }
    );
    assert_eq!(
      parse_args(&["-n".into()]).err().unwrap().to_string(),
      "expected a value following -n"
    );
    assert_eq!(
      parse_args(&["--lines".into()]).err().unwrap().to_string(),
      "expected a value for --lines"
    );
    assert_eq!(
      parse_args(&["--lines=".into()]).err().unwrap().to_string(),
      "expected a value for --lines"
    );
    assert_eq!(
      parse_args(&["--flag".into()]).err().unwrap().to_string(),
      "unsupported flag: --flag"
    );
    assert_eq!(
      parse_args(&["-t".into()]).err().unwrap().to_string(),
      "unsupported flag: -t"
    );
  }
}
