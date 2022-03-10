// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use pretty_assertions::assert_eq;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use std::task::Poll;

use parking_lot::Mutex;
use tokio::io::AsyncWrite;

use crate::environment::Environment;
use crate::execute_with_environment;
use crate::fs_util;
use crate::parser::parse;

enum TestAssertion {
  FileExists(String),
  FileNotExists(String),
}

struct TempDir {
  // hold to keep it alive until drop
  _inner: tempfile::TempDir,
  cwd: PathBuf,
}

impl TempDir {
  pub fn new() -> Self {
    let temp_dir = tempfile::tempdir().unwrap();
    let cwd = fs_util::canonicalize_path(temp_dir.path()).unwrap();
    Self {
      _inner: temp_dir,
      cwd,
    }
  }
}

pub struct TestBuilder {
  // it is much much faster to lazily create this
  temp_dir: Option<TempDir>,
  env_vars: HashMap<String, String>,
  command: String,
  environment: TestEnvironment,
  expected_exit_code: i32,
  expected_stderr: String,
  expected_stdout: String,
  assertions: Vec<TestAssertion>,
}

impl TestBuilder {
  pub fn new() -> Self {
    let env_vars = std::env::vars().collect();

    Self {
      temp_dir: None,
      env_vars,
      command: String::new(),
      environment: TestEnvironment::default(),
      expected_exit_code: 0,
      expected_stderr: String::new(),
      expected_stdout: String::new(),
      assertions: Vec::new(),
    }
  }

  fn ensure_temp_dir(&mut self) -> &mut TempDir {
    if self.temp_dir.is_none() {
      self.temp_dir = Some(TempDir::new());
    }
    self.temp_dir.as_mut().unwrap()
  }

  pub fn command(&mut self, command: &str) -> &mut Self {
    self.command = command.to_string();
    self
  }

  pub fn file(&mut self, path: &str, text: &str) -> &mut Self {
    let temp_dir = self.ensure_temp_dir();
    fs::write(temp_dir.cwd.join(path), text).unwrap();
    self
  }

  pub fn assert_exit_code(&mut self, code: i32) -> &mut Self {
    self.expected_exit_code = code;
    self
  }

  pub fn assert_stderr(&mut self, output: &str) -> &mut Self {
    self.expected_stderr.push_str(output);
    self
  }

  pub fn assert_stdout(&mut self, output: &str) -> &mut Self {
    self.expected_stdout.push_str(output);
    self
  }

  pub fn assert_exists(&mut self, path: &str) -> &mut Self {
    self.ensure_temp_dir();
    self
      .assertions
      .push(TestAssertion::FileExists(path.to_string()));
    self
  }

  pub fn assert_not_exists(&mut self, path: &str) -> &mut Self {
    self.ensure_temp_dir();
    self
      .assertions
      .push(TestAssertion::FileNotExists(path.to_string()));
    self
  }

  pub async fn run(&mut self) {
    let list = parse(&self.command).unwrap();
    let cwd = if let Some(temp_dir) = &self.temp_dir {
      temp_dir.cwd.clone()
    } else {
      std::env::temp_dir()
    };
    let exit_code = execute_with_environment(
      list,
      self.env_vars.clone(),
      &cwd,
      self.environment.clone(),
    )
    .await;
    let temp_dir = if let Some(temp_dir) = &self.temp_dir {
      temp_dir.cwd.display().to_string()
    } else {
      "NO_TEMP_DIR".to_string()
    };
    assert_eq!(
      self.environment.take_stdout(),
      self.expected_stdout.replace("$TEMP_DIR", &temp_dir),
      "\n\nFailed for: {}",
      self.command
    );
    assert_eq!(
      self.environment.take_stderr(),
      self.expected_stderr.replace("$TEMP_DIR", &temp_dir),
      "\n\nFailed for: {}",
      self.command
    );
    assert_eq!(
      exit_code, self.expected_exit_code,
      "\n\nFailed for: {}",
      self.command
    );

    for assertion in &self.assertions {
      match assertion {
        TestAssertion::FileExists(path) => {
          assert!(
            cwd.join(&path).exists(),
            "\n\nFailed for: {}\nExpected '{}' to exist.",
            self.command,
            path,
          )
        }
        TestAssertion::FileNotExists(path) => {
          assert!(
            !cwd.join(&path).exists(),
            "\n\nFailed for: {}\nExpected '{}' to not exist.",
            self.command,
            path,
          )
        }
      }
    }
  }
}

#[derive(Default, Clone)]
pub struct TestEnvironment(Arc<InnerTestEnvironment>);

impl TestEnvironment {
  pub fn take_stderr(&self) -> String {
    self.0.take_stderr()
  }

  pub fn take_stdout(&self) -> String {
    self.0.take_stdout()
  }
}

impl Environment for TestEnvironment {
  fn async_stdout(
    &self,
  ) -> Box<dyn tokio::io::AsyncWrite + Unpin + Send + Sync> {
    Box::new(self.0.stdout.clone())
  }

  fn eprintln(&self, text: &str) {
    self.0.stderr.write_text(&format!("{}\n", text));
  }
}

#[derive(Default)]
struct InnerTestEnvironment {
  stderr: MemoryWriter,
  stdout: MemoryWriter,
}

impl InnerTestEnvironment {
  pub fn take_stderr(&self) -> String {
    self.stderr.take_string()
  }

  pub fn take_stdout(&self) -> String {
    self.stdout.take_string()
  }
}

impl Drop for InnerTestEnvironment {
  fn drop(&mut self) {
    if !std::thread::panicking() {
      assert_eq!(self.take_stderr(), String::new());
      assert_eq!(self.take_stdout(), String::new());
    }
  }
}

#[derive(Clone, Default)]
struct MemoryWriter(Arc<Mutex<Vec<u8>>>);

impl MemoryWriter {
  pub fn write_text(&self, text: &str) {
    self.0.lock().extend(text.as_bytes());
  }

  pub fn take_string(&self) -> String {
    let mut data = self.0.lock();
    let buffer = std::mem::take(&mut *data);
    String::from_utf8_lossy(&buffer).to_string()
  }
}

impl AsyncWrite for MemoryWriter {
  fn poll_write(
    self: Pin<&mut Self>,
    _cx: &mut std::task::Context<'_>,
    buf: &[u8],
  ) -> Poll<Result<usize, std::io::Error>> {
    self.0.lock().extend(buf);
    Poll::Ready(Ok(buf.len()))
  }

  fn poll_flush(
    self: Pin<&mut Self>,
    _cx: &mut std::task::Context<'_>,
  ) -> Poll<Result<(), std::io::Error>> {
    Poll::Ready(Ok(()))
  }

  fn poll_shutdown(
    self: Pin<&mut Self>,
    _cx: &mut std::task::Context<'_>,
  ) -> Poll<Result<(), std::io::Error>> {
    Poll::Ready(Ok(()))
  }
}
