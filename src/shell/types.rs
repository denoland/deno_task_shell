// Copyright 2018-2025 the Deno authors. MIT license.

use std::borrow::Cow;
use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::io::Read;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::rc::Weak;

use anyhow::Result;
use futures::future::LocalBoxFuture;
use tokio::sync::broadcast;
use tokio::task::JoinHandle;

use crate::shell::child_process_tracker::ChildProcessTracker;

use super::commands::ShellCommand;
use super::commands::builtin_commands;

/// Exit code set when an async task fails or the main execution
/// line fail.
#[derive(Debug, Default, Clone)]
pub(crate) struct TreeExitCodeCell(Rc<Cell<i32>>);

impl TreeExitCodeCell {
  pub fn try_set(&self, exit_code: i32) {
    if self.0.get() == 0 {
      // only set it for the first non-zero failure
      self.0.set(exit_code);
    }
  }

  pub fn get(&self) -> Option<i32> {
    match self.0.get() {
      0 => None,
      code => Some(code),
    }
  }
}

#[derive(Clone)]
pub struct ShellState {
  /// Environment variables that should be passed down to sub commands
  /// and used when evaluating environment variables.
  env_vars: HashMap<OsString, OsString>,
  /// Variables that should be evaluated within the shell and
  /// not passed down to any sub commands.
  shell_vars: HashMap<OsString, OsString>,
  cwd: PathBuf,
  commands: Rc<HashMap<String, Rc<dyn ShellCommand>>>,
  kill_signal: KillSignal,
  process_tracker: ChildProcessTracker,
  tree_exit_code_cell: TreeExitCodeCell,
}

impl ShellState {
  pub fn new(
    env_vars: HashMap<OsString, OsString>,
    cwd: PathBuf,
    custom_commands: HashMap<String, Rc<dyn ShellCommand>>,
    kill_signal: KillSignal,
  ) -> Self {
    assert!(cwd.is_absolute());
    let mut commands = builtin_commands();
    commands.extend(custom_commands);
    let mut result = Self {
      env_vars: Default::default(),
      shell_vars: Default::default(),
      cwd: PathBuf::new(),
      commands: Rc::new(commands),
      kill_signal,
      process_tracker: ChildProcessTracker::new(),
      tree_exit_code_cell: Default::default(),
    };
    // ensure the data is normalized
    for (name, value) in env_vars {
      result.apply_env_var(&name, &value);
    }
    result.set_cwd(cwd);
    result
  }

  pub fn cwd(&self) -> &PathBuf {
    &self.cwd
  }

  pub fn env_vars(&self) -> &HashMap<OsString, OsString> {
    &self.env_vars
  }

  pub fn get_var(&self, name: &OsStr) -> Option<&OsString> {
    let name = if cfg!(windows) {
      Cow::Owned(name.to_ascii_uppercase())
    } else {
      Cow::Borrowed(name)
    };
    let name: &OsStr = &name;
    self
      .env_vars
      .get(name)
      .or_else(|| self.shell_vars.get(name))
  }

  pub fn set_cwd(&mut self, cwd: PathBuf) {
    self.cwd = cwd.clone();
    // $PWD holds the current working directory, so we keep cwd and $PWD in sync
    self.env_vars.insert("PWD".into(), cwd.into_os_string());
  }

  pub fn apply_changes(&mut self, changes: &[EnvChange]) {
    for change in changes {
      self.apply_change(change);
    }
  }

  pub fn apply_change(&mut self, change: &EnvChange) {
    match change {
      EnvChange::SetEnvVar(name, value) => self.apply_env_var(name, value),
      EnvChange::SetShellVar(name, value) => {
        if self.env_vars.contains_key(name) {
          self.apply_env_var(name, value);
        } else {
          self
            .shell_vars
            .insert(name.to_os_string(), value.to_os_string());
        }
      }
      EnvChange::UnsetVar(name) => {
        self.shell_vars.remove(name);
        self.env_vars.remove(name);
      }
      EnvChange::Cd(new_dir) => {
        self.set_cwd(new_dir.clone());
      }
    }
  }

  pub fn apply_env_var(&mut self, name: &OsStr, value: &OsStr) {
    let name = if cfg!(windows) {
      // environment variables are case insensitive on windows
      name.to_ascii_uppercase()
    } else {
      name.to_os_string()
    };
    if name == "PWD" {
      let cwd = Path::new(value);
      if cwd.is_absolute()
        && let Ok(cwd) = deno_path_util::fs::canonicalize_path_maybe_not_exists(
          &sys_traits::impls::RealSys,
          cwd,
        )
      {
        // this will update the environment variable too
        self.set_cwd(cwd);
      }
    } else {
      self.shell_vars.remove(&name);
      self.env_vars.insert(name, value.to_os_string());
    }
  }

  pub fn kill_signal(&self) -> &KillSignal {
    &self.kill_signal
  }

  pub fn track_child_process(&self, child: &tokio::process::Child) {
    self.process_tracker.track(child);
  }

  pub(crate) fn tree_exit_code_cell(&self) -> &TreeExitCodeCell {
    &self.tree_exit_code_cell
  }

  /// Resolves a custom command that was injected.
  pub fn resolve_custom_command(
    &self,
    name: &OsStr,
  ) -> Option<Rc<dyn ShellCommand>> {
    // only bother supporting utf8 custom command names for now
    name
      .to_str()
      // uses an Rc to allow resolving a command without borrowing from self
      .and_then(|name| self.commands.get(name).cloned())
  }

  /// Resolves the path to a command from the current working directory.
  ///
  /// Does not take injected custom commands into account.
  pub fn resolve_command_path(
    &self,
    command_name: &OsStr,
  ) -> Result<PathBuf, super::which::CommandPathResolutionError> {
    super::which::resolve_command_path(command_name, self.cwd(), self)
  }

  pub fn with_child_signal(&self) -> ShellState {
    let mut state = self.clone();
    state.kill_signal = self.kill_signal.child_signal();
    state.tree_exit_code_cell = TreeExitCodeCell::default();
    state
  }
}

impl sys_traits::BaseEnvVar for ShellState {
  fn base_env_var_os(&self, key: &OsStr) -> Option<OsString> {
    self.env_vars.get(key).cloned()
  }
}

#[derive(Debug, PartialEq, Eq)]
pub enum EnvChange {
  // `export ENV_VAR=VALUE`
  SetEnvVar(OsString, OsString),
  // `ENV_VAR=VALUE`
  SetShellVar(OsString, OsString),
  // `unset ENV_VAR`
  UnsetVar(OsString),
  Cd(PathBuf),
}

pub type FutureExecuteResult = LocalBoxFuture<'static, ExecuteResult>;

#[derive(Debug)]
pub enum ExecuteResult {
  Exit(i32, Vec<JoinHandle<i32>>),
  Continue(i32, Vec<EnvChange>, Vec<JoinHandle<i32>>),
}

impl ExecuteResult {
  pub fn from_exit_code(exit_code: i32) -> ExecuteResult {
    ExecuteResult::Continue(exit_code, Vec::new(), Vec::new())
  }

  pub fn into_exit_code_and_handles(self) -> (i32, Vec<JoinHandle<i32>>) {
    match self {
      ExecuteResult::Exit(code, handles) => (code, handles),
      ExecuteResult::Continue(code, _, handles) => (code, handles),
    }
  }

  pub fn into_handles(self) -> Vec<JoinHandle<i32>> {
    self.into_exit_code_and_handles().1
  }
}

/// Reader side of a pipe.
#[derive(Debug)]
pub enum ShellPipeReader {
  OsPipe(std::io::PipeReader),
  StdFile(std::fs::File),
}

impl Clone for ShellPipeReader {
  fn clone(&self) -> Self {
    match self {
      Self::OsPipe(pipe) => Self::OsPipe(pipe.try_clone().unwrap()),
      Self::StdFile(file) => Self::StdFile(file.try_clone().unwrap()),
    }
  }
}

impl ShellPipeReader {
  pub fn stdin() -> ShellPipeReader {
    #[cfg(unix)]
    pub fn dup_stdin_as_pipe_reader() -> std::io::PipeReader {
      use std::os::fd::AsFd;
      use std::os::fd::FromRawFd;
      use std::os::fd::IntoRawFd;
      let owned = std::io::stdin().as_fd().try_clone_to_owned().unwrap();
      let raw = owned.into_raw_fd();
      // SAFETY: `raw` is a fresh, owned fd; PipeReader will close it.
      unsafe { std::io::PipeReader::from_raw_fd(raw) }
    }

    #[cfg(windows)]
    pub fn dup_stdin_as_pipe_reader() -> std::io::PipeReader {
      use std::os::windows::io::AsHandle;
      use std::os::windows::io::FromRawHandle;
      use std::os::windows::io::IntoRawHandle;
      let owned = std::io::stdin().as_handle().try_clone_to_owned().unwrap();
      let raw = owned.into_raw_handle();
      // SAFETY: `raw` is a fresh, owned HANDLE; PipeReader will close it.
      unsafe { std::io::PipeReader::from_raw_handle(raw) }
    }

    ShellPipeReader::OsPipe(dup_stdin_as_pipe_reader())
  }

  pub fn from_raw(reader: std::io::PipeReader) -> Self {
    Self::OsPipe(reader)
  }

  pub fn from_std(std_file: std::fs::File) -> Self {
    Self::StdFile(std_file)
  }

  #[cfg(test)]
  #[allow(clippy::should_implement_trait)]
  pub fn from_str(data: &str) -> Self {
    use std::io::Write;
    let (read, mut write) = std::io::pipe().unwrap();
    write.write_all(data.as_bytes()).unwrap();
    Self::OsPipe(read)
  }

  pub fn into_stdio(self) -> std::process::Stdio {
    match self {
      Self::OsPipe(pipe) => pipe.into(),
      Self::StdFile(file) => file.into(),
    }
  }

  /// Pipe everything to the specified writer
  pub fn pipe_to(self, writer: &mut dyn Write) -> Result<()> {
    // don't bother flushing here because this won't ever be called
    // with a Rust wrapped stdout/stderr
    self.pipe_to_inner(writer, false)
  }

  fn pipe_to_with_flushing(self, writer: &mut dyn Write) -> Result<()> {
    self.pipe_to_inner(writer, true)
  }

  fn pipe_to_inner(
    mut self,
    writer: &mut dyn Write,
    flush: bool,
  ) -> Result<()> {
    loop {
      let mut buffer = [0; 512]; // todo: what is an appropriate buffer size?
      let size = match &mut self {
        ShellPipeReader::OsPipe(pipe) => pipe.read(&mut buffer)?,
        ShellPipeReader::StdFile(file) => file.read(&mut buffer)?,
      };
      if size == 0 {
        break;
      }
      writer.write_all(&buffer[0..size])?;
      if flush {
        writer.flush()?;
      }
    }
    Ok(())
  }

  /// Pipes this pipe to the specified sender.
  pub fn pipe_to_sender(self, mut sender: ShellPipeWriter) -> Result<()> {
    match &mut sender {
      ShellPipeWriter::OsPipe(pipe) => self.pipe_to(pipe),
      ShellPipeWriter::StdFile(file) => self.pipe_to(file),
      // Don't lock stdout/stderr here because we want to release the lock
      // when reading from the sending pipe. Additionally, we want
      // to flush after every write because Rust's wrapper has an
      // internal buffer and Deno doesn't buffer stdout/stderr.
      ShellPipeWriter::Stdout => {
        self.pipe_to_with_flushing(&mut std::io::stdout())
      }
      ShellPipeWriter::Stderr => {
        self.pipe_to_with_flushing(&mut std::io::stderr())
      }
      ShellPipeWriter::Null => Ok(()),
    }
  }

  /// Pipes the reader to a string handle that is resolved when the pipe's
  /// writer is closed.
  pub fn pipe_to_string_handle(self) -> JoinHandle<String> {
    tokio::task::spawn_blocking(|| {
      let mut buf = Vec::new();
      self.pipe_to(&mut buf).unwrap();
      String::from_utf8_lossy(&buf).to_string()
    })
  }

  pub fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
    match self {
      ShellPipeReader::OsPipe(pipe) => pipe.read(buf).map_err(|e| e.into()),
      ShellPipeReader::StdFile(file) => file.read(buf).map_err(|e| e.into()),
    }
  }
}

/// Writer side of a pipe.
///
/// Ensure that all of these are dropped when complete in order to
/// prevent deadlocks where the reader hangs waiting for a read.
#[derive(Debug)]
pub enum ShellPipeWriter {
  OsPipe(std::io::PipeWriter),
  StdFile(std::fs::File),
  // For stdout and stderr, instead of directly duplicating the raw pipes
  // and putting them in a ShellPipeWriter::OsPipe(...), we use Rust std's
  // stdout() and stderr() wrappers because it contains some code to solve
  // some encoding issues on Windows (ex. emojis). For more details, see
  // library/std/src/sys/windows/stdio.rs in Rust's source code.
  Stdout,
  Stderr,
  Null,
}

impl Clone for ShellPipeWriter {
  fn clone(&self) -> Self {
    match self {
      Self::OsPipe(pipe) => Self::OsPipe(pipe.try_clone().unwrap()),
      Self::StdFile(file) => Self::StdFile(file.try_clone().unwrap()),
      Self::Stdout => Self::Stdout,
      Self::Stderr => Self::Stderr,
      Self::Null => Self::Null,
    }
  }
}

impl ShellPipeWriter {
  pub fn stdout() -> Self {
    Self::Stdout
  }

  pub fn stderr() -> Self {
    Self::Stderr
  }

  pub fn null() -> Self {
    Self::Null
  }

  pub fn from_std(std_file: std::fs::File) -> Self {
    Self::StdFile(std_file)
  }

  pub fn into_stdio(self) -> std::process::Stdio {
    match self {
      Self::OsPipe(pipe) => pipe.into(),
      Self::StdFile(file) => file.into(),
      Self::Stdout => std::process::Stdio::inherit(),
      Self::Stderr => std::process::Stdio::inherit(),
      Self::Null => std::process::Stdio::null(),
    }
  }

  pub fn write_all(&mut self, bytes: &[u8]) -> Result<()> {
    self.write_all_iter(std::iter::once(bytes))
  }

  pub fn write_all_iter<'a>(
    &mut self,
    iter: impl Iterator<Item = &'a [u8]> + 'a,
  ) -> Result<()> {
    match self {
      Self::OsPipe(pipe) => {
        for bytes in iter {
          pipe.write_all(bytes)?;
        }
      }
      Self::StdFile(file) => {
        for bytes in iter {
          file.write_all(bytes)?
        }
      }
      // For both stdout & stderr, we want to flush after each
      // write in order to bypass Rust's internal buffer.
      Self::Stdout => {
        let mut stdout = std::io::stdout().lock();
        for bytes in iter {
          stdout.write_all(bytes)?;
        }
        stdout.flush()?;
      }
      Self::Stderr => {
        let mut stderr = std::io::stderr().lock();
        for bytes in iter {
          stderr.write_all(bytes)?;
        }
        stderr.flush()?;
      }
      Self::Null => {}
    }
    Ok(())
  }

  pub fn write_line(&mut self, line: &str) -> Result<()> {
    let bytes = format!("{line}\n");
    self.write_all(bytes.as_bytes())
  }
}

/// Used to communicate between commands.
pub fn pipe() -> (ShellPipeReader, ShellPipeWriter) {
  let (reader, writer) = std::io::pipe().unwrap();
  (
    ShellPipeReader::OsPipe(reader),
    ShellPipeWriter::OsPipe(writer),
  )
}

/// Information about the current child process being tracked.
#[derive(Debug, Clone, Copy, Default)]
struct ChildProcessInfo {
  /// The PID of the current foreground child process.
  pid: Option<u32>,
  /// The PGID of the current foreground child process (Unix only).
  /// Cached at spawn time to avoid repeated syscalls.
  #[cfg(unix)]
  pgid: Option<i32>,
}

#[derive(Debug)]
struct KillSignalInner {
  // WARNING: This should struct should not be made Sync.
  // Some of the code in this project depends on this not
  // being cancelled at any time by another thread. For example,
  // the abort code is checked before executing a sub process and
  // then awaited after. If an abort happened between that then
  // it could be missed.
  aborted_code: RefCell<Option<i32>>,
  sender: broadcast::Sender<SignalMessage>,
  children: RefCell<Vec<Weak<KillSignalInner>>>,
  /// Information about the current child process.
  child_process: Cell<ChildProcessInfo>,
}

impl KillSignalInner {
  pub fn send(&self, signal: SignalMessage) {
    if signal.kind.causes_abort() {
      let mut stored_aborted_code = self.aborted_code.borrow_mut();
      if stored_aborted_code.is_none() {
        *stored_aborted_code = Some(signal.kind.aborted_code());
      }
    }
    _ = self.sender.send(signal);

    // notify children
    self.children.borrow_mut().retain(|weak_child| {
      if let Some(child) = weak_child.upgrade() {
        child.send(signal);
        true
      } else {
        false // clean-up dropped children
      }
    });
  }
}

/// Used to send signals to commands.
#[derive(Debug, Clone)]
pub struct KillSignal(Rc<KillSignalInner>);

impl Default for KillSignal {
  fn default() -> Self {
    let (sender, _) = broadcast::channel(100);
    Self(Rc::new(KillSignalInner {
      aborted_code: RefCell::new(None),
      sender,
      children: Default::default(),
      child_process: Cell::new(ChildProcessInfo::default()),
    }))
  }
}

impl KillSignal {
  /// Exit code to use when aborted.
  pub fn aborted_code(&self) -> Option<i32> {
    *self.0.aborted_code.borrow()
  }

  /// Creates a signal that will only send signals to itself
  /// and all descendants--not the parent signal.
  pub fn child_signal(&self) -> Self {
    let (sender, _) = broadcast::channel(100);
    let child = Rc::new(KillSignalInner {
      aborted_code: RefCell::new(self.aborted_code()),
      sender,
      children: RefCell::new(Vec::new()),
      child_process: Cell::new(ChildProcessInfo::default()),
    });

    // Add the child to the parent's list of children
    self.0.children.borrow_mut().push(Rc::downgrade(&child));

    Self(child)
  }

  /// Creates a `DropKillSignalGuard` that will send a `SignalKind::SIGTERM` on drop.
  pub fn drop_guard(self) -> KillSignalDropGuard {
    self.drop_guard_with_kind(SignalKind::SIGTERM)
  }

  /// Creates a `DropKillSignalGuard` that will send the specified signal on drop.
  pub fn drop_guard_with_kind(self, kind: SignalKind) -> KillSignalDropGuard {
    KillSignalDropGuard {
      disarmed: Cell::new(false),
      kill_signal_kind: kind,
      signal: self,
    }
  }

  /// Send a signal to commands being run (programmatic, always forwarded).
  pub fn send(&self, signal: SignalKind) {
    self.0.send(SignalMessage::new(signal))
  }

  /// Send a signal that originated from a specific process group.
  ///
  /// Use this when forwarding signals that came from the terminal (e.g., Ctrl+C).
  /// Children in the same process group as `origin_pgid` will not receive
  /// a duplicate signal, since the terminal already sent it to them directly.
  #[cfg(unix)]
  pub fn send_from_pgid(&self, signal: SignalKind, origin_pgid: i32) {
    self.0.send(SignalMessage::from_pgid(signal, origin_pgid))
  }

  /// Waits for only signals deemed to abort a command.
  pub async fn wait_aborted(&self) -> SignalMessage {
    let mut receiver = self.0.sender.subscribe();
    loop {
      // unwrap is ok because we're holding a sender in `self`
      let signal = receiver.recv().await.unwrap();
      if signal.kind.causes_abort() {
        return signal;
      }
    }
  }

  /// Waits for any signal to be received.
  pub async fn wait_any(&self) -> SignalMessage {
    let mut receiver = self.0.sender.subscribe();
    // unwrap is ok because we're holding a sender in `self`
    receiver.recv().await.unwrap()
  }

  /// Returns the PID of the current foreground child process, if any.
  ///
  /// This is useful for signal forwarding scenarios where you need to check
  /// if the child process is in the same process group as the parent.
  pub fn current_child_pid(&self) -> Option<u32> {
    self.0.child_process.get().pid
  }

  /// Returns the PGID of the current foreground child process, if any.
  ///
  /// The PGID is cached at spawn time to avoid repeated syscalls.
  /// This is useful for determining whether to forward signals:
  /// if the child is in the same process group, the terminal's signal
  /// will already reach it directly.
  #[cfg(unix)]
  pub fn current_child_pgid(&self) -> Option<i32> {
    self.0.child_process.get().pgid
  }

  /// Called internally when a child process is spawned.
  ///
  /// On Unix, this also caches the child's PGID.
  pub(crate) fn set_child_process(&self, pid: u32) {
    #[cfg(unix)]
    let pgid = {
      // Cache the PGID at spawn time
      let pgid = unsafe { nix::libc::getpgid(pid as i32) };
      if pgid > 0 { Some(pgid) } else { None }
    };

    self.0.child_process.set(ChildProcessInfo {
      pid: Some(pid),
      #[cfg(unix)]
      pgid,
    });
  }

  /// Called internally when a child process exits.
  pub(crate) fn clear_child_process(&self) {
    self.0.child_process.set(ChildProcessInfo::default());
  }
}

/// Guard that on drop will send a signal on the associated `KillSignal`.
#[derive(Debug)]
pub struct KillSignalDropGuard {
  disarmed: Cell<bool>,
  kill_signal_kind: SignalKind,
  signal: KillSignal,
}

impl Drop for KillSignalDropGuard {
  fn drop(&mut self) {
    if !self.disarmed.get() {
      self.signal.send(self.kill_signal_kind);
    }
  }
}

impl KillSignalDropGuard {
  /// Prevent the drop guard from sending a signal on drop.
  pub fn disarm(&self) {
    self.disarmed.set(true);
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SignalKind {
  SIGTERM,
  SIGKILL,
  SIGABRT,
  SIGQUIT,
  SIGINT,
  SIGSTOP,
  Other(i32),
}

/// A signal message that includes origin information to prevent double-signaling.
///
/// When a signal originates from the terminal (e.g., Ctrl+C), it's sent to all
/// processes in the foreground process group. If the shell also forwards the
/// signal programmatically, children would receive it twice. The `origin_pgid`
/// field allows recipients to skip forwarding if the child is in the same
/// process group as the signal origin.
#[derive(Debug, Clone, Copy)]
pub struct SignalMessage {
  pub kind: SignalKind,
  /// The process group ID where this signal originated.
  /// If Some, children in the same PGID already received this signal
  /// from the terminal and should not be signaled again.
  /// If None, the signal is programmatic and should always be forwarded.
  #[cfg(unix)]
  pub origin_pgid: Option<i32>,
}

impl SignalMessage {
  /// Create a new signal message for programmatic signals (always forwarded).
  pub fn new(kind: SignalKind) -> Self {
    Self {
      kind,
      #[cfg(unix)]
      origin_pgid: None,
    }
  }

  /// Create a new signal message originating from a specific process group.
  /// Children in the same PGID will not receive a duplicate signal.
  #[cfg(unix)]
  pub fn from_pgid(kind: SignalKind, pgid: i32) -> Self {
    Self {
      kind,
      origin_pgid: Some(pgid),
    }
  }

  /// Check if a signal should be forwarded to a child process.
  /// Returns true if the signal should be forwarded, false if the child
  /// already received it from the terminal.
  #[cfg(unix)]
  pub fn should_forward_to(&self, child_pgid: Option<i32>) -> bool {
    match (self.origin_pgid, child_pgid) {
      (Some(origin), Some(child)) => origin != child,
      _ => true, // Forward if we can't determine (programmatic or unknown PGID)
    }
  }

  /// On non-Unix platforms, always forward signals.
  #[cfg(not(unix))]
  pub fn should_forward_to(&self, _child_pgid: Option<i32>) -> bool {
    true
  }
}

impl SignalKind {
  pub fn causes_abort(&self) -> bool {
    match self {
      SignalKind::SIGTERM
      | SignalKind::SIGKILL
      | SignalKind::SIGQUIT
      | SignalKind::SIGINT
      | SignalKind::SIGSTOP
      // does this make sense?
      | SignalKind::SIGABRT => true,
      SignalKind::Other(_) => false,
    }
  }

  pub fn aborted_code(&self) -> i32 {
    let value: i32 = (*self).into();
    128 + value
  }
}

impl From<i32> for SignalKind {
  fn from(value: i32) -> Self {
    #[cfg(unix)]
    match value {
      nix::libc::SIGINT => SignalKind::SIGINT,
      nix::libc::SIGQUIT => SignalKind::SIGQUIT,
      nix::libc::SIGABRT => SignalKind::SIGABRT,
      nix::libc::SIGKILL => SignalKind::SIGKILL,
      nix::libc::SIGTERM => SignalKind::SIGTERM,
      nix::libc::SIGSTOP => SignalKind::SIGSTOP,
      _ => SignalKind::Other(value),
    }
    #[cfg(not(unix))]
    match value {
      2 => SignalKind::SIGINT,
      3 => SignalKind::SIGQUIT,
      6 => SignalKind::SIGABRT,
      9 => SignalKind::SIGKILL,
      15 => SignalKind::SIGTERM,
      19 => SignalKind::SIGSTOP,
      _ => SignalKind::Other(value),
    }
  }
}

impl From<SignalKind> for i32 {
  fn from(kind: SignalKind) -> i32 {
    #[cfg(unix)]
    match kind {
      SignalKind::SIGINT => nix::libc::SIGINT,
      SignalKind::SIGQUIT => nix::libc::SIGQUIT,
      SignalKind::SIGABRT => nix::libc::SIGABRT,
      SignalKind::SIGKILL => nix::libc::SIGKILL,
      SignalKind::SIGTERM => nix::libc::SIGTERM,
      SignalKind::SIGSTOP => nix::libc::SIGSTOP,
      SignalKind::Other(value) => value,
    }
    #[cfg(not(unix))]
    match kind {
      SignalKind::SIGINT => 2,
      SignalKind::SIGQUIT => 3,
      SignalKind::SIGABRT => 6,
      SignalKind::SIGKILL => 9,
      SignalKind::SIGTERM => 15,
      SignalKind::SIGSTOP => 19,
      SignalKind::Other(value) => value,
    }
  }
}

#[cfg(test)]
mod test {
  use crate::KillSignal;
  use crate::SignalKind;

  #[tokio::test]
  async fn test_send_and_wait_any() {
    let kill_signal = KillSignal::default();

    // Spawn a task to send a signal
    let signal_sender = kill_signal.clone();
    deno_unsync::spawn(async move {
      signal_sender.send(SignalKind::SIGTERM);
    });

    // Wait for the signal in the main task
    let signal = kill_signal.wait_any().await;
    assert_eq!(signal.kind, SignalKind::SIGTERM);
  }

  #[tokio::test]
  async fn test_signal_propagation_to_child_and_grandchild() {
    let parent_signal = KillSignal::default();
    let child_signal = parent_signal.child_signal();
    let sibling_signal = parent_signal.child_signal();
    let grandchild_signal = child_signal.child_signal();

    // Spawn a task to send a signal from the parent
    let parent = parent_signal.clone();
    deno_unsync::spawn(async move {
      parent.send(SignalKind::SIGKILL);
    });

    let signals = futures::join!(
      child_signal.wait_any(),
      sibling_signal.wait_any(),
      grandchild_signal.wait_any()
    );

    for signal in [signals.0, signals.1, signals.2].into_iter() {
      assert_eq!(signal.kind, SignalKind::SIGKILL);
    }
    assert_eq!(child_signal.aborted_code(), Some(128 + 9));
    assert_eq!(sibling_signal.aborted_code(), Some(128 + 9));
    assert_eq!(grandchild_signal.aborted_code(), Some(128 + 9));
  }

  #[tokio::test]
  async fn test_signal_propagation_on_sub_tree() {
    let parent_signal = KillSignal::default();
    let child_signal = parent_signal.child_signal();
    let sibling_signal = parent_signal.child_signal();
    let grandchild_signal = child_signal.child_signal();
    let grandchild2_signal = child_signal.child_signal();

    child_signal.send(SignalKind::SIGABRT);

    assert!(parent_signal.aborted_code().is_none());
    assert!(sibling_signal.aborted_code().is_none());
    assert!(child_signal.aborted_code().is_some());
    assert!(grandchild_signal.aborted_code().is_some());
    assert!(grandchild2_signal.aborted_code().is_some());
  }

  #[tokio::test]
  async fn test_wait_aborted() {
    let kill_signal = KillSignal::default();

    // Spawn a task to send an aborting signal
    let signal_sender = kill_signal.clone();
    deno_unsync::spawn(async move {
      signal_sender.send(SignalKind::SIGABRT);
    });

    // Wait for the aborting signal in the main task
    let signal = kill_signal.wait_aborted().await;
    assert_eq!(signal.kind, SignalKind::SIGABRT);
    assert!(kill_signal.aborted_code().is_some());
  }

  #[tokio::test]
  async fn test_propagation_and_is_aborted_flag() {
    let parent_signal = KillSignal::default();
    let child_signal = parent_signal.child_signal();

    assert!(parent_signal.aborted_code().is_none());
    assert!(child_signal.aborted_code().is_none());

    // Send an aborting signal from the parent
    deno_unsync::spawn({
      let parent_signal = parent_signal.clone();
      async move {
        parent_signal.send(SignalKind::SIGQUIT);
      }
    });

    // Wait for the signal in the child
    let signal = child_signal.wait_aborted().await;
    assert_eq!(signal.kind, SignalKind::SIGQUIT);
    assert_eq!(parent_signal.aborted_code(), Some(128 + 3));
    assert_eq!(child_signal.aborted_code(), Some(128 + 3));
  }

  #[tokio::test]
  async fn test_dropped_child_signal_cleanup() {
    let parent_signal = KillSignal::default();

    // Create a child signal and immediately drop it
    {
      let child_signal = parent_signal.child_signal();
      assert!(child_signal.aborted_code().is_none());
    }

    // Send a signal from the parent
    deno_unsync::spawn({
      let parent_signal = parent_signal.clone();
      async move {
        parent_signal.send(SignalKind::SIGTERM);
      }
    });

    // Verify no panic occurred and the parent still functions
    let signal = parent_signal.wait_any().await;
    assert_eq!(signal.kind, SignalKind::SIGTERM);
  }

  #[tokio::test]
  async fn test_drop_guard() {
    let parent_signal = KillSignal::default();

    // Disarmed drop guard
    {
      let drop_guard = parent_signal.clone().drop_guard();
      drop_guard.disarm();
    }
    assert_eq!(parent_signal.aborted_code(), None);

    // Actually drop
    {
      let drop_guard = parent_signal.clone().drop_guard();
      drop(drop_guard);
    }
    assert_eq!(
      parent_signal.aborted_code(),
      Some(SignalKind::SIGTERM.aborted_code())
    );
  }

  #[test]
  fn test_kill_signal_child_process_tracking() {
    let kill_signal = KillSignal::default();

    // Initially no child process
    assert_eq!(kill_signal.current_child_pid(), None);
    #[cfg(unix)]
    assert_eq!(kill_signal.current_child_pgid(), None);

    // Set a child process
    kill_signal.set_child_process(1234);
    assert_eq!(kill_signal.current_child_pid(), Some(1234));
    // PGID is retrieved via syscall, so it might be None for a fake PID

    // Clear the child process
    kill_signal.clear_child_process();
    assert_eq!(kill_signal.current_child_pid(), None);
    #[cfg(unix)]
    assert_eq!(kill_signal.current_child_pgid(), None);
  }

  #[test]
  fn test_child_signal_has_separate_child_process_tracking() {
    let parent_signal = KillSignal::default();
    let child_signal = parent_signal.child_signal();

    // Set child process on parent
    parent_signal.set_child_process(1234);
    assert_eq!(parent_signal.current_child_pid(), Some(1234));
    // Child signal should not see parent's child process
    assert_eq!(child_signal.current_child_pid(), None);

    // Set child process on child signal
    child_signal.set_child_process(5678);
    assert_eq!(child_signal.current_child_pid(), Some(5678));
    // Parent should still see its own child process
    assert_eq!(parent_signal.current_child_pid(), Some(1234));
  }

  #[cfg(unix)]
  #[test]
  fn test_signal_message_should_forward_to() {
    use super::SignalMessage;

    // Programmatic signal (no origin_pgid) should always forward
    let programmatic = SignalMessage::new(SignalKind::SIGINT);
    assert!(programmatic.should_forward_to(Some(1000)));
    assert!(programmatic.should_forward_to(Some(2000)));
    assert!(programmatic.should_forward_to(None));

    // Signal from PGID 1000 should NOT forward to child in same PGID
    let from_tty = SignalMessage::from_pgid(SignalKind::SIGINT, 1000);
    assert!(!from_tty.should_forward_to(Some(1000))); // Same PGID - don't forward
    assert!(from_tty.should_forward_to(Some(2000))); // Different PGID - forward
    assert!(from_tty.should_forward_to(None)); // Unknown PGID - forward (safe default)
  }

  #[cfg(unix)]
  #[tokio::test]
  async fn test_send_from_pgid() {
    let kill_signal = KillSignal::default();

    // Spawn a task to send a signal from a specific PGID
    let signal_sender = kill_signal.clone();
    deno_unsync::spawn(async move {
      signal_sender.send_from_pgid(SignalKind::SIGINT, 12345);
    });

    // Wait for the signal
    let signal = kill_signal.wait_any().await;
    assert_eq!(signal.kind, SignalKind::SIGINT);
    assert_eq!(signal.origin_pgid, Some(12345));

    // Same PGID should not forward
    assert!(!signal.should_forward_to(Some(12345)));
    // Different PGID should forward
    assert!(signal.should_forward_to(Some(99999)));
  }
}
