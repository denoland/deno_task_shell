use anyhow::Result;
use std::sync::Arc;

use tokio::process::Child;

/// Windows does not have a concept of parent processes and so
/// killing the deno task process will not also kill any spawned
/// processes by default. To make this work, we can use winapi's
/// jobs api, which allows for associating a main process so that
/// when the main process terminates, it will also terminate the
/// associated processes.
///
/// Read more: https://stackoverflow.com/questions/3342941/kill-child-process-when-parent-process-is-killed
#[derive(Clone)]
pub struct ChildProcessTracker(Arc<dyn Tracker>);

impl ChildProcessTracker {
  #[cfg(windows)]
  pub fn new() -> Self {
    match windows::WinChildProcessTracker::new() {
      Ok(tracker) => Self(Arc::new(tracker)),
      Err(err) => {
        if cfg!(debug_assertions) {
          panic!("Could not start tracking processes. {:#}", err);
        } else {
          // fallback to not tracking processes if this fails
          Self(Arc::new(NullChildProcessTracker))
        }
      }
    }
  }

  #[cfg(not(windows))]
  pub fn new() -> Self {
    Self(Arc::new(NullChildProcessTracker))
  }

  pub fn track(&self, child: &Child) {
    if let Err(err) = self.0.track(child) {
      if cfg!(debug_assertions) {
        panic!("Could not track process: {:#}", err);
      }
    }
  }
}

trait Tracker: Send + Sync {
  fn track(&self, child: &Child) -> Result<()>;
}

struct NullChildProcessTracker;

impl Tracker for NullChildProcessTracker {
  fn track(&self, _: &Child) -> Result<()> {
    Ok(())
  }
}

#[cfg(target_os = "windows")]
mod windows {
  use anyhow::bail;
  use anyhow::Result;
  use std::ptr;
  use tokio::process::Child;
  use winapi::shared::minwindef::DWORD;
  use winapi::shared::minwindef::LPVOID;
  use winapi::shared::minwindef::TRUE;
  use winapi::um::handleapi::INVALID_HANDLE_VALUE;
  use winapi::um::jobapi2::AssignProcessToJobObject;
  use winapi::um::jobapi2::CreateJobObjectW;
  use winapi::um::jobapi2::SetInformationJobObject;
  use winapi::um::winnt::JobObjectExtendedLimitInformation;
  use winapi::um::winnt::HANDLE;
  use winapi::um::winnt::JOBOBJECT_EXTENDED_LIMIT_INFORMATION;
  use winapi::um::winnt::JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

  use super::Tracker;

  pub struct WinChildProcessTracker {
    handle: WinHandle,
  }

  impl WinChildProcessTracker {
    pub fn new() -> Result<Self> {
      unsafe {
        let handle = CreateJobObjectW(ptr::null_mut(), ptr::null());
        let mut info: JOBOBJECT_EXTENDED_LIMIT_INFORMATION = std::mem::zeroed();
        info.BasicLimitInformation.LimitFlags =
          JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        let result = SetInformationJobObject(
          handle,
          JobObjectExtendedLimitInformation,
          &mut info as *mut _ as LPVOID,
          std::mem::size_of_val(&info) as DWORD,
        );
        if result != TRUE {
          bail!(
            "Could not set job information object. {:#}",
            std::io::Error::last_os_error()
          );
        }

        Ok(Self {
          handle: WinHandle::new(handle),
        })
      }
    }

    unsafe fn add_process_handle(&self, process_handle: HANDLE) -> Result<()> {
      let result =
        AssignProcessToJobObject(self.handle.as_raw_handle(), process_handle);
      if result != TRUE {
        bail!(
          "Could not assign process to job object. {:#}",
          std::io::Error::last_os_error()
        );
      } else {
        Ok(())
      }
    }
  }

  impl Tracker for WinChildProcessTracker {
    fn track(&self, child: &Child) -> Result<()> {
      if let Some(handle) = child.raw_handle() {
        unsafe { self.add_process_handle(handle) }
      } else {
        // process exited... ignore
        Ok(())
      }
    }
  }

  struct WinHandle {
    inner: HANDLE,
  }

  impl WinHandle {
    pub fn new(handle: HANDLE) -> Self {
      WinHandle { inner: handle }
    }

    pub fn as_raw_handle(&self) -> HANDLE {
      self.inner
    }
  }

  unsafe impl Send for WinHandle {}
  unsafe impl Sync for WinHandle {}

  impl Drop for WinHandle {
    fn drop(&mut self) {
      unsafe {
        if !self.inner.is_null() && self.inner != INVALID_HANDLE_VALUE {
          winapi::um::handleapi::CloseHandle(self.inner);
        }
      }
    }
  }
}
