// Copyright 2018-2024 the Deno authors. MIT license.

use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;

/// Similar to `std::fs::canonicalize()` but strips UNC prefixes on Windows.
pub fn canonicalize_path(path: &Path) -> Result<PathBuf> {
  let path = path.canonicalize()?;
  #[cfg(windows)]
  return Ok(strip_unc_prefix(path));
  #[cfg(not(windows))]
  return Ok(path);
}

// todo(dsherret): This function was copy and pasted from deno
// so maybe we could extract it out to a separate crate in order
// to share the code.

#[cfg(windows)]
fn strip_unc_prefix(path: PathBuf) -> PathBuf {
  use std::path::Component;
  use std::path::Prefix;

  let mut components = path.components();
  match components.next() {
    Some(Component::Prefix(prefix)) => {
      match prefix.kind() {
        // \\?\device
        Prefix::Verbatim(device) => {
          let mut path = PathBuf::new();
          path.push(format!(r"\\{}\", device.to_string_lossy()));
          path.extend(components.filter(|c| !matches!(c, Component::RootDir)));
          path
        }
        // \\?\c:\path
        Prefix::VerbatimDisk(_) => {
          let mut path = PathBuf::new();
          path.push(prefix.as_os_str().to_string_lossy().replace(r"\\?\", ""));
          path.extend(components);
          path
        }
        // \\?\UNC\hostname\share_name\path
        Prefix::VerbatimUNC(hostname, share_name) => {
          let mut path = PathBuf::new();
          path.push(format!(
            r"\\{}\{}\",
            hostname.to_string_lossy(),
            share_name.to_string_lossy()
          ));
          path.extend(components.filter(|c| !matches!(c, Component::RootDir)));
          path
        }
        _ => path,
      }
    }
    _ => path,
  }
}
