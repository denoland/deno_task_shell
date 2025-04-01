// Copyright 2018-2024 the Deno authors. MIT license.

use std::path::Path;
use std::path::PathBuf;

use anyhow::Result;

/// Similar to `std::fs::canonicalize()` but strips UNC prefixes on Windows.
pub fn canonicalize_path(path: &Path) -> Result<PathBuf> {
  Ok(deno_path_util::strip_unc_prefix(path.canonicalize()?))
}
