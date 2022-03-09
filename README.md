# deno_ast

[![](https://img.shields.io/crates/v/deno_task_shell.svg)](https://crates.io/crates/deno_task_shell)

```rs
// parse
let list = deno_task_shell::parser::parse(&text)?;

// execute
let env_vars = HashMap::from(&[
  ("SOME_VAR", "value"),
]);
let cwd = std::env::current_dir()?;
deno_task_shell::execute(
  list,
  env_vars,
  &cwd,
).await;
```
