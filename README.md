# deno_task_shell

[![](https://img.shields.io/crates/v/deno_task_shell.svg)](https://crates.io/crates/deno_task_shell)

```rs
// parse
let list = deno_task_shell::parser::parse(&text)?;

// execute
let env_vars = HashMap::from(&[
  ("SOME_VAR".to_string(), "value".to_string()),
]);
let cwd = std::env::current_dir()?;

let exit_code = deno_task_shell::execute(
  list,
  env_vars,
  &cwd,
  Default::default(), // custom commands
).await;
```
