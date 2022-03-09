use crate::test_builder::TestBuilder;

#[tokio::test]
pub async fn test_commands() {
  TestBuilder::new()
    .command("echo 1")
    .stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 2   3")
    .stdout("1 2 3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(
      r#"VAR=1 deno eval 'console.log(Deno.env.get("VAR"))' && echo $VAR"#,
    )
    .stdout("1\n\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"VAR=1 VAR2=2 deno eval 'console.log(Deno.env.get("VAR") + Deno.env.get("VAR2"))'"#)
    .stdout("12\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_boolean_logic() {
  TestBuilder::new()
    .command("echo 1 && echo 2 || echo 3")
    .stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 || echo 2 && echo 3")
    .stdout("1\n3\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 || (echo 2 && echo 3)")
    .stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("false || false || (echo 2 && false) || echo 3")
    .stdout("2\n3\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_exit() {
  TestBuilder::new()
    .command("exit 1")
    .exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("exit 5")
    .exit_code(5)
    .run()
    .await;

  TestBuilder::new()
    .command("exit 258 && echo 1")
    .exit_code(2)
    .run()
    .await;

  TestBuilder::new()
    .command("(exit 0) && echo 1")
    .stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("(exit 1) && echo 1")
    .exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 && (exit 1)")
    .stdout("1\n")
    .exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("exit ; echo 2")
    .exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn test_async_commands() {
  TestBuilder::new()
    .command("sleep 0.25 && echo 2 & echo 1")
    .stdout("1\n2\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_command_substition() {
  TestBuilder::new()
    .command("echo $(echo 1)")
    .stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo $(echo 1 && echo 2)")
    .stdout("1 2\n")
    .run()
    .await;

  // async inside subshell should wait
  TestBuilder::new()
    .command("$(sleep 0.1 && echo 1 & echo echo) 2")
    .stdout("1 2\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_shell_variables() {
  TestBuilder::new()
    .command(r#"echo $VAR && VAR=1 && echo $VAR && deno eval 'console.log(Deno.env.get("VAR"))'"#)
    .stdout("\n1\nundefined\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"VAR=1 && echo Test$VAR && echo $(echo "Test: $VAR") ; echo Final$($VAR)"#)
    .stdout("Test1\nTest: 1\nFinal\n")
    .stderr("1: command not found\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_env_variables() {
  TestBuilder::new()
    .command(r#"echo $VAR && export VAR=1 && echo $VAR && deno eval 'console.log(Deno.env.get("VAR"))'"#)
    .stdout("\n1\n1\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_sequential_lists() {
  TestBuilder::new()
    .command(r#"echo 1 ; sleep 0.1 && echo 4 & echo 2 ; echo 3;"#)
    .stdout("1\n2\n3\n4\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn test_mkdir() {
  TestBuilder::new()
    .command("mkdir file.txt || echo 2")
    .file("file.txt", "test")
    .stdout("2\n")
    .stderr("mkdir: cannot create directory 'file.txt': File exists\n")
    .run()
    .await;
}
