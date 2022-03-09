use crate::test_builder::TestBuilder;

#[tokio::test]
pub async fn test_boolean_logic() {
  TestBuilder::new()
    .command("echo 1")
    .stdout("1\n")
    .run()
    .await;

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
