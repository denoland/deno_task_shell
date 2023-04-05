// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use futures::FutureExt;

use super::test_builder::TestBuilder;
use super::types::ExecuteResult;

const FOLDER_SEPERATOR: char = if cfg!(windows) { '\\' } else { '/' };

#[tokio::test]
pub async fn commands() {
  TestBuilder::new()
    .command("echo 1")
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 2   3")
    .assert_stdout("1 2 3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo "1 2   3""#)
    .assert_stdout("1 2   3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo 1 2\ \ \ 3"#)
    .assert_stdout("1 2   3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo "1 2\ \ \ 3""#)
    .assert_stdout("1 2\\ \\ \\ 3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo test$(echo "1    2")"#)
    .assert_stdout("test1 2\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"TEST="1   2" ; echo $TEST"#)
    .assert_stdout("1 2\n")
    .run()
    .await;

  TestBuilder::new()
    .command(
      r#"VAR=1 deno eval 'console.log(Deno.env.get("VAR"))' && echo $VAR"#,
    )
    .assert_stdout("1\n\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"VAR=1 VAR2=2 deno eval 'console.log(Deno.env.get("VAR") + Deno.env.get("VAR2"))'"#)
    .assert_stdout("12\n")
    .run()
    .await;

  TestBuilder::new()
    .command(
      r#"EMPTY= deno eval 'console.log(`EMPTY: ${Deno.env.get("EMPTY")}`)'"#,
    )
    .assert_stdout("EMPTY: \n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#""echo" "1""#)
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#""echo" "*""#)
    .assert_stdout("*\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo test-dashes")
    .assert_stdout("test-dashes\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 'a/b'/c")
    .assert_stdout("a/b/c\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 'a/b'ctest\"te  st\"'asdf'")
    .assert_stdout("a/bctestte  stasdf\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo --test=\"2\" --test='2' test\"TEST\" TEST'test'TEST 'test''test' test'test'\"test\" \"test\"\"test\"'test'")
    .assert_stdout("--test=2 --test=2 testTEST TESTtestTEST testtest testtesttest testtesttest\n")
    .run()
    .await;

  TestBuilder::new()
    .command("deno eval 'console.log(1)'")
    .env_var("PATH", "")
    .assert_stderr("deno: command not found\n")
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn boolean_logic() {
  TestBuilder::new()
    .command("echo 1 && echo 2 || echo 3")
    .assert_stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 || echo 2 && echo 3")
    .assert_stdout("1\n3\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 || (echo 2 && echo 3)")
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("false || false || (echo 2 && false) || echo 3")
    .assert_stdout("2\n3\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn exit() {
  TestBuilder::new()
    .command("exit 1")
    .assert_exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("exit 5")
    .assert_exit_code(5)
    .run()
    .await;

  TestBuilder::new()
    .command("exit 258 && echo 1")
    .assert_exit_code(2)
    .run()
    .await;

  TestBuilder::new()
    .command("(exit 0) && echo 1")
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("(exit 1) && echo 1")
    .assert_exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("echo 1 && (exit 1)")
    .assert_stdout("1\n")
    .assert_exit_code(1)
    .run()
    .await;

  TestBuilder::new()
    .command("exit ; echo 2")
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn async_commands() {
  TestBuilder::new()
    .command("sleep 0.1 && echo 2 & echo 1")
    .assert_stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    .command("(sleep 0.1 && echo 2 &) ; echo 1")
    .assert_stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    .command("(sleep 0.1 && echo 2) & echo 1")
    .assert_stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    .command(
      "$(sleep 0.1 && echo 1 & $(sleep 0.2 && echo 2 & echo echo) & echo echo)",
    )
    .assert_stdout("1 2\n")
    .run()
    .await;

  TestBuilder::new()
    .command("exit 1 & exit 0")
    .assert_exit_code(1)
    .run()
    .await;

  // should not output because the `exit 1` will cancel the sleep
  TestBuilder::new()
    .command("sleep 5 && echo 1 & exit 1")
    .assert_exit_code(1)
    .run()
    .await;

  // should fail when async command exits
  TestBuilder::new()
    .command("exit 1 & exit 0")
    .assert_exit_code(1)
    .run()
    .await;

  // should fail when async command fails and cancel any running command
  TestBuilder::new()
    .command("deno eval 'Deno.exit(1)' & sleep 5 && echo 2 & echo 1")
    .assert_stdout("1\n")
    .assert_exit_code(1)
    .run()
    .await;

  // should cancel running command
  TestBuilder::new()
    .command("sleep 10 & sleep 0.5 && deno eval 'Deno.exit(2)' & deno eval 'console.log(1); setTimeout(() => { console.log(3) }, 10_000);'")
    .assert_stdout("1\n")
    .assert_exit_code(2)
    .run()
    .await;

  // should be able to opt out by doing an `|| exit 0`
  TestBuilder::new()
    .command("deno eval 'Deno.exit(1)' || exit 0 & echo 1")
    .assert_stdout("1\n")
    .assert_exit_code(0)
    .run()
    .await;
}

#[tokio::test]
pub async fn command_substition() {
  TestBuilder::new()
    .command("echo $(echo 1)")
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo $(echo 1 && echo 2)")
    .assert_stdout("1 2\n")
    .run()
    .await;

  // async inside subshell should wait
  TestBuilder::new()
    .command("$(sleep 0.1 && echo 1 & echo echo) 2")
    .assert_stdout("1 2\n")
    .run()
    .await;
  TestBuilder::new()
    .command("$(sleep 0.1 && echo 1 && exit 5 &) ; echo 2")
    .assert_stdout("2\n")
    .assert_stderr("1: command not found\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn shell_variables() {
  TestBuilder::new()
    .command(r#"echo $VAR && VAR=1 && echo $VAR && deno eval 'console.log(Deno.env.get("VAR"))'"#)
    .assert_stdout("\n1\nundefined\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"VAR=1 && echo $VAR$VAR"#)
    .assert_stdout("11\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"VAR=1 && echo Test$VAR && echo $(echo "Test: $VAR") ; echo CommandSub$($VAR); echo $ ; echo \$VAR"#)
    .assert_stdout("Test1\nTest: 1\nCommandSub\n$\n$VAR\n")
    .assert_stderr("1: command not found\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn env_variables() {
  TestBuilder::new()
    .command(r#"echo $VAR && export VAR=1 && echo $VAR && deno eval 'console.log(Deno.env.get("VAR"))'"#)
    .assert_stdout("\n1\n1\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"export VAR=1 VAR2=testing VAR3="test this out" && echo $VAR $VAR2 $VAR3"#)
    .assert_stdout("1 testing test this out\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn sequential_lists() {
  TestBuilder::new()
    .command(r#"echo 1 ; sleep 0.1 && echo 4 & echo 2 ; echo 3;"#)
    .assert_stdout("1\n2\n3\n4\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn pipeline() {
  TestBuilder::new()
    .command(r#"echo 1 | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo 1 | echo 2 && echo 3"#)
    .assert_stdout("2\n3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo $(sleep 0.1 && echo 2 & echo 1) | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1 2\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo 2 | echo 1 | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"deno eval 'console.log(1); console.error(2);' | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1\n")
    .assert_stderr("2\n")
    .run()
    .await;

  // stdout and stderr pipeline

  TestBuilder::new()
    .command(r#"deno eval 'console.log(1); console.error(2);' |& deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1\n2\n")
    .run()
    .await;

  TestBuilder::new()
    // add bit of a delay while outputting stdout so that it doesn't race with stderr
    .command(r#"deno eval 'console.log(1); console.error(2);' | deno eval --unstable 'setTimeout(async () => { await Deno.stdin.readable.pipeTo(Deno.stderr.writable) }, 10)' |& deno eval 'await Deno.stdin.readable.pipeTo(Deno.stderr.writable)'"#)
    // still outputs 2 because the first command didn't pipe stderr
    .assert_stderr("2\n1\n")
    .run()
    .await;

  // |& pipeline should still pipe stdout
  TestBuilder::new()
    .command(r#"echo 1 |& deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)'"#)
    .assert_stdout("1\n")
    .run()
    .await;

  // pipeline with redirect
  TestBuilder::new()
    .command(r#"echo 1 | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stdout.writable)' > output.txt"#)
    .assert_file_equals("output.txt", "1\n")
    .run()
    .await;

  // pipeline with stderr redirect
  TestBuilder::new()
    .command(r#"echo 1 | deno eval 'await Deno.stdin.readable.pipeTo(Deno.stderr.writable)' 2> output.txt"#)
    .assert_file_equals("output.txt", "1\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn negated() {
  TestBuilder::new()
    .command(r#"! echo 1 && echo 2"#)
    .assert_stdout("1\n")
    .assert_exit_code(1)
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! echo 1 || echo 2"#)
    .assert_stdout("1\n2\n")
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! (echo 1 | echo 2 && echo 3) || echo 4"#)
    .assert_stdout("2\n3\n4\n")
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! echo 1 | echo 2 && echo 3"#)
    .assert_stdout("2\n")
    .assert_exit_code(1)
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! (exit 5) && echo 1"#)
    .assert_stdout("1\n")
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! exit 5 && echo 1"#)
    .assert_exit_code(5)
    .run()
    .await;
  TestBuilder::new()
    .command(r#"! echo 1 && echo 2 &"#)
    .assert_stdout("1\n")
    // differing behaviour to shells, where this async command will actually fail
    .assert_exit_code(1)
    .run()
    .await;

  // test no spaces
  TestBuilder::new()
    .command(r#"!echo 1 && echo 2"#)
    .assert_stderr("History expansion is not supported:\n  !echo\n  ~\n\nPerhaps you meant to add a space after the exclamation point to negate the command?\n  ! echo\n")
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn redirects() {
  TestBuilder::new()
    .command(r#"echo 5 6 7 > test.txt"#)
    .assert_file_equals("test.txt", "5 6 7\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo 1 2 3 && echo 1 > test.txt"#)
    .assert_stdout("1 2 3\n")
    .assert_file_equals("test.txt", "1\n")
    .run()
    .await;

  // subdir
  TestBuilder::new()
    .command(r#"mkdir subdir && cd subdir && echo 1 2 3 > test.txt"#)
    .assert_file_equals("subdir/test.txt", "1 2 3\n")
    .run()
    .await;

  // absolute path
  TestBuilder::new()
    .command(r#"echo 1 2 3 > "$PWD/test.txt""#)
    .assert_file_equals("test.txt", "1 2 3\n")
    .run()
    .await;

  // stdout
  TestBuilder::new()
    .command(r#"deno eval 'console.log(1); console.error(5)' 1> test.txt"#)
    .assert_stderr("5\n")
    .assert_file_equals("test.txt", "1\n")
    .run()
    .await;

  // stderr
  TestBuilder::new()
    .command(r#"deno eval 'console.log(1); console.error(5)' 2> test.txt"#)
    .assert_stdout("1\n")
    .assert_file_equals("test.txt", "5\n")
    .run()
    .await;

  // invalid fd
  TestBuilder::new()
    .command(r#"echo 2 3> test.txt"#)
    .ensure_temp_dir()
    .assert_stderr(
      "only redirecting to stdout (1) and stderr (2) is supported\n",
    )
    .assert_exit_code(1)
    .run()
    .await;

  // /dev/null
  TestBuilder::new()
    .command(r#"deno eval 'console.log(1); console.error(5)' 2> /dev/null"#)
    .assert_stdout("1\n")
    .run()
    .await;

  // appending
  TestBuilder::new()
    .command(r#"echo 1 > test.txt && echo 2 >> test.txt"#)
    .assert_file_equals("test.txt", "1\n2\n")
    .run()
    .await;

  // &> and &>> redirect
  TestBuilder::new()
    .command(
      concat!(
        "deno eval 'console.log(1); setTimeout(() => console.error(23), 10)' &> file.txt &&",
        "deno eval 'console.log(456); setTimeout(() => console.error(789), 10)' &>> file.txt"
      )
    )
    .assert_file_equals("file.txt", "1\n23\n456\n789\n")
    .run()
    .await;

  // multiple arguments after re-direct
  TestBuilder::new()
    .command(r#"export TwoArgs=testing\ this && echo 1 > $TwoArgs"#)
    .assert_stderr(concat!(
      "redirect path must be 1 argument, but found 2 ",
      "(testing this). Did you mean to quote it (ex. \"testing this\")?\n"
    ))
    .assert_exit_code(1)
    .run()
    .await;

  // zero arguments after re-direct
  TestBuilder::new()
    .command(r#"echo 1 > $EMPTY"#)
    .assert_stderr("redirect path must be 1 argument, but found 0\n")
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn pwd() {
  TestBuilder::new()
    .directory("sub_dir")
    .file("file.txt", "test")
    .command("pwd && cd sub_dir && pwd && cd ../ && pwd")
    // the actual temp directory will get replaced here
    .assert_stdout(&format!(
      "$TEMP_DIR\n$TEMP_DIR{FOLDER_SEPERATOR}sub_dir\n$TEMP_DIR\n"
    ))
    .run()
    .await;

  TestBuilder::new()
    .command("pwd -M")
    .assert_stderr("pwd: unsupported flag: -M\n")
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
#[cfg(unix)]
pub async fn pwd_logical() {
  TestBuilder::new()
    .directory("main")
    .command("ln -s main symlinked_main && cd symlinked_main && pwd && pwd -L")
    .assert_stdout("$TEMP_DIR/symlinked_main\n$TEMP_DIR/main\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn cat() {
  // no args
  TestBuilder::new()
    .command("cat")
    .stdin("hello")
    .assert_stdout("hello")
    .run()
    .await;

  // dash
  TestBuilder::new()
    .command("cat -")
    .stdin("hello")
    .assert_stdout("hello")
    .run()
    .await;

  // file
  TestBuilder::new()
    .command("cat file")
    .file("file", "test")
    .assert_stdout("test")
    .run()
    .await;

  // multiple files
  TestBuilder::new()
    .command("cat file1 file2")
    .file("file1", "test")
    .file("file2", "other")
    .assert_stdout("testother")
    .run()
    .await;

  // multiple files and stdin
  TestBuilder::new()
    .command("cat file1 file2 -")
    .file("file1", "test\n")
    .file("file2", "other\n")
    .stdin("hello")
    .assert_stdout("test\nother\nhello")
    .run()
    .await;

  // multiple files and stdin different order
  TestBuilder::new()
    .command("cat file1 - file2")
    .file("file1", "test\n")
    .file("file2", "other\n")
    .stdin("hello\n")
    .assert_stdout("test\nhello\nother\n")
    .run()
    .await;

  // file containing a command to evaluate
  TestBuilder::new()
    .command("$(cat file)")
    .file("file", "echo hello")
    .assert_stdout("hello\n")
    .run()
    .await;
}

// Basic integration tests as there are unit tests in the commands
#[tokio::test]
pub async fn mv() {
  // single file
  TestBuilder::new()
    .command("mv file1.txt file2.txt")
    .file("file1.txt", "test")
    .assert_not_exists("file1.txt")
    .assert_exists("file2.txt")
    .run()
    .await;

  // multiple files to folder
  TestBuilder::new()
    .command("mkdir sub_dir && mv file1.txt file2.txt sub_dir")
    .file("file1.txt", "test1")
    .file("file2.txt", "test2")
    .assert_not_exists("file1.txt")
    .assert_not_exists("file2.txt")
    .assert_exists("sub_dir/file1.txt")
    .assert_exists("sub_dir/file2.txt")
    .run()
    .await;

  // error message
  TestBuilder::new()
    .command("mv file1.txt file2.txt")
    .assert_exit_code(1)
    .assert_stderr(&format!(
      "mv: could not move file1.txt to file2.txt: {}\n",
      no_such_file_error_text()
    ))
    .run()
    .await;
}

// Basic integration tests as there are unit tests in the commands
#[tokio::test]
pub async fn cp() {
  // single file
  TestBuilder::new()
    .command("cp file1.txt file2.txt")
    .file("file1.txt", "test")
    .assert_exists("file1.txt")
    .assert_exists("file2.txt")
    .run()
    .await;

  // multiple files to folder
  TestBuilder::new()
    .command("mkdir sub_dir && cp file1.txt file2.txt sub_dir")
    .file("file1.txt", "test1")
    .file("file2.txt", "test2")
    .assert_exists("file1.txt")
    .assert_exists("file2.txt")
    .assert_exists("sub_dir/file1.txt")
    .assert_exists("sub_dir/file2.txt")
    .run()
    .await;

  // error message
  TestBuilder::new()
    .command("cp file1.txt file2.txt")
    .assert_exit_code(1)
    .assert_stderr(&format!(
      "cp: could not copy file1.txt to file2.txt: {}\n",
      no_such_file_error_text()
    ))
    .run()
    .await;
}

// Basic integration tests as there are unit tests in the commands
#[tokio::test]
pub async fn mkdir() {
  TestBuilder::new()
    .command("mkdir sub_dir")
    .assert_exists("sub_dir")
    .run()
    .await;

  // error message
  TestBuilder::new()
    .command("mkdir file.txt")
    .file("file.txt", "test")
    .assert_stderr("mkdir: cannot create directory 'file.txt': File exists\n")
    .assert_exit_code(1)
    .run()
    .await;
}

// Basic integration tests as there are unit tests in the commands
#[tokio::test]
pub async fn rm() {
  TestBuilder::new()
    .command("mkdir sub_dir && rm -d sub_dir && rm file.txt")
    .file("file.txt", "")
    .assert_not_exists("sub_dir")
    .assert_not_exists("file.txt")
    .run()
    .await;

  // error message
  TestBuilder::new()
    .command("rm file.txt")
    .assert_stderr(&format!(
      "rm: cannot remove 'file.txt': {}\n",
      no_such_file_error_text()
    ))
    .assert_exit_code(1)
    .run()
    .await;
}

#[tokio::test]
pub async fn xargs() {
  TestBuilder::new()
    .command("echo '1   2   3  ' | xargs")
    .assert_stdout("1 2 3\n")
    .run()
    .await;

  TestBuilder::new()
    .command("echo '1   2 \t\t\t3  ' | xargs echo test")
    .assert_stdout("test 1 2 3\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"deno eval "console.log('testing\nthis')" | xargs"#)
    .assert_stdout("testing this\n")
    .run()
    .await;

  // \n delimiter
  TestBuilder::new()
    .command(r#"deno eval "console.log('testing this out\n\ntest\n')" | xargs -d \n deno eval "console.log(Deno.args)""#)
    .assert_stdout("[ \"testing this out\", \"\", \"test\", \"\" ]\n")
    .run()
    .await;

  // \0 delimiter
  TestBuilder::new()
    .command(r#"deno eval "console.log('testing this out\ntest\0other')" | xargs -0 deno eval "console.log(Deno.args)""#)
    .assert_stdout("[ \"testing this out\\ntest\", \"other\\n\" ]\n")
    .run()
    .await;

  // unmatched single quote
  TestBuilder::new()
    .command(r#"deno eval "console.log(\"'test\")" | xargs"#)
    .assert_stderr("xargs: unmatched quote; by default quotes are special to xargs unless you use the -0 option\n")
    .assert_exit_code(1)
    .run()
    .await;

  // unmatched double quote
  TestBuilder::new()
    .command(r#"deno eval "console.log('\"test')" | xargs"#)
    .assert_stderr("xargs: unmatched quote; by default quotes are special to xargs unless you use the -0 option\n")
    .assert_exit_code(1)
    .run()
    .await;

  // test reading env file
  TestBuilder::new()
    .file(
      ".env",
      r#"VAR1="testing"
VAR2="other"
"#,
    )
    // most likely people would want to do `export $(grep -v '^#' .env | xargs)` though
    // in order to remove comments...
    .command("export $(cat .env | xargs) && echo $VAR1 $VAR2")
    .assert_stdout("testing other\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn stdin() {
  TestBuilder::new()
    .command(r#"deno eval "const b = new Uint8Array(1);Deno.stdin.readSync(b);console.log(b)" && deno eval "const b = new Uint8Array(1);Deno.stdin.readSync(b);console.log(b)""#)
    .stdin("12345")
    .assert_stdout("Uint8Array(1) [ 49 ]\nUint8Array(1) [ 50 ]\n")
    .run()
    .await;

  TestBuilder::new()
    .command(r#"echo "12345" | (deno eval "const b = new Uint8Array(1);Deno.stdin.readSync(b);console.log(b)" && deno eval "const b = new Uint8Array(1);Deno.stdin.readSync(b);console.log(b)")"#)
    .stdin("55555") // should not use this because stdin is piped from the echo
    .assert_stdout("Uint8Array(1) [ 49 ]\nUint8Array(1) [ 50 ]\n")
    .run()
    .await;
}

#[cfg(windows)]
#[tokio::test]
pub async fn windows_resolve_command() {
  // not cross platform, but still allow this
  TestBuilder::new()
    .command("deno.exe eval 'console.log(1)'")
    .assert_stdout("1\n")
    .run()
    .await;

  TestBuilder::new()
    .command("deno eval 'console.log(1)'")
    // handle trailing semi-colon
    .env_var("PATHEXT", ".EXE;")
    .assert_stdout("1\n")
    .run()
    .await;
}

#[tokio::test]
pub async fn custom_command() {
  // not cross platform, but still allow this
  TestBuilder::new()
    .command("add 1 2")
    .custom_command(
      "add",
      Box::new(|mut context| {
        async move {
          let mut sum = 0;
          for val in context.args {
            sum += val.parse::<usize>().unwrap();
          }
          let _ = context.stderr.write_line(&sum.to_string());
          ExecuteResult::from_exit_code(0)
        }
        .boxed_local()
      }),
    )
    .assert_stderr("3\n")
    .run()
    .await;
}

fn no_such_file_error_text() -> &'static str {
  if cfg!(windows) {
    "The system cannot find the file specified. (os error 2)"
  } else {
    "No such file or directory (os error 2)"
  }
}
