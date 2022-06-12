// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;

use crate::combinators::*;

// Shell grammar rules this is loosely based on:
// https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_10_02

#[derive(Debug, Clone, PartialEq)]
pub struct SequentialList {
  pub items: Vec<SequentialListItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SequentialListItem {
  pub is_async: bool,
  pub sequence: Sequence,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Sequence {
  /// `export MY_VAR=5`
  EnvVar(EnvVar),
  /// `MY_VAR=5`
  ShellVar(EnvVar),
  /// `cmd_name <args...>`, `cmd1 | cmd2`
  Pipeline(Pipeline),
  /// `cmd1 && cmd2 || cmd3`
  BooleanList(Box<BooleanList>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline {
  /// `! pipeline`
  pub negated: bool,
  pub inner: PipelineInner,
}

impl From<Pipeline> for Sequence {
  fn from(p: Pipeline) -> Self {
    Sequence::Pipeline(p)
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PipelineInner {
  /// Ex. `cmd_name <args...>`
  Command(Command),
  /// `cmd1 | cmd2`
  PipeSequence(Box<PipeSequence>),
}

impl From<PipeSequence> for PipelineInner {
  fn from(p: PipeSequence) -> Self {
    PipelineInner::PipeSequence(Box::new(p))
  }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BooleanListOperator {
  // &&
  And,
  // ||
  Or,
}

impl BooleanListOperator {
  pub fn as_str(&self) -> &'static str {
    match self {
      BooleanListOperator::And => "&&",
      BooleanListOperator::Or => "||",
    }
  }

  pub fn moves_next_for_exit_code(&self, exit_code: i32) -> bool {
    *self == BooleanListOperator::Or && exit_code != 0
      || *self == BooleanListOperator::And && exit_code == 0
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanList {
  pub current: Sequence,
  pub op: BooleanListOperator,
  pub next: Sequence,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PipeSequenceOperator {
  // |
  Stdout,
  // |&
  StdoutStderr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PipeSequence {
  pub current: Command,
  pub op: PipeSequenceOperator,
  pub next: PipelineInner,
}

impl From<PipeSequence> for Sequence {
  fn from(p: PipeSequence) -> Self {
    Sequence::Pipeline(Pipeline {
      negated: false,
      inner: p.into(),
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Command {
  pub inner: CommandInner,
  pub redirect: Option<Redirect>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommandInner {
  /// `cmd_name <args...>`
  Simple(SimpleCommand),
  /// `(list)`
  Subshell(Box<SequentialList>),
}

impl From<Command> for Sequence {
  fn from(c: Command) -> Self {
    Pipeline {
      negated: false,
      inner: c.into(),
    }
    .into()
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleCommand {
  pub env_vars: Vec<EnvVar>,
  pub args: Vec<StringOrWord>,
}

impl From<SimpleCommand> for Command {
  fn from(c: SimpleCommand) -> Self {
    Command {
      redirect: None,
      inner: CommandInner::Simple(c),
    }
  }
}

impl From<SimpleCommand> for PipelineInner {
  fn from(c: SimpleCommand) -> Self {
    PipelineInner::Command(c.into())
  }
}

impl From<Command> for PipelineInner {
  fn from(c: Command) -> Self {
    PipelineInner::Command(c)
  }
}

impl From<SimpleCommand> for Sequence {
  fn from(c: SimpleCommand) -> Self {
    let command: Command = c.into();
    command.into()
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvVar {
  pub name: String,
  pub value: StringOrWord,
}

impl EnvVar {
  pub fn new(name: String, value: StringOrWord) -> Self {
    EnvVar { name, value }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringOrWord {
  Word(Vec<StringPart>),
  String(Vec<StringPart>),
}

impl StringOrWord {
  pub fn new_string(text: &str) -> Self {
    StringOrWord::String(vec![StringPart::Text(text.to_string())])
  }

  pub fn new_word(text: &str) -> Self {
    StringOrWord::Word(vec![StringPart::Text(text.to_string())])
  }

  pub fn parts(&self) -> &Vec<StringPart> {
    match self {
      StringOrWord::String(parts) => parts,
      StringOrWord::Word(parts) => parts,
    }
  }

  pub fn into_parts(self) -> Vec<StringPart> {
    match self {
      StringOrWord::String(parts) => parts,
      StringOrWord::Word(parts) => parts,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPart {
  /// Text in the string (ex. `some text`)
  Text(String),
  /// Variable substitution (ex. `$MY_VAR`)
  Variable(String),
  /// Command substitution (ex. `$(command)`)
  Command(SequentialList),
}

/// Note: Only used to detect redirects in order to give a better error.
/// Redirects are not part of the first pass of this feature.
#[derive(Debug, Clone, PartialEq)]
pub struct Redirect {
  pub maybe_fd: Option<usize>,
  pub op: RedirectOp,
  pub word: Vec<StringPart>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RedirectOp {
  /// >
  Redirect,
  /// >>
  Append,
}

fn error_for_failure(e: ParseErrorFailure) -> Result<SequentialList> {
  bail!(
    "{}\n  {}\n  ~",
    e.message,
    // truncate the output to prevent wrapping in the console
    e.input.chars().take(60).collect::<String>()
  )
}

pub fn parse(input: &str) -> Result<SequentialList> {
  match parse_sequential_list(input) {
    Ok((input, expr)) => {
      if input.trim().is_empty() {
        if expr.items.is_empty() {
          bail!("Empty command.")
        } else {
          Ok(expr)
        }
      } else {
        error_for_failure(fail_for_trailing_input(input))
      }
    }
    Err(ParseError::Backtrace) => {
      error_for_failure(fail_for_trailing_input(input))
    }
    Err(ParseError::Failure(e)) => error_for_failure(e),
  }
}

fn parse_sequential_list(input: &str) -> ParseResult<SequentialList> {
  let (input, items) = separated_list(
    terminated(parse_sequential_list_item, skip_whitespace),
    terminated(
      skip_whitespace,
      or(
        map(parse_sequential_list_op, |_| ()),
        map(parse_async_list_op, |_| ()),
      ),
    ),
  )(input)?;
  Ok((input, SequentialList { items }))
}

fn parse_sequential_list_item(input: &str) -> ParseResult<SequentialListItem> {
  let (input, sequence) = parse_sequence(input)?;
  Ok((
    input,
    SequentialListItem {
      is_async: maybe(parse_async_list_op)(input)?.1.is_some(),
      sequence,
    },
  ))
}

fn parse_sequence(input: &str) -> ParseResult<Sequence> {
  let (input, current) = terminated(
    or(
      parse_env_or_shell_var_command,
      map(parse_pipeline, Sequence::Pipeline),
    ),
    skip_whitespace,
  )(input)?;

  Ok(match parse_boolean_list_op(input) {
    Ok((input, op)) => {
      let (input, next_sequence) = assert_exists(
        &parse_sequence,
        "Expected command following boolean operator.",
      )(input)?;
      (
        input,
        Sequence::BooleanList(Box::new(BooleanList {
          current,
          op,
          next: next_sequence,
        })),
      )
    }
    Err(ParseError::Backtrace) => (input, current),
    Err(err) => return Err(err),
  })
}

fn parse_env_or_shell_var_command(input: &str) -> ParseResult<Sequence> {
  let env_vars_input = input;
  let (input, maybe_export) =
    maybe(terminated(parse_word_with_text("export"), skip_whitespace))(input)?;
  let (input, mut env_vars) = parse_env_vars(input)?;
  if env_vars.is_empty() {
    return ParseError::backtrace();
  }
  let (input, args) = parse_command_args(input)?;
  if !args.is_empty() {
    return ParseError::backtrace();
  }
  if env_vars.len() > 1 {
    ParseError::fail(env_vars_input, "Cannot set multiple environment variables when there is no following command.")
  } else {
    ParseResult::Ok((
      input,
      if maybe_export.is_some() {
        Sequence::EnvVar(env_vars.remove(0))
      } else {
        Sequence::ShellVar(env_vars.remove(0))
      },
    ))
  }
}

/// Parses a pipeline, which is a sequence of one or more commands.
/// https://www.gnu.org/software/bash/manual/html_node/Pipelines.html
fn parse_pipeline(input: &str) -> ParseResult<Pipeline> {
  let (input, maybe_negated) = maybe(parse_negated_op)(input)?;
  let (input, inner) = parse_pipeline_inner(input)?;

  let pipeline = Pipeline {
    negated: maybe_negated.is_some(),
    inner,
  };

  Ok((input, pipeline))
}

fn parse_pipeline_inner(input: &str) -> ParseResult<PipelineInner> {
  let original_input = input;
  let (input, command) = parse_command(input)?;

  let (input, inner) = match parse_pipe_sequence_op(input) {
    Ok((input, op)) => {
      let (input, next_inner) = assert_exists(
        &parse_pipeline_inner,
        "Expected command following pipeline operator.",
      )(input)?;

      if command.redirect.is_some() {
        return ParseError::fail(
          original_input,
          "Redirects in pipe sequence commands are currently not supported.",
        );
      }

      (
        input,
        PipelineInner::PipeSequence(Box::new(PipeSequence {
          current: command,
          op,
          next: next_inner,
        })),
      )
    }
    Err(ParseError::Backtrace) => (input, PipelineInner::Command(command)),
    Err(err) => return Err(err),
  };

  Ok((input, inner))
}

fn parse_command(input: &str) -> ParseResult<Command> {
  let (input, inner) = terminated(
    or(
      map(parse_subshell, |l| CommandInner::Subshell(Box::new(l))),
      map(parse_simple_command, CommandInner::Simple),
    ),
    skip_whitespace,
  )(input)?;

  let before_redirects_input = input;
  let (input, mut redirects) =
    many0(terminated(parse_redirect, skip_whitespace))(input)?;

  if redirects.len() > 1 {
    return ParseError::fail(
      before_redirects_input,
      "Multiple redirects are currently not supported.",
    );
  }

  let command = Command {
    redirect: redirects.pop(),
    inner,
  };

  Ok((input, command))
}

fn parse_simple_command(input: &str) -> ParseResult<SimpleCommand> {
  let (input, env_vars) = parse_env_vars(input)?;
  let (input, args) = parse_command_args(input)?;
  if args.is_empty() {
    return ParseError::backtrace();
  }
  ParseResult::Ok((input, SimpleCommand { env_vars, args }))
}

fn parse_command_args(input: &str) -> ParseResult<Vec<StringOrWord>> {
  many_till(
    terminated(parse_shell_arg, assert_whitespace_or_end_and_skip),
    or4(
      parse_list_op,
      map(parse_redirect, |_| ()),
      map(parse_pipe_sequence_op, |_| ()),
      map(ch(')'), |_| ()),
    ),
  )(input)
}

fn parse_shell_arg(input: &str) -> ParseResult<StringOrWord> {
  let (input, value) = parse_string_or_word(input)?;
  if value.parts().is_empty() {
    ParseError::backtrace()
  } else {
    Ok((input, value))
  }
}

fn parse_list_op(input: &str) -> ParseResult<()> {
  or(
    map(parse_boolean_list_op, |_| ()),
    map(or(parse_sequential_list_op, parse_async_list_op), |_| ()),
  )(input)
}

fn parse_boolean_list_op(input: &str) -> ParseResult<BooleanListOperator> {
  or(
    map(parse_op_str(BooleanListOperator::And.as_str()), |_| {
      BooleanListOperator::And
    }),
    map(parse_op_str(BooleanListOperator::Or.as_str()), |_| {
      BooleanListOperator::Or
    }),
  )(input)
}

fn parse_sequential_list_op(input: &str) -> ParseResult<&str> {
  terminated(tag(";"), skip_whitespace)(input)
}

fn parse_async_list_op(input: &str) -> ParseResult<&str> {
  parse_op_str("&")(input)
}

fn parse_negated_op(input: &str) -> ParseResult<&str> {
  terminated(
    tag("!"),
    // must have whitespace following
    whitespace,
  )(input)
}

fn parse_op_str<'a>(
  operator: &str,
) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> {
  debug_assert!(operator == "&&" || operator == "||" || operator == "&");
  let operator = operator.to_string();
  terminated(
    tag(operator),
    terminated(check_not(one_of("|&")), skip_whitespace),
  )
}

fn parse_pipe_sequence_op(input: &str) -> ParseResult<PipeSequenceOperator> {
  terminated(
    or(
      map(tag("|&"), |_| PipeSequenceOperator::StdoutStderr),
      map(ch('|'), |_| PipeSequenceOperator::Stdout),
    ),
    terminated(check_not(one_of("|&")), skip_whitespace),
  )(input)
}

fn parse_redirect(input: &str) -> ParseResult<Redirect> {
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_07
  let original_input = input;
  let (input, maybe_fd) = maybe(parse_usize)(input)?;
  let (input, maybe_ampersand) = if maybe_fd.is_none() {
    maybe(ch('&'))(input)?
  } else {
    (input, None)
  };
  let (input, op) = or(
    map(tag(">>"), |_| RedirectOp::Append),
    map(or(tag(">"), tag(">|")), |_| RedirectOp::Redirect),
  )(input)?;
  let (input, _) = skip_whitespace(input)?;
  let (input, word) = parse_word(input)?;

  if maybe_ampersand.is_some() {
    return ParseError::fail(
      original_input,
      "&> redirects are currently not supported.",
    );
  }

  Ok((input, Redirect { maybe_fd, op, word }))
}

fn parse_env_vars(input: &str) -> ParseResult<Vec<EnvVar>> {
  many0(terminated(parse_env_var, skip_whitespace))(input)
}

fn parse_env_var(input: &str) -> ParseResult<EnvVar> {
  let (input, name) = parse_env_var_name(input)?;
  let (input, _) = ch('=')(input)?;
  let (input, value) = with_error_context(
    terminated(parse_env_var_value, assert_whitespace_or_end),
    "Invalid environment variable value.",
  )(input)?;
  Ok((input, EnvVar::new(name.to_string(), value)))
}

fn parse_env_var_name(input: &str) -> ParseResult<&str> {
  let (input, name) = take_while(is_valid_env_var_char)(input)?;
  if name.is_empty() {
    ParseError::backtrace()
  } else {
    Ok((input, name))
  }
}

fn parse_env_var_value(input: &str) -> ParseResult<StringOrWord> {
  parse_string_or_word(input)
}

fn parse_string_or_word(input: &str) -> ParseResult<StringOrWord> {
  or(
    map(parse_quoted_string, StringOrWord::String),
    map(parse_word, StringOrWord::Word),
  )(input)
}

fn parse_word(input: &str) -> ParseResult<Vec<StringPart>> {
  assert(
    parse_string_parts(ParseStringPartsMode::Word),
    |result| {
      result
        .ok()
        .map(|(_, parts)| {
          if parts.len() == 1 {
            if let StringPart::Text(text) = &parts[0] {
              return !is_reserved_word(text);
            }
          }
          true
        })
        .unwrap_or(true)
    },
    "Unsupported reserved word.",
  )(input)
}

fn parse_word_with_text(
  text: &'static str,
) -> impl Fn(&str) -> ParseResult<Vec<StringPart>> {
  debug_assert!(!text.contains('$')); // not implemented
  move |input| {
    let (input, word) = parse_word(input)?;
    if word.len() == 1 {
      if let StringPart::Text(part_text) = &word[0] {
        if part_text == text {
          return ParseResult::Ok((input, word));
        }
      }
    }
    ParseError::backtrace()
  }
}

fn parse_quoted_string(input: &str) -> ParseResult<Vec<StringPart>> {
  // Strings may be up beside each other, and if they are they
  // should be categorized as the same argument.
  map(
    many1(or(
      map(parse_single_quoted_string, |text| {
        vec![StringPart::Text(text.to_string())]
      }),
      parse_double_quoted_string,
    )),
    |vecs| vecs.into_iter().flatten().collect(),
  )(input)
}

fn parse_single_quoted_string(input: &str) -> ParseResult<&str> {
  // single quoted strings cannot contain a single quote
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_02_02
  delimited(
    ch('\''),
    take_while(|c| c != '\''),
    with_failure_input(
      input,
      assert_exists(ch('\''), "Expected closing single quote."),
    ),
  )(input)
}

fn parse_double_quoted_string(input: &str) -> ParseResult<Vec<StringPart>> {
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_02_03
  // Double quotes may have escaped
  delimited(
    ch('"'),
    parse_string_parts(ParseStringPartsMode::DoubleQuotes),
    with_failure_input(
      input,
      assert_exists(ch('"'), "Expected closing double quote."),
    ),
  )(input)
}

#[derive(Clone, Copy, PartialEq)]
enum ParseStringPartsMode {
  DoubleQuotes,
  Word,
}

fn parse_string_parts(
  mode: ParseStringPartsMode,
) -> impl Fn(&str) -> ParseResult<Vec<StringPart>> {
  fn parse_escaped_dollar_sign(input: &str) -> ParseResult<char> {
    or(
      parse_escaped_char('$'),
      terminated(
        ch('$'),
        check_not(or(map(parse_env_var_name, |_| ()), map(ch('('), |_| ()))),
      ),
    )(input)
  }

  fn parse_special_shell_var(input: &str) -> ParseResult<char> {
    // for now, these hard error
    preceded(ch('$'), |input| {
      if let Some(char) = input.chars().next() {
        // $$ - process id
        // $? - last exit code
        // $# - number of arguments in $*
        // $* - list of arguments passed to the current process
        if "$?#*".contains(char) {
          return ParseError::fail(
            input,
            format!("${} is currently not supported.", char),
          );
        }
      }
      ParseError::backtrace()
    })(input)
  }

  fn parse_escaped_char<'a>(
    c: char,
  ) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    preceded(ch('\\'), ch(c))
  }

  fn first_escaped_char<'a>(
    mode: ParseStringPartsMode,
  ) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    or5(
      parse_special_shell_var,
      parse_escaped_dollar_sign,
      parse_escaped_char('`'),
      parse_escaped_char('"'),
      if_true(parse_escaped_char('\''), move |_| {
        mode == ParseStringPartsMode::DoubleQuotes
      }),
    )
  }

  move |input| {
    enum PendingPart<'a> {
      Char(char),
      Variable(&'a str),
      Command(SequentialList),
      Parts(Vec<StringPart>),
    }

    let (input, parts) = many0(or7(
      map(first_escaped_char(mode), PendingPart::Char),
      map(parse_command_substitution, PendingPart::Command),
      map(preceded(ch('$'), parse_env_var_name), PendingPart::Variable),
      |input| {
        let (_, _) = ch('`')(input)?;
        ParseError::fail(
          input,
          "Back ticks in strings is currently not supported.",
        )
      },
      // words can have escaped spaces
      map(
        if_true(preceded(ch('\\'), ch(' ')), |_| {
          mode == ParseStringPartsMode::Word
        }),
        PendingPart::Char,
      ),
      map(
        if_true(next_char, |&c| match mode {
          ParseStringPartsMode::DoubleQuotes => c != '"',
          ParseStringPartsMode::Word => {
            !c.is_whitespace() && !"*~(){}<>?|&;\"'".contains(c)
          }
        }),
        PendingPart::Char,
      ),
      |input| match mode {
        ParseStringPartsMode::DoubleQuotes => ParseError::backtrace(),
        ParseStringPartsMode::Word => {
          let (input, parts) = parse_quoted_string(input)?;
          Ok((input, PendingPart::Parts(parts)))
        }
      },
    ))(input)?;

    let mut result = Vec::new();
    for part in parts {
      match part {
        PendingPart::Char(c) => {
          if let Some(StringPart::Text(text)) = result.last_mut() {
            text.push(c);
          } else {
            result.push(StringPart::Text(c.to_string()));
          }
        }
        PendingPart::Command(s) => result.push(StringPart::Command(s)),
        PendingPart::Variable(v) => {
          result.push(StringPart::Variable(v.to_string()))
        }
        PendingPart::Parts(parts) => {
          result.extend(parts);
        }
      }
    }

    Ok((input, result))
  }
}

fn parse_command_substitution(input: &str) -> ParseResult<SequentialList> {
  delimited(tag("$("), parse_sequential_list, ch(')'))(input)
}

fn parse_subshell(input: &str) -> ParseResult<SequentialList> {
  delimited(
    terminated(ch('('), skip_whitespace),
    parse_sequential_list,
    with_failure_input(
      input,
      assert_exists(ch(')'), "Expected closing parenthesis on subshell."),
    ),
  )(input)
}

fn parse_usize(input: &str) -> ParseResult<usize> {
  let mut value = 0;
  let mut byte_index = 0;
  for c in input.chars() {
    if c.is_ascii_digit() {
      value = value * 10 + (c.to_digit(10).unwrap() as usize);
    } else if byte_index == 0 {
      return ParseError::backtrace();
    } else {
      break;
    }
    byte_index += c.len_utf8();
  }
  Ok((&input[byte_index..], value))
}

fn assert_whitespace_or_end_and_skip(input: &str) -> ParseResult<()> {
  terminated(assert_whitespace_or_end, skip_whitespace)(input)
}

fn assert_whitespace_or_end(input: &str) -> ParseResult<()> {
  if let Some(next_char) = input.chars().next() {
    if !next_char.is_whitespace()
      && !matches!(next_char, ';' | '&' | '|' | '(' | ')')
    {
      return Err(ParseError::Failure(fail_for_trailing_input(input)));
    }
  }
  Ok((input, ()))
}

fn is_valid_env_var_char(c: char) -> bool {
  // [a-zA-Z0-9_]+
  c.is_ascii_alphanumeric() || c == '_'
}

fn is_reserved_word(text: &str) -> bool {
  matches!(
    text,
    "if"
      | "then"
      | "else"
      | "elif"
      | "fi"
      | "do"
      | "done"
      | "case"
      | "esac"
      | "while"
      | "until"
      | "for"
      | "in"
  )
}

fn fail_for_trailing_input(input: &str) -> ParseErrorFailure {
  if input.starts_with('*') || input.starts_with('?') {
    ParseErrorFailure::new(
      input,
      "Globs are currently not supported, but will be soon.",
    )
  } else {
    ParseErrorFailure::new(input, "Unexpected character.")
  }
}

#[cfg(test)]
mod test {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_main() {
    assert_eq!(parse("").err().unwrap().to_string(), "Empty command.");
    assert_eq!(
      parse("&& testing").err().unwrap().to_string(),
      concat!("Unexpected character.\n", "  && testing\n", "  ~",),
    );
    assert_eq!(
      parse("test { test").err().unwrap().to_string(),
      concat!("Unexpected character.\n", "  { test\n", "  ~",),
    );
    assert_eq!(
      parse("cp test/* other").err().unwrap().to_string(),
      concat!(
        "Globs are currently not supported, but will be soon.\n",
        "  * other\n",
        "  ~",
      ),
    );
    assert_eq!(
      parse("cp test/? other").err().unwrap().to_string(),
      concat!(
        "Globs are currently not supported, but will be soon.\n",
        "  ? other\n",
        "  ~",
      ),
    );
    assert_eq!(
      parse("(test").err().unwrap().to_string(),
      concat!(
        "Expected closing parenthesis on subshell.\n",
        "  (test\n",
        "  ~"
      ),
    );
    assert_eq!(
      parse("cmd \"test").err().unwrap().to_string(),
      concat!("Expected closing double quote.\n", "  \"test\n", "  ~"),
    );
    assert_eq!(
      parse("cmd 'test").err().unwrap().to_string(),
      concat!("Expected closing single quote.\n", "  'test\n", "  ~"),
    );

    assert!(parse("( test ||other&&test;test);(t&est );").is_ok());
    assert!(parse("command --arg='value'").is_ok());
    assert!(parse("command --arg=\"value\"").is_ok());

    assert_eq!(
      parse("echo `echo 1`").err().unwrap().to_string(),
      concat!(
        "Back ticks in strings is currently not supported.\n",
        "  `echo 1`\n",
        "  ~",
      ),
    );
    assert!(
      parse("deno run --allow-read=. --allow-write=./testing main.ts").is_ok(),
    );
  }

  #[test]
  fn test_sequential_list() {
    run_test(
      parse_sequential_list,
      concat!(
        "Name=Value OtherVar=Other command arg1 || command2 arg12 arg13 ; ",
        "command3 && command4 & command5 ; export ENV6=5 ; ",
        "ENV7=other && command8 || command9 ; ",
        "cmd10 && (cmd11 || cmd12)"
      ),
      Ok(SequentialList {
        items: vec![
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: SimpleCommand {
                env_vars: vec![
                  EnvVar::new(
                    "Name".to_string(),
                    StringOrWord::new_word("Value"),
                  ),
                  EnvVar::new(
                    "OtherVar".to_string(),
                    StringOrWord::new_word("Other"),
                  ),
                ],
                args: vec![
                  StringOrWord::new_word("command"),
                  StringOrWord::new_word("arg1"),
                ],
              }
              .into(),
              op: BooleanListOperator::Or,
              next: SimpleCommand {
                env_vars: vec![],
                args: vec![
                  StringOrWord::new_word("command2"),
                  StringOrWord::new_word("arg12"),
                  StringOrWord::new_word("arg13"),
                ],
              }
              .into(),
            })),
          },
          SequentialListItem {
            is_async: true,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: SimpleCommand {
                env_vars: vec![],
                args: vec![StringOrWord::new_word("command3")],
              }
              .into(),
              op: BooleanListOperator::And,
              next: SimpleCommand {
                env_vars: vec![],
                args: vec![StringOrWord::new_word("command4")],
              }
              .into(),
            })),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("command5")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::EnvVar(EnvVar::new(
              "ENV6".to_string(),
              StringOrWord::new_word("5"),
            )),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: Sequence::ShellVar(EnvVar::new(
                "ENV7".to_string(),
                StringOrWord::new_word("other"),
              )),
              op: BooleanListOperator::And,
              next: Sequence::BooleanList(Box::new(BooleanList {
                current: SimpleCommand {
                  env_vars: vec![],
                  args: vec![StringOrWord::new_word("command8")],
                }
                .into(),
                op: BooleanListOperator::Or,
                next: SimpleCommand {
                  env_vars: vec![],
                  args: vec![StringOrWord::new_word("command9")],
                }
                .into(),
              })),
            })),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: SimpleCommand {
                env_vars: vec![],
                args: vec![StringOrWord::new_word("cmd10")],
              }
              .into(),
              op: BooleanListOperator::And,
              next: Command {
                inner: CommandInner::Subshell(Box::new(SequentialList {
                  items: vec![SequentialListItem {
                    is_async: false,
                    sequence: Sequence::BooleanList(Box::new(BooleanList {
                      current: SimpleCommand {
                        env_vars: vec![],
                        args: vec![StringOrWord::new_word("cmd11")],
                      }
                      .into(),
                      op: BooleanListOperator::Or,
                      next: SimpleCommand {
                        env_vars: vec![],
                        args: vec![StringOrWord::new_word("cmd12")],
                      }
                      .into(),
                    })),
                  }],
                })),
                redirect: None,
              }
              .into(),
            })),
          },
        ],
      }),
    );

    run_test(
      parse_sequential_list,
      "command1 ; command2 ; A='b' command3",
      Ok(SequentialList {
        items: vec![
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("command1")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("command2")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![EnvVar::new(
                "A".to_string(),
                StringOrWord::new_string("b"),
              )],
              args: vec![StringOrWord::new_word("command3")],
            }
            .into(),
          },
        ],
      }),
    );

    run_test(
      parse_sequential_list,
      "test &&",
      Err("Expected command following boolean operator."),
    );

    run_test(
      parse_sequential_list,
      "command &",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: true,
          sequence: SimpleCommand {
            env_vars: vec![],
            args: vec![StringOrWord::new_word("command")],
          }
          .into(),
        }],
      }),
    );

    run_test(
      parse_sequential_list,
      "test | other",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: false,
          sequence: PipeSequence {
            current: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("test")],
            }
            .into(),
            op: PipeSequenceOperator::Stdout,
            next: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("other")],
            }
            .into(),
          }
          .into(),
        }],
      }),
    );

    run_test(
      parse_sequential_list,
      "test |& other",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: false,
          sequence: PipeSequence {
            current: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("test")],
            }
            .into(),
            op: PipeSequenceOperator::StdoutStderr,
            next: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("other")],
            }
            .into(),
          }
          .into(),
        }],
      }),
    );

    run_test(
      parse_sequential_list,
      "ENV=1 ENV2=3 && test",
      Err("Cannot set multiple environment variables when there is no following command."),
    );

    run_test(
      parse_sequential_list,
      "echo $MY_ENV;",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: false,
          sequence: SimpleCommand {
            env_vars: vec![],
            args: vec![
              StringOrWord::new_word("echo"),
              StringOrWord::Word(vec![StringPart::Variable(
                "MY_ENV".to_string(),
              )]),
            ],
          }
          .into(),
        }],
      }),
    );

    run_test(
      parse_sequential_list,
      "! cmd1 | cmd2 && cmd3",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: false,
          sequence: Sequence::BooleanList(Box::new(BooleanList {
            current: Pipeline {
              negated: true,
              inner: PipeSequence {
                current: SimpleCommand {
                  args: vec![StringOrWord::new_word("cmd1")],
                  env_vars: vec![],
                }
                .into(),
                op: PipeSequenceOperator::Stdout,
                next: SimpleCommand {
                  args: vec![StringOrWord::new_word("cmd2")],
                  env_vars: vec![],
                }
                .into(),
              }
              .into(),
            }
            .into(),
            op: BooleanListOperator::And,
            next: SimpleCommand {
              args: vec![StringOrWord::new_word("cmd3")],
              env_vars: vec![],
            }
            .into(),
          })),
        }],
      }),
    );
  }

  #[test]
  fn test_env_var() {
    run_test(
      parse_env_var,
      "Name=Value",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::new_word("Value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name='quoted value'",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::new_string("quoted value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name=\"double quoted value\"",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::new_string("double quoted value"),
      }),
    );
    run_test_with_end(
      parse_env_var,
      "Name= command_name",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::Word(vec![]),
      }),
      " command_name",
    );

    run_test(
      parse_env_var,
      "Name=$(test)",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::Word(vec![StringPart::Command(SequentialList {
          items: vec![SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![StringOrWord::new_word("test")],
            }
            .into(),
          }],
        })]),
      }),
    );

    run_test(
      parse_env_var,
      "Name=$(OTHER=5)",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringOrWord::Word(vec![StringPart::Command(SequentialList {
          items: vec![SequentialListItem {
            is_async: false,
            sequence: Sequence::ShellVar(EnvVar {
              name: "OTHER".to_string(),
              value: StringOrWord::new_word("5"),
            }),
          }],
        })]),
      }),
    );
  }

  #[test]
  fn test_single_quotes() {
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      "'test'",
      Ok(StringOrWord::new_string("test")),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#"'te\\'"#,
      Ok(StringOrWord::new_string(r#"te\\"#)),
    );
    run_test_with_end(
      map(parse_quoted_string, StringOrWord::String),
      r#"'te\'st'"#,
      Ok(StringOrWord::new_string(r#"te\"#)),
      "st'",
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      "'  '",
      Ok(StringOrWord::new_string("  ")),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      "'  ",
      Err("Expected closing single quote."),
    );
  }

  #[test]
  fn test_double_quotes() {
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#""  ""#,
      Ok(StringOrWord::new_string("  ")),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#""test""#,
      Ok(StringOrWord::new_string("test")),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#""te\"\$\`st""#,
      Ok(StringOrWord::new_string(r#"te"$`st"#)),
    );
    run_test(
      parse_quoted_string,
      r#""  "#,
      Err("Expected closing double quote."),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#""$Test""#,
      Ok(StringOrWord::String(vec![StringPart::Variable(
        "Test".to_string(),
      )])),
    );
    run_test(
      map(parse_quoted_string, StringOrWord::String),
      r#""$Test,$Other_Test""#,
      Ok(StringOrWord::String(vec![
        StringPart::Variable("Test".to_string()),
        StringPart::Text(",".to_string()),
        StringPart::Variable("Other_Test".to_string()),
      ])),
    );
    run_test(
      parse_quoted_string,
      r#""asdf`""#,
      Err("Back ticks in strings is currently not supported."),
    );

    run_test_with_end(
      map(parse_quoted_string, StringOrWord::String),
      r#""test" asdf"#,
      Ok(StringOrWord::new_string("test")),
      " asdf",
    );
  }

  #[test]
  fn test_parse_word() {
    run_test(parse_word, "if", Err("Unsupported reserved word."));
    run_test(parse_word, "$", Ok(vec![StringPart::Text("$".to_string())]));
    // unsupported shell variables
    run_test(parse_word, "$$", Err("$$ is currently not supported."));
    run_test(parse_word, "$?", Err("$? is currently not supported."));
    run_test(parse_word, "$#", Err("$# is currently not supported."));
    run_test(parse_word, "$*", Err("$* is currently not supported."));
    run_test(
      parse_word,
      "test\\ test",
      Ok(vec![StringPart::Text("test test".to_string())]),
    );
  }

  #[test]
  fn test_parse_usize() {
    run_test(parse_usize, "999", Ok(999));
    run_test(parse_usize, "11", Ok(11));
    run_test(parse_usize, "0", Ok(0));
    run_test_with_end(parse_usize, "1>", Ok(1), ">");
    run_test(parse_usize, "-1", Err("backtrace"));
    run_test(parse_usize, "a", Err("backtrace"));
  }

  fn run_test<'a, T: PartialEq + std::fmt::Debug>(
    combinator: impl Fn(&'a str) -> ParseResult<'a, T>,
    input: &'a str,
    expected: Result<T, &str>,
  ) {
    run_test_with_end(combinator, input, expected, "");
  }

  fn run_test_with_end<'a, T: PartialEq + std::fmt::Debug>(
    combinator: impl Fn(&'a str) -> ParseResult<'a, T>,
    input: &'a str,
    expected: Result<T, &str>,
    expected_end: &str,
  ) {
    match combinator(input) {
      Ok((input, value)) => {
        assert_eq!(value, expected.unwrap());
        assert_eq!(input, expected_end);
      }
      Err(ParseError::Backtrace) => {
        assert_eq!("backtrace", expected.err().unwrap());
      }
      Err(ParseError::Failure(err)) => {
        assert_eq!(
          err.message,
          match expected.err() {
            Some(err) => err,
            None =>
              panic!("Got error: {:#}", error_for_failure(err).err().unwrap()),
          }
        );
      }
    }
  }

  #[test]
  fn test_redirects() {
    let expected = Ok(Command {
      inner: CommandInner::Simple(SimpleCommand {
        env_vars: vec![],
        args: vec![StringOrWord::new_word("echo"), StringOrWord::new_word("1")],
      }),
      redirect: Some(Redirect {
        maybe_fd: None,
        op: RedirectOp::Redirect,
        word: vec![StringPart::Text("test.txt".to_string())],
      }),
    });

    run_test(parse_command, "echo 1 > test.txt", expected.clone());
    run_test(parse_command, "echo 1 >test.txt", expected.clone());

    // append
    run_test(
      parse_command,
      "command >> test.txt",
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![StringOrWord::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: None,
          op: RedirectOp::Append,
          word: vec![StringPart::Text("test.txt".to_string())],
        }),
      }),
    );

    run_test_with_end(
      parse_command,
      "echo 1 1> stdout.txt 2> stderr.txt",
      Err("Multiple redirects are currently not supported."),
      "1> stdout.txt 2> stderr.txt",
    );

    // redirect in pipeline sequence command should error
    run_test_with_end(
      parse_sequence,
      "echo 1 1> stdout.txt | cat",
      Err("Redirects in pipe sequence commands are currently not supported."),
      "echo 1 1> stdout.txt | cat",
    );

    run_test_with_end(
      parse_command,
      "echo 1 &> stdout.txt",
      Err("&> redirects are currently not supported."),
      "&> stdout.txt",
    );
  }
}
