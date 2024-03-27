// Copyright 2018-2024 the Deno authors. MIT license.

use anyhow::bail;
use anyhow::Result;
use monch::*;

// Shell grammar rules this is loosely based on:
// https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_10_02

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SequentialList {
  pub items: Vec<SequentialListItem>,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SequentialListItem {
  pub is_async: bool,
  pub sequence: Sequence,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sequence {
  /// `MY_VAR=5`
  ShellVar(EnvVar),
  /// `cmd_name <args...>`, `cmd1 | cmd2`
  Pipeline(Pipeline),
  /// `cmd1 && cmd2 || cmd3`
  BooleanList(Box<BooleanList>),
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanList {
  pub current: Sequence,
  pub op: BooleanListOperator,
  pub next: Sequence,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PipeSequenceOperator {
  // |
  Stdout,
  // |&
  StdoutStderr,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Command {
  pub inner: CommandInner,
  pub redirect: Option<Redirect>,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimpleCommand {
  pub env_vars: Vec<EnvVar>,
  pub args: Vec<Word>,
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

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct EnvVar {
  pub name: String,
  pub value: Word,
}

impl EnvVar {
  pub fn new(name: String, value: Word) -> Self {
    EnvVar { name, value }
  }
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word(Vec<WordPart>);

impl Word {
  pub fn new_string(text: &str) -> Self {
    Word(vec![WordPart::Quoted(vec![WordPart::Text(
      text.to_string(),
    )])])
  }

  pub fn new_word(text: &str) -> Self {
    Word(vec![WordPart::Text(text.to_string())])
  }

  pub fn parts(&self) -> &Vec<WordPart> {
    &self.0
  }

  pub fn into_parts(self) -> Vec<WordPart> {
    self.0
  }
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind", content = "value")
)]
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WordPart {
  /// Text in the string (ex. `some text`)
  Text(String),
  /// Variable substitution (ex. `$MY_VAR`)
  Variable(String),
  /// Command substitution (ex. `$(command)`)
  Command(SequentialList),
  /// Quoted string (ex. `"hello"` or `'test'`)
  Quoted(Vec<WordPart>),
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind", content = "fd")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedirectFd {
  Fd(u32),
  StdoutStderr,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Redirect {
  pub maybe_fd: Option<RedirectFd>,
  pub op: RedirectOp,
  pub io_file: IoFile,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind", content = "value")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IoFile {
  /// Filename to redirect to/from (ex. `file.txt`` in `cmd < file.txt`)
  Word(Word),
  /// File descriptor to redirect to/from (ex. `2` in `cmd >&2`)
  Fd(u32),
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(
  feature = "serialization",
  serde(rename_all = "camelCase", tag = "kind", content = "value")
)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedirectOp {
  Input(RedirectOpInput),
  Output(RedirectOpOutput),
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedirectOpInput {
  /// <
  Redirect,
}

#[cfg_attr(feature = "serialization", derive(serde::Serialize))]
#[cfg_attr(feature = "serialization", serde(rename_all = "camelCase"))]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedirectOpOutput {
  /// >
  Overwrite,
  /// >>
  Append,
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
        fail_for_trailing_input(input)
          .into_result()
          .map_err(|err| err.into())
      }
    }
    Err(ParseError::Backtrace) => fail_for_trailing_input(input)
      .into_result()
      .map_err(|err| err.into()),
    Err(ParseError::Failure(e)) => e.into_result().map_err(|err| err.into()),
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
      parse_shell_var_command,
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

fn parse_shell_var_command(input: &str) -> ParseResult<Sequence> {
  let env_vars_input = input;
  let (input, mut env_vars) = if_not_empty(parse_env_vars)(input)?;
  let (input, args) = parse_command_args(input)?;
  if !args.is_empty() {
    return ParseError::backtrace();
  }
  if env_vars.len() > 1 {
    ParseError::fail(env_vars_input, "Cannot set multiple environment variables when there is no following command.")
  } else {
    ParseResult::Ok((input, Sequence::ShellVar(env_vars.remove(0))))
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
  let (input, args) = if_not_empty(parse_command_args)(input)?;
  ParseResult::Ok((input, SimpleCommand { env_vars, args }))
}

fn parse_command_args(input: &str) -> ParseResult<Vec<Word>> {
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

fn parse_shell_arg(input: &str) -> ParseResult<Word> {
  let (input, value) = parse_word(input)?;
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
  let (input, maybe_fd) = maybe(parse_u32)(input)?;
  let (input, maybe_ampersand) = if maybe_fd.is_none() {
    maybe(ch('&'))(input)?
  } else {
    (input, None)
  };
  let (input, op) = or3(
    map(tag(">>"), |_| RedirectOp::Output(RedirectOpOutput::Append)),
    map(or(tag(">"), tag(">|")), |_| {
      RedirectOp::Output(RedirectOpOutput::Overwrite)
    }),
    map(ch('<'), |_| RedirectOp::Input(RedirectOpInput::Redirect)),
  )(input)?;
  let (input, io_file) = or(
    map(preceded(ch('&'), parse_u32), IoFile::Fd),
    map(preceded(skip_whitespace, parse_word), IoFile::Word),
  )(input)?;

  let maybe_fd = if let Some(fd) = maybe_fd {
    Some(RedirectFd::Fd(fd))
  } else if maybe_ampersand.is_some() {
    Some(RedirectFd::StdoutStderr)
  } else {
    None
  };

  Ok((
    input,
    Redirect {
      maybe_fd,
      op,
      io_file,
    },
  ))
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
  if_not_empty(take_while(is_valid_env_var_char))(input)
}

fn parse_env_var_value(input: &str) -> ParseResult<Word> {
  parse_word(input)
}

fn parse_word(input: &str) -> ParseResult<Word> {
  let parse_quoted_or_unquoted = or(
    map(parse_quoted_string, |parts| vec![WordPart::Quoted(parts)]),
    parse_unquoted_word,
  );
  let (input, mut parts) = parse_quoted_or_unquoted(input)?;
  if parts.is_empty() {
    Ok((input, Word(parts)))
  } else {
    let (input, result) = many0(if_not_empty(parse_quoted_or_unquoted))(input)?;
    parts.extend(result.into_iter().flatten());
    Ok((input, Word(parts)))
  }
}

fn parse_unquoted_word(input: &str) -> ParseResult<Vec<WordPart>> {
  assert(
    parse_word_parts(ParseWordPartsMode::Unquoted),
    |result| {
      result
        .ok()
        .map(|(_, parts)| {
          if parts.len() == 1 {
            if let WordPart::Text(text) = &parts[0] {
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

fn parse_quoted_string(input: &str) -> ParseResult<Vec<WordPart>> {
  // Strings may be up beside each other, and if they are they
  // should be categorized as the same argument.
  map(
    many1(or(
      map(parse_single_quoted_string, |text| {
        vec![WordPart::Text(text.to_string())]
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

fn parse_double_quoted_string(input: &str) -> ParseResult<Vec<WordPart>> {
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_02_03
  // Double quotes may have escaped
  delimited(
    ch('"'),
    parse_word_parts(ParseWordPartsMode::DoubleQuotes),
    with_failure_input(
      input,
      assert_exists(ch('"'), "Expected closing double quote."),
    ),
  )(input)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ParseWordPartsMode {
  DoubleQuotes,
  Unquoted,
}

fn parse_word_parts(
  mode: ParseWordPartsMode,
) -> impl Fn(&str) -> ParseResult<Vec<WordPart>> {
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
        // $# - number of arguments in $*
        // $* - list of arguments passed to the current process
        if "$#*".contains(char) {
          return ParseError::fail(
            input,
            format!("${char} is currently not supported."),
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
    mode: ParseWordPartsMode,
  ) -> impl Fn(&'a str) -> ParseResult<'a, char> {
    or7(
      parse_special_shell_var,
      parse_escaped_dollar_sign,
      parse_escaped_char('`'),
      parse_escaped_char('"'),
      parse_escaped_char('('),
      parse_escaped_char(')'),
      if_true(parse_escaped_char('\''), move |_| {
        mode == ParseWordPartsMode::DoubleQuotes
      }),
    )
  }

  move |input| {
    enum PendingPart<'a> {
      Char(char),
      Variable(&'a str),
      Command(SequentialList),
      Parts(Vec<WordPart>),
    }

    let (input, parts) = many0(or7(
      or(
        map(tag("$?"), |_| PendingPart::Variable("?")),
        map(first_escaped_char(mode), PendingPart::Char),
      ),
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
          mode == ParseWordPartsMode::Unquoted
        }),
        PendingPart::Char,
      ),
      map(
        if_true(next_char, |&c| match mode {
          ParseWordPartsMode::DoubleQuotes => c != '"',
          ParseWordPartsMode::Unquoted => {
            !c.is_whitespace() && !"~(){}<>|&;\"'".contains(c)
          }
        }),
        PendingPart::Char,
      ),
      |input| match mode {
        ParseWordPartsMode::DoubleQuotes => ParseError::backtrace(),
        ParseWordPartsMode::Unquoted => {
          let (input, parts) =
            map(parse_quoted_string, |parts| vec![WordPart::Quoted(parts)])(
              input,
            )?;
          Ok((input, PendingPart::Parts(parts)))
        }
      },
    ))(input)?;

    let mut result = Vec::new();
    for part in parts {
      match part {
        PendingPart::Char(c) => {
          if let Some(WordPart::Text(text)) = result.last_mut() {
            text.push(c);
          } else {
            result.push(WordPart::Text(c.to_string()));
          }
        }
        PendingPart::Command(s) => result.push(WordPart::Command(s)),
        PendingPart::Variable(v) => {
          result.push(WordPart::Variable(v.to_string()))
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

fn parse_u32(input: &str) -> ParseResult<u32> {
  let mut value: u32 = 0;
  let mut byte_index = 0;
  for c in input.chars() {
    if c.is_ascii_digit() {
      let shifted_val = match value.checked_mul(10) {
        Some(val) => val,
        None => return ParseError::backtrace(),
      };
      value = match shifted_val.checked_add(c.to_digit(10).unwrap()) {
        Some(val) => val,
        None => return ParseError::backtrace(),
      };
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
  ParseErrorFailure::new(input, "Unexpected character.")
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
    assert!(parse("cp test/* other").is_ok());
    assert!(parse("cp test/? other").is_ok());
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
                  EnvVar::new("Name".to_string(), Word::new_word("Value")),
                  EnvVar::new("OtherVar".to_string(), Word::new_word("Other")),
                ],
                args: vec![Word::new_word("command"), Word::new_word("arg1")],
              }
              .into(),
              op: BooleanListOperator::Or,
              next: SimpleCommand {
                env_vars: vec![],
                args: vec![
                  Word::new_word("command2"),
                  Word::new_word("arg12"),
                  Word::new_word("arg13"),
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
                args: vec![Word::new_word("command3")],
              }
              .into(),
              op: BooleanListOperator::And,
              next: SimpleCommand {
                env_vars: vec![],
                args: vec![Word::new_word("command4")],
              }
              .into(),
            })),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("command5")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("export"), Word::new_word("ENV6=5")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: Sequence::ShellVar(EnvVar::new(
                "ENV7".to_string(),
                Word::new_word("other"),
              )),
              op: BooleanListOperator::And,
              next: Sequence::BooleanList(Box::new(BooleanList {
                current: SimpleCommand {
                  env_vars: vec![],
                  args: vec![Word::new_word("command8")],
                }
                .into(),
                op: BooleanListOperator::Or,
                next: SimpleCommand {
                  env_vars: vec![],
                  args: vec![Word::new_word("command9")],
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
                args: vec![Word::new_word("cmd10")],
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
                        args: vec![Word::new_word("cmd11")],
                      }
                      .into(),
                      op: BooleanListOperator::Or,
                      next: SimpleCommand {
                        env_vars: vec![],
                        args: vec![Word::new_word("cmd12")],
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
              args: vec![Word::new_word("command1")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("command2")],
            }
            .into(),
          },
          SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![EnvVar::new(
                "A".to_string(),
                Word::new_string("b"),
              )],
              args: vec![Word::new_word("command3")],
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
            args: vec![Word::new_word("command")],
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
              args: vec![Word::new_word("test")],
            }
            .into(),
            op: PipeSequenceOperator::Stdout,
            next: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("other")],
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
              args: vec![Word::new_word("test")],
            }
            .into(),
            op: PipeSequenceOperator::StdoutStderr,
            next: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("other")],
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
              Word::new_word("echo"),
              Word(vec![WordPart::Variable("MY_ENV".to_string())]),
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
                  args: vec![Word::new_word("cmd1")],
                  env_vars: vec![],
                }
                .into(),
                op: PipeSequenceOperator::Stdout,
                next: SimpleCommand {
                  args: vec![Word::new_word("cmd2")],
                  env_vars: vec![],
                }
                .into(),
              }
              .into(),
            }
            .into(),
            op: BooleanListOperator::And,
            next: SimpleCommand {
              args: vec![Word::new_word("cmd3")],
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
        value: Word::new_word("Value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name='quoted value'",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: Word::new_string("quoted value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name=\"double quoted value\"",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: Word::new_string("double quoted value"),
      }),
    );
    run_test_with_end(
      parse_env_var,
      "Name= command_name",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: Word(vec![]),
      }),
      " command_name",
    );

    run_test(
      parse_env_var,
      "Name=$(test)",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: Word(vec![WordPart::Command(SequentialList {
          items: vec![SequentialListItem {
            is_async: false,
            sequence: SimpleCommand {
              env_vars: vec![],
              args: vec![Word::new_word("test")],
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
        value: Word(vec![WordPart::Command(SequentialList {
          items: vec![SequentialListItem {
            is_async: false,
            sequence: Sequence::ShellVar(EnvVar {
              name: "OTHER".to_string(),
              value: Word::new_word("5"),
            }),
          }],
        })]),
      }),
    );
  }

  #[test]
  fn test_single_quotes() {
    run_test(
      parse_quoted_string,
      "'test'",
      Ok(vec![WordPart::Text("test".to_string())]),
    );
    run_test(
      parse_quoted_string,
      r"'te\\'",
      Ok(vec![WordPart::Text(r"te\\".to_string())]),
    );
    run_test_with_end(
      parse_quoted_string,
      r"'te\'st'",
      Ok(vec![WordPart::Text(r"te\".to_string())]),
      "st'",
    );
    run_test(
      parse_quoted_string,
      "'  '",
      Ok(vec![WordPart::Text("  ".to_string())]),
    );
    run_test(
      parse_quoted_string,
      "'  ",
      Err("Expected closing single quote."),
    );
  }

  #[test]
  fn test_single_quotes_mid_word() {
    run_test(
      parse_word,
      "--inspect='[::0]:3366'",
      Ok(Word(vec![
        WordPart::Text("--inspect=".to_string()),
        WordPart::Quoted(vec![WordPart::Text("[::0]:3366".to_string())]),
      ])),
    );
  }

  #[test]
  fn test_double_quotes() {
    run_test(
      parse_quoted_string,
      r#""  ""#,
      Ok(vec![WordPart::Text("  ".to_string())]),
    );
    run_test(
      parse_quoted_string,
      r#""test""#,
      Ok(vec![WordPart::Text("test".to_string())]),
    );
    run_test(
      parse_quoted_string,
      r#""te\"\$\`st""#,
      Ok(vec![WordPart::Text(r#"te"$`st"#.to_string())]),
    );
    run_test(
      parse_quoted_string,
      r#""  "#,
      Err("Expected closing double quote."),
    );
    run_test(
      parse_quoted_string,
      r#""$Test""#,
      Ok(vec![WordPart::Variable("Test".to_string())]),
    );
    run_test(
      parse_quoted_string,
      r#""$Test,$Other_Test""#,
      Ok(vec![
        WordPart::Variable("Test".to_string()),
        WordPart::Text(",".to_string()),
        WordPart::Variable("Other_Test".to_string()),
      ]),
    );
    run_test(
      parse_quoted_string,
      r#""asdf`""#,
      Err("Back ticks in strings is currently not supported."),
    );

    run_test_with_end(
      parse_quoted_string,
      r#""test" asdf"#,
      Ok(vec![WordPart::Text("test".to_string())]),
      " asdf",
    );
  }

  #[test]
  fn test_parse_word() {
    run_test(parse_unquoted_word, "if", Err("Unsupported reserved word."));
    run_test(
      parse_unquoted_word,
      "$",
      Ok(vec![WordPart::Text("$".to_string())]),
    );
    // unsupported shell variables
    run_test(
      parse_unquoted_word,
      "$$",
      Err("$$ is currently not supported."),
    );
    run_test(
      parse_unquoted_word,
      "$#",
      Err("$# is currently not supported."),
    );
    run_test(
      parse_unquoted_word,
      "$*",
      Err("$* is currently not supported."),
    );
    run_test(
      parse_unquoted_word,
      "test\\ test",
      Ok(vec![WordPart::Text("test test".to_string())]),
    );
  }

  #[test]
  fn test_parse_u32() {
    run_test(parse_u32, "999", Ok(999));
    run_test(parse_u32, "11", Ok(11));
    run_test(parse_u32, "0", Ok(0));
    run_test_with_end(parse_u32, "1>", Ok(1), ">");
    run_test(parse_u32, "-1", Err("backtrace"));
    run_test(parse_u32, "a", Err("backtrace"));
    run_test(
      parse_u32,
      "16116951273372934291112534924737",
      Err("backtrace"),
    );
    run_test(parse_u32, "4294967295", Ok(4294967295));
    run_test(parse_u32, "4294967296", Err("backtrace"));
  }

  #[track_caller]
  fn run_test<'a, T: PartialEq + std::fmt::Debug>(
    combinator: impl Fn(&'a str) -> ParseResult<'a, T>,
    input: &'a str,
    expected: Result<T, &str>,
  ) {
    run_test_with_end(combinator, input, expected, "");
  }

  #[track_caller]
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
              panic!("Got error: {:#}", err.into_result::<T>().err().unwrap()),
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
        args: vec![Word::new_word("echo"), Word::new_word("1")],
      }),
      redirect: Some(Redirect {
        maybe_fd: None,
        op: RedirectOp::Output(RedirectOpOutput::Overwrite),
        io_file: IoFile::Word(Word(vec![WordPart::Text(
          "test.txt".to_string(),
        )])),
      }),
    });

    run_test(parse_command, "echo 1 > test.txt", expected.clone());
    run_test(parse_command, "echo 1 >test.txt", expected.clone());

    // append
    run_test(
      parse_command,
      r#"command >> "test.txt""#,
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![Word::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: None,
          op: RedirectOp::Output(RedirectOpOutput::Append),
          io_file: IoFile::Word(Word(vec![WordPart::Quoted(vec![
            WordPart::Text("test.txt".to_string()),
          ])])),
        }),
      }),
    );

    // fd
    run_test(
      parse_command,
      r#"command 2> test.txt"#,
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![Word::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: Some(RedirectFd::Fd(2)),
          op: RedirectOp::Output(RedirectOpOutput::Overwrite),
          io_file: IoFile::Word(Word(vec![WordPart::Text(
            "test.txt".to_string(),
          )])),
        }),
      }),
    );

    // both
    run_test(
      parse_command,
      r#"command &> test.txt"#,
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![Word::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: Some(RedirectFd::StdoutStderr),
          op: RedirectOp::Output(RedirectOpOutput::Overwrite),
          io_file: IoFile::Word(Word(vec![WordPart::Text(
            "test.txt".to_string(),
          )])),
        }),
      }),
    );

    // output redirect to fd
    run_test(
      parse_command,
      r#"command 2>&1"#,
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![Word::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: Some(RedirectFd::Fd(2)),
          op: RedirectOp::Output(RedirectOpOutput::Overwrite),
          io_file: IoFile::Fd(1),
        }),
      }),
    );

    // input redirect to fd
    run_test(
      parse_command,
      r#"command <&0"#,
      Ok(Command {
        inner: CommandInner::Simple(SimpleCommand {
          env_vars: vec![],
          args: vec![Word::new_word("command")],
        }),
        redirect: Some(Redirect {
          maybe_fd: None,
          op: RedirectOp::Input(RedirectOpInput::Redirect),
          io_file: IoFile::Fd(0),
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
  }

  #[cfg(feature = "serialization")]
  #[test]
  fn serializes_command_to_json() {
    assert_json_equals(
      serialize_to_json("./example > output.txt"),
      serde_json::json!({
        "items": [{
          "isAsync": false,
          "sequence": {
            "inner": {
              "inner": {
                "args": [[{
                  "kind": "text",
                  "value": "./example"
                }]],
                "envVars": [],
                "kind": "simple"
              },
              "kind": "command",
              "redirect": {
                "ioFile": {
                  "kind": "word",
                  "value": [{
                    "kind": "text",
                    "value": "output.txt"
                  }],
                },
                "maybeFd": null,
                "op": {
                  "kind": "output",
                  "value": "overwrite",
                }
              }
            },
            "kind": "pipeline",
            "negated": false
          }
        }]
      }),
    );
    assert_json_equals(
      serialize_to_json("./example 2> output.txt"),
      serde_json::json!({
        "items": [{
          "isAsync": false,
          "sequence": {
            "inner": {
              "inner": {
                "args": [[{
                  "kind": "text",
                  "value": "./example"
                }]],
                "envVars": [],
                "kind": "simple"
              },
              "kind": "command",
              "redirect": {
                "ioFile": {
                  "kind": "word",
                  "value": [{
                    "kind": "text",
                    "value": "output.txt"
                  }],
                },
                "maybeFd": {
                  "kind": "fd",
                  "fd": 2,
                },
                "op": {
                  "kind": "output",
                  "value": "overwrite",
                }
              }
            },
            "kind": "pipeline",
            "negated": false
          }
        }]
      }),
    );
    assert_json_equals(
      serialize_to_json("./example &> output.txt"),
      serde_json::json!({
        "items": [{
          "isAsync": false,
          "sequence": {
            "inner": {
              "inner": {
                "args": [[{
                  "kind": "text",
                  "value": "./example"
                }]],
                "envVars": [],
                "kind": "simple"
              },
              "kind": "command",
              "redirect": {
                "ioFile": {
                  "kind": "word",
                  "value": [{
                    "kind": "text",
                    "value": "output.txt"
                  }],
                },
                "maybeFd": {
                  "kind": "stdoutStderr"
                },
                "op": {
                  "kind": "output",
                  "value": "overwrite",
                }
              }
            },
            "kind": "pipeline",
            "negated": false
          }
        }]
      }),
    );
    assert_json_equals(
      serialize_to_json("./example < output.txt"),
      serde_json::json!({
        "items": [{
          "isAsync": false,
          "sequence": {
            "inner": {
              "inner": {
                "args": [[{
                  "kind": "text",
                  "value": "./example"
                }]],
                "envVars": [],
                "kind": "simple"
              },
              "kind": "command",
              "redirect": {
                "ioFile": {
                  "kind": "word",
                  "value": [{
                    "kind": "text",
                    "value": "output.txt"
                  }],
                },
                "maybeFd": null,
                "op": {
                  "kind": "input",
                  "value": "redirect",
                }
              }
            },
            "kind": "pipeline",
            "negated": false
          }
        }]
      }),
    );

    assert_json_equals(
      serialize_to_json("./example <&0"),
      serde_json::json!({
        "items": [{
          "isAsync": false,
          "sequence": {
            "inner": {
              "inner": {
                "args": [[{
                  "kind": "text",
                  "value": "./example"
                }]],
                "envVars": [],
                "kind": "simple"
              },
              "kind": "command",
              "redirect": {
                "ioFile": {
                  "kind": "fd",
                  "value": 0,
                },
                "maybeFd": null,
                "op": {
                  "kind": "input",
                  "value": "redirect",
                }
              }
            },
            "kind": "pipeline",
            "negated": false
          }
        }]
      }),
    );
  }

  #[cfg(feature = "serialization")]
  #[track_caller]
  fn assert_json_equals(
    actual: serde_json::Value,
    expected: serde_json::Value,
  ) {
    if actual != expected {
      let actual = serde_json::to_string_pretty(&actual).unwrap();
      let expected = serde_json::to_string_pretty(&expected).unwrap();
      assert_eq!(actual, expected);
    }
  }

  #[cfg(feature = "serialization")]
  fn serialize_to_json(text: &str) -> serde_json::Value {
    let command = parse(text).unwrap();
    serde_json::to_value(command).unwrap()
  }
}
