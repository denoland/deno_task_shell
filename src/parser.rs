// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use anyhow::bail;
use anyhow::Result;

use super::combinators::assert;
use super::combinators::assert_exists;
use super::combinators::char;
use super::combinators::delimited;
use super::combinators::if_not;
use super::combinators::many0;
use super::combinators::many_till;
use super::combinators::map;
use super::combinators::maybe;
use super::combinators::or;
use super::combinators::separated_list;
use super::combinators::skip_whitespace;
use super::combinators::tag;
use super::combinators::take_while;
use super::combinators::terminated;
use super::combinators::with_error_context;
use super::combinators::ParseError;
use super::combinators::ParseErrorFailure;
use super::combinators::ParseResult;

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
  // cmd_name <args...>
  Command(Command),
  // cmd1 | cmd2
  Pipeline(Box<Pipeline>),
  // cmd1 && cmd2 || cmd3
  BooleanList(Box<BooleanList>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BooleanList {
  pub current: Sequence,
  pub op: BooleanListOperator,
  pub next: Sequence,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline {
  pub current: Sequence,
  pub next: Sequence,
}

impl Pipeline {
  pub fn into_vec(self) -> Vec<Sequence> {
    let mut sequences = vec![self.current];
    match self.next {
      Sequence::Pipeline(pipeline) => {
        sequences.extend(pipeline.into_vec());
      }
      next => sequences.push(next),
    }
    sequences
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
pub struct Command {
  pub env_vars: Vec<EnvVar>,
  pub name: StringParts,
  pub args: Vec<StringParts>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnvVar {
  pub name: String,
  pub value: StringParts,
}

impl EnvVar {
  pub fn new(name: String, value: StringParts) -> Self {
    EnvVar { name, value }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringParts(pub Vec<StringPart>);

impl StringParts {
  pub fn new_text(text: &str) -> Self {
    Self(vec![StringPart::Text(text.to_string())])
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StringPart {
  Variable(String),
  Text(String),
}

/// Note: Only used to detect redirects in order to give a better error.
/// Redirects are not part of the first pass of this feature.
pub struct Redirect {
  pub maybe_fd: Option<usize>,
  pub op: RedirectOp,
  pub word: Option<StringParts>,
}

pub enum RedirectOp {
  /// >
  Redirect,
  /// >>
  Append,
}

pub fn parse(input: &str) -> Result<SequentialList> {
  fn error_for_failure(e: ParseErrorFailure) -> Result<SequentialList> {
    bail!(
      "{}\n  {}\n  ~",
      e.message,
      // truncate the output to prevent wrapping in the console
      e.input.chars().take(60).collect::<String>()
    )
  }

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
      or(parse_sequential_list_op, parse_async_list_op),
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
  let (input, current) = or(
    parse_env_or_shell_var_command,
    map(parse_command, Sequence::Command),
  )(input)?;
  let (input, current) = match parse_boolean_list_op(input) {
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
    Err(ParseError::Backtrace) => match parse_pipeline_op(input) {
      Ok((input, _)) => {
        let (input, next_sequence) = assert_exists(
          &parse_sequence,
          "Expected command following pipeline operator.",
        )(input)?;
        (
          input,
          Sequence::Pipeline(Box::new(Pipeline {
            current,
            next: next_sequence,
          })),
        )
      }
      Err(ParseError::Backtrace) => (input, current),
      Err(err) => return Err(err),
    },
    Err(err) => return Err(err),
  };

  Ok((input, current))
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

fn parse_command(input: &str) -> ParseResult<Command> {
  let (input, env_vars) = parse_env_vars(input)?;
  let (input, mut args) = parse_command_args(input)?;
  if args.is_empty() {
    return ParseError::backtrace();
  }
  ParseResult::Ok((
    input,
    Command {
      env_vars,
      name: args.remove(0),
      args,
    },
  ))
}

fn parse_command_args(input: &str) -> ParseResult<Vec<StringParts>> {
  many_till(
    terminated(parse_shell_arg, assert_whitespace_or_end_and_skip),
    or(
      parse_list_op,
      or(map(parse_redirect, |_| ()), parse_pipeline_op),
    ),
  )(input)
}

fn parse_shell_arg(input: &str) -> ParseResult<StringParts> {
  let (input, value) = or(parse_quoted_string, parse_word)(input)?;
  if value.is_empty() {
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
  parse_op_str(";")(input)
}

fn parse_async_list_op(input: &str) -> ParseResult<&str> {
  parse_op_str("&")(input)
}

fn parse_op_str<'a>(
  operator: &str,
) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> {
  let operator = operator.to_string();
  terminated(
    tag(operator),
    terminated(if_not(special_char), skip_whitespace),
  )
}

fn parse_pipeline_op(input: &str) -> ParseResult<()> {
  terminated(
    map(char('|'), |_| ()),
    terminated(if_not(special_char), skip_whitespace),
  )(input)
}

fn parse_redirect(input: &str) -> ParseResult<Redirect> {
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_07
  let (input, maybe_fd) = maybe(parse_usize)(input)?;
  let (input, op) = or(
    map(or(tag(">"), tag(">|")), |_| RedirectOp::Redirect),
    map(tag(">>"), |_| RedirectOp::Append),
  )(input)?;
  let (input, word) = maybe(parse_word)(input)?;

  Ok((input, Redirect { maybe_fd, op, word }))
}

fn parse_env_vars(input: &str) -> ParseResult<Vec<EnvVar>> {
  many0(terminated(parse_env_var, skip_whitespace))(input)
}

fn parse_env_var(input: &str) -> ParseResult<EnvVar> {
  let (input, name) = parse_env_var_name(input)?;
  let (input, _) = char('=')(input)?;
  let (input, value) = with_error_context(
    terminated(parse_env_var_value, assert_whitespace_or_end),
    concat!(
      "Environment variable values may only be assigned quoted strings ",
      "or a plain string (consisting only of letters, numbers, and underscores)",
    ),
  )(input)?;
  Ok((input, EnvVar::new(name.to_string(), value)))
}

fn parse_env_var_name(input: &str) -> ParseResult<&str> {
  take_while(is_valid_env_var_char)(input)
}

fn parse_env_var_value(input: &str) -> ParseResult<StringParts> {
  or(parse_quoted_string, parse_word)(input)
}

fn parse_word(input: &str) -> ParseResult<StringParts> {
  assert(
    parse_string_parts(|c| {
      !c.is_whitespace() && (c == '$' || !is_special_char(c))
    }),
    |result| {
      result
        .ok()
        .map(|(_, parts)| {
          if parts.0.len() == 1 {
            if let StringPart::Text(text) = &parts.0[0] {
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
) -> impl Fn(&str) -> ParseResult<StringParts> {
  debug_assert!(!text.contains('$')); // not implemented
  move |input| {
    let (input, word) = parse_word(input)?;
    if word.0.len() == 1 {
      if let StringPart::Text(part_text) = &word.0[0] {
        if part_text == text {
          return ParseResult::Ok((input, word));
        }
      }
    }
    ParseError::backtrace()
  }
}

fn parse_quoted_string(input: &str) -> ParseResult<StringParts> {
  or(
    map(parse_single_quoted_string, StringParts::new_text),
    parse_double_quoted_string,
  )(input)
}

fn parse_single_quoted_string(input: &str) -> ParseResult<&str> {
  // single quoted strings cannot contain a single quote
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_02_02
  delimited(
    char('\''),
    take_while(|c| c != '\''),
    assert_exists(char('\''), "Expected closing single quote."),
  )(input)
}

fn parse_double_quoted_string(input: &str) -> ParseResult<StringParts> {
  // https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_02_03
  // Double quotes may have escaped
  delimited(
    char('"'),
    parse_string_parts(|c| c != '"'),
    assert_exists(char('"'), "Expected closing double quote."),
  )(input)
}

pub(crate) fn parse_string_parts(
  allow_char: impl Fn(char) -> bool,
) -> impl Fn(&str) -> ParseResult<StringParts> {
  move |input| {
    // this doesn't seem like the nom way of doing things, but it was a
    // quick implementation

    // todo(dsherret): improve this. This is not good code.
    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut last_escape = false;
    let mut index = 0;
    let mut chars = input.chars().peekable();
    while let Some(c) = chars.next() {
      if !allow_char(c) && !last_escape {
        break;
      }

      if last_escape {
        if !"\"$`".contains(c) {
          current_text.push('\\');
        }
        current_text.push(c);
        last_escape = false;
      } else if c == '\\' {
        last_escape = true;
      } else if c == '$'
        && chars
          .peek()
          .map(|c| is_valid_env_var_char(*c))
          .unwrap_or(false)
      {
        if !current_text.is_empty() {
          parts.push(StringPart::Text(current_text));
          current_text = String::new();
        }
        while chars
          .peek()
          .map(|c| is_valid_env_var_char(*c))
          .unwrap_or(false)
        {
          current_text.push(chars.next().unwrap());
          index += 1;
        }
        parts.push(StringPart::Variable(current_text));
        current_text = String::new();
      } else if c == '$' && chars.peek().map(|c| *c == '(').unwrap_or(false) {
        return ParseError::fail(
          &input[index..],
          "Subshells are currently not supported.",
        );
      } else if c == '`' {
        return ParseError::fail(
          &input[index..],
          "Back ticks in strings is currently not supported.",
        );
      } else {
        current_text.push(c);
      }
      index += 1;
    }
    if !current_text.is_empty() {
      parts.push(StringPart::Text(current_text));
    }
    Ok((&input[index..], StringParts(parts)))
  }
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
    if !next_char.is_whitespace() && !matches!(next_char, ';' | '&' | '|' | '(')
    {
      return Err(ParseError::Failure(fail_for_trailing_input(input)));
    }
  }
  Ok((input, ()))
}

fn special_char(input: &str) -> ParseResult<char> {
  if let Some((index, next_char)) = input.char_indices().next() {
    if is_special_char(next_char) {
      return Ok((&input[index..], next_char));
    }
  }
  ParseError::backtrace()
}

fn is_special_char(c: char) -> bool {
  "*~(){}<>$|&;\"'".contains(c)
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
  if parse_redirect(input).is_ok() {
    ParseErrorFailure::new(input, "Redirects are currently not supported.")
  } else if input.starts_with('*') {
    ParseErrorFailure::new(input, "Globs are currently not supported.")
  } else {
    ParseErrorFailure::new(input, "Unsupported character.")
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
      concat!("Unsupported character.\n", "  && testing\n", "  ~",),
    );
    assert_eq!(
      parse("test { test").err().unwrap().to_string(),
      concat!("Unsupported character.\n", "  { test\n", "  ~",),
    );
    assert_eq!(
      parse("test > redirect").err().unwrap().to_string(),
      concat!(
        "Redirects are currently not supported.\n",
        "  > redirect\n",
        "  ~",
      ),
    );
    assert_eq!(
      parse("cp test/* other").err().unwrap().to_string(),
      concat!("Globs are currently not supported.\n", "  * other\n", "  ~",),
    );
  }

  #[test]
  fn test_sequential_list() {
    run_test(
      parse_sequential_list,
      "Name=Value OtherVar=Other command arg1 || command2 arg12 arg13 ; command3 && command4 & command5 ; export ENV6=5 ; ENV7=other && command8 || command9",
      Ok(SequentialList {
        items: vec![
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: Sequence::Command(Command {
                env_vars: vec![
                  EnvVar::new("Name".to_string(), StringParts::new_text("Value")),
                  EnvVar::new("OtherVar".to_string(), StringParts::new_text("Other")),
                ],
                name: StringParts::new_text("command"),
                args: vec![StringParts::new_text("arg1")],
              }),
              op: BooleanListOperator::Or,
              next: Sequence::Command(Command {
                env_vars: vec![],
                name: StringParts::new_text("command2"),
                args: vec![
                  StringParts::new_text("arg12"),
                  StringParts::new_text("arg13"),
                ],
              }),
            })),
          },
          SequentialListItem {
            is_async: true,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: Sequence::Command(Command {
                env_vars: vec![],
                name: StringParts::new_text("command3"),
                args: vec![],
              }),
              op: BooleanListOperator::And,
              next: Sequence::Command(Command {
                env_vars: vec![],
                name: StringParts::new_text("command4"),
                args: vec![],
              }),
            })),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::Command(Command {
              env_vars: vec![],
              name: StringParts::new_text("command5"),
              args: vec![ ],
            }),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::EnvVar(EnvVar::new("ENV6".to_string(), StringParts::new_text("5"))),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::BooleanList(Box::new(BooleanList {
              current: Sequence::ShellVar(EnvVar::new("ENV7".to_string(), StringParts::new_text("other"))),
              op: BooleanListOperator::And,
              next: Sequence::BooleanList(Box::new(BooleanList {
                current: Sequence::Command(Command {
                  env_vars: vec![],
                  name: StringParts::new_text("command8"),
                  args: vec![],
                }),
                op: BooleanListOperator::Or,
                next: Sequence::Command(Command {
                  env_vars: vec![],
                  name: StringParts::new_text("command9"),
                  args: vec![],
                }),
              })),
            })),
          },
        ],
      })
    );

    run_test(
      parse_sequential_list,
      "command1 ; command2 ; A='b' command3",
      Ok(SequentialList {
        items: vec![
          SequentialListItem {
            is_async: false,
            sequence: Sequence::Command(Command {
              env_vars: vec![],
              name: StringParts::new_text("command1"),
              args: vec![],
            }),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::Command(Command {
              env_vars: vec![],
              name: StringParts::new_text("command2"),
              args: vec![],
            }),
          },
          SequentialListItem {
            is_async: false,
            sequence: Sequence::Command(Command {
              env_vars: vec![EnvVar::new(
                "A".to_string(),
                StringParts::new_text("b"),
              )],
              name: StringParts::new_text("command3"),
              args: vec![],
            }),
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
          sequence: Sequence::Command(Command {
            env_vars: vec![],
            name: StringParts::new_text("command"),
            args: vec![],
          }),
        }],
      }),
    );

    run_test(
      parse_sequential_list,
      "test | other",
      Ok(SequentialList {
        items: vec![SequentialListItem {
          is_async: false,
          sequence: Sequence::Pipeline(Box::new(Pipeline {
            current: Sequence::Command(Command {
              env_vars: vec![],
              name: StringParts::new_text("test"),
              args: vec![],
            }),
            next: Sequence::Command(Command {
              env_vars: vec![],
              name: StringParts::new_text("other"),
              args: vec![],
            }),
          })),
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
          sequence: Sequence::Command(Command {
            env_vars: vec![],
            name: StringParts::new_text("echo"),
            args: vec![StringParts(vec![StringPart::Variable(
              "MY_ENV".to_string(),
            )])],
          }),
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
        value: StringParts::new_text("Value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name='quoted value'",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringParts::new_text("quoted value"),
      }),
    );
    run_test(
      parse_env_var,
      "Name=\"double quoted value\"",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringParts::new_text("double quoted value"),
      }),
    );
    run_test_with_end(
      parse_env_var,
      "Name= command_name",
      Ok(EnvVar {
        name: "Name".to_string(),
        value: StringParts(vec![]),
      }),
      " command_name",
    );
    run_test(
      parse_env_var,
      "Name=$(test)",
      Err(concat!(
        "Environment variable values may only be assigned quoted strings or a ",
        "plain string (consisting only of letters, numbers, and underscores)\n\n",
        "Subshells are currently not supported.")),
    );
  }

  #[test]
  fn test_single_quotes() {
    run_test(
      parse_quoted_string,
      "'test'",
      Ok(StringParts::new_text("test")),
    );
    run_test(
      parse_quoted_string,
      r#"'te\\'"#,
      Ok(StringParts::new_text(r#"te\\"#)),
    );
    run_test_with_end(
      parse_quoted_string,
      r#"'te\'st'"#,
      Ok(StringParts::new_text(r#"te\"#)),
      "st'",
    );
    run_test(parse_quoted_string, "'  '", Ok(StringParts::new_text("  ")));
    run_test(
      parse_quoted_string,
      "'  ",
      Err("Expected closing single quote."),
    );
  }

  #[test]
  fn test_double_quotes() {
    run_test(
      parse_quoted_string,
      r#""  ""#,
      Ok(StringParts::new_text("  ")),
    );
    run_test(
      parse_quoted_string,
      r#""test""#,
      Ok(StringParts::new_text("test")),
    );
    run_test(
      parse_quoted_string,
      r#""te\"\$\`st""#,
      Ok(StringParts::new_text(r#"te"$`st"#)),
    );
    run_test(
      parse_quoted_string,
      r#""  "#,
      Err("Expected closing double quote."),
    );
    run_test(
      parse_quoted_string,
      r#""$Test""#,
      Ok(StringParts(vec![StringPart::Variable("Test".to_string())])),
    );
    run_test(
      parse_quoted_string,
      r#""$Test,$Other_Test""#,
      Ok(StringParts(vec![
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
      parse_quoted_string,
      r#""test" asdf"#,
      Ok(StringParts::new_text("test")),
      " asdf",
    );
  }

  #[test]
  fn test_parse_word() {
    run_test(parse_word, "if", Err("Unsupported reserved word."));
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
        assert_eq!(err.message, expected.err().unwrap());
      }
    }
  }
}
