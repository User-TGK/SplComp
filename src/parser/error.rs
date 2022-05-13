use super::token::{Token, TokenKind, Tokens};

use nom::error::{ContextError, ErrorKind as NomErrorKind, ParseError};

use thiserror::Error as ThisError;

use std::fmt::Write;

/// Lexer generated errors
#[derive(Clone, Debug, Eq, PartialEq, ThisError)]
pub enum LexerErrorKind {
    EmptyInput,
    IllegalToken(String),
    UnclosedMultiLineComment,
    IllegalEscape(String),
    Unknown,
}

impl std::fmt::Display for LexerErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerErrorKind::EmptyInput => write!(f, "{}", "The input file is empty"),
            LexerErrorKind::IllegalToken(i) => write!(f, "{} '{}'", "Illegal token", i),
            LexerErrorKind::UnclosedMultiLineComment => {
                write!(f, "{}", "Unclosed multiline comment")
            }
            LexerErrorKind::IllegalEscape(s) => write!(f, "{} '{}'", "Illegal escape sequence", s),
            LexerErrorKind::Unknown => write!(f, "{}", "Unknown lexer error"),
        }
    }
}

/// Parser generated errors
#[derive(Debug, PartialEq)]
pub enum ParserErrorKind<'a> {
    Context(&'static str),
    Nom(NomErrorKind),
    Expected(TokenKind<'a>),
    ExpectedContext(&'static str),
}

impl Default for LexerErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}

#[derive(Debug, PartialEq)]
pub struct Error<'a> {
    pub errors: Vec<(Tokens<'a>, ParserErrorKind<'a>)>,
}

impl<'a> Error<'a> {
    pub fn add_expected(input: Tokens<'a>, kind: TokenKind<'a>, mut other: Self) -> Self {
        other.errors.push((input, ParserErrorKind::Expected(kind)));
        other
    }

    pub fn add_expected_context(input: Tokens<'a>, e: &'static str, mut other: Self) -> Self {
        other
            .errors
            .push((input, ParserErrorKind::ExpectedContext(e)));
        other
    }

    /// Function that returns the entire string of the raw input on which a
    /// token occured.
    pub fn get_line(token: &Token<'a>, raw: &'a str) -> &'a str {
        let line_begin = raw[0..token.index]
            .chars()
            .rev()
            .position(|b| b == '\n')
            .unwrap_or(token.index);

        let line_end = raw[token.index..]
            .chars()
            .position(|b| b == '\n')
            .unwrap_or(raw.len());

        &raw[token.index - line_begin..token.index + line_end]
    }

    pub fn convert(&self, input: &Tokens) -> String {
        let mut result = std::string::String::new();

        let mut scanner_errors = vec![];

        for (tokens, kind) in &self.errors {
            let raw = tokens.raw;

            // Lexer error
            if let Some((token, e)) = tokens.inner().iter().find_map(|t| {
                if let TokenKind::Error(ref e) = t.kind {
                    Some((t, e))
                } else {
                    None
                }
            }) {
                if !scanner_errors.contains(&e) {
                    scanner_errors.push(e);

                    let line = Error::get_line(&token, raw);

                    match e {
                        LexerErrorKind::EmptyInput => {}

                        LexerErrorKind::IllegalToken(t) => {
                            let mut t = t.to_string();
                            t.truncate(t.trim_end().len());

                            write!(
                                &mut result,
                                "Illegal token at {line_number}:{column_number}:\n\
                            {line}\n\
                            {caret:>column$}\n\
                            found: '{token}'\n\n",
                                line_number = token.line + 1,
                                column_number = token.column + 1,
                                line = line,
                                token = t
                                    .strip_suffix("\r\n")
                                    .or(t.strip_suffix('\n'))
                                    .unwrap_or(&t),
                                caret = '^',
                                column = token.column + 1,
                            )
                            .unwrap();
                        }
                        LexerErrorKind::UnclosedMultiLineComment => {
                            write!(
                                &mut result,
                                "Unclosed multi-line comment starting at {line_number}:{column_number}:\n\
                        {line}\n\
                        {caret:>column$}\n\
                        Missing closing token '*/' \n\n",
                                line_number = token.line + 1,
                                column_number = token.column + 1,
                                line = line,
                                caret = '^',
                                column = token.column + 1,
                            )
                            .unwrap();
                        }

                        LexerErrorKind::IllegalEscape(s) => {
                            write!(
                                &mut result,
                                "Illegal escape sequence at {line_number}:{column_number}:\n\
                        {line}\n\
                        {caret:>column$}\n\
                        Found unknown escaped character '{found}' \n\n",
                                line_number = token.line + 1,
                                column_number = token.column + 1,
                                line = line,
                                caret = '^',
                                column = token.column + 1,
                                found = s,
                            )
                            .unwrap();
                        }

                        LexerErrorKind::Unknown => {}
                    }
                }

                continue;
            }

            let hd = &tokens
                .inner()
                .get(0)
                .unwrap_or(input.inner().last().unwrap());
            let line = Error::get_line(hd, raw);

            match kind {
                ParserErrorKind::Context(s) => {
                    write!(
                        &mut result,
                        "at {line_number}:{column_number}, in {context}:\n\
                        {line}\n\
                        {caret:>column$}\n\n",
                        line_number = hd.line + 1,
                        column_number = hd.column + 1,
                        line = line,
                        context = s,
                        caret = '^',
                        column = hd.column,
                    )
                    .unwrap();
                }
                ParserErrorKind::Expected(t) => {
                    write!(
                        &mut result,
                        "at {line_number}:{column_number}:\n\
                        {line}\n\
                        {caret:>column$}\n\
                        expected '{expected}'\n\n",
                        line_number = hd.line + 1,
                        column_number = hd.column + 1,
                        line = line,
                        caret = '^',
                        column = hd.column,
                        expected = t,
                    )
                    .unwrap();
                }
                ParserErrorKind::ExpectedContext(c) => {
                    write!(
                        &mut result,
                        "at {line_number}:{column_number}:\n\
                        {line}\n\
                        {caret:>column$}\n\
                        expected {context}\n\n",
                        line_number = hd.line + 1,
                        column_number = hd.column + 1,
                        line = line,
                        caret = '^',
                        column = hd.column,
                        context = &c,
                    )
                    .unwrap();
                }

                _ => {}
            }
        }

        result
    }
}

impl<'a> ParseError<Tokens<'a>> for Error<'a> {
    fn from_error_kind(input: Tokens<'a>, kind: NomErrorKind) -> Self {
        Self {
            errors: vec![(input, ParserErrorKind::Nom(kind))],
        }
    }

    fn append(input: Tokens<'a>, kind: NomErrorKind, mut other: Self) -> Self {
        other.errors.push((input, ParserErrorKind::Nom(kind)));
        other
    }
}

impl<'a> ContextError<Tokens<'a>> for Error<'a> {
    fn add_context(input: Tokens<'a>, ctx: &'static str, mut other: Self) -> Self {
        other.errors.push((input, ParserErrorKind::Context(ctx)));
        other
    }
}
