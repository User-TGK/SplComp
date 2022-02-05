#[derive(Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    line: u32,
    column: u32,
}

/// Represents terminal tokens.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind<'a> {
    // Operators
    /// "+"
    Plus,
    /// "-"
    Minus,
    /// "/"
    Divide,
    /// "*"
    Times,
    /// "%"
    Modulo,
    /// "=="
    Equals,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "<="
    Le,
    /// ">="
    Ge,
    /// "!="
    NotEquals,
    /// "&&"
    And,
    /// "||"
    Or,
    /// ":"
    Cons,
    /// "!"
    Not,
    /// "="
    Assign,

    // Literals
    /// Integer literals
    Integer(i64),
    /// Boolean literals, "True" or "False"
    Bool(bool),
    /// Character literals like "'a'"
    Char(char),

    /// Identifiers like "var_name" or "funcName123"
    Identifier(&'a str),

    // Keywords
    /// "var"
    Var,
    /// "if"
    If,
    /// "else"
    Else,
    /// "while"
    While,
    /// "return"
    Return,

    // Types
    /// "Int"
    IntType,
    /// "Bool"
    BoolType,
    /// "Char"
    CharType,

    // Field access
    /// ".hd"
    Hd,
    /// ".tl"
    Tl,
    /// ".fst"
    Fst,
    /// ".snd"
    Snd,

    // Symbols
    /// ";"
    Semicolon,
    /// ","
    Comma,
    /// "("
    OpeningParen,
    /// ")"
    ClosingParen,
    /// "{"
    OpeningBrace,
    /// "}"
    ClosingBrace,
    /// "["
    OpeningSquare,
    /// "]"
    ClosingSquare,

    /// Errors
    Error(&'a str),
}

pub struct Scanner<'a> {
    text: &'a str,
    line: u32,
    column: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            line: 0,
            column: 0,
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.text.is_empty() {
            return None;
        }

        loop {
            if self.text.starts_with(char::is_whitespace) {
                // Skip whitespace characters

                let c = self.text.chars().next().unwrap();
                if c == '\n' {
                    self.line += 1;
                    self.column = 0;
                } else {
                    self.column += c.len_utf8() as u32;
                }
                self.text = &self.text[c.len_utf8()..];
            } else if self.text.starts_with("//") {
                // Skip single line comments

                let line_index = self.text.find('\n')?;
                self.line += 1;
                self.column = 0;
                self.text = &self.text[line_index..];
            } else if self.text.starts_with("/*") {
                match self.text.find("*/") {
                    Some(close_index) => {
                        // Remove multi-line comments

                        for c in self.text[..close_index + 2].chars() {
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                            } else {
                                self.column += c.len_utf8() as u32;
                            }
                        }
                        self.text = &self.text[close_index + 2..];
                    }
                    None => {
                        // Return an error for unclosed multi-line comments

                        let err = Token {
                            kind: TokenKind::Error(self.text),
                            line: self.line,
                            column: self.column,
                        };

                        // Skip the rest of the text
                        for c in self.text.chars() {
                            if c == '\n' {
                                self.line += 1;
                                self.column = 0;
                            } else {
                                self.column += c.len_utf8() as u32;
                            }
                        }
                        self.text = "";

                        return Some(err);
                    }
                }
            } else {
                break;
            }
        }

        // TODO: tokenize non-whitespace/comments

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_remove_single_line_comment() {
        let mut scanner = Scanner::new("// This is a comment.");
        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_remove_multi_line_comment() {
        let multiline_comment = r"/*
        This is the first line of the comment.
        This is the second line of the comment.
        This is the third line of the comment.
        */";

        let mut scanner = Scanner::new(multiline_comment);
        assert_eq!(scanner.next(), None);
    }

    #[test]
    fn test_remove_broken_multi_line_comment() {
        let broken_comment = r"/*
        This is a broken comment";

        let mut scanner = Scanner::new(broken_comment);
        assert_eq!(scanner.next(), Some(Token {
            kind: TokenKind::Error(broken_comment),
            line: 0,
            column: 0,
        }));
    }
}
