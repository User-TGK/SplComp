#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenType,
    line: u32,
    column: u32,
}

/// Represents terminal tokens.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenType {
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

    // Literals
    /// Integer literals
    Integer(i64),
    /// Boolean literals, "True" or "False"
    Bool(bool),
    /// Character literals like "'a'"
    Char(char),

    /// Identifiers like "var_name" or "funcName123"
    Identifier(String),

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
    Error(String),
}

pub struct Scanner<'a> {
    text: &'a str,
    current_line: u32,
    current_column: u32,
}

impl<'a> Scanner<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            current_line: 0,
            current_column: 0,
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // Remove single line comments
        while self.text.starts_with("//") {
            let line_index = self.text.find('\n')?;
            self.text = &self.text[line_index + 1..];

            self.current_line += 1;
        }

        // Remove multiline comments
        while self.text.starts_with("/*") {
            let close_index = self.text.find("*/");
            if close_index.is_none() {
                return Some(Token {
                    kind: TokenType::Error(self.text.to_owned()),
                    line: self.current_line,
                    column: self.current_column,
                });
            }
            self.text = &self.text[close_index.unwrap() + 2..];
        }

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
}
