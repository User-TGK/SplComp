use crate::error::ErrorKind;

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Self { kind, line, column }
    }

    pub fn lexical_analysis_order() -> impl Iterator<Item = TokenKind> {
        [
            TokenKind::Plus,
            // Integer must be before Minus
            TokenKind::Integer(i64::default()),
            TokenKind::Minus,
            TokenKind::Divide,
            TokenKind::Times,
            TokenKind::Modulo,
            // Le before Lt
            TokenKind::Le,
            TokenKind::Lt,
            // Ge before Gt
            TokenKind::Ge,
            TokenKind::Gt,
            TokenKind::Equals,
            TokenKind::NotEquals,
            TokenKind::And,
            TokenKind::Or,
            TokenKind::Cons,
            TokenKind::Not,
            TokenKind::Bool(bool::default()),
            TokenKind::Char(char::default()),
            TokenKind::Var,
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::Return,
            TokenKind::IntType,
            TokenKind::BoolType,
            TokenKind::CharType,
            TokenKind::Hd,
            TokenKind::Tl,
            TokenKind::Fst,
            TokenKind::Snd,
            // Identifier after keywords starting with an alpha character
            TokenKind::Identifier(String::default()),
            TokenKind::Semicolon,
            TokenKind::Comma,
            TokenKind::OpeningParen,
            TokenKind::ClosingParen,
            TokenKind::OpeningBrace,
            TokenKind::ClosingBrace,
            TokenKind::OpeningSquare,
            TokenKind::ClosingSquare,
        ]
        .into_iter()
    }
}

/// Represents terminal tokens.
#[derive(Debug, Eq, PartialEq)]
pub enum TokenKind {
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
    Error(ErrorKind),
}

impl TokenKind {
    /// Unique names for each token kind, used to construct named regular expression
    /// groups for the lexical analysis.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Plus => "Pl",
            Self::Minus => "Mi",
            Self::Divide => "Di",
            Self::Times => "Ti",
            Self::Modulo => "Mo",
            Self::Equals => "Eq",
            Self::Lt => "Lt",
            Self::Gt => "Gt",
            Self::Le => "Le",
            Self::Ge => "Ge",
            Self::NotEquals => "Ne",
            Self::And => "An",
            Self::Or => "Or",
            Self::Cons => "Co",
            Self::Not => "No",
            Self::Integer(_) => "In",
            Self::Bool(_) => "Bo",
            Self::Char(_) => "Ch",
            Self::Identifier(_) => "Id",
            Self::Var => "Va",
            Self::If => "If",
            Self::Else => "El",
            Self::While => "Wh",
            Self::Return => "Re",
            Self::IntType => "It",
            Self::BoolType => "Bt",
            Self::CharType => "Ct",
            Self::Hd => "Hd",
            Self::Tl => "Tl",
            Self::Fst => "Fs",
            Self::Snd => "Snd",
            Self::Semicolon => "Se",
            Self::Comma => "Cm",
            Self::OpeningParen => "Op",
            Self::ClosingParen => "Cp",
            Self::OpeningBrace => "Ob",
            Self::ClosingBrace => "Cb",
            Self::OpeningSquare => "Os",
            Self::ClosingSquare => "Cs",
            Self::Error(_) => "",
        }
    }

    /// Simple regular expressions patterns used to identity tokens. Note that regexes may have overlap,
    /// hence the order in which they are evaluated matters.
    ///
    /// https://docs.rs/regex/latest/regex/#syntax
    pub fn pattern(&self) -> Option<&'static str> {
        match self {
            Self::Plus => Some(r"\+"),
            Self::Minus => Some(r"-"),
            Self::Divide => Some(r"/"),
            Self::Times => Some(r"\*"),
            Self::Modulo => Some(r"%"),
            Self::Equals => Some(r"=="),
            Self::Lt => Some(r"<"),
            Self::Gt => Some(r">"),
            Self::Le => Some(r"<="),
            Self::Ge => Some(r">="),
            Self::NotEquals => Some(r"!="),
            Self::And => Some(r"&&"),
            Self::Or => Some(r"\|\|"),
            Self::Cons => Some(r":"),
            Self::Not => Some(r"!"),
            Self::Integer(_) => Some(r"[-]?([0-9])+"),
            Self::Bool(_) => Some(r"True|False"),
            Self::Char(_) => Some(r"'[0-9a-zA-Z.\[\]]'"),
            Self::Identifier(_) => Some(r"[[:alpha:]](_|[[:alnum:]])*"),
            Self::Var => Some(r"var\b"),
            Self::If => Some(r"if\b"),
            Self::Else => Some(r"else\b"),
            Self::While => Some(r"while\b"),
            Self::Return => Some(r"return\b"),
            Self::IntType => Some(r"Int\b"),
            Self::BoolType => Some(r"Bool\b"),
            Self::CharType => Some(r"Char\b"),
            Self::Hd => Some(r"\.hd"),
            Self::Tl => Some(r"\.tl"),
            Self::Fst => Some(r"\.fst"),
            Self::Snd => Some(r"\.snd"),
            Self::Semicolon => Some(r";"),
            Self::Comma => Some(r","),
            Self::OpeningParen => Some(r"\("),
            Self::ClosingParen => Some(r"\)"),
            Self::OpeningBrace => Some(r"\{"),
            Self::ClosingBrace => Some(r"\}"),
            Self::OpeningSquare => Some(r"\["),
            Self::ClosingSquare => Some(r"\]"),
            Self::Error(_) => None,
        }
    }
}
