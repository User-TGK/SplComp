use crate::error::ErrorKind;

use nom::{InputIter, InputLength, InputTake, Needed, Slice};

use num_bigint::BigUint;

use std::fmt;
use std::iter::Enumerate;
use std::ops::{Deref, Range, RangeFrom, RangeFull, RangeTo};

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Tokens<'a> {
    inner: &'a [Token<'a>],
    pub raw: &'a str,
}

impl<'a> Tokens<'a> {
    pub fn new(tokens: &'a [Token<'a>], raw: &'a str) -> Self {
        Self { inner: tokens, raw }
    }

    pub fn inner(&self) -> &'a [Token<'a>] {
        self.inner
    }

    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}

// impl<'a> Deref for Tokens<'a> {
//     type Target = str;
//     fn deref(&self) -> &'a str {
//         if self.raw.len() > 1 && self.inner.len() > 1 {
//             let hd = &self.inner[0];
//             if self.raw.len() > hd.index {
//                 if self.raw.len() > hd.index + hd.size - 1 {
//                     &self.raw[hd.index..hd.index + hd.size - 1]
//                 } else {
//                     &self.raw[hd.index..]
//                 }
//             } else {
//                 &self.raw
//             }
//         }
//         //

//         // println!("Deref, first token index: {:?}", hd.index);
//         else {
//             &self.raw
//         }
//     }
// }

impl<'a> InputLength for Tokens<'a> {
    fn input_len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    fn take(&self, count: usize) -> Self {
        println!("Called take with count {}", count);

        Tokens {
            inner: &self.inner[0..count],
            raw: &self.raw,
        }
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        let (p, s) = self.inner.split_at(count);

        (
            Self {
                inner: s,
                raw: self.raw,
            },
            Self {
                inner: p,
                raw: self.raw,
            },
        )
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            inner: self.inner.slice(range),
            raw: self.raw,
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..(self.inner.len()))
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            inner: self.inner,
            raw: &self.raw,
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.inner.iter().enumerate()
    }

    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
        self.inner.iter()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.inner.iter().position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.inner.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}

impl<'a, Idx> std::ops::Index<Idx> for Tokens<'a>
where
    Idx: std::slice::SliceIndex<[Token<'a>]>,
{
    type Output = Idx::Output;

    fn index(&self, index: Idx) -> &Self::Output {
        &self.inner[index]
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: usize,
    pub column: usize,
    pub index: usize,
    pub size: usize,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind<'a>, line: usize, column: usize, index: usize, size: usize) -> Self {
        Self {
            kind,
            line,
            column,
            index,
            size,
        }
    }

    pub fn lexical_analysis_order() -> impl Iterator<Item = TokenKind<'a>> {
        [
            // Composite symbol tokens before similar single-symbol tokens
            TokenKind::DoubleColon,
            TokenKind::RightArrow,
            TokenKind::Plus,
            TokenKind::Integer(BigUint::default()),
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
            // Equals before Assignment
            TokenKind::Equals,
            TokenKind::NotEquals,
            TokenKind::And,
            TokenKind::Or,
            TokenKind::Cons,
            TokenKind::Not,
            TokenKind::Assignment,
            TokenKind::Bool(bool::default()),
            TokenKind::Char(char::default()),
            TokenKind::String(String::new()),
            TokenKind::Var,
            TokenKind::If,
            TokenKind::Else,
            TokenKind::While,
            TokenKind::Return,
            TokenKind::IntType,
            TokenKind::BoolType,
            TokenKind::CharType,
            TokenKind::VoidType,
            TokenKind::Hd,
            TokenKind::Tl,
            TokenKind::Fst,
            TokenKind::Snd,
            // EmptyList before OpeningBrace
            TokenKind::EmptyList,
            // Identifier after keywords starting with an alpha character
            TokenKind::Identifier(""),
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
#[derive(Clone, Debug, Eq, PartialEq)]
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
    Assignment,

    // Literals
    /// Integer literals
    Integer(BigUint),
    /// Boolean literals, "True" or "False"
    Bool(bool),
    /// Character literals like "'a'"
    Char(char),
    /// String literals like <"Hello world">, <"Hello \" world"> or <"Hello \\ world">
    String(String),
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

    /// "::"
    DoubleColon,

    /// "->"
    RightArrow,

    /// "Void"
    VoidType,

    // Field access
    /// ".hd"
    Hd,
    /// ".tl"
    Tl,
    /// ".fst"
    Fst,
    /// ".snd"
    Snd,

    // "[]"
    EmptyList,

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

impl<'a> TokenKind<'a> {
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
            Self::Assignment => "As",
            Self::Or => "Or",
            Self::Cons => "Co",
            Self::Not => "No",
            Self::Integer(_) => "In",
            Self::Bool(_) => "Bo",
            Self::Char(_) => "Ch",
            Self::String(_) => "St",
            Self::Identifier(_) => "Id",
            Self::Var => "Va",
            Self::If => "If",
            Self::Else => "El",
            Self::While => "Wh",
            Self::Return => "Re",
            Self::IntType => "It",
            Self::BoolType => "Bt",
            Self::CharType => "Ct",
            Self::VoidType => "Vt",
            Self::DoubleColon => "Ft",
            Self::RightArrow => "Ra",
            Self::Hd => "Hd",
            Self::Tl => "Tl",
            Self::Fst => "Fs",
            Self::Snd => "Snd",
            Self::EmptyList => "Em",
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
            Self::Assignment => Some(r"="),
            Self::Integer(_) => Some(r"([0-9])+"),
            Self::Bool(_) => Some(r"True|False"),
            // Either an escape sequence, or any character other than ', \
            Self::Char(_) => Some(r"'((\\.)|([^'\\]))'"),
            // Either an escape sequence, or any character other than ", \
            Self::String(_) => Some(r#""((\\.)|([^"\\]))*""#),
            Self::Identifier(_) => Some(r"[[:alpha:]](_|[[:alnum:]])*"),
            Self::Var => Some(r"var\b"),
            Self::If => Some(r"if\b"),
            Self::Else => Some(r"else\b"),
            Self::While => Some(r"while\b"),
            Self::Return => Some(r"return\b"),
            Self::IntType => Some(r"Int\b"),
            Self::BoolType => Some(r"Bool\b"),
            Self::CharType => Some(r"Char\b"),
            Self::DoubleColon => Some(r"::"),
            Self::RightArrow => Some("->"),
            Self::VoidType => Some(r"Void\b"),
            Self::Hd => Some(r"\.hd"),
            Self::Tl => Some(r"\.tl"),
            Self::Fst => Some(r"\.fst"),
            Self::Snd => Some(r"\.snd"),
            Self::EmptyList => Some(r"\[\]"),
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

impl<'a> fmt::Display for TokenKind<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Var => "var",
                TokenKind::Assignment => "=",
                TokenKind::Semicolon => ";",
                TokenKind::OpeningBrace => "{",
                TokenKind::ClosingBrace => "}",
                TokenKind::DoubleColon => "::",
                TokenKind::RightArrow => "->",
                TokenKind::IntType => "Int",
                TokenKind::BoolType => "Bool",
                TokenKind::CharType => "Char",
                TokenKind::VoidType => "Void",
                TokenKind::OpeningSquare => "[",
                TokenKind::ClosingSquare => "]",
                TokenKind::If => "if",
                TokenKind::Else => "else",
                TokenKind::While => "while",
                TokenKind::Return => "return",
                TokenKind::Or => "||",
                TokenKind::And => "&&",
                TokenKind::Equals => "==",
                TokenKind::NotEquals => "!=",
                TokenKind::Lt => "<",
                TokenKind::Le => "<=",
                TokenKind::Gt => ">",
                TokenKind::Ge => ">=",
                TokenKind::Cons => ":",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Times => "*",
                TokenKind::Divide => "/",
                TokenKind::Modulo => "%",
                TokenKind::Not => "!",
                TokenKind::OpeningParen => "(",
                TokenKind::ClosingParen => ")",
                TokenKind::EmptyList => "[]",
                TokenKind::Comma => ",",
                TokenKind::Hd => ".hd",
                TokenKind::Tl => ".tl",
                TokenKind::Fst => ".fst",
                TokenKind::Snd => ".snd",
                TokenKind::Integer(_) => "int literal",
                TokenKind::Bool(_) => "bool literal",
                TokenKind::Char(_) => "char literal",
                TokenKind::String(_) => "string literal",
                TokenKind::Identifier(_) => "identifier",
                TokenKind::Error(_e) => "error",
            }
        )
    }
}
