#[derive(Debug, Eq, PartialEq)]
pub enum ErrorKind {
    // Lexer errors
    IllegalToken(String),
    UnclosedMultiLineComment,

    Unknown,
}

impl Default for ErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}