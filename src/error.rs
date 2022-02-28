#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ErrorKind {
    // Lexer errors
    IllegalToken(String),
    UnclosedMultiLineComment,
    IllegalEscape(String),

    Unknown,
}

impl Default for ErrorKind {
    fn default() -> Self {
        Self::Unknown
    }
}
