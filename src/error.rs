use crate::parser;

use thiserror::Error as ThisError;

#[derive(ThisError)]
pub enum Error {
    #[error("Error reading from input file")]
    IOError(#[from] std::io::Error),

    #[error("Remaining input could not be parsed")]
    RemainingInput(String),

    /// An error during lexing that will not be recovered.
    #[error("Error during lexing")]
    LexerError(#[from] parser::error::LexerErrorKind),

    /// An error during parsing.
    #[error("Error during parsing")]
    ParserError(String),

    // An error during semantic analysis.
    #[error("Error during binding time analysis, type checking or return path analysis")]
    SemanticsError(String),

    // An error during code generation.
    #[error("Error during code generation.")]
    CodeGenError(String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::IOError(e) => write!(f, "IO \n\n {:?}", &e),
            Error::RemainingInput(e) => write!(f, "Remaining input \n\n {}", &e),
            Error::LexerError(e) => write!(f, "\n\n {}", &e),
            Error::ParserError(e) => write!(f, "\n\n {}", &e),
            Error::SemanticsError(e) => write!(f, "Semantic analysis \n\n {}", &e),
            Error::CodeGenError(e) => write!(f, "Code generation \n\n {:?}", &e),
        }
    }
}
