use nom::error::{ErrorKind, ParseError, ContextError};

#[derive(Debug, PartialEq)]
pub struct Error<I> {
    pub input: I,
    pub kind: ErrorKind,
    pub context: Option<&'static str>,
}

impl<I> ParseError<I> for Error<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        Self {
            input,
            kind,
            context: None,
        }
    }

    fn append(_input: I, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<I> ContextError<I> for Error<I> {
    fn add_context(_input: I, ctx: &'static str, mut other: Self) -> Self {
        other.context.get_or_insert(ctx);
        other
    }
}
