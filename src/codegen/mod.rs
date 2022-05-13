pub mod ssm;

mod builtin;
#[cfg(test)]
mod test;

pub use ssm::{SsmInstruction, SsmInstructions};
