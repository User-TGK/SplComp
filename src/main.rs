use group3::parser::pp::PrettyPrintable;
use group3::parser::*;
use group3::scanner::Scanner;
use group3::token::{Token, Tokens};

use nom::{Err, Finish};
use pretty_trait::to_string;

use std::error::Error;
use std::{env, fs};

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let binary = args.next().ok_or("No binary available")?;
    let input_filename = args
        .next()
        .ok_or_else(|| format!("Usage: {} <filename>", binary))?;

    let input = fs::read_to_string(input_filename)?;
    let scanner = Scanner::new(&input);
    let tokens: Vec<Token> = scanner.collect();

    let tokens = Tokens::new(&tokens);

    match program_parser(tokens).finish() {
        Ok((t, ast)) => {
            if t.is_empty() {
                let max_line = Some(40);
                let tab_size = 4;

                println!("// Successfully parsed input file.");
                println!("{}", to_string(&ast.to_pretty(), max_line, tab_size));
            } else {
                match program_parser(t).finish() {
                    Ok(_) => {
                        // There was unparsed input, so we know parsing went wrong somewhere
                        unreachable!();
                    }
                    Err(err) => {
                        eprintln!("{:#?}", err);
                    }
                }
            }
        }

        Err(err) => {
            if err.input.is_empty() {
                eprintln!("{:#?}", err);
            } else {
                let head = &err.input[0];
                eprintln!("Error at line {}, column {}: expected {}, found {}", head.line, head.column, err.context.unwrap(), head.kind);
            }
        }
    }

    Ok(())
}
