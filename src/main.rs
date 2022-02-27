use group3::parser::pp::PrettyPrintable;
use group3::parser::*;
use group3::scanner::Scanner;
use group3::token::{Token, Tokens};

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

    if let Ok((t, ast)) = program_parser(tokens) {
        if t.is_empty() {
            let max_line = Some(40);
            let tab_size = 4;

            println!("// Successfully parsed input file.");
            println!("{}", to_string(&ast.to_pretty(), max_line, tab_size));
        } else {
            println!("Parser did not complete, remaining tokens: {:?}", t);
        }
    } else {
        println!("Parser error");
    }

    Ok(())
}
