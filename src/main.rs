use group3::ast::pp::PrettyPrintable;
use group3::parser::*;
use group3::scanner::Scanner;
use group3::tc::*;
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

    match program_parser(tokens) {
        Ok((r, mut program)) => {
            assert!(r.is_empty());

            run(&mut program)?;

            let max_line = Some(40);
            let tab_size = 4;

            println!("// Successfully parsed and type checked input file.");
            println!("{}", to_string(&program.to_pretty(), max_line, tab_size));
        }
        Err(e) => {
            println!("{:?}", e);
        }
    }

    Ok(())
}
