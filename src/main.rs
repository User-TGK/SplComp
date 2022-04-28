use spl_compiler::ast::pp::PrettyPrintable;
use spl_compiler::parser::*;
use spl_compiler::scanner::Scanner;
use spl_compiler::tc::*;
use spl_compiler::token::{Token, Tokens};

use pretty_trait::to_string;

use simple_logger::SimpleLogger;

use nom::{error::*, Finish};

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::{env, fs};

pub fn main() -> Result<(), Box<dyn Error>> {
    SimpleLogger::new().init().unwrap();

    let mut args = env::args();
    let binary = args.next().ok_or("No binary available")?;
    let input_filename = args
        .next()
        .ok_or_else(|| format!("Usage: {} <filename>", binary))?;

    let input = fs::read_to_string(input_filename)?;
    let scanner = Scanner::new(&input);
    let tokens: Vec<Token> = scanner.collect();

    let tokens = Tokens::new(&tokens, &input);

    match program_parser(tokens).finish() {
        Ok((r, mut program)) => {
            if !(r.is_empty()) {
                println!("{:?}", r);
                assert!(r.is_empty());
            }

            run(&mut program)?;

            let max_line = Some(40);
            let tab_size = 4;

            let mut file = File::create("out.spl")?;
            file.write_all(&to_string(&program.to_pretty(), max_line, tab_size).as_bytes())?;
        }
        Err(e) => {
            println!("Parser error: \n\n{}", e.convert(&tokens));
        }
    }

    Ok(())
}
