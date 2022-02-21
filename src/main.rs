use std::error::Error;
use std::{env, fs};

use group3::scanner::Scanner;
use group3::token::Token;

pub fn main() -> Result<(), Box<dyn Error>> {
    let mut args = env::args();
    let binary = args.next().ok_or("No binary available")?;
    let input_filename = args
        .next()
        .ok_or_else(|| format!("Usage: {} <filename>", binary))?;

    let input = fs::read_to_string(input_filename)?;
    let scanner = Scanner::new(&input);
    let tokens: Vec<Token> = scanner.collect();

    println!("{:#?}", tokens);

    Ok(())
}
