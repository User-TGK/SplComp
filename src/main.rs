use spl_compiler::ast;
use spl_compiler::ast::pp::PrettyPrintable;
use spl_compiler::codegen::ssm;
use spl_compiler::codegen::*;
use spl_compiler::error::*;
use spl_compiler::parser;
use spl_compiler::parser::{Scanner, Token, Tokens};
use spl_compiler::tc::*;

use clap::Parser;

use pretty_trait::to_string;

use simple_logger::SimpleLogger;

use nom::Finish;

use std::fs;
use std::fs::File;
use std::io::Write;

/// An SPL-Compiler implementation for NWI-IMC004 Compiler Construction
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to the SPL source file to be compiled
    #[clap(short, long)]
    source_file: String,

    /// Which mode to use for compilation
    #[clap(short, long, default_value_t = Mode::Ssm)]
    mode: Mode,
}

#[derive(Parser, Debug)]
enum Mode {
    /// Parse the input and pretty print the constructed AST.
    PrettyPrint,
    /// Parse the input, perform type inference and print the decorated AST.
    PrettyPrintTyped,
    /// Parse the input, perform semantic analysis and generate SSM instructions.
    Ssm,
    /// Parse the input, perform semantic analysis and generate C code.
    C,
}

impl std::str::FromStr for Mode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "print" => Ok(Mode::PrettyPrint),
            "type" => Ok(Mode::PrettyPrintTyped),
            "ssm" => Ok(Mode::Ssm),
            "C" => Ok(Mode::C),
            _ => Err(format!("Unknown mode {}", s)),
        }
    }
}

impl std::fmt::Display for Mode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Mode::PrettyPrint => "print",
                Mode::PrettyPrintTyped => "type",
                Mode::Ssm => "ssm",
                Mode::C => "C",
            }
        )
    }
}

fn print(program: &ast::Program) -> Result<(), Error> {
    let max_line = Some(40);
    let tab_size = 4;

    let mut file = File::create("out.spl")?;
    file.write_all(&to_string(&program.to_pretty(), max_line, tab_size).as_bytes())?;

    Ok(())
}

pub fn main() -> Result<(), Error> {
    SimpleLogger::new().init().unwrap();

    let args = Args::parse();
    let input_filename = args.source_file;
    let mode = args.mode;

    // Stage 1: Lexing
    log::info!("Lexing...");

    let input = fs::read_to_string(input_filename)?;
    let scanner = Scanner::new(&input)?;
    let tokens: Vec<Token> = scanner.collect();

    let tokens = Tokens::new(&tokens, &input);

    // Stage 2: Parse
    log::info!("Parsing...");

    let parse_result = parser::program_parser(tokens).finish();

    // ERROR: Parsing failed
    if let Err(e) = parse_result {
        return Err(Error::ParserError(e.convert(&tokens)));
    }

    let (r, mut program) = parse_result.unwrap();
    // ERROR: Remaining tokens
    if !r.is_empty() {
        return Err(Error::RemainingInput(
            parser::error::Error::get_line(&r.inner().first().unwrap(), &tokens.raw).into(),
        ));
    }

    if let Mode::PrettyPrint = mode {
        return print(&program);
    }

    // Stage 3: Type checking & Semantic analysis
    log::info!("Typing...");

    run(&mut program).map_err(|e| Error::SemanticsError(e))?;

    match mode {
        Mode::PrettyPrintTyped => return print(&program),

        // Stage 4: SSM CodeGen
        Mode::Ssm => {
            let mut code_gen_env = ssm::LocationEnv::default();
            let mut heap_offset = 0;
            let mut prefix_gen = ssm::LabelPrefixGenerator::default();

            log::info!("Generating SSM instructions...");
            let ssm_instructions: Vec<String> = program
                .instructions(&mut code_gen_env, &mut heap_offset, &mut prefix_gen)
                .map_err(|e| Error::CodeGenError(e))?
                .into_iter()
                .map(|i| i.into())
                .collect();

            let mut file = File::create("out.ssm")?;

            for instruction in ssm_instructions {
                write!(file, "{}\n", instruction)?;
            }
        }
        // Stage 3: (Extension) C CodeGen
        Mode::C => {
            log::info!("Generating C code...");
            log::warn!("Unimplemented C extension");
        }
        _ => unreachable!(),
    }

    log::info!("Finished compiling!");
    Ok(())
}
