#![allow(non_snake_case)]

use spl_compiler::parser::program_parser;
use spl_compiler::scanner::Scanner;
use spl_compiler::token::{Token, Tokens};

include!(concat!(env!("OUT_DIR"), "/tests_gen.rs"));
