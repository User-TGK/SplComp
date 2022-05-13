#![allow(non_snake_case)]

use spl_compiler::ast::*;
use spl_compiler::parser::{program_parser, Scanner, Token, Tokens};
use spl_compiler::tc::run;

include!(concat!(env!("OUT_DIR"), "/tests_gen.rs"));
