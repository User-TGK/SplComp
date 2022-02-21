#![allow(non_snake_case)]

use group3::parser::program_parser;
use group3::scanner::Scanner;
use group3::token::{Token, Tokens};

include!(concat!(env!("OUT_DIR"), "/tests_gen.rs"));
