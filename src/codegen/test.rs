use crate::codegen::ssm;
use crate::codegen::ssm::*;
use crate::parser::*;
use crate::tc;
use crate::{ast::*, codegen::ssm::LabelPrefixGenerator};

use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::Command;

fn ssm_code_gen_test_helper_output(input_filename: String, expected_output: &str) {
    let path = Path::new(&input_filename);
    let input = fs::read_to_string(path).unwrap();
    let tokens: Vec<Token> = Scanner::new(&input).unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, mut program) = program_parser(tokens).unwrap();
    assert!(rest.is_empty());

    assert_eq!(Ok(()), tc::run(&mut program));

    let mut code_gen_env = ssm::LocationEnv::default();
    let mut heap_offset = 0;
    let mut prefix_gen = ssm::LabelPrefixGenerator::default();

    let ssm_instructions: Vec<String> = program
        .instructions(&mut code_gen_env, &mut heap_offset, &mut prefix_gen)
        .into_iter()
        .map(|i| i.into())
        .collect();

    let out_file = format!(
        "test_{}.out.ssm",
        path.file_name().unwrap().to_str().unwrap()
    );
    let mut file = fs::File::create(out_file.clone()).unwrap();

    for instruction in ssm_instructions {
        write!(file, "{}\n", instruction).unwrap();
    }

    let output = Command::new("java")
        .arg("-jar")
        .arg("ssm/ssm.jar")
        .arg("--cli")
        .arg("--haltonerror")
        .arg("--file")
        .arg(format!("{}", out_file))
        .output()
        .expect("Failed to execute SSM command");

    if !output.status.success() {
        println!(
            "SSM failed. stderr: {}",
            std::str::from_utf8(&output.stderr).unwrap()
        );
    }

    assert_eq!(
        std::str::from_utf8(&output.stdout).unwrap(),
        expected_output
    );
}

#[test]
fn ssm_print_output() {
    let output = "1\na\n(2,b)\n((1,2),(a,b))\n[(1,2),(5,6)]\n[[(1,2),(5,6)],[(1,2),(5,6)]]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("tests/ssm/print.spl"), output);
}

fn ssm_code_gen_test_helper(node: impl SsmInstructions, expected: Vec<SsmInstruction>) {
    let mut env = LocationEnv::default();
    let mut heap_offset = 0;
    let mut prefix_gen = LabelPrefixGenerator::default();

    let instructions = node.instructions(&mut env, &mut heap_offset, &mut prefix_gen);

    assert_eq!(instructions, expected);
}

#[test]
fn test_return_statement_ssm() {
    let void_return = Statement::Return(None);
    let expected_void_return_instructions = vec![SsmInstruction::Unlink, SsmInstruction::Ret];
    ssm_code_gen_test_helper(void_return, expected_void_return_instructions);

    let expr_return = Statement::Return(Some(Expr::Atom(Atom::IntLiteral(u32::into(1))).into()));
    let expected_expr_return_instructions = vec![
        SsmInstruction::Ldc(1),
        SsmInstruction::Str(RR.into()),
        SsmInstruction::Unlink,
        SsmInstruction::Ret,
    ];
    ssm_code_gen_test_helper(expr_return, expected_expr_return_instructions);
}

#[test]
fn test_string_lit_equivalence_ssm() {
    let literal = Atom::StringLiteral(String::from("a3"));
    let list = Expr::Cons(
        Box::new(Expr::Atom(Atom::CharLiteral('a'))),
        Box::new(Expr::Cons(
            Box::new(Expr::Atom(Atom::CharLiteral('3'))),
            Box::new(Expr::Atom(Atom::EmptyList)),
        )),
    );

    let expected_instructions = vec![
        SsmInstruction::Ldc(97),
        SsmInstruction::Ldc(51),
        SsmInstruction::Ldc(0),
        SsmInstruction::Stmh(2),
        SsmInstruction::Stmh(2),
    ];

    ssm_code_gen_test_helper(literal, expected_instructions.clone());
    ssm_code_gen_test_helper(list, expected_instructions);
}
