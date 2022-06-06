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
        .unwrap()
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

fn ssm_code_gen_test_helper(node: impl SsmInstructions, expected: Vec<SsmInstruction>) {
    let mut env = LocationEnv::default();
    let mut heap_offset = 0;
    let mut prefix_gen = LabelPrefixGenerator::default();

    let instructions = node.instructions(&mut env, &mut heap_offset, &mut prefix_gen);

    assert_eq!(instructions, Ok(expected));
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
        Box::new(Expr::Atom(Atom::CharLiteral('a')).into()),
        Box::new(
            Expr::Cons(
                Box::new(Expr::Atom(Atom::CharLiteral('3')).into()),
                Box::new(Expr::Atom(Atom::EmptyList).into()),
            )
            .into(),
        ),
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

#[test]
fn test_material_sum_example_ssm() {
    let output = "6\n6\n6\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("material/tests/sum.spl"), output);
}

#[test]
fn test_example_print_ssm() {
    let output = "1\na\n(2,b)\n((1,2),(a,b))\n[(1,2),(5,6)]\n[[(1,2),(5,6)],[(1,2),(5,6)]]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/print.spl"), output);
}

#[test]
fn test_example_mutec_ssm() {
    let output = "-1\n0\n0\n-1\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/mutrec.spl"), output);
}

#[test]
fn test_example_string_ssm() {
    let output = "[H,e,l,l,o, ,w,o,r,l,d]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/string.spl"), output);
}

#[test]
fn test_example_string_literal_ssm() {
    let output = "H\ne\nl\nl\no\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/string_literal.spl"), output);
}

#[test]
fn test_example_shadowing_ssm() {
    let output = "1\n2\n3\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/shadowing.spl"), output);
}

#[test]
fn test_example_vowels_ssm() {
    let output = "-1\n0\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/vowels.spl"), output);
}

#[test]
fn test_example_reverse_ssm() {
    let output = "2\n[1]\n7\n[6,5,4]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/reverse.spl"), output);
}

#[test]
fn test_example_sum_ssm() {
    let output = "15\n5050\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/sum.spl"), output);
}

#[test]
fn test_example_side_effect_eval_ssm() {
    let output = "[l,i,n,k,s]\n[r,e,c,h,t,s]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/side_effect_eval.spl"), output);
}

#[test]
fn test_example_pass_by_value_ssm() {
    let output = "10\n5\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/pass_by_value.spl"), output);
}

#[test]
fn test_example_pass_by_reference_ssm() {
    let output = "0\n1\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/pass_by_reference.spl"), output);
}

#[test]
fn test_example_equals_ssm() {
    let output = "[a, ,=,=, ,b]\n[b, ,!,=, ,c]\n[c,.,f,s,t, ,=,=, ,d,.,f,s,t]\n[d, ,=,=, ,e]\n\nmachine halted\n";
    ssm_code_gen_test_helper_output(String::from("examples/equals.spl"), output);
}
