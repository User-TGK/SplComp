use crate::codegen::c::{CEnv, ToC};
use crate::parser::*;
use crate::tc;

use pretty_trait::to_string;

use std::fs;
use std::io::Write;
use std::path::Path;
use std::process::Command;

fn c_code_gen_test_helper_output(input_filename: String, expected_output: &str) {
    let path = Path::new(&input_filename);
    let input = fs::read_to_string(path).unwrap();
    let tokens: Vec<Token> = Scanner::new(&input).unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, mut program) = program_parser(tokens).unwrap();
    assert!(rest.is_empty());

    assert_eq!(Ok(()), tc::run(&mut program));

    let in_file_name = path.file_name().unwrap().to_str().unwrap();
    let out_file = format!("test_{}.out.c", in_file_name.clone());
    let mut file = fs::File::create(out_file.clone()).unwrap();

    let max_line = Some(40);
    let tab_size = 4;

    let mut env = CEnv::default();

    file.write_all(&to_string(&program.to_c(&mut env).unwrap(), max_line, tab_size).as_bytes())
        .unwrap();

    let binary_name = format!("bin/spl_program_test_{}", in_file_name);

    let build_output = Command::new("gcc")
        .arg("-o")
        .arg(binary_name.clone())
        .arg(format!("{}", out_file))
        .output()
        .expect("Failed to build C binary");

    if !build_output.status.success() {
        println!(
            "GCC failed. stderr: {}",
            std::str::from_utf8(&build_output.stderr).unwrap()
        );
    }

    let program_output = Command::new(binary_name)
        .output()
        .expect("Failed to run C binary");

    if !program_output.status.success() {
        println!(
            "C binary failed with {}. stderr: {}",
            program_output.status,
            std::str::from_utf8(&program_output.stderr).unwrap()
        );
    }

    assert_eq!(
        std::str::from_utf8(&program_output.stdout).unwrap(),
        expected_output
    );
}

#[test]
fn test_material_sum_example_c() {
    let output = "6\n6\n6\n";
    c_code_gen_test_helper_output(String::from("material/tests/sum.spl"), output);
}

#[test]
fn test_example_print_c() {
    let output = "1\na\n(2,b)\n((1,2),(a,b))\n[(1,2),(5,6)]\n[[(1,2),(5,6)],[(1,2),(5,6)]]\n";
    c_code_gen_test_helper_output(String::from("examples/print.spl"), output);
}

#[test]
fn test_example_mutrec_c() {
    let output = "1\n0\n0\n1\n";
    c_code_gen_test_helper_output(String::from("examples/mutrec.spl"), output);
}

#[test]
fn test_example_string_c() {
    let output = "[H,e,l,l,o, ,w,o,r,l,d]\n";
    c_code_gen_test_helper_output(String::from("examples/string.spl"), output);
}

#[test]
fn test_example_string_literal_c() {
    let output = "H\ne\nl\nl\no\n";
    c_code_gen_test_helper_output(String::from("examples/string_literal.spl"), output);
}

#[test]
fn test_example_vowels_c() {
    let output = "1\n0\n";
    c_code_gen_test_helper_output(String::from("examples/vowels.spl"), output);
}

#[test]
fn test_example_reverse_c() {
    let output = "2\n[1]\n7\n[6,5,4]\n";
    c_code_gen_test_helper_output(String::from("examples/reverse.spl"), output);
}

#[test]
fn test_example_sum_c() {
    let output = "15\n5050\n";
    c_code_gen_test_helper_output(String::from("examples/sum.spl"), output);
}

#[test]
fn test_example_side_effect_eval_c() {
    let output = "[l,i,n,k,s]\n[r,e,c,h,t,s]\n";
    c_code_gen_test_helper_output(String::from("examples/side_effect_eval.spl"), output);
}

#[test]
fn test_example_pass_by_value_c() {
    let output = "10\n5\n";
    c_code_gen_test_helper_output(String::from("examples/pass_by_value.spl"), output);
}

#[test]
fn test_example_pass_by_reference_c() {
    let output = "0\n1\n";
    c_code_gen_test_helper_output(String::from("examples/pass_by_reference.spl"), output);
}

#[test]
fn test_example_equals_c() {
    let output = "[a, ,=,=, ,b]\n[b, ,!,=, ,c]\n[c,.,f,s,t, ,=,=, ,d,.,f,s,t]\n[d, ,=,=, ,e]\n";
    c_code_gen_test_helper_output(String::from("examples/equals.spl"), output);
}
