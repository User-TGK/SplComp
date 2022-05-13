use crate::codegen::ssm::*;
use crate::{ast::*, codegen::ssm::LabelPrefixGenerator};

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
