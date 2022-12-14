use super::*;

macro_rules! boxed_int_literal (
        ($value:expr) => (
            Box::new(Expr::Atom(Atom::IntLiteral(u32::into($value))).into())
        )
    );

#[test]
fn test_field_access() {
    const CODE: &str = "x.hd.tl.fst.snd";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::Atom(Atom::Variable(Variable::new(
        None,
        Id(String::from("x")),
        vec![Field::Hd, Field::Tl, Field::Fst, Field::Snd],
    )))
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_tuple_expression() {
    const CODE: &str = "(2 * (1 + 6), 8)";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::Atom(Atom::Tuple(
        Box::new(
            Expr::Mul(
                boxed_int_literal!(2),
                Box::new(Expr::Add(boxed_int_literal!(1), boxed_int_literal!(6)).into()),
            )
            .into(),
        ),
        boxed_int_literal!(8),
    ))
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_parenthesized_expr_precedence() {
    const CODE: &str = "(2 + 6) / 4";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::Div(
        Box::new(Expr::Add(boxed_int_literal!(2), boxed_int_literal!(6)).into()),
        boxed_int_literal!(4),
    )
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_disjun_expr_parser() {
    const CODE: &str = "1 <= 2 || 9 > 3 || 5 == 5";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = disjun_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::Or(
        Box::new(
            Expr::Or(
                Box::new(Expr::Le(boxed_int_literal!(1), boxed_int_literal!(2)).into()),
                Box::new(Expr::Gt(boxed_int_literal!(9), boxed_int_literal!(3)).into()),
            )
            .into(),
        ),
        Box::new(Expr::Equals(boxed_int_literal!(5), boxed_int_literal!(5)).into()),
    )
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_conjun_expr_parser() {
    const CODE: &str = "1 <= 2 && 9 > 3 && 5 == 5";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = conjun_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::And(
        Box::new(
            Expr::And(
                Box::new(Expr::Le(boxed_int_literal!(1), boxed_int_literal!(2)).into()),
                Box::new(Expr::Gt(boxed_int_literal!(9), boxed_int_literal!(3)).into()),
            )
            .into(),
        ),
        Box::new(Expr::Equals(boxed_int_literal!(5), boxed_int_literal!(5)).into()),
    )
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_compare_expr_parser() {
    const CODE: &str = "1 > 2 == 3+1 <= 4";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = compare_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);

    let expected1 = Expr::Le(
        Box::new(
            Expr::Equals(
                Box::new(Expr::Gt(boxed_int_literal!(1), boxed_int_literal!(2)).into()),
                Box::new(Expr::Add(boxed_int_literal!(3), boxed_int_literal!(1)).into()),
            )
            .into(),
        ),
        boxed_int_literal!(4),
    )
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_concat_expr_parser() {
    const CODE: &str = "1:2:3";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = concat_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);
    let expected1 = Expr::Cons(
        boxed_int_literal!(1),
        Box::new(Expr::Cons(boxed_int_literal!(2), boxed_int_literal!(3)).into()),
    )
    .into();

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_term_expr_parser() {
    const CODE: &str = r"24+6/3-8";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = term_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);
    let expected1 = Expr::Sub(
        Box::new(
            Expr::Add(
                boxed_int_literal!(24),
                Box::new(Expr::Div(boxed_int_literal!(6), boxed_int_literal!(3)).into()),
            )
            .into(),
        ),
        boxed_int_literal!(8),
    )
    .into();
    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_factor_expr_parser() {
    const CODE: &str = "3*9%2/-26";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE);

    let r1 = factor_expr_parser(tokens);
    let t1 = Tokens::new(&[], CODE);
    let expected1 = Expr::Div(
        Box::new(
            Expr::Mod(
                Box::new(Expr::Mul(boxed_int_literal!(3), boxed_int_literal!(9)).into()),
                boxed_int_literal!(2),
            )
            .into(),
        ),
        Box::new(Expr::UnaryMinus(boxed_int_literal!(26)).into()),
    )
    .into();
    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_program_parser() {
    const PROGRAM: &str = r"
        var myVar = 123;
        myFun(a, b) {return;}
        var another_var = False;
        ";
    let tokens: Vec<Token> = Scanner::new(PROGRAM).unwrap().collect();
    let tokens = Tokens::new(&tokens, PROGRAM);

    let (rest, program) = program_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        program,
        Program {
            var_decls: vec![
                VarDecl {
                    var_type: None,
                    name: Id("myVar".to_string()),
                    value: Expr::Atom(Atom::IntLiteral(123u32.into())).into()
                },
                VarDecl {
                    var_type: None,
                    name: Id("another_var".to_string()),
                    value: Expr::Atom(Atom::BoolLiteral(false)).into(),
                },
            ],
            fun_decls: vec![FunDecl {
                name: Id("myFun".to_string()),
                params: vec![Id("a".to_string()), Id("b".to_string())],
                fun_type: None,
                var_decls: vec![],
                statements: vec![Statement::Return(None)],
            }]
        }
    )
}

#[test]
fn test_var_decl_parser() {
    // Inferred type
    const ASSIGN_INFERRED: &str = r"var x = True;";

    let tokens: Vec<Token> = Scanner::new(ASSIGN_INFERRED).unwrap().collect();
    let tokens = Tokens::new(&tokens, ASSIGN_INFERRED);

    let (rest, var_decl) = var_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        var_decl,
        VarDecl {
            var_type: None,
            name: Id("x".to_string()),
            value: Expr::Atom(Atom::BoolLiteral(true)).into(),
        }
    );

    // Explicit type

    const ASSIGN_EXPLICIT: &str = r"Int y = 42;";

    let tokens: Vec<Token> = Scanner::new(ASSIGN_EXPLICIT).unwrap().collect();
    let tokens = Tokens::new(&tokens, ASSIGN_EXPLICIT);

    let (rest, var_decl) = var_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        var_decl,
        VarDecl {
            var_type: Some(Type::Int),
            name: Id("y".to_string()),
            value: Expr::Atom(Atom::IntLiteral(42u32.into())).into(),
        }
    );
}

#[test]
fn test_if_statement_parser() {
    // No if_true, no if_false
    const IF_TRUE: &str = r"if (True) {}";

    let tokens: Vec<Token> = Scanner::new(IF_TRUE).unwrap().collect();
    let tokens = Tokens::new(&tokens, IF_TRUE);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)).into(),
            if_true: Vec::new(),
            if_false: Vec::new(),
        })
    );

    // Only if_true

    const ONLY_IF_TRUE: &str = r"if (True) {
            if (False) {}
        }";
    let tokens: Vec<Token> = Scanner::new(ONLY_IF_TRUE).unwrap().collect();
    let tokens = Tokens::new(&tokens, ONLY_IF_TRUE);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)).into(),
            if_true: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)).into(),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
            if_false: Vec::new(),
        })
    );

    // Both if_true and if_false

    // Note that `if ('x') { ... }` is not correctly typed, but it's correct syntax
    const IF_TRUE_IF_FALSE: &str = r"if (True) {
            if (False) {}
        } else {
            if ('x') {}
        }";
    let tokens: Vec<Token> = Scanner::new(IF_TRUE_IF_FALSE).unwrap().collect();
    let tokens = Tokens::new(&tokens, IF_TRUE_IF_FALSE);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)).into(),
            if_true: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)).into(),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
            if_false: vec![Statement::If(If {
                cond: Expr::Atom(Atom::CharLiteral('x')).into(),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
        })
    );
}

#[test]
fn test_while_statement_parser() {
    // No body
    const CODE1: &str = r"while (True) {}";

    let tokens: Vec<Token> = Scanner::new(CODE1).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE1);

    let (rest, statement) = while_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::While(While {
            cond: Expr::Atom(Atom::BoolLiteral(true)).into(),
            body: Vec::new(),
        })
    );

    // With body

    const CODE2: &str = r"while (True) {
            if (False) {}
        }";
    let tokens: Vec<Token> = Scanner::new(CODE2).unwrap().collect();
    let tokens = Tokens::new(&tokens, CODE2);

    let (rest, statement) = while_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::While(While {
            cond: Expr::Atom(Atom::BoolLiteral(true)).into(),
            body: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)).into(),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
        })
    );
}

#[test]
fn test_assign_statement_parser() {
    // Simple assignment

    let tokens: Vec<Token> = Scanner::new("myVar = 123;").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(None, Id("myVar".to_string()), Vec::new()),
            value: Expr::Atom(Atom::IntLiteral(123u32.into())).into(),
        })
    );

    // Complex expression

    let tokens: Vec<Token> = Scanner::new("myVar = !(myVar && True);").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());

    let value = Expr::Not(Box::new(
        Expr::And(
            Box::new(
                Expr::Atom(Atom::Variable(Variable::new(
                    None,
                    Id("myVar".to_string()),
                    Vec::new(),
                )))
                .into(),
            ),
            Box::new(Expr::Atom(Atom::BoolLiteral(true)).into()),
        )
        .into(),
    ))
    .into();

    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(None, Id("myVar".to_string()), Vec::new()),
            value: value,
        })
    );

    // Field access

    let tokens: Vec<Token> = Scanner::new("myVar.fst.snd.tl.hd = [];").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(
                None,
                Id("myVar".to_string()),
                vec![Field::Fst, Field::Snd, Field::Tl, Field::Hd]
            ),
            value: Expr::Atom(Atom::EmptyList).into(),
        })
    );
}

#[test]
fn test_fun_call_statement_parser() {
    // No arguments

    let tokens: Vec<Token> = Scanner::new("someFunction();").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = fun_call_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::FunCall(FunCall::new(Id("someFunction".to_string()), Vec::new()))
    );

    // With arguments

    let tokens: Vec<Token> = Scanner::new("functionCall(123, False);").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = fun_call_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::FunCall(FunCall::new(
            Id("functionCall".to_string()),
            vec![
                Expr::Atom(Atom::IntLiteral(123u32.into())).into(),
                Expr::Atom(Atom::BoolLiteral(false)).into(),
            ]
        ))
    );
}

#[test]
fn test_return_statement_parser() {
    // No value

    let tokens: Vec<Token> = Scanner::new("return;").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = return_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(statement, Statement::Return(None));

    // With value

    let tokens: Vec<Token> = Scanner::new("return 54321;").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, statement) = return_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Return(Some(Expr::Atom(Atom::IntLiteral(54321u32.into())).into()))
    );
}

#[test]
fn test_fun_decl_type_parser() {
    // No params, void return type

    let tokens: Vec<Token> = Scanner::new(":: -> Void").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_type) = function_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(fun_type, Type::Function(Vec::new(), Box::new(Type::Void)));

    // Params and return type

    let tokens: Vec<Token> = Scanner::new(":: Int Bool -> Char").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_type) = function_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_type,
        Type::Function(vec![Type::Int, Type::Bool], Box::new(Type::Char))
    );

    // Complex types

    let tokens: Vec<Token> = Scanner::new(":: (a, [b]) [(Int,c)] -> ((a,b),c)")
        .unwrap()
        .collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_type) = function_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_type,
        Type::Function(
            vec![
                Type::Tuple(
                    Box::new(Type::Var(Id("a".to_string()))),
                    Box::new(Type::List(Box::new(Type::Var(Id("b".to_string()))))),
                ),
                Type::List(Box::new(Type::Tuple(
                    Box::new(Type::Int),
                    Box::new(Type::Var(Id("c".to_string()))),
                )),),
            ],
            Box::new(Type::Tuple(
                Box::new(Type::Tuple(
                    Box::new(Type::Var(Id("a".to_string()))),
                    Box::new(Type::Var(Id("b".to_string()))),
                )),
                Box::new(Type::Var(Id("c".to_string()))),
            ))
        )
    );
}

#[test]
fn test_fun_call_in_return() {
    let tokens: Vec<Token> = Scanner::new("f (x) {return g (x);}").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r1 = program_parser(tokens);

    let t1 = [];

    let expected1 = Program {
        var_decls: vec![],
        fun_decls: vec![FunDecl {
            name: Id("f".to_string()),
            params: vec![Id("x".to_string())],
            fun_type: None,
            var_decls: vec![],
            statements: vec![Statement::Return(Some(
                Expr::Atom(Atom::FunCall(FunCall::new(
                    Id(String::from("g")),
                    vec![Expr::Atom(Atom::Variable(Variable::new(
                        None,
                        Id(String::from("x")),
                        vec![],
                    )))
                    .into()],
                )))
                .into(),
            ))],
        }],
    };

    assert_eq!(r1, Ok((Tokens::new(&t1, ""), expected1)));
}

#[test]
fn test_variable_in_return() {
    let tokens: Vec<Token> = Scanner::new("f (x) {return g;}").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r1 = program_parser(tokens);

    let t1 = [];

    let expected1 = Program {
        var_decls: vec![],
        fun_decls: vec![FunDecl {
            name: Id("f".to_string()),
            params: vec![Id("x".to_string())],
            fun_type: None,
            var_decls: vec![],
            statements: vec![Statement::Return(Some(
                Expr::Atom(
                    Atom::Variable(Variable::new(None, Id(String::from("g")), vec![])).into(),
                )
                .into(),
            ))],
        }],
    };

    assert_eq!(r1, Ok((Tokens::new(&t1, ""), expected1)));
}

#[test]
fn fest_fun_decl_var_decls_at_start() {
    let tokens: Vec<Token> = Scanner::new("myFun() { var x = 1; var y = 2; return; }")
        .unwrap()
        .collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();
    assert!(rest.is_empty());
    assert_eq!(
        fun_decl,
        FunDecl {
            name: Id("myFun".to_string()),
            params: vec![],
            fun_type: None,
            var_decls: vec![
                VarDecl {
                    var_type: None,
                    name: Id("x".to_string()),
                    value: Expr::Atom(Atom::IntLiteral(1u32.into())).into(),
                },
                VarDecl {
                    var_type: None,
                    name: Id("y".to_string()),
                    value: Expr::Atom(Atom::IntLiteral(2u32.into())).into(),
                }
            ],
            statements: vec![Statement::Return(None)],
        }
    );

    let tokens: Vec<Token> = Scanner::new("myFun() { var x = 1;  return; var y = 2;}")
        .unwrap()
        .collect();
    let tokens = Tokens::new(&tokens, "");

    assert!(fun_decl_parser(tokens).is_err());
}

#[test]
fn test_fun_decl_parser() {
    // Body with only statements

    let tokens: Vec<Token> = Scanner::new("myFun(x, y) { return; }").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_decl,
        FunDecl {
            name: Id("myFun".to_string()),
            params: vec![Id("x".to_string()), Id("y".to_string())],
            fun_type: None,
            var_decls: vec![],
            statements: vec![Statement::Return(None)],
        }
    );

    // Body with variable declarations and statements

    const CODE: &str = r"someFunction(a, b) :: Int [Int] -> [Int] {
            Int someVar = 0;
            return;
        }";

    let tokens: Vec<Token> = Scanner::new(CODE).unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_decl,
        FunDecl {
            name: Id("someFunction".to_string()),
            params: vec![Id("a".to_string()), Id("b".to_string())],
            fun_type: Some(Type::Function(
                vec![Type::Int, Type::List(Box::new(Type::Int))],
                Box::new(Type::List(Box::new(Type::Int)))
            )),
            var_decls: vec![VarDecl {
                var_type: Some(Type::Int),
                name: Id("someVar".to_string()),
                value: Expr::Atom(Atom::IntLiteral(0u32.into())).into(),
            }],
            statements: vec![Statement::Return(None),],
        }
    );
}

#[test]
fn test_type_parser() {
    let types = [
        // Basic
        ("Int", Type::Int),
        ("Bool", Type::Bool),
        ("Char", Type::Char),
        (
            "(Int,Bool)",
            Type::Tuple(Box::new(Type::Int), Box::new(Type::Bool)),
        ),
        ("[Char]", Type::List(Box::new(Type::Char))),
        ("a", Type::Var(Id("a".to_string()))),
        // More complex
        (
            "((Int,Bool),Char)",
            Type::Tuple(
                Box::new(Type::Tuple(Box::new(Type::Int), Box::new(Type::Bool))),
                Box::new(Type::Char),
            ),
        ),
        (
            "[(a,b)]",
            Type::List(Box::new(Type::Tuple(
                Box::new(Type::Var(Id("a".to_string()))),
                Box::new(Type::Var(Id("b".to_string()))),
            ))),
        ),
    ];

    for (code, expected) in types {
        let tokens: Vec<Token> = Scanner::new(code).unwrap().collect();
        let tokens = Tokens::new(&tokens, "");

        let (rest, t) = type_parser(tokens).unwrap();

        assert!(rest.is_empty());
        assert_eq!(t, expected);
    }
}

#[test]
fn test_unary_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"!True").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r1 = unary_expr_parser(tokens);
    let t1 = Tokens::new(&[], "");
    let expected1 = Expr::Not(Box::new(Expr::Atom(Atom::BoolLiteral(true)).into())).into();
    assert_eq!(r1, Ok((t1, expected1)));

    let tokens: Vec<Token> = Scanner::new(&"-3").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r2 = unary_expr_parser(tokens);
    let t2 = Tokens::new(&[], "");
    let expected2 = Expr::UnaryMinus(boxed_int_literal!(3)).into();
    assert_eq!(r2, Ok((t2, expected2)));

    let tokens: Vec<Token> = Scanner::new(&"---4").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r2 = unary_expr_parser(tokens);
    let t2 = Tokens::new(&[], "");
    let expected2 = Expr::UnaryMinus(Box::new(
        Expr::UnaryMinus(Box::new(Expr::UnaryMinus(boxed_int_literal!(4)).into())).into(),
    ))
    .into();
    assert_eq!(r2, Ok((t2, expected2)));
}

#[test]
fn test_atom_expr_parser() {
    let tokens = [
        Token::new(TokenKind::Bool(true), 0, 0, 0, 0),
        Token::new(TokenKind::Identifier("x"), 0, 0, 0, 0),
    ];

    let tokens = Tokens::new(&tokens, "");

    let r1 = atom_expr_parser(tokens);
    let t1 = Tokens::new(&tokens[1..], "");
    assert_eq!(r1, Ok((t1, Expr::Atom(Atom::BoolLiteral(true)).into())));

    let r2 = atom_expr_parser(t1);
    let t2 = Tokens::new(&tokens[2..], "");
    let var = Variable::new(None, Id(String::from("x")), Vec::new());
    assert_eq!(r2, Ok((t2, Expr::Atom(Atom::Variable(var)).into())));
}

#[test]
fn test_fun_call_parser() {
    let tokens: Vec<Token> = Scanner::new(&"my_fun(123, x.hd, False)").unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let r1 = fun_call_parser(tokens);
    let t1 = Tokens::new(&[], "");

    let expected1 = FunCall::new(
        Id(String::from("my_fun")),
        vec![
            Expr::Atom(Atom::IntLiteral(123u32.into())).into(),
            Expr::Atom(Atom::Variable(Variable::new(
                None,
                Id(String::from("x")),
                vec![Field::Hd],
            )))
            .into(),
            Expr::Atom(Atom::BoolLiteral(false)).into(),
        ],
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_literal_parser() {
    let tokens = [
        Token::new(TokenKind::Bool(true), 0, 0, 0, 0),
        Token::new(TokenKind::Char('c'), 1, 0, 0, 0),
        Token::new(TokenKind::Integer(123u32.into()), 2, 0, 0, 0),
        Token::new(
            // No escape sequences, those are handled in the scanner already
            TokenKind::String(r#"Hello " world with \ problematic characters"#.to_string()),
            3,
            0,
            0,
            0,
        ),
    ];

    let tokens = Tokens::new(&tokens, "");

    let r1 = literal_atom_parser(tokens);
    let t1 = Tokens::new(&tokens[1..], "");
    assert_eq!(r1, Ok((t1, Atom::BoolLiteral(true))));

    let r2 = literal_atom_parser(t1);
    let t2 = Tokens::new(&tokens[2..], "");
    assert_eq!(r2, Ok((t2, Atom::CharLiteral('c'))));

    let r3 = literal_atom_parser(t2);
    let t3 = Tokens::new(&tokens[3..], "");
    assert_eq!(r3, Ok((t3, Atom::IntLiteral(123u32.into()))));

    let r4 = literal_atom_parser(t3);
    let t4 = Tokens::new(&tokens[4..], "");
    assert_eq!(
        r4,
        Ok((
            t4,
            Atom::StringLiteral(r#"Hello " world with \ problematic characters"#.to_string())
        ))
    );
}
