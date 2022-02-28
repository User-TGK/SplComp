use super::*;
use crate::scanner::*;

macro_rules! boxed_int_literal (
        ($value:expr) => (
            Box::new(Expr::Atom(Atom::IntLiteral(u32::into($value))))
        )
    );

#[test]
fn test_field_access() {
    let tokens: Vec<Token> = Scanner::new(&"x.hd.tl.fst.snd").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::Atom(Atom::Variable(Variable::new(
        Id(String::from("x")),
        vec![Field::Hd, Field::Tl, Field::Fst, Field::Snd],
    )));

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_tuple_expression() {
    let tokens: Vec<Token> = Scanner::new(&"(2 * (1 + 6), 8)").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::Atom(Atom::Tuple(
        Box::new(Expr::Mul(
            boxed_int_literal!(2),
            Box::new(Expr::Add(boxed_int_literal!(1), boxed_int_literal!(6))),
        )),
        boxed_int_literal!(8),
    ));

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_parenthesized_expr_precedence() {
    let tokens: Vec<Token> = Scanner::new(&"(2 + 6) / 4").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::Div(
        Box::new(Expr::Add(boxed_int_literal!(2), boxed_int_literal!(6))),
        boxed_int_literal!(4),
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_disjun_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"1 <= 2 || 9 > 3 || 5 == 5").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = disjun_expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::Or(
        Box::new(Expr::Or(
            Box::new(Expr::Le(boxed_int_literal!(1), boxed_int_literal!(2))),
            Box::new(Expr::Gt(boxed_int_literal!(9), boxed_int_literal!(3))),
        )),
        Box::new(Expr::Equals(boxed_int_literal!(5), boxed_int_literal!(5))),
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_conjun_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"1 <= 2 && 9 > 3 && 5 == 5").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = conjun_expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::And(
        Box::new(Expr::And(
            Box::new(Expr::Le(boxed_int_literal!(1), boxed_int_literal!(2))),
            Box::new(Expr::Gt(boxed_int_literal!(9), boxed_int_literal!(3))),
        )),
        Box::new(Expr::Equals(boxed_int_literal!(5), boxed_int_literal!(5))),
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_compare_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"1 > 2 == 3+1 <= 4").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = compare_expr_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = Expr::Le(
        Box::new(Expr::Equals(
            Box::new(Expr::Gt(boxed_int_literal!(1), boxed_int_literal!(2))),
            Box::new(Expr::Add(boxed_int_literal!(3), boxed_int_literal!(1))),
        )),
        boxed_int_literal!(4),
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_concat_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"1:2:3").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = concat_expr_parser(tokens);
    let t1 = Tokens::new(&[]);
    let expected1 = Expr::Cons(
        boxed_int_literal!(1),
        Box::new(Expr::Cons(boxed_int_literal!(2), boxed_int_literal!(3))),
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_term_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"24+6/3-8").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = term_expr_parser(tokens);
    let t1 = Tokens::new(&[]);
    let expected1 = Expr::Sub(
        Box::new(Expr::Add(
            boxed_int_literal!(24),
            Box::new(Expr::Div(boxed_int_literal!(6), boxed_int_literal!(3))),
        )),
        boxed_int_literal!(8),
    );
    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_factor_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"3*9%2/-26").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = factor_expr_parser(tokens);
    let t1 = Tokens::new(&[]);
    let expected1 = Expr::Div(
        Box::new(Expr::Mod(
            Box::new(Expr::Mul(boxed_int_literal!(3), boxed_int_literal!(9))),
            boxed_int_literal!(2),
        )),
        Box::new(Expr::UnaryMinus(boxed_int_literal!(26))),
    );
    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_program_parser() {
    const PROGRAM: &str = r"
        var myVar = 123;
        myFun(a, b) {return;}
        var another_var = False;
        ";
    let tokens: Vec<Token> = Scanner::new(PROGRAM).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, program) = program_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        program,
        Program(vec![
            Decl::VarDecl(VarDecl {
                var_type: None,
                name: Id("myVar".to_string()),
                value: Expr::Atom(Atom::IntLiteral(123u32.into()))
            }),
            Decl::FunDecl(FunDecl {
                name: Id("myFun".to_string()),
                params: vec![Id("a".to_string()), Id("b".to_string())],
                fun_type: None,
                statements: vec![Statement::Return(None)],
            }),
            Decl::VarDecl(VarDecl {
                var_type: None,
                name: Id("another_var".to_string()),
                value: Expr::Atom(Atom::BoolLiteral(false)),
            }),
        ])
    )
}

#[test]
fn test_var_decl_parser() {
    // Inferred type

    let tokens: Vec<Token> = Scanner::new("var x = True;").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, var_decl) = var_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        var_decl,
        VarDecl {
            var_type: None,
            name: Id("x".to_string()),
            value: Expr::Atom(Atom::BoolLiteral(true)),
        }
    );

    // Explicit type

    let tokens: Vec<Token> = Scanner::new("Int y = 42;").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, var_decl) = var_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        var_decl,
        VarDecl {
            var_type: Some(Type::Int),
            name: Id("y".to_string()),
            value: Expr::Atom(Atom::IntLiteral(42u32.into())),
        }
    );
}

#[test]
fn test_if_statement_parser() {
    // No if_true, no if_false

    let tokens: Vec<Token> = Scanner::new("if (True) {}").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)),
            if_true: Vec::new(),
            if_false: Vec::new(),
        })
    );

    // Only if_true

    const ONLY_IF_TRUE: &str = r"if (True) {
            if (False) {}
        }";
    let tokens: Vec<Token> = Scanner::new(ONLY_IF_TRUE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)),
            if_true: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)),
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
    let tokens: Vec<Token> = Scanner::new(IF_TRUE_IF_FALSE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = if_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::If(If {
            cond: Expr::Atom(Atom::BoolLiteral(true)),
            if_true: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
            if_false: vec![Statement::If(If {
                cond: Expr::Atom(Atom::CharLiteral('x')),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
        })
    );
}

#[test]
fn test_while_statement_parser() {
    // No body

    let tokens: Vec<Token> = Scanner::new("while (True) {}").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = while_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::While(While {
            cond: Expr::Atom(Atom::BoolLiteral(true)),
            body: Vec::new(),
        })
    );

    // With body

    const CODE: &str = r"while (True) {
            if (False) {}
        }";
    let tokens: Vec<Token> = Scanner::new(CODE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = while_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::While(While {
            cond: Expr::Atom(Atom::BoolLiteral(true)),
            body: vec![Statement::If(If {
                cond: Expr::Atom(Atom::BoolLiteral(false)),
                if_true: Vec::new(),
                if_false: Vec::new(),
            })],
        })
    );
}

#[test]
fn test_assign_statement_parser() {
    // Simple assignment

    let tokens: Vec<Token> = Scanner::new("myVar = 123;").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(Id("myVar".to_string()), Vec::new()),
            value: Expr::Atom(Atom::IntLiteral(123u32.into())),
        })
    );

    // Complex expression

    let tokens: Vec<Token> = Scanner::new("myVar = !(myVar && True);").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());

    let value = Expr::Not(Box::new(Expr::And(
        Box::new(Expr::Atom(Atom::Variable(Variable::new(
            Id("myVar".to_string()),
            Vec::new(),
        )))),
        Box::new(Expr::Atom(Atom::BoolLiteral(true))),
    )));

    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(Id("myVar".to_string()), Vec::new()),
            value,
        })
    );

    // Field access

    let tokens: Vec<Token> = Scanner::new("myVar.fst.snd.tl.hd = [];").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = assign_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Assign(Assign {
            target: Variable::new(
                Id("myVar".to_string()),
                vec![Field::Fst, Field::Snd, Field::Tl, Field::Hd]
            ),
            value: Expr::Atom(Atom::EmptyList),
        })
    );
}

#[test]
fn test_fun_call_statement_parser() {
    // No arguments

    let tokens: Vec<Token> = Scanner::new("someFunction();").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = fun_call_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::FunCall(FunCall::new(Id("someFunction".to_string()), Vec::new()))
    );

    // With arguments

    let tokens: Vec<Token> = Scanner::new("functionCall(123, False);").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = fun_call_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::FunCall(FunCall::new(
            Id("functionCall".to_string()),
            vec![
                Expr::Atom(Atom::IntLiteral(123u32.into())),
                Expr::Atom(Atom::BoolLiteral(false)),
            ]
        ))
    );
}

#[test]
fn test_return_statement_parser() {
    // No value

    let tokens: Vec<Token> = Scanner::new("return;").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = return_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(statement, Statement::Return(None));

    // With value

    let tokens: Vec<Token> = Scanner::new("return 54321;").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, statement) = return_statement_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        statement,
        Statement::Return(Some(Expr::Atom(Atom::IntLiteral(54321u32.into()))))
    );
}

#[test]
fn test_var_decl_statement_parser() {
    const CODE: &str = r"f(x) {
    if (x) {
        var y = False;
    }
}";
    let tokens: Vec<Token> = Scanner::new(CODE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(fun_decl, FunDecl {
        name: Id("f".to_owned()),
        params: vec![Id("x".to_string())],
        fun_type: None,
        statements: vec![
            Statement::If(If {
                cond: Expr::Atom(Atom::Variable(Variable::new(Id("x".to_string()), Vec::new()))),
                if_true: vec![Statement::VarDecl(VarDecl {
                    var_type: None,
                    name: Id("y".to_string()),
                    value: Expr::Atom(Atom::BoolLiteral(false)),
                })],
                if_false: Vec::new(),
            }),
        ]
    })
}

#[test]
fn test_fun_decl_type_parser() {
    // No type

    let tokens: Vec<Token> = Scanner::new("").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_type) = fun_decl_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(fun_type, None);

    // No params, void return type

    let tokens: Vec<Token> = Scanner::new(":: -> Void").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_type) = fun_decl_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_type,
        Some(FunType {
            param_types: Vec::new(),
            return_type: ReturnType::Void,
        })
    );

    // Params and return type

    let tokens: Vec<Token> = Scanner::new(":: Int Bool -> Char").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_type) = fun_decl_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_type,
        Some(FunType {
            param_types: vec![Type::Int, Type::Bool],
            return_type: ReturnType::Type(Type::Char),
        })
    );

    // Complex types

    let tokens: Vec<Token> = Scanner::new(":: (a, [b]) [(Int,c)] -> ((a,b),c)").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_type) = fun_decl_type_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_type,
        Some(FunType {
            param_types: vec![
                Type::Tuple(
                    Box::new(Type::Generic(Id("a".to_string()))),
                    Box::new(Type::Array(Box::new(Type::Generic(Id("b".to_string()))))),
                ),
                Type::Array(Box::new(Type::Tuple(
                    Box::new(Type::Int),
                    Box::new(Type::Generic(Id("c".to_string()))),
                )),),
            ],
            return_type: ReturnType::Type(Type::Tuple(
                Box::new(Type::Tuple(
                    Box::new(Type::Generic(Id("a".to_string()))),
                    Box::new(Type::Generic(Id("b".to_string()))),
                )),
                Box::new(Type::Generic(Id("c".to_string()))),
            ))
        })
    );
}

#[test]
fn test_fun_call_in_return() {
    let tokens: Vec<Token> = Scanner::new("f (x) {return g (x);}").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = program_parser(tokens);
    println!("r1: {:?}", r1);

    let t1 = [];

    let expected1 = Program(vec![Decl::FunDecl(FunDecl {
        name: Id("f".to_string()),
        params: vec![Id("x".to_string())],
        fun_type: None,
        statements: vec![Statement::Return(Some(Expr::Atom(Atom::FunCall(
            FunCall::new(
                Id(String::from("g")),
                vec![Expr::Atom(Atom::Variable(Variable::new(
                    Id(String::from("x")),
                    vec![],
                )))],
            ),
        ))))],
    })]);

    assert_eq!(r1, Ok((Tokens::new(&t1), expected1)));
}

#[test]
fn test_variable_in_return() {
    let tokens: Vec<Token> = Scanner::new("f (x) {return g;}").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = program_parser(tokens);
    println!("r1: {:?}", r1);

    let t1 = [];

    let expected1 = Program(vec![Decl::FunDecl(FunDecl {
        name: Id("f".to_string()),
        params: vec![Id("x".to_string())],
        fun_type: None,
        statements: vec![Statement::Return(Some(Expr::Atom(Atom::Variable(
            Variable::new(Id(String::from("g")), vec![]),
        ))))],
    })]);

    assert_eq!(r1, Ok((Tokens::new(&t1), expected1)));
}

#[test]
fn test_fun_decl_parser() {
    // Body with only statements

    let tokens: Vec<Token> = Scanner::new("myFun(x, y) { return; }").collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_decl,
        FunDecl {
            name: Id("myFun".to_string()),
            params: vec![Id("x".to_string()), Id("y".to_string())],
            fun_type: None,
            statements: vec![Statement::Return(None)],
        }
    );

    // Body with variable declarations and statements

    const CODE: &str = r"someFunction(a, b) :: Int [Int] -> [Int] {
            Int someVar = 0;
            return;
        }";

    let tokens: Vec<Token> = Scanner::new(CODE).collect();
    let tokens = Tokens::new(&tokens);

    let (rest, fun_decl) = fun_decl_parser(tokens).unwrap();

    assert!(rest.is_empty());
    assert_eq!(
        fun_decl,
        FunDecl {
            name: Id("someFunction".to_string()),
            params: vec![Id("a".to_string()), Id("b".to_string())],
            fun_type: Some(FunType {
                param_types: vec![Type::Int, Type::Array(Box::new(Type::Int))],
                return_type: ReturnType::Type(Type::Array(Box::new(Type::Int))),
            }),
            statements: vec![
                Statement::VarDecl(VarDecl {
                    var_type: Some(Type::Int),
                    name: Id("someVar".to_string()),
                    value: Expr::Atom(Atom::IntLiteral(0u32.into())),
                }),
                Statement::Return(None),
            ],
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
        ("[Char]", Type::Array(Box::new(Type::Char))),
        ("a", Type::Generic(Id("a".to_string()))),
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
            Type::Array(Box::new(Type::Tuple(
                Box::new(Type::Generic(Id("a".to_string()))),
                Box::new(Type::Generic(Id("b".to_string()))),
            ))),
        ),
    ];

    for (code, expected) in types {
        let tokens: Vec<Token> = Scanner::new(code).collect();
        let tokens = Tokens::new(&tokens);

        let (rest, t) = type_parser(tokens).unwrap();

        assert!(rest.is_empty());
        assert_eq!(t, expected);
    }
}

#[test]
fn test_unary_expr_parser() {
    let tokens: Vec<Token> = Scanner::new(&"!True").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = unary_expr_parser(tokens);
    let t1 = Tokens::new(&[]);
    let expected1 = Expr::Not(Box::new(Expr::Atom(Atom::BoolLiteral(true))));
    assert_eq!(r1, Ok((t1, expected1)));

    let tokens: Vec<Token> = Scanner::new(&"-3").collect();
    let tokens = Tokens::new(&tokens);

    let r2 = unary_expr_parser(tokens);
    let t2 = Tokens::new(&[]);
    let expected2 = Expr::UnaryMinus(boxed_int_literal!(3));
    assert_eq!(r2, Ok((t2, expected2)));

    let tokens: Vec<Token> = Scanner::new(&"---4").collect();
    let tokens = Tokens::new(&tokens);

    let r2 = unary_expr_parser(tokens);
    let t2 = Tokens::new(&[]);
    let expected2 = Expr::UnaryMinus(Box::new(Expr::UnaryMinus(Box::new(Expr::UnaryMinus(
        boxed_int_literal!(4),
    )))));
    assert_eq!(r2, Ok((t2, expected2)));
}

#[test]
fn test_atom_expr_parser() {
    let tokens = [
        Token::new(TokenKind::Bool(true), 0, 0),
        Token::new(TokenKind::Identifier("x"), 0, 0),
    ];

    let tokens = Tokens::new(&tokens);

    let r1 = atom_expr_parser(tokens);
    let t1 = Tokens::new(&tokens[1..]);
    assert_eq!(r1, Ok((t1, Expr::Atom(Atom::BoolLiteral(true)))));

    let r2 = atom_expr_parser(t1);
    let t2 = Tokens::new(&tokens[2..]);
    let var = Variable::new(Id(String::from("x")), Vec::new());
    assert_eq!(r2, Ok((t2, Expr::Atom(Atom::Variable(var)))));
}

#[test]
fn test_fun_call_parser() {
    let tokens: Vec<Token> = Scanner::new(&"my_fun(123, x.hd, False)").collect();
    let tokens = Tokens::new(&tokens);

    let r1 = fun_call_parser(tokens);
    let t1 = Tokens::new(&[]);

    let expected1 = FunCall::new(
        Id(String::from("my_fun")),
        vec![
            Expr::Atom(Atom::IntLiteral(123u32.into())),
            Expr::Atom(Atom::Variable(Variable::new(
                Id(String::from("x")),
                vec![Field::Hd],
            ))),
            Expr::Atom(Atom::BoolLiteral(false)),
        ],
    );

    assert_eq!(r1, Ok((t1, expected1)));
}

#[test]
fn test_literal_parser() {
    let tokens = [
        Token::new(TokenKind::Bool(true), 0, 0),
        Token::new(TokenKind::Char('c'), 1, 0),
        Token::new(TokenKind::Integer(123u32.into()), 2, 0),
        Token::new(
            // No escape sequences, those are handled in the scanner already
            TokenKind::String(r#"Hello " world with \ problematic characters"#.to_string()),
            3,
            0,
        ),
    ];

    let tokens = Tokens::new(&tokens);

    let r1 = literal_atom_parser(tokens);
    let t1 = Tokens::new(&tokens[1..]);
    assert_eq!(r1, Ok((t1, Atom::BoolLiteral(true))));

    let r2 = literal_atom_parser(t1);
    let t2 = Tokens::new(&tokens[2..]);
    assert_eq!(r2, Ok((t2, Atom::CharLiteral('c'))));

    let r3 = literal_atom_parser(t2);
    let t3 = Tokens::new(&tokens[3..]);
    assert_eq!(r3, Ok((t3, Atom::IntLiteral(123u32.into()))));

    let r4 = literal_atom_parser(t3);
    let t4 = Tokens::new(&tokens[4..]);
    assert_eq!(
        r4,
        Ok((
            t4,
            Atom::StringLiteral(r#"Hello " world with \ problematic characters"#.to_string())
        ))
    );
}
