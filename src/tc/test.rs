use super::*;

use crate::parser::*;

fn typing_error_test_helper(input: &str, err: &str) {
    let tokens: Vec<Token> = Scanner::new(input).unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, mut program) = program_parser(tokens).unwrap();
    assert!(rest.is_empty());

    assert_eq!(Err(err.to_string()), run(&mut program));
}

fn typing_success_test_helper(input: &str, expected_output: &str) {
    let tokens: Vec<Token> = Scanner::new(input).unwrap().collect();
    let tokens = Tokens::new(&tokens, "");

    let (rest, mut program) = program_parser(tokens).unwrap();
    assert!(rest.is_empty());

    assert_eq!(Ok(()), run(&mut program));

    let ttokens: Vec<Token> = Scanner::new(expected_output).unwrap().collect();
    let ttokens = Tokens::new(&ttokens, "");

    let (trest, mut typed_program) = program_parser(ttokens).unwrap();
    assert!(trest.is_empty());

    assert_eq!(Ok(()), run(&mut typed_program));

    assert_eq!(program, typed_program)
}

#[test]
fn test_int_comparison_inference() {
    const PROGRAM: &str = r"
        main() {
            var ge = 123 >= 456;
            var le = 123 <= 456;
            var gt = 123 > 456;
            var lt = 123 < 456;

            return;
        }
        ";

    const TYPED_PROGRAM: &str = r"
        main() :: -> Void {
            Bool ge = 123 >= 456;
            Bool le = 123 <= 456;
            Bool gt = 123 > 456;
            Bool lt = 123 < 456;

            return;
        }
    ";

    typing_success_test_helper(PROGRAM, TYPED_PROGRAM);
}

#[test]
fn test_bool_ops() {
    const PROGRAM: &str = r"
        main() {
            var and = True && True;
            var or = False || True;
            var not = !False;

            return;
        }
        ";

    const TYPED_PROGRAM: &str = r"
    main() :: -> Void {
        Bool and = True && True;
        Bool or = False || True;
        Bool not = !False;

        return;
    }
    ";

    typing_success_test_helper(PROGRAM, TYPED_PROGRAM);
}

#[test]
fn test_tuple_inference() {
    const PROGRAM: &str = r"
        main() {
            var bool_tuple = (False, True);
            var int_tuple = (123, 456);
            var tuple_tuple = ((1, 2), (4, 5));

            return;
        }
        ";

    const TYPED_PROGRAM: &str = r"
        main() :: -> Void {
            (Bool, Bool) bool_tuple = (False, True);
            (Int, Int) int_tuple = (123, 456);
            ((Int, Int), (Int, Int)) tuple_tuple = ((1, 2), (4, 5));

            return;
        }
        ";

    typing_success_test_helper(PROGRAM, TYPED_PROGRAM);
}

#[test]
fn test_list_not_generalized() {
    const PROGRAM: &str = r"
        main() :: -> Void {
            var intlist = [];
            var intlist2 = 3 : intlist;

            // If the types were generalized, this would be accepted.
            var intlist3 = True : intlist2;

            return;
        }
        ";

    typing_error_test_helper(PROGRAM, "Unification error: Bool and Int");
}

#[test]
fn test_overloaded_ops() {
    const PROGRAM: &str = r"
        main() {
            var char_eq = 'a' == 'b';
            var int_eq = 1 == 1;

            return;
        }
        ";

    const TYPED_PROGRAM: &str = r"
        main() :: -> Void {
            Bool char_eq = 'a' == 'b';
            Bool int_eq = 1 == 1;

            return;
        }
    ";

    typing_success_test_helper(PROGRAM, TYPED_PROGRAM);
}

#[test]
fn test_infer_identity() {
    const PROGRAM: &str = r"
            id(a) :: a -> b {
                return a;
            }
            
            main() {
                var i = id(1);
                var b = id(True);
            
                return;
            }
        ";

    const TYPED_PROGRAM: &str = r"
            id(a) :: t10 -> t10 {
                return a;
            }

            main() :: -> Void {
                Int i = id(1);
                Bool b = id(True);
                return;
            }
        ";

    typing_success_test_helper(PROGRAM, TYPED_PROGRAM);
}

#[test]
fn test_void_expr_from_fun_call() {
    const PROGRAM0: &str = r"
        fail() {
            var x = print(1) == print(2);

            return;
        }

        main() {
            return;
        }
    ";

    typing_error_test_helper(
        PROGRAM0,
        "Function call print in expression would result in void type.",
    );

    const PROGRAM1: &str = r"
        voidFun() :: -> Void {
            return;
        }

        fail() {
            var v = voidFun();

            return v;
        }

        main() {
            return;
        }
    ";

    typing_error_test_helper(
        PROGRAM1,
        "Function call voidFun in expression would result in void type.",
    );
}

#[test]
fn test_missing_return_if() {
    const PROGRAM0: &str = r"
        id(a) {
            if (a) {

            } else {

            }
        }

        main() {
            return;
        }
    ";

    const TYPED_PROGRAM0: &str = r"
        id(a) :: Bool -> Void {
            if (a) {

            } else {

            }
            return;
        }

        main() {
            return;
        }
    ";

    typing_success_test_helper(PROGRAM0, TYPED_PROGRAM0);

    const PROGRAM1: &str = r"
        id(a) {
            if (a) {
                return;
            } else {

            }
        }

        main() {
            return;
        }
    ";

    typing_error_test_helper(PROGRAM1, "Function id has a missing return");

    const PROGRAM2: &str = r"
        id(a) {
            if (a) {

            } else {
                return;
            }
        }

        main() {
            return;
        }
    ";

    typing_error_test_helper(PROGRAM2, "Function id has a missing return");

    const PROGRAM3: &str = r"
        id(a) {
            if (a) {
                return;
            } else {
                return;
            }
        }

        main() {
            return;
        }
    ";

    const TYPED_PROGRAM3: &str = r"
        id(a) :: Bool -> Void {
            if (a) {
                return;
            } else {
                return;
            }
        }

        main() :: -> Void {
            return;
        }
    ";

    typing_success_test_helper(PROGRAM3, TYPED_PROGRAM3);
}
