mod ast;

use crate::token::*;
use ast::*;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::{fold_many0, many0, many_till, separated_list0};
use nom::sequence::{tuple, delimited};
use nom::{Err, IResult};

macro_rules! token_parser (
    ($name:ident, $kind:expr) => (
        fn $name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| {
                t[0].kind == $kind}
            )(tokens)
        }
    )
);

token_parser!(if_parser, TokenKind::If);
token_parser!(while_parser, TokenKind::While);

// Non-terminal Disjun
token_parser!(or_parser, TokenKind::Or);

// Non-terminal Conjun
token_parser!(and_parser, TokenKind::And);

// Non-terminal Compare
token_parser!(equals_parser, TokenKind::Equals);
token_parser!(not_equals_parser, TokenKind::NotEquals);
token_parser!(lt_parser, TokenKind::Lt);
token_parser!(le_parser, TokenKind::Le);
token_parser!(gt_parser, TokenKind::Gt);
token_parser!(ge_parser, TokenKind::Ge);

// Non-terminal Concat
token_parser!(cons_parser, TokenKind::Cons);

// Non-terminal Term
token_parser!(plus_parser, TokenKind::Plus);
token_parser!(minus_parser, TokenKind::Minus);

// Non-terminal Factor
token_parser!(times_parser, TokenKind::Times);
token_parser!(divide_parser, TokenKind::Divide);
token_parser!(modulo_parser, TokenKind::Modulo);

// Non-terminal Unary
token_parser!(not_parser, TokenKind::Not);

// Non-terminal Atom
token_parser!(opening_paren_parser, TokenKind::OpeningParen);
token_parser!(closing_paren_parser, TokenKind::ClosingParen);
token_parser!(empty_list_parser, TokenKind::EmptyList);
token_parser!(comma_parser, TokenKind::Comma);

// Non-terminal Field
token_parser!(hd_parser, TokenKind::Hd);
token_parser!(tl_parser, TokenKind::Tl);
token_parser!(fst_parser, TokenKind::Fst);
token_parser!(snd_parser, TokenKind::Snd);

fn expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    disjun_expr_parser(tokens)
}

fn disjun_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, (start, conjunctions)) = tuple((
        conjun_expr_parser,
        many0(tuple((or_parser, conjun_expr_parser))),
    ))(tokens)?;

    Ok((
        tail,
        conjunctions
            .into_iter()
            .fold(start, |expression, (_, conjun)| {
                Expr::Or(Box::new(expression), Box::new(conjun))
            }),
    ))
}

fn conjun_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, (start, conjunctions)) = tuple((
        compare_expr_parser,
        many0(tuple((and_parser, compare_expr_parser))),
    ))(tokens)?;

    Ok((
        tail,
        conjunctions
            .into_iter()
            .fold(start, |expression, (_, conjun)| {
                Expr::And(Box::new(expression), Box::new(conjun))
            }),
    ))
}

fn compare_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, mat) = concat_expr_parser(tokens)?;
    let (tail2, ops) = many0(tuple((
        alt((
            equals_parser,
            not_equals_parser,
            lt_parser,
            le_parser,
            gt_parser,
            ge_parser,
        )),
        concat_expr_parser,
    )))(tail)?;

    let mut term_expr = mat;

    for (op, right) in ops {
        let left = Box::new(term_expr);
        let right = Box::new(right);
        term_expr = match op[0].kind {
            TokenKind::Equals => Expr::Equals(left, right),
            TokenKind::NotEquals => Expr::NotEquals(left, right),
            TokenKind::Lt => Expr::Lt(left, right),
            TokenKind::Le => Expr::Le(left, right),
            TokenKind::Gt => Expr::Gt(left, right),
            TokenKind::Ge => Expr::Ge(left, right),
            _ => unreachable!(),
        };
    }

    Ok((tail2, term_expr))
}

fn concat_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, (ops, mut expr)) = tuple((
        many0(tuple((term_expr_parser, cons_parser))),
        term_expr_parser,
    ))(tokens)?;

    // Right associativity.
    for (left, _op) in ops.into_iter().rev() {
        expr = Expr::Cons(Box::new(left), Box::new(expr));
    }

    Ok((tail, expr))
}

fn term_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, mat) = factor_expr_parser(tokens)?;
    let (tail2, ops) = many0(tuple((
        alt((plus_parser, minus_parser)),
        factor_expr_parser,
    )))(tail)?;

    let mut term_expr = mat;

    for (op, right) in ops {
        let left = Box::new(term_expr);
        let right = Box::new(right);
        term_expr = match op[0].kind {
            TokenKind::Plus => Expr::Add(left, right),
            TokenKind::Minus => Expr::Sub(left, right),
            _ => unreachable!(),
        };
    }

    Ok((tail2, term_expr))
}

fn factor_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (tail, mat) = unary_expr_parser(tokens)?;
    let (tail2, ops) = many0(tuple((
        alt((times_parser, divide_parser, modulo_parser)),
        unary_expr_parser,
    )))(tail)?;

    let mut factor_expr = mat;

    for (op, right) in ops {
        let left = Box::new(factor_expr);
        let right = Box::new(right);
        factor_expr = match op[0].kind {
            TokenKind::Times => Expr::Mul(left, right),
            TokenKind::Divide => Expr::Div(left, right),
            TokenKind::Modulo => Expr::Mod(left, right),
            _ => unreachable!(),
        };
    }

    Ok((tail2, factor_expr))
}

fn unary_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    map(
        many_till(alt((minus_parser, not_parser)), atom_expr_parser),
        |(unary_symbols, atom)| {
            let mut expr = atom;

            // Reverse iteration, start with the atom and wrap inside unary
            // expression types for each unary symbol from right to left.
            for s in unary_symbols.iter().rev() {
                expr = match s[0].kind {
                    TokenKind::Not => Expr::Not(Box::new(expr)),
                    TokenKind::Minus => Expr::UnaryMinus(Box::new(expr)),
                    _ => unreachable!(),
                }
            }

            expr
        },
    )(tokens)
}

fn atom_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    alt((
        map(
            alt((
                // id [Field]
                variable_atom_parser,
                // int | char | 'True' | 'False'
                literal_atom_parser,
                // FunCall
                fun_call_parser,
                // '[]'
                map(empty_list_parser, |_| Atom::EmptyList),
                // '(' Exp ',' Exp ')'
                tuple_atom_parser,
            )),
            |a| Expr::Atom(a),
        ),
        // '(' expr ')'
        delimited(
            opening_paren_parser,
            expr_parser,
            closing_paren_parser,
        ),
    ))(tokens)
}

fn identifier_parser(tokens: Tokens) -> IResult<Tokens, Id> {
    let (tail, mat) = take(1usize)(tokens)?;

    match mat[0].kind {
        TokenKind::Identifier(i) => Ok((tail, Id(i.to_string()))),
        _ => Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
    }
}

fn variable_atom_parser(tokens: Tokens) -> IResult<Tokens, Atom> {
    map(tuple((identifier_parser, field_parser)), |(id, fields)| {
        Atom::Variable(Variable::new(id, fields))
    })(tokens)
}

fn field_parser(tokens: Tokens) -> IResult<Tokens, Vec<Field>> {
    fold_many0(
        alt((hd_parser, tl_parser, fst_parser, snd_parser)),
        Vec::new,
        |mut acc: Vec<_>, t| {
            let f = match t[0].kind {
                TokenKind::Hd => Field::Hd,
                TokenKind::Tl => Field::Tl,
                TokenKind::Fst => Field::Fst,
                TokenKind::Snd => Field::Snd,
                _ => unreachable!(),
            };

            acc.push(f);
            acc
        },
    )(tokens)
}

fn literal_atom_parser(tokens: Tokens) -> IResult<Tokens, Atom> {
    let (tail, mat) = take(1usize)(tokens)?;

    match mat[0].kind {
        TokenKind::Bool(b) => Ok((tail, Atom::BoolLiteral(b))),
        TokenKind::Char(c) => Ok((tail, Atom::CharLiteral(c))),
        TokenKind::Integer(i) => Ok((tail, Atom::IntLiteral(i))),
        _ => Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
    }
}

fn fun_call_parser(tokens: Tokens) -> IResult<Tokens, Atom> {
    map(
        tuple((
            identifier_parser,
            opening_paren_parser,
            separated_list0(comma_parser, expr_parser),
            closing_paren_parser,
        )),
        |(id, _, args, _)| Atom::FunCall(FunCall::new(id, args)),
    )(tokens)
}

fn tuple_atom_parser(tokens: Tokens) -> IResult<Tokens, Atom> {
    map(
        tuple((
            opening_paren_parser,
            expr_parser,
            comma_parser,
            expr_parser,
            closing_paren_parser,
        )),
        |(_, e1, _, e2, _)| Atom::Tuple(Box::new(e1), Box::new(e2)),
    )(tokens)
}

#[cfg(test)]
mod test {
    use crate::scanner::*;

    use super::*;

    macro_rules! boxed_int_literal (
        ($value:expr) => (
            Box::new(Expr::Atom(Atom::IntLiteral($value)))
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
        let expected2 = Expr::UnaryMinus(Box::new(
            Expr::UnaryMinus(Box::new(
                Expr::UnaryMinus(boxed_int_literal!(4)),
            ))
        ));
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
    fn test_literal_parser() {
        let tokens = [
            Token::new(TokenKind::Bool(true), 0, 0),
            Token::new(TokenKind::Char('c'), 1, 0),
            Token::new(TokenKind::Integer(123), 2, 0),
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
        assert_eq!(r3, Ok((t3, Atom::IntLiteral(123))));
    }

    #[test]
    fn test_func_call_parser() {
        let tokens: Vec<Token> = Scanner::new(&"my_fun(123, x.hd, False)").collect();
        let tokens = Tokens::new(&tokens);

        let r1 = fun_call_parser(tokens);
        let t1 = Tokens::new(&[]);

        let expected1 = Atom::FunCall(FunCall::new(
            Id(String::from("my_fun")),
            vec![
                Expr::Atom(Atom::IntLiteral(123)),
                Expr::Atom(Atom::Variable(Variable::new(
                    Id(String::from("x")),
                    vec![Field::Hd],
                ))),
                Expr::Atom(Atom::BoolLiteral(false)),
            ],
        ));

        assert_eq!(r1, Ok((t1, expected1)));
    }
}
