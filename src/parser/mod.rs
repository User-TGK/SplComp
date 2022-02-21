mod ast;
#[cfg(test)]
mod test;

use crate::token::*;
use ast::*;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, verify, opt};
use nom::error::{Error, ErrorKind};
use nom::multi::{fold_many0, many0, many1, many_till, separated_list0};
use nom::sequence::{tuple, delimited, pair, separated_pair, preceded, terminated};
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

token_parser!(var_parser, TokenKind::Var);
token_parser!(assignment_parser, TokenKind::Assignment);
token_parser!(semicolon_parser, TokenKind::Semicolon);
token_parser!(opening_brace_parser, TokenKind::OpeningBrace);
token_parser!(closing_brace_parser, TokenKind::ClosingBrace);

token_parser!(double_colon_parser, TokenKind::DoubleColon);
token_parser!(right_arrow_parser, TokenKind::RightArrow);

token_parser!(int_type_parser, TokenKind::IntType);
token_parser!(bool_type_parser, TokenKind::BoolType);
token_parser!(char_type_parser, TokenKind::CharType);

token_parser!(void_type_parser, TokenKind::VoidType);

token_parser!(opening_square_parser, TokenKind::OpeningSquare);
token_parser!(closing_square_parser, TokenKind::ClosingSquare);

token_parser!(if_parser, TokenKind::If);
token_parser!(else_parser, TokenKind::Else);
token_parser!(while_parser, TokenKind::While);
token_parser!(return_parser, TokenKind::Return);

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

fn program_parser(tokens: Tokens) -> IResult<Tokens, Program> {
    map(
        many1(alt((
            map(var_decl_parser, Decl::VarDecl),
            map(fun_decl_parser, Decl::FunDecl),
        ))),
        Program
    )(tokens)
}

/// Parses a tuple type, ie. "(" type "," type ")".
fn tuple_type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    map(
        delimited(
            opening_paren_parser,
            separated_pair(type_parser, comma_parser, type_parser),
            closing_paren_parser,
        ),
        |(t1, t2)| Type::Tuple(Box::new(t1), Box::new(t2))
    )(tokens)
}

/// Parses an array type, ie. "[" type "]".
fn array_type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    map(
        delimited(opening_square_parser, type_parser, closing_square_parser),
        |t| Type::Array(Box::new(t))
    )(tokens)
}

/// Parses a type.
fn type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    alt((
        map(int_type_parser, |_| Type::Int),
        map(bool_type_parser, |_| Type::Bool),
        map(char_type_parser, |_| Type::Char),
        tuple_type_parser,
        array_type_parser,
        map(identifier_parser, Type::Generic),
    ))(tokens)
}

/// Parses the type of a variable declaration, either "var" or a type.
fn var_decl_type_parser(tokens: Tokens) -> IResult<Tokens, Option<Type>> {
    alt((
        map(var_parser, |_| None),
        map(type_parser, Some),
    ))(tokens)
}

/// Parses a variable declaration.
fn var_decl_parser(tokens: Tokens) -> IResult<Tokens, VarDecl> {
    map(
        tuple((
            var_decl_type_parser,
            identifier_parser,
            assignment_parser,
            expr_parser,
            semicolon_parser,
        )),
        |(var_type, name, _, value, _)| VarDecl {
            var_type,
            name,
            value,
        }
    )(tokens)
}

fn fun_ret_type_parser(tokens: Tokens) -> IResult<Tokens, ReturnType> {
    alt((
        map(void_type_parser, |_| ReturnType::Void),
        map(type_parser, ReturnType::Type)
    ))(tokens)
}

fn fun_decl_type_parser(tokens: Tokens) -> IResult<Tokens, Option<FunType>> {
    opt(map(
        tuple((
            double_colon_parser,
            many0(type_parser),
            right_arrow_parser,
            fun_ret_type_parser,
        )),
        |(_, param_types, _, return_type)| FunType {
            param_types,
            return_type,
        }
    ))(tokens)
}

/// Parses a function declaration.
fn fun_decl_parser(tokens: Tokens) -> IResult<Tokens, FunDecl> {
    map(
        tuple((
            identifier_parser,
            delimited(
                opening_paren_parser,
                separated_list0(comma_parser, identifier_parser),
                closing_paren_parser,
            ),
            fun_decl_type_parser,
            delimited(
                opening_brace_parser,
                pair(
                    many0(var_decl_parser),
                    many1(statement_parser),
                ),
                closing_brace_parser,
            ),
        )),
        |(name, params, fun_type, (var_decls, statements))| FunDecl {
            name,
            params,
            fun_type,
            var_decls,
            statements,
        }
    )(tokens)
}

fn if_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            if_parser,
            delimited(opening_paren_parser, expr_parser, closing_paren_parser),
            delimited(opening_brace_parser, many0(statement_parser), closing_brace_parser),
            opt(preceded(
                else_parser,
                delimited(opening_brace_parser, many0(statement_parser), closing_brace_parser)
            )),
        )),
        |(_, cond, if_true, if_false)| Statement::If(If {
            cond,
            if_true,
            if_false: if_false.unwrap_or_else(Vec::new),
        })
    )(tokens)
}

fn while_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        preceded(
            while_parser,
            pair(
                delimited(opening_paren_parser, expr_parser, closing_paren_parser),
                delimited(opening_brace_parser, many0(statement_parser), closing_brace_parser),
            )
        ),
        |(cond, body)| Statement::While(While {
            cond,
            body,
        })
    )(tokens)
}

fn assign_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            pair(identifier_parser, field_parser),
            assignment_parser,
            expr_parser,
            semicolon_parser,
        )),
        |((id, fields), _, value, _)| Statement::Assign(Assign {
            target: Variable::new(id, fields),
            value,
        })
    )(tokens)
}

fn fun_call_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        terminated(fun_call_parser, semicolon_parser),
        Statement::FunCall
    )(tokens)
}

fn return_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        delimited(return_parser, opt(expr_parser), semicolon_parser),
        Statement::Return
    )(tokens)
}

fn statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    alt((
        if_statement_parser,
        while_statement_parser,
        assign_statement_parser,
        return_statement_parser,
        fun_call_statement_parser,
    ))(tokens)
}

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
                // FunCall
                map(fun_call_parser, Atom::FunCall),
                // id [Field]
                variable_atom_parser,
                // int | char | 'True' | 'False'
                literal_atom_parser,
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

fn fun_call_parser(tokens: Tokens) -> IResult<Tokens, FunCall> {
    map(
        tuple((
            identifier_parser,
            opening_paren_parser,
            separated_list0(comma_parser, expr_parser),
            closing_paren_parser,
        )),
        |(id, _, args, _)| FunCall::new(id, args),
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
