#[cfg(test)]
mod test;

use crate::ast::*;
use crate::token::*;

use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::{fold_many0, many0, many1, many_till, separated_list0};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
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

pub fn program_parser(tokens: Tokens) -> IResult<Tokens, Program> {
    map(
        many1(alt((
            map(var_decl_parser, Decl::VarDecl),
            map(fun_decl_parser, Decl::FunDecl),
        ))),
        Program,
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
        |(t1, t2)| Type::Tuple(Box::new(t1), Box::new(t2)),
    )(tokens)
}

fn function_type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    map(
        tuple((
            double_colon_parser,
            many0(type_parser),
            right_arrow_parser,
            alt((type_parser, map(void_type_parser, |_| Type::Void))),
        )),
        |(_, param_types, _, return_type)| Type::Function(param_types, Box::new(return_type)),
    )(tokens)
}

/// Parses an array type, ie. "[" type "]".
fn array_type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    map(
        delimited(opening_square_parser, type_parser, closing_square_parser),
        |t| Type::List(Box::new(t)),
    )(tokens)
}

/// Parses a type.
fn type_parser(tokens: Tokens) -> IResult<Tokens, Type> {
    alt((
        map(int_type_parser, |_| Type::Int),
        map(bool_type_parser, |_| Type::Bool),
        map(char_type_parser, |_| Type::Char),
        tuple_type_parser,
        function_type_parser,
        array_type_parser,
        map(identifier_parser, Type::Var),
    ))(tokens)
}

/// Parses the type of a variable declaration, either "var" or a type.
fn var_decl_type_parser(tokens: Tokens) -> IResult<Tokens, Option<Type>> {
    alt((map(var_parser, |_| None), map(type_parser, Some)))(tokens)
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
        },
    )(tokens)
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
            opt(type_parser),
            tuple((
                opening_brace_parser,
                many0(var_decl_parser),
                many1(statement_parser),
                closing_brace_parser,
            )),
        )),
        |(name, params, fun_type, (_, var_decls, statements, _))| FunDecl {
            name,
            params,
            fun_type,
            var_decls,
            statements,
        },
    )(tokens)
}

fn if_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        tuple((
            if_parser,
            delimited(opening_paren_parser, expr_parser, closing_paren_parser),
            delimited(
                opening_brace_parser,
                many0(statement_parser),
                closing_brace_parser,
            ),
            opt(preceded(
                else_parser,
                delimited(
                    opening_brace_parser,
                    many0(statement_parser),
                    closing_brace_parser,
                ),
            )),
        )),
        |(_, cond, if_true, if_false)| {
            Statement::If(If {
                cond,
                if_true,
                if_false: if_false.unwrap_or_else(Vec::new),
            })
        },
    )(tokens)
}

fn while_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        preceded(
            while_parser,
            pair(
                delimited(opening_paren_parser, expr_parser, closing_paren_parser),
                delimited(
                    opening_brace_parser,
                    many0(statement_parser),
                    closing_brace_parser,
                ),
            ),
        ),
        |(cond, body)| Statement::While(While { cond, body }),
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
        |((id, fields), _, value, _)| {
            Statement::Assign(Assign {
                target: Variable::new(id, fields),
                value,
            })
        },
    )(tokens)
}

fn fun_call_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        terminated(fun_call_parser, semicolon_parser),
        Statement::FunCall,
    )(tokens)
}

fn return_statement_parser(tokens: Tokens) -> IResult<Tokens, Statement> {
    map(
        delimited(return_parser, opt(expr_parser), semicolon_parser),
        Statement::Return,
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
    let (rest, start) = conjun_expr_parser(tokens)?;

    fold_many0(
        preceded(or_parser, conjun_expr_parser),
        move || start.clone(),
        |acc, rhs| Expr::Or(Box::new(acc), Box::new(rhs)),
    )(rest)
}

fn conjun_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, start) = compare_expr_parser(tokens)?;

    fold_many0(
        preceded(and_parser, compare_expr_parser),
        move || start.clone(),
        |acc, rhs| Expr::And(Box::new(acc), Box::new(rhs)),
    )(rest)
}

fn compare_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, start) = concat_expr_parser(tokens)?;

    fold_many0(
        pair(
            alt((
                equals_parser,
                not_equals_parser,
                lt_parser,
                le_parser,
                gt_parser,
                ge_parser,
            )),
            concat_expr_parser,
        ),
        move || start.clone(),
        |acc, (op, rhs)| {
            let acc = Box::new(acc);
            let rhs = Box::new(rhs);
            match op[0].kind {
                TokenKind::Equals => Expr::Equals(acc, rhs),
                TokenKind::NotEquals => Expr::NotEquals(acc, rhs),
                TokenKind::Lt => Expr::Lt(acc, rhs),
                TokenKind::Le => Expr::Le(acc, rhs),
                TokenKind::Gt => Expr::Gt(acc, rhs),
                TokenKind::Ge => Expr::Ge(acc, rhs),
                _ => unreachable!(),
            }
        },
    )(rest)
}

fn concat_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, last) = term_expr_parser(tokens)?;
    let (tail, pairs) = many0(tuple((cons_parser, term_expr_parser)))(rest)?;

    if pairs.is_empty() {
        Ok((rest, last))
    } else {
        // Right associativity.
        let mut expr = pairs.last().unwrap().1.clone();

        for (_, e) in pairs.into_iter().rev().skip(1) {
            expr = Expr::Cons(Box::new(e), Box::new(expr));
        }

        expr = Expr::Cons(Box::new(last), Box::new(expr));

        Ok((tail, expr))
    }
}

fn term_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, start) = factor_expr_parser(tokens)?;

    fold_many0(
        pair(alt((plus_parser, minus_parser)), factor_expr_parser),
        move || start.clone(),
        |acc, (op, rhs)| {
            let acc = Box::new(acc);
            let rhs = Box::new(rhs);
            match op[0].kind {
                TokenKind::Plus => Expr::Add(acc, rhs),
                TokenKind::Minus => Expr::Sub(acc, rhs),
                _ => unreachable!(),
            }
        },
    )(rest)
}

fn factor_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, start) = unary_expr_parser(tokens)?;

    fold_many0(
        pair(
            alt((times_parser, divide_parser, modulo_parser)),
            unary_expr_parser,
        ),
        move || start.clone(),
        |acc, (op, rhs)| {
            let acc = Box::new(acc);
            let rhs = Box::new(rhs);
            match op[0].kind {
                TokenKind::Times => Expr::Mul(acc, rhs),
                TokenKind::Divide => Expr::Div(acc, rhs),
                TokenKind::Modulo => Expr::Mod(acc, rhs),
                _ => unreachable!(),
            }
        },
    )(rest)
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
        // '(' Expr ')'
        // '(' Expr ',' Expr ')'
        tuple_parenthesized_expr_parser,
        // FunCall
        map(fun_call_parser, |f| Expr::Atom(Atom::FunCall(f))),
        // id [Field]
        map(variable_atom_parser, Expr::Atom),
        // int | char | 'True' | 'False'
        map(literal_atom_parser, Expr::Atom),
        // '[]'
        map(empty_list_parser, |_| Expr::Atom(Atom::EmptyList)),
    ))(tokens)
}

fn tuple_parenthesized_expr_parser(tokens: Tokens) -> IResult<Tokens, Expr> {
    let (rest, expr) = preceded(opening_paren_parser, expr_parser)(tokens)?;

    let res = alt((
        map(closing_paren_parser, |_| expr.clone()),
        map(
            delimited(comma_parser, expr_parser, closing_paren_parser),
            |expr2| Expr::Atom(Atom::Tuple(Box::new(expr.clone()), Box::new(expr2))),
        ),
    ))(rest);

    res
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

    let atom = match mat[0].kind {
        TokenKind::Bool(b) => Atom::BoolLiteral(b),
        TokenKind::Char(c) => Atom::CharLiteral(c),
        TokenKind::Integer(ref i) => Atom::IntLiteral(i.clone()),
        TokenKind::String(ref string) => Atom::StringLiteral(string.clone()),
        _ => return Err(Err::Error(Error::new(tokens, ErrorKind::Tag))),
    };

    Ok((tail, atom))
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
