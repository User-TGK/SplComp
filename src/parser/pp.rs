use super::ast::*;

use pretty_trait::{block, delimited, Group, JoinExt, Newline, Pretty, Sep};

macro_rules! bin_expr (
    ($e1:expr, $e2:expr, $parent_precedence:expr, $op:expr) => (
        Box::new(child_expr!($e1, $parent_precedence).join($op).join(child_expr!($e2, $parent_precedence)))
    )
);

macro_rules! child_expr (
    ($e:expr, $parent_precedence:expr) => (
        if should_be_paranthesized($e, $parent_precedence) {
            Box::new(Group::new("(".join(($e.to_pretty()).join(")"))))
        } else {
            $e.to_pretty()
        }
    )
);

fn precedence(expr: &Expr) -> i32 {
    match expr {
        Expr::Or(..) => 2,
        Expr::And(..) => 3,
        Expr::Equals(..)
        | Expr::NotEquals(..)
        | Expr::Lt(..)
        | Expr::Le(..)
        | Expr::Gt(..)
        | Expr::Ge(..) => 4,
        Expr::Cons(..) => 5,
        Expr::Add(..) | Expr::Sub(..) => 6,
        Expr::Mul(..) | Expr::Div(..) | Expr::Mod(..) => 7,
        Expr::UnaryMinus(..) | Expr::Not(..) => 8,
        Expr::Atom(..) => 9,
    }
}

fn should_be_paranthesized(expr: &Expr, parent_precedence: i32) -> bool {
    precedence(expr) < parent_precedence
}

pub trait PrettyPrintable {
    fn to_pretty(&self) -> Box<dyn Pretty>;
}

impl PrettyPrintable for Program {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(delimited(
            &"".join(Sep(1)).join(Sep(1)),
            self.0.iter().map(Decl::to_pretty),
        ))
    }
}

impl PrettyPrintable for Decl {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Decl::VarDecl(v) => v.to_pretty(),
            Decl::FunDecl(f) => f.to_pretty(),
        }
    }
}

impl PrettyPrintable for Option<Type> {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Some(t) => Box::new(t.to_pretty().join(" ")),
            None => Box::new("var "),
        }
    }
}

impl PrettyPrintable for VarDecl {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(
            self.var_type
                .to_pretty()
                .join(self.name.to_pretty())
                .join(" = ")
                .join(self.value.to_pretty())
                .join(";"),
        )
    }
}

impl PrettyPrintable for Option<FunType> {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Some(t) => Box::new(" :: ".join(t.to_pretty())),
            None => Box::new("".join("")),
        }
    }
}

impl PrettyPrintable for FunDecl {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(
            self.name.to_pretty().join("(").join(
                delimited(&", ", self.params.iter().map(Id::to_pretty))
                    .join(")")
                    .join(self.fun_type.to_pretty())
                    .join(Newline)
                    .join("{")
                    .join(block(delimited(
                        &"".join(Sep(0)),
                        self.statements.iter().map(Statement::to_pretty),
                    )))
                    .join("}"),
            ),
        )
    }
}

impl PrettyPrintable for FunType {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        let delim = if self.param_types.is_empty() { "" } else { " " };

        Box::new(
            delimited(&" ", self.param_types.iter().map(Type::to_pretty))
                .join(delim)
                .join("-> ")
                .join(self.return_type.to_pretty()),
        )
    }
}

impl PrettyPrintable for ReturnType {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            ReturnType::Type(t) => t.to_pretty(),
            ReturnType::Void => Box::new("Void"),
        }
    }
}

impl PrettyPrintable for Id {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(self.0.to_string())
    }
}

impl PrettyPrintable for Type {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Type::Int => Box::new("Int"),
            Type::Bool => Box::new("Bool"),
            Type::Char => Box::new("Char"),
            Type::String => Box::new("String"),
            Type::Tuple(t1, t2) => Box::new(
                "(".join(t1.to_pretty())
                    .join(",")
                    .join(t2.to_pretty())
                    .join(")"),
            ),
            Type::Array(t) => Box::new("[".join(t.to_pretty()).join("]")),
            Type::Generic(id) => Box::new(id.to_pretty()),
        }
    }
}

impl PrettyPrintable for Statement {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Statement::If(i) => i.to_pretty(),
            Statement::While(w) => w.to_pretty(),
            Statement::VarDecl(v) => v.to_pretty(),
            Statement::Assign(a) => a.to_pretty(),
            Statement::FunCall(f) => Box::new(f.to_pretty().join(";")),
            Statement::Return(e) => match e {
                Some(e) => Box::new("return ".join(e.to_pretty()).join(";")),
                None => Box::new("return;"),
            },
        }
    }
}

fn to_pretty_else_case(false_body: &Vec<Statement>) -> Box<dyn Pretty> {
    if false_body.is_empty() {
        Box::new("".join(""))
    } else {
        Box::new(
            "".join(Sep(1))
                .join("else {")
                .join(block(delimited(
                    &"".join(Sep(1)),
                    false_body.iter().map(Statement::to_pretty),
                )))
                .join("}"),
        )
    }
}

impl PrettyPrintable for If {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(
            "if (".join(self.cond.to_pretty()).join(") {").join(
                block(delimited(
                    &"".join(Sep(1)),
                    self.if_true.iter().map(Statement::to_pretty),
                ))
                .join("}")
                .join(to_pretty_else_case(&self.if_false)),
            ),
        )
    }
}

impl PrettyPrintable for While {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(
            "while (".join(
                self.cond
                    .to_pretty()
                    .join(") {")
                    .join(block(delimited(
                        &"".join(Sep(1)),
                        self.body.iter().map(Statement::to_pretty),
                    )))
                    .join("}"),
            ),
        )
    }
}

impl PrettyPrintable for Assign {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(
            self.target
                .to_pretty()
                .join(" = ")
                .join(self.value.to_pretty())
                .join(";"),
        )
    }
}

impl PrettyPrintable for Expr {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        let precedence = precedence(self);

        match self {
            Expr::Or(e1, e2) => bin_expr!(e1, e2, precedence, " || "),
            Expr::And(e1, e2) => bin_expr!(e1, e2, precedence, " && "),
            Expr::Equals(e1, e2) => bin_expr!(e1, e2, precedence, " == "),
            Expr::NotEquals(e1, e2) => bin_expr!(e1, e2, precedence, " != "),
            Expr::Lt(e1, e2) => bin_expr!(e1, e2, precedence, " < "),
            Expr::Le(e1, e2) => bin_expr!(e1, e2, precedence, " <= "),
            Expr::Gt(e1, e2) => bin_expr!(e1, e2, precedence, " > "),
            Expr::Ge(e1, e2) => bin_expr!(e1, e2, precedence, " >= "),
            Expr::Add(e1, e2) => bin_expr!(e1, e2, precedence, " + "),
            Expr::Sub(e1, e2) => bin_expr!(e1, e2, precedence, " - "),
            Expr::Mul(e1, e2) => bin_expr!(e1, e2, precedence, " * "),
            Expr::Div(e1, e2) => bin_expr!(e1, e2, precedence, " / "),
            Expr::Mod(e1, e2) => bin_expr!(e1, e2, precedence, " % "),
            Expr::Cons(e1, e2) => bin_expr!(e1, e2, precedence, " : "),
            Expr::UnaryMinus(e) => Box::new(Group::new("-".join(child_expr!(e, precedence)))),
            Expr::Not(e) => Box::new(Group::new("!".join(child_expr!(e, precedence)))),
            Expr::Atom(a) => a.to_pretty(),
        }
    }
}

impl PrettyPrintable for Atom {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        match self {
            Atom::IntLiteral(i) => Box::new(i.to_str_radix(10)),
            Atom::BoolLiteral(b) => Box::new(match b {
                true => String::from("True"),
                false => String::from("False"),
            }),
            Atom::CharLiteral(c) => Box::new(format!("'{}'", c.to_string())),
            Atom::StringLiteral(string) => {
                let escaped = string.replace("\\", "\\\\").replace("\"", "\\\"");
                Box::new(format!("\"{}\"", escaped))
            }
            Atom::FunCall(f) => f.to_pretty(),
            Atom::Variable(v) => v.to_pretty(),
            Atom::EmptyList => Box::new(String::from("[]")),
            Atom::Tuple(e1, e2) => Box::new(Group::new(
                "(".join((*e1).to_pretty().join(", ").join((*e2).to_pretty()))
                    .join(")"),
            )),
        }
    }
}

impl PrettyPrintable for FunCall {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(Group::new(
            self.name.0.to_string().join("(").join(
                block(delimited(
                    &",".join(Sep(1)),
                    self.args.iter().map(Expr::to_pretty),
                ))
                .join(")"),
            ),
        ))
    }
}

impl PrettyPrintable for Variable {
    fn to_pretty(&self) -> Box<dyn Pretty> {
        Box::new(self.name.0.to_string().join(self.fields.iter().fold(
            String::new(),
            |mut acc, field| {
                let field_op = match field {
                    Field::Hd => ".hd",
                    Field::Tl => ".tl",
                    Field::Fst => ".fst",
                    Field::Snd => ".snd",
                };

                acc.push_str(field_op);
                acc
            },
        )))
    }
}

#[cfg(test)]
mod test {
    use crate::parser::pp::PrettyPrintable;
    use crate::parser::*;
    use crate::scanner::Scanner;
    use crate::token::{Token, Tokens};

    use pretty_trait::to_string;

    use std::fs;

    fn pp_tester(filename: &str) {
        let input = fs::read_to_string(filename).unwrap();
        let scanner = Scanner::new(&input);
        let tokens: Vec<Token> = scanner.collect();
        let tokens = Tokens::new(&tokens);

        let (_, ast1) = program_parser(tokens).unwrap();
        let output = to_string(&ast1.to_pretty(), Some(40), 4);
        let scanner = Scanner::new(&output);
        let tokens: Vec<Token> = scanner.collect();
        let tokens = Tokens::new(&tokens);

        let (_, ast2) = program_parser(tokens).unwrap();

        assert_eq!(ast1, ast2);
    }

    #[test]
    fn test_pretty_print_2_d_example() {
        pp_tester("material/tests/2D.spl");
    }

    #[test]
    fn test_pretty_print_example_example() {
        pp_tester("material/tests/Example.spl");
    }

    #[test]
    fn test_pretty_print_unary_minus_example() {
        pp_tester("material/tests/unary_minus.spl");
    }

    #[test]
    fn test_pretty_print_sum_example() {
        pp_tester("material/tests/sum.spl");
    }

    #[test]
    fn test_pretty_print_whitespaces_example() {
        pp_tester("material/tests/whitespaces.spl");
    }

    #[test]
    fn test_pretty_print_a_bit_of_everything_example() {
        pp_tester("material/tests/a_bit_of_everything.spl");
    }
}
