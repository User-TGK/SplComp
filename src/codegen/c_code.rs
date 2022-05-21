use crate::ast::*;

use indoc::indoc;
use pretty_trait::{block, delimited, Group, Indent, JoinExt, Newline, Pretty, Sep};

use std::collections::HashMap;

macro_rules! bin_expr (
    ($e1:expr, $e2:expr, $cast_to_intptr:expr, $parent_precedence:expr, $op:expr) => (
        Box::new(child_expr!($e1, $parent_precedence, $cast_to_intptr).join($op).join(child_expr!($e2, $parent_precedence, $cast_to_intptr)))
    )
);

macro_rules! child_expr (
    ($e:expr, $parent_precedence:expr, $cast_to_intptr:expr) => (
        if $e.should_be_paranthesized($parent_precedence) {
            Box::new(Group::new("(".join(($e.to_c($cast_to_intptr)).join(")"))))
        } else {
            $e.to_c($cast_to_intptr)
        }
    )
);

#[derive(Default)]
pub struct CompositeTypeEnv(HashMap<Type, String>);

#[derive(Default)]
pub struct TypePrefixGenerator {
    counter: usize,
}

impl TypePrefixGenerator {
    pub fn new_prefix(&mut self) -> String {
        let var = format!("T_{}_", self.counter);
        self.counter += 1;

        var
    }
}

pub trait ToC {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty>;
}

fn global_var_decls(var_decls: &Vec<VarDecl>, cast_to_intptr: bool) -> Box<dyn Pretty> {
    Box::new(
        delimited(
            &"".join(Sep(1)).join(Sep(1)),
            var_decls.iter().map(|v| {
                v.var_type
                    .as_ref()
                    .unwrap()
                    .to_c(cast_to_intptr)
                    .join(" ")
                    .join(v.name.to_c(cast_to_intptr))
                    .join(";")
            }),
        )
        .join(Newline)
        .join(Newline)
        .join("// Initialization function for global variables")
        .join(Newline)
        .join("void initialize() {")
        .join(Indent(Sep(0).join(delimited(
            &"".join(Sep(1)).join(Sep(1)),
            var_decls.iter().map(|v| {
                v.name
                    .to_c(cast_to_intptr)
                    .join(" = ")
                    .join(v.value.expr.to_c(cast_to_intptr))
                    .join(";")
            }),
        ))))
        .join(Newline)
        .join("}")
        .join(Newline),
    )
}

impl ToC for Program {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        let prelude = indoc! {"
            #include <assert.h>
            #include <stdint.h>
            #include <stdio.h>
            #include <stdbool.h>
            #include <stdlib.h>

            // Start of the prelude section

            struct tuple { 
                intptr_t fst;
                intptr_t snd;
            };

            struct tuple* tuple_new(intptr_t fst, intptr_t snd) {
                struct tuple* t = malloc(sizeof(struct tuple));
                assert(t != NULL);
            
                t->fst = fst;
                t->snd = snd;
            
                return t;
            }

            struct tuple* to_tuple_ptr(intptr_t ptr) {
                return (struct tuple*) ptr;
            }

            // End of the prelude section

        "};

        if self.var_decls.is_empty() {
            Box::new(prelude.join(delimited(
                &"".join(Sep(1)).join(Sep(1)),
                self.fun_decls.iter().map(|f| f.to_c(cast_to_intptr)),
            )))
        } else {
            Box::new(
                prelude.join("// Global variables").join(Newline).join(
                    global_var_decls(&self.var_decls, cast_to_intptr)
                        .join(Newline)
                        .join("// Function declarations")
                        .join(Newline)
                        .join(delimited(
                            &"".join(Sep(1)).join(Sep(1)),
                            self.fun_decls.iter().map(|f| f.to_c(cast_to_intptr)),
                        )),
                ),
            )
        }
    }
}

impl ToC for VarDecl {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(
            self.var_type
                .as_ref()
                .unwrap()
                .to_c(cast_to_intptr)
                .join(" ")
                .join(self.name.to_c(cast_to_intptr))
                .join(" = ")
                .join(self.value.expr.to_c(cast_to_intptr))
                .join(";"),
        )
    }
}

impl ToC for (&Type, &Id) {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(
            self.0
                .to_c(cast_to_intptr)
                .join(" ")
                .join(self.1.to_c(cast_to_intptr)),
        )
    }
}

impl ToC for FunDecl {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        if let Some(t) = &self.fun_type {
            if let Type::Function(arg_types, rt) = t {
                if self.var_decls.is_empty() {
                    return Box::new(
                        rt.to_c(cast_to_intptr)
                            .join(" ")
                            .join(self.name.to_c(cast_to_intptr))
                            .join("(")
                            .join(delimited(
                                &", ",
                                arg_types
                                    .iter()
                                    .zip(self.params.iter())
                                    .map(|p| p.to_c(cast_to_intptr)),
                            ))
                            .join(")")
                            .join(Newline)
                            .join("{")
                            .join(if self.name.0 == "main" {
                                Box::new("\tinitialize();\n\n\t")
                            } else {
                                Box::new("")
                            })
                            .join(Indent(Sep(0).join(delimited(
                                &Sep(1),
                                self.statements.iter().map(|s| s.to_c(cast_to_intptr)),
                            ))))
                            .join(Newline)
                            .join("}"),
                    );
                } else {
                    return Box::new(
                        rt.to_c(cast_to_intptr)
                            .join(" ")
                            .join(self.name.to_c(cast_to_intptr))
                            .join("(")
                            .join(delimited(
                                &", ",
                                arg_types
                                    .iter()
                                    .zip(self.params.iter())
                                    .map(|p| p.to_c(cast_to_intptr)),
                            ))
                            .join(")")
                            .join(Newline)
                            .join("{")
                            .join(Indent(
                                Sep(0)
                                    .join(if self.name.0 == "main" {
                                        Box::new("initialize();\n\n\t")
                                    } else {
                                        Box::new("")
                                    })
                                    .join(delimited(
                                        &Sep(1),
                                        self.var_decls.iter().map(|v| v.to_c(cast_to_intptr)),
                                    )),
                            ))
                            .join(Newline)
                            .join(Indent(Sep(0).join(delimited(
                                &Sep(1),
                                self.statements.iter().map(|s| s.to_c(cast_to_intptr)),
                            ))))
                            .join(Newline)
                            .join("}"),
                    );
                }
            }

            unreachable!()
        }

        unreachable!()
    }
}

impl ToC for Id {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(self.0.to_string())
    }
}

impl ToC for Type {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        match self {
            Type::Int => Box::new("int"),
            Type::Bool => Box::new("bool"),
            Type::Char => Box::new("char"),
            Type::Void => Box::new("void"),
            Type::Tuple(_, _) => Box::new("struct tuple*"),
            Type::List(_) => {
                unimplemented!()
            }
            // maybe intptr as well
            Type::Var(_id) => Box::new("void*"),

            Type::Function(_, _) => {
                unreachable!()
            }
        }
    }
}

impl ToC for Statement {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        match self {
            Statement::If(i) => i.to_c(cast_to_intptr),
            Statement::While(w) => w.to_c(cast_to_intptr),
            Statement::Assign(a) => a.to_c(cast_to_intptr),
            Statement::FunCall(f) => Box::new(f.to_c(cast_to_intptr).join(";")),
            Statement::Return(e) => match e {
                Some(e) => Box::new("return ".join(e.expr.to_c(cast_to_intptr)).join(";")),
                None => Box::new("return;"),
            },
        }
    }
}

fn to_c_else_case(false_body: &Vec<Statement>, cast_to_intptr: bool) -> Box<dyn Pretty> {
    if false_body.is_empty() {
        Box::new("".join(""))
    } else {
        Box::new(
            "".join(Sep(1))
                .join("else {")
                .join(block(delimited(
                    &"".join(Sep(1)),
                    false_body.iter().map(|s| s.to_c(cast_to_intptr)),
                )))
                .join("}"),
        )
    }
}

impl ToC for If {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(
            "if ("
                .join(self.cond.expr.to_c(cast_to_intptr))
                .join(") {")
                .join(
                    block(delimited(
                        &"".join(Sep(1)),
                        self.if_true.iter().map(|s| s.to_c(cast_to_intptr)),
                    ))
                    .join("}")
                    .join(to_c_else_case(&self.if_false, cast_to_intptr)),
                ),
        )
    }
}

impl ToC for While {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(
            "while (".join(
                self.cond
                    .expr
                    .to_c(cast_to_intptr)
                    .join(") {")
                    .join(block(delimited(
                        &"".join(Sep(1)),
                        self.body.iter().map(|p| p.to_c(cast_to_intptr)),
                    )))
                    .join("}"),
            ),
        )
    }
}

impl ToC for Assign {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        let lhs = if self.target.fields.is_empty() {
            self.target.to_c(cast_to_intptr)
        } else {
            variable_field_access(&self.target, false)
        };

        let new_cast_to_intptr = !self.target.fields.is_empty();

        Box::new(
            lhs.join(" = ")
                .join(self.value.expr.to_c(new_cast_to_intptr))
                .join(";"),
        )
    }
}

impl ToC for Expr {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        let precedence = self.precedence();

        match self {
            Expr::Or(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " || "),
            Expr::And(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " && "),
            Expr::Equals(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " == "),
            Expr::NotEquals(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " != "),
            Expr::Lt(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " < "),
            Expr::Le(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " <= "),
            Expr::Gt(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " > "),
            Expr::Ge(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " >= "),
            Expr::Add(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " + "),
            Expr::Sub(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " - "),
            Expr::Mul(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " * "),
            Expr::Div(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " / "),
            Expr::Mod(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " % "),
            Expr::Cons(e1, e2) => bin_expr!(e1, e2, cast_to_intptr, precedence, " : "),
            Expr::UnaryMinus(e) => Box::new(Group::new("-".join(child_expr!(
                e,
                precedence,
                cast_to_intptr
            )))),
            Expr::Not(e) => Box::new(Group::new("!".join(child_expr!(
                e,
                precedence,
                cast_to_intptr
            )))),
            Expr::Atom(a) => a.to_c(cast_to_intptr),
        }
    }
}

impl ToC for Atom {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        match self {
            Atom::IntLiteral(i) => Box::new(i.to_str_radix(10)),
            Atom::BoolLiteral(b) => Box::new(match b {
                true => String::from("1"),
                false => String::from("0"),
            }),
            Atom::CharLiteral(c) => Box::new(format!("'{}'", c.to_string())),
            Atom::StringLiteral(string) => {
                let escaped = string.replace("\\", "\\\\").replace("\"", "\\\"");
                Box::new(format!("\"{}\"", escaped))
            }
            Atom::FunCall(f) => f.to_c(cast_to_intptr),
            Atom::Variable(v) => v.to_c(cast_to_intptr),
            Atom::EmptyList => Box::new(String::from("NULL")),
            Atom::Tuple(e1, e2) => {
                let prefix = if cast_to_intptr {
                    "(intptr_t)tuple_new("
                } else {
                    "tuple_new("
                };
                Box::new(
                    prefix
                        .join(e1.to_c(true))
                        .join(", ")
                        .join(e2.to_c(true))
                        .join(")"),
                )
            }
        }
    }
}

impl ToC for FunCall {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        Box::new(Group::new(
            self.name.0.to_string().join("(").join(
                block(delimited(
                    &",".join(Sep(1)),
                    self.args.iter().map(|e| e.expr.to_c(cast_to_intptr)),
                ))
                .join(")"),
            ),
        ))
    }
}

fn variable_field_access(v: &Variable, with_lhs_cast: bool) -> Box<dyn Pretty> {
    let mut acc = v.name.0.to_string();
    let mut t = v.var_type.as_ref().unwrap();

    let mut field_it = v.fields.iter().peekable();

    while let Some(field) = field_it.next() {
        match field {
            Field::Fst => {
                if let Type::Tuple(t1, _) = &t {
                    t = t1;
                    acc = acc + "->fst";
                } else {
                    unreachable!()
                }
            }
            Field::Snd => {
                if let Type::Tuple(_, t2) = &t {
                    t = t2;
                    acc = acc + "->snd";
                } else {
                    unreachable!()
                }
            }

            Field::Hd => {
                unimplemented!()
            }

            Field::Tl => {
                unimplemented!()
            }
        }

        // This is to prevent assigning to e.g. to_tuple_ptr(t->snd), which is forbidden.
        // out.c:53:26: error: lvalue required as left operand of assignment
        // 53 |     to_tuple_ptr(t->snd) = tuple_new('c', 'd')
        if !with_lhs_cast && field_it.peek().is_none() {
            break;
        }

        match t {
            Type::Char => {
                if with_lhs_cast {
                    acc = "(char) ".to_string() + acc.as_str();
                }
            }
            Type::Int => {
                if with_lhs_cast {
                    acc = "(int) ".to_string() + acc.as_str();
                }
            }
            Type::Tuple(_, _) => {
                acc = "to_tuple_ptr(".to_string() + acc.as_str() + ")";
            }
            _ => unimplemented!("{:?} // {}", t, acc),
        }
    }
    Box::new(acc)
}

impl ToC for Variable {
    fn to_c(&self, cast_to_intptr: bool) -> Box<dyn Pretty> {
        if self.fields.is_empty() {
            Box::new(self.name.0.to_string())
        } else {
            variable_field_access(&self, true)
        }
    }
}
