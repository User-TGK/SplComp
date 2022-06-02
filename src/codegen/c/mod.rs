mod builtin;

use crate::ast::*;

use builtin::*;

use indoc::indoc;
use pretty_trait::{block, delimited, Group, Indent, Join, JoinExt, Newline, Pretty, Sep};

use std::ops::Deref;

macro_rules! bin_expr (
    ($e1:expr, $e2:expr, $env:expr, $parent_precedence:expr, $op:expr) => (
        Ok(Box::new(child_expr!($e1, $parent_precedence, $env)
                    .join($op).join(child_expr!($e2, $parent_precedence, $env))))
    )
);

macro_rules! child_expr (
    ($e:expr, $parent_precedence:expr, $env:expr) => (
        if $e.should_be_paranthesized($parent_precedence) {
            Box::new("(".join($e.to_c($env)?).join(")"))
        } else {
            Box::new("".join($e.to_c($env)?).join(""))
        }
    )
);

macro_rules! error_iter(
    ($vec:expr, $env:expr) => (
        $vec
            .map(|item| item.to_c($env))
            .collect::<Result<Vec<Box<dyn Pretty>>, String>>()?
    )
);

#[derive(Clone, Debug)]
pub struct CEnv {
    /// Whether the main function needs to initialize the globals before executing
    /// the rest of the program. Required if globals aren't empty.
    initialize: bool,

    /// Whether specific values need to be casted to an intptr_t in the current call.
    /// Used by the composite type tuple, either on assignment (lhs) or expressions (rhs).
    cast_to_intptr: bool,
}

impl CEnv {
    pub fn with_initialize(mut self, value: bool) -> Self {
        self.initialize = value;

        self
    }

    pub fn with_cast_to_intptr(mut self, value: bool) -> Self {
        self.cast_to_intptr = value;

        self
    }
}

impl Default for CEnv {
    fn default() -> Self {
        Self {
            initialize: true,
            cast_to_intptr: false,
        }
    }
}

pub trait ToC {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String>;
}

// Instance for global variable declarations without initialization.
impl ToC for (&Type, &String) {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            self.0.to_c(env)?.join(" ").join(self.1.clone()).join(";"),
        ))
    }
}

// Instance for initializing (assigning to) a global variable in the initialization function.
impl ToC for (&Id, &TypedExpr) {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            self.0
                .to_c(env)?
                .join(" = ")
                .join(self.1.to_c(env)?)
                .join(";"),
        ))
    }
}

fn global_var_decls(var_decls: &Vec<VarDecl>, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
    Ok(Box::new(
        delimited(
            &"".join(Sep(1)),
            error_iter!(
                var_decls
                    .iter()
                    .map(|v| (v.var_type.as_ref().unwrap(), &v.name.0)),
                env
            ),
        )
        .join(Newline)
        .join(Newline)
        .join("// Initialization function for global variables")
        .join(Newline)
        .join("void initialize() {")
        .join(Indent(Sep(0).join(delimited(
            &"".join(Sep(1)),
            error_iter!(var_decls.iter().map(|v| (&v.name, &v.value)), env),
        ))))
        .join(Newline)
        .join("}")
        .join(Newline),
    ))
}

impl ToC for Program {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
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

            struct node {
                intptr_t data;
                struct node* tail;
            };

            struct tuple* tuple_new(intptr_t fst, intptr_t snd) {
                struct tuple* t = malloc(sizeof(struct tuple));
                assert(t != NULL);
            
                t->fst = fst;
                t->snd = snd;
            
                return t;
            }

            struct node* list_node_new(intptr_t data, struct node* tail) {
                struct node* l = malloc(sizeof(struct node));
                assert(l != NULL);

                l->data = data;
                l-> tail = tail;

                return l;
            }

            struct tuple* to_tuple_ptr(intptr_t ptr) {
                return (struct tuple*) ptr;
            }

            struct node* to_list_ptr(intptr_t ptr) {
                return (struct node*) ptr;
            }

            bool isEmpty(struct node* list) {
                return list == NULL;
            }

            typedef enum {Int, Char, Bool, Tuple, List} Type;

            // Print function that prints a value based on its type.
            // Returns a pointer to the types list remainder (which still needs to be printed)
            struct node* print(intptr_t value, struct node* types) {
                struct node* current_type = types;
                
                if (current_type->data == Int) {
                    printf(\"%d\", (int) value);
                } else if (current_type->data == Char) {
                    printf(\"%c\", (char) value);
                } else if (current_type->data == Bool) {
                    printf(\"%i\", (bool) value);
                } else if (current_type->data == List) {
                    struct node* hd = (struct node*) value;
                    current_type = current_type->tail;
            
                    struct node* processed_types = current_type->tail;
            
                    printf(\"%c\", '[');
            
                    while (hd != NULL) {
                        processed_types = print(hd->data, current_type);
                        hd = hd->tail;
                        
                        if (hd != NULL) {
                            printf(\"%c\", ',');
                        }
                    }
                    printf(\"%c\", ']');
            
                    return processed_types;
                } else if (current_type->data == Tuple) {
                    struct tuple* t = to_tuple_ptr(value);
                    current_type = current_type->tail;
            
                    printf(\"%c\", '(');
                    current_type = print(t->fst, current_type);
                    printf(\"%c\", ',');
                    current_type = print(t->snd, current_type);
                    printf(\"%c\", ')');

                    return current_type;
                }
            
                return types->tail;
            }
            
            // End of the prelude section

        "};

        let decls: Result<Vec<Box<dyn Pretty>>, String> =
            self.fun_decls.iter().map(|f| to_c_decl(f, env)).collect();

        if self.var_decls.is_empty() {
            let mut new_env = env.clone().with_initialize(false);
            let fun_decls: Result<Vec<Box<dyn Pretty>>, String> = self
                .fun_decls
                .iter()
                .map(|f| f.to_c(&mut new_env))
                .collect();
            let fun_decls = fun_decls?;

            Ok(Box::new(
                prelude
                    .join(delimited(&"".join(Sep(1)), decls?))
                    .join(Newline)
                    .join(Newline)
                    .join(delimited(&"".join(Sep(1)).join(Sep(1)), fun_decls)),
            ))
        } else {
            Ok(Box::new(
                prelude
                    .join(delimited(&"".join(Sep(1)), decls?))
                    .join(Newline)
                    .join(Newline)
                    .join("// Global variables")
                    .join(Newline)
                    .join(
                        global_var_decls(&self.var_decls, env)?
                            .join(Newline)
                            .join("// Function declarations")
                            .join(Newline)
                            .join(delimited(
                                &"".join(Sep(1)).join(Sep(1)),
                                error_iter!(self.fun_decls.iter(), env),
                            )),
                    ),
            ))
        }
    }
}

impl ToC for VarDecl {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            self.var_type
                .as_ref()
                .unwrap()
                .to_c(env)?
                .join(" ")
                .join(self.name.to_c(env)?)
                .join(" = ")
                .join(self.value.to_c(env)?)
                .join(";"),
        ))
    }
}

impl ToC for (&Type, &Id) {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            self.0.to_c(env)?.join(" ").join(self.1.to_c(env)?),
        ))
    }
}

fn to_c_decl(f: &FunDecl, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
    if let Some(t) = &f.fun_type {
        if let Type::Function(arg_types, rt) = t {
            return Ok(Box::new(
                rt.to_c(env)?
                    .join(" ")
                    .join(f.name.to_c(env)?)
                    .join("(")
                    .join(delimited(
                        &", ",
                        error_iter!(arg_types.iter().zip(f.params.iter()), env),
                    ))
                    .join(");"),
            ));
        }
    }
    unreachable!()
}

impl ToC for FunDecl {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        if let Some(t) = &self.fun_type {
            if let Type::Function(arg_types, rt) = t {
                if self.var_decls.is_empty() {
                    return Ok(Box::new(
                        rt.to_c(env)?
                            .join(" ")
                            .join(self.name.to_c(env)?)
                            .join("(")
                            .join(delimited(
                                &", ",
                                error_iter!(arg_types.iter().zip(self.params.iter()), env),
                            ))
                            .join(")")
                            .join(Newline)
                            .join("{")
                            .join(if self.name.0 == "main" && env.initialize {
                                Box::new("\tinitialize();\n\n\t")
                            } else {
                                Box::new("")
                            })
                            .join(Indent(Sep(0).join(delimited(
                                &Sep(1),
                                error_iter!(self.statements.iter(), env),
                            ))))
                            .join(Newline)
                            .join("}"),
                    ));
                } else {
                    return Ok(Box::new(
                        rt.to_c(env)?
                            .join(" ")
                            .join(self.name.to_c(env)?)
                            .join("(")
                            .join(delimited(
                                &", ",
                                error_iter!(arg_types.iter().zip(self.params.iter()), env),
                            ))
                            .join(")")
                            .join(Newline)
                            .join("{")
                            .join(Indent(
                                Sep(0)
                                    .join(if self.name.0 == "main" && env.initialize {
                                        Box::new("initialize();\n\n\t")
                                    } else {
                                        Box::new("")
                                    })
                                    .join(delimited(
                                        &Sep(1),
                                        error_iter!(self.var_decls.iter(), env),
                                    )),
                            ))
                            .join(Newline)
                            .join(Indent(Sep(0).join(delimited(
                                &Sep(1),
                                error_iter!(self.statements.iter(), env),
                            ))))
                            .join(Newline)
                            .join("}"),
                    ));
                }
            }

            unreachable!()
        }

        unreachable!()
    }
}

impl ToC for Id {
    fn to_c(&self, _env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(self.0.to_string()))
    }
}

impl ToC for Type {
    fn to_c(&self, _env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(match self {
            Type::Int => Box::new("int"),
            Type::Bool => Box::new("bool"),
            Type::Char => Box::new("char"),
            Type::Void => Box::new("void"),
            Type::Tuple(_, _) => Box::new("struct tuple*"),
            Type::List(_) => Box::new("struct node*"),
            Type::Var(_) => Box::new("intptr_t"),
            Type::Function(_, _) => {
                unreachable!()
            }
        })
    }
}

impl ToC for Statement {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        match self {
            Statement::If(i) => i.to_c(env),
            Statement::While(w) => w.to_c(env),
            Statement::Assign(a) => a.to_c(env),
            Statement::FunCall(f) => {
                if let Some(builtin_fun) = builtin_function(f, env)? {
                    Ok(builtin_fun)
                } else {
                    Ok(Box::new(f.to_c(env)?.join(";")))
                }
            }
            Statement::Return(e) => match e {
                Some(e) => Ok(Box::new("return ".join(e.to_c(env)?).join(";"))),

                // This is only useful for "early returns"
                None => Ok(Box::new("return;")),
            },
        }
    }
}

fn to_c_else_case(false_body: &Vec<Statement>, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
    if false_body.is_empty() {
        Ok(Box::new("".join("")))
    } else {
        Ok(Box::new(
            "".join(Sep(1))
                .join("else {")
                .join(block(delimited(
                    &"".join(Sep(1)),
                    error_iter!(false_body.iter(), env),
                )))
                .join("}"),
        ))
    }
}

impl ToC for If {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            "if (".join(self.cond.to_c(env)?).join(") {").join(
                block(delimited(
                    &"".join(Sep(1)),
                    error_iter!(self.if_true.iter(), env),
                ))
                .join("}")
                .join(to_c_else_case(&self.if_false, env)?),
            ),
        ))
    }
}

impl ToC for While {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(Box::new(
            "while (".join(
                self.cond
                    .to_c(env)?
                    .join(") {")
                    .join(block(delimited(
                        &"".join(Sep(1)),
                        error_iter!(self.body.iter(), env),
                    )))
                    .join("}"),
            ),
        ))
    }
}

impl ToC for Assign {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        let lhs = if self.target.fields.is_empty() {
            self.target.to_c(env)?
        } else {
            variable_field_access(&self.target, false)?
        };

        Ok(Box::new(
            lhs.join(" = ")
                .join(
                    self.value.to_c(
                        &mut env
                            .clone()
                            .with_cast_to_intptr(!self.target.fields.is_empty()),
                    )?,
                )
                .join(";"),
        ))
    }
}

impl ToC for TypedExpr {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        let precedence = self.precedence();

        match &self.expr {
            Expr::Or(e1, e2) => bin_expr!(e1, e2, env, precedence, " || "),
            Expr::And(e1, e2) => bin_expr!(e1, e2, env, precedence, " && "),
            Expr::Equals(e1, e2) => bin_expr!(e1, e2, env, precedence, " == "),
            Expr::NotEquals(e1, e2) => bin_expr!(e1, e2, env, precedence, " != "),
            Expr::Lt(e1, e2) => bin_expr!(e1, e2, env, precedence, " < "),
            Expr::Le(e1, e2) => bin_expr!(e1, e2, env, precedence, " <= "),
            Expr::Gt(e1, e2) => bin_expr!(e1, e2, env, precedence, " > "),
            Expr::Ge(e1, e2) => bin_expr!(e1, e2, env, precedence, " >= "),
            Expr::Add(e1, e2) => bin_expr!(e1, e2, env, precedence, " + "),
            Expr::Sub(e1, e2) => bin_expr!(e1, e2, env, precedence, " - "),
            Expr::Mul(e1, e2) => bin_expr!(e1, e2, env, precedence, " * "),
            Expr::Div(e1, e2) => bin_expr!(e1, e2, env, precedence, " / "),
            Expr::Mod(e1, e2) => bin_expr!(e1, e2, env, precedence, " % "),
            Expr::Cons(e1, e2) => {
                let prefix = if env.cast_to_intptr {
                    "(intptr_t)list_node_new((intptr_t)"
                } else {
                    "list_node_new((intptr_t)"
                };
                Ok(Box::new(
                    prefix
                        .join(e1.to_c(env)?)
                        .join(",")
                        .join(e2.to_c(&mut env.clone().with_cast_to_intptr(false))?)
                        .join(")"),
                ))
            }
            Expr::UnaryMinus(e) => Ok(Box::new(Group::new(
                "-".join(child_expr!(e, precedence, env)),
            ))),
            Expr::Not(e) => Ok(Box::new(Group::new(
                "!".join(child_expr!(e, precedence, env)),
            ))),
            Expr::Atom(a) => a.to_c(env),
        }
    }
}

impl ToC for Atom {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        Ok(match self {
            Atom::IntLiteral(i) => Box::new(i.to_str_radix(10)),
            Atom::BoolLiteral(b) => Box::new(match b {
                true => String::from("1"),
                false => String::from("0"),
            }),
            Atom::CharLiteral(c) => Box::new(format!("'{}'", c.to_string())),
            Atom::StringLiteral(string) => {
                let mut list_expr: TypedExpr = Expr::Atom(Atom::EmptyList).into();

                for c in string.chars().rev() {
                    list_expr = Expr::Cons(
                        Box::new(Expr::Atom(Atom::CharLiteral(c)).into()),
                        Box::new(list_expr),
                    )
                    .into();
                }

                list_expr.to_c(env)?
            }
            Atom::FunCall(f) => f.to_c(env)?,
            Atom::Variable(v) => v.to_c(env)?,
            Atom::EmptyList => Box::new(String::from("NULL")),
            Atom::Tuple(e1, e2) => {
                let prefix = if env.cast_to_intptr {
                    "(intptr_t)tuple_new("
                } else {
                    "tuple_new("
                };
                let mut new_env = env.clone().with_cast_to_intptr(true);

                Box::new(
                    prefix
                        .join(e1.to_c(&mut new_env)?)
                        .join(", ")
                        .join(e2.to_c(&mut new_env)?)
                        .join(")"),
                )
            }
        })
    }
}

impl ToC for FunCall {
    fn to_c(&self, env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        if let Some(Type::Function(at, rt)) = &self.fun_type {
            let pref = if let Type::Var(rtv) = rt.deref() {
                if let Some(i) = at.iter().position(|t| {
                    if let Type::Var(rtv2) = t {
                        rtv == rtv2
                    } else {
                        false
                    }
                }) {
                    match self.args[i].expr_type.as_ref().unwrap() {
                        Type::Int => "(int)",
                        Type::Char => "(char)",
                        Type::Bool => "(bool)",
                        Type::Tuple(_, _) => "(struct tuple*)",
                        Type::List(_) => "(struct node*)",
                        _ => "",
                    }
                } else {
                    ""
                }
            } else {
                ""
            };

            let annotated_args: Vec<Join<&str, Box<dyn Pretty>>> = at
                .iter()
                .zip(error_iter!(self.args.iter(), env))
                .map(|(t, e)| {
                    let pre = match t {
                        Type::Var(_) => "(intptr_t)",
                        _ => "",
                    };

                    pre.join(e)
                })
                .collect();

            Ok(Box::new(Group::new(
                pref.join(self.name.0.to_string())
                    .join("(")
                    .join(block(delimited(&",".join(Sep(1)), annotated_args)).join(")")),
            )))
        } else {
            Ok(Box::new(Group::new(
                self.name.0.to_string().join("(").join(
                    block(delimited(
                        &",".join(Sep(1)),
                        error_iter!(self.args.iter(), env),
                    ))
                    .join(")"),
                ),
            )))
        }
    }
}

fn variable_field_access(v: &Variable, with_lhs_cast: bool) -> Result<Box<dyn Pretty>, String> {
    let mut acc = v.name.0.to_string();
    let mut t = v.var_type.as_ref().unwrap();

    let mut field_it = v.fields.iter().peekable();

    // Only cast if we didn't get a list by tail (because we already were a list).
    let mut list_cast;

    while let Some(field) = field_it.next() {
        list_cast = true;
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
                if let Type::List(t1) = &t {
                    t = t1;
                    acc = acc + "->data";
                } else {
                    unreachable!();
                }
            }

            Field::Tl => {
                list_cast = false;
                acc = acc + "->tail";
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
            Type::Bool => {
                if with_lhs_cast {
                    acc = "(bool) ".to_string() + acc.as_str();
                }
            }
            Type::Tuple(_, _) => {
                acc = "to_tuple_ptr(".to_string() + acc.as_str() + ")";
            }
            Type::List(_) => {
                if list_cast {
                    acc = "to_list_ptr(".to_string() + acc.as_str() + ")";
                }
            }
            _ => {}
        }
    }
    Ok(Box::new(acc))
}

impl ToC for Variable {
    fn to_c(&self, _env: &mut CEnv) -> Result<Box<dyn Pretty>, String> {
        if self.fields.is_empty() {
            Ok(Box::new(self.name.0.to_string()))
        } else {
            variable_field_access(&self, true)
        }
    }
}
