use super::{Subst, TypeInstance};

use crate::ast::*;

/// Helper trait to update the type of AST nodes by a substitution. This
/// is used to set the correct type of typed expressions within mutually recursive
/// functions that would not get the correct type otherwise.
pub trait UpdateType {
    fn update_type(&mut self, subst: &Subst);
}

impl UpdateType for Variable {
    fn update_type(&mut self, subst: &Subst) {
        match &mut self.var_type {
            Some(t) => *t = t.apply(subst),
            None => {}
        }
    }
}

impl UpdateType for Statement {
    fn update_type(&mut self, subst: &Subst) {
        match self {
            Statement::While(w) => w.update_type(subst),
            Statement::If(i) => i.update_type(subst),
            Statement::Assign(a) => a.update_type(subst),
            Statement::FunCall(f) => f.update_type(subst),
            Statement::Return(e) => match e {
                Some(e) => e.update_type(subst),
                None => {}
            },
        }
    }
}

impl UpdateType for While {
    fn update_type(&mut self, subst: &Subst) {
        self.cond.update_type(subst);

        for s in &mut self.body {
            s.update_type(subst);
        }
    }
}

impl UpdateType for If {
    fn update_type(&mut self, subst: &Subst) {
        self.cond.update_type(subst);

        for s in &mut self.if_true {
            s.update_type(subst);
        }

        for s in &mut self.if_false {
            s.update_type(subst);
        }
    }
}

impl UpdateType for Assign {
    fn update_type(&mut self, subst: &Subst) {
        self.target.update_type(subst);
        self.value.update_type(subst);
    }
}

impl UpdateType for FunCall {
    fn update_type(&mut self, subst: &Subst) {
        for a in &mut self.args {
            a.update_type(subst);
        }
    }
}

impl UpdateType for Expr {
    fn update_type(&mut self, subst: &Subst) {
        match self {
            Expr::Or(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::And(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Equals(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::NotEquals(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Lt(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Le(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Gt(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Ge(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Add(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Sub(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Mul(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Div(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Mod(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::Cons(t1, t2) => {
                t1.update_type(subst);
                t2.update_type(subst);
            }
            Expr::UnaryMinus(t1) => {
                t1.update_type(subst);
            }
            Expr::Not(t1) => {
                t1.update_type(subst);
            }
            Expr::Atom(a) => match a {
                Atom::IntLiteral(_)
                | Atom::BoolLiteral(_)
                | Atom::CharLiteral(_)
                | Atom::StringLiteral(_)
                | Atom::EmptyList => {}
                Atom::FunCall(f) => {
                    f.update_type(subst);
                }
                Atom::Variable(v) => {
                    v.update_type(subst);
                }
                Atom::Tuple(t1, t2) => {
                    t1.update_type(subst);
                    t2.update_type(subst);
                }
            },
        }
    }
}

impl UpdateType for TypedExpr {
    fn update_type(&mut self, subst: &Subst) {
        match &mut self.expr_type {
            Some(t) => {
                *t = t.apply(subst);
            }
            None => {}
        }

        self.expr.update_type(subst);
    }
}
