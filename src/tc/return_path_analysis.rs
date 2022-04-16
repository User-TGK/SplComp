use crate::ast::{FunDecl, Statement};

pub enum ReturnType {
    None,
    Some,
    Incomplete,
    Void,
}

pub trait ReturnPathAnalysis {
    fn check_returns(&self) -> ReturnType;
}

impl ReturnPathAnalysis for FunDecl {
    fn check_returns(&self) -> ReturnType {
        self.statements.check_returns()
    }
}

impl ReturnPathAnalysis for Vec<Statement> {
    fn check_returns(&self) -> ReturnType {
        let mut incomplete = None;

        for s in self {
            let ret = s.check_returns();

            match ret {
                ReturnType::None => {
                    continue;
                }
                ReturnType::Incomplete => {
                    incomplete = Some(ReturnType::Incomplete);
                }
                _ => {
                    return ret;
                }
            }
        }

        match incomplete {
            Some(i) => i,
            None => ReturnType::None,
        }
    }
}

impl ReturnPathAnalysis for Statement {
    fn check_returns(&self) -> ReturnType {
        match self {
            Statement::If(i) => match (i.if_true.check_returns(), i.if_false.check_returns()) {
                (ReturnType::None, ReturnType::None) => ReturnType::None,
                (ReturnType::None, _) => ReturnType::Incomplete,
                (_, ReturnType::None) => ReturnType::Incomplete,
                (ReturnType::Incomplete, _) => ReturnType::Incomplete,
                (_, ReturnType::Incomplete) => ReturnType::Incomplete,
                (ReturnType::Void, ReturnType::Void) => ReturnType::Void,
                (ReturnType::Some, ReturnType::Some) => ReturnType::Some,

                // Here we have a type conflict, this will become a proper error from the
                // type inferencer in the next analysis.
                (ReturnType::Some, ReturnType::Void) => ReturnType::Some,
                (ReturnType::Void, ReturnType::Some) => ReturnType::Some,
            },

            Statement::While(w) => w.body.check_returns(),

            Statement::Assign(_) => ReturnType::None,
            Statement::FunCall(_) => ReturnType::None,
            Statement::Return(e) => match e {
                Some(_) => ReturnType::Some,
                None => ReturnType::Void,
            },
        }
    }
}
