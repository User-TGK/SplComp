use crate::ast::{FunDecl, Statement};

pub enum Returning {
    NoneExplicit,
    NoneImplicit,
    All,
    Incomplete,
}

pub trait ReturnPathAnalysis {
    fn check_returns(&self) -> Returning;
}

impl ReturnPathAnalysis for FunDecl {
    fn check_returns(&self) -> Returning {
        self.statements.check_returns()
    }
}

impl ReturnPathAnalysis for Vec<Statement> {
    fn check_returns(&self) -> Returning {
        let mut incomplete = None;

        for s in self {
            let ret = s.check_returns();

            match ret {
                Returning::NoneImplicit => {
                    continue;
                }
                Returning::Incomplete => {
                    incomplete = Some(Returning::Incomplete);
                }
                _ => {
                    return ret;
                }
            }
        }

        match incomplete {
            Some(i) => i,
            None => Returning::NoneImplicit,
        }
    }
}

impl ReturnPathAnalysis for Statement {
    fn check_returns(&self) -> Returning {
        match self {
            Statement::If(i) => match (i.if_true.check_returns(), i.if_false.check_returns()) {
                (Returning::NoneImplicit, Returning::NoneImplicit) => Returning::NoneImplicit,
                (Returning::NoneImplicit, _) => Returning::Incomplete,
                (_, Returning::NoneImplicit) => Returning::Incomplete,
                (Returning::Incomplete, _) => Returning::Incomplete,
                (_, Returning::Incomplete) => Returning::Incomplete,
                (Returning::NoneExplicit, Returning::NoneExplicit) => Returning::NoneExplicit,
                (Returning::All, Returning::All) => Returning::All,

                // Here we have a type conflict, this will become a proper error from the
                // type inferencer in the next analysis.
                (Returning::All, Returning::NoneExplicit) => Returning::All,
                (Returning::NoneExplicit, Returning::All) => Returning::All,
            },

            Statement::While(_) | Statement::Assign(_) | Statement::FunCall(_) => {
                Returning::NoneImplicit
            }
            Statement::Return(e) => match e {
                Some(_) => Returning::All,
                None => Returning::NoneExplicit,
            },
        }
    }
}
