use std::collections::{HashMap, HashSet};

pub enum DType {
    Int,
    Bool,
    Char,
    String,
    Function(Box<DType>, Box<DType>),
    Tuple(Box<DType>, Box<DType>),

    // LType
    Array(Box<DType>),

    // Need to move?
    Generic(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(usize);

// Γ
// type TypeEnv = HashMap<>;

// Finite mapping from type variables to types.
type Subst = HashMap<TypeVar, DType>;

trait Types {
    // Determines the free type variables of a type.
    fn ftv(&self) -> HashSet<TypeVar>;

    // Apply a substitution.
    fn apply(&self, subst: Subst);
}

impl Types for DType {
    fn ftv(&self) -> HashSet<TypeVar> {
        return HashSet::new()
    }

    fn apply(&self, subst: Subst) {

    }
}
