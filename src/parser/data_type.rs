use std::ops::{Deref, DerefMut};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use super::ast::VarDecl;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
    Int,
    Bool,
    Char,
    String,
    Function(Box<DataType>, Box<DataType>),
    Tuple(Box<DataType>, Box<DataType>),

    // LType
    Array(Box<DataType>),

    // TypeVar
    Var(TypeVar),
}

impl DataType {
    pub fn mgu(&self, other: &Self) -> Result<Subst, String> {
        match (self, other) {
            (DataType::Int, DataType::Int)
            | (DataType::Bool, DataType::Bool)
            | (DataType::Char, DataType::Char)
            | (DataType::String, DataType::String) => Ok(Subst::default()),

            (DataType::Var(t1), t2) => t1.bind(t2),
            (t1, DataType::Var(t2)) => t2.bind(t1),

            (DataType::Function(t1, t2), DataType::Function(t3, t4))
            | (DataType::Tuple(t1, t2), DataType::Tuple(t3, t4)) => {
                let s1 = t1.mgu(t2)?;
                let s2 = t3.apply(&s1).mgu(&t4.apply(&s1))?;

                Ok(s1.compose(s2))
            }

            (DataType::Array(t1), DataType::Array(t2)) => t1.mgu(&t2),

            (t1, t2) => Err(String::from(format!(
                "Unification error: {:?} and {:?}",
                t1, t2
            ))),
        }
    }
}

#[derive(Default)]
pub struct VarGenerator {
    counter: usize,
}

impl VarGenerator {
    pub fn new_var(&mut self) -> TypeVar {
        let var = TypeVar(self.counter);
        self.counter += 1;

        var
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeVar(usize);

impl TypeVar {
    fn bind(&self, ty: &DataType) -> Result<Subst, String> {
        if let DataType::Var(v) = ty {
            if v == self {
                return Ok(Subst::default());
            }
        }

        if ty.ftv().contains(self) {
            return Err(String::from(format!(
                "Occur check cannot construct infinite type: {:?}",
                self
            )));
        }

        let mut s = Subst::default();
        s.insert(self.clone(), ty.clone());
        Ok(s)
    }
}

// Finite mapping from type variables to types.

#[derive(Clone, Default)]
pub struct Subst(HashMap<TypeVar, DataType>);

impl Deref for Subst {
    type Target = HashMap<TypeVar, DataType>;
    fn deref(&self) -> &HashMap<TypeVar, DataType> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TypeVar, DataType> {
        &mut self.0
    }
}

impl Subst {
    pub fn compose(&self, mut other: Subst) -> Subst {
        let applied = other
            .iter_mut()
            .map(|(k, v)| (k.clone(), v.apply(self)))
            .collect();

        self.union(&Subst(applied))
    }

    fn union(&self, other: &Subst) -> Subst {
        let mut unified = Subst::default();

        for (k, v) in self.iter() {
            unified.insert(k.clone(), v.clone());
        }
        for (k, v) in other.iter() {
            unified.insert(k.clone(), v.clone());
        }

        unified
    }
}

trait TypeInstance {
    // Determines the free type variables of a type.
    fn ftv(&self) -> HashSet<TypeVar>;

    // Apply a substitution.
    fn apply(&self, subst: &Subst) -> Self;
}

impl<'a, T> TypeInstance for Vec<T>
where
    T: TypeInstance,
{
    // The free type variables of a vector of types is the union of the free type variables of each
    // of the types in the vector.
    fn ftv(&self) -> HashSet<TypeVar> {
        self.iter()
            .map(|x| x.ftv())
            .fold(HashSet::new(), |set, x| set.union(&x).cloned().collect())
    }

    // To apply a substitution to a vector of types, just apply to each type in the vector.
    fn apply(&self, s: &Subst) -> Vec<T> {
        self.iter().map(|x| x.apply(s)).collect()
    }
}

impl TypeInstance for DataType {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            // Primitive types have no ftv.
            DataType::Int | DataType::Bool | DataType::Char | DataType::String => HashSet::new(),

            // A TypeVar has one ftv: itself
            DataType::Var(t) => HashSet::from([t.to_owned()]),

            // A function and a tuple have the unification of ftv of the inner types.
            DataType::Function(a, b) | DataType::Tuple(a, b) => {
                a.ftv().union(&b.ftv()).copied().collect()
            }

            // A list has the inner type as ftv.
            DataType::Array(inner) => inner.ftv(),
        }
    }

    fn apply(&self, substitution: &Subst) -> DataType {
        match self {
            DataType::Int | DataType::Bool | DataType::Char | DataType::String => self.clone(),

            DataType::Var(t) => substitution.get(t).cloned().unwrap_or(self.clone()),

            DataType::Function(a, b) => DataType::Function(
                Box::new(a.apply(substitution)),
                Box::new(b.apply(substitution)),
            ),

            DataType::Tuple(a, b) => DataType::Tuple(
                Box::new(a.apply(substitution)),
                Box::new(b.apply(substitution)),
            ),

            DataType::Array(inner) => DataType::Array(Box::new(inner.apply(substitution))),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeScheme {
    pub ty_vars: Vec<TypeVar>,
    pub ty: DataType,
}

impl TypeInstance for TypeScheme {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.ty
            .ftv()
            .difference(&self.ty_vars.iter().cloned().collect())
            .cloned()
            .collect()
    }

    fn apply(&self, s: &Subst) -> TypeScheme {
        let mut filtered_substitution = s.clone();
        filtered_substitution.retain(|k, _| !self.ty_vars.contains(k));

        Self::new(self.ty_vars.clone(), self.ty.apply(&filtered_substitution))
    }
}

impl TypeScheme {
    pub fn new(vars: Vec<TypeVar>, ty: DataType) -> Self {
        Self {
            ty_vars: vars,
            ty: ty,
        }
    }

    fn instantiate(&self, generator: &mut VarGenerator) -> DataType {
        let newvars = self
            .ty_vars
            .iter()
            .map(|_| DataType::Var(generator.new_var()));
        self.ty
            .apply(&Subst(self.ty_vars.iter().cloned().zip(newvars).collect()))
    }
}

#[derive(Default)]
pub struct TypeEnv(HashMap<String, TypeScheme>);

impl TypeInstance for TypeEnv {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.0
            .values()
            .map(|x| x.clone())
            .collect::<Vec<TypeScheme>>()
            .ftv()
    }

    fn apply(&self, s: &Subst) -> TypeEnv {
        TypeEnv(
            self.0
                .iter()
                .map(|(k, v)| (k.clone(), v.apply(s)))
                .collect(),
        )
    }
}

impl TypeEnv {
    fn generalize(&self, ty: &DataType) -> TypeScheme {
        TypeScheme::new(
            ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty.clone(),
        )
    }
}
