use super::ast::*;

use std::ops::{Deref, DerefMut};
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

impl Type {
    pub fn mgu(&self, other: &Self) -> Result<Subst, String> {
        match (self, other) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::String, Type::String) => Ok(Subst::default()),

            (Type::Var(t1), t2) => t1.bind(t2),
            (t1, Type::Var(t2)) => t2.bind(t1),

            (Type::Function(at1, rt1), Type::Function(at2, rt2)) => {
                let s1 = rt1.mgu(&rt2)?;
                let mut composed_subst = s1.clone();

                if at1.len() != at2.len() {
                    return Err(String::from(
                        "Functions with different argument length cannot be unified",
                    ));
                }

                let args_to_unify = at1.iter().zip(at2.iter());

                for (t1, t2) in args_to_unify {
                    let s = t1.apply(&composed_subst).mgu(&t2.apply(&composed_subst))?;
                    composed_subst = composed_subst.compose(s);
                }

                Ok(composed_subst)
            }

            (Type::Tuple(t1, t2), Type::Tuple(t3, t4)) => {
                let s1 = t1.mgu(t3)?;
                let s2 = t2.apply(&s1).mgu(&t4.apply(&s1))?;

                Ok(s1.compose(s2))
            }

            (Type::List(t1), Type::List(t2)) => t1.mgu(&t2),

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
        let var = String::from(format!("t{}", self.counter));
        self.counter += 1;

        Id(var)
    }
}

// #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
// pub struct TypeVar(usize);

type TypeVar = Id;

impl TypeVar {
    fn bind(&self, ty: &Type) -> Result<Subst, String> {
        if let Type::Var(v) = ty {
            if v == self {
                println!("Ehh");
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

#[derive(Clone, Debug, Default)]
pub struct Subst(HashMap<TypeVar, Type>);

impl Deref for Subst {
    type Target = HashMap<TypeVar, Type>;
    fn deref(&self) -> &HashMap<TypeVar, Type> {
        &self.0
    }
}
impl DerefMut for Subst {
    fn deref_mut(&mut self) -> &mut HashMap<TypeVar, Type> {
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

impl TypeInstance for Type {
    fn ftv(&self) -> HashSet<TypeVar> {
        match self {
            // Primitive types have no ftv.
            Type::Int | Type::Bool | Type::Char | Type::String | Type::Void => HashSet::new(),

            // A TypeVar has one ftv: itself
            Type::Var(t) => HashSet::from([t.to_owned()]),

            // A tuple has the unification of ftv of the inner types.
            Type::Tuple(a, b) => a.ftv().union(&b.ftv()).cloned().collect(),

            Type::Function(arg_types, ret_type) => {
                let mut ftv = ret_type.ftv();
                for at in arg_types.clone() {
                    ftv.extend(at.ftv());
                }

                ftv
            }

            // A list has the inner type as ftv.
            Type::List(inner) => inner.ftv(),
        }
    }

    fn apply(&self, substitution: &Subst) -> Type {
        match self {
            Type::Int | Type::Bool | Type::Char | Type::String | Type::Void => self.clone(),

            Type::Var(t) => substitution.get(t).cloned().unwrap_or(self.clone()),

            Type::Function(a, b) => Type::Function(
                a.iter().map(|t| t.apply(substitution)).collect(),
                Box::new(b.apply(substitution)),
            ),

            Type::Tuple(a, b) => Type::Tuple(
                Box::new(a.apply(substitution)),
                Box::new(b.apply(substitution)),
            ),

            Type::List(inner) => Type::List(Box::new(inner.apply(substitution))),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeScheme {
    pub ty_vars: Vec<TypeVar>,
    pub ty: Type,
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
    pub fn new(vars: Vec<TypeVar>, ty: Type) -> Self {
        Self {
            ty_vars: vars,
            ty: ty,
        }
    }

    fn instantiate(&self, generator: &mut VarGenerator) -> Type {
        let newvars = self.ty_vars.iter().map(|_| Type::Var(generator.new_var()));
        self.ty
            .apply(&Subst(self.ty_vars.iter().cloned().zip(newvars).collect()))
    }
}

#[derive(Default, Debug)]
pub struct TypeEnv(HashMap<Id, TypeScheme>);

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

impl Deref for TypeEnv {
    type Target = HashMap<Id, TypeScheme>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl DerefMut for TypeEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl TypeEnv {
    fn generalize(&self, ty: &Type) -> TypeScheme {
        TypeScheme::new(
            ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty.clone(),
        )
    }

    // Function M from slides
    fn ti(
        &self,
        expr: &Expr,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        // match d {
        //     // VarDecl
        //     _ => Err(String::from("Unimplemented")),
        // }

        match expr {
            Expr::Add(e1, e2)
            | Expr::Sub(e1, e2)
            | Expr::Mul(e1, e2)
            | Expr::Div(e1, e2)
            | Expr::Mod(e1, e2) => {
                let s1 = self.ti(e1, &Type::Int, generator)?;
                self.apply(&s1);

                let s2 = self.ti(e2, &Type::Int, generator)?.compose(s1);
                let expected_applied = expected.apply(&s2);

                Ok(expected_applied.mgu(&Type::Int)?.compose(s2))
            }

            Expr::Atom(ref a) => match a {
                // Atom::Variable(v) => {
                //     // TODO: this doesnt work for field calls on a variable. needs to be extended.
                //     match self.get(&v.name) {
                //         Some(s) => Ok((Subst::default(), s.instantiate(generator))),

                //         None => Err(String::from(format!("Unbounded variable: {:?}", v.name))),
                //     }
                // },
                Atom::IntLiteral(_) => expected.mgu(&Type::Int),

                Atom::BoolLiteral(_) => expected.mgu(&Type::Bool),

                _ => Err(String::from("Unimplemented")),
            },
            _ => Err(String::from("Unimplemented")),
        }
    }
}

pub fn run(program: &mut Program) -> Result<(), String> {
    let mut context = TypeEnv::default();
    let mut generator = VarGenerator::default();

    for p in program.iter_mut() {
        match p {
            Decl::VarDecl(v) => {
                let fresh = generator.new_var();

                if let Ok(s) = context.ti(&v.value, &Type::Var(fresh.clone()), &mut generator) {
                    context.apply(&s);
                    context.insert(v.name.clone(), TypeScheme::new(vec![], s[&fresh].clone()));
                }

                // match &v.var_type {
                //     // Type check required.
                //     Some(t) => {

                //     }
                //     // Type inference required.
                //     None => {

                //     }
                // }
            }
            Decl::FunDecl(f) => return Err(String::from("Unimplemented")),
        }
    }

    println!("Context: {:?}", context);

    Ok(())
}

#[cfg(test)]

mod test {

    use super::*;

    use crate::parser::*;
    use crate::scanner::*;
    use crate::token::*;

    #[test]
    fn test_literal_inference() {
        const PROGRAM: &str = r"
            var myInt = 123 + 456;
        ";

        let tokens: Vec<Token> = Scanner::new(PROGRAM).collect();
        let tokens = Tokens::new(&tokens);

        let (rest, mut program) = program_parser(tokens).unwrap();
        assert!(rest.is_empty());

        assert_eq!(Ok(()), run(&mut program));
    }
}
