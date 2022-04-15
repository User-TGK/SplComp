#[cfg(test)]
mod test;

mod builtin;
use builtin::Builtin;

use crate::ast::*;

use log;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

impl Type {
    pub fn mgu(&self, other: &Self) -> Result<Subst, String> {
        match (self, other) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Char, Type::Char) => {
                Ok(Subst::default())
            }

            (Type::Var(t1), t2) => t1.bind(t2),
            (t1, Type::Var(t2)) => t2.bind(t1),

            (Type::Function(at1, rt1), Type::Function(at2, rt2)) => {
                let s1 = rt1.mgu(rt2)?;
                let mut composed_subst = s1;

                if at1.len() != at2.len() {
                    return Err(String::from(
                        "Functions with different argument length cannot be unified",
                    ));
                }

                let args_to_unify = at1.iter().zip(at2.iter());

                for (t1, t2) in args_to_unify {
                    let s = t1.apply(&composed_subst).mgu(&t2.apply(&composed_subst))?;
                    composed_subst = composed_subst.compose(&s);
                }

                Ok(composed_subst)
            }

            (Type::Tuple(t1, t2), Type::Tuple(t3, t4)) => {
                let s1 = t1.mgu(t3)?;
                let s2 = t2.apply(&s1).mgu(&t4.apply(&s1))?;

                Ok(s1.compose(&s2))
            }

            (Type::List(t1), Type::List(t2)) => t1.mgu(t2),

            (t1, t2) => Err(format!("Unification error: {:?} and {:?}", t1, t2)),
        }
    }
}

#[derive(Default)]
pub struct VarGenerator {
    counter: usize,
}

impl VarGenerator {
    pub fn new_var(&mut self) -> TypeVar {
        let var = format!("t{}", self.counter);
        self.counter += 1;

        var.into()
    }
}

type TypeVar = Id;

impl TypeVar {
    // Binds `self` to a type and return the associated substitution, except
    // binding to itself and infinite types (occur check).
    fn bind(&self, ty: &Type) -> Result<Subst, String> {
        if let Type::Var(v) = ty {
            if v == self {
                return Ok(Subst::default());
            }
        }

        if ty.ftv().contains(self) {
            return Err(format!(
                "Occur check cannot construct infinite type: {}",
                self
            ));
        }

        let mut s = Subst::default();
        s.insert(self.clone(), ty.clone());

        Ok(s)
    }
}

impl From<TypeVar> for Type {
    fn from(var: TypeVar) -> Self {
        Type::Var(var)
    }
}

impl From<Type> for TypeScheme {
    // This can be used to create a TypeScheme from a Type, where
    // `generalize` is not applicable (e.g. for var types, enforcing Value restriction)
    fn from(ty: Type) -> Self {
        Self::new(vec![], ty)
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
    pub fn compose(&self, other: &Subst) -> Subst {
        let applied = other
            .clone()
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

trait TypeInference {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String>;
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
            Type::Int | Type::Bool | Type::Char | Type::Void => HashSet::new(),

            // A TypeVar has one ftv: itself.
            Type::Var(t) => HashSet::from([t.to_owned()]),

            // A tuple has the unification of ftv of the inner types.
            Type::Tuple(a, b) => a.ftv().union(&b.ftv()).cloned().collect(),

            // A function has the unification of ftv of the arg and return types.
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
            Type::Int | Type::Bool | Type::Char | Type::Void => self.clone(),

            Type::Var(t) => substitution.get(t).cloned().unwrap_or_else(|| self.clone()),

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
        Self { ty_vars: vars, ty }
    }

    fn instantiate(&self, generator: &mut VarGenerator) -> Type {
        let newvars = self.ty_vars.iter().map(|_| Type::Var(generator.new_var()));
        self.ty
            .apply(&Subst(self.ty_vars.iter().cloned().zip(newvars).collect()))
    }
}

#[derive(Clone, Default, Debug)]
pub struct Context {
    pub functions: TypeEnv,
    pub global_vars: TypeEnv,
    pub local_vars: TypeEnv,
}

impl Context {
    pub fn get_fun(&self, identifier: &Id) -> Option<&TypeScheme> {
        self.functions.get(identifier)
    }

    pub fn get_var(&self, identifier: &Id) -> Option<&TypeScheme> {
        match (
            self.local_vars.contains_key(identifier),
            self.global_vars.contains_key(identifier),
        ) {
            // The variable from local scope is returned.
            (true, true) => self.local_vars.get(identifier),
            (true, false) => self.local_vars.get(identifier),
            (false, true) => self.global_vars.get(identifier),
            (false, false) => {
                log::error!("Unbounded variable '{}'.", identifier);

                None
            }
        }
    }
}

impl TypeInstance for Context {
    fn ftv(&self) -> HashSet<TypeVar> {
        let mut res = HashSet::default();

        res.extend(self.functions.ftv());
        res.extend(self.global_vars.ftv());
        res.extend(self.local_vars.ftv());

        res
    }

    fn apply(&self, s: &Subst) -> Context {
        Context {
            functions: self.functions.apply(&s),
            global_vars: self.global_vars.apply(&s),
            local_vars: self.local_vars.apply(&s),
        }
    }
}

#[derive(Clone, Default, Debug)]
pub struct TypeEnv(HashMap<Id, TypeScheme>);

impl TypeInstance for TypeEnv {
    fn ftv(&self) -> HashSet<TypeVar> {
        self.0.values().cloned().collect::<Vec<TypeScheme>>().ftv()
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

impl TypeInference for Variable {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        match context.get_var(&self.name) {
            Some(s) => {
                if self.fields.is_empty() {
                    expected.mgu(&s.instantiate(generator))
                } else {
                    let mut call_expr =
                        Expr::Atom(Atom::Variable(Variable::new(self.name.clone(), vec![])));

                    for f in &self.fields {
                        let builtin_identifier = f.builtin_identifier();
                        call_expr = Expr::Atom(Atom::FunCall(FunCall::new(
                            builtin_identifier,
                            vec![call_expr],
                        )));
                    }

                    call_expr.infer(context, expected, generator)
                }
            }
            None => Err(format!("Unbounded variable: {}", self.name)),
        }
    }
}

impl TypeInference for Vec<Statement> {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        let mut s = Subst::default();

        for stat in self {
            s = stat
                .infer(&context.apply(&s), &expected.apply(&s), generator)?
                .compose(&s);
        }

        Ok(s)
    }
}

impl TypeInference for Statement {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        match self {
            Statement::While(w) => {
                let s = w.cond.infer(context, &Type::Bool, generator)?;
                Ok(w.body
                    .infer(&context.apply(&s), &expected.apply(&s), generator)?
                    .compose(&s))
            }

            Statement::If(i) => {
                let s1 = i.cond.infer(context, &Type::Bool, generator)?;
                let (expected, context) = (expected.apply(&s1), context.apply(&s1));
                let s2 = i
                    .if_true
                    .infer(&context.apply(&s1), &expected.apply(&s1), generator)?
                    .compose(&s1);
                let s3 = i
                    .if_false
                    .infer(&context.apply(&s2), &expected.apply(&s2), generator)?
                    .compose(&s2);

                Ok(s3)
            }

            Statement::VarDecl(v) => {
                let fresh = generator.new_var();
                v.infer(context, &fresh.into(), generator)
            }
            Statement::Assign(a) => match context.get_var(&a.target.name) {
                Some(ts) => a.value.infer(context, &ts.ty, generator),
                None => Err(format!("Unknown identifier '{}'", a.target.name)),
            },

            Statement::FunCall(f) => f.infer(context, expected, generator),

            Statement::Return(e) => match e {
                Some(e) => e.infer(context, expected, generator),
                None => expected.mgu(&Type::Void),
            },
        }
    }
}

impl TypeInference for FunCall {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        match context.get_fun(&self.name) {
            Some(ts) => {
                let mut arg_types = vec![];
                let mut sub = Subst::default();

                for a in &mut self.args {
                    let fresh = generator.new_var();

                    let s = a.infer(&context.apply(&sub), &fresh.clone().into(), generator)?;
                    arg_types.push(s[&fresh].clone());

                    sub = sub.compose(&s);
                }

                let fun_type = Type::Function(arg_types, Box::new(expected.clone())).apply(&sub);
                let final_s = ts.instantiate(generator).apply(&sub).mgu(&fun_type)?;

                Ok(sub.compose(&final_s))
            }
            None => Err(format!("Unknown function identifier '{}'", self.name)),
        }
    }
}

impl TypeInference for FunDecl {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        let mut context = context.clone();
        let mut arg_types = vec![];

        for p in &self.params {
            let alpha = Type::Var(generator.new_var());

            if context.global_vars.contains_key(&p) {
                log::warn!("Function parameter '{}' hides global variable.", p);
            }
            if context.local_vars.contains_key(&p) {
                return Err(format!("Multiple definitions of parameter '{}'.", p));
            }

            context.local_vars.insert(p.clone(), alpha.clone().into());
            arg_types.push(alpha);
        }

        let beta = Type::Var(generator.new_var());
        let fun_type = Type::Function(arg_types, Box::new(beta.clone()));

        context
            .functions
            .insert(self.name.clone(), fun_type.clone().into());

        let mut subst = expected.mgu(&fun_type)?;

        for s in &mut self.statements {
            // We do something special here: we need to update our environment (function scope)
            // when we have a local variable.
            if let Statement::VarDecl(v) = s {
                if context.global_vars.contains_key(&v.name) {
                    log::warn!("Local variable '{}' hides global variable.", v.name);
                }

                let fresh = generator.new_var();

                subst = v
                    .infer(&context, &fresh.clone().into(), generator)?
                    .compose(&subst);

                context = context.apply(&subst);
                context
                    .local_vars
                    .insert(v.name.clone(), subst[&fresh].clone().into());

                continue;
            }

            subst = s
                .infer(&context, &beta.apply(&subst), generator)?
                .compose(&subst);
            context = context.apply(&subst);
        }

        Ok(subst)
    }
}

impl TypeInference for VarDecl {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        if let Type::Var(v) = expected {
            // Determine the variable type from the expression.
            let s = self.value.infer(context, expected, generator)?;
            let infered_var_type = s[v].clone().apply(&s);

            match &self.var_type {
                // We already had a type specified by the programmer.
                // Compose substitution s2 from the infer substitution s,
                // and the unification of the infered type and specified type.
                Some(t) => {
                    let s2 = t.mgu(&infered_var_type)?.compose(&s);
                    self.var_type = Some(t.apply(&s2));

                    Ok(s2)
                }
                None => {
                    self.var_type = Some(infered_var_type);

                    Ok(s)
                }
            }
        } else {
            // This should never be executed. Above, we need the fresh type variable
            // identifier (Sigma) in order to decorate the AST with the infered
            // type, which means that we have to handle the case where Sigma is
            // not a type variable.
            Err(String::from(
                "VarDecl inference specialized to non-type var type.",
            ))
        }
    }
}

impl TypeInference for Expr {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        match self {
            Expr::Or(e1, e2) | Expr::And(e1, e2) => {
                let s1 = e1.infer(context, &Type::Bool, generator)?;
                let context = context.apply(&s1);

                let s2 = e2.infer(&context, &Type::Bool, generator)?.compose(&s1);
                let expected_applied = expected.apply(&s2);

                Ok(expected_applied.mgu(&Type::Bool)?.compose(&s2))
            }

            Expr::Equals(e1, e2) | Expr::NotEquals(e1, e2) => {
                let alpha = Type::Var(generator.new_var());
                let s1 = e1.infer(context, &alpha, generator)?;
                let context = context.apply(&s1);

                let s2 = e2.infer(&context, &alpha.apply(&s1), generator)?;
                let expected_applied = expected.apply(&s2);

                Ok(expected_applied.mgu(&Type::Bool)?.compose(&s2))
            }

            Expr::Lt(e1, e2) | Expr::Le(e1, e2) | Expr::Gt(e1, e2) | Expr::Ge(e1, e2) => {
                let s1 = e1.infer(context, &Type::Int, generator)?;
                let context = context.apply(&s1);

                let s2 = e2.infer(&context, &Type::Int, generator)?.compose(&s1);
                let expected_applied = expected.apply(&s2);

                Ok(expected_applied.mgu(&Type::Bool)?.compose(&s2))
            }

            Expr::Add(e1, e2)
            | Expr::Sub(e1, e2)
            | Expr::Mul(e1, e2)
            | Expr::Div(e1, e2)
            | Expr::Mod(e1, e2) => {
                let s1 = e1.infer(context, &Type::Int, generator)?;
                let context = context.apply(&s1);

                let s2 = e2.infer(&context, &Type::Int, generator)?.compose(&s1);
                let expected_applied = expected.apply(&s2);

                Ok(expected_applied.mgu(&Type::Int)?.compose(&s2))
            }

            Expr::Cons(e1, e2) => {
                let alpha = Type::Var(generator.new_var());

                let s1 = e1.infer(context, &alpha, generator)?;

                let list_of_alpha_type = Type::List(Box::new(alpha.apply(&s1)));

                let context = context.apply(&s1);

                let s2 = e2
                    .infer(&context, &list_of_alpha_type, generator)?
                    .compose(&s1);
                let expected_applied = expected.apply(&s2);
                let list_type_applied = list_of_alpha_type.apply(&s2);

                Ok(expected_applied.mgu(&list_type_applied)?.compose(&s2))
            }

            Expr::UnaryMinus(e) => {
                let s = e.infer(context, &Type::Int, generator)?;

                let expected_applied = expected.apply(&s);
                Ok(expected_applied.mgu(&Type::Int)?.compose(&s))
            }

            Expr::Not(e) => {
                let s = e.infer(context, &Type::Bool, generator)?;

                let expected_applied = expected.apply(&s);
                Ok(expected_applied.mgu(&Type::Bool)?.compose(&s))
            }

            Expr::Atom(a) => match a {
                Atom::IntLiteral(_) => expected.mgu(&Type::Int),
                Atom::BoolLiteral(_) => expected.mgu(&Type::Bool),
                Atom::CharLiteral(_) => expected.mgu(&Type::Char),
                Atom::StringLiteral(_) => expected.mgu(&Type::List(Box::new(Type::Char))),

                Atom::FunCall(f) => f.infer(context, expected, generator),
                Atom::Variable(v) => v.infer(context, expected, generator),

                Atom::EmptyList => {
                    let alpha = Type::Var(generator.new_var());
                    expected.mgu(&Type::List(Box::new(alpha)))
                }

                Atom::Tuple(e1, e2) => {
                    let alpha1 = Type::Var(generator.new_var());
                    let alpha2 = Type::Var(generator.new_var());

                    let s1 = e1.infer(context, &alpha1, generator)?;
                    let context = context.apply(&s1);

                    let s2 = e2.infer(&context, &alpha2, generator)?.compose(&s1);

                    let expected_applied = expected.apply(&s2);
                    let tuple_type_applied =
                        Type::Tuple(Box::new(alpha1), Box::new(alpha2)).apply(&s2);

                    Ok(expected_applied.mgu(&tuple_type_applied)?.compose(&s2))
                }
            },
        }
    }
}

impl TypeEnv {
    fn generalize(&self, ty: &Type) -> TypeScheme {
        TypeScheme::new(
            ty.ftv().difference(&self.ftv()).cloned().collect(),
            ty.clone(),
        )
    }
}

impl TypeInference for Program {
    fn infer(
        &mut self,
        context: &Context,
        _expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        let mut context = context.clone();

        for p in self.iter_mut() {
            match p {
                Decl::VarDecl(v) => {
                    if context.global_vars.contains_key(&v.name) {
                        return Err(format!("Global variable {} previously declared", v.name));
                    }

                    let fresh = generator.new_var();
                    let s = v.infer(&context, &fresh.clone().into(), generator)?;

                    context.apply(&s);
                    context
                        .global_vars
                        .insert(v.name.clone(), s[&fresh].clone().into());
                }
                Decl::FunDecl(f) => {
                    if context.functions.contains_key(&f.name) {
                        return Err(format!("Function {} previously declared", f.name));
                    }

                    let fresh = generator.new_var();
                    let s = f.infer(&context, &fresh.clone().into(), generator)?;

                    let infered_fun_type = s[&fresh].clone().apply(&s);

                    match &f.fun_type {
                        // We already had a type specified by the programmer.
                        // Compose substitution s2 from the infer substitution s,
                        // and the unification of the infered type and specified type.
                        Some(t) => {
                            let s2 = t.mgu(&infered_fun_type)?.compose(&s);
                            context.apply(&s2);

                            let generalized = context.functions.generalize(&t.apply(&s2));
                            context.functions.insert(f.name.clone(), generalized);

                            f.fun_type = Some(t.apply(&s2));
                        }
                        None => {
                            context.apply(&s);

                            let generalized =
                                context.functions.generalize(&infered_fun_type.apply(&s));
                            context.functions.insert(f.name.clone(), generalized);

                            f.fun_type = Some(infered_fun_type.apply(&s));
                        }
                    }
                }
            }
        }

        Ok(Subst::default())
    }
}

pub fn run(program: &mut Program) -> Result<(), String> {
    let mut context = Context::default();

    let mut generator = VarGenerator::default();
    let builtin = Builtin::default();

    // Load builtin functions to the fun context
    builtin.load(&mut context.functions, &mut generator);

    let fresh = generator.new_var();

    program.infer(&context, &fresh.into(), &mut generator)?;

    Ok(())
}
