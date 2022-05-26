#[cfg(test)]
mod test;

mod builtin;
mod return_path_analysis;
pub mod tarjan;

use builtin::Builtin;
use return_path_analysis::{ReturnPathAnalysis, Returning};
use tarjan::Preprocess;

use crate::ast::*;

use log;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

impl Type {
    /// Determine the most general unifier preserving the most general type,
    /// while checking that the inferred type (self) is not more general than
    /// the programmer specified type. In the latter case, we return an error.
    pub fn mgu_specified(&self, specified: &Self) -> Result<Subst, String> {
        match (self, specified) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::Void, Type::Void) => Ok(Subst::default()),
            (Type::Var(tv1), Type::Var(tv2)) => tv2.bind(&Type::Var(tv1.clone())),
            (Type::Var(tv), t2) => tv.bind(t2),
            (t1, Type::Var(_)) => Err(format!(
                "Specified type is more general than actual type {:?}",
                t1
            )),

            (Type::Function(at1, rt1), Type::Function(at2, rt2)) => {
                let s1 = rt1.mgu_specified(rt2)?;
                let mut composed_subst = s1;

                if at1.len() != at2.len() {
                    return Err(String::from(
                        "Functions with different argument length cannot be unified",
                    ));
                }

                let args_to_unify = at1.iter().zip(at2.iter());

                for (t1, t2) in args_to_unify {
                    let s = t1
                        .apply(&composed_subst)
                        .mgu_specified(&t2.apply(&composed_subst))?;
                    composed_subst = composed_subst.compose(&s);
                }

                Ok(composed_subst)
            }

            (Type::Tuple(t1, t2), Type::Tuple(t3, t4)) => {
                let s1 = t1.mgu_specified(t3)?;
                let s2 = t2.apply(&s1).mgu_specified(&t4.apply(&s1))?;

                Ok(s1.compose(&s2))
            }

            (Type::List(t1), Type::List(t2)) => t1.mgu_specified(t2),

            (t1, t2) => Err(format!(
                "Unification error (inferred/specified): {:?} and {:?}",
                t1, t2
            )),
        }
    }

    /// Determine the most general unifier between `self` and `other`.
    pub fn mgu(&self, other: &Self) -> Result<Subst, String> {
        match (self, other) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Char, Type::Char)
            | (Type::Void, Type::Void) => Ok(Subst::default()),

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
                "Occur check cannot construct infinite type: {:?} {}",
                ty, self
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

pub trait TypeInstance {
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
                    let mut call_expr = Expr::Atom(Atom::Variable(Variable::new(
                        None,
                        self.name.clone(),
                        vec![],
                    )));

                    for f in &self.fields {
                        let builtin_identifier = f.builtin_identifier();
                        call_expr = Expr::Atom(Atom::FunCall(FunCall::new(
                            builtin_identifier,
                            vec![call_expr.into()],
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
            Statement::Assign(a) => {
                let s = a.infer(context, expected, generator)?;

                a.target.var_type = Some(
                    context
                        .apply(&s)
                        .get_var(&a.target.name)
                        .ok_or(format!("Missing var from env: {}", a.target.name))?
                        .instantiate(generator)
                        .apply(&s),
                );

                Ok(s)
            }

            Statement::FunCall(f) => f.infer(context, expected, generator),

            Statement::Return(e) => match e {
                Some(e) => e.infer(context, expected, generator),
                None => expected.mgu(&Type::Void),
            },
        }
    }
}

impl TypeInference for Assign {
    fn infer(
        &mut self,
        context: &Context,
        _expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        match context.get_var(&self.target.name) {
            Some(ts) => {
                if !self.target.fields.is_empty() {
                    let fresh = generator.new_var();

                    let mut call_expr = Expr::Atom(Atom::Variable(Variable::new(
                        None,
                        self.target.name.clone(),
                        vec![],
                    )));

                    for f in &self.target.fields {
                        let builtin_identifier = f.builtin_identifier();
                        call_expr = Expr::Atom(Atom::FunCall(FunCall::new(
                            builtin_identifier,
                            vec![call_expr.into()],
                        )));
                    }

                    let s = call_expr.infer(context, &fresh.clone().into(), generator)?;

                    let s2 = self
                        .value
                        .infer(
                            &context.apply(&s),
                            &Into::<Type>::into(fresh.clone()).apply(&s),
                            generator,
                        )?
                        .compose(&s);

                    Ok(s2)
                } else {
                    self.value.infer(context, &ts.ty, generator)
                }
            }
            None => Err(format!("Unknown identifier '{}'", self.target.name)),
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

                    sub = sub.compose(&s);

                    if let Some(t) = s.get(&fresh) {
                        arg_types.push(t.clone().apply(&sub));
                    } else {
                        arg_types.push(fresh.clone().into());
                    }
                }

                let fun_type = Type::Function(arg_types, Box::new(expected.clone())).apply(&sub);
                let gen_type = ts.instantiate(generator);
                let final_s = gen_type.apply(&sub).mgu(&fun_type)?;

                self.fun_type = Some(gen_type);

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

        for v in &mut self.var_decls {
            // We do something special here: we need to update our environment (function scope)
            // when we have a local variable.
            if context.global_vars.contains_key(&v.name) {
                log::warn!("Local variable '{}' hides global variable.", v.name);
            }

            let fresh = generator.new_var();

            subst = v
                .infer(&context, &fresh.clone().into(), generator)?
                .compose(&subst);

            context
                .local_vars
                .insert(v.name.clone(), Into::<Type>::into(fresh.clone()).into());

            context = context.apply(&subst);
        }

        for s in &mut self.statements {
            // Btw, this was also red colored on slide 6 of typing lecture 3. This ruins example
            // examples/disallow_overloading_polymorphic.spl.
            if let Statement::FunCall(_) = s {
                let beta_t = Type::Var(generator.new_var());
                subst = s
                    .infer(&context.apply(&subst), &beta_t.apply(&subst), generator)?
                    .compose(&subst);
                context = context.apply(&subst);
            } else {
                subst = s
                    .infer(&context.apply(&subst), &beta.apply(&subst), generator)?
                    .compose(&subst);
                context = context.apply(&subst);
            }
        }

        for v in &mut self.var_decls {
            if let Some(t) = &mut v.var_type {
                *t = t.apply(&subst);
            }
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

            let infered_var_type = if let Some(t) = s.get(&v) {
                t.clone().apply(&s)
            } else {
                Into::<Type>::into(v.clone()).apply(&s)
            };

            match &self.var_type {
                // We already had a type specified by the programmer.
                // Compose substitution s2 from the infer substitution s,
                // and the unification of the infered type and specified type.
                Some(t) => {
                    let s2 = infered_var_type.mgu_specified(&t)?.compose(&s);
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

impl TypeInference for TypedExpr {
    fn infer(
        &mut self,
        context: &Context,
        expected: &Type,
        generator: &mut VarGenerator,
    ) -> Result<Subst, String> {
        let s = self.expr.infer(context, expected, generator)?;

        if let Type::Var(v) = expected {
            if let Some(t) = s.get(v) {
                // This was needed to do things as `print(bool_list.tl)`, the type var
                // would not be substituted otherwise.
                self.expr_type = Some(t.apply(&s));
            } else {
                self.expr_type = Some(expected.apply(&s));
            }
        } else {
            self.expr_type = Some(expected.apply(&s));
        }

        Ok(s)
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

                Atom::FunCall(f) => {
                    if let Type::Var(id) = expected {
                        let s = f.infer(context, expected, generator)?;

                        if s.contains_key(id) && s[id].apply(&s) == Type::Void {
                            Err(format!(
                                "Function call {} in expression would result in void type.",
                                f.name
                            ))
                        } else {
                            Ok(s)
                        }
                    } else {
                        f.infer(context, expected, generator)
                    }
                }
                Atom::Variable(v) => {
                    let s = v.infer(context, expected, generator)?;

                    v.var_type = Some(
                        context
                            .apply(&s)
                            .get_var(&v.name)
                            .ok_or(format!("Missing var from env: {}", v.name))?
                            .instantiate(generator),
                    );

                    Ok(s)
                }

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

impl Context {
    /// Generalization for new function declarations.
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

        // Resort global variables
        self.resort_vars()?;

        // Determine the SCC which must be typed together
        let scc = self.run_tarjan(&context.functions)?;

        // Infer the type of all global variables
        for v in &mut self.var_decls {
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

        // Binding time analysis and return path analysis
        for f in &mut self.fun_decls {
            if context.functions.contains_key(&f.name) {
                return Err(format!("Function {} previously declared", f.name));
            }

            let returns = f.check_returns();

            match returns {
                Returning::NoneImplicit => f.statements.push(Statement::Return(None)),
                Returning::Incomplete => {
                    return Err(format!("Function {} has a missing return", f.name))
                }
                _ => {}
            }
        }

        for component in &scc {
            let mut context_prime = context.clone();
            let mut fresh_sequence = vec![];

            let mut subst = Subst::default();

            // Add all functions to the context
            for i in component {
                let fresh = generator.new_var();
                fresh_sequence.push(fresh.clone());

                context_prime.functions.insert(
                    self.fun_decls[*i].name.clone(),
                    TypeScheme::new(vec![], fresh.into()),
                );
            }

            for (var_index, fun_index) in component.iter().enumerate() {
                let alpha: Type = fresh_sequence[var_index].clone().into();

                let s = self.fun_decls[*fun_index].infer(
                    &context_prime.apply(&subst),
                    &alpha.apply(&subst),
                    generator,
                )?;

                subst = subst.compose(&s);
            }

            context = context.apply(&subst);

            for (var_index, fun_index) in component.iter().enumerate() {
                let infered_fun_type = subst[&fresh_sequence[var_index]].clone().apply(&subst);
                let name = self.fun_decls[*fun_index].name.clone();

                match &mut self.fun_decls[*fun_index].fun_type {
                    Some(t) => {
                        let s2 = infered_fun_type.mgu_specified(&t)?.compose(&subst);
                        context.apply(&s2);

                        let generalized = context.generalize(&t.apply(&s2));
                        context.functions.insert(name, generalized);

                        self.fun_decls[*fun_index].fun_type = Some(t.apply(&s2));
                    }
                    None => {
                        context.apply(&subst);

                        let generalized = context.generalize(&infered_fun_type.apply(&subst));
                        context.functions.insert(name, generalized);

                        self.fun_decls[*fun_index].fun_type = Some(infered_fun_type.apply(&subst));
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
    builtin.load(&mut context, &mut generator);

    let fresh = generator.new_var();

    program.infer(&context, &fresh.into(), &mut generator)?;

    Ok(())
}
