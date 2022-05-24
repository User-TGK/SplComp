pub mod pp;

use num_bigint::BigUint;

use std::fmt;
use std::ops::{Deref, DerefMut};

#[derive(PartialEq, Debug)]
pub struct Program {
    pub var_decls: Vec<VarDecl>,
    pub fun_decls: Vec<FunDecl>,
}

#[derive(PartialEq, Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(pub String);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Id {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl Deref for Id {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl DerefMut for Id {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

#[derive(PartialEq, Debug)]
pub struct VarDecl {
    pub var_type: Option<Type>,
    pub name: Id,
    pub value: TypedExpr,
}

#[derive(PartialEq, Debug)]
pub struct FunDecl {
    pub name: Id,
    pub params: Vec<Id>,
    pub fun_type: Option<Type>,
    pub var_decls: Vec<VarDecl>,
    pub statements: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Bool,
    Char,
    Void,
    Function(Vec<Type>, Box<Type>),
    Tuple(Box<Type>, Box<Type>),
    List(Box<Type>),
    Var(Id),
}

impl Type {
    pub fn is_composite(&self) -> bool {
        match self {
            Type::List(_) | Type::Tuple(_, _) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    If(If),
    While(While),
    Assign(Assign),
    FunCall(FunCall),
    Return(Option<TypedExpr>),
}

#[derive(PartialEq, Debug)]
pub struct If {
    pub cond: TypedExpr,
    pub if_true: Vec<Statement>,
    pub if_false: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct While {
    pub cond: TypedExpr,
    pub body: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct Assign {
    pub target: Variable,
    pub value: TypedExpr,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunCall {
    pub name: Id,
    pub args: Vec<TypedExpr>,
    pub fun_type: Option<Type>,
}

impl FunCall {
    pub fn new(name: Id, args: Vec<TypedExpr>) -> Self {
        Self {
            name,
            args,
            fun_type: None,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypedExpr {
    pub expr_type: Option<Type>,
    pub expr: Expr,
}

impl From<Expr> for TypedExpr {
    fn from(expr: Expr) -> Self {
        Self {
            expr_type: None,
            expr,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Or(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Equals(Box<Expr>, Box<Expr>),
    NotEquals(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Gt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    UnaryMinus(Box<Expr>),
    Not(Box<Expr>),
    Atom(Atom),
}

impl Expr {
    pub fn precedence(&self) -> i32 {
        match self {
            Expr::Or(..) => 2,
            Expr::And(..) => 3,
            Expr::Equals(..)
            | Expr::NotEquals(..)
            | Expr::Lt(..)
            | Expr::Le(..)
            | Expr::Gt(..)
            | Expr::Ge(..) => 4,
            Expr::Cons(..) => 5,
            Expr::Add(..) | Expr::Sub(..) => 6,
            Expr::Mul(..) | Expr::Div(..) | Expr::Mod(..) => 7,
            Expr::UnaryMinus(..) | Expr::Not(..) => 8,
            Expr::Atom(..) => 9,
        }
    }

    pub fn should_be_paranthesized(&self, parent_precedence: i32) -> bool {
        self.precedence() < parent_precedence
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Atom {
    IntLiteral(BigUint),
    BoolLiteral(bool),
    CharLiteral(char),
    StringLiteral(String),
    FunCall(FunCall),
    Variable(Variable),
    EmptyList,
    Tuple(Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Variable {
    pub(crate) var_type: Option<Type>,
    pub(crate) name: Id,
    pub(crate) fields: Vec<Field>,
}

impl Variable {
    pub fn new(var_type: Option<Type>, name: Id, fields: Vec<Field>) -> Self {
        Self {
            var_type,
            name,
            fields,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Field {
    Hd,
    Tl,
    Fst,
    Snd,
}

impl Field {
    pub fn builtin_identifier(&self) -> Id {
        String::from(match self {
            Field::Hd => "hd",
            Field::Tl => "tl",
            Field::Fst => "fst",
            Field::Snd => "snd",
        })
        .into()
    }
}
