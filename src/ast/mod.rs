pub mod pp;

use num_bigint::BigUint;

use std::ops::{Deref, DerefMut};

#[derive(PartialEq, Debug)]
pub struct Program(pub Vec<Decl>);

impl Deref for Program {
    type Target = Vec<Decl>;
    fn deref(&self) -> &Vec<Decl> {
        &self.0
    }
}

impl DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Vec<Decl> {
        &mut self.0
    }
}

#[derive(PartialEq, Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Id(pub String);

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
    pub value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct FunDecl {
    pub name: Id,
    pub params: Vec<Id>,
    pub fun_type: Option<Type>,
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

#[derive(PartialEq, Debug)]
pub enum Statement {
    If(If),
    While(While),
    VarDecl(VarDecl),
    Assign(Assign),
    FunCall(FunCall),
    Return(Option<Expr>),
}

#[derive(PartialEq, Debug)]
pub struct If {
    pub cond: Expr,
    pub if_true: Vec<Statement>,
    pub if_false: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct While {
    pub cond: Expr,
    pub body: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct Assign {
    pub target: Variable,
    pub value: Expr,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunCall {
    pub(crate) name: Id,
    pub(crate) args: Vec<Expr>,
}

impl FunCall {
    pub fn new(name: Id, args: Vec<Expr>) -> Self {
        Self { name, args }
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
    pub(crate) name: Id,
    pub(crate) fields: Vec<Field>,
}

impl Variable {
    pub fn new(name: Id, fields: Vec<Field>) -> Self {
        Self { name, fields }
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
