use num_bigint::BigUint;

#[derive(PartialEq, Debug)]
pub struct Program(pub Vec<Decl>);

#[derive(PartialEq, Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Id(pub String);

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
    pub fun_type: Option<FunType>,
    pub var_decls: Vec<VarDecl>,
    pub statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct FunType {
    pub param_types: Vec<Type>,
    pub return_type: ReturnType,
}

#[derive(PartialEq, Debug)]
pub enum ReturnType {
    Type(Type),
    Void,
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Int,
    Bool,
    Char,
    Tuple(Box<Type>, Box<Type>),
    Array(Box<Type>),
    Generic(Id),
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    If(If),
    While(While),
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
