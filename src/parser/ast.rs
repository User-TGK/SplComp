#[derive(PartialEq, Debug)]
pub struct Program(Vec<Decl>);

#[derive(PartialEq, Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Id(pub String);

#[derive(PartialEq, Debug)]
pub struct VarDecl {
    var_type: Option<Type>,
    name: Id,
    value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct FunDecl {
    name: Id,
    params: Vec<Id>,
    fun_type: Option<FunType>,
    var_decls: Vec<VarDecl>,
    statements: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct FunType {
    param_types: Vec<Type>,
    return_type: ReturnType,
}

#[derive(PartialEq, Debug)]
pub enum ReturnType {
    Type(Type),
    Void,
}

#[derive(PartialEq, Debug)]
pub enum Type {
    Basic(BasicType),
    Tuple(Box<Type>, Box<Type>),
    Array(Box<Type>),
}

#[derive(PartialEq, Debug)]
pub enum BasicType {
    Int,
    Bool,
    Char,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    If(If),
    While(While),
    Assign(Assign),
    FunCall(FunCall),
    Return(Return),
}

#[derive(PartialEq, Debug)]
pub struct If {
    cond: Expr,
    if_true: Vec<Statement>,
    if_false: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct While {
    cond: Expr,
    body: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub struct Assign {
    target: Variable,
    value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct FunCall {
    name: Id,
    args: Vec<Expr>,
}

impl FunCall {
    pub fn new(name: Id, args: Vec<Expr>) -> Self {
        Self { name, args }
    }
}

#[derive(PartialEq, Debug)]
pub struct Return {
    value: Option<Expr>,
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub enum Atom {
    IntLiteral(i64),
    BoolLiteral(bool),
    CharLiteral(char),
    FunCall(FunCall),
    Variable(Variable),
    EmptyList,
    Tuple(Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub struct Variable {
    name: Id,
    fields: Vec<Field>,
}

impl Variable {
    pub fn new(name: Id, fields: Vec<Field>) -> Self {
        Self { name, fields }
    }
}

#[derive(PartialEq, Debug)]
pub enum Field {
    Hd,
    Tl,
    Fst,
    Snd,
}
