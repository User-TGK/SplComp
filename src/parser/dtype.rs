pub enum DType {
    Int,
    Bool,
    Char,
    String,
    Function(Box<DType>, Box<DType>),
    Tuple(Box<DType>, Box<DType>),
    Array(Box<DType>),
    Generic(String),
}
