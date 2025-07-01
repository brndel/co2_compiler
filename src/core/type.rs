use std::fmt::Display;


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'src> {
    Int,
    Bool,
    Struct(&'src str),
    Pointer(Box<Self>),
    Array(Box<Self>),
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Struct(ident) => write!(f, "{}", ident),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Array(ty) => write!(f, "{}[]", ty),
        }
    }
}
