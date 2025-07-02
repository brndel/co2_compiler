use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'src> {
    Int,
    Bool,
    Struct(&'src str),
    Pointer(Box<Self>),
    Array(Box<Self>),
    NullPtr,
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Struct(ident) => write!(f, "{}", ident),
            Type::Pointer(ty) => write!(f, "*{}", ty),
            Type::Array(ty) => write!(f, "{}[]", ty),
            Type::NullPtr => write!(f, "NULL"),
        }
    }
}

impl<'src> Type<'src> {
    pub fn is_big_type(&self) -> bool {
        match self {
            Type::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_ptr(&self) -> bool {
        match self {
            Type::Pointer(_) => true,
            _ => false,
        }
    }

    pub fn can_assign(&self, other: &Self) -> bool {
        self == other || self.is_ptr() && other == &Type::NullPtr
    }
}
