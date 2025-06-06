use std::ops::Range;

use ariadne::Label;
use chumsky::span::SimpleSpan;

use crate::{
    SourceFile,
    core::Type,
    lexer::{Keyword, Spanned},
};

#[derive(Debug)]
#[must_use]
pub enum SemanticError<'a> {
    AlreadyDeclared {
        ident: Spanned<&'a str>,
        declared_at: SimpleSpan,
    },
    NotDeclared {
        ident: Spanned<&'a str>,
    },
    NotAssigned {
        ident: Spanned<&'a str>,
        declared_at: SimpleSpan,
    },
    NoReturnInFunction {
        ident: Spanned<&'a str>,
    },
    IntOverflow {
        ident: Spanned<&'a str>,
    },
    MissmatchedType {
        ty: Spanned<Type>,
        expected_type: Type,
    },
    MissmatchedBinaryType {
        a: Spanned<Type>,
        b: Spanned<Type>,
    },
    LoopControlsOutsideLoop {
        keyword: Spanned<Keyword>,
    },
    DeclareInForLoopStep {
        span: SimpleSpan,
    },
}

impl<'a> SemanticError<'a> {
    pub fn span(&self) -> &SimpleSpan {
        match self {
            SemanticError::AlreadyDeclared {
                ident,
                declared_at: _,
            } => &ident.1,
            SemanticError::NotDeclared { ident } => &ident.1,
            SemanticError::NotAssigned {
                ident,
                declared_at: _,
            } => &ident.1,
            SemanticError::NoReturnInFunction { ident } => &ident.1,
            SemanticError::IntOverflow { ident } => &ident.1,
            SemanticError::MissmatchedType {
                ty,
                expected_type: _,
            } => &ty.1,
            SemanticError::MissmatchedBinaryType { a, b: _ } => &a.1,
            SemanticError::LoopControlsOutsideLoop { keyword } => &keyword.1,
            SemanticError::DeclareInForLoopStep { span } => &span,
        }
    }

    pub fn message(&self) -> String {
        match self {
            SemanticError::AlreadyDeclared {
                ident,
                declared_at: _,
            } => {
                format!("Redeclaration of variable '{}'", ident.0)
            }
            SemanticError::NotDeclared { ident } => {
                format!("Usage of undeclared variable '{}'", ident.0)
            }
            SemanticError::NotAssigned {
                ident,
                declared_at: _,
            } => {
                format!("Usage of unassigned variable '{}'", ident.0)
            }
            SemanticError::NoReturnInFunction { ident } => {
                format!("Function '{}' has no return statement", ident.0)
            }
            SemanticError::IntOverflow { ident } => {
                format!("Int '{}' is too large", ident.0)
            }
            SemanticError::MissmatchedType { ty, expected_type } => format!(
                "Missmatched type. Found type '{}' but expected type '{}'",
                ty.0, expected_type
            ),
            SemanticError::MissmatchedBinaryType { a, b } => format!(
                "Missmatched types. Type '{}' and '{}' are different but need to be the same",
                a.0, b.0
            ),
            SemanticError::LoopControlsOutsideLoop { keyword } => {
                format!("'{}' found outside of loop", keyword.0)
            }
            SemanticError::DeclareInForLoopStep { span: _ } => {
                format!("declare statements are not allowed in for loop step")
            }
        }
    }

    pub fn labels(&self, source: SourceFile) -> Vec<Label<(String, Range<usize>)>> {
        match self {
            SemanticError::AlreadyDeclared { ident, declared_at } => {
                vec![
                    Label::new(source.span(&ident.1)).with_message(self.message()),
                    Label::new(source.span(declared_at))
                        .with_message(format!("Variable '{}' was already declared here", ident.0)),
                ]
            }
            SemanticError::NotDeclared { ident } => {
                vec![Label::new(source.span(&ident.1)).with_message(self.message())]
            }
            SemanticError::NotAssigned { ident, declared_at } => {
                vec![
                    Label::new(source.span(&ident.1)).with_message(self.message()),
                    Label::new(source.span(declared_at)).with_message(format!(
                        "Variable '{}' is declared here but not assigned",
                        ident.0
                    )),
                ]
            }
            SemanticError::NoReturnInFunction { ident } => {
                vec![Label::new(source.span(&ident.1)).with_message(self.message())]
            }
            SemanticError::IntOverflow { ident } => {
                vec![Label::new(source.span(&ident.1)).with_message(self.message())]
            }
            SemanticError::MissmatchedType {
                ty,
                expected_type: _,
            } => vec![Label::new(source.span(&ty.1)).with_message(self.message())],
            SemanticError::MissmatchedBinaryType { a, b } => {
                vec![
                    Label::new(source.span(&a.1)).with_message(format!("Has type '{}'", a.0)),
                    Label::new(source.span(&b.1)).with_message(format!("Has type '{}'", b.0)),
                ]
            }
            SemanticError::LoopControlsOutsideLoop { keyword } => {
                vec![Label::new(source.span(&keyword.1)).with_message(self.message())]
            }
            SemanticError::DeclareInForLoopStep { span } => {
                vec![Label::new(source.span(span)).with_message(self.message())]
            },
        }
    }
}
