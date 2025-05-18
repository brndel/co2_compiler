use std::ops::Range;

use ariadne::Label;
use chumsky::span::SimpleSpan;

use crate::{SourceFile, lexer::Spanned};

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
        }
    }
}
