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
    FunctionAlreadyDefined {
        ident: Spanned<&'a str>,
        defined_at: SimpleSpan,
    },
    FunctionNotDefined {
        call_ident: Spanned<&'a str>,
    },
    FunctionCallMissingArg {
        call_ident: Spanned<&'a str>,
        arg_name: &'a str,
    },
    FunctionCallTooManyArgs {
        call_ident: Spanned<&'a str>,
        arg_count: usize,
        expected_count: usize,
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
            SemanticError::FunctionAlreadyDefined {
                ident,
                defined_at: _,
            } => &ident.1,
            SemanticError::FunctionNotDefined { call_ident } => &call_ident.1,
            SemanticError::FunctionCallMissingArg {
                call_ident,
                arg_name: _,
            } => &call_ident.1,
            SemanticError::FunctionCallTooManyArgs {
                call_ident,
                arg_count: _,
                expected_count: _,
            } => &call_ident.1,
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
            SemanticError::FunctionAlreadyDefined {
                ident,
                defined_at: _,
            } => format!("Function '{}' is already defined", ident.0),
            SemanticError::FunctionNotDefined { call_ident } => {
                format!("Function '{}' is not defined", call_ident.0)
            }
            SemanticError::FunctionCallMissingArg {
                call_ident,
                arg_name,
            } => format!(
                "Call to function '{}' is missing arg '{}'",
                call_ident.0, arg_name
            ),
            SemanticError::FunctionCallTooManyArgs {
                call_ident,
                arg_count,
                expected_count,
            } => format!(
                "Call to function '{}' has {} args defined, expected {}",
                call_ident.0, arg_count, expected_count
            ),
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
            }
            SemanticError::FunctionAlreadyDefined { ident, defined_at } => {
                vec![
                    Label::new(source.span(&ident.1)).with_message(self.message()),
                    Label::new(source.span(defined_at))
                        .with_message(format!("Function '{}' already defined here", ident.0)),
                ]
            }
            SemanticError::FunctionNotDefined { call_ident } => {
                vec![Label::new(source.span(&call_ident.1)).with_message(self.message())]
            }
            SemanticError::FunctionCallMissingArg {
                call_ident,
                arg_name: _,
            } => vec![Label::new(source.span(&call_ident.1)).with_message(self.message())],
            SemanticError::FunctionCallTooManyArgs {
                call_ident,
                arg_count: _,
                expected_count: _,
            } => vec![Label::new(source.span(&call_ident.1)).with_message(self.message())],
        }
    }
}
