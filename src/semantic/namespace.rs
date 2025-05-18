use std::collections::BTreeMap;

use chumsky::span::SimpleSpan;

use crate::lexer::Spanned;

use super::SemanticError;

pub struct Namespace<'a, T = VariableStatus> {
    variables: BTreeMap<&'a str, T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableStatus {
    declared_at: SimpleSpan,
    assigned_at: Option<SimpleSpan>,
}

impl<'a, T> Namespace<'a, T> {
    pub fn new() -> Self {
        Self {
            variables: BTreeMap::new(),
        }
    }
}

impl<'a> Namespace<'a, VariableStatus> {
    pub fn declare(&mut self, ident: Spanned<&'a str>, assign: bool) -> Result<(), SemanticError<'a>> {
        if let Some(staus) = self.variables.get(ident.0) {
            Err(SemanticError::AlreadyDeclared {
                ident,
                declared_at: staus.declared_at,
            })
        } else {
            self.variables.insert(
                ident.0,
                VariableStatus {
                    declared_at: ident.1,
                    assigned_at: assign.then_some(ident.1),
                },
            );
            Ok(())
        }
    }

    pub fn assign(&mut self, ident: Spanned<&'a str>) -> Result<(), SemanticError<'a>> {
        if let Some(status) = self.variables.get_mut(ident.0) {
            if status.assigned_at.is_none() {
                status.assigned_at = Some(ident.1);
            }
            Ok(())
        } else {
            Err(SemanticError::NotDeclared { ident })
        }
    }

    pub fn is_assigned(&self, ident: Spanned<&'a str>) -> Result<(), SemanticError<'a>> {
        match self.variables.get(ident.0) {
            Some(VariableStatus {
                declared_at,
                assigned_at: None,
            }) => Err(SemanticError::NotAssigned { ident, declared_at: *declared_at }),
            Some(VariableStatus {
                declared_at: _,
                assigned_at: Some(_),
            }) => Ok(()),
            None => Err(SemanticError::NotDeclared { ident }),
        }
    }
}
