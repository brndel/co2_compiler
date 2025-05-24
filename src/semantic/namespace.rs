use std::collections::BTreeMap;

use chumsky::span::SimpleSpan;

use crate::{lexer::Spanned, parser::Type};

use super::SemanticError;

pub struct Namespace<'src, T = VariableStatus> {
    parent: Option<Box<Self>>,
    variables: BTreeMap<&'src str, T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableStatus {
    declared_at: SimpleSpan,
    ty: Type,
    assigned_at: Option<SimpleSpan>,
}

impl<'src, T> Namespace<'src, T> {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: BTreeMap::new(),
        }
    }

    pub fn new_child(self) -> Self {
        Self {
            parent: Some(Box::new(self)),
            variables: BTreeMap::new(),
        }
    }

    pub fn parent(self) -> Option<Self> {
        self.parent.map(|parent| *parent)
    }
}

impl<'src> Namespace<'src, VariableStatus> {
    fn get_var(&self, ident: &str) -> Option<&VariableStatus> {
        if let Some(parent) = &self.parent {
            if let Some(var) = parent.get_var(ident) {
                return Some(var);
            }
        }

        self.variables.get(ident)
    }

    fn get_var_mut(&mut self, ident: &str) -> Option<&mut VariableStatus> {
        if let Some(parent) = &mut self.parent {
            if let Some(var) = parent.get_var_mut(ident) {
                return Some(var);
            }
        }

        self.variables.get_mut(ident)
    }

    pub fn declare(
        &mut self,
        ident: Spanned<&'src str>,
        ty: Type,
        assign: bool,
    ) -> Result<(), SemanticError<'src>> {
        if let Some(staus) = self.get_var(ident.0) {
            Err(SemanticError::AlreadyDeclared {
                ident,
                declared_at: staus.declared_at,
            })
        } else {
            self.variables.insert(
                ident.0,
                VariableStatus {
                    declared_at: ident.1,
                    ty,
                    assigned_at: assign.then_some(ident.1),
                },
            );
            Ok(())
        }
    }

    pub fn assign(&mut self, ident: Spanned<&'src str>, ty: Spanned<Type>) -> Result<(), SemanticError<'src>> {
        if let Some(status) = self.get_var_mut(ident.0) {
            if status.assigned_at.is_none() {
                status.assigned_at = Some(ident.1);
            }
            
            if status.ty == ty.0 {
                Ok(())
            } else {
                Err(SemanticError::MissmatchedType { ty, expected_type: status.ty })
            }
        } else {
            Err(SemanticError::NotDeclared { ident })
        }
    }

    pub fn is_assigned(&self, ident: Spanned<&'src str>) -> Result<(), SemanticError<'src>> {
        match self.get_var(ident.0) {
            Some(VariableStatus {
                declared_at,
                ty: _,
                assigned_at: None,
            }) => Err(SemanticError::NotAssigned {
                ident,
                declared_at: *declared_at,
            }),
            Some(VariableStatus {
                declared_at: _,
                ty: _,
                assigned_at: Some(_),
            }) => Ok(()),
            None => Err(SemanticError::NotDeclared { ident }),
        }
    }

    pub fn get_type(&self, ident: Spanned<&'src str>) -> Result<Type, SemanticError<'src>> {
        match self.get_var(ident.0) {
            Some(VariableStatus {
                declared_at: _,
                ty,
                assigned_at: None,
            }) => Ok(*ty),
            Some(VariableStatus {
                declared_at: _,
                ty,
                assigned_at: Some(_),
            }) => Ok(*ty),
            None => Err(SemanticError::NotDeclared { ident }),
        }
    }
}
