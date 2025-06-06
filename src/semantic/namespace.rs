use std::collections::{BTreeMap, BTreeSet, btree_map::Keys};

use chumsky::span::SimpleSpan;

use crate::{core::Type, lexer::Spanned};

use super::SemanticError;

pub struct Namespace<'src, 'parent, T = VariableStatus> {
    parent: Option<&'parent Self>,
    variables: BTreeMap<&'src str, T>,
    local_assigned_variables: BTreeSet<&'src str>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct VariableStatus {
    declared_at: SimpleSpan,
    ty: Type,
    is_assigned: bool,
}

// #[derive(Debug, Clone)]
// pub enum LocalAssignedVariables<'a> {
//     Everything,
//     Vars(BTreeSet<&'a str>),
// }

// impl<'a> LocalAssignedVariables<'a> {
//     pub fn new() -> Self {
//         Self::Vars(BTreeSet::new())
//     }

//     fn insert(&mut self, var: &'a str) {
//         match self {
//             LocalAssignedVariables::Everything => (),
//             LocalAssignedVariables::Vars(btree_set) => {
//                 btree_set.insert(var);
//             }
//         }
//     }

//     fn contains(&self, var: &'a str) -> bool {
//         match self {
//             LocalAssignedVariables::Everything => true,
//             LocalAssignedVariables::Vars(btree_set) => btree_set.contains(var),
//         }
//     }

//     fn assign_everything(&mut self) {
//         *self = Self::Everything
//     }

//     pub fn intersection(self, other: Self) -> Self {
//         match (self, other) {
//             (LocalAssignedVariables::Everything, LocalAssignedVariables::Everything) => {
//                 LocalAssignedVariables::Everything
//             }
//             (LocalAssignedVariables::Everything, vars)
//             | (vars, LocalAssignedVariables::Everything) => vars,
//             (LocalAssignedVariables::Vars(a), LocalAssignedVariables::Vars(b)) => {
//                 Self::Vars(a.intersection(&b).cloned().collect())
//             }
//         }
//     }
// }

impl<'src, 'parent, T> Namespace<'src, 'parent, T> {
    pub fn new() -> Self {
        Self {
            parent: None,
            variables: BTreeMap::new(),
            local_assigned_variables: BTreeSet::new(),
        }
    }

    pub fn new_child<'a: 'parent>(&'a self) -> Namespace<'src, 'a, T> {
        Self {
            parent: Some(&self),
            variables: BTreeMap::new(),
            local_assigned_variables: BTreeSet::new(),
        }
    }

    pub fn with_parent<'a: 'parent>(parent: Option<&'parent Namespace<'src, 'a, T>>) -> Self {
        Self {
            parent,
            variables: BTreeMap::new(),
            local_assigned_variables: BTreeSet::new(),
        }
    }
}

impl<'src, 'parent> Namespace<'src, 'parent, VariableStatus> {
    fn get_var(&self, ident: &str) -> Option<&VariableStatus> {
        if let Some(parent) = &self.parent {
            if let Some(var) = parent.get_var(ident) {
                return Some(var);
            }
        }

        self.variables.get(ident)
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
                    is_assigned: assign,
                },
            );
            Ok(())
        }
    }

    pub fn assign(
        &mut self,
        ident: Spanned<&'src str>,
        ty: Spanned<Type>,
    ) -> Result<(), SemanticError<'src>> {
        let var_ty;

        if let Some(status) = self.variables.get_mut(ident.0) {
            status.is_assigned = true;
            var_ty = status.ty;
        } else if let Some(status) = self.get_var(ident.0) {
            var_ty = status.ty;

            if !status.is_assigned {
                self.local_assigned_variables.insert(ident.0);
            }
        } else {
            return Err(SemanticError::NotDeclared { ident });
        }

        if var_ty == ty.0 {
            Ok(())
        } else {
            Err(SemanticError::MissmatchedType {
                ty,
                expected_type: var_ty,
            })
        }
    }

    pub fn assign_everything(&mut self) {
        for (_, status) in &mut self.variables {
            status.is_assigned = true;
        }

        if let Some(parent) = &self.parent {
            self.local_assigned_variables
                .extend(parent.get_declared_vars());
        }
    }

    fn get_declared_vars(&self) -> impl Iterator<Item = &'src str> {
        NamespaceVariableIter::new(self)
    }

    fn set_assigned_raw(&mut self, ident: &'src str) {
        if let Some(status) = self.variables.get_mut(ident) {
            status.is_assigned = true;
        } else if let Some(status) = self.get_var(ident) {
            if !status.is_assigned {
                self.local_assigned_variables.insert(ident);
            }
        }
    }

    pub fn is_assigned(&self, ident: Spanned<&'src str>) -> Result<(), SemanticError<'src>> {
        if let Some(parent) = &self.parent {
            let result = parent.is_assigned(ident);
            if result.is_ok() {
                return result;
            }
        }

        if self.local_assigned_variables.contains(ident.0) {
            return Ok(());
        }

        if let Some(status) = self.variables.get(ident.0) {
            if status.is_assigned {
                Ok(())
            } else {
                Err(SemanticError::NotAssigned {
                    ident,
                    declared_at: status.declared_at,
                })
            }
        } else {
            Err(SemanticError::NotDeclared { ident })
        }
    }

    pub fn get_type(&self, ident: Spanned<&'src str>) -> Result<Type, SemanticError<'src>> {
        match self.get_var(ident.0) {
            Some(VariableStatus {
                declared_at: _,
                ty,
                is_assigned: _,
            }) => Ok(*ty),
            None => Err(SemanticError::NotDeclared { ident }),
        }
    }

    pub fn into_local_assigned_variables(self) -> BTreeSet<&'src str> {
        self.local_assigned_variables
    }

    pub fn local_assigned_variables(&self) -> &BTreeSet<&'src str> {
        &self.local_assigned_variables
    }

    pub fn assign_variable_set(&mut self, variables: impl IntoIterator<Item = &'src str>) {
        for ident in variables {
            self.set_assigned_raw(ident);
        }
    }
}

struct NamespaceVariableIter<'parent, 'src> {
    namespace: &'parent Namespace<'src, 'parent>,
    iter: Keys<'parent, &'src str, VariableStatus>,
}

impl<'parent, 'src> NamespaceVariableIter<'parent, 'src> {
    pub fn new(namespace: &'parent Namespace<'src, 'parent>) -> Self {
        Self {
            namespace,
            iter: namespace.variables.keys(),
        }
    }
}

impl<'parent, 'src> Iterator for NamespaceVariableIter<'parent, 'src> {
    type Item = &'src str;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(item) => Some(item),
            None => match self.namespace.parent {
                Some(parent) => {
                    self.namespace = parent;
                    self.iter = self.namespace.variables.keys();

                    self.next()
                }
                None => None,
            },
        }
    }
}
