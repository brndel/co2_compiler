use std::{cmp::max, collections::BTreeMap};

use chumsky::span::SimpleSpan;

use crate::{
    core::Type,
    lexer::{GetSpan, Spanned},
    program::{Program, StructDef},
    semantic::SemanticError,
    util::align_bytes,
};

#[derive(Debug, Default)]
pub struct StructNamespace<'src, T = StructInfo<'src>> {
    structs: BTreeMap<&'src str, T>,
}

#[derive(Debug, Clone)]
pub struct StructInfo<'src> {
    pub size: StructSize,
    pub fields: BTreeMap<&'src str, StructFieldInfo<'src>>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone)]
pub struct StructFieldInfo<'src> {
    pub offset: usize,
    pub ty: Type<'src>,
    pub span: SimpleSpan,
}

#[derive(Debug, Clone, Copy)]
pub struct StructSize {
    pub byte_count: usize,
    pub alignment: usize,
}

#[derive(Debug)]
pub enum StructStatus<'a, 'src> {
    Registered { def: &'a StructDef<'src> },
    CurrentlyCalculatingSize,
    Error,
    Info(StructInfo<'src>),
}

impl<'a, 'src> StructNamespace<'src, StructStatus<'a, 'src>> {
    pub fn new<Num>(
        program: &'a Program<'src, Num>,
    ) -> StructNamespace<'src, StructStatus<'a, 'src>> {
        let mut this = StructNamespace {
            structs: BTreeMap::new(),
        };

        for def in program.structs() {
            this.structs
                .insert(def.ident.0, StructStatus::Registered { def });
        }

        this
    }

    pub fn calculate_size_of(
        &mut self,
        ident: Spanned<&'src str>,
    ) -> Result<StructSize, SemanticError<'src>> {
        let size = self.calculate_size_of_inner(ident);

        if size.is_err() {
            self.structs.insert(ident.0, StructStatus::Error);
        }

        size
    }

    fn calculate_size_of_inner(
        &mut self,
        ident: Spanned<&'src str>,
    ) -> Result<StructSize, SemanticError<'src>> {
        match self.structs.get(ident.0) {
            None => Err(SemanticError::UnknownStruct { ident }),
            Some(StructStatus::Registered { .. }) => {
                let Some(StructStatus::Registered { def }) = self
                    .structs
                    .insert(ident.0, StructStatus::CurrentlyCalculatingSize)
                else {
                    unreachable!();
                };

                let info = self.get_struct_info(def)?;

                let size = info.size;

                self.structs.insert(ident.0, StructStatus::Info(info));

                Ok(size)
            }
            Some(StructStatus::CurrentlyCalculatingSize) => {
                return Err(SemanticError::RecursiveStruct { ident });
            }
            Some(StructStatus::Error) => {
                return Ok(StructSize {
                    byte_count: 0,
                    alignment: 0,
                });
            }
            Some(StructStatus::Info(info)) => return Ok(info.size),
        }
    }

    fn get_struct_info(
        &mut self,
        def: &StructDef<'src>,
    ) -> Result<StructInfo<'src>, SemanticError<'src>> {
        let mut struct_size = StructSize {
            byte_count: 0,
            alignment: 0,
        };

        let mut offsets = BTreeMap::new();

        for field in &def.fields {
            self.validate_type_name(&field.ty)?;

            let size = match field.ty.0 {
                Type::Int => StructSize {
                    byte_count: 4,
                    alignment: 4,
                },
                Type::Bool => StructSize {
                    byte_count: 1,
                    alignment: 1,
                },
                Type::Struct(ident) => self.calculate_size_of((ident, field.ty.1))?,
                Type::Pointer(_) => StructSize {
                    byte_count: 8,
                    alignment: 8,
                },
                Type::Array(_) => StructSize {
                    byte_count: 8,
                    alignment: 8,
                },
                Type::NullPtr => unreachable!("NullPtr can not be a field type"),
            };

            struct_size.byte_count = align_bytes(struct_size.byte_count, size.alignment);
            if let Some(previous_field) = offsets.insert(
                field.ident.0,
                StructFieldInfo {
                    offset: struct_size.byte_count,
                    ty: field.ty.0.clone(),
                    span: field.ident.span(),
                },
            ) {
                return Err(SemanticError::AlreadyDeclared {
                    ident: field.ident,
                    declared_at: previous_field.span,
                });
            }
            struct_size.byte_count += size.byte_count;

            struct_size.alignment = max(struct_size.alignment, size.alignment);
        }

        Ok(StructInfo {
            size: struct_size,
            fields: offsets,
            span: def.ident.1,
        })
    }

    pub fn validate_type_name(&self, ty: &Spanned<Type<'src>>) -> Result<(), SemanticError<'src>> {
        match &ty.0 {
            Type::Int => Ok(()),
            Type::Bool => Ok(()),
            Type::NullPtr => Ok(()),
            Type::Struct(ident) => {
                if self.structs.contains_key(ident) {
                    Ok(())
                } else {
                    Err(SemanticError::UnknownStruct {
                        ident: (ident, ty.1),
                    })
                }
            }
            Type::Pointer(inner_ty) => self.validate_type_name(&(*inner_ty.clone(), ty.1)),
            Type::Array(inner_ty) => self.validate_type_name(&(*inner_ty.clone(), ty.1)),
        }
    }

    pub fn finish(self) -> StructNamespace<'src> {
        let structs = self
            .structs
            .into_iter()
            .filter_map(|(ident, status)| match status {
                StructStatus::Info(info) => Some((ident, info)),
                _ => None,
            })
            .collect();

        StructNamespace { structs }
    }
}

impl<'src> StructNamespace<'src> {
    pub fn get_field_info(
        &self,
        struct_name: &'src str,
        field: Spanned<&'src str>,
    ) -> Result<Type<'src>, SemanticError<'src>> {
        let struct_info = self.structs.get(struct_name).unwrap();

        let field = struct_info
            .fields
            .get(field.0)
            .ok_or(SemanticError::UnkownField {
                struct_def: (struct_name, struct_info.span),
                field,
            })?;

        Ok(field.ty.clone())
    }
}
