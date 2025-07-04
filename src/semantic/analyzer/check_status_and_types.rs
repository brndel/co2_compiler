use chumsky::span::Span;

use crate::{
    core::Type,
    lexer::{BinaryOperator, GetSpan, Spanned, UnaryOperator},
    parser::{Block, Expression, FunctionCall, Lvalue, Ptr, Statement},
    program::{FunctionDef, Program},
    semantic::{
        Namespace, SemanticError, StructFieldInfo,
        namespace::{FunctionNamespace, StructNamespace},
    },
};

pub fn check_status_and_types<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    program: &Program<'src, Num>,
    functions: &FunctionNamespace<'src>,
    structs: &StructNamespace<'src>,
) where
    Num: GetSpan,
{
    for func in program.functions() {
        let mut namespace = Namespace::new();

        for param in &func.params {
            if let Err(err) = namespace.declare(param.name, param.ty.0.clone(), true) {
                errors.push(err);
            }
        }

        validate_block(
            errors,
            &func.block,
            Some(&namespace),
            &functions,
            func,
            structs,
        );
    }
}

fn validate_block<'src, 'ns, 'program, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    block: &Block<'src, Num>,
    namespace: Option<&'ns Namespace<'src, '_>>,
    functions: &FunctionNamespace<'src>,
    current_function: &FunctionDef<'src, Num>,
    structs: &StructNamespace<'src>,
) -> Namespace<'src, 'ns>
where
    Num: GetSpan,
{
    let mut namespace = Namespace::with_parent(namespace);

    for statement in &block.statements {
        validate_statement(
            errors,
            statement,
            &mut namespace,
            functions,
            current_function,
            structs,
        )
    }

    namespace
}

fn validate_statement<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    statement: &Statement<'src, Num>,
    namespace: &mut Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
    current_function: &FunctionDef<'src, Num>,
    structs: &StructNamespace<'src>,
) where
    Num: GetSpan,
{
    match statement {
        Statement::Declaration { ty, ident, value } => {
            if ty.0.is_big_type() {
                errors.push(SemanticError::DisallowedBigType { ty: ty.clone() });
                return;
            }

            if let Some(value) = value {
                if let Some(value_ty) =
                    validate_expression(errors, value, &namespace, functions, structs)
                {
                    if !ty.0.can_assign(&value_ty.0) {
                        errors.push(SemanticError::MissmatchedType {
                            ty: value_ty,
                            expected_type: ty.0.clone(),
                        });
                    }
                };
            }

            if let Err(err) = namespace.declare(*ident, ty.0.clone(), value.is_some()) {
                errors.push(err);
            }
        }
        Statement::Assignment { lvalue, op, value } => {
            let value_ty = validate_expression(errors, value, &namespace, functions, structs);

            let lvalue_ty = validate_lvalue(errors, lvalue, &namespace, functions, structs);

            let (Some(value_ty), Some(lvalue_ty)) = (value_ty, lvalue_ty) else {
                return;
            };

            let ident = lvalue.ident();

            if lvalue_ty.0.is_big_type() {
                errors.push(SemanticError::DisallowedBigType { ty: lvalue_ty });
                return;
            }

            if op.is_some() {
                if let Err(err) = namespace.is_assigned(ident) {
                    errors.push(err);
                }

                check_binary_type(
                    errors,
                    lvalue_ty,
                    value_ty.clone(),
                    Some(Type::Int),
                    Type::Int,
                );
            } else {
                if !lvalue_ty.0.can_assign(&value_ty.0) {
                    errors.push(SemanticError::MissmatchedType {
                        ty: value_ty.clone(),
                        expected_type: lvalue_ty.0,
                    });
                }
            }

            if let Err(err) = namespace.assign(ident) {
                errors.push(err);
            }
        }
        Statement::FunctionCall(fn_call) => {
            validate_function_call(errors, fn_call, namespace, functions, structs);
        }
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            if let Some(condition_ty) =
                validate_expression(errors, condition, &namespace, functions, structs)
            {
                if condition_ty.0 != Type::Bool {
                    errors.push(SemanticError::MissmatchedType {
                        ty: condition_ty,
                        expected_type: Type::Bool,
                    });
                }
            };

            let mut then_namespace = Namespace::with_parent(Some(namespace));
            validate_statement(
                errors,
                then,
                &mut then_namespace,
                functions,
                current_function,
                structs,
            );

            if let Some(r#else) = r#else {
                let mut else_namespace = Namespace::with_parent(Some(namespace));
                validate_statement(
                    errors,
                    r#else,
                    &mut else_namespace,
                    functions,
                    current_function,
                    structs,
                );

                let then_vars = then_namespace.into_local_assigned_variables();
                let else_vars = else_namespace.into_local_assigned_variables();

                let combined_assignments = then_vars.intersection(&else_vars).cloned();

                namespace.assign_variable_set(combined_assignments);
            }
        }
        Statement::While {
            condition,
            body: then,
        } => {
            if let Some(condition_ty) =
                validate_expression(errors, condition, &namespace, functions, structs)
            {
                if condition_ty.0 != Type::Bool {
                    errors.push(SemanticError::MissmatchedType {
                        ty: condition_ty,
                        expected_type: Type::Bool,
                    });
                }
            };

            let mut inner = Namespace::with_parent(Some(namespace));
            validate_statement(
                errors,
                then,
                &mut inner,
                functions,
                current_function,
                structs,
            );
        }
        Statement::For {
            init,
            condition,
            step,
            body: then,
        } => {
            let mut init_vars = None;
            let mut loop_namespace = namespace.new_child();

            if let Some(init) = init {
                validate_statement(
                    errors,
                    init,
                    &mut loop_namespace,
                    functions,
                    current_function,
                    structs,
                );
                let local_vars = loop_namespace.local_assigned_variables().clone();
                init_vars = Some(local_vars);
            }
            validate_expression(errors, condition, &mut loop_namespace, functions, structs);

            let mut inner = loop_namespace.new_child();
            validate_statement(
                errors,
                &then,
                &mut inner,
                functions,
                current_function,
                structs,
            );
            loop_namespace.assign_variable_set(inner.local_assigned_variables().clone());

            if let Some(step) = step {
                validate_statement(
                    errors,
                    step,
                    &mut loop_namespace,
                    functions,
                    current_function,
                    structs,
                );
            }

            if let Some(init_vars) = init_vars {
                namespace.assign_variable_set(init_vars);
            }
        }
        Statement::Return { value: expr } => {
            if let Some(expr_ty) = validate_expression(errors, expr, &namespace, functions, structs)
            {
                let return_ty = current_function.return_type.0.clone();
                if expr_ty.0 != return_ty {
                    errors.push(SemanticError::MissmatchedType {
                        ty: expr_ty,
                        expected_type: return_ty,
                    });
                }
            };
            namespace.assign_everything();
        }
        Statement::Break(_) | Statement::Continue(_) => namespace.assign_everything(),
        Statement::Block(block) => {
            let inner = validate_block(
                errors,
                block,
                Some(namespace),
                functions,
                current_function,
                structs,
            );

            let assigned_vars = inner.into_local_assigned_variables();

            namespace.assign_variable_set(assigned_vars);
        }
    }
}

fn validate_lvalue<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    lvalue: &Lvalue<'src, Num>,
    namespace: &Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
    structs: &StructNamespace<'src>,
) -> Option<Spanned<Type<'src>>>
where
    Num: GetSpan,
{
    match lvalue {
        Lvalue::Ident(ident) => match namespace.get_type(*ident) {
            Ok(lvalue) => Some((lvalue, ident.1)),
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Lvalue::Ptr {
            lvalue,
            ptr,
            size_hint,
        } => {
            let ty = validate_lvalue(errors, &lvalue, namespace, functions, structs)?;

            let (return_ty, hint) = validate_ptr(errors, namespace, functions, structs, ptr, ty)?;

            *size_hint.borrow_mut() = Some(hint);

            Some(return_ty)
        }
    }
}

fn validate_expression<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    expression: &Expression<'src, Num>,
    namespace: &Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
    structs: &StructNamespace<'src>,
) -> Option<Spanned<Type<'src>>>
where
    Num: GetSpan,
{
    match expression {
        Expression::Ident(ident) => {
            if let Err(err) = namespace.is_assigned(*ident) {
                errors.push(err);
            }

            let ty = match namespace.get_type(*ident) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

            Some((ty, ident.1))
        }
        Expression::Num(value) => Some((Type::Int, value.span())),
        Expression::Bool(value) => Some((Type::Bool, value.span())),
        Expression::Binary { a, op, b } => {
            let a_ty = validate_expression(errors, &a, namespace, functions, structs);
            let b_ty = validate_expression(errors, &b, namespace, functions, structs);

            let a = a_ty?;
            let b = b_ty?;

            match op.0 {
                BinaryOperator::Plus
                | BinaryOperator::Minus
                | BinaryOperator::Mul
                | BinaryOperator::Div
                | BinaryOperator::Mod
                | BinaryOperator::BitAnd
                | BinaryOperator::BitOr
                | BinaryOperator::BitXor
                | BinaryOperator::ShiftLeft
                | BinaryOperator::ShiftRight => {
                    check_binary_type(errors, a, b, Some(Type::Int), Type::Int)
                }
                BinaryOperator::LogicAnd | BinaryOperator::LogicOr => {
                    check_binary_type(errors, a, b, Some(Type::Bool), Type::Bool)
                }
                BinaryOperator::Less
                | BinaryOperator::LessEq
                | BinaryOperator::Greater
                | BinaryOperator::GreaterEq => {
                    check_binary_type(errors, a, b, Some(Type::Int), Type::Bool)
                }
                BinaryOperator::Equals | BinaryOperator::NotEquals => {
                    check_binary_type(errors, a, b, None, Type::Bool)
                }
            }
        }
        Expression::Unary { op, expr } => {
            let ty = validate_expression(errors, expr, namespace, functions, structs)?;
            match op.0 {
                UnaryOperator::Minus | UnaryOperator::BitNot => {
                    if ty.0 == Type::Int {
                        Some(ty)
                    } else {
                        errors.push(SemanticError::MissmatchedType {
                            ty,
                            expected_type: Type::Int,
                        });
                        None
                    }
                }
                UnaryOperator::LogicNot => {
                    if ty.0 == Type::Bool {
                        Some(ty)
                    } else {
                        errors.push(SemanticError::MissmatchedType {
                            ty,
                            expected_type: Type::Bool,
                        });
                        None
                    }
                }
            }
        }
        Expression::Ternary { condition, a, b } => {
            let condition_ty =
                validate_expression(errors, condition, namespace, functions, structs);
            let a_ty = validate_expression(errors, a, namespace, functions, structs);
            let b_ty = validate_expression(errors, b, namespace, functions, structs);

            let condition = condition_ty?;
            let a = a_ty?;
            let b = b_ty?;

            if condition.0 != Type::Bool {
                errors.push(SemanticError::MissmatchedType {
                    ty: condition,
                    expected_type: Type::Bool,
                });
                None
            } else if a.0 != b.0 {
                errors.push(SemanticError::MissmatchedBinaryType { a, b });
                None
            } else {
                Some((a.0, condition.1.union(a.1).union(b.1)))
            }
        }
        Expression::FunctionCall(fn_call) => {
            validate_function_call(errors, fn_call, namespace, functions, structs)
        }
        Expression::NullPtr(simple_span) => Some((Type::NullPtr, *simple_span)),
        Expression::Access {
            expr,
            ptr,
            size_hint,
        } => {
            let ty = validate_expression(errors, &expr, namespace, functions, structs)?;

            let (return_ty, hint) = validate_ptr(errors, namespace, functions, structs, ptr, ty)?;

            *size_hint.borrow_mut() = Some(hint);

            Some(return_ty)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SizeHint {
    pub offset: usize,
    pub size: usize,
}

impl SizeHint {
    pub fn new(info: &StructFieldInfo, structs: &StructNamespace) -> Self {
        SizeHint {
            offset: info.offset,
            size: structs.get_size(&info.ty).byte_count,
        }
    }
}

fn validate_ptr<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    namespace: &Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
    structs: &StructNamespace<'src>,
    ptr: &Ptr<'src, Num>,
    ty: Spanned<Type<'src>>,
) -> Option<(Spanned<Type<'src>>, SizeHint)>
where
    Num: GetSpan,
{
    match (ptr, &ty.0) {
        (Ptr::FieldAccess { ident }, Type::Struct(struct_name)) => {
            let info = match structs.get_field_info(struct_name, *ident) {
                Ok(ty) => ty,
                Err(err) => {
                    errors.push(err);
                    return None;
                }
            };

            let size_hint = SizeHint::new(&info, structs);
            return Some(((info.ty, ident.1), size_hint));
        }
        (Ptr::FieldAccess { ident }, _) => {
            errors.push(SemanticError::FieldAccessOnNonStruct { ty, field: *ident });
            return None;
        }
        (Ptr::PtrFieldAccess { ident }, Type::Pointer(inner_ty)) => {
            if let Type::Struct(struct_name) = inner_ty.as_ref() {
                let info = match structs.get_field_info(struct_name, *ident) {
                    Ok(ty) => ty,
                    Err(err) => {
                        errors.push(err);
                        return None;
                    }
                };

                let size_hint = SizeHint::new(&info, structs);
                Some(((info.ty, ident.1), size_hint))
            } else {
                errors.push(SemanticError::PtrFieldAccessOnNonPtr { ty, field: *ident });
                None
            }
        }
        (Ptr::PtrFieldAccess { ident }, Type::NullPtr) => {
            errors.push(SemanticError::NullPtrDeref { span: ident.1 });
            None
        }
        (Ptr::PtrFieldAccess { ident }, _) => {
            errors.push(SemanticError::PtrFieldAccessOnNonPtr { ty, field: *ident });
            None
        }
        (Ptr::ArrayAccess { index }, Type::Array(inner_ty)) => {
            let index_ty = validate_expression(errors, &index, namespace, functions, structs)?;

            if index_ty.0 == Type::Int {
                let inner_ty_size = structs.get_size(inner_ty);

                Some((
                    (inner_ty.as_ref().to_owned(), ty.1),
                    SizeHint {
                        offset: 0,
                        size: inner_ty_size.byte_count,
                    },
                ))
            } else {
                errors.push(SemanticError::MissmatchedType {
                    ty: index_ty,
                    expected_type: Type::Int,
                });
                None
            }
        }
        (Ptr::ArrayAccess { .. }, _) => {
            errors.push(SemanticError::ArrayAccessOnNonArray { ty });
            None
        }
        (Ptr::PtrDeref, Type::Pointer(inner_ty)) => {
            let size = structs.get_size(&inner_ty).byte_count;
            Some((
                (inner_ty.as_ref().clone(), ty.1),
                SizeHint { offset: 0, size },
            ))
        }
        (Ptr::PtrDeref, Type::NullPtr) => {
            errors.push(SemanticError::NullPtrDeref { span: ty.1 });
            None
        }
        (Ptr::PtrDeref, _) => {
            errors.push(SemanticError::PtrDerefOnNonPtr { ty });
            None
        }
    }
}

fn check_binary_type<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    a: Spanned<Type<'a>>,
    b: Spanned<Type<'a>>,
    value_ty: Option<Type<'a>>,
    return_ty: Type<'a>,
) -> Option<Spanned<Type<'a>>> {
    if let Some(value_ty) = value_ty {
        if a.0 == value_ty && b.0 == value_ty {
            return Some((return_ty, a.1.union(b.1)));
        }

        if a.0 != value_ty {
            errors.push(SemanticError::MissmatchedType {
                ty: a,
                expected_type: value_ty.clone(),
            });
        }
        if b.0 != value_ty {
            errors.push(SemanticError::MissmatchedType {
                ty: b,
                expected_type: value_ty.clone(),
            });
        }

        return None;
    } else {
        let nullptr_compare = {
            a.0.is_ptr() && b.0.is_nullptr() || b.0.is_ptr() && a.0.is_nullptr()
        };

        if a.0 != b.0 && !nullptr_compare {
            errors.push(SemanticError::MissmatchedBinaryType { a, b });
            None
        } else {
            Some((return_ty, a.1.union(b.1)))
        }
    }
}

fn validate_function_call<'src, Num: GetSpan>(
    errors: &mut Vec<SemanticError<'src>>,
    fn_call: &FunctionCall<'src, Num>,
    namespace: &Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
    structs: &StructNamespace<'src>,
) -> Option<Spanned<Type<'src>>> {
    let func = match functions.get_function(fn_call) {
        Ok(func) => func,
        Err(err) => {
            errors.push(err);
            return None;
        }
    };
    let ident = fn_call.ident();

    match fn_call {
        FunctionCall::Alloc { .. } => {
            return Some((func.return_type(), ident.span()));
        }
        FunctionCall::AllocArray { len, .. } => {
            let len = validate_expression(errors, &len, namespace, functions, structs);

            let Some(len) = len else {
                return Some((func.return_type(), ident.span()));
            };

            if len.0 != Type::Int {
                errors.push(SemanticError::MissmatchedType {
                    ty: len,
                    expected_type: Type::Int,
                });
                return Some((func.return_type(), ident.span()));
            }

            return Some((func.return_type(), ident.span()));
        }
        FunctionCall::Fn { ident, args } => {
            for (index, param) in func.params().iter().enumerate() {
                let arg = match args.get(index) {
                    Some(arg) => arg,
                    None => {
                        errors.push(SemanticError::FunctionCallMissingArg {
                            call_ident: *ident,
                            arg_name: param.name.0,
                        });
                        continue;
                    }
                };

                let ty = match validate_expression(errors, arg, namespace, functions, structs) {
                    Some(ty) => ty,
                    None => continue,
                };

                if ty.0 != param.ty.0 {
                    errors.push(SemanticError::MissmatchedType {
                        ty,
                        expected_type: param.ty.0.clone(),
                    });
                }
            }

            if args.len() > func.params().len() {
                errors.push(SemanticError::FunctionCallTooManyArgs {
                    call_ident: *ident,
                    arg_count: args.len(),
                    expected_count: func.params().len(),
                });
                return None;
            }
        }
    }

    Some((func.return_type(), ident.span()))
}
