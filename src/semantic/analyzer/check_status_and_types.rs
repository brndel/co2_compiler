use chumsky::span::Span;

use crate::{
    core::Type,
    lexer::{BinaryOperator, GetSpan, Spanned, UnaryOperator},
    parser::{Block, Expression, FunctionCall, Statement},
    program::{FunctionDef, Program},
    semantic::{Namespace, SemanticError, namespace::FunctionNamespace},
};

pub fn check_status_and_types<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    program: &Program<'src, Num>,
) where
    Num: GetSpan,
{
    let functions = FunctionNamespace::new(program, errors);

    todo!()

    // for func in &program.functions {
    //     let mut namespace = Namespace::new();

    //     for param in &func.params {
    //         if let Err(err) = namespace.declare(param.name, param.ty.0, true) {
    //             errors.push(err);
    //         }
    //     }

    //     validate_block(errors, &func.block, Some(&namespace), &functions, func);
    // }
}

fn validate_block<'src, 'ns, 'program, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    block: &Block<'src, Num>,
    namespace: Option<&'ns Namespace<'src, '_>>,
    functions: &FunctionNamespace<'src>,
    current_function: &FunctionDef<'src, Num>,
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
) where
    Num: GetSpan,
{
    match statement {
        Statement::Declaration { ty, ident, value } => {
            if let Some(value) = value {
                if let Some(value_ty) = validate_expression(errors, value, &namespace, functions) {
                    if value_ty.0 != ty.0 {
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
            todo!()
            // let Some(value_ty) = validate_expression(errors, value, &namespace, functions) else {
            //     return;
            // };

            // let var_ty = match namespace.get_type(*ident) {
            //     Ok(var_ty) => var_ty,
            //     Err(err) => {
            //         errors.push(err);
            //         return;
            //     }
            // };

            // if op.is_some() {
            //     if let Err(err) = namespace.is_assigned(*ident) {
            //         errors.push(err);
            //     }

            //     check_binary_type(
            //         errors,
            //         (var_ty, ident.1),
            //         value_ty.clone(),
            //         Some(Type::Int),
            //         Type::Int,
            //     );
            // }

            // if let Err(err) = namespace.assign(*ident, value_ty.clone()) {
            //     errors.push(err);
            // }
        }
        Statement::FunctionCall(fn_call) => {
            validate_function_call(errors, fn_call, namespace, functions);
        }
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            if let Some(condition_ty) =
                validate_expression(errors, condition, &namespace, functions)
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
            );

            if let Some(r#else) = r#else {
                let mut else_namespace = Namespace::with_parent(Some(namespace));
                validate_statement(
                    errors,
                    r#else,
                    &mut else_namespace,
                    functions,
                    current_function,
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
                validate_expression(errors, condition, &namespace, functions)
            {
                if condition_ty.0 != Type::Bool {
                    errors.push(SemanticError::MissmatchedType {
                        ty: condition_ty,
                        expected_type: Type::Bool,
                    });
                }
            };

            let mut inner = Namespace::with_parent(Some(namespace));
            validate_statement(errors, then, &mut inner, functions, current_function);
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
                );
                let local_vars = loop_namespace.local_assigned_variables().clone();
                init_vars = Some(local_vars);
            }
            validate_expression(errors, condition, &mut loop_namespace, functions);

            let mut inner = loop_namespace.new_child();
            validate_statement(errors, &then, &mut inner, functions, current_function);
            loop_namespace.assign_variable_set(inner.local_assigned_variables().clone());

            if let Some(step) = step {
                validate_statement(
                    errors,
                    step,
                    &mut loop_namespace,
                    functions,
                    current_function,
                );
            }

            if let Some(init_vars) = init_vars {
                namespace.assign_variable_set(init_vars);
            }
        }
        Statement::Return { value: expr } => {
            if let Some(expr_ty) = validate_expression(errors, expr, &namespace, functions) {
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
            let inner = validate_block(errors, block, Some(namespace), functions, current_function);

            let assigned_vars = inner.into_local_assigned_variables();

            namespace.assign_variable_set(assigned_vars);
        }
    }
}

fn validate_expression<'src, Num>(
    errors: &mut Vec<SemanticError<'src>>,
    expression: &Expression<'src, Num>,
    namespace: &Namespace<'src, '_>,
    functions: &FunctionNamespace<'src>,
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
            let a_ty = validate_expression(errors, &a, namespace, functions);
            let b_ty = validate_expression(errors, &b, namespace, functions);

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
            let ty = validate_expression(errors, expr, namespace, functions)?;
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
            let condition_ty = validate_expression(errors, condition, namespace, functions);
            let a_ty = validate_expression(errors, a, namespace, functions);
            let b_ty = validate_expression(errors, b, namespace, functions);

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
            validate_function_call(errors, fn_call, namespace, functions)
        }
        _ => todo!()
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
        if a.0 != b.0 {
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
) -> Option<Spanned<Type<'src>>> {
    todo!()
    // let func = match functions.get_function(fn_call) {
    //     Ok(func) => func,
    //     Err(err) => {
    //         errors.push(err);
    //         return None;
    //     }
    // };

    // for (index, param) in func.params().iter().enumerate() {
    //     let arg = match fn_call.args.get(index) {
    //         Some(arg) => arg,
    //         None => {
    //             errors.push(SemanticError::FunctionCallMissingArg {
    //                 call_ident: fn_call.ident,
    //                 arg_name: param.name.0,
    //             });
    //             continue;
    //         }
    //     };

    //     let ty = match validate_expression(errors, arg, namespace, functions) {
    //         Some(ty) => ty,
    //         None => continue,
    //     };

    //     if ty.0 != param.ty.0 {
    //         errors.push(SemanticError::MissmatchedType { ty, expected_type: param.ty.0 });
    //     }
    // }

    // if fn_call.args.len() > func.params().len()  {
    //     errors.push(SemanticError::FunctionCallTooManyArgs { call_ident: fn_call.ident, arg_count: fn_call.args.len(), expected_count: func.params().len() });
    //     return None;
    // }

    // Some((func.return_type(), fn_call.ident.span()))
}
