use chumsky::{error, span::Span};

use crate::{
    core::Type,
    lexer::{AssignOperator, BinaryOperator, GetSpan, Spanned, UnaryOperator},
    parser::{Block, Expression, Statement},
    program::Program,
    semantic::{Namespace, SemanticError, namespace},
};

pub fn check_status_and_types<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    program: &Program<'a, Num>,
) where
    Num: GetSpan,
{
    validate_block(errors, &program.block, None);
}

fn validate_block<'a, 'b, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    block: &Block<'a, Num>,
    namespace: Option<&'b Namespace<'a, '_>>,
) -> Namespace<'a, 'b>
where
    Num: GetSpan,
{
    let mut namespace = Namespace::with_parent(namespace);

    for statement in &block.statements {
        validate_statement(errors, statement, &mut namespace)
    }

    namespace
}

fn validate_statement<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    statement: &Statement<'a, Num>,
    namespace: &mut Namespace<'a, '_>,
) where
    Num: GetSpan,
{
    match statement {
        Statement::Declaration { ty, ident, value } => {
            if let Some(value) = value {
                if let Some(value_ty) = validate_expression(errors, value, &namespace) {
                    if value_ty.0 != ty.0 {
                        errors.push(SemanticError::MissmatchedType {
                            ty: value_ty,
                            expected_type: ty.0,
                        });
                    }
                };
            }

            if let Err(err) = namespace.declare(*ident, ty.0, value.is_some()) {
                errors.push(err);
            }
        }
        Statement::Assignment { ident, op, value } => {
            let Some(value_ty) = validate_expression(errors, value, &namespace) else {
                return;
            };

            let var_ty = match namespace.get_type(*ident) {
                Ok(var_ty) => var_ty,
                Err(err) => {
                    errors.push(err);
                    return;
                }
            };

            if op.is_some() {
                if let Err(err) = namespace.is_assigned(*ident) {
                    errors.push(err);
                }

                check_binary_type(
                    errors,
                    (var_ty, ident.1),
                    value_ty,
                    Some(Type::Int),
                    Type::Int,
                );
            }

            if let Err(err) = namespace.assign(*ident, value_ty) {
                errors.push(err);
            }
        }
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            if let Some(condition_ty) = validate_expression(errors, condition, &namespace) {
                if condition_ty.0 != Type::Bool {
                    errors.push(SemanticError::MissmatchedType {
                        ty: condition_ty,
                        expected_type: Type::Bool,
                    });
                }
            };

            let mut then_namespace = Namespace::with_parent(Some(namespace));
            validate_statement(errors, then, &mut then_namespace);

            if let Some(r#else) = r#else {
                let mut else_namespace = Namespace::with_parent(Some(namespace));
                validate_statement(errors, r#else, &mut else_namespace);

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
            if let Some(condition_ty) = validate_expression(errors, condition, &namespace) {
                if condition_ty.0 != Type::Bool {
                    errors.push(SemanticError::MissmatchedType {
                        ty: condition_ty,
                        expected_type: Type::Bool,
                    });
                }
            };

            let mut inner = Namespace::with_parent(Some(namespace));
            validate_statement(errors, then, &mut inner);
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
                validate_statement(errors, init, &mut loop_namespace);
                let local_vars = loop_namespace.local_assigned_variables().clone();
                init_vars = Some(local_vars);
            }
            validate_expression(errors, condition, &mut loop_namespace);

            let mut inner = loop_namespace.new_child();
            validate_statement(errors, &then, &mut inner);

            if let Some(step) = step {
                validate_statement(errors, step, &mut loop_namespace);
            }

            if let Some(init_vars) = init_vars {
                namespace.assign_variable_set(init_vars);
            }
        }
        Statement::Return { value: expr } => {
            if let Some(expr_ty) = validate_expression(errors, expr, &namespace) {
                if expr_ty.0 != Type::Int {
                    errors.push(SemanticError::MissmatchedType {
                        ty: expr_ty,
                        expected_type: Type::Int,
                    });
                }
            };
            namespace.assign_everything();
        }
        Statement::Break(_) | Statement::Continue(_) => namespace.assign_everything(),
        Statement::Block(block) => {
            let inner = validate_block(errors, block, Some(namespace));

            let assigned_vars = inner.into_local_assigned_variables();

            namespace.assign_variable_set(assigned_vars);
        }
    }
}

fn validate_expression<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    expression: &Expression<'a, Num>,
    namespace: &Namespace<'a, '_>,
) -> Option<Spanned<Type>>
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
            let a_ty = validate_expression(errors, &a, namespace);
            let b_ty = validate_expression(errors, &b, namespace);

            let a = a_ty?;
            let b = b_ty?;

            match op {
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
            let ty = validate_expression(errors, expr, namespace)?;
            match op {
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
            let condition_ty = validate_expression(errors, condition, namespace);
            let a_ty = validate_expression(errors, a, namespace);
            let b_ty = validate_expression(errors, b, namespace);

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
    }
}

fn check_binary_type<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    a: Spanned<Type>,
    b: Spanned<Type>,
    value_ty: Option<Type>,
    return_ty: Type,
) -> Option<Spanned<Type>> {
    if let Some(value_ty) = value_ty {
        if a.0 != value_ty {
            errors.push(SemanticError::MissmatchedType {
                ty: a,
                expected_type: value_ty,
            });
        }
        if b.0 != value_ty {
            errors.push(SemanticError::MissmatchedType {
                ty: b,
                expected_type: value_ty,
            });
        }

        if a.0 == value_ty && b.0 == value_ty {
            Some((return_ty, a.1.union(b.1)))
        } else {
            None
        }
    } else {
        if a.0 != b.0 {
            errors.push(SemanticError::MissmatchedBinaryType { a, b });
            None
        } else {
            Some((return_ty, a.1.union(b.1)))
        }
    }
}
