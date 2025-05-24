use chumsky::span::Span;

use crate::{
    lexer::{AssignOperator, BinaryOperator, GetSpan, Spanned, UnaryOperator},
    parser::{Block, Expression, Statement, Type},
    program::Program,
    semantic::{Namespace, SemanticError},
};

pub fn check_status_and_types<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    program: &Program<'a, Num>,
) where Num: GetSpan {
    let namespace = Namespace::new();
    validate_block(errors, &program.block, namespace);
}

fn validate_block<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    block: &Block<'a, Num>,
    mut namespace: Namespace<'a>,
) -> Namespace<'a>
where
    Num: GetSpan,
{
    for statement in &block.statements {
        namespace = validate_statement(errors, statement, namespace);
    }

    namespace
}

fn validate_statement<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    statement: &Statement<'a, Num>,
    mut namespace: Namespace<'a>,
) -> Namespace<'a>
where
    Num: GetSpan,
{
    match statement {
        Statement::Declaration { ty, ident, value } => {
            if let Some(value) = value {
                let Some(value_ty) = validate_expression(errors, value, &namespace) else {
                    return namespace;
                };

                if value_ty.0 != ty.0 {
                    errors.push(SemanticError::MissmatchedType {
                        ty: value_ty,
                        expected_type: ty.0,
                    });
                }
            }

            if let Err(err) = namespace.declare(*ident, ty.0, value.is_some()) {
                errors.push(err);
            }
        }
        Statement::Assignment { ident, op, value } => {
            let Some(value_ty) = validate_expression(errors, value, &namespace) else {
                return namespace;
            };

            let var_ty = match namespace.get_type(*ident) {
                Ok(var_ty) => var_ty,
                Err(err) => {
                    errors.push(err);
                    return namespace;
                }
            };

            if let Some(op) = op {
                match op {
                    AssignOperator::Plus
                    | AssignOperator::Minus
                    | AssignOperator::Mul
                    | AssignOperator::Div
                    | AssignOperator::Mod
                    | AssignOperator::ShiftLeft
                    | AssignOperator::ShiftRight
                    | AssignOperator::BitAnd
                    | AssignOperator::BitOr
                    | AssignOperator::BitXor => {
                        check_binary_type(
                            errors,
                            (var_ty, ident.1),
                            value_ty,
                            Some(Type::Int),
                            Type::Int,
                        );
                    }
                }
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
            let Some(condition_ty) = validate_expression(errors, condition, &namespace) else {
                return namespace;
            };

            if condition_ty.0 != Type::Bool {
                errors.push(SemanticError::MissmatchedType {
                    ty: condition_ty,
                    expected_type: Type::Bool,
                });
                return namespace;
            }

            namespace = validate_statement(errors, then, namespace);
            if let Some(r#else) = r#else {
                namespace = validate_statement(errors, r#else, namespace);
            }
        }
        Statement::While { condition, then } => {
            let Some(condition_ty) = validate_expression(errors, condition, &namespace) else {
                return namespace;
            };

            if condition_ty.0 != Type::Bool {
                errors.push(SemanticError::MissmatchedType {
                    ty: condition_ty,
                    expected_type: Type::Bool,
                });
                return namespace;
            }

            namespace = validate_statement(errors, then, namespace);
        }
        Statement::For {
            init,
            condition,
            end,
            then,
        } => {
            let inner = namespace.new_child();

            let inner = validate_statement(errors, init, inner);
            validate_expression(errors, condition, &inner);

            let inner = validate_statement(errors, &end, inner);
            let inner = validate_statement(errors, &then, inner);

            namespace = inner.parent().unwrap();
        },
        Statement::Return { expr } => {
            let Some(expr_ty) = validate_expression(errors, expr, &namespace) else {
                return namespace;
            };

            if expr_ty.0 != Type::Int {
                errors.push(SemanticError::MissmatchedType {
                    ty: expr_ty,
                    expected_type: Type::Int,
                });
            }
        }
        Statement::Break(_) | Statement::Continue(_) => (),
        Statement::Block(block) => {
            let inner = namespace.new_child();

            let inner = validate_block(errors, block, inner);

            namespace = inner.parent().unwrap();
        }
    }

    namespace
}

fn validate_expression<'a, Num>(
    errors: &mut Vec<SemanticError<'a>>,
    expression: &Expression<'a, Num>,
    namespace: &Namespace<'a>,
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
