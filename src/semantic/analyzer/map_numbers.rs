use std::num::IntErrorKind;

use crate::{
    lexer::Spanned,
    parser::{Block, Expression, FunctionCall, ParseNum, Statement, ValueNum},
    program::{Function, Program},
    semantic::SemanticError,
};

pub fn map_number<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    program: Program<'a, ParseNum<'a>>,
) -> Option<Program<'a>> {
    let functions = program
        .functions
        .into_iter()
        .map(
            |Function {
                 return_type,
                 ident,
                 params,
                 block,
             }| {
                Some(Function {
                    return_type,
                    ident,
                    params,
                    block: map_number_block(errors, block)?,
                })
            },
        )
        .collect::<Option<_>>()?;

    Some(Program { functions })
}

fn map_number_block<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    block: Block<'a, ParseNum<'a>>,
) -> Option<Block<'a, ValueNum>> {
    let statements = block
        .statements
        .into_iter()
        .map(|statement| map_number_statement(errors, statement))
        .collect::<Option<_>>()?;
    Some(Block { statements })
}

fn map_number_statement<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    statement: Statement<'a, ParseNum<'a>>,
) -> Option<Statement<'a>> {
    Some(match statement {
        Statement::Declaration {
            ty,
            ident,
            value: None,
        } => Statement::Declaration {
            ty,
            ident,
            value: None,
        },
        Statement::Declaration {
            ty,
            ident,
            value: Some(expr),
        } => Statement::Declaration {
            ty,
            ident,
            value: Some(map_num_expr(errors, expr)?),
        },
        Statement::Assignment { ident, op, value } => Statement::Assignment {
            ident,
            op,
            value: map_num_expr(errors, value)?,
        },
        Statement::FunctionCall(FunctionCall { ident, args }) => {
            Statement::FunctionCall(FunctionCall {
                ident,
                args: args
                    .into_iter()
                    .map(|arg| map_num_expr(errors, arg))
                    .collect::<Option<_>>()?,
            })
        }
        Statement::Return { value: expr } => Statement::Return {
            value: map_num_expr(errors, expr)?,
        },
        Statement::If {
            condition,
            then,
            r#else,
        } => Statement::If {
            condition: map_num_expr(errors, condition)?,
            then: Box::new(map_number_statement(errors, *then)?),
            r#else: match r#else {
                Some(r#else) => Some(Box::new(map_number_statement(errors, *r#else)?)),
                None => None,
            },
        },
        Statement::While { condition, body } => Statement::While {
            condition: map_num_expr(errors, condition)?,
            body: Box::new(map_number_statement(errors, *body)?),
        },
        Statement::For {
            init,
            condition,
            step,
            body,
        } => Statement::For {
            init: match init {
                Some(init) => Some(Box::new(map_number_statement(errors, *init)?)),
                None => None,
            },
            condition: map_num_expr(errors, condition)?,
            step: match step {
                Some(step) => Some(Box::new(map_number_statement(errors, *step)?)),
                None => None,
            },
            body: Box::new(map_number_statement(errors, *body)?),
        },
        Statement::Break(span) => Statement::Break(span),
        Statement::Continue(span) => Statement::Continue(span),
        Statement::Block(block) => Statement::Block(map_number_block(errors, block)?),
    })
}

fn map_num_expr<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    expression: Expression<'a, ParseNum<'a>>,
) -> Option<Expression<'a>> {
    match expression {
        Expression::Num(num) => match parse(num) {
            Ok(num) => Some(Expression::Num(ValueNum(num))),
            Err(err) => {
                errors.push(err);
                None
            }
        },
        Expression::Binary { a, op, b } => {
            let a = map_num_expr(errors, *a);
            let b = map_num_expr(errors, *b);

            Some(Expression::Binary {
                a: Box::new(a?),
                op,
                b: Box::new(b?),
            })
        }
        Expression::Unary { op, expr } => {
            let expr = map_num_expr(errors, *expr);

            Some(Expression::Unary {
                op,
                expr: Box::new(expr?),
            })
        }
        Expression::Ident(ident) => Some(Expression::Ident(ident)),
        Expression::Bool(value) => Some(Expression::Bool(value)),
        Expression::Ternary { condition, a, b } => {
            let condition = map_num_expr(errors, *condition);
            let a = map_num_expr(errors, *a);
            let b = map_num_expr(errors, *b);

            Some(Expression::Ternary {
                condition: Box::new(condition?),
                a: Box::new(a?),
                b: Box::new(b?),
            })
        }
        Expression::FunctionCall(FunctionCall { ident, args }) => {
            Some(Expression::FunctionCall(FunctionCall {
                ident,
                args: args
                    .into_iter()
                    .map(|arg| map_num_expr(errors, arg))
                    .collect::<Option<_>>()?,
            }))
        }
    }
}

fn parse<'a>(num: ParseNum<'a>) -> Result<Spanned<i32>, SemanticError<'a>> {
    match num {
        ParseNum::Dec(("2147483648", span)) => Ok((-2147483648, span)),
        ParseNum::Dec(ident) => match i32::from_str_radix(ident.0, 10) {
            Ok(value) => Ok((value, ident.1)),
            Err(err) => {
                if err.kind() == &IntErrorKind::PosOverflow {
                    Err(SemanticError::IntOverflow { ident })
                } else {
                    panic!(
                        "unexpected int error while parsing {:?}: {:?}",
                        ident.0, err
                    )
                }
            }
        },
        ParseNum::Hex(ident) => {
            let value = ident.0;
            match u32::from_str_radix(value, 16) {
                Ok(value) => {
                    let value = i32::from_be_bytes(value.to_be_bytes());
                    Ok((value, ident.1))
                }
                Err(err) => {
                    if err.kind() == &IntErrorKind::PosOverflow {
                        Err(SemanticError::IntOverflow { ident })
                    } else {
                        panic!(
                            "unexpected int error while parsing {:?}: {:?}",
                            ident.0, err
                        )
                    }
                }
            }
        }
    }
}
