use std::num::IntErrorKind;

use crate::{
    lexer::Spanned,
    parser::{Block, Expression, ParseNum, Statement, ValueNum},
    program::Program,
    semantic::SemanticError,
};

pub fn map_number<'a>(
    errors: &mut Vec<SemanticError<'a>>,
    program: Program<'a, ParseNum<'a>>,
) -> Option<Program<'a>> {
    let block = map_number_block(errors, program.block)?;

    Some(Program {
        block,
        main_fn_span: program.main_fn_span,
    })
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
        Statement::Return { expr } => Statement::Return {
            expr: map_num_expr(errors, expr)?,
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
        Statement::While { condition, then } => Statement::While {
            condition: map_num_expr(errors, condition)?,
            then: Box::new(map_number_statement(errors, *then)?),
        },
        Statement::For {
            init,
            condition,
            end,
            then,
        } => Statement::For {
            init: Box::new(map_number_statement(errors, *init)?),
            condition: map_num_expr(errors, condition)?,
            end: Box::new(map_number_statement(errors, *end)?),
            then: Box::new(map_number_statement(errors, *then)?),
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
    }
}

fn parse<'a>(num: ParseNum<'a>) -> Result<Spanned<i32>, SemanticError<'a>> {
    match num {
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
            let value = ident
                .0
                .strip_prefix("0x")
                .expect("Hex num missing 0x prefix");
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
