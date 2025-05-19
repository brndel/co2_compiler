use std::num::IntErrorKind;

use crate::{
    lexer::Spanned,
    parser::{Expression, ParseNum, Statement, ValueNum},
    program::Program,
};

use super::{Namespace, SemanticError};

pub struct Analyzed<'a> {
    pub program: Program<'a>,
}

impl<'a> Analyzed<'a> {
    pub fn new(program: Program<'a, ParseNum<'a>>) -> Result<Self, Vec<SemanticError<'a>>> {
        let mut errors = Vec::new();

        Self::analyze_variable_status(&mut errors, &program);
        Self::analyze_return(&mut errors, &program);
        let program = Self::analyze_int_literal_range(&mut errors, program);

        match program {
            Some(program) if errors.is_empty() => Ok(Self { program }),
            _ => Err(errors),
        }
    }

    fn analyze_variable_status<Num>(
        errors: &mut Vec<SemanticError<'a>>,
        program: &Program<'a, Num>,
    ) {
        let mut namespace = Namespace::new();

        for statement in &program.statements {
            match statement {
                Statement::Declaration { ident, value } => {
                    if let Some(value) = value {
                        Self::analyze_variable_status_expression(errors, &namespace, value);
                    }

                    if let Err(err) = namespace.declare(*ident, value.is_some()) {
                        errors.push(err);
                    }
                }
                Statement::Assignment {
                    ident,
                    op: None,
                    value,
                } => {
                    Self::analyze_variable_status_expression(errors, &namespace, value);

                    if let Err(err) = namespace.assign(*ident) {
                        errors.push(err);
                    }
                }
                Statement::Assignment {
                    ident,
                    op: Some(_),
                    value,
                } => {
                    Self::analyze_variable_status_expression(errors, &namespace, value);

                    if let Err(err) = namespace.is_assigned(*ident) {
                        errors.push(err);
                    }
                }
                Statement::Return { expr } => {
                    Self::analyze_variable_status_expression(errors, &namespace, expr)
                }
            }
        }
    }

    fn analyze_variable_status_expression<Num>(
        errors: &mut Vec<SemanticError<'a>>,
        namespace: &Namespace<'a>,
        expression: &Expression<'a, Num>,
    ) {
        match expression {
            Expression::Ident(ident) => {
                if let Err(err) = namespace.is_assigned(*ident) {
                    errors.push(err)
                }
            }
            Expression::Binary { a, op: _, b } => {
                Self::analyze_variable_status_expression(errors, namespace, &a);
                Self::analyze_variable_status_expression(errors, namespace, &b);
            }
            Expression::Unary { op: _, expr } => {
                Self::analyze_variable_status_expression(errors, namespace, &expr);
            }
            Expression::Num(_) => (),
        }
    }

    fn analyze_return<Num>(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a, Num>) {
        for statement in &program.statements {
            match statement {
                Statement::Return { expr: _ } => return,
                _ => (),
            }
        }

        errors.push(SemanticError::NoReturnInFunction {
            ident: program.main_fn_span,
        });
    }

    fn analyze_int_literal_range(
        errors: &mut Vec<SemanticError<'a>>,
        program: Program<'a, ParseNum<'a>>,
    ) -> Option<Program<'a>> {
        let statements: Option<Vec<_>> = program
            .statements
            .into_iter()
            .map(|expr| {
                Some(match expr {
                    Statement::Declaration { ident, value: None } => {
                        Statement::Declaration { ident, value: None }
                    }
                    Statement::Declaration {
                        ident,
                        value: Some(expr),
                    } => Statement::Declaration {
                        ident,
                        value: Some(Self::map_num_expr(errors, expr)?),
                    },
                    Statement::Assignment { ident, op, value } => Statement::Assignment {
                        ident,
                        op,
                        value: Self::map_num_expr(errors, value)?,
                    },
                    Statement::Return { expr } => Statement::Return {
                        expr: Self::map_num_expr(errors, expr)?,
                    },
                })
            })
            .collect();

        Some(Program {
            statements: statements?,
            main_fn_span: program.main_fn_span,
        })
    }

    fn map_num_expr(
        errors: &mut Vec<SemanticError<'a>>,
        expression: Expression<'a, ParseNum<'a>>,
    ) -> Option<Expression<'a>> {
        match expression {
            Expression::Num(num) => {
                match Self::parse(num) {
                    Ok(num) => Some(Expression::Num(ValueNum(num))),
                    Err(err) => {
                        errors.push(err);
                        None
                    }
                }
                // if let Err(err) = Self::check_parse(ident, 10) {
                //     errors.push(err);
                // }
            }
            Expression::Binary { a, op, b } => {
                let a = Self::map_num_expr(errors, *a);
                let b = Self::map_num_expr(errors, *b);

                Some(Expression::Binary {
                    a: Box::new(a?),
                    op,
                    b: Box::new(b?),
                })
            }
            Expression::Unary { op, expr } => {
                let expr = Self::map_num_expr(errors, *expr);

                Some(Expression::Unary {
                    op,
                    expr: Box::new(expr?),
                })
            }
            Expression::Ident(ident) => Some(Expression::Ident(ident)),
        }
    }

    fn parse(num: ParseNum<'a>) -> Result<Spanned<i32>, SemanticError<'a>> {
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
                let value = ident.0.strip_prefix("0x").expect("Hex num missing 0x prefix");
                match u32::from_str_radix(value, 16) {
                            Ok(value) => {
                                let value = i32::from_be_bytes(value.to_be_bytes());
                                Ok((value, ident.1))
                            },
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
            },
        }

        // let (value, radix, ident) = match num {
        //     ParseNum::Dec(ident) => (ident.0, 10, ident),
        //     ParseNum::Hex(ident) => (ident.0.strip_prefix("0x").unwrap(), 16, ident),
        // };

        // match i32::from_str_radix(value, radix) {
        //     Ok(value) => Ok((value, ident.1)),
        //     Err(err) => {
        //         if err.kind() == &IntErrorKind::PosOverflow {
        //             Err(SemanticError::IntOverflow { ident })
        //         } else {
        //             panic!(
        //                 "unexpected int error while parsing {:?}: {:?}",
        //                 ident.0, err
        //             )
        //         }
        //     }
        // }
    }
}
