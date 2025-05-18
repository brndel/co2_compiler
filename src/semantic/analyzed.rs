use std::num::IntErrorKind;

use crate::{
    lexer::Spanned,
    parser::{Expression, Statement},
    program::Program,
};

use super::{Namespace, SemanticError};

pub struct Analyzed<'a> {
    program: Program<'a>,
}

impl<'a> Analyzed<'a> {
    pub fn new(program: Program<'a>) -> Result<Self, Vec<SemanticError<'a>>> {
        let mut errors = Vec::new();

        Self::analyze_variable_status(&mut errors, &program);
        Self::analyze_return(&mut errors, &program);
        Self::analyze_int_literal_range(&mut errors, &program);

        if errors.is_empty() {
            Ok(Self { program })
        } else {
            Err(errors)
        }
    }

    fn analyze_variable_status(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a>) {
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
                Statement::Assignment { ident, op: None, value } => {
                    Self::analyze_variable_status_expression(errors, &namespace, value);

                    if let Err(err) = namespace.assign(*ident) {
                        errors.push(err);
                    }
                }
                Statement::Assignment { ident, op: Some(_), value } => {
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

    fn analyze_variable_status_expression(
        errors: &mut Vec<SemanticError<'a>>,
        namespace: &Namespace<'a>,
        expression: &Expression<'a>,
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
            Expression::DecNum(_) | Expression::HexNum(_) => (),
        }
    }

    fn analyze_return(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a>) {
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

    fn analyze_int_literal_range(errors: &mut Vec<SemanticError<'a>>, program: &Program<'a>) {
        for statement in &program.statements {
            match statement {
                Statement::Declaration { ident: _, value } => {
                    if let Some(value) = value {
                        Self::analyze_int_literal_range_expression(errors, value);
                    }
                }
                Statement::Assignment {
                    ident: _,
                    op: _,
                    value,
                } => {
                    Self::analyze_int_literal_range_expression(errors, value);
                }
                Statement::Return { expr } => {
                    Self::analyze_int_literal_range_expression(errors, expr)
                }
            }
        }
    }

    fn analyze_int_literal_range_expression(
        errors: &mut Vec<SemanticError<'a>>,
        expression: &Expression<'a>,
    ) {
        match expression {
            Expression::DecNum(ident) => {
                if let Err(err) = Self::check_parse(ident, 10) {
                    errors.push(err);
                }
            }
            Expression::HexNum(ident) => {
                if let Err(err) = Self::check_parse(ident, 16) {
                    errors.push(err);
                }
            }
            Expression::Binary { a, op: _, b } => {
                Self::analyze_int_literal_range_expression(errors, &a);
                Self::analyze_int_literal_range_expression(errors, &b);
            }
            Expression::Unary { op: _, expr } => {
                Self::analyze_int_literal_range_expression(errors, &expr);
            }
            Expression::Ident(_) => (),
        }
    }

    fn check_parse(ident: &Spanned<&'a str>, radix: u32) -> Result<(), SemanticError<'a>> {
        let value = match radix {
            10 => ident.0,
            16 => ident.0.strip_prefix("0x").unwrap(),
            _ => unimplemented!(),
        };

        if let Err(err) = u32::from_str_radix(value, radix) {
            if err.kind() == &IntErrorKind::PosOverflow {
                Err(SemanticError::IntOverflow { ident: *ident })
            } else {
                panic!(
                    "unexpected int error while parsing {:?}: {:?}",
                    ident.0, err
                )
            }
        } else {
            Ok(())
        }
    }
}
