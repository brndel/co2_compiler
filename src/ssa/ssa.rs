
use std::{collections::BTreeMap, fmt::Display};

use crate::parser::Statement;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Register(usize);

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

pub fn to_ssa<'a>(statements: Vec<Statement<'a>>) {

    // let mut variables = BTreeMap::new();
    // let mut instructions = Vec::new();

    // for statement in statements {
    //     match statement {
    //         Statement::Declaration { ident, value } => todo!(),
    //         Statement::Assignment { ident, op, value } => todo!(),
    //         Statement::Return { expr } => todo!(),
    //     }
    // }

}