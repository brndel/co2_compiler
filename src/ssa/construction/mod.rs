mod basic_block;
mod build_expr;
mod build_statement;
mod builder;

use basic_block::*;
use build_statement::build_ir_statement;
use builder::{BlockBuilder, Context};

use crate::{
    parser::Statement,
    program::{Function, Program},
    ssa::BlockLabel,
};

use super::{basic_block::BasicBlock, ir_graph::IrGraph};

pub struct FunctionIrGraph<'a> {
    pub name: &'a str,
    pub start_label: BlockLabel,
    pub graph: IrGraph<BasicBlock<'a>>,
}

impl<'a> FunctionIrGraph<'a> {
    pub fn new(func: Function<'a>) -> Self {
        let mut ctx = Context::new();

        let start_label = ctx.counter.next_block_label("main");
        let end_label = ctx.counter.next_block_label("end");

        let mut scope = BlockBuilder::new(start_label, end_label, &mut ctx);

        for param in func.params {
            scope.set_variable(param.name.0, ctx.counter.next_register(param.name.0));
        }

        build_ir_statement(Statement::Block(func.block), &mut ctx, &mut scope);

        scope.close(&mut ctx);

        let graph = ctx.finish();

        Self {
            name: func.ident.0,
            start_label,
            graph,
        }
    }
}
