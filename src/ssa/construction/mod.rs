mod basic_block;
mod build_expr;
mod build_statement;
mod builder;

use basic_block::*;
use build_statement::build_ir_statement;
use builder::{BlockBuilder, Context};

use crate::{parser::Statement, program::Program};

use super::{basic_block::BasicBlock, ir_graph::IrGraph};

pub fn build_ir_graph<'a>(program: Program<'a>) -> IrGraph<BasicBlock<'a>> {
    let mut ctx = Context::new();

    let start_label = ctx.counter.next_block_label("main");
    let end_label = ctx.counter.next_block_label("end");

    let mut scope = BlockBuilder::new(start_label, end_label, &mut ctx);

    todo!();
    // build_ir_statement(Statement::Block(program.block), &mut ctx, &mut scope);

    scope.close(&mut ctx);

    ctx.finish()
}
