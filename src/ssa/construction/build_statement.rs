use crate::{
    parser::Statement,
    ssa::{SsaInstruction, SsaValue, basic_block::BasicBlockEnd},
};

use super::{
    build_expr::{build_fn_call, build_ir_expr},
    builder::{BlockBuilder, Context, Loop},
};

pub fn build_ir_statement<'a>(
    statement: &Statement<'a>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
) {
    match statement {
        Statement::Declaration {
            ty: _,
            ident,
            value,
        } => {
            if let Some(value) = value {
                let value = build_ir_expr(value, ctx, builder);

                let target = ctx.counter.next_register(ident.0);

                builder.push_instruction(SsaInstruction::Move {
                    target,
                    source: value,
                });

                builder.set_variable(ident.0, target);
            }
        }
        Statement::Assignment { lvalue, op, value } => {
            todo!()
            // let value = build_ir_expr(value, ctx, builder);

            // let target = ctx.counter.next_register(ident.0);

            // match op {
            //     Some(op) => {
            //         let current_value = builder.get_variable(ident.0, ctx);

            //         builder.push_instruction(SsaInstruction::BinaryOp {
            //             target,
            //             a: SsaValue::Register(current_value),
            //             op: (*op).into(),
            //             b: value,
            //         });
            //     }
            //     None => {
            //         builder.push_instruction(SsaInstruction::Move {
            //             target,
            //             source: value,
            //         });
            //     }
            // }

            // builder.set_variable(ident.0, target);
        }
        Statement::FunctionCall(fn_call) => {
            build_fn_call(fn_call, None, ctx, builder);
        },
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            let condition = build_ir_expr(condition, ctx, builder);

            let next_label = builder.next_label();
            let then_label = ctx.counter.next_block_label(Some("then"));
            let else_label = if r#else.is_some() {
                ctx.counter.next_block_label(Some("else"))
            } else {
                next_label
            };

            builder.end(
                BasicBlockEnd::ConditionalJump {
                    condition,
                    on_true: then_label,
                    on_false: else_label,
                },
                ctx,
            );

            let mut then_scope = BlockBuilder::new(then_label, next_label, ctx);
            build_ir_statement(then, ctx, &mut then_scope);
            then_scope.close(ctx);

            if let Some(r#else) = r#else {
                let mut else_scope = BlockBuilder::new(else_label, next_label, ctx);
                build_ir_statement(r#else, ctx, &mut else_scope);
                else_scope.close(ctx);
            }
        }
        Statement::While { condition, body } => {
            let next_label = builder.next_label();
            let condition_label = ctx.counter.next_block_label(Some("while_condition"));
            let body_label = ctx.counter.next_block_label(Some("while_body"));

            builder.end(
                BasicBlockEnd::Goto {
                    label: condition_label,
                },
                ctx,
            );

            let mut condition_scope =
                BlockBuilder::new(condition_label, next_label, ctx).unsealed();
            let condition = build_ir_expr(condition, ctx, &mut condition_scope);
            condition_scope.close_with(
                BasicBlockEnd::ConditionalJump {
                    condition,
                    on_true: body_label,
                    on_false: next_label,
                },
                ctx,
            );

            {
                ctx.push_loop(Loop {
                    next: condition_label,
                    end: next_label,
                });
                let mut body_scope = BlockBuilder::new(body_label, condition_label, ctx);
                build_ir_statement(body, ctx, &mut body_scope);
                body_scope.close(ctx);
                ctx.pop_loop();
            }

            ctx.seal_block(&condition_label);
        }
        Statement::For {
            init,
            condition,
            step,
            body,
        } => {
            let next_label = builder.next_label();

            let condition_label = ctx.counter.next_block_label(Some("for_condition"));
            let body_label = ctx.counter.next_block_label(Some("for_body"));

            let init_label = if init.is_some() {
                ctx.counter.next_block_label(Some("for_init"))
            } else {
                condition_label
            };

            let step_label = if step.is_some() {
                ctx.counter.next_block_label(Some("for_step"))
            } else {
                condition_label
            };

            builder.end(BasicBlockEnd::Goto { label: init_label }, ctx);

            if let Some(init) = init {
                let mut init_scope = BlockBuilder::new(init_label, condition_label, ctx);
                build_ir_statement(init, ctx, &mut init_scope);
                init_scope.close(ctx);
            }

            {
                let mut condition_scope =
                    BlockBuilder::new(condition_label, body_label, ctx).unsealed();
                let condition = build_ir_expr(condition, ctx, &mut condition_scope);
                condition_scope.close_with(
                    BasicBlockEnd::ConditionalJump {
                        condition,
                        on_true: body_label,
                        on_false: next_label,
                    },
                    ctx,
                );
            }

            if let Some(step) = step {
                let mut step_scope = BlockBuilder::new(step_label, condition_label, ctx).unsealed();
                build_ir_statement(step, ctx, &mut step_scope);
                step_scope.close(ctx);
            }

            {
                ctx.push_loop(Loop {
                    next: step_label,
                    end: next_label,
                });
                let mut body_scope = BlockBuilder::new(body_label, step_label, ctx);
                build_ir_statement(body, ctx, &mut body_scope);
                body_scope.close(ctx);
                ctx.pop_loop();
            }

            ctx.seal_block(&step_label);
            ctx.seal_block(&condition_label);
        }
        Statement::Return { value } => {
            let value = build_ir_expr(value, ctx, builder);
            builder.close_with(BasicBlockEnd::Return { value }, ctx);
        }
        Statement::Break(_) => {
            let loop_end = ctx.get_loop().end;

            builder.close_with(BasicBlockEnd::Goto { label: loop_end }, ctx);
        }
        Statement::Continue(_) => {
            let loop_next = ctx.get_loop().next;

            builder.close_with(BasicBlockEnd::Goto { label: loop_next }, ctx);
        }
        Statement::Block(block) => {
            for statement in &block.statements {
                if builder.is_closed() {
                    break;
                }
                build_ir_statement(statement, ctx, builder);
            }
        }
    }
}
