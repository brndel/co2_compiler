use chumsky::span::SimpleSpan;

use crate::{
    lexer::BinaryOperator,
    parser::Expression,
    ssa::{SsaInstruction, SsaValue, basic_block::BasicBlockEnd},
};

use super::builder::{BlockBuilder, Context};

pub fn build_ir_expr<'a>(
    expr: Expression<'a>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
) -> SsaValue<'a> {
    match expr {
        Expression::Ident(ident) => SsaValue::Register(builder.get_variable(ident.0, ctx)),
        Expression::Num(num) => SsaValue::ImmediateNum(num.num()),
        Expression::Bool((value, _)) => SsaValue::ImmediateBool(value),
        Expression::Binary { a, op, b } => match op {
            BinaryOperator::LogicAnd => {
                build_ir_ternary(
                    *a,
                    *b,
                    Expression::Bool((false, SimpleSpan::new(0, 0))),
                    ctx,
                    builder,
                )
                // let target = ctx.counter.next();

                // let next_label = builder.next_label();
                // let on_true = ctx.counter.next_block_label("and_true");
                // let on_false = ctx.counter.next_block_label("and_false");

                // let a = build_ir_expr(*a, ctx, builder);
                // builder.end(
                //     BasicBlockEnd::ConditionalJump {
                //         condition: a,
                //         on_true,
                //         on_false,
                //     },
                //     ctx,
                // );

                // let mut false_scope = BlockBuilder::new(on_false, next_label, ctx);
                // false_scope.push_instruction(SsaInstruction::Move {
                //     target,
                //     source: SsaValue::ImmediateNum(0),
                // });
                // false_scope.close(ctx);

                // let mut true_scope = BlockBuilder::new(on_true, next_label, ctx);
                // let b = build_ir_expr(*b, ctx, &mut true_scope);
                // true_scope.push_instruction(SsaInstruction::Move { target, source: b });
                // true_scope.close(ctx);

                // SsaValue::Register(target)
            }
            BinaryOperator::LogicOr => {
                    build_ir_ternary(
                    *a,
                    Expression::Bool((true, SimpleSpan::new(0, 0))),
                    *b,
                    ctx,
                    builder,
                )
                // let target = ctx.counter.next();

                // let next_label = builder.next_label();
                // let on_true = ctx.counter.next_block_label("or_true");
                // let on_false = ctx.counter.next_block_label("or_false");

                // let a = build_ir_expr(*a, ctx, builder);
                // builder.end(
                //     BasicBlockEnd::ConditionalJump {
                //         condition: a,
                //         on_true,
                //         on_false,
                //     },
                //     ctx,
                // );

                // let mut false_scope = BlockBuilder::new(on_false, next_label, ctx);
                // let b = build_ir_expr(*b, ctx, &mut false_scope);
                // false_scope.push_instruction(SsaInstruction::Move { target, source: b });
                // false_scope.close(ctx);

                // let mut true_scope = BlockBuilder::new(on_true, next_label, ctx);
                // true_scope.push_instruction(SsaInstruction::Move {
                //     target,
                //     source: SsaValue::ImmediateNum(0),
                // });
                // true_scope.close(ctx);

                // SsaValue::Register(target)
            }
            op => {
                let a = build_ir_expr(*a, ctx, builder);
                let b = build_ir_expr(*b, ctx, builder);

                let target = ctx.counter.next();

                builder.push_instruction(SsaInstruction::BinaryOp { target, a, op, b });

                SsaValue::Register(target)
            }
        },
        Expression::Unary { op, expr } => {
            let value = build_ir_expr(*expr, ctx, builder);

            let value = match value {
                SsaValue::Register(virtual_register) => virtual_register,
                source => {
                    let temp_reg = ctx.counter.next();
                    builder.push_instruction(SsaInstruction::Move {
                        target: temp_reg,
                        source,
                    });

                    temp_reg
                }
            };

            let target = ctx.counter.next();
            builder.push_instruction(SsaInstruction::UnaryOp { target, op, value });

            SsaValue::Register(target)
        }
        Expression::Ternary { condition, a, b } => {
            build_ir_ternary(*condition, *a, *b, ctx, builder)
            // let target = ctx.counter.next();

            // let next_label = builder.next_label();
            // let on_true = ctx.counter.next_block_label("ternary_true");
            // let on_false = ctx.counter.next_block_label("ternary_false");

            // let condition = build_ir_expr(*condition, ctx, builder);
            // builder.end(
            //     BasicBlockEnd::ConditionalJump {
            //         condition,
            //         on_true,
            //         on_false,
            //     },
            //     ctx,
            // );

            // let mut true_scope = BlockBuilder::new(on_true, next_label, ctx);
            // let a = build_ir_expr(*a, ctx, &mut true_scope);
            // true_scope.push_instruction(SsaInstruction::Move { target, source: a });
            // true_scope.close(ctx);

            // let mut false_scope = BlockBuilder::new(on_false, next_label, ctx);
            // let b = build_ir_expr(*b, ctx, &mut false_scope);
            // false_scope.push_instruction(SsaInstruction::Move { target, source: b });
            // false_scope.close(ctx);

            // SsaValue::Register(target)
        }
    }
}

fn build_ir_ternary<'a>(
    condition: Expression<'a>,
    true_expr: Expression<'a>,
    false_expr: Expression<'a>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
) -> SsaValue<'a> {
    let target = ctx.counter.next();

    let next_label = builder.next_label();
    let on_true = ctx.counter.next_block_label("ternary_true");
    let on_false = ctx.counter.next_block_label("ternary_false");

    let condition = build_ir_expr(condition, ctx, builder);
    builder.end(
        BasicBlockEnd::ConditionalJump {
            condition,
            on_true,
            on_false,
        },
        ctx,
    );

    let mut true_scope = BlockBuilder::new(on_true, next_label, ctx);
    let true_value = build_ir_expr(true_expr, ctx, &mut true_scope);
    true_scope.push_instruction(SsaInstruction::PhiMove { target, source: true_value });
    true_scope.close(ctx);

    let mut false_scope = BlockBuilder::new(on_false, next_label, ctx);
    let false_value = build_ir_expr(false_expr, ctx, &mut false_scope);
    false_scope.push_instruction(SsaInstruction::PhiMove { target, source: false_value });
    false_scope.close(ctx);

    SsaValue::Register(target)
}
