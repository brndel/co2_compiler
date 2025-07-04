use chumsky::span::SimpleSpan;

use crate::{
    lexer::BinaryOperator,
    parser::{Expression, FunctionCall, Ptr},
    ssa::{SsaInstruction, SsaValue, VirtualRegister, basic_block::BasicBlockEnd},
};

use super::builder::{BlockBuilder, Context};

pub fn build_ir_expr<'a>(
    expr: &Expression<'a>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
) -> SsaValue<'a> {
    match expr {
        Expression::Ident(ident) => SsaValue::Register(builder.get_variable(ident.0, ctx)),
        Expression::Num(num) => SsaValue::ImmediateNum(num.num()),
        Expression::Bool((value, _)) => SsaValue::ImmediateBool(*value),
        Expression::Binary { a, op, b } => match op.0 {
            BinaryOperator::LogicAnd => build_ir_ternary(
                a,
                b,
                &Expression::Bool((false, SimpleSpan::new(0, 0))),
                ctx,
                builder,
                "and_true",
                "and_false",
            ),
            BinaryOperator::LogicOr => build_ir_ternary(
                a,
                &Expression::Bool((true, SimpleSpan::new(0, 0))),
                b,
                ctx,
                builder,
                "or_true",
                "or_false",
            ),
            op => {
                let a = build_ir_expr(a, ctx, builder);
                let b = build_ir_expr(b, ctx, builder);

                let target = ctx.counter.next();

                builder.push_instruction(SsaInstruction::BinaryOp { target, a, op, b });

                SsaValue::Register(target)
            }
        },
        Expression::Unary { op, expr } => {
            let value = build_ir_expr(expr, ctx, builder);

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
            builder.push_instruction(SsaInstruction::UnaryOp {
                target,
                op: op.0,
                value,
            });

            SsaValue::Register(target)
        }
        Expression::Ternary { condition, a, b } => build_ir_ternary(
            &condition,
            &a,
            &b,
            ctx,
            builder,
            "ternary_true",
            "ternary_false",
        ),
        Expression::FunctionCall(fn_call) => {
            let target = ctx.counter.next();
            build_fn_call(fn_call, Some(target), ctx, builder);

            SsaValue::Register(target)
        }
        Expression::Access {
            expr,
            ptr,
            size_hint,
        } => {
            let hint = size_hint.take().unwrap();

            match ptr {
                Ptr::FieldAccess { .. } => {
                    builder.add_mem_offset(hint.offset);
                    builder.set_mem_field_size(hint.size);

                    build_ir_expr(expr, ctx, builder)
                }
                Ptr::PtrFieldAccess { .. } => {
                    builder.add_mem_offset(hint.offset);
                    builder.set_mem_field_size(hint.size);
                    let (offset, field_size) = builder.take_mem_access();

                    let target = ctx.counter.next();
                    let ptr = build_ir_expr(expr, ctx, builder);

                    builder.push_instruction(SsaInstruction::MemGet {
                        target,
                        source_ptr: ptr,
                        offset,
                        field_size,
                    });

                    SsaValue::Register(target)
                }
                Ptr::ArrayAccess { index } => {
                    builder.set_mem_field_size(hint.size);
                    let (offset, field_size) = builder.take_mem_access();

                    let array_ptr_target = ctx.counter.next();
                    let array_ptr = build_ir_expr(expr, ctx, builder);
                    let index = build_ir_expr(index, ctx, builder);

                    builder.push_instruction(SsaInstruction::CalcArrayPtr {
                        target: array_ptr_target,
                        ptr: array_ptr,
                        index,
                        struct_size: hint.size,
                    });

                    let target = ctx.counter.next();

                    builder.push_instruction(SsaInstruction::MemGet {
                        target,
                        source_ptr: SsaValue::Register(array_ptr_target),
                        offset,
                        field_size,
                    });

                    SsaValue::Register(target)
                }
                Ptr::PtrDeref => {
                    builder.set_mem_field_size(hint.size.try_into().unwrap());
                    let (offset, field_size) = builder.take_mem_access();

                    let target = ctx.counter.next();
                    let ptr = build_ir_expr(expr, ctx, builder);

                    builder.push_instruction(SsaInstruction::MemGet {
                        target,
                        source_ptr: ptr,
                        offset,
                        field_size,
                    });

                    SsaValue::Register(target)
                }
            }
        }
        Expression::NullPtr(_) => SsaValue::ImmediateNum(0),
    }
}

fn build_ir_ternary<'a>(
    condition: &Expression<'a>,
    true_expr: &Expression<'a>,
    false_expr: &Expression<'a>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
    true_label: &'static str,
    false_label: &'static str,
) -> SsaValue<'a> {
    let next_label = builder.next_label();
    let condition_label = ctx.counter.next_block_label(Some("condition"));
    let on_true = ctx.counter.next_block_label(Some(true_label));
    let on_false = ctx.counter.next_block_label(Some(false_label));
    let target = ctx.counter.next();

    builder.end(
        BasicBlockEnd::Goto {
            label: condition_label,
        },
        ctx,
    );

    let mut condition_builder = BlockBuilder::new(condition_label, next_label, ctx);

    let condition = build_ir_expr(condition, ctx, &mut condition_builder);
    condition_builder.close_with(
        BasicBlockEnd::ConditionalJump {
            condition,
            on_true,
            on_false,
        },
        ctx,
    );

    let mut true_scope = BlockBuilder::new(on_true, next_label, ctx);
    let true_value = build_ir_expr(true_expr, ctx, &mut true_scope);
    true_scope.push_instruction(SsaInstruction::PhiMove {
        target,
        source: true_value,
    });
    true_scope.close(ctx);

    let mut false_scope = BlockBuilder::new(on_false, next_label, ctx);
    let false_value = build_ir_expr(false_expr, ctx, &mut false_scope);
    false_scope.push_instruction(SsaInstruction::PhiMove {
        target,
        source: false_value,
    });
    false_scope.close(ctx);

    SsaValue::Register(target)
}

pub fn build_fn_call<'a>(
    fn_call: &FunctionCall<'a>,
    target: Option<VirtualRegister<'a>>,
    ctx: &mut Context<'a>,
    builder: &mut BlockBuilder<'a>,
) {
    match fn_call {
        FunctionCall::Alloc { ty, .. } => {
            builder.push_instruction(SsaInstruction::Allocate {
                target,
                ty: ty.0.clone(),
                array_len: None,
            });
        }
        FunctionCall::AllocArray { ty, len, .. } => {
            let array_len = build_ir_expr(&len, ctx, builder);

            builder.push_instruction(SsaInstruction::Allocate {
                target,
                ty: ty.0.clone(),
                array_len: Some(array_len),
            });
        }
        FunctionCall::Fn { ident, args } => {
            let args = args
                .iter()
                .map(|arg| build_ir_expr(arg, ctx, builder))
                .collect();
            builder.push_instruction(SsaInstruction::FunctionCall {
                target,
                name: ident.0,
                args,
            });
        }
    }
}
