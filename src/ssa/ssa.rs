use std::{
    collections::{BTreeMap, BTreeSet},
    mem::swap,
};

use crate::{
    lexer::BinaryOperator,
    parser::{Expression, Statement},
    program::Program,
};

use super::{
    basic_block::{self, BasicBlockEnd, BlockLabel},
    construction::{AssignPhi, ConBasicBlock as BasicBlock, PhiAssignment},
    counter::Counter,
    instruction::{SsaInstruction, SsaValue},
    register::VirtualRegister,
};

pub fn to_ssa<'a>(program: Program<'a>) -> Vec<basic_block::BasicBlock<'a>> {
    let mut ctx = Context::new();

    let start_label = ctx.counter.next_block_label("main");
    let end_label = ctx.counter.next_block_label("end");

    let mut scope = BuilderScope::new(start_label, end_label, &mut ctx);

    statement_to_ssa(Statement::Block(program.block), &mut ctx, &mut scope);

    scope.close(&mut ctx);

    ctx.block_order
        .into_iter()
        .map(|label| {
            let block = ctx.blocks.remove(&label).unwrap();

            if !block.is_sealed() {
                eprintln!("BLOCK {} IS NOT SEALED AT END", block.label);
            }

            block
        }).map(|block| block.into())
        .collect()
}

#[derive(Default)]
pub struct Context<'a> {
    counter: Counter,
    loops: Vec<Loop>,
    blocks: BTreeMap<BlockLabel, BasicBlock<'a>>,
    block_order: Vec<BlockLabel>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_block(&mut self, block: BasicBlock<'a>) {
        let label = block.label;
        self.block_order.push(label);
        if self.blocks.insert(block.label, block).is_some() {
            println!("inserting block for label that already exists {}", label);
        }
    }

    pub fn get_predecessors(&self, label: BlockLabel) -> Vec<BlockLabel> {
        self.blocks
            .iter()
            .filter_map(move |(block_label, block)| match block.end {
                BasicBlockEnd::Goto { label: to_label } if to_label == label => Some(*block_label),
                BasicBlockEnd::ConditionalJump {
                    condition: _,
                    on_true,
                    on_false,
                } if on_true == label || on_false == label => Some(*block_label),
                _ => None,
            })
            .collect()
    }

    pub fn get_var_recursive(&mut self, var: &'a str, label: BlockLabel) -> VirtualRegister<'a> {
        if let Some(block) = self.blocks.get_mut(&label) {
            if let Some(reg) = block.variables.get(var) {
                return *reg;
            }

            // Block is not sealed
            if let Some(unfinished_variables) = &mut block.unfinished_variables {
                let phi_register = self.counter.next_register(var);

                block.variables.insert(var, phi_register);
                unfinished_variables.insert(var, phi_register);

                return phi_register;
            }
        }

        let predecessors = self.get_predecessors(label);

        if predecessors.len() == 1 {
            let label = predecessors[0];

            return self.get_var_recursive(var, label);
        }

        let phi_register = self.counter.next_register(var);

        let block = self.blocks.get_mut(&label).unwrap();
        block.variables.insert(var, phi_register);

        let mut phi_values = BTreeSet::new();

        for label in predecessors {
            let register = self.get_var_recursive(var, label);

            phi_values.insert(register);
            self.blocks
                .get_mut(&label)
                .unwrap()
                .assigned_phis
                .push(AssignPhi {
                    target: phi_register,
                    source: register,
                });
        }

        let block = self.blocks.get_mut(&label).unwrap();
        block.phis.push(PhiAssignment {
            target: phi_register,
            phi_sources: phi_values,
        });

        return phi_register;
    }

    fn seal_block(&mut self, label: BlockLabel) {
        let block = self.blocks.get_mut(&label).unwrap();

        let Some(vars) = block.unfinished_variables.take() else {
            return;
        };

        for (var, phi_register) in vars {
            let predecessors = self.get_predecessors(label);

            let mut phi_values = BTreeSet::new();

            if predecessors.len() <= 1 {
                let label = predecessors[0];

                let register = self.get_var_recursive(var, label);
                phi_values.insert(register);
                self.blocks
                    .get_mut(&label)
                    .unwrap()
                    .assigned_phis
                    .push(AssignPhi {
                        target: phi_register,
                        source: register,
                    });
            } else {
                for label in predecessors {
                    let register = self.get_var_recursive(var, label);

                    phi_values.insert(register);
                    self.blocks
                        .get_mut(&label)
                        .unwrap()
                        .assigned_phis
                        .push(AssignPhi {
                            target: phi_register,
                            source: register,
                        });
                }
            }

            let block = self.blocks.get_mut(&label).unwrap();
            block.phis.push(PhiAssignment {
                target: phi_register,
                phi_sources: phi_values,
            });
        }
    }
}

pub struct Loop {
    next: BlockLabel,
    end: BlockLabel,
}

pub struct BuilderScope<'a> {
    start_label: BlockLabel,
    end_label: BlockLabel,

    builder: BasicBlockBuilder<'a>,
    is_closed: bool,
}

pub struct BasicBlockBuilder<'a> {
    label: BlockLabel,
    next: BlockLabel,
    instructions: Vec<SsaInstruction<'a>>,
    phis: Vec<PhiAssignment<'a>>,
    assigned_phis: Vec<AssignPhi<'a>>,
    variables: BTreeMap<&'a str, VirtualRegister<'a>>,
    unfinished_variables: Option<BTreeMap<&'a str, VirtualRegister<'a>>>,
}

impl<'a> BasicBlockBuilder<'a> {
    pub fn end(&mut self, end: BasicBlockEnd<'a>, ctx: &mut Context<'a>) {
        let block = self.get_basic_block(end, ctx);

        ctx.push_block(block);
    }

    fn get_basic_block(&mut self, end: BasicBlockEnd<'a>, ctx: &mut Context<'a>) -> BasicBlock<'a> {
        let mut instructions = Vec::new();
        swap(&mut self.instructions, &mut instructions);

        let label = self.label;
        self.label = self.next;
        self.next = ctx.counter.next();

        let mut variables = BTreeMap::new();
        swap(&mut self.variables, &mut variables);

        let unfinished_variables = self.unfinished_variables.take();

        let mut phis = Vec::new();
        swap(&mut self.phis, &mut phis);

        let mut assigned_phis = Vec::new();
        swap(&mut self.assigned_phis, &mut assigned_phis);

        BasicBlock {
            label,
            instructions,
            phis,
            assigned_phis,
            end,
            variables,
            unfinished_variables,
        }
    }

    pub fn get_variable(&mut self, var: &'a str, ctx: &mut Context<'a>) -> VirtualRegister<'a> {
        if let Some(register) = self.variables.get(var) {
            return *register;
        }

        if let Some(unfinished_variables) = &mut self.unfinished_variables {
            let phi_register = ctx.counter.next_register(var);

            unfinished_variables.insert(var, phi_register);
            self.variables.insert(var, phi_register);

            return phi_register;
        }

        let predecessors = ctx.get_predecessors(self.label);

        if predecessors.len() == 1 {
            let label = predecessors[0];

            return ctx.get_var_recursive(var, label);
        }

        let phi_register = ctx.counter.next_register(var);

        self.variables.insert(var, phi_register);

        let mut phi_values = BTreeSet::new();

        for label in predecessors {
            let register = ctx.get_var_recursive(var, label);

            phi_values.insert(register);
            ctx.blocks
                .get_mut(&label)
                .unwrap()
                .assigned_phis
                .push(AssignPhi {
                    target: phi_register,
                    source: register,
                });
        }

        self.phis.push(PhiAssignment {
            target: phi_register,
            phi_sources: phi_values,
        });

        return phi_register;
    }
}

impl<'a> BuilderScope<'a> {
    pub fn new(start_label: BlockLabel, end_label: BlockLabel, ctx: &mut Context<'a>) -> Self {
        Self {
            start_label,
            end_label,
            builder: BasicBlockBuilder {
                label: start_label,
                next: ctx.counter.next(),
                instructions: Vec::new(),
                phis: Vec::new(),
                assigned_phis: Vec::new(),
                variables: BTreeMap::new(),
                unfinished_variables: None,
            },
            is_closed: false,
        }
    }

    pub fn new_unsealed(
        start_label: BlockLabel,
        end_label: BlockLabel,
        ctx: &mut Context<'a>,
    ) -> Self {
        Self {
            start_label,
            end_label,
            builder: BasicBlockBuilder {
                label: start_label,
                next: ctx.counter.next(),
                instructions: Vec::new(),
                phis: Vec::new(),
                assigned_phis: Vec::new(),
                variables: BTreeMap::new(),
                unfinished_variables: Some(BTreeMap::new()),
            },
            is_closed: false,
        }
    }

    pub fn close(&mut self, ctx: &mut Context<'a>) {
        self.close_with(
            BasicBlockEnd::Goto {
                label: self.end_label,
            },
            ctx,
        );
    }

    fn close_with(&mut self, end: BasicBlockEnd<'a>, ctx: &mut Context<'a>) {
        if !self.is_closed {
            self.is_closed = true;

            self.builder.end(end, ctx);
        }
    }
}

pub fn statement_to_ssa<'a>(
    statement: Statement<'a>,
    ctx: &mut Context<'a>,
    scope: &mut BuilderScope<'a>,
) {
    match statement {
        Statement::Declaration {
            ty: _,
            ident,
            value,
        } => {
            if let Some(value) = value {
                let value = expr_to_ssa(value, ctx, scope);

                let target = ctx.counter.next_register(ident.0);

                scope.builder.instructions.push(SsaInstruction::Move {
                    target,
                    source: value,
                });

                scope.builder.variables.insert(ident.0, target);
            }
        }
        Statement::Assignment { ident, op, value } => {
            let value = expr_to_ssa(value, ctx, scope);

            let target = ctx.counter.next_register(ident.0);

            match op {
                Some(op) => {
                    let current_value = scope.builder.get_variable(ident.0, ctx);

                    scope.builder.instructions.push(SsaInstruction::BinaryOp {
                        target,
                        a: SsaValue::Register(current_value),
                        op: op.into(),
                        b: value,
                    });
                }
                None => {
                    scope.builder.instructions.push(SsaInstruction::Move {
                        target,
                        source: value,
                    });
                }
            }

            scope.builder.variables.insert(ident.0, target);
        }
        Statement::If {
            condition,
            then,
            r#else,
        } => {
            let condition = expr_to_ssa(condition, ctx, scope);

            let next_label = scope.builder.next;
            let then_label = ctx.counter.next_block_label("then");
            let else_label = if r#else.is_some() {
                ctx.counter.next_block_label("else")
            } else {
                next_label
            };

            scope.builder.end(
                BasicBlockEnd::ConditionalJump {
                    condition,
                    on_true: then_label,
                    on_false: else_label,
                },
                ctx,
            );

            let mut then_scope = BuilderScope::new(then_label, next_label, ctx);
            statement_to_ssa(*then, ctx, &mut then_scope);
            then_scope.close(ctx);

            if let Some(r#else) = r#else {
                let mut else_scope = BuilderScope::new(else_label, next_label, ctx);
                statement_to_ssa(*r#else, ctx, &mut else_scope);
                else_scope.close(ctx);
            }
        }
        Statement::While { condition, body } => {
            let next_label = scope.builder.next;
            let condition_label = ctx.counter.next_block_label("while_condition");
            let body_label = ctx.counter.next_block_label("while_body");

            scope.builder.end(
                BasicBlockEnd::Goto {
                    label: condition_label,
                },
                ctx,
            );

            let mut condition_scope = BuilderScope::new_unsealed(condition_label, next_label, ctx);
            let condition = expr_to_ssa(condition, ctx, &mut condition_scope);
            condition_scope.close_with(
                BasicBlockEnd::ConditionalJump {
                    condition,
                    on_true: body_label,
                    on_false: next_label,
                },
                ctx,
            );

            {
                ctx.loops.push(Loop {
                    next: condition_label,
                    end: next_label,
                });
                let mut body_scope = BuilderScope::new(body_label, condition_label, ctx);
                statement_to_ssa(*body, ctx, &mut body_scope);
                body_scope.close(ctx);
                ctx.loops.pop();
            }

            ctx.seal_block(condition_label);
        }
        Statement::For {
            init,
            condition,
            step,
            body,
        } => {
            let next_label = scope.builder.next;

            let condition_label = ctx.counter.next_block_label("for_condition");
            let body_label = ctx.counter.next_block_label("for_body");

            let init_label = if init.is_some() {
                ctx.counter.next_block_label("for_init")
            } else {
                condition_label
            };

            let step_label = if step.is_some() {
                ctx.counter.next_block_label("for_step")
            } else {
                body_label
            };

            scope
                .builder
                .end(BasicBlockEnd::Goto { label: init_label }, ctx);

            if let Some(init) = init {
                let mut init_scope = BuilderScope::new(init_label, condition_label, ctx);
                statement_to_ssa(*init, ctx, &mut init_scope);
                init_scope.close(ctx);
            }

            {
                let mut condition_scope =
                    BuilderScope::new_unsealed(condition_label, body_label, ctx);
                let condition = expr_to_ssa(condition, ctx, &mut condition_scope);
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
                let mut step_scope = BuilderScope::new_unsealed(step_label, condition_label, ctx);
                statement_to_ssa(*step, ctx, &mut step_scope);
                step_scope.close(ctx);
            }

            {
                ctx.loops.push(Loop {
                    next: step_label,
                    end: next_label,
                });
                let mut body_scope = BuilderScope::new(body_label, step_label, ctx);
                statement_to_ssa(*body, ctx, &mut body_scope);
                body_scope.close(ctx);
                ctx.loops.pop();
            }

            ctx.seal_block(step_label);
            ctx.seal_block(condition_label);
        }
        Statement::Return { value } => {
            let value = expr_to_ssa(value, ctx, scope);
            scope.close_with(BasicBlockEnd::Return { value }, ctx);
        }
        Statement::Break(_) => {
            let loop_end = ctx.loops.last().unwrap().end;

            scope.close_with(BasicBlockEnd::Goto { label: loop_end }, ctx);
        }
        Statement::Continue(_) => {
            let loop_next = ctx.loops.last().unwrap().next;

            scope.close_with(BasicBlockEnd::Goto { label: loop_next }, ctx);
        }
        Statement::Block(block) => {
            for statement in block.statements {
                if scope.is_closed {
                    break;
                }
                statement_to_ssa(statement, ctx, scope);
            }
        }
    }
}

pub fn expr_to_ssa<'a>(
    expr: Expression<'a>,
    ctx: &mut Context<'a>,
    scope: &mut BuilderScope<'a>,
) -> SsaValue<'a> {
    match expr {
        Expression::Ident(ident) => SsaValue::Register(scope.builder.get_variable(ident.0, ctx)),
        Expression::Num(num) => SsaValue::Immediate(num.num()),
        Expression::Bool((value, _)) => SsaValue::Immediate(if value { 1 } else { 0 }),
        Expression::Binary { a, op, b } => match op {
            BinaryOperator::LogicAnd => {
                let target = ctx.counter.next();

                let next_label = scope.builder.next;
                let on_true = ctx.counter.next_block_label("and_true");
                let on_false = ctx.counter.next_block_label("and_false");

                let a = expr_to_ssa(*a, ctx, scope);
                scope.builder.end(
                    BasicBlockEnd::ConditionalJump {
                        condition: a,
                        on_true,
                        on_false,
                    },
                    ctx,
                );

                let mut false_scope = BuilderScope::new(on_false, next_label, ctx);
                false_scope.builder.instructions.push(SsaInstruction::Move {
                    target,
                    source: SsaValue::Immediate(0),
                });
                false_scope.close(ctx);

                let mut true_scope = BuilderScope::new(on_true, next_label, ctx);
                let b = expr_to_ssa(*b, ctx, &mut true_scope);
                true_scope
                    .builder
                    .instructions
                    .push(SsaInstruction::Move { target, source: b });
                true_scope.close(ctx);

                SsaValue::Register(target)
            }
            BinaryOperator::LogicOr => {
                let target = ctx.counter.next();

                let next_label = scope.builder.next;
                let on_true = ctx.counter.next_block_label("or_true");
                let on_false = ctx.counter.next_block_label("or_false");

                let a = expr_to_ssa(*a, ctx, scope);
                scope.builder.end(
                    BasicBlockEnd::ConditionalJump {
                        condition: a,
                        on_true,
                        on_false,
                    },
                    ctx,
                );

                let mut false_scope = BuilderScope::new(on_false, next_label, ctx);
                let b = expr_to_ssa(*b, ctx, &mut false_scope);
                false_scope
                    .builder
                    .instructions
                    .push(SsaInstruction::Move { target, source: b });
                false_scope.close(ctx);

                let mut true_scope = BuilderScope::new(on_true, next_label, ctx);
                true_scope.builder.instructions.push(SsaInstruction::Move {
                    target,
                    source: SsaValue::Immediate(0),
                });
                true_scope.close(ctx);

                SsaValue::Register(target)
            }
            op => {
                let a = expr_to_ssa(*a, ctx, scope);
                let b = expr_to_ssa(*b, ctx, scope);

                let target = ctx.counter.next();

                scope
                    .builder
                    .instructions
                    .push(SsaInstruction::BinaryOp { target, a, op, b });

                SsaValue::Register(target)
            }
        },
        Expression::Unary { op, expr } => {
            let value = expr_to_ssa(*expr, ctx, scope);

            let value = match value {
                SsaValue::Register(virtual_register) => virtual_register,
                SsaValue::Immediate(value) => {
                    let temp_reg = ctx.counter.next();
                    scope.builder.instructions.push(SsaInstruction::Move {
                        target: temp_reg,
                        source: SsaValue::Immediate(value),
                    });

                    temp_reg
                }
            };

            let target = ctx.counter.next();
            scope
                .builder
                .instructions
                .push(SsaInstruction::UnaryOp { target, op, value });

            SsaValue::Register(target)
        }
        Expression::Ternary { condition, a, b } => {
            let target = ctx.counter.next();

            let next_label = scope.builder.next;
            let on_true = ctx.counter.next_block_label("ternary_true");
            let on_false = ctx.counter.next_block_label("ternary_false");

            let condition = expr_to_ssa(*condition, ctx, scope);
            scope.builder.end(
                BasicBlockEnd::ConditionalJump {
                    condition,
                    on_true,
                    on_false,
                },
                ctx,
            );

            let mut true_scope = BuilderScope::new(on_true, next_label, ctx);
            let a = expr_to_ssa(*a, ctx, &mut true_scope);
            true_scope
                .builder
                .instructions
                .push(SsaInstruction::Move { target, source: a });
            true_scope.close(ctx);

            let mut false_scope = BuilderScope::new(on_false, next_label, ctx);
            let b = expr_to_ssa(*b, ctx, &mut false_scope);
            false_scope
                .builder
                .instructions
                .push(SsaInstruction::Move { target, source: b });
            false_scope.close(ctx);

            SsaValue::Register(target)
        }
    }
}
