use std::fmt::Display;

use super::{SsaInstruction, SsaValue, counter::Counter, ir_graph::IrGraphNode};

pub struct BasicBlock<'a> {
    pub label: BlockLabel,
    pub instructions: Vec<SsaInstruction<'a>>,
    pub end: BasicBlockEnd<'a>,
}

impl<'a> Display for BasicBlock<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.label)?;

        for (line, instr) in self.instructions.iter().enumerate() {
            writeln!(f, "{:2}: {}", line, instr)?;
        }

        writeln!(f, "{}", self.end)?;

        Ok(())
    }
}

impl<'a> IrGraphNode for BasicBlock<'a> {
    type Id = BlockLabel;

    fn id(&self) -> Self::Id {
        self.label
    }

    fn is_predecessor(&self, id: &Self::Id) -> bool {
        self.end.goes_to(&id)
    }
}

#[derive(Debug, Clone, Copy, Eq)]
pub struct BlockLabel {
    id: usize,
    tag: Option<&'static str>,
}

impl From<usize> for BlockLabel {
    fn from(value: usize) -> Self {
        Self {
            id: value,
            tag: None,
        }
    }
}

impl Display for BlockLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tag = self.tag.unwrap_or("label");

        write!(f, "{}_{}", tag, self.id)
    }
}

impl PartialEq for BlockLabel {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialOrd for BlockLabel {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.id.partial_cmp(&other.id)
    }
}

impl Ord for BlockLabel {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl Counter {
    pub fn next_block_label(&mut self, tag: &'static str) -> BlockLabel {
        let mut label: BlockLabel = self.next();

        label.tag = Some(tag);

        label
    }
}

pub enum BasicBlockEnd<'a> {
    Goto {
        label: BlockLabel,
    },
    Return {
        value: SsaValue<'a>,
    },
    ConditionalJump {
        condition: SsaValue<'a>,
        on_true: BlockLabel,
        on_false: BlockLabel,
    },
}

impl<'a> BasicBlockEnd<'a> {
    pub fn goes_to(&self, label: &BlockLabel) -> bool {
        match self {
            BasicBlockEnd::Goto { label: to_label } if to_label == label => true,
            BasicBlockEnd::ConditionalJump {
                condition: _,
                on_true,
                on_false,
            } if on_true == label || on_false == label => true,
            _ => false,
        }
    }
}

impl<'a> Display for BasicBlockEnd<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicBlockEnd::Goto { label } => write!(f, "goto {}", label),
            BasicBlockEnd::Return { value } => write!(f, "return {}", value),
            BasicBlockEnd::ConditionalJump {
                condition,
                on_true,
                on_false,
            } => write!(f, "if ({}) goto {} else {}", condition, on_true, on_false),
        }
    }
}
