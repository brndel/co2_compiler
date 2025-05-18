use std::fmt::Display;

use crate::lexer::Spanned;

#[derive(Debug, Clone, Copy, Eq)]
pub struct VirtualRegister<'a> {
    pub reg: usize,
    pub label: Option<Spanned<&'a str>>,
}

impl<'a> PartialEq for VirtualRegister<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.reg == other.reg
    }
}

impl<'a> PartialOrd for VirtualRegister<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.reg.partial_cmp(&other.reg)
    }
}

impl<'a> Ord for VirtualRegister<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.reg.cmp(&other.reg)
    }
}

impl<'a> Display for VirtualRegister<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.reg)?;

        if let Some((label, _)) = self.label {
            write!(f, ":{}", label)?;
        }

        Ok(())
    }
}

pub struct RegisterCounter {
    reg: usize,
}

impl RegisterCounter {
    pub fn new() -> Self {
        Self { reg: 0 }
    }

    pub fn next<'a>(&mut self) -> VirtualRegister<'a> {
        let reg = self.reg;
        self.reg += 1;
        VirtualRegister { reg, label: None }
    }

    pub fn next_label<'a>(&mut self, label: Spanned<&'a str>) -> VirtualRegister<'a> {
        let reg = self.reg;
        self.reg += 1;
        VirtualRegister {
            reg,
            label: Some(label),
        }
    }
}
