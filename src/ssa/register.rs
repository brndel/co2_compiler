use std::fmt::Display;

use super::counter::Counter;

#[derive(Debug, Clone, Copy)]
pub struct VirtualRegister<'a> {
    pub reg: usize,
    pub ident: Option<&'a str>,
}

impl<'a> PartialEq for VirtualRegister<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.reg == other.reg
    }
}

impl<'a> Eq for VirtualRegister<'a> {}

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
        let label = self.ident.unwrap_or("_");

        write!(f, "{}{}", label, self.reg)
        // write!(f, "r{}", self.reg)?;

        // if let Some(label) = self.ident {
        //     write!(f, ":{}", label)?;
        // }

        // Ok(())
    }
}

impl<'a> From<usize> for VirtualRegister<'a> {
    fn from(value: usize) -> Self {
        Self {
            reg: value,
            ident: None,
        }
    }
}

impl<'a> Counter<'a> {
    pub fn next_register(&mut self, ident: &'a str) -> VirtualRegister<'a> {
        let mut next: VirtualRegister<'a> = self.next();

        next.ident = Some(ident);

        next
    }
}
