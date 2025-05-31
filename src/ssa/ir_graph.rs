use std::collections::{btree_map::{IntoKeys, IntoValues, Keys, Values}, BTreeMap};

use super::{BasicBlock, BasicBlockEnd};

pub struct IrGraph<T: IrGraphNode> {
    blocks: BTreeMap<T::Id, T>,
}

impl<T: IrGraphNode> Default for IrGraph<T> {
    fn default() -> Self {
        Self {
            blocks: Default::default(),
        }
    }
}

pub trait IrGraphNode {
    type Id: Ord + Copy;

    fn id(&self) -> Self::Id;
    fn is_predecessor(&self, id: &Self::Id) -> bool;
}

impl<T: IrGraphNode> IrGraph<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, block: T) {
        self.blocks.insert(block.id(), block);
    }

    pub fn get_predecessors(&self, id: &T::Id) -> impl Iterator<Item = &T> {
        self.blocks.values().filter(move |block| {
            if block.is_predecessor(id) {
                true
            } else {
                false
            }
        })
    }

    pub fn get_mut(&mut self, id: &T::Id) -> Option<&mut T> {
        self.blocks.get_mut(id)
    }

    pub fn get(&self, id: &T::Id) -> Option<&T> {
        self.blocks.get(id)
    }

    pub fn get_first(&self) -> Option<&T> {
        self.blocks.first_key_value().map(|(_, v)| v)
    }

    pub fn map<R: IrGraphNode>(self, f: impl Fn(T) -> R) -> IrGraph<R> {
        let blocks = self
            .blocks
            .into_values()
            .map(f)
            .map(|block| (block.id(), block))
            .collect();

        IrGraph { blocks }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.blocks.values()
    }
}

impl<T: IrGraphNode> IntoIterator for IrGraph<T> {
    type Item = T;

    type IntoIter = IntoValues<T::Id, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.blocks.into_values()
    }
}

impl<'a, T: IrGraphNode> IntoIterator for &'a IrGraph<T> {
    type Item = &'a T;

    type IntoIter = Values<'a, T::Id, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.blocks.values()
    }
}


impl<'a> IrGraph<BasicBlock<'a>> {
    pub fn get_return_blocks(&self) -> impl Iterator<Item = &BasicBlock<'a>> {
        self.blocks.values().filter(|block| match block.end {
            BasicBlockEnd::Return { .. } => true,
            _ => false,
        })
    }
}