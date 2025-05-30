use std::collections::BTreeMap;

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
    fn is_predecessor(&self, id: Self::Id) -> bool;
}

impl<T: IrGraphNode> IrGraph<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, block: T) {
        self.blocks.insert(block.id(), block);
    }

    pub fn get_predecessors(&self, id: T::Id) -> impl Iterator<Item = T::Id> {
        self.blocks.iter().filter_map(move |(block_id, block)| {
            if block.is_predecessor(id) {
                Some(*block_id)
            } else {
                None
            }
        })
    }

    
}
