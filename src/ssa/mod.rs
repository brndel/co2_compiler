mod ssa;
mod register;
mod instruction;
mod dead_code;

pub use ssa::to_ssa;
pub use instruction::*;
pub use register::*;
pub use dead_code::remove_dead_code;