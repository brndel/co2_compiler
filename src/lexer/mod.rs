mod lexer;
mod token;
mod op;

pub type Spanned<T> = (T, chumsky::span::SimpleSpan);

use chumsky::span::SimpleSpan;
pub use lexer::lexer;
pub use token::*;
pub use op::*;


pub trait GetSpan {
    fn span(&self) -> SimpleSpan;
}

impl<T> GetSpan for Spanned<T> {
    fn span(&self) -> SimpleSpan {
        self.1
    }
}