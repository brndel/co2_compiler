mod lexer;
mod token;
mod op;

pub type Spanned<T> = (T, chumsky::span::SimpleSpan);

pub use lexer::lexer;
pub use token::*;
pub use op::*;