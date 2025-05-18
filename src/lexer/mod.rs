mod lexer;
mod token;

pub type Spanned<T> = (T, chumsky::span::SimpleSpan);

pub use lexer::lexer;
pub use token::*;
