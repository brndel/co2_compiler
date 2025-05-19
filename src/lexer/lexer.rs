use std::str::FromStr;

use super::{Keyword, Operator, Separator, Token};
use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    prelude::{any, just, one_of, recursive},
    select,
    span::SimpleSpan,
    text::{digits, ident, int, newline},
};

type Spanned<T> = (T, SimpleSpan);

pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, SimpleSpan>>>
{
    let ident_keyword = ident()
        .map(|s| {
            Keyword::from_str(s)
                .map(Token::Keyword)
                .unwrap_or(Token::Ident(s))
        })
        .labelled("ident/keyword");

    let dec_num = int(10).map(Token::DecNum).labelled("dec_num");

    let hex_num = just("0")
        .then(one_of("xX"))
        .then(digits(16))
        .to_slice()
        .map(Token::HexNum)
        .labelled("hex_num");

    let separator = select! {
        '(' => Separator::ParenOpen,
        ')' => Separator::ParenClose,
        '{' => Separator::BraceOpen,
        '}' => Separator::BraceClose,
        ';' => Separator::Semicolon,
    }
    .labelled("separator");

    let operator = select! {
        '+' => Operator::Plus,
        '-' => Operator::Minus,
        '*' => Operator::Mul,
        '/' => Operator::Div,
        '%' => Operator::Mod,
    }
    .labelled("operator");

    let assign = operator
        .or_not()
        .then_ignore(just("="))
        .map(Token::Assign)
        .labelled("assign");

    let control_token = assign
        .or(separator.map(Token::Separator))
        .or(operator.map(Token::Operator));

    let word_token = hex_num.or(dec_num).or(ident_keyword);

    let token = word_token.or(control_token);

    let single_comment = just("//").then(any().and_is(newline().not()).repeated());

    let multi_comment = recursive(|comment| {
        let any = any()
            .and_is(just("*/").or(just("/*")).not())
            .repeated()
            .at_least(1)
            .ignored();

        comment
            .delimited_by(just("/*"), just("*/"))
            .or(any)
            .repeated()
    })
    .delimited_by(just("/*"), just("*/"));

    let whitespace = one_of(" \t\n\r").repeated();

    let comment = single_comment
        .ignored()
        .or(multi_comment.ignored())
        .padded_by(whitespace);

    token
        .map_with(|token, extra| (token, extra.span()))
        .padded_by(comment.repeated())
        .padded_by(whitespace)
        .repeated()
        .collect()
}
