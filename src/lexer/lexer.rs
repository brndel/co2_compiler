use std::str::FromStr;

use super::{AssignOperator, Keyword, Operator, Separator, Spanned, Token};
use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    prelude::{any, just, one_of, recursive},
    select,
    span::SimpleSpan,
    text::{digits, int, newline},
};


pub fn lexer<'src>()
-> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, SimpleSpan>>>
{
    let ident_keyword = chumsky_fix::ident()
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

    let operator = Operator::parser().labelled("operator");

    let assign = operator
        .clone()
        .try_map(|op, span| AssignOperator::try_from(op).map_err(|_| Rich::custom(span, "invalid assign operator")))
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

mod chumsky_fix {

    use chumsky::{
        extra::ParserExtra, input::{SliceInput, StrInput}, label::LabelError, prelude::any, text::{Char, TextExpected}, util::MaybeRef, Parser
    };

    /// A parser that accepts a C-style identifier.
    ///
    /// The output type of this parser is [`SliceInput::Slice`] (i.e: [`&str`] when `I` is [`&str`], and [`&[u8]`] when `I` is
    /// [`&[u8]`]).
    ///
    /// An identifier is defined as an ASCII alphabetic character or an underscore followed by any number of alphanumeric
    /// characters or underscores. The regex pattern for it is `[a-zA-Z_][a-zA-Z0-9_]*`.
    #[must_use]
    pub fn ident<'src, I, E>()
    -> impl Parser<'src, I, <I as SliceInput<'src>>::Slice, E> + Copy
    where
        I: StrInput<'src>,
        I::Token: Char + 'src,
        E: ParserExtra<'src, I>,
        E::Error: LabelError<'src, I, TextExpected<'src, I>>,
    {
        any()
            .try_map(|c: I::Token, span| {
                if c.to_ascii()
                    .map(|i| i.is_ascii_alphabetic() || i == b'_')
                    .unwrap_or(false)
                {
                    Ok(c)
                } else {
                    Err(LabelError::expected_found(
                        [TextExpected::IdentifierPart],
                        Some(MaybeRef::Val(c)),
                        span,
                    ))
                }
            })
            .then(
                any()
                    .try_map(|c: I::Token, span| {
                        if c.to_ascii()
                            .map_or(false, |i| i.is_ascii_alphanumeric() || i == b'_')
                        {
                            Ok(())
                        } else {
                            Err(LabelError::expected_found(
                                [TextExpected::IdentifierPart],
                                Some(MaybeRef::Val(c)),
                                span,
                            ))
                        }
                    })
                    .repeated(),
            )
            .to_slice()
    }
}
