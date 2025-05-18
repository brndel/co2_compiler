use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    input::ValueInput,
    prelude::{just, recursive},
    select,
    span::SimpleSpan,
};

use crate::{
    lexer::{Keyword, Operator, Separator, Token, UnaryOperator},
    program::Program,
};

use super::ast::{Expression, Statement};

pub fn program_parser<'token, 'src: 'token, I>()
-> impl Parser<'token, I, Program<'src>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let fn_content = parse_statements();

    let main_name_span = select! {
        Token::Ident(name) = e if name == "main" => (name, e.span())
    };

    just(Token::Keyword(Keyword::Int))
        .ignore_then(main_name_span)
        .then_ignore(just(Token::Separator(Separator::ParenOpen)))
        .then_ignore(just(Token::Separator(Separator::ParenClose)))
        .then_ignore(just(Token::Separator(Separator::BraceOpen)))
        .then(fn_content)
        .then_ignore(just(Token::Separator(Separator::BraceClose)))
        .map(|(name, content)| Program {
            main_fn_span: name,
            statements: content,
        })
        .labelled("main fn")
        .as_context()
}

pub fn parse_statements<'token, 'src: 'token, I>()
-> impl Parser<'token, I, Vec<Statement<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let expr = recursive(|expr| {
        let value = select! {
            Token::Ident(ident) = e => Expression::Ident((ident, e.span())),
            Token::DecNum(num) = e => Expression::DecNum((num, e.span())),
            Token::HexNum(num) = e => Expression::HexNum((num, e.span())),
        }
        .labelled("value")
        .as_context();

        let atom = value.or(expr.delimited_by(
            just(Token::Separator(Separator::ParenOpen)),
            just(Token::Separator(Separator::ParenClose)),
        ));

        let unary_op = {
            let op = select! {
                Token::Operator(Operator::Minus) => UnaryOperator::Minus,
            };

            op.repeated().foldr(atom, |op, expr| Expression::Unary {
                op,
                expr: Box::new(expr),
            })
        };

        let mul = {
            let op = select! {
                Token::Operator(Operator::Mul) => Operator::Mul,
                Token::Operator(Operator::Div) => Operator::Div,
                Token::Operator(Operator::Mod) => Operator::Mod,
            };

            unary_op
                .clone()
                .foldl(op.then(unary_op).repeated(), |a, (op, b)| {
                    Expression::Binary {
                        a: Box::new(a),
                        op,
                        b: Box::new(b),
                    }
                })
        };

        let sum = {
            let op = select! {
                Token::Operator(Operator::Plus) => Operator::Plus,
                Token::Operator(Operator::Minus) => Operator::Minus,
            };

            mul.clone()
                .foldl(op.then(mul).repeated(), |a, (op, b)| Expression::Binary {
                    a: Box::new(a),
                    op,
                    b: Box::new(b),
                })
        };

        sum
    })
    .labelled("expr")
    .as_context();

    let lvalue = recursive(|lvalue| {
        lvalue
            .delimited_by(
                just(Token::Separator(Separator::ParenOpen)),
                just(Token::Separator(Separator::ParenClose)),
            )
            .or(select! {
                Token::Ident(ident) => ident,
            }
            .map_with(|ident, extra| (ident, extra.span())))
    })
    .labelled("lvalue");

    let assign = lvalue
        .then(select! {Token::Assign(op) => op})
        .then(expr.clone())
        .map(|((ident, op), value)| Statement::Assignment { ident, op, value })
        .labelled("assign");

    let decl = just(Token::Keyword(Keyword::Int))
        .ignore_then(
            select! {
                Token::Ident(ident) => ident,
            }
            .map_with(|ident, extra| (ident, extra.span())),
        )
        .then(just(Token::Assign(None)).ignore_then(expr.clone()).or_not())
        .map(|(ident, value)| Statement::Declaration { ident, value })
        .labelled("declaration");

    let ret = just(Token::Keyword(Keyword::Return))
        .ignore_then(expr.clone())
        .map(|expr| Statement::Return { expr });

    let statement = decl
        .or(assign)
        .or(ret)
        .then_ignore(just(Token::Separator(Separator::Semicolon)))
        .labelled("statement")
        .as_context();

    statement.repeated().collect()
}
