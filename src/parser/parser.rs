use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    input::{MapExtra, ValueInput},
    pratt::{infix, left, prefix, right},
    prelude::{just, recursive},
    select,
    span::SimpleSpan,
};

use crate::{
    lexer::{BinaryOperator, Keyword, Operator, Separator, Token, UnaryOperator},
    program::Program,
};

use super::{
    Block, ParseNum, Type,
    ast::{Expression, Statement},
};

pub fn program_parser<'token, 'src: 'token, I>() -> impl Parser<
    'token,
    I,
    Program<'src, ParseNum<'src>>,
    extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let fn_block = parse_block();

    let main_name_span = select! {
        Token::Ident(name) = e if name == "main" => (name, e.span())
    };

    just(Token::Keyword(Keyword::Int))
        .ignore_then(main_name_span)
        .then_ignore(just(Token::Separator(Separator::ParenOpen)))
        .then_ignore(just(Token::Separator(Separator::ParenClose)))
        .then(fn_block)
        .map(|(name, block)| Program {
            main_fn_span: name,
            block,
        })
        .labelled("main fn")
        .as_context()
}

pub fn parse_block<'token, 'src: 'token, I>()
-> impl Parser<'token, I, Block<'src, ParseNum<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let expr = recursive(|expr| {
        let value = select! {
            Token::Ident(ident) = e => Expression::Ident((ident, e.span())),
            Token::DecNum(num) = e => Expression::Num(ParseNum::Dec((num, e.span()))),
            Token::HexNum(num) = e => Expression::Num(ParseNum::Hex((num, e.span()))),
            Token::Keyword(Keyword::True) = e => Expression::Bool((true, e.span())),
            Token::Keyword(Keyword::False) = e => Expression::Bool((false, e.span())),
        }
        .labelled("value")
        .as_context();

        let atom = value.or(expr.delimited_by(
            just(Token::Separator(Separator::ParenOpen)),
            just(Token::Separator(Separator::ParenClose)),
        ));

        // let binary_expr = |a: Expression<'_, _>, op: BinaryOperator, b: Expression<'_, _>, _| {
        //     Expression::Binary {
        //         a: Box::new(a),
        //         op,
        //         b: Box::new(b),
        //     }
        // };
        macro_rules! binary_fold {
            () => {
                |a, op, b, _| Expression::Binary {
                    a: Box::new(a),
                    op,
                    b: Box::new(b),
                }
            };
        }

        let operators = atom.pratt((
            // logical not, bitwise not, unary minus
            prefix(
                10,
                select! {
                    Token::Operator(Operator::Minus) => UnaryOperator::Minus,
                    Token::Operator(Operator::LogicNot) => UnaryOperator::LogicNot,
                    Token::Operator(Operator::BitNot) => UnaryOperator::BitNot,
                },
                |op, expr, _| Expression::Unary {
                    op,
                    expr: Box::new(expr),
                },
            ),
            // integer times, divide, modulo
            infix(
                left(9),
                select! {
                    Token::Operator(Operator::Mul) => BinaryOperator::Mul,
                    Token::Operator(Operator::Div) => BinaryOperator::Div,
                    Token::Operator(Operator::Mod) => BinaryOperator::Mod,
                },
                binary_fold!(),
            ),
            // integer plus, minus
            infix(
                left(8),
                select! {
                Token::Operator(Operator::Plus) => BinaryOperator::Plus,
                Token::Operator(Operator::Minus) => BinaryOperator::Minus,
                },
                binary_fold!(),
            ),
            // (arithmetic) shift left, right
            infix(
                left(7),
                select! {
                Token::Operator(Operator::ShiftLeft) => BinaryOperator::ShiftLeft,
                Token::Operator(Operator::ShiftRight) => BinaryOperator::ShiftRight,
                },
                binary_fold!(),
            ),
            // integer comparison
            infix(
                left(6),
                select! {
                Token::Operator(Operator::Greater) => BinaryOperator::Greater,
                Token::Operator(Operator::GreaterEq) => BinaryOperator::GreaterEq,
                Token::Operator(Operator::Less) => BinaryOperator::Less,
                Token::Operator(Operator::LessEq) => BinaryOperator::LessEq,
                },
                binary_fold!(),
            ),
            // overloaded equality, disequality
            infix(
                left(5),
                select! {
                Token::Operator(Operator::Equals) => BinaryOperator::Equals,
                Token::Operator(Operator::NotEquals) => BinaryOperator::NotEquals,
                },
                binary_fold!(),
            ),
            // bitwise and
            infix(
                left(4),
                select! {
                Token::Operator(Operator::BitAnd) => BinaryOperator::BitAnd
                },
                binary_fold!(),
            ),
            // bitwise exclusive or
            infix(
                left(3),
                select! {
                Token::Operator(Operator::BitXor) => BinaryOperator::BitXor
                },
                binary_fold!(),
            ),
            // bitwise or
            infix(
                left(2),
                select! {
                Token::Operator(Operator::BitOr) => BinaryOperator::BitOr
                },
                binary_fold!(),
            ),
            // logical and
            infix(
                left(1),
                select! {
                Token::Operator(Operator::LogicAnd) => BinaryOperator::LogicAnd
                },
                binary_fold!(),
            ),
            // logical or
            infix(
                left(1),
                select! {
                Token::Operator(Operator::LogicOr) => BinaryOperator::LogicOr
                },
                binary_fold!(),
            ),
        ));

        let ternary = operators
            .clone()
            .then_ignore(just(Token::Operator(Operator::TernaryQuestionMark)))
            .then(operators.clone())
            .then_ignore(just(Token::Operator(Operator::TernaryColon)))
            .then(operators.clone())
            .map(|((condition, a), b)| Expression::Ternary {
                condition: Box::new(condition),
                a: Box::new(a),
                b: Box::new(b),
            });

        ternary.or(operators)
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

    let ty = select! {
        Token::Keyword(Keyword::Int) => Type::Int,
        Token::Keyword(Keyword::Bool) => Type::Bool,
    }
    .map_with(|ty, extra| (ty, extra.span()));

    let decl = ty
        .then(
            select! {
                Token::Ident(ident) => ident,
            }
            .map_with(|ident, extra| (ident, extra.span())),
        )
        .then(just(Token::Assign(None)).ignore_then(expr.clone()).or_not())
        .map(|((ty, ident), value)| Statement::Declaration { ty, ident, value })
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

    let statements = statement.repeated().collect();

    let block = just(Token::Separator(Separator::BraceOpen))
        .ignore_then(statements)
        .then_ignore(just(Token::Separator(Separator::BraceClose)));

    block.map(|statements| Block { statements })
}

pub fn binary_op<'token, 'src: 'token, I, T, P>(
    value: T,
    op: P,
) -> impl Parser<
    'token,
    I,
    Expression<'src, ParseNum<'src>>,
    extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
> + Clone
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
    T: Parser<
            'token,
            I,
            Expression<'src, ParseNum<'src>>,
            extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
        > + Clone,
    P: Parser<'token, I, BinaryOperator, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>> + Clone,
{
    value
        .clone()
        .foldl(op.then(value).repeated(), |a, (op, b)| Expression::Binary {
            a: Box::new(a),
            op,
            b: Box::new(b),
        })
}
