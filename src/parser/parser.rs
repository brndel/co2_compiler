use chumsky::{
    IterParser, Parser,
    error::Rich,
    extra,
    input::ValueInput,
    pratt::{infix, left, prefix},
    prelude::{end, just, recursive},
    select,
    span::SimpleSpan,
};

use crate::{
    core::Type,
    lexer::{
        BinaryOperator, Brace, Keyword, Operator, Paren, ParserBracketExt, Separator, Spanned,
        SqBracket, Token, UnaryOperator,
    },
    parser::{FunctionCall, FunctionIdent, Lvalue, Ptr},
    program::{FunctionDef, FunctionParam, Program, StructDef, StructField, TopLevelDef},
};

use super::{
    Block, ParseNum,
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
    let function_def = parse_function_def().map(TopLevelDef::Function);
    let struct_def = parse_struct_def().map(TopLevelDef::Struct);

    let def = struct_def.or(function_def);

    def.repeated()
        .collect()
        .then_ignore(end())
        .map(|defs| Program { defs })
}

pub fn parse_function_def<'token, 'src: 'token, I>() -> impl Parser<
    'token,
    I,
    FunctionDef<'src, ParseNum<'src>>,
    extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let ty = parse_ty();

    let ident = select! {
        Token::Ident(ident) = e => (ident, e.span())
    };

    let block = parse_block();

    let param = ty
        .clone()
        .then(ident)
        .map(|(ty, name)| FunctionParam { ty, name });

    ty.then(ident)
        .then(
            param
                .separated_by(just(Token::Separator(Separator::Comma)))
                .collect()
                .delimited_by_bracket(Paren),
        )
        .then(block)
        .map(|(((return_type, ident), params), block)| FunctionDef {
            return_type,
            ident,
            params,
            block,
        })
        .labelled("function_def")
        .as_context()
}

pub fn parse_struct_def<'token, 'src: 'token, I>()
-> impl Parser<'token, I, StructDef<'src>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let ty = parse_ty();

    let ident = select! {
        Token::Ident(ident) = e => (ident, e.span()),
    };

    let field = ty
        .then(ident.clone())
        .then_ignore(just(Token::Separator(Separator::Semicolon)))
        .map(|(ty, ident)| StructField { ty, ident });

    let fields = field.repeated().collect().delimited_by_bracket(Brace);

    just(Token::Keyword(Keyword::Struct))
        .ignore_then(ident)
        .then(fields)
        .then_ignore(just(Token::Separator(Separator::Semicolon)))
        .map(|(ident, fields)| StructDef { ident, fields })
        .labelled("struct_def")
        .as_context()
}

pub fn parse_block<'token, 'src: 'token, I>()
-> impl Parser<'token, I, Block<'src, ParseNum<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    recursive(|block| {
        let statement = parse_statement(block);

        let statements = statement.repeated().collect();

        let block = statements.delimited_by_bracket(Brace);

        block.map(|statements| Block { statements })
    })
}

pub fn parse_statement<'token, 'src: 'token, I, T>(
    block_parser: T,
) -> impl Parser<
    'token,
    I,
    Statement<'src, ParseNum<'src>>,
    extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
> + Clone
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
    T: Parser<
            'token,
            I,
            Block<'src, ParseNum<'src>>,
            extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
        > + Clone
        + 'token,
{
    recursive(|statement| {
        let ty = parse_ty();

        let expr = parse_expr(ty.clone());

        let lvalue = recursive(|lvalue| {
            let ident = select! {
                Token::Ident(ident) => ident,
            }
            .map_with(|ident, extra| Lvalue::Ident((ident, extra.span())));

            let lvalue = ident.or(lvalue.delimited_by_bracket(Paren));

            let ptr = parse_ptr_modifiers(lvalue, expr.clone());

            ptr
        })
        .labelled("lvalue");

        let assign = lvalue
            .then(select! {Token::Assign(op) => op})
            .then(expr.clone())
            .map(|((lvalue, op), value)| Statement::Assignment { lvalue, op, value })
            .labelled("assign");

        let decl = ty
            .clone()
            .then(
                select! {
                    Token::Ident(ident) => ident,
                }
                .map_with(|ident, extra| (ident, extra.span())),
            )
            .then(just(Token::Assign(None)).ignore_then(expr.clone()).or_not())
            .map(|((ty, ident), value)| Statement::Declaration { ty, ident, value })
            .labelled("declaration");

        let fn_call = parse_function_call(expr.clone(), ty.clone()).map(Statement::FunctionCall);

        let simple_statement = assign.or(decl).or(fn_call);

        let ret = just(Token::Keyword(Keyword::Return))
            .ignore_then(expr.clone())
            .map(|expr| Statement::Return { value: expr });

        let ctrl = select! {
            Token::Keyword(Keyword::Break) = e => Statement::Break(e.span()),
            Token::Keyword(Keyword::Continue) = e => Statement::Continue(e.span()),
        };

        let control_statement = ret.or(ctrl);

        let r#if = just(Token::Keyword(Keyword::If))
            .ignore_then(expr.clone().delimited_by_bracket(Paren))
            .then(statement.clone())
            .then(
                just(Token::Keyword(Keyword::Else))
                    .ignore_then(statement.clone())
                    .or_not(),
            )
            .map(|((condition, then), r#else)| Statement::If {
                condition,
                then: Box::new(then),
                r#else: r#else.map(Box::new),
            });

        let r#while = just(Token::Keyword(Keyword::While))
            .ignore_then(expr.clone().delimited_by_bracket(Paren))
            .then(statement.clone())
            .map(|(condition, then)| Statement::While {
                condition,
                body: Box::new(then),
            });

        let for_header = simple_statement
            .clone()
            .or_not()
            .then_ignore(just(Token::Separator(Separator::Semicolon)))
            .then(expr.clone())
            .then_ignore(just(Token::Separator(Separator::Semicolon)))
            .then(simple_statement.clone().or_not());

        let r#for = just(Token::Keyword(Keyword::For))
            .ignore_then(for_header.delimited_by_bracket(Paren))
            .then(statement.clone())
            .map(|(((init, condition), step), body)| Statement::For {
                init: init.map(Box::new),
                condition,
                step: step.map(Box::new),
                body: Box::new(body),
            });

        let statement_semicolon = simple_statement
            .or(control_statement)
            .then_ignore(just(Token::Separator(Separator::Semicolon)));

        let statement = statement_semicolon
            .or(block_parser.map(|block| Statement::Block(block)))
            .or(r#if)
            .or(r#while)
            .or(r#for)
            .labelled("statement")
            .as_context();

        statement
    })
}

pub fn parse_expr<'token, 'src: 'token, I, T>(
    ty: T,
) -> impl Parser<
    'token,
    I,
    Expression<'src, ParseNum<'src>>,
    extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
> + Clone
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
    T: Parser<'token, I, Spanned<Type<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
        + Clone
        + 'token,
{
    recursive(|expr| {
        let fn_call = parse_function_call(expr.clone(), ty).map(Expression::FunctionCall);

        let value = select! {
            Token::Ident(ident) = e => Expression::Ident((ident, e.span())),
            Token::DecNum(num) = e => Expression::Num(ParseNum::Dec((num, e.span()))),
            Token::HexNum(num) = e => Expression::Num(ParseNum::Hex((num, e.span()))),
            Token::Keyword(Keyword::True) = e => Expression::Bool((true, e.span())),
            Token::Keyword(Keyword::False) = e => Expression::Bool((false, e.span())),
            Token::Keyword(Keyword::Null) = e => Expression::NullPtr(e.span()),
        }
        .labelled("value")
        .as_context();

        let atom = fn_call
            .or(value)
            .or(expr.clone().delimited_by_bracket(Paren));

        macro_rules! binary_fold {
            () => {
                |a, op, b, e| Expression::Binary {
                    a: Box::new(a),
                    op: (op, e.span()),
                    b: Box::new(b),
                }
            };
        }

        let atom = parse_ptr_modifiers(atom, expr.clone());

        let operators = atom.pratt((
            // logical not, bitwise not, unary minus
            prefix(
                10,
                select! {
                    Token::Operator(Operator::Minus) => UnaryOperator::Minus,
                    Token::Operator(Operator::LogicNot) => UnaryOperator::LogicNot,
                    Token::Operator(Operator::BitNot) => UnaryOperator::BitNot,
                },
                |op, expr, e| Expression::Unary {
                    op: (op, e.span()),
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
                left(0),
                select! {
                Token::Operator(Operator::LogicOr) => BinaryOperator::LogicOr
                },
                binary_fold!(),
            ),
        ));

        let ternary_appendix = just(Token::Operator(Operator::TernaryQuestionMark))
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Operator(Operator::TernaryColon)))
            .then(expr.clone());

        let ternary = operators
            .then(ternary_appendix.or_not())
            .map(|(condition, values)| match values {
                Some((a, b)) => Expression::Ternary {
                    condition: Box::new(condition),
                    a: Box::new(a),
                    b: Box::new(b),
                },
                None => condition,
            });

        ternary
    })
    .labelled("expr")
    .as_context()
}

pub fn parse_ptr_modifiers<'token, 'src: 'token, I, T, U, V>(
    parser: U,
    expr_parser: V,
) -> impl Parser<'token, I, T, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>> + Clone
where
    (T, Ptr<'src, ParseNum<'src>>): Into<T>,
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
    U: Parser<'token, I, T, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>> + Clone + 'token,
    V: Parser<
            'token,
            I,
            Expression<'src, ParseNum<'src>>,
            extra::Err<Rich<'token, Token<'src>, SimpleSpan>>,
        > + Clone
        + 'token,
{
    let ptr_deref = just(Token::Operator(Operator::Mul))
        .repeated()
        .foldr(parser.clone(), |_, expr| (expr, Ptr::PtrDeref).into());

    let ident = select! {
        Token::Ident(ident) = e => (ident, e.span())
    };

    let field_access = just(Token::Separator(Separator::Dot))
        .ignore_then(ident.clone())
        .map(|ident| Ptr::FieldAccess { ident });
    let ptr_field_access = just(Token::Operator(Operator::Arrow))
        .ignore_then(ident.clone())
        .map(|ident| Ptr::PtrFieldAccess { ident });
    let array_access = expr_parser
        .delimited_by_bracket(SqBracket)
        .map(|expr| Ptr::ArrayAccess {
            index: Box::new(expr),
        });

    let access = ptr_deref.foldl(
        field_access
            .or(ptr_field_access)
            .or(array_access)
            .repeated(),
        |expr, ptr| (expr, ptr).into(),
    );

    access
}

pub fn parse_function_call<'token, 'src: 'token, I, T, U>(
    expr_parser: T,
    ty_parser: U,
) -> impl Parser<
    'token,
    I,
    FunctionCall<'src, ParseNum<'src>>,
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
    U: Parser<'token, I, Spanned<Type<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>>
        + Clone,
{
    let ident = select! {
        Token::Ident(ident) = e => (FunctionIdent::User(ident), e.span()),
        Token::Keyword(Keyword::Print) = e => (FunctionIdent::Print, e.span()),
        Token::Keyword(Keyword::Read) = e => (FunctionIdent::Read, e.span()),
        Token::Keyword(Keyword::Flush) = e => (FunctionIdent::Flush, e.span()),
    };

    let arg_list = expr_parser
        .clone()
        .separated_by(just(Token::Separator(Separator::Comma)))
        .collect();

    let normal_fn = ident
        .then(arg_list.delimited_by_bracket(Paren))
        .map(|(ident, args)| FunctionCall::Fn { ident, args });

    let alloc_fn = just(Token::Keyword(Keyword::Alloc))
        .ignore_then(ty_parser.clone().delimited_by_bracket(Paren))
        .map(|ty| FunctionCall::Alloc { ty });

    let alloc_array_fn = just(Token::Keyword(Keyword::AllocArray))
        .ignore_then(
            ty_parser
                .then_ignore(just(Token::Separator(Separator::Comma)))
                .then(expr_parser)
                .delimited_by_bracket(Paren),
        )
        .map(|(ty, len)| FunctionCall::AllocArray {
            ty,
            len: Box::new(len),
        });

    normal_fn.or(alloc_fn).or(alloc_array_fn)
}

pub fn parse_ty<'token, 'src: 'token, I>()
-> impl Parser<'token, I, Spanned<Type<'src>>, extra::Err<Rich<'token, Token<'src>, SimpleSpan>>> + Clone
where
    I: ValueInput<'token, Token = Token<'src>, Span = SimpleSpan>,
{
    let builtin_ty = select! {
        Token::Keyword(Keyword::Int) => Type::Int,
        Token::Keyword(Keyword::Bool) => Type::Bool,
    }
    .map_with(|ty, extra| (ty, extra.span()));

    let struct_ty = just(Token::Keyword(Keyword::Struct))
        .ignore_then(select! {
            Token::Ident(ident) => ident,
        })
        .map_with(|ident, extra| (Type::Struct(ident), extra.span()));

    let ty = builtin_ty.or(struct_ty);

    let ptr = just(Token::Operator(Operator::Mul)).to(true);
    let arr = just(Token::Separator(Separator::SqBracketOpen))
        .then(just(Token::Separator(Separator::SqBracketClose)))
        .to(false);

    ty.foldl(ptr.or(arr).repeated(), |ty, is_ptr| {
        if is_ptr {
            (Type::Pointer(Box::new(ty.0)), ty.1)
        } else {
            (Type::Array(Box::new(ty.0)), ty.1)
        }
    })
    .labelled("type")
}
