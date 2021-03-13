use crate::span::Span;
use super::tokens::*;
use std::thread::current;
use crate::syntax::literal::Literal;

struct LexingContext {
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
    source: Vec<char>,
}

impl LexingContext {
    fn new(source: String) -> LexingContext {
        LexingContext {
            start: 0,
            current: 0,
            line: 1,
            tokens: Vec::new(),
            source: source.chars().collect(),
        }
    }
}

fn peek_offset(ctx: &mut LexingContext, offset: usize) -> char {
    if is_done(ctx) {
        '\0'
    } else {
        *ctx.source.get(ctx.current + offset).unwrap()
    }
}

fn peek(ctx: &mut LexingContext) -> char {
    peek_offset(ctx, 0)
}

fn advance(ctx: &mut LexingContext) -> char {
    ctx.current += 1;
    *ctx.source.get(ctx.current - 1).unwrap()
}

fn add_token(ctx: &mut LexingContext, kind: TokenKind) {
    let span = Span::new(ctx.start as u32, ctx.current as u32);
    ctx.tokens.push(Token::new(kind, span));
}

fn add_lookahead_conditional_token(
    ctx: &mut LexingContext,
    expect: char,
    first: TokenKind,
    second: TokenKind,
) {
    let kind = if peek(ctx) == expect {
        advance(ctx);
        first
    } else {
        second
    };
    add_token(ctx, kind);
}

fn get_lexeme(ctx: &mut LexingContext) -> String {
    ctx.source[ctx.start..ctx.current].iter().collect()
}

fn single_line_comment(ctx: &mut LexingContext) {
    while peek(ctx) != '\n' && !is_done(ctx) {
        advance(ctx);
    }
}

fn string(ctx: &mut LexingContext) {
    while peek(ctx) != '"' && !is_done(ctx) {
        if peek(ctx) == '\n' {
            ctx.line += 1;
        }
        advance(ctx);
    }

    if is_done(ctx) {
        panic!("Unterminated string");
    }

    //Consume closing "
    advance(ctx);
    let lexeme = get_lexeme(ctx);
    let string_content = lexeme[1..(lexeme.len()-1)].to_string();
    add_token(ctx, TokenKind::Lit(Literal::new_str(string_content)));
}

fn number(ctx: &mut LexingContext) {

    while peek(ctx).is_numeric() {
        advance(ctx);
    }

    let mut dot_encountered = false;
    if peek(ctx) == '.' && peek_offset(ctx, 1).is_numeric() {
        dot_encountered = true;
        advance(ctx);
        while peek(ctx).is_numeric() {
            advance(ctx);
        }
    }

    let lexeme = get_lexeme(ctx);
    if dot_encountered {
        add_token(ctx, TokenKind::Lit(Literal::new_float(lexeme)));
    } else {
        add_token(ctx, TokenKind::Lit(Literal::new_int(lexeme)));
    };

}

fn identifier(ctx: &mut LexingContext) {
    while peek(ctx).is_alphanumeric() || peek(ctx) == '_' {
        advance(ctx);
    }
    let lexeme = get_lexeme(ctx);
    if let Some(t) = is_keyword(lexeme.as_str()) {
        add_token(ctx, t);
    } else {
        let lexeme = get_lexeme(ctx);
        add_token(ctx, TokenKind::Identifier(lexeme));
    }
}

fn scan_token(ctx: &mut LexingContext) {
    use TokenKind::*;

    let c = advance(ctx);

    match c {
        '(' => add_token(ctx, OpenParen),
        ')' => add_token(ctx, CloseParen),
        '[' => add_token(ctx, OpenBracket),
        ']' => add_token(ctx, CloseBracket),
        '{' => add_token(ctx, OpenCurly),
        '}' => add_token(ctx, CloseCurly),
        '+' => add_token(ctx, Plus),
        '*' => add_token(ctx, Star),
        '%' => add_token(ctx, Percent),
        '^' => add_token(ctx, Hat),
        ';' => add_token(ctx, Semicolon),
        '.' => add_token(ctx, Dot),
        ',' => add_token(ctx, Comma),
        '-' => add_lookahead_conditional_token(ctx,'>', Arrow, Minus),
        ':' => add_lookahead_conditional_token(ctx, ':', ColonColon, Colon),
        '=' => add_lookahead_conditional_token(ctx, '=', EqualEqual, Equal),
        '!' => add_lookahead_conditional_token(ctx, '=', BangEqual, Bang),
        '&' => add_lookahead_conditional_token(ctx, '&', AndAnd, And),
        '|' => add_lookahead_conditional_token(ctx, '|', OrOr, Or),
        '<' => {
            let t = match peek(ctx) {
                '=' => {advance(ctx); LessEqual},
                '<' => {advance(ctx); LessLess},
                _ => Less
            };
            add_token(ctx,t);
        },
        '>' => {
            let t = match peek(ctx) {
                '=' => {advance(ctx); GreaterEqual},
                '>' => {advance(ctx); GreaterGreater},
                _ => Greater
            };
            add_token(ctx,t);
        },

        '/' => {
            let next = peek(ctx);
            if next == '/' {
                single_line_comment(ctx);
            } else {
                add_token(ctx, Slash);
            }
        }
        ' ' => {}
        '\t' => {}
        '\n' => ctx.line += 1,
        '"' => string(ctx),
        _ => {
            if c.is_numeric() {
                number(ctx);
            } else if c.is_alphabetic() || c == '_' {
                identifier(ctx);
            } else {
                panic!("Unexpected character {} on line {}", c, ctx.line);
            }
        }
    }
}

fn is_done(ctx: &LexingContext) -> bool {
    ctx.current >= ctx.source.len()
}

pub fn lex(source: String) -> Vec<Token> {
    let mut ctx = LexingContext::new(source);

    while !is_done(&ctx) {
        ctx.start = ctx.current;
        scan_token(&mut ctx);
    }
    ctx.tokens
}
