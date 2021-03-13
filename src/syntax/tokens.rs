use crate::span::Span;
use super::literal::Literal;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    //Keywords
    Break,
    Cast,
    Const,
    Continue,
    Defer,
    Enum,
    Else,
    For,
    If,
    Return,
    Struct,
    While,
    //Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    And,
    AndAnd,
    Or,
    OrOr,
    LessLess,
    GreaterGreater,
    Hat,
    Equal,
    Bang,
    BangEqual,
    EqualEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    //Punctuation
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurly,
    CloseCurly,
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    //Other
    Identifier(String),
    Lit(Literal),
    EOF,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {

    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token {
            kind,
            span,
        }
    }
}


pub fn is_keyword(s: &str) -> Option<TokenKind> {
    use self::TokenKind::*;

    // Apparently Rust does not yet support static const HashMaps?
    match s {
        "break" => Some(Break),
        "cast" => Some(Cast),
        "const" => Some(Const),
        "continue" => Some(Continue),
        "defer" => Some(Defer),
        "enum" => Some(Enum),
        "else" => Some(Else),
        "false" => Some(Lit(Literal::new_bool(s.to_owned()))),
        "for" => Some(For),
        "if" => Some(If),
        "return" => Some(Return),
        "struct" => Some(Struct),
        "true" => Some(Lit(Literal::new_bool(s.to_owned()))),
        "while" => Some(While),
        _ => None
    }
}

