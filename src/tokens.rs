#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Directive,
    EOF,
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
    Union,
    While,
    //Operators and punctuation
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
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Comma,
    Dot,
    Colon,
    ColonColon,
    Semicolon,
    Arrow,
    //Literals
    Integer,
    Float,
    String,
    True,
    False
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub line: usize,
}


pub fn is_keyword(s: &str) -> Option<TokenType> {
    use self::TokenType::*;

    // Apparently Rust does not yet support static const HashMaps?
    match s {
        "break" => Some(Break),
        "cast" => Some(Cast),
        "const" => Some(Const),
        "continue" => Some(Continue),
        "defer" => Some(Defer),
        "enum" => Some(Enum),
        "else" => Some(Else),
        "false" => Some(False),
        "for" => Some(For),
        "if" => Some(If),
        "return" => Some(Return),
        "struct" => Some(Struct),
        "union" => Some(Union),
        "true" => Some(True),
        "while" => Some(While),
        _ => None
    }
}

