

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LiteralKind {
    Bool,
    Int,
    Float,
    Str
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Literal {
    pub kind: LiteralKind,
    pub lexeme: String
}

impl Literal {

    pub fn new_bool(lexeme: String) -> Literal {
        Literal {
            kind: LiteralKind::Bool,
            lexeme
        }
    }

    pub fn new_int(lexeme: String) -> Literal {
        Literal {
            kind: LiteralKind::Int,
            lexeme
        }
    }

    pub fn new_float(lexeme: String) -> Literal {
        Literal {
            kind: LiteralKind::Float,
            lexeme
        }
    }

    pub fn new_str(lexeme: String) -> Literal {
        Literal {
            kind: LiteralKind::Str,
            lexeme
        }
    }
}