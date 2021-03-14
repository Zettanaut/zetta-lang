

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub from: u32,
    pub to: u32
}

impl Span {
    pub fn new(from: u32, to: u32) -> Span {
        Span {
            from, to
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct Symbol(u32);