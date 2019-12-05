use codespan::Span;

#[derive(Debug, PartialEq, Clone)]
pub struct Location {
    pub begin: usize,
    pub end: usize
}

impl From<&Location> for Span {
    fn from(loc: &Location) -> Span {
        Span::new(loc.begin as u32, loc.end as u32)
    }
}
