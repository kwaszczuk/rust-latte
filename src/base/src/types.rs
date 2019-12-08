use codespan::Span;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Location {
    pub begin: usize,
    pub end: usize
}

#[derive(Debug, PartialEq)]
pub struct Located<T> {
    pub all_loc: Location,
    pub value: T,
}

impl Location {
    pub fn new(begin: usize, end: usize) -> Self {
        Location {
            begin,
            end,
        }
    }
}

impl From<&Location> for Span {
    fn from(loc: &Location) -> Span {
        Span::new(loc.begin as u32, loc.end as u32)
    }
}
