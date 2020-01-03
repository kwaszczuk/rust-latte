use codespan::Span;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Location {
    pub begin: usize,
    pub end: usize
}

#[derive(Debug, PartialEq, Clone)]
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

pub trait Labeled {
    fn new(prefix: String, counter: usize) -> Self;
}

pub struct Labeler<T: Labeled> {
    pub prefix: String,
    pub counter: usize,
    dummy: T,
}

impl<T: Labeled> Labeler<T> {
    pub fn new(prefix: String) -> Self {
        Labeler {
            prefix: prefix.clone(),
            counter: 0,
            dummy: T::new(prefix.clone(), 0),
        }
    }
    pub fn set(&mut self, counter: usize) {
        self.counter = counter;
    }

    pub fn reset(&mut self) {
        self.set(0);
    }

    pub fn next(&mut self) -> T {
        let ctr = self.counter.clone();
        self.counter += 1;
        T::new(self.prefix.clone(), ctr)
    }
}
