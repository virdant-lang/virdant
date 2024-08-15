/// A line-col pair (1-indexed)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos(usize, usize);

/// A start-end position pair
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span(Pos, Pos);

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos(line, col)
    }

    /// The line numbrer (1-indexed).
    pub fn line(&self) -> usize {
        self.0
    }

    /// The column numbrer (1-indexed).
    pub fn col(&self) -> usize {
        self.1
    }
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span(start, end)
    }

    /// The start position.
    pub fn start(&self) -> Pos {
        self.0
    }

    /// The end position.
    pub fn end(&self) -> Pos {
        self.1
    }
}
