#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pos(u32);

impl Pos {
    pub fn new(pos: u32) -> Self {
        Pos(pos)
    }
}

impl From<Pos> for u32 {
    fn from(value: Pos) -> Self {
        value.0
    }
}

impl From<Pos> for usize {
    fn from(value: Pos) -> Self {
        value.0 as usize
    }
}

/// A line-col pair (1-indexed)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineCol(usize, usize);

/// A start-end position pair
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span(LineCol, LineCol);

impl LineCol {
    pub fn new(line: usize, col: usize) -> Self {
        LineCol(line, col)
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
    pub fn new(start: LineCol, end: LineCol) -> Self {
        Span(start, end)
    }

    /// The start position.
    pub fn start(&self) -> LineCol {
        self.0
    }

    /// The end position.
    pub fn end(&self) -> LineCol {
        self.1
    }
}
