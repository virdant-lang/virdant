use std::sync::Arc;
use std::os::unix::ffi::OsStrExt;

use bstr::{BStr, BString};

use crate::fqn::PackageFqn;

/// A source file loaded into memory for use by the tokenizer with a given package name.
#[derive(Clone, Debug)]
pub struct Source {
    package: PackageFqn,
    text: BString,
}

/// A byte offset in a `Source`.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct SourceOffset(pub u32);

/// A line-col pair (1-indexed)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LineCol(usize, usize);

/// A start-end line-col pair
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span(LineCol, LineCol);

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Location {
    package: PackageFqn,
    linecol: LineCol,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Region {
    package: PackageFqn,
    span: Span,
}

impl Source {
    pub fn new(package: PackageFqn, text: BString) -> Source {
        Source { package, text }
    }

    pub fn load_file<P: AsRef<std::path::Path>>(filepath: P) -> Source {
        let package_name = BString::from(filepath.as_ref().file_stem().unwrap().as_bytes());
        let package = PackageFqn::new(package_name);
        let text = BString::new(std::fs::read(filepath).unwrap());
        Source::new(package, text)
    }

    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }

    pub fn text(&self) -> &BStr {
        BStr::new(&self.text)
    }

    pub fn str(&self) -> String {
        String::from_utf8_lossy(&self.text).to_string()
    }

    pub fn to_offset(&self, linecol: LineCol) -> SourceOffset {
        let mut offset = 0;
        let LineCol(mut line, mut col) = linecol;
        while line > 1 {
            if self.text[offset] == b'\n' {
                line -= 1;
            }
            offset += 1;
        }

        while col > 1 {
            offset += 1;
            col -= 1;
        }

        SourceOffset(offset.try_into().unwrap())
    }

    pub fn to_linecol(&self, offset: SourceOffset) -> LineCol {
        let mut line = 1;
        let mut col = 1;

        let mut i = 0;

        let SourceOffset(offset) = offset;
        while offset - i > 0 {
            if self.text[i as usize] == b'\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            i += 1;
        }

        LineCol(line, col)
    }

    pub fn to_region(&self, start: SourceOffset, end: SourceOffset) -> Region {
        let start_linecol = self.to_linecol(start);
        let end_linecol = self.to_linecol(end);
        let span = Span::new(start_linecol, end_linecol);
        Region::new(self.package.clone(), span)
    }

    pub fn summary(&self) -> String {
        use bstr::ByteSlice as _;

        let preview_len = self.text.len().min(20);
        if self.text.len() < 20 {
            let text = BStr::new(&self.text).to_str_lossy()
                .replace("\n", "\\n")
                .replace("\"", "\\\"");
            format!("[Source \"{}\" \"{}\"]", self.package, text)
        } else {
             let text = BStr::new(&self.text[0..preview_len]).to_str_lossy()
                .replace("\n", "\\n")
                .replace("\"", "\\\"");
            format!("[Source \"{}\" \"{}\"...]", self.package, text)
        }
    }
}

impl std::ops::Index<SourceOffset> for Source {
    type Output = u8;

    fn index(&self, index: SourceOffset) -> &Self::Output {
        &self.text[index.0 as usize]
    }
}

impl std::ops::Index<Span> for Source {
    type Output = [u8];

    fn index(&self, span: Span) -> &Self::Output {
        let start_idx = self.to_offset(span.start());
        let end_idx = self.to_offset(span.end());
        &self.text[start_idx.0 as usize..end_idx.0 as usize]
    }
}

impl std::ops::Add<u32> for SourceOffset {
    type Output = SourceOffset;

    fn add(self, rhs: u32) -> Self::Output {
        SourceOffset(self.0 + rhs)
    }
}

impl std::fmt::Debug for SourceOffset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SourceOffset({})", self.0)
    }
}

impl SourceOffset {
    pub fn new(pos: u32) -> Self {
        SourceOffset(pos)
    }
}

impl From<SourceOffset> for u32 {
    fn from(value: SourceOffset) -> Self {
        value.0
    }
}

impl From<SourceOffset> for usize {
    fn from(value: SourceOffset) -> Self {
        value.0 as usize
    }
}

impl PartialOrd for LineCol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.line() < other.line() {
            Some(std::cmp::Ordering::Less)
        } else if self.line() > other.line() {
            Some(std::cmp::Ordering::Greater)
        } else {
            if self.col() < other.col() {
                Some(std::cmp::Ordering::Less)
            } else if self.col() > other.col() {
                Some(std::cmp::Ordering::Greater)
            } else {
                Some(std::cmp::Ordering::Equal)
            }
        }
    }
}

impl Ord for LineCol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

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

    pub fn contains(&self, linecol: LineCol) -> bool {
        self.start() <= linecol && linecol < self.end()
    }

    /// Returns whether or not the span contains a second given span.
    ///
    /// This containment is inclusive, meaning the starts may coincide,
    /// and similarly, the ends may coincide.
    ///
    /// This containment relation is partial,
    /// since for spans which overlap only partly or not at all,
    /// neither contains the other.
    pub fn contains_span(&self, span: Span) -> bool {
        self.start() <= span.start() && span.end() <= self.end()
    }
}

impl Location {
    pub fn new(package: PackageFqn, linecol: LineCol) -> Self {
        Location { package, linecol }
    }

    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }

    pub fn linecol(&self) -> LineCol {
        self.linecol
    }

    pub fn line(&self) -> usize {
        self.linecol.line()
    }

    pub fn col(&self) -> usize {
        self.linecol.col()
    }
}

impl Region {
    pub fn new(package: PackageFqn, span: Span) -> Self {
        Region { package, span }
    }

    pub fn package(&self) -> PackageFqn {
        self.package.clone()
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn start(&self) -> LineCol {
        self.span.start()
    }

    pub fn end(&self) -> LineCol {
        self.span.end()
    }
}

impl std::fmt::Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.span.start().line() == self.span.end().line() {
            write!(
                f,
                "{}[{}:{}-{}]",
                self.package,
                self.span.start().line(),
                self.span.start().col(),
                self.span.end().col(),
            )
        } else {
            write!(
                f,
                "{}[{}:{}-{}:{}]",
                self.package,
                self.span.start().line(),
                self.span.start().col(),
                self.span.end().line(),
                self.span.end().col(),
            )
        }
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start().line() == self.end().line() {
            write!(
                f,
                "[{}:{}-{}]",
                self.start().line(),
                self.start().col(),
                self.end().col(),
            )
        } else {
            write!(
                f,
                "[{}:{}-{}:{}]",
                self.start().line(),
                self.start().col(),
                self.end().line(),
                self.end().col(),
            )
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start().line() == self.end().line() {
            write!(
                f,
                "[{}:{}-{}]",
                self.start().line(),
                self.start().col(),
                self.end().col(),
            )
        } else {
            write!(
                f,
                "[{}:{}-{}:{}]",
                self.start().line(),
                self.start().col(),
                self.end().line(),
                self.end().col(),
            )
        }
    }
}
