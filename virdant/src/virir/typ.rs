use crate::virir::Width;

#[derive(Debug)]
pub enum Type {
    Bit,
    Word(Width),
    Clock,
}

impl Type {
    pub fn width(&self) -> Width {
        match self {
            Type::Bit | Type::Clock => 1,
            Type::Word(width) => *width,
        }
    }
}
