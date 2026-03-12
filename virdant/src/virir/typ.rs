use crate::virir::Width;

#[derive(Debug)]
pub enum Type {
    Bit,
    Word(Width),
    Clock,
}
