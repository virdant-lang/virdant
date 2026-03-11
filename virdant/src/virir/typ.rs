use crate::virir::Width;

pub enum Type {
    Bit,
    Word(Width),
    Clock,
}
