pub mod component;
pub mod location;
pub mod package;
pub mod symbols;
pub mod drivers;
pub mod elaboration;
pub mod ports;
pub mod structs;

pub use location::Location; // TODO remove these pub uses
pub use package::PackageAnalysis;
pub use ports::Port;
pub use structs::StructField;
