pub type WordVal = u64;
pub type Width = u64;
pub type Offset = u64;
pub type Tag = u64;
pub type StaticIndex = u64;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ComponentClass {
    Port,
    SubPort,
    Node,
    Reg,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Flow {
    Source,
    Sink,
    Duplex,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub enum DriverType {
    Continuous,
    Latched,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum ChannelDir {
    Mosi,
    Miso,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub enum PortRole {
    Master,
    Slave,
}
