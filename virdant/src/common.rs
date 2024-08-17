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
pub enum SocketRole {
    Master,
    Slave,
}

pub fn pow(n: u64, k: u64) -> u64 {
    let mut p = 1;
    for _ in 0..k {
        p *= n
    }
    p
}

pub fn clog2(n: u64) -> u64 {
    let mut result = 0;
    while n > (1 << result) {
        result += 1;
    }
    result
}

pub fn is_pow2(n: u64) -> bool {
    n != 0 && (n & (n - 1)) == 0
}
