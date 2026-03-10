use super::*;

#[macro_export]
macro_rules! refr {
    ($name:ident) => {
        Expr::Reference(expr::Reference { name: stringify!($name).to_string() })
    };
    ($name:literal) => {
        Expr::Reference(expr::Reference { name: $name.to_string() })
    };
}

#[macro_export]
macro_rules! lit {
    ($value:literal, $width:literal) => {
        Expr::WordLit(expr::WordLit {
            value: $value,
            width: $width,
            radix: Radix::Hex,
        })
    };
}

#[macro_export]
macro_rules! str {
    ($value:literal) => {
        Expr::StrLit(expr::StrLit {
            value: $value.into(),
        })
    };
}

#[macro_export]
macro_rules! binop {
    ($lhs:expr, $op:ident, $rhs:expr) => {
        Expr::BinOp(expr::BinOp {
            op: BinOp::$op,
            lhs: $lhs.into(),
            rhs: $rhs.into(),
        })
    }
}

#[macro_export]
macro_rules! index {
    ($subject:expr, $index:literal) => {
        Expr::Index(expr::Index {
            subject: $subject.into(),
            index: refr!($index).into(),
        })
    };
    ($subject:expr, $index:expr) => {
        Expr::Index(expr::Index {
            subject: $subject.into(),
            index: $index.into(),
        })
    };
}

#[macro_export]
macro_rules! index_range {
    ($subject:expr, $index_hi:literal, $index_lo:literal) => {
        Expr::IndexRange(expr::IndexRange {
            subject: $subject.into(),
            index_hi: refr!($index_hi).into(),
            index_lo: refr!($index_lo).into(),
        })
    };
    ($subject:expr, $index_hi:literal, $index_lo:expr) => {
        Expr::IndexRange(expr::IndexRange {
            subject: $subject.into(),
            index_hi: refr!($index_hi).into(),
            index_lo: $index_lo.into(),
        })
    };
    ($subject:expr, $index_hi:expr, $index_lo:literal) => {
        Expr::IndexRange(expr::IndexRange {
            subject: $subject.into(),
            index_hi: $index_hi.into(),
            index_lo: refr!($index_lo).into(),
        })
    };
    ($subject:expr, $index_hi:expr, $index_lo:expr) => {
        Expr::IndexRange(expr::IndexRange {
            subject: $subject.into(),
            index_hi: $index_hi.into(),
            index_lo: $index_lo.into(),
        })
    };
}

#[macro_export]
macro_rules! input {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            dir: PortDir::Input,
            width: $width,
        }
    }
}

#[macro_export]
macro_rules! output {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            dir: PortDir::Output,
            width: $width,
        }
    }
}

#[macro_export]
macro_rules! assign {
    ($name:ident, $expr:expr) => {
        Element::Assign(Assign {
            name: stringify!($name).to_string(),
            expr: $expr.into(),
        })
    };
}

#[macro_export]
macro_rules! wire {
    ($name:ident, $width:literal) => {
        Element::Wire(Wire {
            name: stringify!($name).to_string(),
            width: $width,
            expr: None,
        })
    };
    ($name:ident, $width:literal, $expr:expr) => {
        Element::Wire(Wire {
            name: stringify!($name).to_string(),
            width: $width,
            expr: Some($expr.into()),
        })
    };
}


#[macro_export]
macro_rules! reg {
    ($name:ident, $width:literal) => {
        Element::Reg(Reg {
            name: stringify!($name).to_string(),
            width: $width,
            expr: None,
        })
    };
    ($name:ident, $width:literal, $expr:expr) => {
        Element::Reg(Reg {
            name: stringify!($name).to_string(),
            width: $width,
            expr: Some($expr.into()),
        })
    };
}

#[macro_export]
macro_rules! module {
    ($name:ident ports { $( $ports:expr ,)* } elements { $($elements:expr,)* }) => {
        Module {
            name: stringify!($name).to_string(),
            ports: vec![$( $ports ),*],
            elements: vec![$( $elements ),*],
        }
    }
}

#[macro_export]
macro_rules! always {
    (@ $clock:ident $( $stmt:expr ,)*) => {
        Element::Always(Always {
            clock: Some(stringify!($clock).into()),
            stmts: vec![$( $stmt ),*],
        })
    };
    (@* $( $stmt:expr ,)*) => {
        Element::Always(Always {
            clock: None,
            stmts: vec![$( $stmt ),*],
        })
    };
}

#[macro_export]
macro_rules! display {
    ($( $expr:expr ),*) => {
        Stmt::Display(stmt::Display {
            exprs: vec![$( $expr ),*],
        })
    }
}

#[macro_export]
macro_rules! assign_blocking {
    ($name:ident, $expr:expr) => {
        Stmt::AssignBlocking(stmt::AssignBlocking {
            name: stringify!($name).to_string(),
            expr: $expr.into(),
        })
    }
}

#[macro_export]
macro_rules! assign_non_blocking {
    ($name:ident, $expr:expr) => {
        Stmt::AssignNonBlocking(stmt::AssignNonBlocking {
            name: stringify!($name).to_string(),
            expr: $expr.into(),
        })
    }
}

#[rustfmt::skip]
#[test]
fn test_verilog() {
    let verilog = Verilog {
        files: vec![
            VerilogFile {
                name: "top.v".to_string(),
                modules: vec![
                    module!(
                        Top
                        ports {
                            input!(inp, 8),
                            output!(out, 1),
                        }
                        elements {
                            assign!(out, binop!(refr!(inp), Add, lit!(4095, 13))),
                            wire!(foo, 7, lit!(10, 7)),
                            wire!(bar, 1, lit!(0, 1)),
                            wire!(baz, 10),
                            reg!(beez, 10, refr!(foo)),
                        }
                    )
                ],
            },
            VerilogFile {
                name: "adder.v".to_string(),
                modules: vec![
                    module!(
                        Adder
                        ports {
                            input!(clock, 1),
                            input!(a, 8),
                            input!(b, 8),
                            output!(z, 8),
                        }
                        elements {
                            assign!(out, binop!(refr!(a), Add, refr!(b))),
                            reg!(x, 1),
                            reg!(y, 1),
                            always![
                                @*
                                display!(str!("Hello world!")),
                                assign_non_blocking!(x, lit!(1, 1)),
                            ],
                            always![
                                @clock
                                assign_blocking!(y, lit!(0, 1)),
                            ],
                        }
                    )
                ],
            },
            VerilogFile {
                name: "indexing.v".to_string(),
                modules: vec![
                    module!(
                        Indexing
                        ports {
                            input!(x, 8),
                            input!(y, 16),
                            output!(x0, 1),
                            output!(y_lo, 8),
                        }
                        elements {
                            assign!(x0, index!(refr!(x), 0)),
                            assign!(y_lo, index_range!(refr!(y), 7, 0)),
                        }
                    )
                ],
            },
        ],
    };

    verilog.write_to_stdout().unwrap();
}
