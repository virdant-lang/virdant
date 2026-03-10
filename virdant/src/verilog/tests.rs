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
    ($( $stmt:expr ,)*) => {
        Element::Always(Always {
            stmts: vec![$( $stmt ),*],
        })
    }
}

#[macro_export]
macro_rules! display {
    ($( $expr:expr ),*) => {
        Stmt::Display(stmt::Display {
            exprs: vec![$( $expr ),*],
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
                            input!(a, 8),
                            input!(b, 8),
                            output!(z, 8),
                        }
                        elements {
                            assign!(out, binop!(refr!(a), Add, refr!(b))),
                            always![
                                display!(str!("Hello world!")),
                            ],
                        }
                    )
                ],
            },
        ],
    };

    verilog.write_to_stdout().unwrap();
}
