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
macro_rules! unop {
    ($op:ident, $expr:expr) => {
        Expr::UnOp(expr::UnOp {
            op: UnOp::$op,
            expr: $expr.into(),
        })
    }
}

#[macro_export]
macro_rules! if_ {
    ($cond:expr, $then_expr:expr, $else_expr:expr) => {
        Expr::If(expr::If {
            cond: $cond.into(),
            then_expr: $then_expr.into(),
            else_expr: $else_expr.into(),
        })
    }
}

#[macro_export]
macro_rules! concat {
    ($( $expr:expr ),+ $(,)?) => {
        Expr::Concat(expr::Concat {
            exprs: vec![$( $expr.into() ),*],
            width: 0,
        })
    }
}

#[macro_export]
macro_rules! repeat {
    ($count:literal; $( $expr:expr ),+ $(,)?) => {
        Expr::Repeat(expr::Repeat {
            count: refr!($count).into(),
            exprs: vec![$( $expr.into() ),*],
            width: 0,
        })
    };
    ($count:expr; $( $expr:expr ),+ $(,)?) => {
        Expr::Repeat(expr::Repeat {
            count: $count.into(),
            exprs: vec![$( $expr.into() ),*],
            width: 0,
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
        input_wire!($name, $width)
    }
}

#[macro_export]
macro_rules! input_wire {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            kind: PortKind::Wire,
            dir: PortDir::Input,
            width: $width,
        }
    }
}

#[macro_export]
macro_rules! input_reg {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            kind: PortKind::Reg,
            dir: PortDir::Input,
            width: $width,
        }
    }
}

#[macro_export]
macro_rules! output {
    ($name:ident, $width:literal) => {
        output_wire!($name, $width)
    }
}

#[macro_export]
macro_rules! output_wire {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            kind: PortKind::Wire,
            dir: PortDir::Output,
            width: $width,
        }
    }
}

#[macro_export]
macro_rules! output_reg {
    ($name:ident, $width:literal) => {
        Port {
            name:stringify!($name).to_string(),
            kind: PortKind::Reg,
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
            clock: Some(refr!($clock)),
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
macro_rules! initial {
    ($( $stmt:expr ,)*) => {
        Element::Initial(Initial {
            stmts: vec![$( $stmt ),*],
        })
    };
}

#[macro_export]
macro_rules! case {
    ($subject:expr, $( $item:expr ),+ $(,)?) => {
        Stmt::Case(stmt::Case {
            subject: $subject.into(),
            items: vec![$( $item ),*],
        })
    };
}

#[macro_export]
macro_rules! casez {
    ($subject:expr, $( $item:expr ),+ $(,)?) => {
        Stmt::CaseZ(stmt::CaseZ {
            subject: $subject.into(),
            items: vec![$( $item ),*],
        })
    };
}

#[macro_export]
macro_rules! case_item {
    (default => $( $stmt:expr ),+ $(,)?) => {
        stmt::CaseItem {
            pattern: stmt::CasePattern::Default,
            stmts: vec![$( $stmt ),*],
        }
    };
    ($pattern:expr => $( $stmt:expr ),+ $(,)?) => {
        stmt::CaseItem {
            pattern: $pattern,
            stmts: vec![$( $stmt ),*],
        }
    };
}

#[macro_export]
macro_rules! case_expr {
    ($expr:expr) => {
        stmt::CasePattern::Expr($expr.into())
    };
}

#[macro_export]
macro_rules! pat {
    ($pattern:literal, $width:literal, $radix:ident) => {
        stmt::CasePattern::PatternLit(stmt::PatternLit {
            width: $width,
            radix: Radix::$radix,
            pattern: $pattern.to_string(),
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
macro_rules! assert_stmt {
    ($( $expr:expr ),*) => {
        Stmt::Assert(stmt::Assert {
            exprs: vec![$( $expr ),*],
        })
    }
}

#[macro_export]
macro_rules! finish {
    () => {
        Stmt::Finish
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
                            assign!(z, binop!(refr!(a), Add, refr!(b))),
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
            VerilogFile {
                name: "ternary.v".to_string(),
                modules: vec![
                    module!(
                        Ternary
                        ports {
                            input!(sel, 1),
                            input!(a, 8),
                            input!(b, 8),
                            output!(z, 8),
                        }
                        elements {
                            assign!(z, if_!(refr!(sel), refr!(a), refr!(b))),
                        }
                    )
                ],
            },
            VerilogFile {
                name: "initial.v".to_string(),
                modules: vec![
                    module!(
                        InitialExample
                        ports {
                            output_reg!(done, 1),
                        }
                        elements {
                            initial![
                                assign_blocking!(done, lit!(0, 1)),
                                assign_blocking!(done, lit!(1, 1)),
                            ],
                        }
                    )
                ],
            },
            VerilogFile {
                name: "binops.v".to_string(),
                modules: vec![
                    module!(
                        BinOps
                        ports {
                            input!(a, 8),
                            input!(b, 8),
                            input!(shamt, 4),
                            output!(pow, 16),
                            output!(mul, 16),
                            output!(div, 8),
                            output!(modulo, 8),
                            output!(add, 8),
                            output!(sub, 8),
                            output!(shl, 8),
                            output!(shr, 8),
                            output!(ashl, 8),
                            output!(ashr, 8),
                            output!(lt, 1),
                            output!(le, 1),
                            output!(gt, 1),
                            output!(ge, 1),
                            output!(eq, 1),
                            output!(ne, 1),
                            output!(case_eq, 1),
                            output!(case_ne, 1),
                            output!(bit_and, 8),
                            output!(bit_xor, 8),
                            output!(bit_xnor, 8),
                            output!(bit_or, 8),
                            output!(log_and, 1),
                            output!(log_or, 1),
                        }
                        elements {
                            assign!(pow, binop!(refr!(a), Pow, refr!(b))),
                            assign!(mul, binop!(refr!(a), Mul, refr!(b))),
                            assign!(div, binop!(refr!(a), Div, refr!(b))),
                            assign!(modulo, binop!(refr!(a), Mod, refr!(b))),
                            assign!(add, binop!(refr!(a), Add, refr!(b))),
                            assign!(sub, binop!(refr!(a), Sub, refr!(b))),
                            assign!(shl, binop!(refr!(a), Shl, refr!(shamt))),
                            assign!(shr, binop!(refr!(a), Shr, refr!(shamt))),
                            assign!(ashl, binop!(refr!(a), AShl, refr!(shamt))),
                            assign!(ashr, binop!(refr!(a), AShr, refr!(shamt))),
                            assign!(lt, binop!(refr!(a), Lt, refr!(b))),
                            assign!(le, binop!(refr!(a), Lte, refr!(b))),
                            assign!(gt, binop!(refr!(a), Gt, refr!(b))),
                            assign!(ge, binop!(refr!(a), Gte, refr!(b))),
                            assign!(eq, binop!(refr!(a), Eq, refr!(b))),
                            assign!(ne, binop!(refr!(a), Ne, refr!(b))),
                            assign!(case_eq, binop!(refr!(a), CaseEq, refr!(b))),
                            assign!(case_ne, binop!(refr!(a), CaseNe, refr!(b))),
                            assign!(bit_and, binop!(refr!(a), BitAnd, refr!(b))),
                            assign!(bit_xor, binop!(refr!(a), BitXor, refr!(b))),
                            assign!(bit_xnor, binop!(refr!(a), BitXnor, refr!(b))),
                            assign!(bit_or, binop!(refr!(a), BitOr, refr!(b))),
                            assign!(log_and, binop!(refr!(a), LogAnd, refr!(b))),
                            assign!(log_or, binop!(refr!(a), LogOr, refr!(b))),
                        }
                    )
                ],
            },
            VerilogFile {
                name: "unops.v".to_string(),
                modules: vec![
                    module!(
                        UnaryOps
                        ports {
                            input!(a, 8),
                            output!(pos, 8),
                            output!(neg, 8),
                            output!(log_not, 1),
                            output!(bit_not, 8),
                            output!(red_and, 1),
                            output!(red_nand, 1),
                            output!(red_or, 1),
                            output!(red_nor, 1),
                            output!(red_xor, 1),
                            output!(red_xnor, 1),
                        }
                        elements {
                            assign!(pos, unop!(Pos, refr!(a))),
                            assign!(neg, unop!(Neg, refr!(a))),
                            assign!(log_not, unop!(LogNot, refr!(a))),
                            assign!(bit_not, unop!(BitNot, refr!(a))),
                            assign!(red_and, unop!(RedAnd, refr!(a))),
                            assign!(red_nand, unop!(RedNand, refr!(a))),
                            assign!(red_or, unop!(RedOr, refr!(a))),
                            assign!(red_nor, unop!(RedNor, refr!(a))),
                            assign!(red_xor, unop!(RedXor, refr!(a))),
                            assign!(red_xnor, unop!(RedXnor, refr!(a))),
                        }
                    )
                ],
            },
            VerilogFile {
                name: "concat_repeat.v".to_string(),
                modules: vec![
                    module!(
                        ConcatRepeat
                        ports {
                            input!(a, 8),
                            input!(b, 8),
                            input!(c, 8),
                            output!(abc, 24),
                            output!(abab, 16),
                        }
                        elements {
                            assign!(abc, concat!(refr!(a), refr!(b), refr!(c))),
                            assign!(abab, repeat!(2; refr!(a), refr!(b))),
                        }
                    )
                ],
            },
            VerilogFile {
                name: "cases.v".to_string(),
                modules: vec![
                    module!(
                        Cases
                        ports {
                            input!(clock, 1),
                            input!(sel, 2),
                            input!(state, 16),
                            output_reg!(out, 8),
                            output_reg!(match_out, 1),
                        }
                        elements {
                            always![
                                @*
                                case!(
                                    refr!(sel),
                                    case_item!(case_expr!(lit!(0, 2)) => assign_blocking!(out, lit!(0, 8))),
                                    case_item!(case_expr!(lit!(1, 2)) => assign_blocking!(out, lit!(1, 8))),
                                    case_item!(default => assign_blocking!(out, lit!(2, 8))),
                                ),
                            ],
                            always![
                                @clock
                                casez!(
                                    refr!(state),
                                    case_item!(pat!("??????????????00", 16, Bin) => assign_non_blocking!(match_out, lit!(0, 1))),
                                    case_item!(pat!("??????????????01", 16, Bin) => assign_non_blocking!(match_out, lit!(1, 1))),
                                    case_item!(pat!("??????????????10", 16, Bin) => assign_non_blocking!(match_out, lit!(0, 1))),
                                    case_item!(pat!("??????????????11", 16, Bin) => assign_non_blocking!(match_out, lit!(1, 1))),
                                    case_item!(default => assign_non_blocking!(match_out, lit!(0, 1))),
                                ),
                            ],
                        }
                    )
                ],
            },
            VerilogFile {
                name: "commands.v".to_string(),
                modules: vec![
                    module!(
                        Commands
                        ports {
                        }
                        elements {
                            reg!(done, 1, lit!(0, 1)),
                            initial![
                                display!(str!("commands")),
                                assert_stmt!(binop!(refr!(done), Eq, lit!(0, 1))),
                                assign_non_blocking!(done, lit!(1, 1)),
                                assert_stmt!(binop!(refr!(done), Eq, lit!(1, 1))),
                                Stmt::Fatal,
                                finish!(),
                            ],
                        }
                    )
                ],
            },
            VerilogFile {
                name: "submodules.v".to_string(),
                modules: vec![
                    module!(
                        ByteAdder
                        ports {
                            input!(a, 8),
                            input!(b, 8),
                            output!(sum, 8),
                        }
                        elements {
                            assign!(sum, binop!(refr!(a), Add, refr!(b))),
                        }
                    ),
                    module!(
                        SubmoduleShowcase
                        ports {
                            input!(lhs, 8),
                            input!(rhs, 8),
                            output!(sum, 8),
                        }
                        elements {
                            wire!(sum_from_adder, 8),
                            Element::Submodule(Submodule {
                                name: "u_adder".to_string(),
                                submodule_name: "ByteAdder".to_string(),
                                connects: vec![
                                    ("a".to_string(), "lhs".to_string()),
                                    ("b".to_string(), "rhs".to_string()),
                                    ("sum".to_string(), "sum_from_adder".to_string()),
                                ],
                            }),
                            assign!(sum, refr!(sum_from_adder)),
                        }
                    ),
                ],
            },
        ],
    };

    verilog.write_to_stdout().unwrap();
}
