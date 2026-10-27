//! Test-helper macros (`refr!`, `lit!`, `str!`, `binop!`, `unop!`,
//! `if_!`, `concat!`, `repeat!`) for concisely constructing Verilog IR
//! expression trees in unit tests.

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
            width: 0,
        })
    }
}

#[macro_export]
macro_rules! unop {
    ($op:ident, $expr:expr) => {
        Expr::UnOp(expr::UnOp {
            op: UnOp::$op,
            expr: $expr.into(),
            width: 0,
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
            width: 0,
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

use super::*;

use crate::{binop, index_range, lit, refr};

/// An `IndexRange` with constant bounds has a known width of
/// `index_hi - index_lo + 1` (the emitter stores `index_hi` as the
/// Verilog MSB, already decremented from the Virdant `hi` bound).
#[test]
fn compute_expr_width_knows_constant_index_range() {
    let expr = index_range!(refr!(w), lit!(11, 32), lit!(3, 32));
    assert_eq!(compute_expr_width(&expr), Some(9));
}

/// A slice whose subject is itself a slice must be lifted into a temp wire,
/// so the emitted Verilog applies exactly one range-select to a plain
/// identifier instead of an illegal chained part-select.
#[test]
fn slice_of_slice_subject_is_lifted_into_temp_wire() {
    let mut module = Module {
        name: "Top".to_string(),
        is_ext: false,
        ports: vec![],
        elements: vec![Element::Assign(Assign {
            name: "chained".to_string(),
            expr: index_range!(index_range!(refr!(w), lit!(11, 32), lit!(3, 32)), lit!(5, 32), lit!(4, 32)),
            strength: crate::common::Strength::Strong,
        })],
        origin: None,
    };
    module.normalize();

    // The original assign now selects on a plain temp-wire reference.
    let last = module.elements.last().unwrap();
    match last {
        Element::Assign(a) => {
            assert_eq!(a.name, "chained");
            match &a.expr {
                // Exactly one range-select applied to a plain identifier.
                Expr::IndexRange(r) => assert!(matches!(r.subject.as_ref(), Expr::Reference(_)),
                    "expected temp-wire subject, got {a:?}"),
                other => panic!("expected IndexRange, got {other:?}"),
            }
        }
        other => panic!("unexpected trailing element {other:?}"),
    }

    // A temp wire (width 9) and its defining assign were inserted before it.
    let mut temp_wires = module.elements.iter().filter_map(|e| match e {
        Element::Wire(w) => Some(w),
        _ => None,
    });
    let wire = temp_wires.next().expect("expected a temp wire");
    assert_eq!(wire.width, 9);

    let temp_assigns = module.elements.iter().filter_map(|e| match e {
        Element::Assign(a) => Some(a),
        _ => None,
    }).collect::<Vec<_>>();
    assert_eq!(temp_assigns.len(), 2, "expected original assign plus one temp assign");
    match &temp_assigns[0].expr {
        Expr::IndexRange(r) => {
            assert!(matches!(r.subject.as_ref(), Expr::Reference(_)));
        }
        other => panic!("expected inner IndexRange in temp assign, got {other:?}"),
    }
}

/// An `Index`/`IndexRange` whose subject is any known-width non-`Reference`
/// expression (here, an arithmetic expression) is lifted into a temp wire,
/// never emitted as a select on a parenthesized expression.
#[test]
fn operator_subject_of_slice_is_lifted_into_temp_wire() {
    let mut module = Module {
        name: "Top".to_string(),
        is_ext: false,
        ports: vec![],
        elements: vec![Element::Assign(Assign {
            name: "c".to_string(),
            // The conversion pass records the operand width on operator
            // nodes; simulate a Word[7] `b ^ d` result here.
            expr: index_range!(Expr::BinOp(expr::BinOp {
                op: BinOp::BitXor,
                lhs: refr!(b).into(),
                rhs: refr!(d).into(),
                width: 7,
            }), lit!(8, 32), lit!(2, 32)),
            strength: crate::common::Strength::Strong,
        })],
        origin: None,
    };
    module.normalize();

    let last = module.elements.last().unwrap();
    match last {
        Element::Assign(a) => match &a.expr {
            Expr::IndexRange(r) => {
                assert!(matches!(r.subject.as_ref(), Expr::Reference(_)),
                    "expected temp-wire subject, got {a:?}");
            }
            other => panic!("expected IndexRange, got {other:?}"),
        },
        other => panic!("unexpected trailing element {other:?}"),
    }

    // The temp wire carries the BinOp (width 7 from the BinOp node).
    let temp_wire = module.elements.iter().find_map(|e| match e {
        Element::Wire(w) if w.name.contains("temp") => Some(w),
        _ => None,
    }).expect("expected a temp wire");
    assert_eq!(temp_wire.width, 7);
    match &temp_wire.expr {
        None => {}
        other => panic!("temp wire should have no initial expr, got {other:?}"),
    }
}
