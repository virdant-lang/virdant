#![allow(unused, dead_code, unused_variables)] // TODO

use bstr::BStr;

use crate::ast::*;
use crate::fqn::*;
use crate::source::*;

pub struct Vir {
    asts: Vec<Ast>,
}

impl Vir {
    pub fn new() -> Vir {
        Vir { asts: vec![] }
    }

    pub fn add_package_source(&mut self, package: PackageFqn, text: &BStr) -> anyhow::Result<()> {
        let source = Source::new(package.clone(), text);
        Ok(())
    }
}
