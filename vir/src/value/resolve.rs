//! Name resolution for `vir value`: type spellings, constructor
//! lookup, and struct lookup against a project `Db`.
//!
//! Type names are fully-qualified (`pkg::Name`), except names from the
//! `builtin` package, which are bare (`Bit`, `Word[8]`,
//! `Valid[Word[8]]`).  Resolution goes through
//! `SymbolTable::resolve_item`, which already implements exactly this
//! rule; bare names that do not resolve inside `builtin` are rejected.

use std::sync::Arc;

use bstr::{BString, ByteSlice};

use virdant::analysis::symbols::{Symbol, SymbolId, SymbolKind, SymbolTable};
use virdant::db::Db;
use virdant::types::typ::{Kind, Type};
use virdant::types::typedef::TypeDef;

use crate::ast::{VType, VTypeArg};

/// The maximum width expressible in a type spelling (`Width` is u16).
const MAX_WIDTH: u128 = u16::MAX as u128;

/// Shared empty-args slice (avoids a temp in `unwrap_or`).
static EMPTY_ARGS: Vec<VTypeArg> = Vec::new();

pub struct Resolver<'a> {
    db: &'a Db,
    symboltable: Arc<SymbolTable>,
}

/// A resolved constructor: its containing union/enum typedef and its
/// parameter types *as written* (with `Param` references into the
/// typedef's parameter list).
pub struct CtorDef {
    pub typedef_sym: SymbolId,
    pub typedef: TypeDef,
    /// Parameter types as written, in declaration order.
    pub param_types: Vec<Type>,
}

impl<'a> Resolver<'a> {
    pub fn new(db: &'a Db) -> Resolver<'a> {
        Resolver { db, symboltable: db.get_symboltable() }
    }

    /// The underlying `Db` (for symbol-table access in messages).
    pub fn db(&self) -> &'a Db {
        self.db
    }

    /// Resolve a type spelling to a semantic `Type`.
    ///
    /// Bare names resolve in the `builtin` package only; everything
    /// else must be written `pkg::Name`.  Argument counts and kinds
    /// (`Type` vs `Nat`) are checked against the def's declared
    /// parameters.
    pub fn resolve_type(&self, vt: &VType) -> Result<Type, String> {
        let symbol = match vt {
            VType::Qualified { package, name, .. } => {
                let fqn = format!("{package}::{name}");
                self.symboltable
                    .resolve_item_fqn(fqn.as_bytes().as_bstr())
                    .cloned()
                    .ok_or_else(|| format!("unknown type '{fqn}'"))?
            }
            VType::Bare { name, .. } => self
                .symboltable
                .resolve_item(name.as_bytes().as_bstr(), self.db.get_packages().builtin())
                .cloned()
                .ok_or_else(|| {
                    format!(
                        "unknown type '{name}' (non-builtin types must be fully qualified: 'pkg::{name}')"
                    )
                })?,
        };

        // The primitive builtins.
        let fqn = symbol.fqn().to_str_lossy().into_owned();
        match fqn.as_str() {
            "builtin::Bit" => return self.no_args_primitive(vt, symbol, "Bit").map(|_| Type::Bit),
            "builtin::Clock" => {
                return self.no_args_primitive(vt, symbol, "Clock").map(|_| Type::Clock)
            }
            "builtin::Reset" => {
                return self.no_args_primitive(vt, symbol, "Reset").map(|_| Type::Reset)
            }
            "builtin::Word" => return self.resolve_word(vt, &symbol),
            _ => {}
        }

        // A user-defined (or library) type.
        if !matches!(
            symbol.kind(),
            SymbolKind::UnionDef | SymbolKind::StructDef | SymbolKind::EnumDef | SymbolKind::BuiltinDef
        ) {
            return Err(format!("'{fqn}' is not a type"));
        }
        let typedef = self.db.get_typedef(symbol.id());
        let args = self.resolve_args(vt, &typedef)?;
        Ok(Type::Usual(symbol.id(), args))
    }

    fn no_args_primitive(&self, vt: &VType, symbol: Symbol, name: &str) -> Result<(), String> {
        let args = type_args(vt).unwrap_or(&EMPTY_ARGS);
        if !args.is_empty() {
            return Err(format!(
                "type {name} takes no parameters (got {})",
                args.len()
            ));
        }
        let _ = symbol;
        Ok(())
    }

    fn resolve_word(&self, vt: &VType, symbol: &Symbol) -> Result<Type, String> {
        let args = type_args(vt).unwrap_or(&EMPTY_ARGS);
        if args.len() != 1 {
            return Err(format!(
                "type Word takes exactly one width parameter (got {})",
                args.len()
            ));
        }
        match &args[0] {
            VTypeArg::Nat(w) => {
                let width = nat_width(*w, "Word")?;
                let _ = symbol;
                Ok(Type::Word(width))
            }
            VTypeArg::Type(_) => Err("Word's width parameter must be a Nat literal".to_string()),
        }
    }

    /// Resolve the type arguments written on an instantiation against
    /// the def's declared generic parameters: counts and kinds.
    fn resolve_args(&self, vt: &VType, typedef: &TypeDef) -> Result<Vec<Type>, String> {
        let args = type_args(vt).unwrap_or(&EMPTY_ARGS);
        let params = &typedef.params;
        if args.len() != params.len() {
            let def_name = self
                .symboltable
                .symbol(typedef.symbol_id)
                .fqn()
                .to_str_lossy()
                .into_owned();
            return Err(format!(
                "type {def_name} expects {} parameter{}, got {}",
                params.len(),
                if params.len() == 1 { "" } else { "s" },
                args.len(),
            ));
        }
        let mut out = Vec::with_capacity(args.len());
        for (param, arg) in params.iter().zip(args.iter()) {
            match (param.kind, arg) {
                (Kind::Type, VTypeArg::Type(inner)) => {
                    out.push(self.resolve_type(inner)?);
                }
                (Kind::Type, VTypeArg::Nat(_)) => {
                    return Err(format!(
                        "parameter '{}' of type '{}' expects a type, got a Nat literal",
                        param.name.to_str_lossy(),
                        typedef_name(&self.symboltable, typedef.symbol_id),
                    ));
                }
                (Kind::Nat, VTypeArg::Nat(w)) => {
                    let width = nat_width(*w, &param.name.to_str_lossy())?;
                    out.push(Type::Word(width));
                }
                (Kind::Nat, VTypeArg::Type(_)) => {
                    return Err(format!(
                        "parameter '{}' of type '{}' expects a Nat literal, got a type",
                        param.name.to_str_lossy(),
                        typedef_name(&self.symboltable, typedef.symbol_id),
                    ));
                }
            }
        }
        Ok(out)
    }

    /// Find a union/enum constructor (or enumerant) by simple name and
    /// arity, searching every typedef in every package.  The value
    /// grammar uses bare constructor names (`@Valid`), so an ambiguous
    /// or missing name is an error listing the candidates.
    pub fn find_ctor(&self, name: &str, arity: usize) -> Result<CtorDef, String> {
        let mut candidates: Vec<(Symbol, Symbol, Vec<Type>)> = Vec::new();
        for typedef_sym in self.symboltable.typedefs() {
            if !matches!(typedef_sym.kind(), SymbolKind::UnionDef | SymbolKind::EnumDef) {
                continue;
            }
            for slot in self.symboltable.slots(typedef_sym.id()) {
                if slot.name().as_bstr() != name.as_bytes() {
                    continue;
                }
                let sig = self.db.get_ctor_signature(slot.id());
                if sig.parameters.len() != arity {
                    continue;
                }
                candidates.push((
                    typedef_sym.clone(),
                    slot.clone(),
                    sig.parameters.iter().map(|(_n, t)| t.clone()).collect(),
                ));
            }
        }
        match candidates.len() {
            0 => Err(format!("no constructor named '@{name}'")),
            1 => {
                let (typedef_sym, _ctor_sym, param_types) = candidates.pop().unwrap();
                let typedef = self.db.get_typedef(typedef_sym.id()).as_ref().clone();
                Ok(CtorDef { typedef_sym: typedef_sym.id(), typedef, param_types })
            }
            n => {
                let mut names: Vec<BString> = candidates
                    .iter()
                    .map(|(td, _, _)| td.fqn().to_owned())
                    .collect();
                names.sort();
                let names: Vec<String> =
                    names.iter().map(|f| f.to_str_lossy().into_owned()).collect();
                Err(format!(
                    "constructor '@{name}' is ambiguous ({n} candidates): {}",
                    names.join(", ")
                ))
            }
        }
    }

    /// Find a struct typedef by simple name (for `${Name(...)}` struct
    /// literals).  Bare names search every package; an ambiguous name
    /// is an error listing the candidates.
    pub fn find_struct(&self, name: &str) -> Result<SymbolId, String> {
        let matches: Vec<Symbol> = self
            .symboltable
            .typedefs()
            .into_iter()
            .filter(|sym| sym.kind() == SymbolKind::StructDef && sym.name().as_bstr() == name.as_bytes())
            .collect();
        match matches.len() {
            0 => Err(format!("no struct type named '{name}'")),
            1 => Ok(matches[0].id()),
            n => {
                let mut names: Vec<BString> =
                    matches.iter().map(|sym| sym.fqn().to_owned()).collect();
                names.sort();
                let names: Vec<String> =
                    names.iter().map(|f| f.to_str_lossy().into_owned()).collect();
                Err(format!(
                    "struct '{name}' is ambiguous ({n} candidates): {}",
                    names.join(", ")
                ))
            }
        }
    }
}

fn type_args(vt: &VType) -> Option<&Vec<VTypeArg>> {
    match vt {
        VType::Bare { args, .. } | VType::Qualified { args, .. } => Some(args),
    }
}

fn typedef_name(symboltable: &SymbolTable, sym: SymbolId) -> String {
    symboltable.symbol(sym).fqn().to_str_lossy().into_owned()
}

fn nat_width(w: u128, what: &str) -> Result<virdant::common::Width, String> {
    if w > MAX_WIDTH {
        return Err(format!(
            "width parameter of '{what}' is too large ({w}; maximum {MAX_WIDTH})"
        ));
    }
    Ok(w as virdant::common::Width)
}
