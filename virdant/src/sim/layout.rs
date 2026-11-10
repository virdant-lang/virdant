//! Bit-layout for the simulator — now a thin shim over the shared
//! implementation in [`crate::typelayout`].
//!
//! The layout *rules* (one-hot union tags, enum tags, struct field
//! concatenation, validity) live in exactly one place,
//! `crate::typelayout`; this module only supplies the simulator's
//! seeding: the same construction core run over the elaborated
//! component types.

use indexmap::IndexMap;

use crate::analysis::elaboration::Elaboration;
use crate::db::Db;
use crate::types::typ::Type;
use crate::typelayout::TypeLayout;

pub use crate::typelayout::{TypeId, TypeInterner, TypeLayoutIndex};

impl TypeLayoutIndex {
    /// Build the bit-layout map for every user-defined type
    /// instantiation reachable from the elaborated component types,
    /// interning every type encountered (component types, layout keys,
    /// and member types) into `types`.  Returns the bare layout map;
    /// wrap it with [`TypeLayoutIndex::from_parts`] once expression
    /// conversion has finished interning the expression types.
    pub(super) fn build_layouts(
        db: &Db,
        elab: &Elaboration,
        types: &mut TypeInterner,
    ) -> IndexMap<Type, TypeLayout> {
        let seeds: Vec<Type> = elab
            .components()
            .iter()
            .map(|comp| comp.typ().clone())
            .collect();
        crate::typelayout::build_layouts(db, &seeds, types)
    }
}
