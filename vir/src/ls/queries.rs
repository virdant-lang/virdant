//! The language-query core shared by the `vir-ls` LSP server and the
//! `vir ls` CLI query subcommands.
//!
//! Each query function takes the analysis `Db` (plus whatever targets
//! the caller resolved) and returns owned data; the LSP handlers and
//! the CLI both render from the same core, so the CLI's JSON output is
//! the canonical form of what the server serves.  Project loading goes
//! through [`Project`], which mirrors the standard `vir` project
//! discovery (`virdant.toml` + `src/`).
//!
//! Each query lives in its own submodule under `queries/`; this module
//! root re-exports them all so callers can use flat `queries::...`
//! paths.

mod components;
mod definition;
mod diagnostics;
mod hover;
mod location;
mod name;
mod project;
mod references;
mod symbol_at;
mod symbols;
mod type_of;
mod types;

pub use components::components;
pub use definition::resolve_definition_at;
pub use diagnostics::diagnostics;
pub use hover::hover_at;
pub use location::{location_json, span_json};
pub use name::{NameResolution, resolve_name};
pub use project::Project;
pub use references::references;
pub use symbol_at::resolve_symbol_at;
pub use symbols::symbols;
pub use type_of::typeof_at;
pub use types::type_to_source;
