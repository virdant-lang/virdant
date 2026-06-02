//! virdant-db: A build-time incremental database/query system generator
//!
//! This crate provides a framework similar to salsa for creating
//! incremental computation systems with automatic dependency tracking. It works as a build
//! dependency, scanning your code for `#[input]` and `#[query]` annotations and
//! generating the custom DB type with caching and dependency tracking.

pub mod runtime;
mod scanner;
mod codegen;

// Re-export the procedural macros
pub use virdant_db_macros::{input, query};

use std::path::PathBuf;
use anyhow::Result;

/// Main configuration builder for the code generator
#[derive(Debug)]
pub struct Configuration {
    /// Directory to scan for annotated Rust files
    input_dir: PathBuf,
    /// Output file path for the generated database code
    output_file: PathBuf,
    /// Name of the generated database struct
    db_name: String,
    /// Name of the builder struct
    builder_name: String,
    /// Name of the query enum
    query_enum_name: String,
    /// Whether to enable tracing
    enable_tracing: bool,
    /// Whether to enable graphviz export
    enable_graphviz: bool,
}

impl Configuration {
    /// Create a new configuration with defaults
    pub fn new() -> Self {
        Configuration {
            input_dir: "src".into(),
            output_file: "src/db.rs".into(),
            db_name: "Db".to_string(),
            builder_name: "Builder".to_string(),
            query_enum_name: "Query".to_string(),
            enable_tracing: true,
            enable_graphviz: true,
        }
    }

    /// Set the input directory for scanning
    pub fn input_dir(mut self, dir: impl Into<PathBuf>) -> Self {
        self.input_dir = dir.into();
        self
    }

    /// Set the output file path
    pub fn output_file(mut self, path: impl Into<PathBuf>) -> Self {
        self.output_file = path.into();
        self
    }

    /// Set the name of the generated DB struct
    pub fn db_name(mut self, name: impl Into<String>) -> Self {
        self.db_name = name.into();
        self
    }

    /// Set the name of the generated Builder struct
    pub fn builder_name(mut self, name: impl Into<String>) -> Self {
        self.builder_name = name.into();
        self
    }

    /// Set the name of the query enum
    pub fn query_enum_name(mut self, name: impl Into<String>) -> Self {
        self.query_enum_name = name.into();
        self
    }

    /// Set whether to enable tracing for queries
    pub fn enable_tracing(mut self, enable: bool) -> Self {
        self.enable_tracing = enable;
        self
    }

    /// Set whether to enable graphviz export
    pub fn enable_graphviz(mut self, enable: bool) -> Self {
        self.enable_graphviz = enable;
        self
    }

    /// Process the configuration and generate the DB file
    pub fn process(self) -> Result<()> {
        // Scan for annotated functions
        let functions = scanner::scan_directory(&self.input_dir)?;
        
        // Use cargo:warning for build script output
        eprintln!("cargo:warning=Found {} annotated functions", functions.len());
        for func in &functions {
            eprintln!("cargo:warning=  - {} ({})", func.name, if func.is_input { "input" } else { "query" });
        }
        
        // Generate the database code
        let generated_code = codegen::generate_database(&functions, &self)?;
        
        // Create output directory if needed
        if let Some(parent) = self.output_file.parent() {
            std::fs::create_dir_all(parent)?;
        }
        
        // Write the generated code
        std::fs::write(&self.output_file, generated_code)?;
        
        eprintln!("cargo:warning=Generated {} with {} queries/inputs", 
                self.output_file.display(), 
                functions.len());
        
        Ok(())
    }
}