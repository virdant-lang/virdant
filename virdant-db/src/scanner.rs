//! Scanner for finding functions annotated with #[input] or #[query]

use std::path::Path;
use walkdir::WalkDir;
use syn::{ItemFn, Attribute, FnArg, Pat};
use syn::visit::Visit;
use quote::ToTokens;

/// Metadata about a query or input function
#[derive(Debug, Clone)]
pub struct QueryFunction {
    /// The function name (e.g., "parsing")
    pub name: String,
    /// The generated accessor method name (e.g., "get_parsing")
    pub method_name: String,
    /// The generated setter method name (e.g., "set_source"), only used for inputs
    pub setter_name: String,
    /// Full path to the function (e.g., "crate::queries::build_parsing")
    pub path: String,
    /// Whether this is an input (true) or query (false)
    pub is_input: bool,
    /// Parameters (excluding builder for queries)
    pub params: Vec<Param>,
    /// The return type as token stream
    pub return_type: String,
}

#[derive(Debug, Clone)]
pub struct Param {
    /// Parameter name
    pub name: String,
    /// Type as token stream  
    pub ty: String,
}

/// Scan a directory tree for annotated functions
pub fn scan_directory(dir: &Path) -> anyhow::Result<Vec<QueryFunction>> {
    let mut functions = Vec::new();
    
    for entry in WalkDir::new(dir) {
        let entry = entry?;
        let path = entry.path();
        
        // Skip non-Rust files
        if path.extension().and_then(|s| s.to_str()) != Some("rs") {
            continue;
        }
        
        eprintln!("cargo:warning=Scanning {:?}", path);
        
        // Parse the file
        let content = std::fs::read_to_string(path)?;
        let ast = syn::parse_file(&content)?;
        
        eprintln!("cargo:warning=  Found {} items", ast.items.len());
        
        // Calculate the module path from the file path
        // Convert src/queries/parsing.rs -> ["queries", "parsing"]
        let relative_path = path.strip_prefix(&dir)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
        
        let module_parts: Vec<String> = if relative_path.file_stem().unwrap().to_str() == Some("mod") {
            // For mod.rs files, the directory IS the module
            relative_path
                .parent()
                .map(|p| p.to_str().unwrap_or(""))
                .unwrap_or("")
                .split('/')
                .filter(|s| !s.is_empty())
                .map(String::from)
                .collect()
        } else {
            // For regular files like parsing.rs, we include the file stem as module name
            let mut parts: Vec<String> = relative_path
                .parent()
                .map(|p| p.to_str().unwrap_or(""))
                .unwrap_or("")
                .split('/')
                .filter(|s| !s.is_empty())
                .map(String::from)
                .collect();
            
            // Add the file stem as the final module component (e.g., "parsing" from "parsing.rs")
            if let Some(stem) = relative_path.file_stem().and_then(|s| s.to_str()) {
                parts.push(stem.to_string());
            }
            
            parts
        };
        
        // Find annotated functions in this file
        let mut visitor = FunctionScanner {
            functions: Vec::new(),
            current_module: module_parts,
        };
        visitor.visit_file(&ast);
        
        eprintln!("cargo:warning=  Found {} annotated functions", visitor.functions.len());
        
        functions.extend(visitor.functions);
    }
    
    // Validate no duplicates
    validate_functions(&functions)?;
    
    Ok(functions)
}

/// Visitor that finds annotated functions
struct FunctionScanner {
    functions: Vec<QueryFunction>,
    current_module: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for FunctionScanner {
    fn visit_item_fn(&mut self, func: &'ast ItemFn) {
        // Check for #[input] or #[query] attributes
        let has_input = has_attribute(&func.attrs, "input");
        let has_query = has_attribute(&func.attrs, "query");
        
        if !has_input && !has_query {
            return;
        }
        
        if has_input && has_query {
            eprintln!("Warning: Function {} has both #[input] and #[query] attributes", func.sig.ident);
            return;
        }
        
        // Extract metadata
        let name = func.sig.ident.to_string();
        let is_input = has_input;

        // Determine accessor method name
        // Allow custom name via #[query(get = name)] or #[input(get = name)]
        let custom_method_name = extract_custom_method_name(&func.attrs);
        let method_name = custom_method_name.unwrap_or_else(|| {
            if is_input {
                format!("get_{}", name)
            } else {
                // For queries, use "get_" prefix for consistency
                if name.starts_with("get_") {
                    name.clone()
                } else {
                    format!("get_{}", name)
                }
            }
        });

        // Determine setter method name (for inputs)
        // Allow custom name via #[input(set = name)]
        let custom_setter_name = extract_custom_setter_name(&func.attrs);
        let setter_name = custom_setter_name.unwrap_or_else(|| format!("set_{}", name));
        
        // Build full path (including module path from file location)
        let path = if is_input {
            // Input functions don't have a real implementation
            format!("unimplemented!()")
        } else {
            // For queries, build the full path including module hierarchy
            let mut module_path = vec!["crate".to_string()];
            module_path.extend(self.current_module.clone());
            module_path.push(name.clone());
            module_path.join("::")
        };
        
        // Extract parameters (skip builder for queries)
        let params = extract_params(&func.sig.inputs, is_input);
        
        // Extract return type
        let return_type = match &func.sig.output {
            syn::ReturnType::Default => "()".to_string(),
            syn::ReturnType::Type(_, ty) => ty.to_token_stream().to_string(),
        };
        
        self.functions.push(QueryFunction {
            name,
            method_name,
            setter_name,
            path,
            is_input,
            params,
            return_type,
        });
    }
    
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        let mod_name = module.ident.to_string();
        self.current_module.push(mod_name.clone());
        
        // Continue visiting
        syn::visit::visit_item_mod(self, module);
        
        self.current_module.pop();
    }
}

/// Check if an attribute path ends with the given name (handles `query` and `virdant_db::query`)
fn attr_ends_with(attr: &Attribute, name: &str) -> bool {
    attr.path().segments.last().map(|s| s.ident == name).unwrap_or(false)
}

/// Extract a custom method name from `#[query(get = name)]` or `#[input(get = name)]`
fn extract_custom_method_name(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if !attr_ends_with(attr, "query") && !attr_ends_with(attr, "input") {
            continue;
        }
        // Try to parse as #[query(get = name)] — the "get = name" inside parens
        if let Ok(meta) = attr.parse_args::<syn::MetaNameValue>() {
            if meta.path.is_ident("get") {
                return match &meta.value {
                    // #[query(get = elaboration)] — a bare identifier
                    syn::Expr::Path(ep) if ep.path.segments.len() == 1 => {
                        Some(ep.path.segments[0].ident.to_string())
                    }
                    // #[query(get = "elaboration")] — a string literal
                    syn::Expr::Lit(lit) => {
                        if let syn::Lit::Str(s) = &lit.lit {
                            Some(s.value())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
            }
        }
    }
    None
}

/// Extract a custom setter name from `#[input(set = name)]`
fn extract_custom_setter_name(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if !attr_ends_with(attr, "input") {
            continue;
        }
        if let Ok(meta) = attr.parse_args::<syn::MetaNameValue>() {
            if meta.path.is_ident("set") {
                return match &meta.value {
                    // #[input(set = set_source)] — a bare identifier
                    syn::Expr::Path(ep) if ep.path.segments.len() == 1 => {
                        Some(ep.path.segments[0].ident.to_string())
                    }
                    // #[input(set = "set_source")] — a string literal
                    syn::Expr::Lit(lit) => {
                        if let syn::Lit::Str(s) = &lit.lit {
                            Some(s.value())
                        } else {
                            None
                        }
                    }
                    _ => None,
                };
            }
        }
    }
    None
}

/// Check if the attributes contain a specific attribute name
fn has_attribute(attrs: &[Attribute], name: &str) -> bool {
    attrs.iter().any(|attr| {
        attr.path()
            .segments
            .last()
            .map(|seg| seg.ident == name)
            .unwrap_or(false)
    })
}

/// Extract function parameters
fn extract_params(inputs: &syn::punctuated::Punctuated<FnArg, syn::token::Comma>, is_input: bool) -> Vec<Param> {
    inputs
        .iter()
        .skip(if is_input { 0 } else { 1 }) // Skip builder for queries
        .filter_map(|arg| {
            match arg {
                FnArg::Typed(pat_type) => {
                    let name = extract_param_name(&*pat_type.pat);
                    let ty = pat_type.ty.to_token_stream().to_string();
                    Some(Param { name, ty })
                }
                _ => None,
            }
        })
        .collect()
}

/// Extract parameter name from a pattern
fn extract_param_name(pat: &Pat) -> String {
    match pat {
        Pat::Ident(ident) => ident.ident.to_string(),
        _ => "unknown".to_string(),
    }
}

/// Validate that there are no duplicate query names
fn validate_functions(functions: &[QueryFunction]) -> anyhow::Result<()> {
    use std::collections::HashSet;
    
    let mut seen = HashSet::new();
    
    for func in functions {
        if !seen.insert(&func.name) {
            anyhow::bail!("Duplicate query/input function: {}", func.name);
        }
    }
    
    Ok(())
}