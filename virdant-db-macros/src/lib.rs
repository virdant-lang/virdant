//! Procedural macros for marking query and input functions

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, ItemFn, FnArg, Pat, ReturnType};

/// Mark a function as an input query
///
/// Input queries are never built - their values are set manually via `Db::set_*()` methods.
/// The function body should contain `unimplemented!()` as it's never called.
#[proc_macro_attribute]
pub fn input(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);

    // Validate that the function signature is appropriate for an input
    if let Err(e) = validate_input_signature(&input_fn) {
        return syn::Error::new_spanned(&input_fn.sig.ident, e)
            .to_compile_error()
            .into();
    }

    // For now, just pass through the function unchanged
    // The build script will scan for these attributes
    let expanded = quote! {
        #input_fn
    };

    TokenStream::from(expanded)
}

/// Mark a function as a query
///
/// Query functions are built on-demand when dirty. They must take
/// `&mut Builder` as their first parameter to track dependencies.
#[proc_macro_attribute]
pub fn query(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(input as ItemFn);

    // Validate that the function signature is appropriate for a query
    if let Err(e) = validate_query_signature(&input_fn) {
        return syn::Error::new_spanned(&input_fn.sig, e)
            .to_compile_error()
            .into();
    }

    // For now, just pass through the function unchanged
    // The build script will scan for these attributes
    let expanded = quote! {
        #input_fn
    };

    TokenStream::from(expanded)
}

/// Validate that an input function has the right structure
fn validate_input_signature(func: &ItemFn) -> Result<(), String> {
    // Should not take a Builder parameter
    if let Some(first_arg) = func.sig.inputs.first() {
        if let FnArg::Typed(pat_type) = first_arg {
            if let Pat::Ident(ident) = &*pat_type.pat {
                if ident.ident == "builder" {
                    return Err("Input functions should not take a builder parameter".to_string());
                }
            }
        }
    }

    // Should have a return type
    if matches!(func.sig.output, ReturnType::Default) {
        return Err("Input functions must have a return type".to_string());
    }

    Ok(())
}

/// Validate that a query function has the right structure
fn validate_query_signature(func: &ItemFn) -> Result<(), String> {
    // First parameter should be &mut Builder
    let first_param = func.sig.inputs
        .first()
        .ok_or_else(|| "Query functions must take &mut Builder as first parameter".to_string())?;

    match first_param {
        FnArg::Receiver(_) => {
            return Err("Query functions cannot be methods".to_string());
        }
        FnArg::Typed(pat_type) => {
            // Check if the name is "builder" (just a convention check, not required)
            if let Pat::Ident(ident) = &*pat_type.pat {
                if ident.ident != "builder" {
                    // This is just a warning, not an error
                    // The build script will handle it properly
                }
            }
        }
    }

    // Should have a return type
    if matches!(func.sig.output, ReturnType::Default) {
        return Err("Query functions must have a return type".to_string());
    }

    Ok(())
}
