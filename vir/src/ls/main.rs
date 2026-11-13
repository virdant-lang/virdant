//! Implements the Virdant LSP backend via `tower-lsp`, providing hover,
//! go-to-definition, and diagnostics by maintaining an analysis `Db`.

use std::os::unix::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::{CommandFactory, Parser, Subcommand};

use bstr::{BStr, BString, ByteSlice as _};
use indexmap::IndexSet;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;

// The CLI query subcommands return a plain text error (unlike the LSP
// handlers, which use `tower_lsp::jsonrpc::Result`).
type CliResult<T> = std::result::Result<T, String>;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use serde_json::json;

use virdant::LIB_DIR;
use virdant::diagnostics::DiagnosticPayload;
use virdant::analysis::platform::resolve_platform_for;
use virdant::analysis::ports::Port;
use virdant::analysis::symbols::{SymbolId, SymbolKind, SymbolTable};
use virdant::types::{ExprRoot, Type};
use virdant::common::{ComponentKind, DriverType, PortDir};
use virdant::db::{Builder, Db};

#[path = "../common/help_json.rs"]
mod help_json;

// The language-query core shared by the LSP handlers and the CLI query
// subcommands below.
mod queries;
use queries::{NameResolution, Project};
use queries::type_to_source;

use virdant::package::{PackageId, PackageTable};
use virdant::common::source::{LineCol, Region, Source, SourceOffset, Span};
use virdant::syntax::ast::{AstNode, AstNodeId, item_children, match_arm_children};
use virdant::syntax::payload::AstNodePayload;
use virdant::syntax::parsing::Parsing;

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

struct ServerState {
    db: Db,
    uris: IndexSet<Url>,
    hover_mode: HoverMode,
}

#[derive(Clone, Copy)]
enum HoverMode {
    Normal,
    Debug,
}

fn new_db() -> Db {
    let mut db = Db::new();
    let builtin_text: BString = include_bytes!("../../../lib/builtin.vir").as_ref().into();
    db.set_packages(PackageTable::new(vec!["builtin".into()]));
    let builtin = db.get_packages().builtin();
    db.set_source(builtin, Source::new(builtin, builtin_text));
    db
}

/// The project root for an opened file: the nearest ancestor directory
/// (starting at the file's own directory) holding `virdant.toml`.
/// Falls back to the file's directory when no manifest is found, so
/// standalone files still resolve image paths against something
/// sensible.
fn project_root_for(dir: &std::path::Path) -> std::path::PathBuf {
    let mut current = Some(dir);
    while let Some(dir) = current {
        if vircommon::project_manifest(dir).is_some() {
            return dir.to_path_buf();
        }
        current = dir.parent();
    }
    dir.to_path_buf()
}

fn db_from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Db {
    // Load every *.vir in the standard library directory (builtin.vir,
    // ice40.vir, ...) so that `import` can resolve platform libraries, then
    // every *.vir in the source directory.  The loading logic lives in
    // `virdant::util`, shared with the CLI queries — but the LSP server
    // must not die on an unreadable source directory, so it uses a
    // fallible loader and degrades to a warning plus the library-only
    // Db.  That is the same warn-and-continue behavior the pre-`vir ls`
    // inline loader here had; the CLI keeps strict error handling on top
    // of the same fallible loader.
    //
    // Files whose names are not valid package identifiers are skipped
    // rather than failing the load: the rest of the project stays
    // analyzed, and the skipped file reports its own diagnostic (the
    // package-name check) once opened in the editor.
    let source_dir = source_dir.into();
    match vircommon::scan_source_dir(&source_dir) {
        Ok(scan) => {
            for path in &scan.invalid_names {
                eprintln!(
                    "warning: skipping {}: {}",
                    path.display(),
                    vircommon::INVALID_PACKAGE_NAME_REASON,
                );
            }
            let mut db = vircommon::db_from_source_dir_scan(scan);
            // The scanned directory is the opened file's directory;
            // the project root for memory image paths
            // (`@image`/`@image_hex`) is the nearest ancestor holding
            // `virdant.toml`.
            db.set_project_root(project_root_for(&source_dir));
            db
        }
        Err(e) => {
            eprintln!("warning: cannot read source directory {}: {e}", source_dir.display());
            vircommon::db_from_lib_dir()
        }
    }
}

fn db_is_empty(db: &Db) -> bool {
    db.get_packages().ids().count() < 2
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState {
                db: new_db(),
                hover_mode: HoverMode::Debug,
                uris: IndexSet::new(),
            })),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Hello from Virdant")
            .await;

        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![
                    "virdant.panic".into(),
                    "virdant.debug".into(),
                ],
                ..Default::default()
            }),

            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),

//            code_lens_provider: Some(CodeLensOptions {
//                resolve_provider: Some(false),
//            }),

//            inlay_hint_provider: Some(OneOf::Left(false)),

//            completion_provider: Some(CompletionOptions {
//                resolve_provider: Some(false),
//                trigger_characters: Some(vec![".".to_string()]),
//                ..Default::default()
//            }),

//            document_highlight_provider: Some(OneOf::Left(false)),

            ..Default::default()
        };

        Ok(InitializeResult {
            capabilities,
            ..Default::default()
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;

        self.client
            .log_message(MessageType::INFO, format!("File opened: {uri}"))
            .await;

        let workspace = workspace_root(&uri);
        let package_name = uri_to_package_name(&uri);
        let text: BString = params.text_document.text.into();

        {
            let mut state = self.state.lock().await;
            if db_is_empty(&state.db) {
                state.db = db_from_dir(&workspace);
            }

            let package = match state.db.get_packages().id(package_name.as_bstr()) {
                Some(package) => package,
                None => {
                    // The package is new: rebuild the table wholesale with
                    // the new name included, then re-resolve its id.
                    let packages = state.db.get_packages();
                    let mut names: Vec<BString> = packages.ids().map(|id| packages.name(id).to_owned()).collect();
                    names.push(package_name.clone());
                    state.db.set_packages(PackageTable::new(names));
                    state.db.get_packages().id(package_name.as_bstr()).unwrap()
                }
            };

            let new_source = Source::new(package, text);
            state.db.set_source(package, new_source);

            state.uris.insert(uri.clone());
        }

        self.check_all_documents().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        self.client
            .log_message(MessageType::ERROR, format!("File changed: {uri}"))
            .await;

        let package_name = uri_to_package_name(&uri);
        let text: BString = params.content_changes[0].text.clone().into();

        {
            let mut state = self.state.lock().await;
            let workspace = workspace_root(&uri);
            if db_is_empty(&state.db) {
                state.db = db_from_dir(workspace);
            }
            let package = match state.db.get_packages().id(package_name.as_bstr()) {
                Some(package) => package,
                None => {
                    let packages = state.db.get_packages();
                    let mut names: Vec<BString> = packages.ids().map(|id| packages.name(id).to_owned()).collect();
                    names.push(package_name.clone());
                    state.db.set_packages(PackageTable::new(names));
                    state.db.get_packages().id(package_name.as_bstr()).unwrap()
                }
            };
            let new_source = Source::new(package, text);
            state.db.set_source(package, new_source);
        }

        self.check_all_documents().await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File saved")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        match self.hover_mode().await {
            HoverMode::Normal => self.hover_normal(params).await,
            HoverMode::Debug => self.hover_debug(params).await,
        }
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);
        let uri = params.text_document_position_params.text_document.uri;

        self.client
            .log_message(MessageType::INFO, &format!("GOTO DEF at {linecol:?}"))
            .await;

        let state = self.state.lock().await;
        let db = &state.db;
        let package = uri_to_package(db, &uri);

        // Definition resolution is shared with the `vir ls definition`
        // CLI query; see queries::resolve_definition_at.
        let Some(target_loc) = queries::resolve_definition_at(db, package, linecol) else {
            return Ok(None);
        };

        // Convert the Virdant Location to an LSP Location.
        let target_parsing = db.get_parsing(target_loc.package());
        let target_node = target_parsing.ast_node(target_loc.ast_node_id());
        let target_uri = package_id_to_uri(db, target_loc.package(), &uri);

        let package_name = db.get_packages().name(target_loc.package()).to_str_lossy().into_owned();
        self.client
            .log_message(MessageType::INFO, &format!("GOing TO (package: {package_name}) {target_uri} {}", target_node.span()))
            .await;

        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range: span_to_range(target_node.span()),
        })))
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        let items = vec![
            CompletionItem {
                label: "func".to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some("Example function".to_string()),
                ..Default::default()
            },
            CompletionItem {
                label: "var".to_string(),
                kind: Some(CompletionItemKind::VARIABLE),
                detail: Some("Example variable".to_string()),
                ..Default::default()
            },
        ];

        Ok(Some(CompletionResponse::Array(items)))
    }

//    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
//        let uri = params.text_document.uri;
//        let package = uri_to_packagefqn(&uri);
//
//        let parsing = {
//            let mut state = self.state.lock().await;
//            state.vir.parsing(&package.to_string())
//        };
//        let mut lenses = vec![];
//        self.client
//            .log_message(MessageType::ERROR, format!("Root is {}", parsing.root().summary()))
//            .await;
//
//        for node in parsing.root().children() {
//            self.client
//                .log_message(MessageType::ERROR, format!("Considering lens for: {}", node.summary()))
//                .await;
//
//            if node.is_item() {
//                let range = span_to_range(node.span());
//                let lens = CodeLens {
//                    range,
//                    command: Some(Command {
//                        title: "Run test".into(),
//                        command: "VirdantRunTest".into(),
//                        arguments: Some(vec![
//                            json!(parsing.string(node.name().unwrap()).to_string()),
//                        ]),
//                    }),
//                    data: None,
//                };
//
//                self.client
//                    .log_message(MessageType::ERROR, format!("{lens:?}"))
//                    .await;
//
//                lenses.push(lens);
//            }
//        }
//
//        Ok(Some(lenses))
//    }


    async fn inlay_hint(
        &self,
        params: InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let _range = params.range;
        let _linecol = LineCol::new(1, 1);

        let _parsing = {
            let state = self.state.lock().await;
            let package = uri_to_package(&state.db, &uri);
            state.db.get_parsing(package.clone())
        };

        let mut hints = vec![];

        let hint = InlayHint {
            position: Position {
                line: 0,
                character: 0,
            },
            label: InlayHintLabel::String(": i32".into()),
            kind: None,
            tooltip: Some(InlayHintTooltip::String("Example tooltip".into())),
            text_edits: None,
            padding_left: Some(true),
            padding_right: Some(false),
            data: None,
        };
        hints.push(hint);

        Ok(Some(hints))
    }

    async fn document_highlight(
        &self,
        params: DocumentHighlightParams,
    ) -> Result<Option<Vec<DocumentHighlight>>> {

        let position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);

        let uri = params.text_document_position_params.text_document.uri;
        let parsing = {
            let state = self.state.lock().await;
            let package = uri_to_package(&state.db, &uri);
            state.db.get_parsing(package.clone())
        };

        if let Some(node_id) = parsing.at(linecol) {
            let node = parsing.ast_node(node_id);
            let highlight = DocumentHighlight {
                range: span_to_range(node.span()),
                kind: Some(DocumentHighlightKind::TEXT),
            };

            Ok(Some(vec![highlight]))
        } else {
            Ok(None)
        }

    }

    async fn execute_command(
        &self,
        params: ExecuteCommandParams,
    ) -> Result<Option<serde_json::Value>> {
        match params.command.as_str() {
            "virdant.panic" => {
                panic!("virdant.panic: {:?}", params.arguments)
            }
            "virdant.debug" => {
                let mut state = self.state.lock().await;
                    if params.arguments.get(0) == Some(&json!("true")) {
                    state.hover_mode = HoverMode::Debug;
                } else {
                    state.hover_mode = HoverMode::Normal;
                }
            }
            _ => {}
        }

        Ok(None)
    }

    async fn workspace_diagnostic(
        &self,
        _params: WorkspaceDiagnosticParams,
    ) -> Result<WorkspaceDiagnosticReportResult> {
        self.check_all_documents().await;
        Ok(WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport { items: vec![] }))
    }


    async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri;
        let position = params.range.start;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);
        let state = self.state.lock().await;
        let db = &state.db;
        let package = uri_to_package(db, &uri);
        let parsing = db.get_parsing(package);

        let mut actions = vec![];

        // The two syntax-derived actions below trigger on the node
        // under the cursor (no diagnostics involved).
        if let Some(node_id) = parsing.at(linecol) {
            if let Some(action) = missing_match_cases_action(db, &parsing, package, node_id, &uri) {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }

            if let Some(action) = missing_platform_ports_action(db, &parsing, package, node_id, &uri) {
                actions.push(CodeActionOrCommand::CodeAction(action));
            }
        }

        // Quickfixes derived from the compiler's own diagnostics at
        // the cursor's line (see `diagnostic_quickfixes`).
        for action in diagnostic_quickfixes(db, &parsing, package, linecol, &uri) {
            actions.push(CodeActionOrCommand::CodeAction(action));
        }

        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }
}

impl Backend {
    #[allow(dead_code)]
    async fn source(&self, uri: &Url) -> Option<Source> {
        let state = self.state.lock().await;
        let package = uri_to_package(&state.db, uri);
        Some(state.db.get_source(package))
    }

    async fn check_all_documents(&self) {
        let uris = {
            let state = self.state.lock().await;
            state.uris.clone()
        };

        for uri in &uris {
            self.check_document(uri).await
        }
    }

    async fn check_document(&self, uri: &Url) {
        self.client
            .log_message(MessageType::ERROR, format!("check_document: {uri}"))
            .await;

        let mut diagnostics = Vec::new();

        let state = self.state.lock().await;
        let diags = state.db.check();

        let package = uri_to_package(&state.db, &uri);

        for diag in diags.iter() {
            // Only display diagnostics relevant to this file.
            if diag.region().package() != package {
                continue;
            }

            let severity = match diag.level() {
                virdant::diagnostics::DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
                virdant::diagnostics::DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
                virdant::diagnostics::DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
            };
            let diagnostic = Diagnostic {
                range: span_to_range(diag.region().span()),
                severity: Some(severity),
                // The stable payload-variant code ("UnusedImport",
                // "WrongDriverType", ...): lets clients filter or
                // suppress diagnostics, and mirrors what the quickfix
                // dispatch below matches on.
                code: Some(NumberOrString::String(diag.code().to_string())),
                code_description: None,
                source: Some("virdant".to_string()),
                message: diag.message().to_string(),
                related_information: None,
                tags: None,
                data: None,
            };
            diagnostics.push(diagnostic);
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    async fn hover_mode(&self) -> HoverMode {
        let state = self.state.lock().await;
        state.hover_mode
    }

    async fn hover_normal(&self, params: HoverParams) -> Result<Option<Hover>> {
        let position: Position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);

        let uri = params.text_document_position_params.text_document.uri;
        let (parsing, _hover_mode) = {
            let state = self.state.lock().await;
            let package = uri_to_package(&state.db, &uri);
            (state.db.get_parsing(package), state.hover_mode)
        };

        if let Some(node_id) = parsing.at(linecol) {
            let node = parsing.ast_node(node_id);
            let contents = HoverContents::Scalar(MarkedString::String(
                format!("Node: {}", node.summary())
            ));

            Ok(Some(Hover {
                contents,
                range: None,
            }))
        } else {
            Ok(None)
        }

    }

    async fn hover_debug(&self, params: HoverParams) -> Result<Option<Hover>> {
        let position: Position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);

        let uri = params.text_document_position_params.text_document.uri;
        let state = self.state.lock().await;
        let db = &state.db;
        let package = uri_to_package(db, &uri);
        let parsing = db.get_parsing(package);

        if let Some(node_id) = parsing.at(linecol) {
            let node = parsing.ast_node(node_id);

            let typof = if node.is_expr() {
                if let Some(exprroot) = get_expr_root(&self.client, db, &parsing, node.clone()).await {
                    let _typing = db.get_typing(ExprRoot { location: exprroot.location() });
                    db.get_typeof(node.location()).ok() // TODO is this .ok() OK?
                } else {
                    None
                }
            } else {
                None
            };
            let contents = HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("# DEBUG NODE
## Spelling
```virdant
{}
```

## Summary
{}

## Package
{}

## Location
{:?}

## Typeof
{:?}
",
                    node.spelling(),
                    node.summary(),
                    db.get_packages().name(package).to_str_lossy(),
                    node.id(),
                    typof,
                ),

            });

            Ok(Some(Hover {
                contents,
                range: Some(span_to_range(node.span())),
            }))
        } else {
            Ok(None)
        }

    }
}

async fn get_expr_root<'a>(client: &Client, db: &'a Db, parsing: &'a Parsing, node: AstNode<'a>) -> Option<AstNode<'a>> {
    let exprroots: Vec<AstNodeId> = db.get_exprroots().iter().map(|exprroot| exprroot.location().ast_node_id()).collect();

        client
            .log_message(MessageType::ERROR, format!("get_expr_root({:?})", node.id()))
            .await;

        client
            .log_message(MessageType::ERROR, format!("exprroots = ({exprroots:?})"))
            .await;

    let mut current = node;
    while !exprroots.contains(&current.id()) {
        let parent_id = current.parent()?.id();
        current = parsing.ast_node(parent_id);
    }
    Some(current)
}

/// Builds the "Add missing match cases" quickfix for a `match` expression
/// or statement enclosing `node_id`, if it is non-exhaustive.
fn missing_match_cases_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    node_id: AstNodeId,
    uri: &Url,
) -> Option<CodeAction> {
    let match_node = find_enclosing_match(parsing, node_id)?;

    let children = match_node.children();
    if children.is_empty() { return None; }
    let subject = &children[0];
    let subject_typ = db.get_typeof(subject.location()).ok()?;

    let symboltable = db.get_symboltable();
    let missing = missing_match_patterns(db, parsing, &match_node, &subject_typ, &symboltable);
    if missing.is_empty() { return None; }

    let is_stmt = !match_node.is_expression_position();
    let arm_body = if is_stmt { "{ }" } else { "?" };

    // A `with` clause is always indented 4 spaces relative to its `match`.
    let indent_col = match_node.span().start().col().saturating_add(4);
    let indent_str = " ".repeat(indent_col.saturating_sub(1));

    // Find an insertion point immediately after the last non-whitespace byte
    // before the closing `}` of the match, so the `}` keeps its layout.
    let source = db.get_source(package);
    let close_col = match_node.span().end().col().saturating_sub(1).max(1);
    let close_line = match_node.span().end().line();
    let close_offset: usize = source.to_offset(LineCol::new(close_line, close_col)).into();
    let text = source.text();
    let mut insert_offset = close_offset;
    while insert_offset > 0 {
        let b = text[insert_offset - 1];
        if b == b' ' || b == b'\t' || b == b'\n' || b == b'\r' {
            insert_offset -= 1;
        } else {
            break;
        }
    }
    let insert_lc = source.to_linecol(SourceOffset::new(insert_offset as u32));
    let insert_position = linecol_to_position(insert_lc);

    let mut new_text = String::new();
    for pat in &missing {
        new_text.push('\n');
        new_text.push_str(&indent_str);
        new_text.push_str("with ");
        new_text.push_str(pat);
        new_text.push_str(" => ");
        new_text.push_str(arm_body);
    }

    let edit = TextEdit {
        range: Range { start: insert_position, end: insert_position },
        new_text,
    };

    Some(quickfix_action("Add missing match cases".into(), uri, vec![edit]))
}

/// The nearest ancestor-or-self of `node_id` (inclusive of the node
/// itself) whose payload satisfies `matches_payload`.
fn find_enclosing<'p>(
    parsing: &'p Parsing,
    node_id: AstNodeId,
    matches_payload: impl Fn(AstNodePayload) -> bool,
) -> Option<AstNode<'p>> {
    let mut current_id = node_id;
    loop {
        let node = parsing.ast_node(current_id);
        if matches_payload(node.payload()) {
            return Some(node);
        }
        match node.parent().map(|p| p.id()) {
            Some(id) => current_id = id,
            None => return None,
        }
    }
}

fn find_enclosing_match<'p>(parsing: &'p Parsing, node_id: AstNodeId) -> Option<AstNode<'p>> {
    find_enclosing(parsing, node_id, |payload| matches!(payload, AstNodePayload::Match))
}

fn missing_match_patterns(
    db: &Db,
    parsing: &Parsing,
    match_node: &AstNode<'_>,
    subject_typ: &Type,
    symboltable: &SymbolTable,
) -> Vec<String> {
    use indexmap::IndexSet;
    use bstr::ByteSlice;

    let children = match_node.children();
    let arms = match_arm_children(&children);

    let mut covered_syms: IndexSet<SymbolId> = IndexSet::new();
    let mut covered_words: IndexSet<u64> = IndexSet::new();
    let mut covered_bits: IndexSet<bool> = IndexSet::new();
    let mut has_else = false;

    for (pat_opt, _body) in &arms {
        let Some(pat) = pat_opt else { has_else = true; continue; };
        match pat.payload() {
            AstNodePayload::PatCtor(pat_ctor) => {
                let Type::Usual(td, _) = subject_typ else { continue };
                let name = parsing.string(pat_ctor.name);
                if let Some(sym) = symboltable.slot(*td, name) {
                    covered_syms.insert(sym.id());
                }
            }
            AstNodePayload::PatEnumerant(pat_enum) => {
                let Type::Usual(td, _) = subject_typ else { continue };
                let name = parsing.string(pat_enum.name);
                if let Some(sym) = symboltable.slot(*td, name) {
                    covered_syms.insert(sym.id());
                }
            }
            AstNodePayload::PatWordLit(pat_word) => {
                let literal = parsing.string(pat_word.literal).to_str_lossy();
                covered_words.insert(parse_word_literal_value(&literal));
            }
            AstNodePayload::PatBitLit(pat_bit) => {
                covered_bits.insert(pat_bit.literal);
            }
            _ => continue,
        }
    }

    if has_else {
        return vec![];
    }

    match subject_typ {
        Type::Bit | Type::Reset => {
            let mut out = vec![];
            for v in [false, true] {
                if !covered_bits.contains(&v) {
                    out.push(if v { "true".to_string() } else { "false".to_string() });
                }
            }
            out
        }
        Type::Word(width) => {
            if *width > 10 { return vec![]; }
            let total: u64 = 1u64 << width;
            (0..total)
                .filter(|v| !covered_words.contains(v))
                .map(|v| v.to_string())
                .collect()
        }
        Type::Usual(td, _args) => {
            let sym = symboltable.symbol(*td);
            match sym.kind() {
                SymbolKind::EnumDef => symboltable
                    .slots(*td)
                    .iter()
                    .filter(|s| !covered_syms.contains(&s.id()))
                    .map(|s| {
                        // Enumerants carrying payload parameters are
                        // matched with `@Name(params)`, others with `#Name`.
                        let params = db
                            .get_ctor_signature(s.id())
                            .parameters
                            .iter()
                            .map(|(name, _)| name.to_str_lossy().into_owned())
                            .collect::<Vec<_>>();
                        if params.is_empty() {
                            format!("#{}", s.name().to_str_lossy())
                        } else {
                            format!("@{}({})", s.name().to_str_lossy(), params.join(", "))
                        }
                    })
                    .collect(),
                SymbolKind::UnionDef => symboltable
                    .slots(*td)
                    .iter()
                    .filter(|s| !covered_syms.contains(&s.id()))
                    .map(|s| {
                        let sig = db.get_ctor_signature(s.id());
                        let params: Vec<String> = sig.parameters.iter()
                            .map(|(name, _)| name.to_str_lossy().into_owned())
                            .collect();
                        format!("@{}({})", s.name().to_str_lossy(), params.join(", "))
                    })
                    .collect(),
                _ => vec![],
            }
        }
        _ => vec![],
    }
}

/// Builds the "Add missing platform ports" quickfix for the `mod`
/// enclosing `node_id`, when it has a `for` clause naming a platform
/// that requires ports the module does not declare. Each added port
/// gets a stub it-block: `unused it` for incoming ports, `it := ?`
/// for outgoing ports.
fn missing_platform_ports_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    node_id: AstNodeId,
    uri: &Url,
) -> Option<CodeAction> {
    let moddef_node = find_enclosing_moddef(parsing, node_id)?;
    let item_name = parsing.string(moddef_node.name()?);

    let moddef_symbol = db.get_symboltable()
        .resolve_item_in_package(item_name, package)?
        .clone();

    let mut builder = Builder::new(db);
    let platform_id = resolve_platform_for(&mut builder, moddef_symbol.id).ok()??;

    let platform_ports = db.get_ports_of(platform_id);
    // A module bound to a platform *inherits* its ports: the component
    // analysis synthesizes them (`Component::synthetic`), so
    // `get_ports_of(moddef)` already contains them even when the body
    // is empty.  The quickfix must offer exactly the ports the module
    // does not declare in its own body, so compare against declared
    // (non-synthetic) port components instead.
    let module_components = db.get_component_analysis(moddef_symbol.id);
    let declared_port_paths: Vec<BString> = module_components.components().iter()
        .filter(|(_, c)|
            !c.synthetic()
            && matches!(c.kind(),
                Some(ComponentKind::Incoming)
                | Some(ComponentKind::Outgoing)
                | Some(ComponentKind::OutgoingWire)
                | Some(ComponentKind::OutgoingReg)
                | Some(ComponentKind::Bus))
        )
        .map(|(path, _)| path.clone())
        .collect();
    let missing: Vec<&Port> = platform_ports.iter()
        .filter(|pp| !declared_port_paths.iter().any(|mp| mp == &pp.path))
        .collect();
    if missing.is_empty() { return None; }

    let platform_name = db.get_symboltable().symbol(platform_id).name.to_str_lossy().into_owned();

    let source = db.get_source(package);

    let (insert_offset, indent_col, insert_before_stmt) =
        moddef_statement_insertion_point(&source, &moddef_node)?;
    let indent_str = " ".repeat(indent_col.saturating_sub(1));
    let inner_indent_str = format!("{indent_str}    ");

    let mut new_text = String::new();
    for port in &missing {
        let dir_kw = match port.dir {
            PortDir::Input => "incoming",
            PortDir::Output => "outgoing",
            PortDir::Bidir => "bus",
        };
        // Stub the port's it-block: incoming ports are marked `unused`
        // (nothing drives them yet), outgoing ports are driven by the
        // hole value `?` (a placeholder to be filled in later).
        let body = match port.dir {
            PortDir::Input => "unused it",
            PortDir::Output => "it := ?",
            PortDir::Bidir => "",
        };
        let typ_str = port.typ.as_ref()
            .map(|t| type_to_source(db, package, t))
            .unwrap_or_else(|| "Bit".to_string());
        let name = port.path.to_str_lossy();
        let declaration = format!(
            "{dir_kw} {name} : {typ_str} {{\n{inner_indent_str}{body}\n{indent_str}}}"
        );
        if insert_before_stmt {
            new_text.push_str(&declaration);
            new_text.push('\n');
            new_text.push_str(&indent_str);
        } else {
            new_text.push('\n');
            new_text.push_str(&indent_str);
            new_text.push_str(&declaration);
        }
    }

    let insert_lc = source.to_linecol(SourceOffset::new(insert_offset as u32));
    let insert_position = linecol_to_position(insert_lc);

    let edit = TextEdit {
        range: Range { start: insert_position, end: insert_position },
        new_text,
    };

    Some(quickfix_action(
        format!("Add missing ports required by platform {platform_name}"),
        uri,
        vec![edit],
    ))
}

/// Where a new statement should be inserted in a mod def's body: right
/// before the first body statement (skipping the `for`-clause child,
/// which is not a statement), or right after the opening `{` when the
/// body is empty.  Returns the insertion offset, the body's indent
/// column (1-based), and whether the offset sits at the start of an
/// existing statement (as opposed to just after the `{`).
fn moddef_statement_insertion_point(
    source: &Source,
    moddef_node: &AstNode<'_>,
) -> Option<(usize, usize, bool)> {
    let mut stmt_children = item_children(&moddef_node);
    if matches!(stmt_children.first().map(|c| c.payload()), Some(AstNodePayload::Ofness(_))) {
        stmt_children.remove(0);
    }

    if let Some(first) = stmt_children.first() {
        let lc = first.span().start();
        let offset: usize = source.to_offset(lc).into();
        Some((offset, lc.col(), true))
    } else {
        // Empty body: insert right after the opening `{`.
        let text = source.text();
        let node_span = moddef_node.span();
        let start_off: usize = source.to_offset(node_span.start()).into();
        let end_off: usize = source.to_offset(node_span.end()).into();
        let brace_off = text[start_off..end_off].iter().position(|&b| b == b'{')
            .map(|i| start_off + i + 1)?;
        let indent_col = node_span.start().col().saturating_add(4);
        Some((brace_off, indent_col, false))
    }
}

fn find_enclosing_moddef<'p>(parsing: &'p Parsing, node_id: AstNodeId) -> Option<AstNode<'p>> {
    find_enclosing(parsing, node_id, |payload| matches!(payload, AstNodePayload::ModDef(_)))
}

// ---------------------------------------------------------------------------
// Diagnostic-driven quickfixes
//
// The actions above derive their context from the cursor's syntax node;
// these dispatch off the compiler's own diagnostics instead.  Rather
// than trusting the client to echo `params.context.diagnostics` back,
// `code_action` re-runs `db.check()` and keeps the diagnostics whose
// region contains or abuts the cursor's line, so each fix works from a
// typed `DiagnosticPayload` instead of re-deriving context from syntax.
// ---------------------------------------------------------------------------

/// The quickfix actions for every diagnostic whose region touches
/// `linecol`'s line in `package`.
fn diagnostic_quickfixes(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    linecol: LineCol,
    uri: &Url,
) -> Vec<CodeAction> {
    let mut actions = vec![];
    for diag in db.check().iter() {
        let region = diag.region();
        if region.package() != package {
            continue;
        }
        // "At the cursor" is line-granular: the cursor may sit anywhere
        // on the diagnostic's line, including in whitespace where
        // `parsing.at` finds no node.
        let span = region.span();
        if linecol.line() < span.start().line() || linecol.line() > span.end().line() {
            continue;
        }

        let action = match &diag.payload {
            DiagnosticPayload::UnusedImport(d) => remove_import_action(
                db,
                package,
                span,
                format!("Remove unused import `{}`", d.imported_package.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::DuplicateImport(d) => remove_import_action(
                db,
                package,
                span,
                format!("Remove duplicate import `{}`", d.imported_package.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::RedundantUnused(d) => remove_unused_action(
                db,
                parsing,
                package,
                region,
                format!("Remove redundant `unused` mark on `{}`", d.path.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::RedundantOnClause(d) => remove_on_clause_action(
                db,
                package,
                region,
                format!("Remove redundant `on` clause on `{}`", d.component.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::WrongDriverType(d) => {
                let op = match d.expected_driver_type {
                    DriverType::Continuous => ":=",
                    DriverType::Latched => "<=",
                };
                fix_driver_type_action(
                    db,
                    parsing,
                    package,
                    region,
                    d.expected_driver_type,
                    format!("Use `{op}` to drive `{}`", d.target.to_str_lossy()),
                    uri,
                )
            }
            DiagnosticPayload::MemoryAssignInsteadOfLassign(d) => fix_memory_write_action(
                db,
                parsing,
                package,
                region,
                format!("Use `<=` to write memory `{}`", d.component.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::DriveUndeclaredPlatformPort(d) => declare_platform_port_action(
                db,
                parsing,
                package,
                region,
                &d.port,
                format!(
                    "Declare port `{}` required by platform `{}`",
                    d.port.to_str_lossy(),
                    d.platform.to_str_lossy(),
                ),
                uri,
            ),
            DiagnosticPayload::PackageNotImported(d) => add_import_action(
                db,
                parsing,
                package,
                &d.package,
                format!("Add `import {}`", d.package.to_str_lossy()),
                uri,
            ),
            DiagnosticPayload::ImportNotAtTopError => move_import_to_top_action(
                db,
                parsing,
                package,
                region,
                uri,
            ),
            _ => None,
        };
        if let Some(action) = action {
            actions.push(action);
        }
    }
    actions
}

/// Builds a quickfix `CodeAction` applying `edits` to `uri`.
fn quickfix_action(title: String, uri: &Url, edits: Vec<TextEdit>) -> CodeAction {
    let mut changes = std::collections::HashMap::new();
    changes.insert(uri.clone(), edits);

    CodeAction {
        title,
        kind: Some(CodeActionKind::QUICKFIX),
        diagnostics: None,
        edit: Some(WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        }),
        command: None,
        is_preferred: Some(true),
        disabled: None,
        data: None,
    }
}

/// An LSP (0-based) `Position` for a 1-based `LineCol`.
fn linecol_to_position(linecol: LineCol) -> Position {
    Position {
        line: (linecol.line().saturating_sub(1)) as u32,
        character: (linecol.col().saturating_sub(1)) as u32,
    }
}

/// The quickfix for `UnusedImport` and `DuplicateImport`: delete the
/// import statement's line.  The diagnostic's region is the whole
/// `import pkg` statement, and imports are one-per-line, so deleting
/// the region's lines deletes the statement cleanly.
fn remove_import_action(
    db: &Db,
    package: PackageId,
    span: Span,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let edit = delete_lines_edit(&db.get_source(package), span);
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `RedundantUnused`: delete the duplicate `unused`
/// statement.  The diagnostic's region covers only the path inside
/// `unused <path>`, so jump to the enclosing `ModDefStmtUnused` node
/// for the full statement before deleting its line.
fn remove_unused_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    region: Region,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let stmt = find_enclosing(
        parsing,
        parsing.at(region.start())?,
        |payload| matches!(payload, AstNodePayload::ModDefStmtUnused),
    )?;
    let edit = delete_lines_edit(&db.get_source(package), stmt.span());
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `RedundantOnClause`: remove the redundant
/// `on <clock>` clause from a declaration.  The diagnostic's region is
/// the clock expression only (the `OnClause` grammar rule contributes
/// just the expression, so the span starts at the clock name), so the
/// fix back-scans from there over the `on` keyword: deleting the
/// whitespace before `on` through the end of the clock expression
/// leaves the declaration single-spaced (`reg r : Bit on clock {`
/// becomes `reg r : Bit {`).
fn remove_on_clause_action(
    db: &Db,
    package: PackageId,
    region: Region,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let source = db.get_source(package);
    let text = source.text();
    let expr_start: usize = source.to_offset(region.start()).into();
    let expr_end: usize = source.to_offset(region.end()).into();

    // Back-scan over the whitespace separating `on` from the clock
    // expression, then require the `on` keyword itself.
    let mut i = expr_start;
    while i > 0 && matches!(text[i - 1], b' ' | b'\t') {
        i -= 1;
    }
    if i < 2 || text[i - 2..i] != b"on" {
        return None;
    }
    // Swallow the whitespace before `on` too, so the tokens around the
    // removed clause end up with exactly one space between them.
    let mut del_start = i - 2;
    while del_start > 0 && matches!(text[del_start - 1], b' ' | b'\t') {
        del_start -= 1;
    }

    let edit = TextEdit {
        range: Range {
            start: linecol_to_position(source.to_linecol(SourceOffset::new(del_start as u32))),
            end: linecol_to_position(source.to_linecol(SourceOffset::new(expr_end as u32))),
        },
        new_text: String::new(),
    };
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `WrongDriverType`: respell the driver operator as
/// the expected type (`:=` for continuous, `<=` for latched).  The
/// diagnostic's region is the driver statement's RHS expression (see
/// `Driver::Expr`, which carries the RHS location), so walk up to the
/// enclosing `Driver` statement for the operator site.
fn fix_driver_type_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    region: Region,
    expected: DriverType,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let stmt = find_enclosing(
        parsing,
        parsing.at(region.start())?,
        |payload| matches!(payload, AstNodePayload::Driver(_)),
    )?;
    let op = match expected {
        DriverType::Continuous => ":=",
        DriverType::Latched => "<=",
    };
    let edit = replace_driver_operator_edit(&db.get_source(package), &stmt, op)?;
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `MemoryAssignInsteadOfLassign`: respell a memory
/// write's `:=` as `<=`.  Safe by construction — the warning exists
/// precisely because both spellings write.  The diagnostic's region is
/// the whole write statement; the operator sits between the index and
/// the RHS expression.
fn fix_memory_write_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    region: Region,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let stmt = find_enclosing(
        parsing,
        parsing.at(region.start())?,
        |payload| matches!(payload, AstNodePayload::IndexedDriver(_)),
    )?;
    let edit = replace_driver_operator_edit(&db.get_source(package), &stmt, "<=")?;
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `DriveUndeclaredPlatformPort`: declare the missing
/// port in the module, dir keyword and type copied from the platform
/// port.  The offending driver statement stays where it is and becomes
/// the port's legal driver once the declaration exists, so — unlike
/// `missing_platform_ports_action`'s stubs — the declaration carries no
/// it-block: a `?` stub would double-drive the port, and an empty block
/// would warn `EmptyDriverBlock`.
fn declare_platform_port_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    region: Region,
    port_name: &BString,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let moddef_node = find_enclosing_moddef(parsing, parsing.at(region.start())?)?;
    let item_name = parsing.string(moddef_node.name()?);
    let moddef_symbol = db
        .get_symboltable()
        .resolve_item_in_package(item_name, package)?
        .clone();

    let mut builder = Builder::new(db);
    let platform_id = resolve_platform_for(&mut builder, moddef_symbol.id).ok()??;
    let platform_ports = db.get_ports_of(platform_id);
    let port = platform_ports
        .iter()
        .find(|port| port.path.as_bstr() == port_name.as_bstr())?;

    let dir_kw = match port.dir {
        PortDir::Input => "incoming",
        PortDir::Output => "outgoing",
        PortDir::Bidir => "bus",
    };
    let typ_str = port
        .typ
        .as_ref()
        .map(|typ| type_to_source(db, package, typ))
        .unwrap_or_else(|| "Bit".to_string());
    let name = port.path.to_str_lossy();
    let declaration = format!("{dir_kw} {name} : {typ_str}");

    let source = db.get_source(package);
    let (insert_offset, indent_col, insert_before_stmt) =
        moddef_statement_insertion_point(&source, &moddef_node)?;
    let indent_str = " ".repeat(indent_col.saturating_sub(1));
    let new_text = if insert_before_stmt {
        format!("{declaration}\n{indent_str}")
    } else {
        format!("\n{indent_str}{declaration}")
    };

    let insert_lc = source.to_linecol(SourceOffset::new(insert_offset as u32));
    let insert_position = linecol_to_position(insert_lc);
    let edit = TextEdit {
        range: Range { start: insert_position, end: insert_position },
        new_text,
    };

    Some(quickfix_action(title, uri, vec![edit]))
}

/// An edit deleting the whole lines spanned by `span`: from the first
/// line's start through the last line's trailing newline (or through
/// the text end when the file has no trailing newline).  Virdant
/// statements are one-per-line (formatting is compiler-enforced), so
/// deleting a statement means deleting its lines.
fn delete_lines_edit(source: &Source, span: Span) -> TextEdit {
    let text = source.text();
    let start_off: usize = source.to_offset(LineCol::new(span.start().line(), 1)).into();
    let mut end_off: usize = source.to_offset(LineCol::new(span.end().line(), 1)).into();
    while end_off < text.len() && text[end_off] != b'\n' {
        end_off += 1;
    }
    if end_off < text.len() {
        end_off += 1;
    }
    TextEdit {
        range: Range {
            start: linecol_to_position(source.to_linecol(SourceOffset::new(start_off as u32))),
            end: linecol_to_position(source.to_linecol(SourceOffset::new(end_off as u32))),
        },
        new_text: String::new(),
    }
}

/// The quickfix for `PackageNotImported`: insert `import pkg` at the
/// top of the file, in the existing top-of-file import block (or
/// starting one after the package docstring when no imports exist).
fn add_import_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    imported: &BString,
    title: String,
    uri: &Url,
) -> Option<CodeAction> {
    let source = db.get_source(package);
    let edit = insert_import_edit(&source, parsing, imported.as_bstr());
    Some(quickfix_action(title, uri, vec![edit]))
}

/// The quickfix for `ImportNotAtTopError`: move the misplaced `import`
/// statement to the top of the file — delete it here, insert it after
/// the package docstring and the existing top-of-file import block.
/// The payload carries no data, so the imported name comes from the
/// statement's AST node.
fn move_import_to_top_action(
    db: &Db,
    parsing: &Parsing,
    package: PackageId,
    region: Region,
    uri: &Url,
) -> Option<CodeAction> {
    let source = db.get_source(package);
    let stmt = find_enclosing(
        parsing,
        parsing.at(region.start())?,
        |payload| matches!(payload, AstNodePayload::Import(_)),
    )?;
    let imported = parsing.string(stmt.import_package()?);
    let insert = insert_import_edit(&source, parsing, imported);
    let delete = delete_lines_edit(&source, stmt.span());
    Some(quickfix_action(
        format!("Move `import {}` to the top of the file", imported.to_str_lossy()),
        uri,
        vec![insert, delete],
    ))
}

/// An edit inserting `import <name>` at the top of the file: right
/// after the last statement of the leading run of `import`
/// statements, or — when the file has no imports yet — before its
/// first statement, past the leading `//!` package docstring (which
/// the `Package` node's span starts after, so the doc lines are
/// skipped in the raw text).
fn insert_import_edit(source: &Source, parsing: &Parsing, imported: &BStr) -> TextEdit {
    let text = source.text();
    let root = parsing.root();

    let top_imports_end: Option<usize> = root
        .children()
        .into_iter()
        .take_while(|child| matches!(child.payload(), AstNodePayload::Import(_)))
        .last()
        .map(|last| source.to_offset(last.span().end()).into());

    let (offset, new_text) = match top_imports_end {
        // Existing imports: extend the block on a new line of its own.
        Some(end) => (
            end,
            format!("\nimport {}", imported.to_str_lossy()),
        ),
        // No imports yet: the insertion point is the start of the
        // first non-docstring line.
        None => {
            let mut off = 0;
            loop {
                let Some(line_len) = text[off..].find(b"\n") else { break };
                if !text[off..].starts_with(b"//!") {
                    break;
                }
                off += line_len + 1;
            }
            (off, format!("import {}\n", imported.to_str_lossy()))
        }
    };

    let position = linecol_to_position(source.to_linecol(SourceOffset::new(offset as u32)));
    TextEdit {
        range: Range { start: position, end: position },
        new_text,
    }
}

/// An edit replacing the driver operator of the driver statement
/// `stmt_node` (a `Driver` or `IndexedDriver` node) with `new_op`.  The
/// operator is the two bytes just before the RHS expression start,
/// after back-scanning whitespace — every driver operator (`:=`,
/// `<=`, `#=`, `#-`) is exactly two bytes.
fn replace_driver_operator_edit(
    source: &Source,
    stmt_node: &AstNode<'_>,
    new_op: &str,
) -> Option<TextEdit> {
    let rhs = stmt_node.driver()?;
    let text = source.text();
    let mut i: usize = source.to_offset(rhs.span().start()).into();
    while i > 0 && matches!(text[i - 1], b' ' | b'\t' | b'\r' | b'\n') {
        i -= 1;
    }
    let op = &text[i - 2..i];
    if op != b":=" && op != b"<=" && op != b"#=" && op != b"#-" {
        return None;
    }
    Some(TextEdit {
        range: Range {
            start: linecol_to_position(source.to_linecol(SourceOffset::new((i - 2) as u32))),
            end: linecol_to_position(source.to_linecol(SourceOffset::new(i as u32))),
        },
        new_text: new_op.to_string(),
    })
}

fn parse_word_literal_value(literal: &str) -> u64 {
    let trimmed = if let Some((value, _width)) = literal.split_once('w') {
        value.to_string()
    } else {
        literal.to_string()
    };
    let trimmed = trimmed.replace('_', "");
    if let Some(hex) = trimmed.strip_prefix("0x") {
        u64::from_str_radix(hex, 16).unwrap_or(0)
    } else if let Some(bin) = trimmed.strip_prefix("0b") {
        u64::from_str_radix(bin, 2).unwrap_or(0)
    } else {
        trimmed.parse().unwrap_or(0)
    }
}

fn workspace_root(uri: &Url) -> std::path::PathBuf {
    let dirpath = std::path::PathBuf::from(uri.path()).parent().unwrap().to_path_buf();
    dirpath
}

fn uri_to_package_name(uri: &Url) -> BString {
    Path::new(uri.path())
        .file_stem()
        .map(|stem| BString::from(stem.as_bytes()))
        .unwrap_or_else(|| BString::from(uri.path().as_bytes()))
}

fn uri_to_package(db: &Db, uri: &Url) -> PackageId {
    use bstr::ByteSlice as _;

    let name = uri_to_package_name(uri);
    db.get_packages()
        .id(name.as_bstr())
        .unwrap_or_else(|| panic!("Package not loaded: {name:?}"))
}

fn package_id_to_uri(db: &Db, package: PackageId, sibling: &Url) -> Url {
    use bstr::ByteSlice as _;

    let packages = db.get_packages();
    if package == packages.builtin() {
        let path = LIB_DIR.join("builtin.vir");
        Url::from_file_path(&path).unwrap()
    } else {
        let name = packages.name(package).to_str_lossy();
        let path = Path::new(sibling.path())
            .parent()
            .unwrap()
            .join(format!("{name}.vir"));
        Url::from_file_path(&path).unwrap()
    }
}

fn span_to_range(span: Span) -> Range {
    Range {
        start: linecol_to_position(span.start()),
        end: linecol_to_position(span.end()),
    }
}

#[cfg(test)]
#[test]
fn test_escape_markdown() {
    assert_eq!(escape_markdown("Word[8]"), "Word\\[8\\]");
}

#[allow(dead_code)]
fn escape_markdown(text: impl AsRef<str>) -> String {
    let mut escaped = String::with_capacity(text.as_ref().len());

    for ch in text.as_ref().chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '`' => escaped.push_str("\\`"),
            '*' => escaped.push_str("\\*"),
            '_' => escaped.push_str("\\_"),
            '{' => escaped.push_str("\\{"),
            '}' => escaped.push_str("\\}"),
            '[' => escaped.push_str("\\["),
            ']' => escaped.push_str("\\]"),
            '(' => escaped.push_str("\\("),
            ')' => escaped.push_str("\\)"),
            '#' => escaped.push_str("\\#"),
            '+' => escaped.push_str("\\+"),
            '-' => escaped.push_str("\\-"),
            '.' => escaped.push_str("\\."),
            '!' => escaped.push_str("\\!"),
            '|' => escaped.push_str("\\|"),
            '>' => escaped.push_str("&gt;"),
            '<' => escaped.push_str("&lt;"),
            '&' => escaped.push_str("&amp;"),
            '"' => escaped.push_str("&quot;"),
            '\'' => escaped.push_str("&#39;"),
            _ => escaped.push(ch),
        }
    }

    escaped
}

// ---------------------------------------------------------------------------
// CLI query subcommands
// ---------------------------------------------------------------------------

// The CLI query surface shares its query core with the LSP handlers
// (see queries.rs): every subcommand below renders the same data the
// server serves, as human text or JSON.

/// Parses a CLI location argument in the `<file_or_package>:<line>:<col>`
/// format (1-based, matching how `vir` reports diagnostics).
fn parse_cli_location(location: &str) -> CliResult<(String, usize, usize)> {
    let parts: Vec<&str> = location.rsplitn(3, ':').collect();
    if parts.len() != 3 {
        return Err(format!(
            "Location must be in the format <file_or_package>:<line>:<col>, got {location:?}",
        ));
    }
    let col: usize = parts[0]
        .parse()
        .map_err(|_| format!("Invalid column number: {:?}", parts[0]))?;
    let line: usize = parts[1]
        .parse()
        .map_err(|_| format!("Invalid line number: {:?}", parts[1]))?;
    Ok((parts[2].to_string(), line, col))
}

/// Standard project discovery from a directory: a manifest project when
/// `virdant.toml` (or deprecated `Virdant.toml`) is found there, else an
/// ad-hoc source directory when the directory directly contains `*.vir`
/// files (the pre-existing `vir ls hover <file>` behavior), else a
/// missing-project error.
fn project_for_dir(dir: &Path) -> CliResult<Project> {
    if vircommon::project_manifest(dir).is_some() {
        return Project::from_project_dir(dir);
    }
    let has_sources = std::fs::read_dir(dir)
        .map_err(|e| format!("Cannot read directory {}: {e}", dir.display()))?
        .filter_map(|entry| entry.ok().map(|entry| entry.path()))
        .any(|path| matches!(path.extension(), Some(ext) if ext == "vir"));
    if has_sources {
        Project::from_source_dir(dir)
    } else {
        Err(format!("No virdant.toml found in {}", dir.display()))
    }
}

/// The project for a `.vir` file location: standard discovery when an
/// ancestor of the file has a manifest, else the file's directory as an
/// ad-hoc source directory.
fn project_for_file_dir(source_dir: &Path) -> CliResult<Project> {
    let mut ancestor = Some(source_dir.to_path_buf());
    while let Some(dir) = ancestor {
        if vircommon::project_manifest(&dir).is_some() {
            return Project::from_project_dir(&dir);
        }
        ancestor = dir.parent().map(|parent| parent.to_path_buf());
    }
    Project::from_source_dir(source_dir)
}

/// Resolves a `<file_or_package>:<line>:<col>` location argument against
/// the project dir: the file part may be a `.vir` path (resolved
/// relative to the project dir) or a bare package name.  Returns the
/// loaded project, the package id (when it exists in the project), and
/// the 1-based line/col.
fn resolve_location(dir: &Path, location: &str) -> CliResult<(Project, Option<PackageId>, LineCol)> {
    let (file_or_package, line, col) = parse_cli_location(location)?;
    let linecol = LineCol::new(line, col);

    let is_file = Path::new(&file_or_package).extension().map_or(false, |ext| ext == "vir");
    let (package_name, source_dir) = if is_file {
        let path = if Path::new(&file_or_package).is_absolute() {
            PathBuf::from(&file_or_package)
        } else {
            dir.join(&file_or_package)
        };
        if !path.is_file() {
            return Err(format!("File not found: {}", path.display()));
        }
        let stem = path.file_stem().unwrap().to_string_lossy().into_owned();
        let parent = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        (stem, parent)
    } else {
        (file_or_package.clone(), dir.to_path_buf())
    };

    let project = if is_file {
        project_for_file_dir(&source_dir)?
    } else {
        project_for_dir(dir)?
    };

    let package = project.db.get_packages().id(BString::from(package_name.as_bytes()).as_bstr());

    // File-location fallback: when the manifest project does not contain
    // the package (e.g. a stdlib file or a stray source), retry loading
    // the file's directory as an ad-hoc source directory.
    let (project, package) = match package {
        Some(package) => (project, Some(package)),
        None if is_file => {
            let project = Project::from_source_dir(&source_dir)?;
            let package = project.db.get_packages().id(BString::from(package_name.as_bytes()).as_bstr());
            (project, package)
        }
        None => (project, None),
    };

    Ok((project, package, linecol))
}

fn run_hover(dir: &Path, location: &str, json: bool) -> CliResult<()> {
    let (project, package, linecol) = resolve_location(dir, location)?;

    let Some(package) = package else {
        if json {
            println!("{}", json!({ "found": false }));
        } else {
            println!("No package found for {location} in {}", dir.display());
        }
        return Ok(());
    };

    let Some(hover) = queries::hover_at(&project.db, package, linecol) else {
        if json {
            println!("{}", json!({ "found": false }));
        } else {
            println!("No node at {location}");
        }
        return Ok(());
    };

    let package_name = project.db.get_packages().name(package).to_str_lossy().into_owned();
    if json {
        let mut value = json!({
            "found": true,
            "package": package_name,
            "spelling": hover.spelling,
            "summary": hover.summary,
        });
        if let Some(typ) = hover.typof_source {
            value["type"] = json!(typ);
        }
        println!("{value}");
    } else {
        println!("Spelling : {}", hover.spelling);
        println!("Summary  : {}", hover.summary);
        println!("Package  : {package_name}");
        // The node's source span (1-based, like diagnostics), not the
        // internal AstNodeId debug form.
        println!(
            "Location : {}:{}:{}",
            project.file_json_path(package),
            hover.span.start().line(),
            hover.span.start().col(),
        );
        match (&hover.typof_source, &hover.typof_debug) {
            (Some(source), _) => println!("Typeof   : {source}"),
            (None, Some(debug)) => println!("Typeof   : {debug}"),
            (None, None) => {}
        }
    }
    Ok(())
}

fn run_definition(dir: &Path, target: &str, json: bool) -> CliResult<()> {
    // The target is either a location or a (possibly qualified) name.
    let (definitions, ambiguous) = if parse_cli_location(target).is_ok() {
        let (project, package, linecol) = resolve_location(dir, target)?;
        let definitions = match package {
            None => vec![],
            Some(package) => match queries::resolve_definition_at(&project.db, package, linecol) {
                Some(location) => vec![queries::location_json(&project, location)],
                None => vec![],
            },
        };
        (definitions, false)
    } else {
        let project = project_for_dir(dir)?;
        match queries::resolve_name(&project.db, target) {
            NameResolution::Found(symbol) =>
                (vec![queries::location_json(&project, symbol.location())], false),
            NameResolution::Ambiguous(symbols) => (
                symbols.iter().map(|symbol| queries::location_json(&project, symbol.location())).collect(),
                true,
            ),
            NameResolution::NotFound => (vec![], false),
        }
    };

    let found = !definitions.is_empty();
    if json {
        println!("{}", json!({ "found": found, "ambiguous": ambiguous, "definitions": definitions }));
    } else if definitions.is_empty() {
        println!("No definition found for {target}");
    } else {
        for definition in &definitions {
            println!("{} {}:{}", definition["file"], definition["line"], definition["col"]);
        }
    }
    Ok(())
}

/// The JSON form of a reference scan: the canonical location shape plus
/// the reference kind.
fn references_json(project: &Project, symbol: &virdant::analysis::symbols::Symbol) -> Vec<serde_json::Value> {
    queries::references(&project.db, symbol)
        .into_iter()
        .map(|reference| {
            let mut value = queries::location_json(project, reference.location);
            value["kind"] = json!(reference.kind);
            value
        })
        .collect()
}

fn run_references(dir: &Path, target: &str, json: bool) -> CliResult<()> {
    let (found, ambiguous, references) = if parse_cli_location(target).is_ok() {
        let (project, package, linecol) = resolve_location(dir, target)?;
        match package.and_then(|package| queries::resolve_symbol_at(&project.db, package, linecol)) {
            Some(symbol) => (true, false, references_json(&project, &symbol)),
            None => (false, false, vec![]),
        }
    } else {
        let project = project_for_dir(dir)?;
        match queries::resolve_name(&project.db, target) {
            NameResolution::Found(symbol) => (true, false, references_json(&project, &symbol)),
            NameResolution::Ambiguous(symbols) => (false, true, vec![
                // An ambiguous target has no single reference set.
                json!({ "ambiguous": symbols.iter().map(|symbol| symbol.fqn().to_str_lossy().into_owned()).collect::<Vec<_>>() })
            ]),
            NameResolution::NotFound => (false, false, vec![]),
        }
    };

    if json {
        println!("{}", json!({ "found": found, "ambiguous": ambiguous, "references": references }));
    } else if !found && ambiguous {
        println!("Ambiguous name {target}");
    } else if references.is_empty() {
        println!("No references found for {target}");
    } else {
        for reference in &references {
            println!("{} {}:{}  [{}]", reference["file"], reference["line"], reference["col"], reference["kind"]);
        }
    }
    Ok(())
}

fn run_diagnostics(dir: &Path, json: bool) -> CliResult<()> {
    let project = project_for_dir(dir)?;
    let diagnostics = queries::diagnostics(&project.db);
    if json {
        let diagnostics: Vec<serde_json::Value> = diagnostics.iter().map(|diag| {
            let mut value = queries::span_json(&project, diag.package, diag.span);
            value["level"] = json!(diag.level);
            value["message"] = json!(diag.message);
            value
        }).collect();
        println!("{}", json!(diagnostics));
    } else {
        for diag in &diagnostics {
            let packages = project.db.get_packages();
            let package = packages.name(diag.package).to_str_lossy().into_owned();
            println!(
                "{:6}  {}.vir [{}:{}]  {}",
                diag.level,
                package,
                diag.span.start().line(),
                diag.span.start().col(),
                diag.message,
            );
        }
    }
    Ok(())
}

fn run_symbols(dir: &Path, package: Option<&str>, json: bool) -> CliResult<()> {
    let project = project_for_dir(dir)?;
    let package_id = match package {
        Some(package) => {
            let package_id = project.db.get_packages().id(BString::from(package.as_bytes()).as_bstr());
            if package_id.is_none() {
                if json {
                    println!("[]");
                } else {
                    println!("No package named {package} in {}", dir.display());
                }
                return Ok(());
            }
            package_id
        }
        None => None,
    };

    let symbols = queries::symbols(&project.db, package_id);
    if json {
        let symbols: Vec<serde_json::Value> = symbols.iter().map(|symbol| {
            let mut value = queries::location_json(&project, symbol.location);
            value["kind"] = json!(symbol.kind);
            value["name"] = json!(symbol.name);
            value["package"] = json!(project.db.get_packages().name(symbol.package).to_str_lossy());
            value
        }).collect();
        println!("{}", json!(symbols));
    } else {
        for symbol in &symbols {
            let packages = project.db.get_packages();
            let package_name = packages.name(symbol.package).to_str_lossy().into_owned();
            let parsing = project.db.get_parsing(symbol.package);
            let node = parsing.ast_node(symbol.location.ast_node_id());
            println!("{:9}  {}.{}  [{}]", symbol.kind, package_name, symbol.name, node.span());
        }
    }
    Ok(())
}

fn run_components(dir: &Path, moddef: &str, json: bool) -> CliResult<()> {
    let project = project_for_dir(dir)?;
    let symbol = match queries::resolve_name(&project.db, moddef) {
        NameResolution::Found(symbol) => symbol,
        NameResolution::Ambiguous(symbols) => {
            let names: Vec<String> = symbols.iter()
                .map(|symbol| symbol.fqn().to_str_lossy().into_owned())
                .collect();
            if json {
                println!("{}", json!({ "found": false, "ambiguous": names, "components": [] }));
            } else {
                println!("Ambiguous name {moddef}: {}", names.join(", "));
            }
            return Ok(());
        }
        NameResolution::NotFound => {
            if json {
                println!("{}", json!({ "found": false, "components": [] }));
            } else {
                println!("No item named {moddef} in {}", dir.display());
            }
            return Ok(());
        }
    };

    if !matches!(symbol.kind(), virdant::analysis::symbols::SymbolKind::ModDef | virdant::analysis::symbols::SymbolKind::Platform) {
        let fqn = symbol.fqn().to_str_lossy();
        if json {
            println!("{}", json!({ "found": false, "components": [], "error": format!("{fqn} is not a module or platform") }));
        } else {
            println!("{fqn} is not a module or platform");
        }
        return Ok(());
    }

    let components = queries::components(&project.db, &symbol);
    if json {
        let components: Vec<serde_json::Value> = components.iter().map(|component| {
            json!({
                "path": component.path,
                "kind": component.kind,
                "type": component.typ,
                "synthetic": component.synthetic,
            })
        }).collect();
        println!("{}", json!({ "found": true, "moddef": symbol.fqn().to_str_lossy(), "components": components }));
    } else {
        println!("Components of {}:", symbol.fqn().to_str_lossy());
        for component in &components {
            let kind = component.kind.unwrap_or("socket");
            let typ = component.typ.as_deref().unwrap_or("?");
            let synthetic = if component.synthetic { " (synthetic)" } else { "" };
            println!("  {:9}  {} : {}{}", kind, component.path, typ, synthetic);
        }
    }
    Ok(())
}

fn run_type_of(dir: &Path, location: &str, json: bool) -> CliResult<()> {
    let (project, package, linecol) = resolve_location(dir, location)?;
    let Some(package) = package else {
        if json {
            println!("{}", json!({ "found": false }));
        } else {
            println!("No package found for {location} in {}", dir.display());
        }
        return Ok(());
    };

    let Some(info) = queries::typeof_at(&project.db, package, linecol) else {
        if json {
            println!("{}", json!({ "found": false }));
        } else {
            println!("No node at {location}");
        }
        return Ok(());
    };

    match info.typ {
        Some(typ) => {
            if json {
                println!("{}", json!({ "found": true, "type": typ }));
            } else {
                println!("Type of {location}: {typ}");
            }
        }
        None => {
            let error = info.error.unwrap_or_default();
            if json {
                println!("{}", json!({ "found": false, "error": error }));
            } else {
                println!("No type at {location}: {error}");
            }
        }
    }
    Ok(())
}

#[derive(Parser, Debug)]
#[command(name = "vir-ls", author, version, about, subcommand_required = true, arg_required_else_help = true)]
struct Cli {
    /// Dump the arg-parse data as JSON and exit (hidden)
    #[arg(long, hide = true)]
    help_json: bool,

    /// Project directory (default: the current directory)
    #[arg(short = 'C', long = "dir", global = true)]
    dir: Option<std::path::PathBuf>,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Run the language server
    Serve {},

    /// Hover information at a source location
    Hover {
        /// Location as <file_or_package>:<line>:<col>
        /// e.g. src/top.vir:10:5 or top:10:5
        location: String,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// The definition of the item at a location, or of a name
    Definition {
        /// <file_or_package>:<line>:<col> or a name (e.g. Foo::Bar)
        target: String,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// Locations that reference the target
    References {
        /// A name (e.g. Foo::Bar) or <file_or_package>:<line>:<col>
        target: String,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// Project diagnostics (exits 0 even when errors are reported)
    Diagnostics {
        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// Items (moddef, structdef, ...) in one package or the whole project
    Symbols {
        /// Restrict to one package (default: all packages)
        package: Option<String>,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// A module's components (incoming/outgoing/reg/wire/...)
    Components {
        /// A module definition name (optionally qualified: pkg::ModDef)
        moddef: String,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },

    /// The type of the expression at a location
    TypeOf {
        /// Location as <file_or_package>:<line>:<col>
        location: String,

        /// Render the output as JSON
        #[arg(long)]
        json: bool,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    if cli.help_json {
        help_json::dump(&Cli::command());
    }

    // The project directory: -C/--dir when given, else the current
    // directory.  All queries resolve their targets against it.
    let dir: PathBuf = match &cli.dir {
        Some(dir) => match std::fs::canonicalize(dir) {
            Ok(path) => path,
            Err(e) => {
                eprintln!("error: cannot resolve directory {}: {e}", dir.display());
                std::process::exit(2);
            }
        },
        None => match std::env::current_dir() {
            Ok(path) => path,
            Err(e) => {
                eprintln!("error: cannot determine current directory: {e}");
                std::process::exit(2);
            }
        },
    };

    let result = match cli.command {
        Command::Serve {} => {
            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();
            let (service, socket) = LspService::new(|client| Backend::new(client));
            Server::new(stdin, stdout, socket).serve(service).await;
            Ok(())
        }
        Command::Hover { location, json } => run_hover(&dir, &location, json),
        Command::Definition { target, json } => run_definition(&dir, &target, json),
        Command::References { target, json } => run_references(&dir, &target, json),
        Command::Diagnostics { json } => run_diagnostics(&dir, json),
        Command::Symbols { package, json } => run_symbols(&dir, package.as_deref(), json),
        Command::Components { moddef, json } => run_components(&dir, &moddef, json),
        Command::TypeOf { location, json } => run_type_of(&dir, &location, json),
    };

    // Exit-code contract: exit 2 only when the tool could not run (bad
    // args, missing project, internal failure); a query that ran but
    // found nothing exits 0 with the answer in its output.
    if let Err(msg) = result {
        eprintln!("error: {msg}");
        std::process::exit(2);
    }
}

#[cfg(test)]
mod platform_code_action_tests {
    use super::*;

    fn test_db(design: &str) -> Db {
        let builtin_text: BString = include_bytes!("../../../lib/builtin.vir").as_ref().into();
        let design_text: BString = design.as_bytes().into();

        let mut db = Db::new();
        db.set_packages(PackageTable::new(vec!["builtin".into(), "design".into()]));
        let packages = db.get_packages();
        let builtin = packages.id(b"builtin".as_bstr()).unwrap();
        let design_pkg = packages.id(b"design".as_bstr()).unwrap();
        db.set_source(builtin, Source::new(builtin, builtin_text));
        db.set_source(design_pkg, Source::new(design_pkg, design_text));
        db
    }

    #[test]
    fn adds_missing_port_before_existing_statement() {
        let design = "\
platform IceSugar {
    @pin(35)
    incoming clock : Clock

    @pin(39)
    outgoing led : Bit
}

mod Top for IceSugar {
    incoming clock : Clock

    unused clock
}
";
        let db = test_db(design);
        let design_pkg = db.get_packages().id(b"design".as_bstr()).unwrap();
        let parsing = db.get_parsing(design_pkg);

        // Inside `incoming clock : Clock` on the first line of Top's body.
        let node_id = parsing.at(LineCol::new(10, 14)).expect("node at position");
        let uri = Url::parse("file:///design.vir").unwrap();

        let action = missing_platform_ports_action(&db, &parsing, design_pkg, node_id, &uri)
            .expect("expected a code action");

        assert_eq!(action.title, "Add missing ports required by platform IceSugar");

        let edits = action.edit.unwrap().changes.unwrap().remove(&uri).unwrap();
        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "outgoing led : Bit {\n        it := ?\n    }\n    ",
        );
        // The insertion point is right before the existing first statement.
        assert_eq!(edits[0].range.start, Position { line: 9, character: 4 });
        assert_eq!(edits[0].range, Range { start: edits[0].range.start, end: edits[0].range.start });
    }

    #[test]
    fn adds_all_ports_to_empty_body() {
        let design = "\
platform IceSugar {
    @pin(35)
    incoming clock : Clock
}

mod Top for IceSugar {
}
";
        let db = test_db(design);
        let design_pkg = db.get_packages().id(b"design".as_bstr()).unwrap();
        let parsing = db.get_parsing(design_pkg);

        // On `Top` in the `mod Top for IceSugar {` line.
        let node_id = parsing.at(LineCol::new(6, 5)).expect("node at position");
        let uri = Url::parse("file:///design.vir").unwrap();

        let action = missing_platform_ports_action(&db, &parsing, design_pkg, node_id, &uri)
            .expect("expected a code action");

        let edits = action.edit.unwrap().changes.unwrap().remove(&uri).unwrap();
        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "\n    incoming clock : Clock {\n        unused it\n    }",
        );
    }

    #[test]
    fn no_action_when_ports_already_match() {
        let design = "\
platform IceSugar {
    @pin(35)
    incoming clock : Clock
}

mod Top for IceSugar {
    incoming clock : Clock
    unused clock
}
";
        let db = test_db(design);
        let design_pkg = db.get_packages().id(b"design".as_bstr()).unwrap();
        let parsing = db.get_parsing(design_pkg);

        let node_id = parsing.at(LineCol::new(6, 5)).expect("node at position");
        let uri = Url::parse("file:///design.vir").unwrap();

        assert!(missing_platform_ports_action(&db, &parsing, design_pkg, node_id, &uri).is_none());
    }

    #[test]
    fn no_action_without_for_clause() {
        let design = "\
mod Top {
    incoming clock : Clock
    unused clock
}
";
        let db = test_db(design);
        let design_pkg = db.get_packages().id(b"design".as_bstr()).unwrap();
        let parsing = db.get_parsing(design_pkg);

        let node_id = parsing.at(LineCol::new(1, 5)).expect("node at position");
        let uri = Url::parse("file:///design.vir").unwrap();

        assert!(missing_platform_ports_action(&db, &parsing, design_pkg, node_id, &uri).is_none());
    }
}
