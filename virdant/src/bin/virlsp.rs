use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::sync::Arc;

use clap::{Parser, Subcommand};

use bstr::BString;
use indexmap::IndexSet;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use serde_json::json;

use virdant::LIB_DIR;
use virdant::util::{db_from_dir, db_from_dir_with_platform_in, read_platform_from_toml};
use virdant::analysis::symbols::{SymbolId, SymbolKind, SymbolTable};
use virdant::types::{ExprRoot, Type};
use virdant::db::Db;
use virdant::fqn::PackageFqn;
use virdant::common::source::{LineCol, Source, SourceOffset, Span};
use virdant::syntax::ast::{AstNode, AstNodeId, match_arm_children};
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
    db.set_packages(vec![]);
    let builtin = PackageFqn::new("builtin".into());
    let mut packages = db.get_packages().as_ref().clone();
    packages.push(builtin.clone());
    db.set_packages(packages);
    let builtin_source = Source::new(builtin.clone(), include_bytes!("../../../lib/builtin.vir").as_ref().into());
    db.set_source(builtin, builtin_source);
    db
}

fn db_is_empty(db: &Db) -> bool {
    db.get_packages().len() < 2
}

fn find_project_root(uri: &Url) -> Option<std::path::PathBuf> {
    let mut dir = std::path::PathBuf::from(uri.path());
    dir.pop();
    loop {
        if dir.join("Virdant.toml").is_file() {
            return Some(dir);
        }
        if !dir.pop() {
            return None;
        }
    }
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

        let package = uri_to_packagefqn(&uri);
        let text: BString = params.text_document.text.into();

        {
            let mut state = self.state.lock().await;
            if db_is_empty(&state.db) {
                let workspace = workspace_root(&uri);
                let project = find_project_root(&uri);
                if let Some(ref project_root) = project {
                    let src_dir = project_root.join("src");
                    if src_dir.is_dir() {
                        match read_platform_from_toml(project_root) {
                            Ok(Some(platform)) => {
                                state.db = db_from_dir_with_platform_in(&src_dir, project_root, &platform);
                            }
                            Ok(None) => {
                                state.db = db_from_dir(&src_dir);
                            }
                            Err(e) => {
                                self.client
                                    .log_message(MessageType::WARNING, format!("failed to read Virdant.toml: {e}"))
                                    .await;
                                state.db = db_from_dir(&workspace);
                            }
                        }
                    } else {
                        state.db = db_from_dir(&workspace);
                    }
                } else {
                    state.db = db_from_dir(&workspace);
                }
            }

            let new_source = Source::new(package.clone(), text);
            state.db.set_source(package.clone(), new_source);
            let packages = state.db.get_packages();
            if !packages.contains(&package) {
                let mut packages = packages.as_ref().clone();
                packages.push(package.clone());
                state.db.set_packages(packages);
            }

            state.uris.insert(uri.clone());
        }

        self.check_all_documents().await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        self.client
            .log_message(MessageType::ERROR, format!("File changed: {uri}"))
            .await;

        let package = uri_to_packagefqn(&uri);
        let text: BString = params.content_changes[0].text.clone().into();

        {
            let mut state = self.state.lock().await;
            if db_is_empty(&state.db) {
                let workspace = workspace_root(&uri);
                let project = find_project_root(&uri);
                if let Some(ref project_root) = project {
                    let src_dir = project_root.join("src");
                    if src_dir.is_dir() {
                        match read_platform_from_toml(project_root) {
                            Ok(Some(platform)) => {
                                state.db = db_from_dir_with_platform_in(&src_dir, project_root, &platform);
                            }
                            Ok(None) => {
                                state.db = db_from_dir(&src_dir);
                            }
                            Err(e) => {
                                self.client
                                    .log_message(MessageType::WARNING, format!("failed to read Virdant.toml: {e}"))
                                    .await;
                                state.db = db_from_dir(&workspace);
                            }
                        }
                    } else {
                        state.db = db_from_dir(&workspace);
                    }
                } else {
                    state.db = db_from_dir(&workspace);
                }
            }
            let packages = state.db.get_packages();
            if !packages.contains(&package) {
                let mut packages = packages.as_ref().clone();
                packages.push(package.clone());
                state.db.set_packages(packages);
            }
            let new_source = Source::new(package.clone(), text);
            state.db.set_source(package.clone(), new_source);
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
        let package = uri_to_packagefqn(&uri);

        self.client
            .log_message(MessageType::INFO, &format!("GOTO DEF at {linecol:?}"))
            .await;

        let state = self.state.lock().await;
        let db = &state.db;
        let parsing = db.get_parsing(package.clone());

        let Some(node_id) = parsing.at(linecol) else {
            return Ok(None);
        };
        let node = parsing.ast_node(node_id);

        self.client
            .log_message(MessageType::INFO, &format!("NODE: {:?}", node.summary()))
            .await;

        let target_loc = match node.payload() {
            AstNodePayload::Ofness(ofness) => {
                // Resolve the item this Ofness names (e.g. `Gcd` in `mod gcd of Gcd`).
                let symboltable = db.get_symboltable();
                let item_pkg = ofness.package
                    .map(|pkg| PackageFqn::new(parsing.string(pkg).to_owned()))
                    .unwrap_or_else(|| package.clone());
                let item_name = parsing.string(ofness.name);
                self.client
                    .log_message(MessageType::INFO, &format!("item_name: {item_name:?}"))
                    .await;
                symboltable.resolve_item(item_name, item_pkg)
                    .map(|sym| sym.location())
            }
            AstNodePayload::Path(path_payload) => {
                // Walk up the AST to find the module that contains this path.
                let mut current_id = node_id;
                loop {
                    let n = parsing.ast_node(current_id);
                    if n.is_item() { break; }
                    match n.parent().map(|p| p.id()) {
                        Some(id) => current_id = id,
                        None => return Ok(None),
                    }
                }
                let current = parsing.ast_node(current_id);
                let item_name = parsing.string(current.name().unwrap());
                let symboltable = db.get_symboltable();
                let Some(item_symbol) = symboltable.resolve_item_in_package(item_name, package.clone()) else {
                    return Ok(None);
                };
                // Resolve the path string to a component in that module's analysis.
                let component_analysis = db.get_component_analysis(item_symbol.id);
                let path_str = parsing.string(path_payload.path);
                component_analysis.resolve(path_str)
                    .map(|comp| comp.location())
            }
            AstNodePayload::ExprReference => {
                // ExprReference wraps a Path child; node.path() extracts its interned string.
                let Some(path_interned) = node.path() else { return Ok(None); };
                // Walk up to the enclosing module item.
                let mut current_id = node_id;
                loop {
                    let n = parsing.ast_node(current_id);
                    if n.is_item() { break; }
                    match n.parent().map(|p| p.id()) {
                        Some(id) => current_id = id,
                        None => return Ok(None),
                    }
                }
                let current = parsing.ast_node(current_id);
                let item_name = parsing.string(current.name().unwrap());
                let symboltable = db.get_symboltable();
                let Some(item_symbol) = symboltable.resolve_item_in_package(item_name, package.clone()) else {
                    return Ok(None);
                };
                let component_analysis = db.get_component_analysis(item_symbol.id);
                let path_str = parsing.string(path_interned);
                component_analysis.resolve(path_str)
                    .map(|comp| comp.location())
            }
            AstNodePayload::Type(_) => {
                // A Type node's first child is always an Ofness carrying the type name.
                let ofness_node = node.child(0);
                self.client
                    .log_message(MessageType::INFO, &format!("ofness_node = {:?}", ofness_node.summary()))
                    .await;
                let AstNodePayload::Ofness(ofness) = ofness_node.payload() else { return Ok(None); };
                let symboltable = db.get_symboltable();
                let item_pkg = ofness.package
                    .map(|pkg| PackageFqn::new(parsing.string(pkg).to_owned()))
                    .unwrap_or_else(|| package.clone());
                let item_name = parsing.string(ofness.name);
                symboltable.resolve_item(item_name, item_pkg)
                    .map(|sym| sym.location())
            }
            _ => None,
        };

        let Some(target_loc) = target_loc else {
            return Ok(None);
        };

        // Convert the Virdant Location to an LSP Location.
        let target_parsing = db.get_parsing(target_loc.package());
        let target_node = target_parsing.ast_node(target_loc.ast_node_id());
        let target_uri = packagefqn_to_uri(target_loc.package(), &uri);

        let package = target_loc.package();
        self.client
            .log_message(MessageType::INFO, &format!("GOing TO (package: {package}) {target_uri} {}", target_node.span()))
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

        let package = uri_to_packagefqn(&uri);
        let _parsing = {
            let state = self.state.lock().await;
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
        let package = uri_to_packagefqn(&uri);
        let parsing = {
            let state = self.state.lock().await;
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
        let package = uri_to_packagefqn(&uri);

        let state = self.state.lock().await;
        let db = &state.db;
        let parsing = db.get_parsing(package.clone());

        let Some(node_id) = parsing.at(linecol) else { return Ok(None); };
        let Some(match_node) = find_enclosing_match(&parsing, node_id) else { return Ok(None); };

        let children = match_node.children();
        if children.is_empty() { return Ok(None); }
        let subject = &children[0];
        let Ok(subject_typ) = db.get_typeof(subject.location()) else { return Ok(None); };

        let symboltable = db.get_symboltable();
        let missing = missing_match_patterns(db, &parsing, &match_node, &subject_typ, &symboltable);
        if missing.is_empty() { return Ok(None); }

        let is_stmt = matches!(match_node.payload(), AstNodePayload::ModDefStmtMatch);
        let arm_body = if is_stmt { "{ }" } else { "?" };

        let arms = match_arm_children(&children);
        let first_pat_col = arms.iter().find_map(|(p, _)| p.map(|n| n.span().start().col()));
        let indent_col = match first_pat_col {
            Some(col) => col.saturating_sub(5).max(1),
            None => match_node.span().start().col().saturating_add(4),
        };
        let indent_str = " ".repeat(indent_col.saturating_sub(1));

        // Find an insertion point immediately after the last non-whitespace byte
        // before the closing `}` of the match, so the `}` keeps its layout.
        let source = db.get_source(package.clone());
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
        let insert_position = Position {
            line: (insert_lc.line().saturating_sub(1)) as u32,
            character: (insert_lc.col().saturating_sub(1)) as u32,
        };

        let mut new_text = String::new();
        for pat in &missing {
            new_text.push('\n');
            new_text.push_str(&indent_str);
            new_text.push_str("case ");
            new_text.push_str(pat);
            new_text.push_str(" => ");
            new_text.push_str(arm_body);
        }

        let edit = TextEdit {
            range: Range { start: insert_position, end: insert_position },
            new_text,
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert(uri.clone(), vec![edit]);

        let workspace_edit = WorkspaceEdit {
            changes: Some(changes),
            document_changes: None,
            change_annotations: None,
        };

        let action = CodeAction {
            title: "Add missing match cases".into(),
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: None,
            edit: Some(workspace_edit),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: None,
        };

        Ok(Some(vec![CodeActionOrCommand::CodeAction(action)]))
    }
}

impl Backend {
    #[allow(dead_code)]
    async fn source(&self, uri: &Url) -> Option<Source> {
        let state = self.state.lock().await;
        let package = uri_to_packagefqn(uri);
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

        let package = uri_to_packagefqn(&uri);

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
                code: None,
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
        let package = uri_to_packagefqn(&uri);

        let (parsing, _hover_mode) = {
            let state = self.state.lock().await;
            (state.db.get_parsing(package.clone()), state.hover_mode)
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
        let package = uri_to_packagefqn(&uri);

        let state = self.state.lock().await;
        let db = &state.db;

        let parsing = db.get_parsing(package.clone());

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
                    package,
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

fn find_enclosing_match<'p>(parsing: &'p Parsing, node_id: AstNodeId) -> Option<AstNode<'p>> {
    let mut current_id = node_id;
    loop {
        let node = parsing.ast_node(current_id);
        match node.payload() {
            AstNodePayload::ExprMatch | AstNodePayload::ModDefStmtMatch => return Some(node),
            _ => match node.parent().map(|p| p.id()) {
                Some(id) => current_id = id,
                None => return None,
            }
        }
    }
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
                let Type::Usual(td) = subject_typ else { continue };
                let name = parsing.string(pat_ctor.name);
                if let Some(sym) = symboltable.slot(*td, name) {
                    covered_syms.insert(sym.id());
                }
            }
            AstNodePayload::PatEnumerant(pat_enum) => {
                let Type::Usual(td) = subject_typ else { continue };
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
        Type::Usual(td) => {
            let sym = symboltable.symbol(*td);
            match sym.kind() {
                SymbolKind::EnumDef => symboltable
                    .slots(*td)
                    .iter()
                    .filter(|s| !covered_syms.contains(&s.id()))
                    .map(|s| format!("#{}", s.name().to_str_lossy()))
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

fn uri_to_packagefqn(uri: &Url) -> PackageFqn {
    Path::new(uri.path())
        .file_stem()
        .map(|stem| PackageFqn::new(stem.as_bytes().into()))
        .unwrap_or_else(|| PackageFqn::new(uri.path().as_bytes().into()))
}

fn packagefqn_to_uri(package: PackageFqn, sibling: &Url) -> Url {
    if package == PackageFqn::new("builtin".into()) {
        let path = LIB_DIR.join("builtin.vir");
        Url::from_file_path(&path).unwrap()
    } else {
        let path = Path::new(sibling.path())
            .parent()
            .unwrap()
            .join(format!("{package}.vir"));
        Url::from_file_path(&path).unwrap()
    }
}

fn span_to_range(span: Span) -> Range {
    Range {
        start: Position {
            line: span.start().line().saturating_sub(1) as u32,
            character: span.start().col().saturating_sub(1) as u32,
        },
        end: Position {
            line: span.end().line().saturating_sub(1) as u32,
            character: span.end().col().saturating_sub(1) as u32,
        },
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

fn dump_hover(workspace_dir: &std::path::Path, package: PackageFqn, linecol: LineCol) {
    let db = db_from_dir(workspace_dir);
    if !db.get_packages().contains(&package) {
        eprintln!(
            "error: package {package} not found in workspace {}",
            workspace_dir.display(),
        );
        return;
    }
    let parsing = db.get_parsing(package.clone());

    if let Some(node_id) = parsing.at(linecol) {
        let node = parsing.ast_node(node_id);

        println!("Spelling : {}", node.spelling());
        println!("Summary  : {}", node.summary());
        println!("Package  : {}", package);
        println!("Location : {:?}", node.id());

        if node.is_expr() {
            let typof = db.get_typeof(node.location()).ok();
            println!("Typeof   : {:?}", typof);
        }
    } else {
        println!("No node at {package} {linecol:?}");
    }
}

#[derive(Parser, Debug)]
#[command(name = "vir-lsp", author, version, about)]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Dump hover information for a given location
    Hover {
        /// Location as <file_or_package>:<line>:<col>
        /// e.g. temp/top.vir:10:5 or top:10:5
        location: String,
    },
}

fn parse_hover_location(location: &str) -> std::result::Result<(std::path::PathBuf, PackageFqn, LineCol), String> {
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
    let file_or_package = parts[2];

    let (workspace_dir, package) = if Path::new(file_or_package).extension().map_or(false, |ext| ext == "vir") {
        let path = Path::new(file_or_package);
        let workspace = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let pkg = path
            .file_stem()
            .map(|stem| PackageFqn::new(stem.as_bytes().into()))
            .ok_or_else(|| format!("Invalid file path: {file_or_package:?}"))?;
        (workspace, pkg)
    } else {
        let workspace = std::env::current_dir()
            .map_err(|e| format!("Cannot determine current directory: {e}"))?;
        let pkg = PackageFqn::new(file_or_package.as_bytes().into());
        (workspace, pkg)
    };

    Ok((workspace_dir, package, LineCol::new(line, col)))
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match cli.command {
        None => {
            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();
            let (service, socket) = LspService::new(|client| Backend::new(client));
            Server::new(stdin, stdout, socket).serve(service).await;
        }
        Some(Command::Hover { location }) => {
            match parse_hover_location(&location) {
                Ok((workspace_dir, package, linecol)) => {
                    dump_hover(&workspace_dir, package, linecol);
                }
                Err(msg) => {
                    eprintln!("error: {msg}");
                    std::process::exit(2);
                }
            }
        }
    }
}
