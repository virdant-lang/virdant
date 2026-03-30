use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::sync::Arc;

use clap::{Parser, Subcommand};

use bstr::BString;
use hashbrown::HashSet;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use serde_json::json;

use virdant::LIB_DIR;
use virdant::types::ExprRoot;
use virdant::db::Db;
use virdant::fqn::PackageFqn;
use virdant::common::source::{LineCol, Source, Span};
use virdant::syntax::ast::{AstNode, AstNodeId};
use virdant::syntax::parsing::Parsing;

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

struct ServerState {
    db: Db,
    uris: HashSet<Url>,
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
    let mut packages = db.get_packages();
    packages.push(builtin.clone());
    db.set_packages(packages);
    let builtin_source = Source::new(builtin.clone(), include_bytes!("../../../lib/builtin.vir").as_ref().into());
    db.set_source(builtin, builtin_source);
    db
}

fn db_from_dir<P: Into<std::path::PathBuf>>(source_dir: P) -> Db {
    let mut db = Db::new();
    db.set_packages(vec![]);
    let builtin_source = Source::load_file(LIB_DIR.join("builtin.vir"));
    let mut sources = vec![builtin_source.clone()];
    db.set_source(builtin_source.package(), builtin_source);
    for filepath in std::fs::read_dir(source_dir.into()).unwrap() {
        let filepath = match filepath {
            Ok(filepath) => filepath.path(),
            Err(_) => continue,
        };
        match filepath.extension() {
            Some(ext) if ext.to_string_lossy() == "vir" => (),
            _ => continue,
        }
        let source = Source::load_file(filepath);
        db.set_source(source.package(), source.clone());
        sources.push(source);
    }
    db.set_packages(sources.iter().map(|source| source.package()).collect());
    db
}

fn db_is_empty(db: &Db) -> bool {
    db.get_packages().len() < 2
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState {
                db: new_db(),
                hover_mode: HoverMode::Debug,
                uris: HashSet::new(),
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

//            code_action_provider: Some(CodeActionProviderCapability::Simple(true)),

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
        let package = uri_to_packagefqn(&uri);
        let text: BString = params.text_document.text.into();

        {
            let mut state = self.state.lock().await;
            if db_is_empty(&state.db) {
                state.db = db_from_dir(&workspace);
            }

            let new_source = Source::new(package.clone(), text);
            state.db.set_source(package.clone(), new_source);
            let packages = state.db.get_packages();
            if !packages.contains(&package) {
                let mut packages = packages;
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
            let workspace = workspace_root(&uri);
            if db_is_empty(&state.db) {
                state.db = db_from_dir(workspace);
            }
            let packages = state.db.get_packages();
            if !packages.contains(&package) {
                let mut packages = packages;
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
        // TODO find the AstNode to jump to
        if let Some(node) = None::<AstNode> {
            let loc = Location {
                uri: params.text_document_position_params.text_document.uri,
                range: span_to_range(node.span()),
            };

            Ok(Some(GotoDefinitionResponse::Scalar(loc)))
        } else {
            Ok(None)
        }
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


//    async fn code_action(
//        &self,
//        params: CodeActionParams,
//    ) -> Result<Option<Vec<CodeActionOrCommand>>> {
//        let mut actions = Vec::new();
//
//        let uri = params.text_document.uri;
//        let package = uri_to_packagefqn(&uri);
//        let parsing = {
//            let mut state = self.state.lock().await;
//            state.vir.parsing(&package.to_string())
//        };
//        let mut code_actions = vec![];
//
//        for node in parsing.root().children() {
//            if node.is_item() {
//                let range = span_to_range(node.span());
//
//                let mut changes = std::collections::HashMap::new();
//                changes.insert(
//                    uri.clone(),
//                    vec![],
//                );
//
//                let edit = WorkspaceEdit {
//                    changes: Some(changes),
//                    document_changes: None,
//                    change_annotations: None,
//                };
//
//                let command = Command {
//                    title: "Run test".into(),
//                    command: "VirdantRunTest".into(),
//                    arguments: Some(vec![
//                        json!(parsing.string(node.name().unwrap()).to_string()),
//                    ]),
//                };
//
//                let diagnostic = Diagnostic {
//                    range: span_to_range(node.span()),
//                    severity: Some(DiagnosticSeverity::ERROR),
//                    code: None,
//                    code_description: None,
//                    source: Some("virdant".to_string()),
//                    message: "Syntax error".to_string(),
//                    related_information: None,
//                    tags: None,
//                    data: None,
//                };
//
//                let action = CodeAction {
//                    title: "Run Test".into(),
//                    kind: None,
//                    diagnostics: Some(vec![diagnostic]),
//                    edit: None,
//                    command: Some(command),
//                    is_preferred: Some(true),
//                    data: None,
//                    disabled: None,
//                };
//
//                code_actions.push(CodeActionOrCommand::CodeAction(action));
//            }
//        }
//
//        // Just an example: if the user selected a range called "placeholder"
//        for diag in &params.context.diagnostics {
//            if diag.message.contains("placeholder") {
//                // WorkspaceEdit that replaces the placeholder text
//            }
//        }
//
//        Ok(Some(actions))
//    }
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

        for diag in diags {
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

            let typof = if let Some(exprroot) = get_expr_root(&self.client, db, &parsing, node.clone()).await {
                let _typing = db.get_typing(ExprRoot { location: exprroot.location() });
                db.get_typeof(node.location()).ok() // TODO is this .ok() OK?
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
    let exprroots: Vec<AstNodeId> = db.get_exprroots().into_iter().map(|exprroot| exprroot.location().ast_node_id()).collect();

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
    let parsing = db.get_parsing(package.clone());

    if let Some(node_id) = parsing.at(linecol) {
        let node = parsing.ast_node(node_id);
        let typof = db.get_typeof(node.location()).ok();
        println!("Spelling : {}", node.spelling());
        println!("Summary  : {}", node.summary());
        println!("Package  : {}", package);
        println!("Location : {:?}", node.id());
        println!("Typeof   : {:?}", typof);
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

fn parse_hover_location(location: &str) -> (std::path::PathBuf, PackageFqn, LineCol) {
    let parts: Vec<&str> = location.rsplitn(3, ':').collect();
    assert!(parts.len() == 3, "Location must be in the format <file_or_package>:<line>:<col>");
    let col: usize = parts[0].parse().expect("Invalid column number");
    let line: usize = parts[1].parse().expect("Invalid line number");
    let file_or_package = parts[2];

    let (workspace_dir, package) = if Path::new(file_or_package).extension().map_or(false, |ext| ext == "vir") {
        let path = Path::new(file_or_package);
        let workspace = path.parent().unwrap_or(Path::new(".")).to_path_buf();
        let pkg = path
            .file_stem()
            .map(|stem| PackageFqn::new(stem.as_bytes().into()))
            .expect("Invalid file path");
        (workspace, pkg)
    } else {
        let workspace = std::env::current_dir().expect("Cannot determine current directory");
        let pkg = PackageFqn::new(file_or_package.as_bytes().into());
        (workspace, pkg)
    };

    (workspace_dir, package, LineCol::new(line, col))
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
            let (workspace_dir, package, linecol) = parse_hover_location(&location);
            dump_hover(&workspace_dir, package, linecol);
        }
    }
}
