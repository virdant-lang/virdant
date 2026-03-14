use std::collections::HashMap;
use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::sync::Arc;

use bstr::BString;
use serde_json::json;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use virdant::Vir;
use virdant::fqn::PackageFqn;
use virdant::source::{LineCol, Source, Span};
use virdant::syntax::ast::AstNode;
use virdant::syntax::parsing::{Parsing, parse};

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

struct ServerState {
    vir: Vir,
    sources: HashMap<Url, Source>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState {
                vir: Vir::new(),
                sources: HashMap::new(),
            })),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::INFO, "Hello from Virdant")
            .await;

        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),

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

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File opened")
            .await;

        let uri = params.text_document.uri;
        let package = uri_to_packagefqn(&uri);
        let text: BString = params.text_document.text.into();
        let source = Source::new(package.clone(), text.clone());

        {
            let mut state = self.state.lock().await;
            state.sources.insert(uri.clone(), source);
            if !state.vir.packages().contains(&package.to_string()) {
                state.vir.add_package(&package.to_string());
            }
            state.vir.set_package_text(&package.to_string(), text);
        }

        self.check_document(&uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File changed")
            .await;

        let uri = params.text_document.uri;
        let package = uri_to_packagefqn(&uri);
        let text: BString = params.content_changes[0].text.clone().into();
        let source = Source::new(package.clone(), text.clone());

        {
            let mut state = self.state.lock().await;
            state.sources.insert(uri.clone(), source);
            if !state.vir.packages().contains(&package.to_string()) {
                state.vir.add_package(&package.to_string());
            }
            state.vir.set_package_text(&package.to_string(), text);
        }

        self.check_document(&uri).await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File saved")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let position: Position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);

        let uri = params.text_document_position_params.text_document.uri;
        let package = uri_to_packagefqn(&uri);

        let parsing = {
            let mut state = self.state.lock().await;
            state.vir.parsing(&package.to_string())
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
        let range = params.range;
        let linecol = LineCol::new(1, 1);

        let package = uri_to_packagefqn(&uri);
        let parsing = {
            let mut state = self.state.lock().await;
            state.vir.parsing(&package.to_string())
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
            let mut state = self.state.lock().await;
            state.vir.parsing(&package.to_string())
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
    async fn source(&self, uri: &Url) -> Option<Source> {
        let state = self.state.lock().await;
        state.sources.get(uri).cloned()
    }

    async fn check_document(&self, uri: &Url) {
        let mut diagnostics = Vec::new();

        let mut state = self.state.lock().await;
        if let Err(diags) = state.vir.check() {
            for diag in diags {
                let diagnostic = Diagnostic {
                    range: span_to_range(diag.region().span()),
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("virdant".to_string()),
                    message: "Syntax error".to_string(),
                    related_information: None,
                    tags: None,
                    data: None,
                };
                diagnostics.push(diagnostic);
            }
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
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

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend::new(client));

    Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
