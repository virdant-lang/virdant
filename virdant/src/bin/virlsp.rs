use std::sync::Arc;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

struct ServerState {
    open_files: Vec<Url>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState {
                open_files: Vec::new(),
            })),
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(
                TextDocumentSyncKind::FULL,
            )),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string()]),
                ..Default::default()
            }),
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
        let mut state = self.state.lock().await;
        state.open_files.push(params.text_document.uri.clone());

        self.client
            .log_message(MessageType::INFO, "File opened")
            .await;
        self.check_document(params.text_document.uri, "TODO".to_string()).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File changed")
            .await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File saved")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let contents = HoverContents::Scalar(MarkedString::String(
            "Example hover information".to_string(),
        ));

        Ok(Some(Hover {
            contents,
            range: None,
        }))
    }

    async fn completion(
        &self,
        _: CompletionParams,
    ) -> Result<Option<CompletionResponse>> {
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
}

use tower_lsp::lsp_types::*;

impl Backend {
    async fn check_document(&self, uri: Url, text: String) {
        let mut diagnostics = Vec::new();

        if let Some(pos) = text.find("TODO") {
            let diagnostic = Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: pos as u32,
                    },
                    end: Position {
                        line: 0,
                        character: (pos + 4) as u32,
                    },
                },
                severity: Some(DiagnosticSeverity::WARNING),
                code: None,
                code_description: None,
                source: Some("mylang".to_string()),
                message: "Found a TODO".to_string(),
                related_information: None,
                tags: None,
                data: None,
            };

            diagnostics.push(diagnostic);
        } else {
            panic!();
        }

        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
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
