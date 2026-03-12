use std::os::unix::ffi::OsStrExt;
use std::path::Path;
use std::sync::Arc;
use bstr::BString;
use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use virdant::fqn::PackageFqn;

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
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        self.client
            .log_message(MessageType::ERROR, "Hello from Virdant")
            .await;

        let capabilities = ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
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

        self.check_document(&params.text_document.uri, &params.text_document.text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File changed")
            .await;

        self.check_document(&params.text_document.uri, &params.content_changes[0].text).await;
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "File saved")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let position: Position = params.text_document_position_params.position;
        let linecol = LineCol::new((position.line + 1) as usize, (position.character + 1) as usize);
        let parsing = self.parsing(&params.text_document_position_params.text_document.uri);

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
        let loc = Location {
            uri: params.text_document_position_params.text_document.uri,
            range: Range {
                start: Position {
                    line: 10,
                    character: 0,
                },
                end: Position {
                    line: 11,
                    character: 1,
                },
            },
        };

        Ok(Some(GotoDefinitionResponse::Scalar(loc)))
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
}

use tower_lsp::lsp_types::*;
use virdant::source::{LineCol, Source};
use virdant::syntax::parsing::{Parsing, parse};

impl Backend {
    fn source(&self, uri: &Url) -> Source {
        let package = Path::new(uri.path())
            .file_stem()
            .map(|stem| PackageFqn::new(stem.as_bytes().into()))
            .unwrap_or_else(|| PackageFqn::new(uri.path().as_bytes().into()));
        let uri_path = uri
            .to_file_path()
            .unwrap_or_else(|_| Path::new(uri.path()).to_path_buf());
        let path = uri_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(format!("{package}.vir"));
        let text: BString = std::fs::read(&path)
            .unwrap_or_else(|err| panic!("failed to read {}: {err}", path.display()))
            .into();

        Source::new(package, text)
    }

    fn parsing(&self, uri: &Url) -> Parsing {
        let source = self.source(uri);
        parse(&source)
    }

    async fn check_document(&self, uri: &Url, text: &str) {
        let mut diagnostics = Vec::new();

        let parsing: Parsing = self.parsing(&uri);
        for error_node in parsing.errors() {
            let region = error_node.region();
            let diagnostic = Diagnostic {
                range: Range {
                    start: Position {
                        line: region.start().line().saturating_sub(1) as u32,
                        character: region.start().col().saturating_sub(1) as u32,
                    },
                    end: Position {
                        line: region.end().line().saturating_sub(1) as u32,
                        character: region.end().col().saturating_sub(1) as u32,
                    },
                },
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

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
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
