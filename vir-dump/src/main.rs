use std::sync::Arc;

use virdant::location::Pos;
use virdant::parser::{Ast, ParseError, Parser};
use virdant::Virdant;
use virdant::tokenizer::{Token, Tokenizer};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args[1] == "--parse" {
        let top_path: String = args[2].clone().into();
        dump_parse(&top_path);
    } else if args[1] == "--tokenize" {
        let top_path: String = args[2].clone().into();
        dump_tokenize(&top_path);
    } else {
        let top_path: String = args[1].clone().into();
        let mut virdant = Virdant::new(&top_path);

        match virdant.check() {
            Err(errors) => {
                eprintln!("{virdant:?}");
                eprintln!("ERRORS:");
                for error in errors.into_iter() {
                    eprintln!("    {error:?}");
                }
                std::process::exit(1);
            },
            Ok(_) => println!("{virdant:?}"),
        }
    }
}

fn dump_parse(filename: &str) {
    let text = std::fs::read_to_string(filename).unwrap();
    let text_bytes = Arc::from(text.as_bytes().to_vec().into_boxed_slice());
    let mut parser = Parser::new(text_bytes);
    let ast = parser.ast();
    let errors = parser.errors();
    if errors.len() > 0 {
        for error in errors {
            let (lineno, col) = token_to_linecol(&text, error.token());
            eprint!("ERROR: line {lineno} col {col}: ");
            match error {
                ParseError::Unexpected(token) => eprintln!("Unexpected token: {:?}", token.kind()),
                ParseError::Expected(expected_item_kind, token) => eprintln!("Expected {:?} but found {:?}", expected_item_kind, token.kind()),
                ParseError::ExpectedItemStart(token) => eprintln!("Expected the start of an item, but found{:?}", token.kind()),
                ParseError::Unknown(token) => eprintln!("UNKNOWN error near {token:?}"),
            }
        }
    } else {
        pretty_print_ast(&ast, &parser, 0);
    }
}

pub fn pretty_print_ast(ast: &Ast, parser: &Parser, indent_level: usize) {
    let indent = "  ".repeat(indent_level);
    match ast {
        Ast::None => println!("{indent}NONE"),
        Ast::Package { imports, items } => {
            println!("{indent}Package");
            for import in imports {
                pretty_print_ast(import.as_ref(), parser, indent_level + 1)
            }

            for item in items {
                pretty_print_ast(item.as_ref(), parser, indent_level + 1);
            }
        },
        Ast::Import(name_token) => {
            println!("{indent}Import");
            let name = token_to_str(name_token, parser);
            println!("{indent}  {name:?}");
        },
        Ast::Item { kind, name, stmts, attrs } => {
            println!("{indent}Item");
            println!("{indent}  kind:  {kind:?}");
            let name_str = token_to_str(name, parser);
            println!("{indent}  name:  {name_str:?}");
            for attr in attrs {
                pretty_print_ast(attr.as_ref(), parser, indent_level + 1);
            };
            for stmt in stmts {
                pretty_print_ast(stmt.as_ref(), parser, indent_level + 1);
            }
        },
        Ast::Type { name: _, args: _ } => println!("{indent}Type"),
        Ast::TypeArgWidth { width: _ } => println!("{indent}TypeArgWidth"),
        Ast::Component { kind, name, typ, on } => {
            println!("{indent}Component");
            println!("{indent}  kind:  {kind:?}");
            let name_str = token_to_str(name, parser);
            println!("{indent}  name:  {name_str:?}");
            println!("{indent}  type:");
            pretty_print_ast(typ.as_ref(), parser, indent_level + 2);
            if let Some(on) = on {
                println!("{indent}  on:");
                pretty_print_ast(on.as_ref(), parser, indent_level + 2);
            }
        },
        Ast::Driver(path, driver_kind, expr) => {
            println!("{indent}Driver");
            let path_str = token_vec_to_path_str(path, parser);
            println!("{indent}  path: {path_str}");
            let driver_kind_str = token_to_str(driver_kind, parser);
            println!("{indent}  kind: {driver_kind_str:?}");
            println!("{indent}  expr:");
            pretty_print_ast(expr.as_ref(), parser, indent_level + 2);
        },
        Ast::FieldDef(_, _) => println!("{indent}FieldDef"),
        Ast::CtorDef(_, _) => println!("{indent}CtorDef"),
        Ast::EnumerantDef(_, _) => println!("{indent}EnumerantDef"),
        Ast::Reference(_) => println!("{indent}Reference"),
        Ast::Lit(value) => {
            println!("{indent}Lit");
            let value_str = token_to_str(value, parser);
            println!("{indent}  {value_str:?}")
        },
        Ast::Word(_) => println!("{indent}Word"),
        Ast::Bit(_) => println!("{indent}Bit"),
        Ast::UnOp(_, _) => println!("{indent}UnOp"),
        Ast::BinOp(_, _, _) => println!("{indent}BinOp"),
        Ast::MethodCall(_, _, _) => println!("{indent}MethodCall"),
        Ast::Struct(name, assigns) => {
            println!("{indent}Struct");
            let name_str = token_to_str(name, parser);
            println!("{indent}  name: {name_str:?}");
            println!("{indent}  assigns:");
            for assign in assigns {
                pretty_print_ast(assign.as_ref(), parser, indent_level + 2);
            }
        },
        Ast::Assign(name, expr) => {
            println!("{indent}Assign");
            let name_str = token_to_str(name, parser);
            println!("{indent}  name: {name_str:?}");
            println!("{indent}  expr");
            pretty_print_ast(expr.as_ref(), parser, indent_level + 2);
        },
        Ast::FnCall(_, _) => println!("{indent}FnCall"),
        Ast::Field(_, _) => println!("{indent}Field"),
        Ast::Ctor(_, _) => println!("{indent}Ctor"),
        Ast::Param(_, _) => println!("{indent}Param"),
        Ast::Enumerant(_) => println!("{indent}Enumerant"),
        Ast::As(_, _) => println!("{indent}As"),
        Ast::Idx(_, _) => println!("{indent}Idx"),
        Ast::IdxRange(_, _, _) => println!("{indent}IdxRange"),
        Ast::Cat(_) => println!("{indent}Cat"),
        Ast::Zext(_) => println!("{indent}Zext"),
        Ast::If { subject: _, true_branch: _, false_branch : _} => println!("{indent}If"),
        Ast::Match { subject: _, arms : _} => println!("{indent}Match"),
        Ast::MatchArm { pat: _, expr: _ } => println!("{indent}MatchArm"),
        Ast::PatBind(_) => println!("{indent}PatBind"),
        Ast::PatAt(_, _) => println!("{indent}PatAt"),
        _ => todo!(),
    };
}

fn token_vec_to_path_str<'a>(tokens: &[Token], parser: &'a Parser) -> String {
    let mut pieces = vec![];
    for token in tokens {
        pieces.push(token_to_str(token, parser).to_string());
    }
    pieces.join(".")
}

fn token_to_str<'a>(token: &Token, parser: &'a Parser) -> std::borrow::Cow<'a, str> {
    let start = usize::from(token.pos());
    let len = usize::from(token.len());
    let end = start + len;

    String::from_utf8_lossy(&parser.text()[start..end])
}

fn token_to_linecol(text: &str, token: Token) -> (usize, usize) {
    let pos = usize::from(token.pos());

    let mut lineno = 1;
    let mut col = 0;

    for (i, ch) in text.as_bytes().iter().copied().enumerate() {
        if pos == i {
            break;
        }
        if ch == b'\n' {
            col = 1;
            lineno += 1;
        } else {
            col += 1;
        }
    }

    (lineno, col)
}

fn dump_tokenize(filename: &str) {
    let text = std::fs::read_to_string(filename).unwrap();
    let text_bytes = Arc::from(text.as_bytes().to_vec().into_boxed_slice());
    let tokenizer = Tokenizer::new(text_bytes);
    pretty_token_debug(&tokenizer);
}

pub fn pos_to_lineno(text: &str, pos: Pos) -> usize {
    let mut lineno = 1;

    for (i, ch) in text.as_bytes().iter().copied().enumerate() {
        if usize::from(pos) == i {
            break;
        }
        if ch == b'\n' {
            lineno += 1;
        }
    }

    lineno
}

pub fn pretty_token_debug(tokenizer: &Tokenizer) {
    let toks = tokenizer.tokens();

    let mut tok_iter = toks.into_iter();
    let mut current_tok = tok_iter.next();

    let text = String::from_utf8_lossy(tokenizer.text());

    for (i, line) in text.lines().enumerate() {
        let lineno = i + 1;
        println!("{lineno}: {line}");
        loop {
            if let Some(tok) = current_tok.clone() {
                if pos_to_lineno(&text, tok.pos()) == lineno {
                    eprint!("{:?}({}) ", tok.kind(), usize::from(tok.pos()));
                    current_tok = tok_iter.next();
                } else {
                    eprintln!();
                    eprintln!();
                    break;
                }
            } else {
                eprintln!();
                eprintln!();
                break;
            }
        }
    }
}
