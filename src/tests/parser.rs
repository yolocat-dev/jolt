use crate::{error::Result, lexer::Lexer, parser::{node::Node, Parser}};

fn parse(source: &str) -> Result<Node> {
    let filename = "test.jt";

    let lexer = Lexer::new(filename, source);
    let tokens = lexer.lex()?;

    let parser = Parser::new(tokens.into_iter(), filename, source);

    parser.parse()
}

fn assert_use_stmt(source: &str, expect: Vec<&'static str>) {
    let ast = parse(source);

    eprintln!("testing: `{}`", source);
    
    if ast.is_err() {
        eprintln!("{:#?}", ast.as_ref().unwrap_err());
    }

    assert!(ast.is_ok());
    let ast = ast.unwrap();

    assert!(matches!(ast, Node::File(_)));

    let nodes = match ast {
        Node::File(nodes) => nodes,
        _ => unreachable!(),
    };

    assert_eq!(nodes.len(), expect.len());

    let paths = nodes.into_iter().map(|n| match n {
        Node::Use(path) => path,
        _ => panic!("only use statements should be used in assert_use_stmt"),
    }).collect::<Vec<_>>();

    for (path, expect) in paths.into_iter().zip(expect) {
        assert_eq!(path, expect);
    }
}

#[test]
fn basic_use() {
    assert_use_stmt("use a::b;", vec!["a::b"]);
    assert_use_stmt("use a::b::c;", vec!["a::b::c"]);
}

#[test]
fn multi_use() {
    assert_use_stmt("use a::b::{c, d};", vec!["a::b::c", "a::b::d"]);
}

#[test]
fn nested_use() {
    assert_use_stmt("use a::b::{c::{d, e}, f};", vec!["a::b::c::d", "a::b::c::e", "a::b::f"]);
}

#[test]
fn use_errors() {
    assert!(parse("use a::::b;").is_err());
    assert!(parse("use a::,::b;").is_err());
    assert!(parse("use a::b,;").is_err());
    assert!(parse("use a::{};").is_err());
    assert!(parse("use a::{{}};").is_err());
    assert!(parse("use a::{b::};").is_err());
    assert!(parse("use a::{b::{}};").is_err());
}

#[test]
fn basic_use_self() {
    assert_use_stmt("use a::b::self;", vec!["a::b"]);
}

#[test]
fn nested_use_self() {
    assert_use_stmt("use a::b::{c::{d, e}, f::{self, g}};", vec!["a::b::c::d", "a::b::c::e", "a::b::f", "a::b::f::g"]);
}
