pub mod ast;
mod lexer;
pub mod tokens;
// use std::error::Error;

use lalrpop_util::lalrpop_mod;
lalrpop_mod!(pub og);
use og::ScriptParser;

pub fn parse(s: &str) -> Vec<ast::Stmt> {
    let source_code = std::fs::read_to_string(s).unwrap();
    // println!("source code: {} {}", source_code, source_code.len());
    let lexer = lexer::Lexer::new(&source_code);
    let parser = ScriptParser::new();
    parser.parse(lexer).unwrap()
}
