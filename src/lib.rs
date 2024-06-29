pub mod ast;
pub mod error;
mod lexer;
pub mod tokens;

use error::print_bytes;
use lalrpop_util::{lalrpop_mod, ParseError};
use std::process::exit;
lalrpop_mod!(pub og);
use og::ScriptParser;

pub fn parse(s: &str) -> Vec<ast::Stmt> {
    let source_code = std::fs::read_to_string(s).expect("File could not be opened");
    // println!("source code: {} {}", source_code, source_code.len());
    let lexer = lexer::Lexer::new(&source_code);
    let parser = ScriptParser::new();
    // parser.parse(lexer).expect("Could not parse the input")
    let parse_res = parser.parse(lexer);
    match parse_res {
        Ok(x) => x,
        Err(y) => {
            match y {
                ParseError::UnrecognizedEof { location, expected } => {
                    let _ = print_bytes(s, location, location + 1);
                    println!(
                        "Expected: {}",
                        expected
                            .iter()
                            .enumerate()
                            .fold(String::from(""), |acc, (j, x)| {
                                let orr = if j > 0 { " or " } else { " " };
                                acc + &orr.to_string() + x
                            })
                    );
                }
                ParseError::ExtraToken { token } => {
                    let (f, _, r) = token;
                    let _ = print_bytes(s, f, r);
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    let (f, _, r) = token;
                    let _ = print_bytes(s, f, r);
                    println!(
                        "Expected: {}",
                        expected
                            .iter()
                            .enumerate()
                            .fold(String::from(""), |acc, (j, x)| {
                                let orr = if j > 0 { " or " } else { " " };
                                acc + &orr.to_string() + x
                            })
                    );
                }
                ParseError::InvalidToken { location } => {
                    let _ = print_bytes(s, location, location + 1);
                }
                ParseError::User { error } => println!("{:?}", error),
            }
            println!("Error while parsing!");
            exit(1);
        }
    }
}
