pub mod ast;
mod lexer;
pub mod tokens;

use lalrpop_util::{lalrpop_mod, ParseError};
lalrpop_mod!(pub og);
use og::ScriptParser;

use std::{
    fs::File,
    io::{self, Read, Seek, SeekFrom},
};
pub fn print_bytes(ff: &str, start: usize, end: usize) -> io::Result<()> {
    let mut f = File::open(ff)?;
    // XXX: Read from start into contents2
    let mut _nc: Vec<u8> = vec![0; start];
    f.read_exact(&mut _nc)?;
    let mut nl = 1usize;
    _nc.into_iter().for_each(|x| {
        if x == 10 {
            nl += 1;
        };
    });

    // XXX: The error line
    f.seek(SeekFrom::Start(start as u64))?;
    let mut contents = vec![0; end - start];
    f.read_exact(&mut contents)?;
    print!("\x1B[41mError Line {}\x1B[0m : ", nl);
    contents.into_iter().for_each(|x| print!("{}", x as char));
    println!("\n\t\t^^^^^^^^^^^^^^^^^^^");
    Ok(())
}

pub fn parse(s: &str) -> Vec<ast::Stmt> {
    let source_code = std::fs::read_to_string(s).unwrap();
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
            panic!("Error while parsing!")
        }
    }
}
