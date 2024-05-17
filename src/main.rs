use std::env;
use sysrust::parse;
mod rewrite;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);
    // XXX: Now rewrite the immediate abort and suspend statement
    let mut _state = rewrite::State::new();
    let _ast = rewrite::rewrite_stmts(_ast, &mut _state);
    println!("{:?}", _ast);
}
