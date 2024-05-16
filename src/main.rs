use std::env;
use sysrust::parse;

fn main() {
    let args: Vec<String> = env::args().collect();
    let _ast = parse(&args[1]);
    println!("{:?}", _ast);
}
