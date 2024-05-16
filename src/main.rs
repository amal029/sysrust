use sysrust::parse;
use std::env;
use std::io;

fn main() -> io::Result<()>{
    let args: Vec<String> = env::args().collect();
    // let mut f = File::open("first.txt")?;
    // let mut source_code = String::new();
    // f.read_to_string(&mut source_code)?;
    // let source_code = std::fs::read_to_string(&args[1]).unwrap();
    // println!("source code: {} {}", source_code, source_code.len());
    let _ = parse(&args[1]);
    Ok(())
}
