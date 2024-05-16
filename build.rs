fn main() {
    println!("Calling build.rs");
    let rr = lalrpop::process_root();
    match rr {
        Ok(_) => (),
        Err(_) => panic!(),
    }
}
