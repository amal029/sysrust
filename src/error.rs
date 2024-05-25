use std::{
    fs::File,
    io::{self, Read, Seek, SeekFrom},
};

pub fn print_bytes(ff: &str, start: usize, end: usize) -> io::Result<()> {
    let mut f = File::open(ff)?;
    f.seek(SeekFrom::Start(start as u64))?;
    let mut contents = vec![0; end-start];
    f.read_exact(&mut contents)?;
    contents.into_iter().for_each(|x| print!("{}", x as char));
    println!("\n^^^^^^^^^^^^^^^^^^^");
    Ok(())
}
