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
    print!("\x1b[1m\x1b[4m\x1b[31mError Line {}\x1B[0m : ", nl);
    contents.into_iter().for_each(|x| print!("{}", x as char));
    println!("\n\t\t^^^^^^^^^^^^^^^^^^^");
    Ok(())
}

pub fn print_bytes_warn(ff: &str, start: usize, end: usize) -> io::Result<()> {
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
    print!("\x1B[43mWarning Line {}\x1B[0m : ", nl);
    contents.into_iter().for_each(|x| print!("{}", x as char));
    println!("\n\t\t^^^^^^^^^^^^^^^^^^^");
    Ok(())
}

