#![feature(rust_2018_preview)]
#![allow(dead_code)]

mod cpu65;

use crate::cpu65::emu::EMU_FUNCS;
use std::fs;
use std::io;

struct G {
    f: usize,
}

fn main() -> io::Result<()> {
    let fname = "mply3.com";
    let buf = read_program(fname)?;

    let mem: [u8; cpu65::MEM_SIZE] = [0; cpu65::MEM_SIZE];
    let mut cpu = cpu65::CPU::new(mem);

    if cpu.load(buf) != true {
        panic!("Load failed!");
    }

    count_implemented();

    let win = &cpu.get_mem()[0x8000..0x8010];
    for b in win {
        print!("{} ", b);
    }
    println!();

    cpu.set_pc(0x8000);
    for _ in 0..20 {
        cpu.step();
    }

    // let fname = "mem.bin";
    // fs::write(fname, &cpu.get_mem()[..])?;

    Ok(())
}

fn count_implemented() {
    // count implemented
    let n = cpu65::CPU::emu_not_impl as *const fn(&cpu65::CPU);
    let mut c = 0;
    for i in EMU_FUNCS.iter().take(256) {
        //        let f = EMU_FUNCS[i] as usize;
        if *i as *const fn(&cpu65::CPU) != n {
            c += 1;
        }
    }
    println!("{} instructions implememnted!", c);
}

fn read_program(fname: &str) -> Result<Vec<u8>, io::Error> {
    use std::io::{Error, ErrorKind};

    if fs::metadata(&fname)?.is_dir() {
        return Err(Error::new(ErrorKind::Other, "Input file is a directory!"));
    }
    let buf = fs::read(&fname)?;

    Ok(buf)
}
