#![feature(rust_2018_preview)]
#![allow(dead_code)]

mod cpu65;

use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let fname = "mply3.com";
    let buf = read_program(fname)?;

    let mem: [u8; cpu65::MAX_MEM] = [0; cpu65::MAX_MEM];

    let mut cpu = cpu65::CPU::new(mem);
    cpu.emu_cld();

    if cpu.load(buf) != true {
        panic!("Load failed!");
    }

    let win = &cpu.get_mem()[0x8000..0x8010];
    for b in win {
        print!("{} ", b);
    }
    println!();

    cpu.set_pc(0x8000);
    for _ in 0..6 {
        let oc = cpu.get_opcode();
        println!("opcode: {:>04X} -> {}: {:?}", oc.code, oc.mnemonic, oc.mode);
        cpu.step();
    }
    cpu65::emu::EMU_FUNCS[0](&mut cpu);
    cpu65::emu::EMU_FUNCS[1](&mut cpu);

    // let fname = "mem.bin";
    // fs::write(fname, &cpu.get_mem()[..])?;

    Ok(())
}

fn read_program(fname: &str) -> Result<Vec<u8>, io::Error> {
    use std::io::{Error, ErrorKind};

    if fs::metadata(&fname)?.is_dir() {
        return Err(Error::new(ErrorKind::Other, "Input file is a directory!"));
    }
    let buf = fs::read(&fname)?;

    Ok(buf)
}
