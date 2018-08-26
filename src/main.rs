//#![feature(rust_2018_preview)]
#![allow(dead_code)]

mod cpu65;

//use crate::cpu65::emu::EMU_FUNCS;
use crate::cpu65::CPU;
use crate::cpu65::OPCODES;
use std::fs;
use std::io;

struct G {
    f: usize,
}

fn main() -> io::Result<()> {
    let fname = "mply3.com";
    let buf = read_program(fname)?;

    // let mem: [u8; cpu65::MEM_SIZE] = [0; cpu65::MEM_SIZE];
    let mut cpu = cpu65::CPU::new();

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
    for _ in 0..500 {
        cpu.step();
    }

    // let fname = "mem.bin";
    // fs::write(fname, &cpu.get_mem()[..])?;

    Ok(())
}

fn count_implemented() {
    // count implemented
    let n = CPU::emu_err as *const fn(&mut CPU);
    let mut c = 0;
    for i in OPCODES.iter().take(256) {
        //        let f = EMU_FUNCS[i] as usize;
        if i.ef as *const fn(&mut CPU) != n {
            c += 1;
        }
    }
    println!("{} instructions implememnted!", c);

    // for i in cpu65::OPCODES.iter().take(256) {
    //     // match i.mode {
    //     //     cpu65::Modes::Acc => acc &= i.code,
    //     //     cpu65::Modes::Abs => abs &= i.code,
    //     //     cpu65::Modes::Zpg => zpg &= i.code,
    //     //     cpu65::Modes::Zpx => zpx &= i.code,
    //     //     cpu65::Modes::Zpy => zpy &= i.code,
    //     //     _ => {}
    //     // }
    //     println!(
    //         "Opcode {{ code: 0x{:02x}, length: {}, mode: {:?}, ef: CPU::emu_not_impl, af: CPU::get_imm, mnemonic: \"{}\", }},",
    //         i.code, i.length, i.mode, i.mnemonic
    //     );
    // }
}

fn read_program(fname: &str) -> Result<Vec<u8>, io::Error> {
    use std::io::{Error, ErrorKind};

    if fs::metadata(&fname)?.is_dir() {
        return Err(Error::new(ErrorKind::Other, "Input file is a directory!"));
    }
    let buf = fs::read(&fname)?;

    Ok(buf)
}
