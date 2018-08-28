//#![feature(rust_2018_preview)]
#![allow(dead_code)]

mod cpu65;

//use crate::cpu65::emu::EMU_FUNCS;
use crate::cpu65::Op::*;
use crate::cpu65::CPU;
use crate::cpu65::OPCODES;
use std::fs;
use std::io;

struct G {
    f: usize,
}

fn main() -> io::Result<()> {
    let fname = "a.bin";
    let buf = read_program(fname)?;

    // let mem: [u8; cpu65::MEM_SIZE] = [0; cpu65::MEM_SIZE];
    let mut cpu = cpu65::CPU::new();

    if cpu.load(&buf) != true {
        panic!("Load failed!");
    }

    count_implemented();

    let win = &cpu.get_mem()[0x8000..0x8010];
    for b in win {
        print!("{} ", b);
    }
    println!();

    let mut td = cpu65::TraceData::new();

    use std::{thread, time};

    let delay = time::Duration::from_millis(250);

    cpu.set_pc(0x8000);
    for i in 1..=10000 {
        cpu.trace(&mut td);
        if i != 2 && td.pc == 0x8001 {
            break;
        }
        match td.op {
            Op16(_o) => println!(
                "{:04}:{:>04X} {} {:<10}{:>02X} {:>02X} {:>02X}  |{}| A={:>02X} X={:>02X} Y={:>02X}",
                i, td.pc, td.instruction, td.opstr, td.oc, td.opl, td.oph, td.status, td.a, td.x, td.y
            ),
            _ => println!(
                "{:04}:{:>04X} {} {:<10}{:>02X} {:>02X}     |{}| A={:>02X} X={:>02X} Y={:>02X}",
                i, td.pc, td.instruction, td.opstr, td.oc, td.opl, td.status, td.a, td.x, td.y
            ),
            // _ => println!(
            //     "{:04}:{:>04X} {}           {:>02X}",
            //     i, td.pc, td.instruction, td.oc
            // ),
        };
        //thread::sleep(delay);
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
}

fn read_program(fname: &str) -> Result<Vec<u8>, io::Error> {
    use std::io::{Error, ErrorKind};

    if fs::metadata(&fname)?.is_dir() {
        return Err(Error::new(ErrorKind::Other, "Input file is a directory!"));
    }
    let buf = fs::read(&fname)?;

    Ok(buf)
}
