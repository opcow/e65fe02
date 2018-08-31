//#![feature(rust_2018_preview)]
#![allow(dead_code)]

mod cpu65;

extern crate clap;
use clap::{App, Arg};
use crate::cpu65::CPU;
use crate::cpu65::OPCODES;
use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let matches = App::new("fe02")
        .version("0.1")
        .about("NMOS 6502 emulator")
        .author("Mitch Crane")
        .arg(
            Arg::with_name("ifile")
                .short("i")
                .long("input")
                .value_name("INFILE")
                .takes_value(true)
                .required(true)
                .help("Sets the binary file to read"),
        ).arg(
            Arg::with_name("disassemble")
                .short("d")
                .long("disassemble")
                .multiple(false)
                .help("Disassemble the binary"),
        ).arg(
            Arg::with_name("steps")
                .short("s")
                .long("steps")
                .takes_value(true)
                .help("Positive interger: number of steps to trace"),
        ).get_matches();

    let steps = matches
        .value_of("steps")
        .unwrap_or("5")
        .parse::<u32>()
        .unwrap();

    let fname = matches.value_of("ifile").unwrap();

    let buf = read_program(fname)?;

    let mut cpu = cpu65::CPU::new();

    if cpu.load(&buf) != true {
        panic!("Load failed!");
    }

    count_implemented();

    println!();

    cpu.trace(0x8000, steps, matches.is_present("disassemble"));

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
