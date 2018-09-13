// #![feature(rust_2018_preview)]
#![allow(dead_code)]
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate clap;

//#![feature(rust_2018_preview)]

mod cpu65;
mod disasm;
mod prascii;

use clap::{App, Arg};
use crate::cpu65::CPU;
use crate::cpu65::INSTRUCTIONS;
use crate::prascii::print_ascii;

use std::fs;
use std::io;

fn main() -> io::Result<()> {
    let matches = App::new("fe02")
        .version(crate_version!())
        .about("NMOS 6502 emulator")
        .author(crate_authors!())
        .arg(
            Arg::with_name("ifile")
                // .short("i")
                // .long("input")
                .value_name("INFILE")
                .takes_value(true)
                .required(true)
                .index(1)
                .help("Sets the binary file to read"),
        ).arg(
            Arg::with_name("disassemble")
                .short("d")
                .multiple(false)
                // .conflicts_with("trace")
                // .required(true)
                .help("Disassemble the binary"),
        ).arg(
            Arg::with_name("trace")
                .short("t")
                .multiple(false)
                // .required(true)
                .help("Trace execution of the binary"),
        ).arg(
            Arg::with_name("steps")
                .short("s")
                .long("steps")
                .takes_value(true)
                .requires("trace")
                .help("Positive interger: number of steps to trace"),
        ).get_matches();

    let steps = matches
        .value_of("steps")
        .unwrap_or("5")
        .parse::<u32>()
        .unwrap_or(5);

    let fname = matches.value_of("ifile").unwrap();

    let buf = read_program(fname)?;

    let mut cpu = cpu65::CPU::new();

    let segs = cpu.load(&buf)?;

    // count_implemented();
    // println!();
    println!("{:?}", b"foo");

    if matches.is_present("disassemble") {
        print_ascii(&";;; begin disassembley ;;;\n");
        print_ascii(&"    PROCESSOR 6502");
        print_ascii(&"    LIST ON\n\n");
        print_ascii(&"START\n");

        for seg in &segs {
            // assuming all segments are code
            disasm::first_pass(&cpu, seg.start, seg.end);
            disasm::disasm(&cpu, seg.start, seg.end);
        }
    }
    if matches.is_present("trace") {
        let start = segs[0].start as u16;
        disasm::trace(&mut cpu, start, steps);
    }

    // let fname = "mem.bin";
    // fs::write(fname, &cpu.get_mem()[..])?;

    Ok(())
}

fn count_implemented() {
    // count implemented
    let n = CPU::emu_err as *const fn(&mut CPU);
    let k = INSTRUCTIONS
        .iter()
        .filter(|f| f.ef as *const fn(&mut CPU) != n)
        .count();
    println!("{} instructions implememnted!", k);
}

fn read_program(fname: &str) -> Result<Vec<u8>, io::Error> {
    use std::io::{Error, ErrorKind};

    if fs::metadata(&fname)?.is_dir() {
        return Err(Error::new(ErrorKind::Other, "Input file is a directory!"));
    }
    let buf = fs::read(&fname)?;

    Ok(buf)
}
