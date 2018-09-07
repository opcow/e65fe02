use super::cpu65::{Instruction, Modes, CPU, INSTRUCTIONS};
use crate::cpu65::get_format;
use std::collections::HashMap;
use std::sync::Mutex;

macro_rules! reg {
    ($m:ident, a) => {
        $m[0x10000]
    };
    ($m:ident, x) => {
        $m[0x10001]
    };
    ($m:ident, y) => {
        $m[0x10002]
    };
}

lazy_static! {
    static ref BRAMAP: Mutex<HashMap<usize, String>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

pub fn disasm(cpu: &CPU, start: usize, end: usize) {
    let mut ins: &Instruction;
    let mut pc = start;
    let mut target: usize; // target of instruction
    let defstr = String::from("");
    let mem = cpu.get_mem();
    let b = BRAMAP.lock().unwrap();

    println!("        .ORG ${:04X}", start);
    while pc <= end {
        ins = &INSTRUCTIONS[mem[pc] as usize];
        // check for a label matching this address
        let label = b.get(&pc).unwrap_or(&defstr);
        if ins.isbr {
            match ins.mode {
                Modes::Rel => target = cpu.get_br_addr(pc as isize),
                _ => target = cpu.get_mem_usize(pc + 1),
            }
            println!("{:7}   {} LOC{:04X}", label, ins.mnemonic, target);
        } else {
            println!(
                "{:7}   {} {}",
                label,
                ins.mnemonic,
                get_format(ins.mode, pc, &mem[(pc as usize + 1)..=(pc as usize + 2)],)
            );
        }
        pc += ins.ops as usize + 1;
    }
    print!("\n");
}

pub fn trace(cpu: &mut CPU, start: u16, count: u32) {
    let defstr = String::from("");
    let mut ins: Instruction;
    let b = BRAMAP.lock().unwrap();

    use std::{thread, time};
    let delay = time::Duration::from_millis(200);

    cpu.set_pc(start);

    for i in 1..=count {
        ins = cpu.get_opcode();
        cpu.step();
        let label = b.get(&(cpu.pc as usize)).unwrap_or(&defstr);
        let mem: &[u8] = cpu.get_mem();
        let opstr = get_format(
            ins.mode,
            cpu.pc as usize,
            &mem[(cpu.pc as usize + 1)..=(cpu.pc as usize + 2)],
        );
        match ins.ops {
            2 => println!(
                "{:8} {:04}:{:>04X} {} {:<10}{:>02X} {:>02X} {:>02X}  |{}| A={:>02X} X={:>02X} Y={:>02X}",
                label, i, cpu.get_pc(), ins.mnemonic, opstr, ins.opcode, mem[cpu.pc as usize + 1],
                mem[cpu.pc as usize + 2], status_as_string(cpu.get_status()),
                reg!(mem, a), reg!(mem, x), reg!(mem, y)
            ),
            _ => println!(
                "{:8} {:04}:{:>04X} {} {:<10}{:>02X} {:>02X}     |{}| A={:>02X} X={:>02X} Y={:>02X}",
                label, i, cpu.get_pc(), ins.mnemonic, opstr, ins.opcode, mem[cpu.pc as usize + 1],
                status_as_string(cpu.get_status()),
                reg!(mem, a), reg!(mem, x), reg!(mem, y)
            ),
        };
        thread::sleep(delay);
    }
}

pub fn first_pass(cpu: &CPU, mut pc: usize, end: usize) {
    let mut ins: &Instruction;
    let mut target: usize;
    let mut b = BRAMAP.lock().unwrap(); // for storing branch/jump
    let mem = cpu.get_mem();

    while pc <= end {
        ins = &INSTRUCTIONS[mem[pc] as usize];
        // if the instuction is a jump or a branch save the address
        // as a label name for subsequent passes
        if ins.isbr {
            match ins.mode {
                Modes::Rel => {
                    target = cpu.get_br_addr(pc as isize);
                    if !b.contains_key(&target) {
                        b.insert(target, format!("LOC{:04X}", target));
                    }
                }
                _ => {
                    target = cpu.get_mem_usize(pc + 1);
                    if !b.contains_key(&target) {
                        b.insert(target, format!("LOC{:04X}", target));
                    }
                }
            }
        }
        pc += ins.ops as usize + 1;
    }
}

fn status_as_string(status: u8) -> String {
    let s1 = ['-', '-', '-', '-', '-', '-', '-', '-'];
    let s2 = ['N', 'V', 'U', 'B', 'D', 'I', 'Z', 'C'];
    let mut ss = String::from("");

    let mut bit = 1 << 7;
    for i in 0..8 {
        if status & bit != 0 {
            ss.push(s2[i]);
        } else {
            ss.push(s1[i]);
        }
        bit >>= 1;
    }
    ss
}
