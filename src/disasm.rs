use super::cpu65::{get_format, Instruction, Modes, CPU, INSTRUCTIONS};
use crate::prascii::print_ascii;
use std::collections::HashMap;
use std::sync::Mutex;

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

    while pc <= end {
        ins = &INSTRUCTIONS[mem[pc] as usize];
        // check for a label matching this address
        let label = b.get(&pc).unwrap_or(&defstr);
        // use available label for branch instructions
        if ins.isbr {
            match ins.mode {
                Modes::Rel => target = cpu.get_br_addr(pc as isize),
                _ => target = cpu.get_mem_usize(pc + 1),
            }
            print_ascii(&format!(
                "{}    {} LOC{:04X}\n",
                label, ins.mnemonic, target
            ));
        } else {
            print_ascii(&format!(
                "{}    {} {}\n",
                label,
                ins.mnemonic,
                get_format(ins.mode, pc, &mem[(pc as usize + 1)..=(pc as usize + 2)],)
            ));
        }
        pc += ins.ops as usize + 1;
    }
    print_ascii("\n");
}

pub fn trace(cpu: &mut CPU, start: u16, count: u32) {
    let defstr = String::from("");
    let b = BRAMAP.lock().unwrap();

    use std::{thread, time};
    let delay = time::Duration::from_millis(200);

    cpu.set_pc(start);

    for i in 1..=count {
        let label = b.get(&(cpu.get_pc())).unwrap_or(&defstr);
        println!("{:8} {:04}:{}", label, i, &cpu);
        thread::sleep(delay);
        cpu.step();
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
                        b.insert(target, format!("LOC{:04X}\n", target));
                    }
                }
                _ => {
                    target = cpu.get_mem_usize(pc + 1);
                    if !b.contains_key(&target) {
                        b.insert(target, format!("LOC{:04X}\n", target));
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
