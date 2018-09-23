use super::cpu65::{Instruction, Mode, CPU, INSTRUCTIONS};
use crate::prascii::print_ascii;
use std::collections::HashMap;

pub fn disasm(cpu: &CPU, start: usize, end: usize, label_map: Option<&HashMap<usize, String>>) {
    let mut ins: &Instruction;
    let mut pc = start;
    let defstr = String::from("");
    let mem = cpu.mem();

    while pc <= end {
        ins = &INSTRUCTIONS[mem[pc] as usize];
        // check for a label matching this address
        let label = match label_map {
            Some(m) => m.get(&pc).unwrap_or(&defstr),
            None => &defstr,
        };
        // use available label for branch instructions
        if ins.isbr {
            let target = match ins.mode {
                Mode::Rel => cpu.branch_addr(pc as isize),
                _ => cpu.mem_ptr(pc + 1),
            };
            print_ascii(&format!(
                "{}    {} LOC{:04X}\n",
                label, ins.mnemonic, target
            ));
        } else {
            print_ascii(&format!(
                "{}    {} {}\n",
                label,
                ins.mnemonic,
                CPU::get_format(ins.mode, pc, &mem[(pc as usize + 1)..=(pc as usize + 2)],)
            ));
        }
        pc += ins.ops as usize + 1;
    }
    print_ascii("\n");
}

pub fn trace(cpu: &mut CPU, start: u16, count: u32, label_map: Option<&HashMap<usize, String>>) {
    let defstr = String::from("");

    use std::{thread, time};
    let delay = time::Duration::from_millis(200);

    cpu.set_pc(start);

    for i in 1..=count {
        let label = match label_map {
            Some(m) => m.get(&cpu.get_pc()).unwrap_or(&defstr),
            None => &defstr,
        };
        println!("{:8} {:04}:{}", label, i, &cpu);
        thread::sleep(delay);
        cpu.step();
    }
}

pub fn first_pass(cpu: &CPU, mut pc: usize, end: usize, label_map: &mut HashMap<usize, String>) {
    let mut ins: &Instruction;
    let mem = cpu.mem();

    while pc <= end {
        ins = &INSTRUCTIONS[mem[pc] as usize];
        // if the instuction is a jump or a branch save the address
        // as a label name for subsequent passes
        if ins.isbr {
            let target = match ins.mode {
                Mode::Rel => cpu.branch_addr(pc as isize),
                _ => cpu.mem_ptr(pc + 1),
            };
            label_map
                .entry(target)
                .or_insert_with(|| format!("LOC{:04X}\n", target));
        }
        pc += ins.ops as usize + 1;
    }
}
