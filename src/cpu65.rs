use crate::cpu65::{Modes::*, Regs::*};

pub const MAX_MEM: usize = 0x10000;

pub enum Modes {
    Inx, // indirect x
    Zpg, // zero page
    Imm, // immediate mode
    Abs, // absolute
    Iny, // indirect y
    Zpx, // zero page x
    Aby, // absolute y
    Abx, // absolute x
    Ind, // indirect
    // no 0x09 mode
    Acc, // accumulator
    Rel, // relative
    Imp, // implied
    Zpy, // zero page y
    Unk,
}

enum Target {
    Mem([u8; MAX_MEM]),
    Reg(u8),
}

pub enum Regs {
    Norg = 0,
    Preg = 1,
    Areg = 2,
    Xreg = 3,
    Yreg = 4,
    Memm = 5,
    Addr = 6,
}

// CPU virtual processor + memory
pub struct CPU {
    a:      u8,
    x:      u8,
    y:      u8,
    status: u8,
    sp:     u8,
    pc:     usize,
    mem:    [u8; MAX_MEM],
}

use std::fmt;
impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", "I'm a 6502!")
    }
}

impl<'a> CPU {
    pub fn new(m: [u8; 65536]) -> CPU {
        CPU {
            a:      0,
            x:      0,
            y:      0,
            status: 0,
            sp:     0,
            pc:     0,
            mem:    m,
        }
    }

    pub fn set_pc(&mut self, pc: usize) {
        self.pc = pc;
    }

    pub fn get_mem_mut(&mut self) -> &mut [u8] {
        &mut self.mem
    }
    pub fn get_mem(&self) -> &[u8] {
        &self.mem
    }

    pub fn load(&mut self, buf: Vec<u8>) -> bool {
        let mut start_add: usize;
        let mut end_add: usize;

        let mut offset = 2; // skip the header
        while offset < buf.len() {
            start_add = (buf[offset] as usize) | (buf[offset + 1] as usize) << 8;
            end_add = (buf[offset + 2] as usize) | (buf[offset + 3] as usize) << 8;

            if start_add > 0xfffc || end_add > 0xffff || end_add < start_add + 2 {
                return false;
            }

            let seg_beg = offset + 4;
            let seg_end = seg_beg + (end_add - start_add) + 1;
            self.mem[start_add..end_add + 1].clone_from_slice(&buf[seg_beg..seg_end]);
            offset = seg_end;
        }
        true
    }

    fn get_op_16(&self) -> usize {
        (self.mem[self.pc + 1] as usize) | (self.mem[self.pc + 1] as usize) << 8
    }

    fn get_mem_16(&self, a: usize) -> usize {
        (self.mem[a] as usize) & (self.mem[a + 1] as usize) << 8
    }

    pub fn get_eff_addr(&self) -> &u8 {
        match &OPCODES[self.mem[self.pc] as usize].mode {
            Imm => &self.mem[self.pc + 1],
            Zpg => &self.mem[self.mem[self.pc + 1] as usize],
            Abs => &self.mem[self.get_op_16()],
            Inx => &self.mem[self.get_mem_16((self.mem[self.pc + 1] + self.x) as usize)],
            Iny => &self.mem[self.get_op_16() + self.y as usize], //fix me
            Zpx => &self.mem[self.pc + 1 + self.x as usize],
            Aby => &self.mem[self.get_op_16() + self.y as usize], //fix me
            Abx => &self.mem[self.get_op_16() + self.x as usize],
            Acc => &self.a,
            _ => panic!("Instruction mode doesn't target an address!"),
        }
    }

    pub fn step(&mut self) {
        self.pc += &OPCODES[self.mem[self.pc] as usize].length
    }

    pub fn get_opcode(&self) -> &Opcode {
        &OPCODES[self.mem[self.pc] as usize]
    }
}

pub struct Opcode {
    pub code:     i32, // the opcode
    pub length:   usize,
    pub mode:     Modes,
    pub target:   Regs, // the thing that gets the result
    pub mnemonic: &'static str,
}

// use cpu65::Modes::*;
#[cfg_attr(rustfmt, rustfmt_skip)]
const OPCODES: [Opcode; 256] = [
Opcode { code: 0x00, length: 1, mode: Imp, target: Norg, mnemonic: "BRK", },
Opcode { code: 0x01, length: 2, mode: Inx, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x02, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x03, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x04, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x05, length: 2, mode: Zpg, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x06, length: 2, mode: Zpg, target: Memm, mnemonic: "ASL", },
Opcode { code: 0x07, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x08, length: 1, mode: Imp, target: Norg, mnemonic: "PHP", },
Opcode { code: 0x09, length: 2, mode: Imm, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x0a, length: 1, mode: Acc, target: Areg, mnemonic: "ASL", },
Opcode { code: 0x0b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x0c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x0d, length: 3, mode: Abs, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x0e, length: 3, mode: Abs, target: Memm, mnemonic: "ASL", },
Opcode { code: 0x0f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x10, length: 2, mode: Rel, target: Addr, mnemonic: "BPL", },
Opcode { code: 0x11, length: 2, mode: Iny, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x12, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x13, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x14, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x15, length: 2, mode: Zpx, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x16, length: 2, mode: Zpx, target: Memm, mnemonic: "ASL", },
Opcode { code: 0x17, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x18, length: 1, mode: Imp, target: Norg, mnemonic: "CLC", },
Opcode { code: 0x19, length: 3, mode: Aby, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x1a, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x1b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x1c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x1d, length: 3, mode: Abx, target: Areg, mnemonic: "ORA", },
Opcode { code: 0x1e, length: 3, mode: Abx, target: Memm, mnemonic: "ASL", },
Opcode { code: 0x1f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x20, length: 3, mode: Abs, target: Addr, mnemonic: "JSR", },
Opcode { code: 0x21, length: 2, mode: Inx, target: Areg, mnemonic: "AND", },
Opcode { code: 0x22, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x23, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x24, length: 2, mode: Zpg, target: Memm, mnemonic: "BIT", },
Opcode { code: 0x25, length: 2, mode: Zpg, target: Areg, mnemonic: "AND", },
Opcode { code: 0x26, length: 2, mode: Zpg, target: Memm, mnemonic: "ROL", },
Opcode { code: 0x27, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x28, length: 1, mode: Imp, target: Norg, mnemonic: "PLP", },
Opcode { code: 0x29, length: 2, mode: Imm, target: Areg, mnemonic: "AND", },
Opcode { code: 0x2a, length: 1, mode: Acc, target: Areg, mnemonic: "ROL", },
Opcode { code: 0x2b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x2c, length: 3, mode: Abs, target: Memm, mnemonic: "BIT", },
Opcode { code: 0x2d, length: 3, mode: Abs, target: Areg, mnemonic: "AND", },
Opcode { code: 0x2e, length: 3, mode: Abs, target: Memm, mnemonic: "ROL", },
Opcode { code: 0x2f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x30, length: 2, mode: Rel, target: Addr, mnemonic: "BMI", },
Opcode { code: 0x31, length: 2, mode: Iny, target: Areg, mnemonic: "AND", },
Opcode { code: 0x32, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x33, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x34, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x35, length: 2, mode: Zpx, target: Areg, mnemonic: "AND", },
Opcode { code: 0x36, length: 2, mode: Zpx, target: Memm, mnemonic: "ROL", },
Opcode { code: 0x37, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x38, length: 1, mode: Imp, target: Norg, mnemonic: "SEC", },
Opcode { code: 0x39, length: 3, mode: Aby, target: Areg, mnemonic: "AND", },
Opcode { code: 0x3a, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x3b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x3c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x3d, length: 3, mode: Abx, target: Areg, mnemonic: "AND", },
Opcode { code: 0x3e, length: 3, mode: Abx, target: Memm, mnemonic: "ROL", },
Opcode { code: 0x3f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x40, length: 1, mode: Imp, target: Norg, mnemonic: "RTI", },
Opcode { code: 0x41, length: 2, mode: Inx, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x42, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x43, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x44, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x45, length: 2, mode: Zpg, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x46, length: 2, mode: Zpg, target: Memm, mnemonic: "LSR", },
Opcode { code: 0x47, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x48, length: 1, mode: Imp, target: Norg, mnemonic: "PHA", },
Opcode { code: 0x49, length: 2, mode: Imm, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x4a, length: 1, mode: Acc, target: Areg, mnemonic: "LSR", },
Opcode { code: 0x4b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x4c, length: 3, mode: Abs, target: Addr, mnemonic: "JMP", },
Opcode { code: 0x4d, length: 3, mode: Abs, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x4e, length: 3, mode: Abs, target: Memm, mnemonic: "LSR", },
Opcode { code: 0x4f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x50, length: 2, mode: Rel, target: Addr, mnemonic: "BVC", },
Opcode { code: 0x51, length: 2, mode: Iny, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x52, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x53, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x54, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x55, length: 2, mode: Zpx, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x56, length: 2, mode: Zpx, target: Memm, mnemonic: "LSR", },
Opcode { code: 0x57, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x58, length: 1, mode: Imp, target: Norg, mnemonic: "CLI", },
Opcode { code: 0x59, length: 3, mode: Aby, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x5a, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x5b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x5c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x5d, length: 3, mode: Abx, target: Areg, mnemonic: "EOR", },
Opcode { code: 0x5e, length: 3, mode: Abx, target: Memm, mnemonic: "LSR", },
Opcode { code: 0x5f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x60, length: 1, mode: Imp, target: Norg, mnemonic: "RTS", },
Opcode { code: 0x61, length: 2, mode: Inx, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x62, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x63, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x64, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x65, length: 2, mode: Zpg, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x66, length: 2, mode: Zpg, target: Memm, mnemonic: "ROR", },
Opcode { code: 0x67, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x68, length: 1, mode: Imp, target: Norg, mnemonic: "PLA", },
Opcode { code: 0x69, length: 2, mode: Imm, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x6a, length: 1, mode: Acc, target: Areg, mnemonic: "ROR", },
Opcode { code: 0x6b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x6c, length: 3, mode: Ind, target: Addr, mnemonic: "JMP", },
Opcode { code: 0x6d, length: 3, mode: Abs, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x6e, length: 3, mode: Abs, target: Memm, mnemonic: "ROR", },
Opcode { code: 0x6f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x70, length: 2, mode: Rel, target: Addr, mnemonic: "BVS", },
Opcode { code: 0x71, length: 2, mode: Iny, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x72, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x73, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x74, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x75, length: 2, mode: Zpx, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x76, length: 2, mode: Zpx, target: Memm, mnemonic: "ROR", },
Opcode { code: 0x77, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x78, length: 1, mode: Imp, target: Norg, mnemonic: "SEI", },
Opcode { code: 0x79, length: 3, mode: Aby, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x7a, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x7b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x7c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x7d, length: 3, mode: Abx, target: Areg, mnemonic: "ADC", },
Opcode { code: 0x7e, length: 3, mode: Abx, target: Memm, mnemonic: "ROR", },
Opcode { code: 0x7f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x80, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x81, length: 2, mode: Inx, target: Memm, mnemonic: "STA", },
Opcode { code: 0x82, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x83, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x84, length: 2, mode: Zpg, target: Memm, mnemonic: "STY", },
Opcode { code: 0x85, length: 2, mode: Zpg, target: Memm, mnemonic: "STA", },
Opcode { code: 0x86, length: 2, mode: Zpg, target: Memm, mnemonic: "STX", },
Opcode { code: 0x87, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x88, length: 1, mode: Imp, target: Yreg, mnemonic: "DEY", },
Opcode { code: 0x89, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x8a, length: 1, mode: Imp, target: Areg, mnemonic: "TXA", },
Opcode { code: 0x8b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x8c, length: 3, mode: Abs, target: Memm, mnemonic: "STY", },
Opcode { code: 0x8d, length: 3, mode: Abs, target: Memm, mnemonic: "STA", },
Opcode { code: 0x8e, length: 3, mode: Abs, target: Memm, mnemonic: "STX", },
Opcode { code: 0x8f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x90, length: 2, mode: Rel, target: Addr, mnemonic: "BCC", },
Opcode { code: 0x91, length: 2, mode: Iny, target: Memm, mnemonic: "STA", },
Opcode { code: 0x92, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x93, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x94, length: 2, mode: Zpx, target: Memm, mnemonic: "STY", },
Opcode { code: 0x95, length: 2, mode: Zpx, target: Memm, mnemonic: "STA", },
Opcode { code: 0x96, length: 2, mode: Zpy, target: Memm, mnemonic: "STX", },
Opcode { code: 0x97, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x98, length: 1, mode: Imp, target: Areg, mnemonic: "TYA", },
Opcode { code: 0x99, length: 3, mode: Aby, target: Memm, mnemonic: "STA", },
Opcode { code: 0x9a, length: 1, mode: Imp, target: Norg, mnemonic: "TXS", },
Opcode { code: 0x9b, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x9c, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x9d, length: 3, mode: Abx, target: Memm, mnemonic: "STA", },
Opcode { code: 0x9e, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0x9f, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xa0, length: 2, mode: Imm, target: Yreg, mnemonic: "LDY", },
Opcode { code: 0xa1, length: 2, mode: Inx, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xa2, length: 2, mode: Imm, target: Xreg, mnemonic: "LDX", },
Opcode { code: 0xa3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xa4, length: 2, mode: Zpg, target: Yreg, mnemonic: "LDY", },
Opcode { code: 0xa5, length: 2, mode: Zpg, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xa6, length: 2, mode: Zpg, target: Xreg, mnemonic: "LDX", },
Opcode { code: 0xa7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xa8, length: 1, mode: Imp, target: Yreg, mnemonic: "TAY", },
Opcode { code: 0xa9, length: 2, mode: Imm, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xaa, length: 1, mode: Imp, target: Xreg, mnemonic: "TAX", },
Opcode { code: 0xab, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xac, length: 3, mode: Abs, target: Yreg, mnemonic: "LDY", },
Opcode { code: 0xad, length: 3, mode: Abs, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xae, length: 3, mode: Abs, target: Xreg, mnemonic: "LDX", },
Opcode { code: 0xaf, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xb0, length: 2, mode: Rel, target: Addr, mnemonic: "BCS", },
Opcode { code: 0xb1, length: 2, mode: Iny, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xb2, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xb3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xb4, length: 2, mode: Zpx, target: Yreg, mnemonic: "LDY", },
Opcode { code: 0xb5, length: 2, mode: Zpx, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xb6, length: 2, mode: Zpy, target: Xreg, mnemonic: "LDX", },
Opcode { code: 0xb7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xb8, length: 1, mode: Imp, target: Norg, mnemonic: "CLV", },
Opcode { code: 0xb9, length: 3, mode: Aby, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xba, length: 1, mode: Imp, target: Norg, mnemonic: "TSX", },
Opcode { code: 0xbb, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xbc, length: 3, mode: Abx, target: Yreg, mnemonic: "LDY", },
Opcode { code: 0xbd, length: 3, mode: Abx, target: Areg, mnemonic: "LDA", },
Opcode { code: 0xbe, length: 3, mode: Aby, target: Xreg, mnemonic: "LDX", },
Opcode { code: 0xbf, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xc0, length: 2, mode: Imm, target: Norg, mnemonic: "CPY", },
Opcode { code: 0xc1, length: 2, mode: Inx, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xc2, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xc3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xc4, length: 2, mode: Zpg, target: Norg, mnemonic: "CPY", },
Opcode { code: 0xc5, length: 2, mode: Zpg, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xc6, length: 2, mode: Zpg, target: Memm, mnemonic: "DEC", },
Opcode { code: 0xc7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xc8, length: 1, mode: Imp, target: Yreg, mnemonic: "INY", },
Opcode { code: 0xc9, length: 2, mode: Imm, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xca, length: 1, mode: Imp, target: Xreg, mnemonic: "DEX", },
Opcode { code: 0xcb, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xcc, length: 3, mode: Abs, target: Norg, mnemonic: "CPY", },
Opcode { code: 0xcd, length: 3, mode: Abs, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xce, length: 3, mode: Abs, target: Memm, mnemonic: "DEC", },
Opcode { code: 0xcf, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xd0, length: 2, mode: Rel, target: Addr, mnemonic: "BNE", },
Opcode { code: 0xd1, length: 2, mode: Iny, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xd2, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xd3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xd4, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xd5, length: 2, mode: Zpx, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xd6, length: 2, mode: Zpx, target: Memm, mnemonic: "DEC", },
Opcode { code: 0xd7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xd8, length: 1, mode: Imp, target: Norg, mnemonic: "CLD", },
Opcode { code: 0xd9, length: 3, mode: Aby, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xda, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xdb, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xdc, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xdd, length: 3, mode: Abx, target: Norg, mnemonic: "CMP", },
Opcode { code: 0xde, length: 3, mode: Abx, target: Memm, mnemonic: "DEC", },
Opcode { code: 0xdf, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xe0, length: 2, mode: Imm, target: Norg, mnemonic: "CPX", },
Opcode { code: 0xe1, length: 2, mode: Inx, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xe2, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xe3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xe4, length: 2, mode: Zpg, target: Norg, mnemonic: "CPX", },
Opcode { code: 0xe5, length: 2, mode: Zpg, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xe6, length: 2, mode: Zpg, target: Memm, mnemonic: "INC", },
Opcode { code: 0xe7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xe8, length: 1, mode: Imp, target: Xreg, mnemonic: "INX", },
Opcode { code: 0xe9, length: 2, mode: Imm, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xea, length: 1, mode: Imp, target: Norg, mnemonic: "NOP", },
Opcode { code: 0xeb, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xec, length: 3, mode: Abs, target: Norg, mnemonic: "CPX", },
Opcode { code: 0xed, length: 3, mode: Abs, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xee, length: 3, mode: Abs, target: Memm, mnemonic: "INC", },
Opcode { code: 0xef, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xf0, length: 2, mode: Rel, target: Addr, mnemonic: "BEQ", },
Opcode { code: 0xf1, length: 2, mode: Iny, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xf2, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xf3, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xf4, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xf5, length: 2, mode: Zpx, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xf6, length: 2, mode: Zpx, target: Memm, mnemonic: "INC", },
Opcode { code: 0xf7, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xf8, length: 1, mode: Imp, target: Norg, mnemonic: "SED", },
Opcode { code: 0xf9, length: 3, mode: Aby, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xfa, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xfb, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xfc, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
Opcode { code: 0xfd, length: 3, mode: Abx, target: Areg, mnemonic: "SBC", },
Opcode { code: 0xfe, length: 3, mode: Abx, target: Memm, mnemonic: "INC", },
Opcode { code: 0xff, length: 0, mode: Unk, target: Norg, mnemonic: "---", },
];
