// use crate::cpu65::emu::EMU_FUNCS;
//use crate::cpu65::emu::EMU_FUNCS;
use crate::cpu65::Modes::*;

macro_rules! reg {
    ($sel:ident, a) => {
        $sel.mem[0x10000]
    };
    ($sel:ident, x) => {
        $sel.mem[0x10001]
    };
    ($sel:ident, y) => {
        $sel.mem[0x10002]
    };
}

pub const MAX_ADD: usize = 0xffff;
pub const MEM_SIZE: usize = MAX_ADD + 6; // registers stored in array
pub const A_REG: usize = 0x10000;
pub const X_REG: usize = 0x10001;
pub const Y_REG: usize = 0x10002;

//#[derive(Debug)]
#[derive(Copy, Clone)]
pub enum Modes {
    Inx, // indirect x
    Iny, // indirect y
    Zpg, // zero page
    Imm, // immediate mode
    Abs, // absolute
    Zpx, // zero page x
    Zpy, // zero page y
    Aby, // absolute y
    Abx, // absolute x
    Acc, // accumulator
    Ind, // indirect
    Rel, // relative
    Imp, // implied
    Unk,
    Tst(fn(&mut CPU) -> &mut u8),
}

// the status register
enum Status {
    C, // carry
    Z, // zero
    I, // interrupt
    D, // decimal
    B, // break
    U, // unused
    V, // overflow
    N, // negative
}

// CPU virtual processor + memory
pub struct CPU {
    // a:      usize,
    // x:      usize,
    // y:      usize,
    status: u8,
    sp:     u8,
    pc:     u16,
    mem:    [u8; MEM_SIZE],
    ins:    [Opcode; 256],
}

// use std::fmt;
// impl fmt::Display for CPU {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", "I'm a 6502!")
//     }
// }

impl<'a> CPU {
    pub fn new() -> CPU {
        CPU {
            // register a is stored in extra byte of memory
            // this makes Addessing modes easier
            // (or could be a huge mistake)
            // a:      0x10000, // Addess of reg a in Memory
            // x:      0x10001,
            // y:      0x10002,
            status: 0,
            sp:     0xff,
            pc:     0,
            mem:    [0; MEM_SIZE],
            ins:    OPCODES,
        }
    }

    #[inline(always)]
    fn push_8(&mut self, n: u8) {
        self.mem[0x100 + self.sp as usize] = n;
        self.sp = self.sp.wrapping_sub(1);
    }
    #[inline(always)]
    fn push_16(&mut self, n: u16) {
        self.mem[0x100 + self.sp as usize - 1] = n as u8;
        self.mem[0x100 + self.sp as usize] = (n >> 8) as u8;
        self.sp = self.sp.wrapping_sub(2);
    }

    fn push_op_16(&mut self) {
        self.mem[0x100 + self.sp as usize - 1] = self.mem[self.pc as usize + 1];
        self.mem[0x100 + self.sp as usize] = self.mem[self.pc as usize + 2];
        self.sp = self.sp.wrapping_sub(2);
    }

    fn pop_8(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mem[0x100 + self.sp as usize]
    }

    fn pop_16(&mut self) -> u16 {
        self.sp = self.sp.wrapping_add(2);
        u16::from(self.mem[0x100 + self.sp as usize - 1])
            & u16::from(self.mem[0x100 + self.sp as usize]) << 8
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    pub fn get_mem_mut(&mut self) -> &mut [u8] {
        &mut self.mem
    }
    pub fn get_mem(&self) -> &[u8] {
        &self.mem
    }

    pub fn load(&mut self, buf: &[u8]) -> bool {
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
        self.mem[0] = 0x40;
        true
    }

    #[inline(always)]
    fn get_op_add(&self) -> usize {
        (self.mem[self.pc as usize + 1] as usize) | (self.mem[self.pc as usize + 2] as usize) << 8
    }
    #[inline(always)]
    fn get_op_16(&self) -> u16 {
        (self.mem[self.pc as usize + 1] as u16) | (self.mem[self.pc as usize + 2] as u16) << 8
    }
    #[inline(always)]
    fn get_op_8(&self) -> u8 {
        self.mem[self.pc as usize + 1]
    }
    #[inline(always)]
    fn get_mem_16(&self, a: usize) -> u16 {
        // let m16 = |m: &[u8], i: usize| (m[i] as usize) & (m[i + 1] as usize) << 8;
        (self.mem[a] as u16) & (self.mem[a + 1] as u16) << 8
    }

    #[inline(always)]
    fn get_mem_usize(&self, a: usize) -> usize {
        // let m16 = |m: &[u8], i: usize| (m[i] as usize) & (m[i + 1] as usize) << 8;
        (self.mem[a] as usize) & (self.mem[a + 1] as usize) << 8
    }

    fn get_eff_add(&self) -> usize {
        // fix me zero page wrapping
        reg!(self, x);

        match OPCODES[self.mem[self.pc as usize] as usize].mode {
            Imm => self.pc as usize + 1,
            Zpg => self.mem[self.pc as usize + 1] as usize,
            Abs => {
                (self.mem[self.pc as usize + 1] as usize)
                    | (self.mem[self.pc as usize + 2] as usize) << 8
            }
            Inx => {
                (self.mem[(self.mem[self.pc as usize + 1] + reg!(self, x)) as usize] as usize)
                    & (self.mem[(self.mem[self.pc as usize + 1] + reg!(self, x)) as usize + 1]
                        as usize)
                        << 8
            }
            Iny => {
                self.get_mem_usize(self.mem[self.pc as usize + 1] as usize) + reg!(self, y) as usize
            }
            Zpx => (self.mem[self.pc as usize + 1]).wrapping_add(reg!(self, x)) as usize,
            Zpy => (self.mem[self.pc as usize + 1]).wrapping_add(reg!(self, y)) as usize,
            Abx => self.get_op_add() + reg!(self, x) as usize,
            Aby => self.get_op_add() + reg!(self, y) as usize,
            Acc => A_REG,
            _ => panic!("Instruction mode doesn't target an Addess!"),
        }
    }

    #[inline(always)]
    fn set_nz_reg(&mut self, r: u8) {
        if r == 0 {
            self.status |= 1 << Status::Z as u8;
            self.status &= !(1 << (Status::N as u8));
        } else {
            self.status &= !(1 << (Status::Z as u8));
            if r & (1 << 7) != 0 {
                self.status |= 1 << Status::N as u8;
            } else {
                self.status &= !(1 << (Status::N as u8));
            }
        }
    }

    pub fn fetch(&mut self) {
        // let opcode = self.get_opcode();

        // match opcode.target {
        //     Mem => self.get_eff_add(),
        //     _ => 0,
        // };
    }

    pub fn step(&mut self) {
        let oc = &OPCODES[self.mem[self.pc as usize] as usize];
        unsafe {
            static mut C: u32 = 1;
            print!(
                "{:04}:{:>04X} {:>02X} -> {}",
                C, self.pc, oc.code, oc.mnemonic,
            );
            match oc.ops {
                2 => println!(
                    " {:>02X}{:>02X}",
                    self.mem[self.pc as usize + 2],
                    self.mem[self.pc as usize + 1]
                ),
                1 => println!(" {:>02X}", self.mem[self.pc as usize + 1]),
                _ => println!(""),
            };
            C += 1;
        }
        (oc.ef)(self); // call emu fn
        self.pc += oc.step;
    }

    #[inline(always)]
    pub fn get_opcode(&self) -> &Opcode {
        &OPCODES[self.mem[self.pc as usize] as usize]
    }

    pub fn emu_err(&mut self) {
        let o = self.get_opcode();
        panic!(format!(
            "Emulation for instruction {} not implemented!",
            o.mnemonic
        ));
    }

    fn emu_nop(&mut self) {}

    fn emu_cld(&mut self) {
        self.status &= !(1 << (Status::D as u8));
    }

    fn emu_clc(&mut self) {
        self.status &= !(1 << (Status::C as u8));
    }

    fn emu_jsr(&mut self) {
        let t = self.pc + 2;
        self.mem[0x100 + self.sp as usize - 1] = t as u8;
        self.mem[0x100 + self.sp as usize] = (t >> 8) as u8;
        self.sp -= 2;

        self.pc = u16::from(self.mem[self.pc as usize + 1])
            | u16::from(self.mem[self.pc as usize + 2]) << 8
    }

    fn emu_rts(&mut self) {
        self.sp = self.sp.wrapping_add(2);
        self.pc = (u16::from(self.mem[0x100 + self.sp as usize - 1])
            | (u16::from(self.mem[0x100 + self.sp as usize]) << 8))
            + 1;
    }

    fn emu_sta(&mut self) {
        self.mem[self.get_eff_add()] = reg!(self, a);
    }

    fn emu_ldy(&mut self) {
        reg!(self, y) = self.mem[self.get_eff_add()];
        self.set_nz_reg(reg!(self, y));
    }

    fn emu_dey(&mut self) {
        reg!(self, y) -= 1;
        self.set_nz_reg(reg!(self, y));
    }

    fn emu_lda(&mut self) {
        reg!(self, a) = self.mem[self.get_eff_add()];
        self.set_nz_reg(reg!(self, a));
    }

    fn emu_asl(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        let bit7 = *addr & (1 << 7);
        *addr <<= 1;
        self.status = bit7 >> 7;
    }

    fn emu_ror(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        let bit0 = *addr & 1; // save bit0 before shifting
        *addr >>= 1;
        *addr |= self.status & ((Status::C as u8) << 7); // carry goes into bit 7
        self.status |= bit0; // bit 0 goes into carry bit
    }

    fn emu_rol(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        let bit7 = *addr & (1 << 7); // save bit7 before shifting
        *addr <<= 1;
        *addr |= self.status & (Status::C as u8); // carry goes into bit 0
        self.status |= bit7 >> 7; // bit 0 goes into carry bit
    }

    fn emu_bra(&mut self) {
        let status: bool;
        match self.mem[self.pc as usize] {
            0x10 => status = (self.status & (1 << Status::N as u8)) == 0,
            0x30 => status = (self.status & (1 << Status::N as u8)) != 0,
            0x50 => status = (self.status & (1 << Status::V as u8)) == 0,
            0x70 => status = (self.status & (1 << Status::V as u8)) != 0,
            0x90 => status = (self.status & (1 << Status::C as u8)) == 0,
            0xb0 => status = (self.status & (1 << Status::C as u8)) != 0,
            0xd0 => status = (self.status & (1 << Status::Z as u8)) == 0,
            0xf0 => status = (self.status & (1 << Status::Z as u8)) != 0,
            _ => panic!("Encountered unknown branch opcode!"),
        }
        if status {
            self.pc =
                2 + ((self.pc as i16) + i16::from(self.mem[self.pc as usize + 1] as i8)) as u16;
        } else {
            self.pc += 2;
        }
    }

    fn emu_cmp(&mut self) {
        let r: i8 = reg!(self, a) as i8 - self.mem[self.get_eff_add()] as i8;
        self.set_nz_reg(r as u8);
        if r >= 0 {
            self.status |= (Status::C as u8) << 1
        } else {
            self.status &= !(1 << (Status::C as u8))
        }
    }

    fn emu_pla(&mut self) {
        self.sp += 1;
        reg!(self, a) = self.mem[0x100 + self.sp as usize];
        self.set_nz_reg(reg!(self, a));
    }

    fn emu_pha(&mut self) {
        self.mem[0x100 + self.sp as usize] = reg!(self, a);
        self.sp -= 1;
    }

    fn emu_sbc(&mut self) {
        let n = i32::from(reg!(self, a)) - i32::from(self.mem[self.get_eff_add()]);
        if n < -128 || n > 127 {
            self.status |= (Status::V as u8) << 1
        } else {
            self.status &= !(1 << (Status::V as u8))
        }
        if n > 0 {
            self.status |= (Status::C as u8) << 1
        } else {
            self.status &= !(1 << (Status::C as u8))
        }
        reg!(self, a) = n as u8;
    }

    fn emu_rti(&mut self) {
        self.status = self.pop_8();
        self.pc = self.pop_16();
    }

    fn emu_brk(&mut self) {
        self.push_16(self.pc as u16);
        self.push_8(self.status);
        self.pc = self.get_mem_16(0xfffe);
        self.status |= 1 << Status::B as u8;
        // self.pc += 1;
    }
}

const DECODE_ZPG: fn(usize, &mut [u8; 0x10000]) -> &mut u8 =
    |pc: usize, m: &mut [u8; 0x10000]| &mut m[m[pc as usize + 1] as usize];

struct Instrs {
    op: [Opcode; 256],
}

impl<'a> Instrs {
    pub fn new() -> Instrs {
        Instrs { op: OPCODES }
    }
}

#[derive(Copy, Clone)]
pub struct Opcode {
    pub code:     i32,          // the opcode
    pub ef:       fn(&mut CPU), // the thing that gets the result
    pub step:     u16,
    pub ops:      i32,
    pub mode:     Modes,
    pub mnemonic: &'static str,
}

// use cpu65::Modes::*;
#[cfg_attr(rustfmt, rustfmt_skip)]
pub const OPCODES: [Opcode; 256] = [
Opcode { code: 0x00, ef: CPU::emu_brk, step: 0, ops: 0, mode: Imp, mnemonic: "BRK", },
Opcode { code: 0x01, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, mnemonic: "ORA", },
Opcode { code: 0x02, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x03, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x04, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x05, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "ORA", },
Opcode { code: 0x06, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpg, mnemonic: "ASL", },
Opcode { code: 0x07, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x08, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "PHP", },
Opcode { code: 0x09, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "ORA", },
Opcode { code: 0x0a, ef: CPU::emu_asl, step: 1, ops: 0, mode: Acc, mnemonic: "ASL", },
Opcode { code: 0x0b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x0c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x0d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "ORA", },
Opcode { code: 0x0e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abs, mnemonic: "ASL", },
Opcode { code: 0x0f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x10, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BPL", },
Opcode { code: 0x11, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, mnemonic: "ORA", },
Opcode { code: 0x12, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x13, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x14, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x15, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "ORA", },
Opcode { code: 0x16, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpx, mnemonic: "ASL", },
Opcode { code: 0x17, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x18, ef: |c: &mut CPU| c.status &= !(1 << (Status::C as u8)), step: 1, ops: 0, mode: Imp, mnemonic: "CLC", },
Opcode { code: 0x19, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, mnemonic: "ORA", },
Opcode { code: 0x1a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "ORA", },
Opcode { code: 0x1e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abx, mnemonic: "ASL", },
Opcode { code: 0x1f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x20, ef: CPU::emu_jsr, step: 0, ops: 2, mode: Abs, mnemonic: "JSR", },
Opcode { code: 0x21, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, mnemonic: "AND", },
Opcode { code: 0x22, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x23, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x24, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "BIT", },
Opcode { code: 0x25, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "AND", },
Opcode { code: 0x26, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpg, mnemonic: "ROL", },
Opcode { code: 0x27, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x28, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "PLP", },
Opcode { code: 0x29, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "AND", },
Opcode { code: 0x2a, ef: CPU::emu_rol, step: 1, ops: 0, mode: Acc, mnemonic: "ROL", },
Opcode { code: 0x2b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x2c, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "BIT", },
Opcode { code: 0x2d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "AND", },
Opcode { code: 0x2e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abs, mnemonic: "ROL", },
Opcode { code: 0x2f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x30, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BMI", },
Opcode { code: 0x31, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, mnemonic: "AND", },
Opcode { code: 0x32, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x33, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x34, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x35, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "AND", },
Opcode { code: 0x36, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpx, mnemonic: "ROL", },
Opcode { code: 0x37, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x38, ef: |c: &mut CPU| c.status |= 1 << (Status::C as u8), step: 1, ops: 0, mode: Imp, mnemonic: "SEC", },
Opcode { code: 0x39, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, mnemonic: "AND", },
Opcode { code: 0x3a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "AND", },
Opcode { code: 0x3e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abx, mnemonic: "ROL", },
Opcode { code: 0x3f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x40, ef: CPU::emu_rti, step: 0, ops: 0, mode: Imp, mnemonic: "RTI", },
Opcode { code: 0x41, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, mnemonic: "EOR", },
Opcode { code: 0x42, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x43, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x44, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x45, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "EOR", },
Opcode { code: 0x46, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "LSR", },
Opcode { code: 0x47, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x48, ef: CPU::emu_pha, step: 1, ops: 0, mode: Imp, mnemonic: "PHA", },
Opcode { code: 0x49, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "EOR", },
Opcode { code: 0x4a, ef: CPU::emu_err, step: 1, ops: 0, mode: Acc, mnemonic: "LSR", },
Opcode { code: 0x4b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x4c, ef: CPU::emu_err, step: 0, ops: 2, mode: Abs, mnemonic: "JMP", },
Opcode { code: 0x4d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "EOR", },
Opcode { code: 0x4e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "LSR", },
Opcode { code: 0x4f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x50, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BVC", },
Opcode { code: 0x51, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, mnemonic: "EOR", },
Opcode { code: 0x52, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x53, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x54, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x55, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "EOR", },
Opcode { code: 0x56, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "LSR", },
Opcode { code: 0x57, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x58, ef: |c: &mut CPU| c.status &= !(1 << (Status::I as u8)), step: 1, ops: 0, mode: Imp, mnemonic: "CLI", },
Opcode { code: 0x59, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, mnemonic: "EOR", },
Opcode { code: 0x5a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "EOR", },
Opcode { code: 0x5e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "LSR", },
Opcode { code: 0x5f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x60, ef: CPU::emu_rts, step: 1, ops: 0, mode: Imp, mnemonic: "RTS", },
Opcode { code: 0x61, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, mnemonic: "ADC", },
Opcode { code: 0x62, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x63, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x64, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x65, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "ADC", },
Opcode { code: 0x66, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpg, mnemonic: "ROR", },
Opcode { code: 0x67, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x68, ef: CPU::emu_pla, step: 1, ops: 0, mode: Imp, mnemonic: "PLA", },
Opcode { code: 0x69, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "ADC", },
Opcode { code: 0x6a, ef: CPU::emu_ror, step: 1, ops: 0, mode: Acc, mnemonic: "ROR", },
Opcode { code: 0x6b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x6c, ef: CPU::emu_err, step: 0, ops: 2, mode: Ind, mnemonic: "JMP", },
Opcode { code: 0x6d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "ADC", },
Opcode { code: 0x6e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abs, mnemonic: "ROR", },
Opcode { code: 0x6f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x70, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BVS", },
Opcode { code: 0x71, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, mnemonic: "ADC", },
Opcode { code: 0x72, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x73, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x74, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x75, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "ADC", },
Opcode { code: 0x76, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpx, mnemonic: "ROR", },
Opcode { code: 0x77, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x78, ef: |c: &mut CPU| c.status |= 1 << (Status::C as u8), step: 1, ops: 0, mode: Imp, mnemonic: "SEI", },
Opcode { code: 0x79, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, mnemonic: "ADC", },
Opcode { code: 0x7a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "ADC", },
Opcode { code: 0x7e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abx, mnemonic: "ROR", },
Opcode { code: 0x7f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x80, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x81, ef: CPU::emu_sta, step: 2, ops: 1, mode: Inx, mnemonic: "STA", },
Opcode { code: 0x82, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x83, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x84, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "STY", },
Opcode { code: 0x85, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpg, mnemonic: "STA", },
Opcode { code: 0x86, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "STX", },
Opcode { code: 0x87, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x88, ef: CPU::emu_dey, step: 1, ops: 0, mode: Imp, mnemonic: "DEY", },
Opcode { code: 0x89, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x8a, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TXA", },
Opcode { code: 0x8b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x8c, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "STY", },
Opcode { code: 0x8d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abs, mnemonic: "STA", },
Opcode { code: 0x8e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "STX", },
Opcode { code: 0x8f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x90, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BCC", },
Opcode { code: 0x91, ef: CPU::emu_sta, step: 2, ops: 1, mode: Iny, mnemonic: "STA", },
Opcode { code: 0x92, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x93, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x94, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "STY", },
Opcode { code: 0x95, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpx, mnemonic: "STA", },
Opcode { code: 0x96, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpy, mnemonic: "STX", },
Opcode { code: 0x97, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x98, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TYA", },
Opcode { code: 0x99, ef: CPU::emu_sta, step: 3, ops: 2, mode: Aby, mnemonic: "STA", },
Opcode { code: 0x9a, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TXS", },
Opcode { code: 0x9b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abx, mnemonic: "STA", },
Opcode { code: 0x9e, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa0, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Imm, mnemonic: "LDY", },
Opcode { code: 0xa1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Inx, mnemonic: "LDA", },
Opcode { code: 0xa2, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "LDX", },
Opcode { code: 0xa3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpg, mnemonic: "LDY", },
Opcode { code: 0xa5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpg, mnemonic: "LDA", },
Opcode { code: 0xa6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "LDX", },
Opcode { code: 0xa7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TAY", },
Opcode { code: 0xa9, ef: CPU::emu_lda, step: 2, ops: 1, mode: Imm, mnemonic: "LDA", },
Opcode { code: 0xaa, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TAX", },
Opcode { code: 0xab, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xac, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abs, mnemonic: "LDY", },
Opcode { code: 0xad, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abs, mnemonic: "LDA", },
Opcode { code: 0xae, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "LDX", },
Opcode { code: 0xaf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb0, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BCS", },
Opcode { code: 0xb1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Iny, mnemonic: "LDA", },
Opcode { code: 0xb2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpx, mnemonic: "LDY", },
Opcode { code: 0xb5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpx, mnemonic: "LDA", },
Opcode { code: 0xb6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpy, mnemonic: "LDX", },
Opcode { code: 0xb7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb8, ef: |c: &mut CPU| c.status &= !(1 << (Status::V as u8)), step: 1, ops: 0, mode: Imp, mnemonic: "CLV", },
Opcode { code: 0xb9, ef: CPU::emu_lda, step: 3, ops: 2, mode: Aby, mnemonic: "LDA", },
Opcode { code: 0xba, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "TSX", },
Opcode { code: 0xbb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xbc, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abx, mnemonic: "LDY", },
Opcode { code: 0xbd, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abx, mnemonic: "LDA", },
Opcode { code: 0xbe, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, mnemonic: "LDX", },
Opcode { code: 0xbf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc0, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "CPY", },
Opcode { code: 0xc1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Inx, mnemonic: "CMP", },
Opcode { code: 0xc2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc4, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "CPY", },
Opcode { code: 0xc5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpg, mnemonic: "CMP", },
Opcode { code: 0xc6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "DEC", },
Opcode { code: 0xc7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "INY", },
Opcode { code: 0xc9, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Imm, mnemonic: "CMP", },
Opcode { code: 0xca, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "DEX", },
Opcode { code: 0xcb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xcc, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "CPY", },
Opcode { code: 0xcd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abs, mnemonic: "CMP", },
Opcode { code: 0xce, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "DEC", },
Opcode { code: 0xcf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd0, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BNE", },
Opcode { code: 0xd1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Iny, mnemonic: "CMP", },
Opcode { code: 0xd2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpx, mnemonic: "CMP", },
Opcode { code: 0xd6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "DEC", },
Opcode { code: 0xd7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd8, ef: |c: &mut CPU| c.status &= !(1 << (Status::D as u8)), step: 1, ops: 0, mode: Imp, mnemonic: "CLD", },
Opcode { code: 0xd9, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Aby, mnemonic: "CMP", },
Opcode { code: 0xda, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abx, mnemonic: "CMP", },
Opcode { code: 0xde, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "DEC", },
Opcode { code: 0xdf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe0, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, mnemonic: "CPX", },
Opcode { code: 0xe1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Inx, mnemonic: "SBC", },
Opcode { code: 0xe2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe4, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "CPX", },
Opcode { code: 0xe5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpg, mnemonic: "SBC", },
Opcode { code: 0xe6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, mnemonic: "INC", },
Opcode { code: 0xe7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, mnemonic: "INX", },
Opcode { code: 0xe9, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Imm, mnemonic: "SBC", },
Opcode { code: 0xea, ef: |_c: &mut CPU|{}, step: 1, ops: 0, mode: Imp, mnemonic: "NOP", },
Opcode { code: 0xeb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xec, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "CPX", },
Opcode { code: 0xed, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abs, mnemonic: "SBC", },
Opcode { code: 0xee, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, mnemonic: "INC", },
Opcode { code: 0xef, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf0, ef: CPU::emu_bra, step: 0, ops: 0, mode: Rel, mnemonic: "BEQ", },
Opcode { code: 0xf1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Iny, mnemonic: "SBC", },
Opcode { code: 0xf2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpx, mnemonic: "SBC", },
Opcode { code: 0xf6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, mnemonic: "INC", },
Opcode { code: 0xf7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf8, ef: |c: &mut CPU| c.status |= 1 << (Status::D as u8), step: 1, ops: 0, mode: Imp, mnemonic: "SED", },
Opcode { code: 0xf9, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Aby, mnemonic: "SBC", },
Opcode { code: 0xfa, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfd, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abx, mnemonic: "SBC", },
Opcode { code: 0xfe, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, mnemonic: "INC", },
Opcode { code: 0xff, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, mnemonic: "---", },
];
