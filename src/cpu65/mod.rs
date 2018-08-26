pub mod emu;

// use crate::cpu65::emu::EMU_FUNCS;
//use crate::cpu65::emu::EMU_FUNCS;
use crate::cpu65::Modes::*;

pub const MAX_ADD: usize = 0xffff;
pub const MEM_SIZE: usize = MAX_ADD + 1;

//#[derive(Debug)]
pub enum Modes {
    Inx(fn(&mut CPU) -> &mut u8), // indirect x
    Zpg(fn(&mut CPU) -> &mut u8), // zero page
    Imm(fn(&mut CPU) -> &mut u8), // immediate mode
    Abs(fn(&mut CPU) -> &mut u8), // absolute
    Iny(fn(&mut CPU) -> &mut u8), // indirect y
    Zpx(fn(&mut CPU) -> &mut u8), // zero page x
    Aby(fn(&mut CPU) -> &mut u8), // absolute y
    Abx(fn(&mut CPU) -> &mut u8), // absolute x
    Ind,                          // indirect
    Acc(fn(&mut CPU) -> &mut u8), // accumulator
    Rel,                          // relative
    Imp,                          // implied
    Zpy(fn(&mut CPU) -> &mut u8), // zero page y
    Unk,
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
    a:      u8,
    x:      u8,
    y:      u8,
    status: u8,
    sp:     u8,
    pc:     u16,
    mem:    [u8; MEM_SIZE],
}

// use std::fmt;
// impl fmt::Display for CPU {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         write!(f, "{}", "I'm a 6502!")
//     }
// }
static mut X: u8 = 0;
impl<'a> CPU {
    pub fn new() -> CPU {
        CPU {
            // register a is stored in extra byte of memory
            // this makes Addessing modes easier
            // (or could be a huge mistake)
            a:      0, // Addess of reg a in Memory
            x:      0,
            y:      0,
            status: 0,
            sp:     0xff,
            pc:     0,
            mem:    [0; 0x10000],
        }
    }

    #[inline(always)]
    fn push_8(&mut self, n: u8) {
        self.mem[0x100 + self.sp as usize] = n;
        self.sp -= 1;
    }
    #[inline(always)]
    fn push_16(&mut self, n: u16) {
        self.mem[0x100 + self.sp as usize - 1] = n as u8;
        self.mem[0x100 + self.sp as usize] = (n >> 8) as u8;
        self.sp -= 2;
    }

    fn push_op_16(&mut self) {
        self.mem[0x100 + self.sp as usize - 1] = self.mem[self.pc as usize + 1];
        self.mem[0x100 + self.sp as usize] = self.mem[self.pc as usize + 2];
        self.sp -= 2;
    }

    fn pop_16(&mut self) -> u16 {
        self.sp += 2;
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

    fn get_eff_add(&mut self) -> &mut u8 {
        // fix me zero page wrapping
        match OPCODES[self.mem[self.pc as usize] as usize].mode {
            Imm(f) => f(self),
            Zpg(f) => f(self),
            Abs(f) => f(self),
            Inx(f) => f(self),
            Iny(f) => f(self),
            Zpx(f) => f(self),
            Aby(f) => f(self),
            Abx(f) => f(self),
            Acc(f) => f(self),
            _ => panic!("Instruction mode doesn't target an Addess!"),
        }
    }

    #[inline(always)]
    fn get_imm_add(&mut self) -> &mut u8 {
        &mut self.mem[self.pc as usize + 1]
    }

    #[inline(always)]
    fn get_zpg_add(&mut self) -> &mut u8 {
        &mut self.mem[self.mem[self.pc as usize + 1] as usize]
    }

    #[inline(always)]
    fn get_abs_add(&mut self) -> &mut u8 {
        &mut self.mem[self.get_op_add()]
    }

    #[inline(always)]
    fn get_inx_add(&mut self) -> &mut u8 {
        &mut self.mem[self.get_mem_usize((self.mem[self.pc as usize + 1] + self.x) as usize)]
    }

    #[inline(always)]
    fn get_iny_add(&mut self) -> &mut u8 {
        &mut self.mem[self.get_mem_usize(self.mem[self.pc as usize + 1] as usize) + self.y as usize] //fix me?
    }

    #[inline(always)]
    fn get_zpx_add(&mut self) -> &mut u8 {
        &mut self.mem[self.pc as usize + 1 + self.x as usize]
    }

    #[inline(always)]
    fn get_zpy_add(&mut self) -> &mut u8 {
        &mut self.mem[self.pc as usize + 1 + self.y as usize]
    }

    #[inline(always)]
    fn get_abx_add(&mut self) -> &mut u8 {
        &mut self.mem[self.get_op_add() + self.x as usize]
    }

    #[inline(always)]
    fn get_aby_add(&mut self) -> &mut u8 {
        &mut self.mem[self.get_op_add() + self.y as usize]
    }

    #[inline(always)]
    fn get_acc_add(&mut self) -> &mut u8 {
        &mut self.a
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
        println!("{:>04X} {:>02X} -> {}", self.pc, oc.code, oc.mnemonic);

        (oc.ef)(self); // call emu fn
        self.pc += oc.length;
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
        self.sp += 2;
        self.pc = u16::from(self.mem[0x100 + self.sp as usize - 1])
            | ((u16::from(self.mem[0x100 + self.sp as usize]) << 8) + 1)
    }

    fn emu_sta(&mut self) {
        *self.get_eff_add() = self.a;
    }

    fn emu_ldy(&mut self) {
        self.y = *self.get_eff_add();
        self.set_nz_reg(self.y);
    }

    fn emu_dey(&mut self) {
        self.y -= 1;
        self.set_nz_reg(self.y);
    }

    fn emu_lda(&mut self) {
        self.a = *self.get_eff_add();
        self.set_nz_reg(self.a);
    }

    fn emu_ror(&mut self) {
        // unecessary copying because the compiler is a little bitch
        let carry = self.status & ((Status::C as u8) << 7);
        let add = self.get_eff_add();
        let bit0 = *add & 1; // save bit0 before shifting
        *add >>= 1;
        *add |= carry; // carry goes into bit 7
        self.status |= bit0; // bit 0 goes into carry bit
    }

    fn emu_rol(&mut self) {
        // ditto
        let carry = self.status & (Status::C as u8);
        let add = self.get_eff_add();
        let bit7 = *add & (1 << 7); // save bit7 before shifting
        *add <<= 1;
        *add |= carry; // carry goes into bit 0
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

    fn emu_brk(&mut self) {
        self.push_16(self.pc as u16);
        self.push_8(self.status);
        self.pc = self.get_mem_16(0xfffe);
        self.status |= 1 << Status::B as u8;
        // self.pc += 1;
    }
}

pub struct Opcode {
    pub code:     i32,          // the opcode
    pub ef:       fn(&mut CPU), // the thing that gets the result
    pub length:   u16,
    pub mode:     Modes,
    pub mnemonic: &'static str,
}

// use cpu65::Modes::*;
#[cfg_attr(rustfmt, rustfmt_skip)]
pub const OPCODES: [Opcode; 256] = [
Opcode { code: 0x00, ef: CPU::emu_brk, length: 1, mode: Imp, mnemonic: "BRK", },
Opcode { code: 0x01, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "ORA", },
Opcode { code: 0x02, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x03, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x04, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x05, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "ORA", },
Opcode { code: 0x06, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "ASL", },
Opcode { code: 0x07, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x08, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "PHP", },
Opcode { code: 0x09, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "ORA", },
Opcode { code: 0x0a, ef: CPU::emu_err, length: 1, mode: Acc(CPU::get_acc_add), mnemonic: "ASL", },
Opcode { code: 0x0b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x0c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x0d, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "ORA", },
Opcode { code: 0x0e, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "ASL", },
Opcode { code: 0x0f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x10, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BPL", },
Opcode { code: 0x11, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "ORA", },
Opcode { code: 0x12, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x13, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x14, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x15, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "ORA", },
Opcode { code: 0x16, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "ASL", },
Opcode { code: 0x17, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x18, ef: CPU::emu_clc, length: 1, mode: Imp, mnemonic: "CLC", },
Opcode { code: 0x19, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "ORA", },
Opcode { code: 0x1a, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x1d, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "ORA", },
Opcode { code: 0x1e, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "ASL", },
Opcode { code: 0x1f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x20, ef: CPU::emu_jsr, length: 0, mode: Abs(CPU::get_abs_add), mnemonic: "JSR", },
Opcode { code: 0x21, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "AND", },
Opcode { code: 0x22, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x23, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x24, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "BIT", },
Opcode { code: 0x25, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "AND", },
Opcode { code: 0x26, ef: CPU::emu_rol, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "ROL", },
Opcode { code: 0x27, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x28, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "PLP", },
Opcode { code: 0x29, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "AND", },
Opcode { code: 0x2a, ef: CPU::emu_rol, length: 1, mode: Acc(CPU::get_acc_add), mnemonic: "ROL", },
Opcode { code: 0x2b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x2c, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "BIT", },
Opcode { code: 0x2d, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "AND", },
Opcode { code: 0x2e, ef: CPU::emu_rol, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "ROL", },
Opcode { code: 0x2f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x30, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BMI", },
Opcode { code: 0x31, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "AND", },
Opcode { code: 0x32, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x33, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x34, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x35, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "AND", },
Opcode { code: 0x36, ef: CPU::emu_rol, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "ROL", },
Opcode { code: 0x37, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x38, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "SEC", },
Opcode { code: 0x39, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "AND", },
Opcode { code: 0x3a, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x3d, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "AND", },
Opcode { code: 0x3e, ef: CPU::emu_rol, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "ROL", },
Opcode { code: 0x3f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x40, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "RTI", },
Opcode { code: 0x41, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "EOR", },
Opcode { code: 0x42, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x43, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x44, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x45, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "EOR", },
Opcode { code: 0x46, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "LSR", },
Opcode { code: 0x47, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x48, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "PHA", },
Opcode { code: 0x49, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "EOR", },
Opcode { code: 0x4a, ef: CPU::emu_err, length: 1, mode: Acc(CPU::get_acc_add), mnemonic: "LSR", },
Opcode { code: 0x4b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x4c, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "JMP", },
Opcode { code: 0x4d, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "EOR", },
Opcode { code: 0x4e, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "LSR", },
Opcode { code: 0x4f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x50, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BVC", },
Opcode { code: 0x51, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "EOR", },
Opcode { code: 0x52, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x53, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x54, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x55, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "EOR", },
Opcode { code: 0x56, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "LSR", },
Opcode { code: 0x57, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x58, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "CLI", },
Opcode { code: 0x59, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "EOR", },
Opcode { code: 0x5a, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x5d, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "EOR", },
Opcode { code: 0x5e, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "LSR", },
Opcode { code: 0x5f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x60, ef: CPU::emu_rts, length: 1, mode: Imp, mnemonic: "RTS", },
Opcode { code: 0x61, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "ADC", },
Opcode { code: 0x62, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x63, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x64, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x65, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "ADC", },
Opcode { code: 0x66, ef: CPU::emu_ror, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "ROR", },
Opcode { code: 0x67, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x68, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "PLA", },
Opcode { code: 0x69, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "ADC", },
Opcode { code: 0x6a, ef: CPU::emu_ror, length: 1, mode: Acc(CPU::get_acc_add), mnemonic: "ROR", },
Opcode { code: 0x6b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x6c, ef: CPU::emu_err, length: 3, mode: Ind, mnemonic: "JMP", },
Opcode { code: 0x6d, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "ADC", },
Opcode { code: 0x6e, ef: CPU::emu_ror, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "ROR", },
Opcode { code: 0x6f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x70, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BVS", },
Opcode { code: 0x71, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "ADC", },
Opcode { code: 0x72, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x73, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x74, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x75, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "ADC", },
Opcode { code: 0x76, ef: CPU::emu_ror, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "ROR", },
Opcode { code: 0x77, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x78, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "SEI", },
Opcode { code: 0x79, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "ADC", },
Opcode { code: 0x7a, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x7d, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "ADC", },
Opcode { code: 0x7e, ef: CPU::emu_ror, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "ROR", },
Opcode { code: 0x7f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x80, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x81, ef: CPU::emu_sta, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "STA", },
Opcode { code: 0x82, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x83, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x84, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "STY", },
Opcode { code: 0x85, ef: CPU::emu_sta, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "STA", },
Opcode { code: 0x86, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "STX", },
Opcode { code: 0x87, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x88, ef: CPU::emu_dey, length: 1, mode: Imp, mnemonic: "DEY", },
Opcode { code: 0x89, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x8a, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TXA", },
Opcode { code: 0x8b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x8c, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "STY", },
Opcode { code: 0x8d, ef: CPU::emu_sta, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "STA", },
Opcode { code: 0x8e, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "STX", },
Opcode { code: 0x8f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x90, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BCC", },
Opcode { code: 0x91, ef: CPU::emu_sta, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "STA", },
Opcode { code: 0x92, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x93, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x94, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "STY", },
Opcode { code: 0x95, ef: CPU::emu_sta, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "STA", },
Opcode { code: 0x96, ef: CPU::emu_err, length: 2, mode: Zpy(CPU::get_zpy_add), mnemonic: "STX", },
Opcode { code: 0x97, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x98, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TYA", },
Opcode { code: 0x99, ef: CPU::emu_sta, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "STA", },
Opcode { code: 0x9a, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TXS", },
Opcode { code: 0x9b, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9c, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9d, ef: CPU::emu_sta, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "STA", },
Opcode { code: 0x9e, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0x9f, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa0, ef: CPU::emu_ldy, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "LDY", },
Opcode { code: 0xa1, ef: CPU::emu_lda, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "LDA", },
Opcode { code: 0xa2, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "LDX", },
Opcode { code: 0xa3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa4, ef: CPU::emu_ldy, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "LDY", },
Opcode { code: 0xa5, ef: CPU::emu_lda, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "LDA", },
Opcode { code: 0xa6, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "LDX", },
Opcode { code: 0xa7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xa8, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TAY", },
Opcode { code: 0xa9, ef: CPU::emu_lda, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "LDA", },
Opcode { code: 0xaa, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TAX", },
Opcode { code: 0xab, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xac, ef: CPU::emu_ldy, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "LDY", },
Opcode { code: 0xad, ef: CPU::emu_lda, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "LDA", },
Opcode { code: 0xae, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "LDX", },
Opcode { code: 0xaf, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb0, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BCS", },
Opcode { code: 0xb1, ef: CPU::emu_lda, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "LDA", },
Opcode { code: 0xb2, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb4, ef: CPU::emu_ldy, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "LDY", },
Opcode { code: 0xb5, ef: CPU::emu_lda, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "LDA", },
Opcode { code: 0xb6, ef: CPU::emu_err, length: 2, mode: Zpy(CPU::get_zpy_add), mnemonic: "LDX", },
Opcode { code: 0xb7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xb8, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "CLV", },
Opcode { code: 0xb9, ef: CPU::emu_lda, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "LDA", },
Opcode { code: 0xba, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "TSX", },
Opcode { code: 0xbb, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xbc, ef: CPU::emu_ldy, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "LDY", },
Opcode { code: 0xbd, ef: CPU::emu_lda, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "LDA", },
Opcode { code: 0xbe, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "LDX", },
Opcode { code: 0xbf, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc0, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "CPY", },
Opcode { code: 0xc1, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "CMP", },
Opcode { code: 0xc2, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc4, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "CPY", },
Opcode { code: 0xc5, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "CMP", },
Opcode { code: 0xc6, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "DEC", },
Opcode { code: 0xc7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xc8, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "INY", },
Opcode { code: 0xc9, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "CMP", },
Opcode { code: 0xca, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "DEX", },
Opcode { code: 0xcb, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xcc, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "CPY", },
Opcode { code: 0xcd, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "CMP", },
Opcode { code: 0xce, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "DEC", },
Opcode { code: 0xcf, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd0, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BNE", },
Opcode { code: 0xd1, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "CMP", },
Opcode { code: 0xd2, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd4, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd5, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "CMP", },
Opcode { code: 0xd6, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "DEC", },
Opcode { code: 0xd7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xd8, ef: CPU::emu_cld, length: 1, mode: Imp, mnemonic: "CLD", },
Opcode { code: 0xd9, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "CMP", },
Opcode { code: 0xda, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdb, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdc, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xdd, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "CMP", },
Opcode { code: 0xde, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "DEC", },
Opcode { code: 0xdf, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe0, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "CPX", },
Opcode { code: 0xe1, ef: CPU::emu_err, length: 2, mode: Inx(CPU::get_inx_add), mnemonic: "SBC", },
Opcode { code: 0xe2, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe4, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "CPX", },
Opcode { code: 0xe5, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "SBC", },
Opcode { code: 0xe6, ef: CPU::emu_err, length: 2, mode: Zpg(CPU::get_zpg_add), mnemonic: "INC", },
Opcode { code: 0xe7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xe8, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "INX", },
Opcode { code: 0xe9, ef: CPU::emu_err, length: 2, mode: Imm(CPU::get_imm_add), mnemonic: "SBC", },
Opcode { code: 0xea, ef: CPU::emu_nop, length: 1, mode: Imp, mnemonic: "NOP", },
Opcode { code: 0xeb, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xec, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "CPX", },
Opcode { code: 0xed, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "SBC", },
Opcode { code: 0xee, ef: CPU::emu_err, length: 3, mode: Abs(CPU::get_abs_add), mnemonic: "INC", },
Opcode { code: 0xef, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf0, ef: CPU::emu_bra, length: 0, mode: Rel, mnemonic: "BEQ", },
Opcode { code: 0xf1, ef: CPU::emu_err, length: 2, mode: Iny(CPU::get_iny_add), mnemonic: "SBC", },
Opcode { code: 0xf2, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf3, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf4, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf5, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "SBC", },
Opcode { code: 0xf6, ef: CPU::emu_err, length: 2, mode: Zpx(CPU::get_zpx_add), mnemonic: "INC", },
Opcode { code: 0xf7, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xf8, ef: CPU::emu_err, length: 1, mode: Imp, mnemonic: "SED", },
Opcode { code: 0xf9, ef: CPU::emu_err, length: 3, mode: Aby(CPU::get_aby_add), mnemonic: "SBC", },
Opcode { code: 0xfa, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfb, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfc, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
Opcode { code: 0xfd, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "SBC", },
Opcode { code: 0xfe, ef: CPU::emu_err, length: 3, mode: Abx(CPU::get_abx_add), mnemonic: "INC", },
Opcode { code: 0xff, ef: CPU::emu_err, length: 0, mode: Unk, mnemonic: "---", },
];
