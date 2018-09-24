use crate::cpu65::Mode::{Abs, Abx, Aby, Acc, Imm, Imp, Ind, Inx, Iny, Rel, Unk, Zpg, Zpx, Zpy};

use std::fmt;
use std::io::{Error, ErrorKind};

const MAX_ADD: usize = 0xffff;
const MEM_SIZE: usize = MAX_ADD + 6;
// registers stored in array
const A_REG: usize = 0x10000;
const X_REG: usize = 0x10001;
const Y_REG: usize = 0x10002;

// so that CPU::load() can return a Result
#[derive(Debug, PartialEq)]
pub enum LoadError {
    SegmentAddress,
}
// required for custom error type
impl From<LoadError> for std::io::Error {
    fn from(e: LoadError) -> std::io::Error {
        match e {
            LoadError::SegmentAddress => Error::new(ErrorKind::Other, "Segment address error!"),
        }
    }
}

// for saving addresses where memory was loaded
pub struct Segment {
    pub start: usize,
    pub end: usize,
}

// 6502 instruction modes
#[derive(Debug, Copy, Clone)]
pub enum Mode {
    Imm, // immediate mode
    Abs, // absolute
    Abx, // absolute x
    Aby, // absolute y
    Inx, // indirect x
    Iny, // indirect y
    Zpg, // zero page
    Zpx, // zero page x
    Zpy, // zero page y
    Acc, // accumulator
    Ind, // indirect
    Rel, // relative
    Imp, // implied
    Unk,
}

// the 6502 status register
enum Flag {
    C = 1 << 0, // carry
    Z = 1 << 1, // zero
    I = 1 << 2, // interrupt
    D = 1 << 3, // decimal
    B = 1 << 4, // break
    // U = 1 << 5, // unused
    V = 1 << 6, // overflow
    N = 1 << 7, // negative
}

/// `CPU` virtual 6502 processor + memory
pub struct CPU {
    /// 6502 status register
    status: u8,
    /// 6502 stack pointer
    sp: u8,
    /// 6502 program counter
    pc: u16,
    /// the 64k memory space
    mem: [u8; MEM_SIZE],
    // 6502 instructions
    // ins: [Instruction; 256],
}

/// Constructs a new `CPU`.
///
/// # Examples
///
/// ```
/// use cpu65::CPU;
///
/// let mut cpu = cpu65::CPU::new();
/// ```
impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ins = INSTRUCTIONS[self.mem[self.pc as usize] as usize];
        let opstr = CPU::get_format(
            ins.mode,
            self.pc as usize,
            &self.mem[(self.pc as usize + 1)..=(self.pc as usize + 2)],
        );
        let operands = match ins.ops {
            2 => format!(
                "{:>02X} {:>02X}",
                self.mem[self.pc as usize + 1],
                self.mem[self.pc as usize + 2],
            ),
            1 => format!("{:>02X}", self.mem[self.pc as usize + 1],),
            _ => "".to_string(),
        };

        write!(
            f,
            "{:>04X} {} {:<10}{:>02X} {:7}|{}| A={:>02X} X={:>02X} Y={:>02X} SP={:>02X}",
            self.pc,
            ins.mnemonic,
            opstr,
            ins.opcode,
            operands,
            status_as_string(self.status),
            self.mem[A_REG],
            self.mem[X_REG],
            self.mem[Y_REG],
            self.sp
        )
    }
}

fn status_as_string(status: u8) -> String {
    let mut s = ['N', 'V', 'U', 'B', 'D', 'I', 'Z', 'C'];

    let mut bit = 1 << 7;
    for i in s.iter_mut() {
        if status & bit == 0 {
            *i = '_';
        }
        bit >>= 1;
    }
    s.iter().collect()
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            // registers a, x, y are stored in extra bytes of memory
            // this makes Addessing modes easier
            // (or could be a huge mistake)
            status: 0,
            sp: 0xff,
            pc: 0,
            mem: [0; MEM_SIZE],
            // ins: INSTRUCTIONS,
        }
    }

    ///////////////////////////////////////////////////
    /// pub methods for accessing the machine state ///
    ///////////////////////////////////////////////////
    pub fn set_pc(&mut self, pc: u16) {
        self.pc = pc;
    }

    pub fn get_pc(&self) -> usize {
        self.pc as usize
    }

    // gets the absolute address of a relative branch
    #[inline(always)]
    pub fn branch_addr(&self, pc: isize) -> usize {
        (self.mem[pc as usize + 1] as i8 as isize + pc as isize + 2) as usize
    }

    // get the indirect 16-bit address as usize
    #[inline(always)]
    pub fn mem_ptr(&self, a: usize) -> usize {
        self.mem[a] as usize | (self.mem[a + 1] as usize) << 8
    }

    // pub fn mem_mut(&mut self) -> &mut [u8] {
    //     &mut self.mem
    // }

    pub fn mem(&self) -> &[u8] {
        &self.mem
    }

    pub fn step(&mut self) {
        let ins = &INSTRUCTIONS[self.mem[self.pc as usize] as usize];
        (ins.ef)(self); // call emu fn
        self.pc += ins.step; // update program counter
    }

    pub fn load(&mut self, buf: &[u8], load_add: Option<usize>) -> Result<Vec<Segment>, LoadError> {
        let start_add: usize;
        let end_add: usize;
        let mut offset: usize;
        let mut segs: Vec<Segment> = Vec::new();

        match load_add {
            Some(add) => {
                let seg_end = add + buf.len() - 1;
                if add > 0xfffe || seg_end <= add || seg_end > 0xffff {
                    return Err(LoadError::SegmentAddress);
                }
                segs.push(Segment {
                    start: add,
                    end: seg_end,
                });
                self.mem[add..=seg_end].clone_from_slice(&buf[0..]);
                return Ok(segs);
            }
            None => {
                offset = 2; // skip the header
                start_add = (buf[offset] as usize) | (buf[offset + 1] as usize) << 8;
                end_add = (buf[offset + 2] as usize) | (buf[offset + 3] as usize) << 8;
            }
        };

        while offset < buf.len() {
            if start_add > 0xfffc || end_add > 0xffff || end_add < start_add + 2 {
                return Err(LoadError::SegmentAddress);
            }

            let seg_beg = offset + 4;
            let seg_end = seg_beg + (end_add - start_add) + 1;
            segs.push(Segment {
                start: start_add,
                end: end_add,
            });
            self.mem[start_add..=end_add].clone_from_slice(&buf[seg_beg..seg_end]);
            offset = seg_end;
        }
        Ok(segs)
    }

    ///////////////////////////
    /// convenience methods ///
    ///////////////////////////
    #[inline(always)]
    fn push_16(&mut self, n: u16) {
        self.mem[0x100 + self.sp as usize - 1] = n as u8;
        self.mem[0x100 + self.sp as usize] = (n >> 8) as u8;
        self.sp = self.sp.wrapping_sub(2);
    }

    #[inline(always)]
    fn pop_16(&mut self) -> u16 {
        self.sp = self.sp.wrapping_add(2);
        u16::from(self.mem[0x100 + self.sp as usize - 1])
            & u16::from(self.mem[0x100 + self.sp as usize]) << 8
    }

    #[inline(always)]
    fn push_8(&mut self, n: u8) {
        self.mem[0x100 + self.sp as usize] = n;
        self.sp = self.sp.wrapping_sub(1);
    }

    #[inline(always)]
    fn pop_8(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.mem[0x100 + self.sp as usize]
    }

    #[inline(always)]
    fn get_mem_16(&self, a: usize) -> u16 {
        u16::from(self.mem[a]) & u16::from(self.mem[a + 1]) << 8
    }

    #[inline(always)]
    fn check_status(&self, r: Flag) -> bool {
        self.status & r as u8 != 0
    }

    #[inline(always)]
    fn get_op_add(&self) -> usize {
        (self.mem[self.pc as usize + 1] as usize) | (self.mem[self.pc as usize + 2] as usize) << 8
    }

    fn get_eff_add(&self) -> usize {
        match INSTRUCTIONS[self.mem[self.pc as usize] as usize].mode {
            Imm => self.pc as usize + 1,
            Zpg => self.mem[self.pc as usize + 1] as usize,
            Abs => {
                (self.mem[self.pc as usize + 1] as usize)
                    | (self.mem[self.pc as usize + 2] as usize) << 8
            }
            Inx => {
                (self.mem[(self.mem[self.pc as usize + 1] + self.mem[X_REG]) as usize] as usize)
                    & (self.mem[(self.mem[self.pc as usize + 1] + self.mem[X_REG]) as usize + 1]
                        as usize)
                        << 8
            }
            Iny => self.mem_ptr(self.mem[self.pc as usize + 1] as usize) + self.mem[Y_REG] as usize,
            Zpx => (self.mem[self.pc as usize + 1]).wrapping_add(self.mem[X_REG]) as usize,
            Zpy => (self.mem[self.pc as usize + 1]).wrapping_add(self.mem[Y_REG]) as usize,
            // page wrapping for abx/y?
            Abx => {
                // let addr = self.get_op_add();
                // addr & 0xff00 | (addr as u8).wrapping_add(self.mem[X_REG]) as usize
                self.get_op_add() + self.mem[X_REG] as usize
            }
            Aby => {
                // let addr = self.get_op_add();
                // addr & 0xff00 | (addr as u8).wrapping_add(self.mem[Y_REG]) as usize
                self.get_op_add() + self.mem[Y_REG] as usize
            }
            Acc => A_REG,
            _ => panic!("Instruction mode doesn't target an Addess!"),
        }
    }

    ///////////////////////////////////////////////////////////////
    ///            instruction emulation functions              ///
    ///////////////////////////////////////////////////////////////

    // unhandled instruction
    pub fn emu_err(&mut self) {
        panic!(format!(
            "Emulation for instruction {} not implemented!",
            INSTRUCTIONS[self.mem[self.pc as usize] as usize].mnemonic
        ));
    }

    // bitwise logic
    fn emu_and(&mut self) {
        self.mem[A_REG] &= self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_bit(&mut self) {
        let target = self.get_eff_add();
        if self.mem[A_REG] & self.mem[target] == 0 {
            self.status |= Flag::Z as u8;
        } else {
            self.status &= !(Flag::Z as u8);
        }
        self.status |= self.mem[target] | 0xC0;
    }

    fn emu_eor(&mut self) {
        self.mem[A_REG] ^= self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_ora(&mut self) {
        self.mem[A_REG] |= self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    // register-to-register transfer operations
    fn emu_tax(&mut self) {
        self.mem[X_REG] = self.mem[A_REG];
        CPU::set_nz_reg(&mut self.status, self.mem[X_REG]);
    }

    fn emu_tay(&mut self) {
        self.mem[Y_REG] = self.mem[A_REG];
        CPU::set_nz_reg(&mut self.status, self.mem[Y_REG]);
    }

    fn emu_txa(&mut self) {
        self.mem[A_REG] = self.mem[X_REG];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_tya(&mut self) {
        self.mem[A_REG] = self.mem[Y_REG];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    // load and store operations
    fn emu_lda(&mut self) {
        self.mem[A_REG] = self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_ldx(&mut self) {
        self.mem[X_REG] = self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[X_REG]);
    }

    fn emu_ldy(&mut self) {
        self.mem[Y_REG] = self.mem[self.get_eff_add()];
        CPU::set_nz_reg(&mut self.status, self.mem[Y_REG]);
    }

    fn emu_sta(&mut self) {
        self.mem[self.get_eff_add()] = self.mem[A_REG];
    }

    fn emu_stx(&mut self) {
        self.mem[self.get_eff_add()] = self.mem[X_REG];
    }

    fn emu_sty(&mut self) {
        self.mem[self.get_eff_add()] = self.mem[Y_REG];
    }

    // shift operations
    fn emu_asl(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr >> 7;
        *addr <<= 1;
    }

    fn emu_lsr(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr & 1; // bit 0 goes into carry bit
        *addr >>= 1;
    }

    fn emu_rol(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr >> 7; // bit 7 goes into carry bit
        *addr <<= 1;
        *addr |= self.status & (Flag::C as u8); // carry goes into bit 0
    }

    fn emu_ror(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr & 1; // bit 0 goes into carry bit
        *addr >>= 1;
        *addr |= self.status & ((Flag::C as u8) << 7); // carry goes into bit 7
    }

    // flow control operations
    fn emu_bra(&mut self) {
        let status = match self.mem[self.pc as usize] {
            0x10 => self.status & Flag::N as u8 == 0,
            0x30 => self.status & Flag::N as u8 != 0,
            0x50 => self.status & Flag::V as u8 == 0,
            0x70 => self.status & Flag::V as u8 != 0,
            0x90 => self.status & Flag::C as u8 == 0,
            0xb0 => self.status & Flag::C as u8 != 0,
            0xd0 => self.status & Flag::Z as u8 == 0,
            0xf0 => self.status & Flag::Z as u8 != 0,
            // should never happen
            _ => panic!("Encountered unknown branch opcode!"),
        };
        if status {
            self.pc =
                2 + ((self.pc as i16) + i16::from(self.mem[self.pc as usize + 1] as i8)) as u16;
        } else {
            self.pc += 2;
        }
    }

    fn emu_jmp(&mut self) {
        self.pc = u16::from(self.mem[self.pc as usize + 1])
            | u16::from(self.mem[self.pc as usize + 2]) << 8
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

    // increment and decrement operations
    fn emu_dec(&mut self) {
        let target = self.get_eff_add();
        self.mem[target] = self.mem[target].wrapping_sub(1);
        CPU::set_nz_reg(&mut self.status, self.mem[target]);
    }

    fn emu_dex(&mut self) {
        self.mem[X_REG] = self.mem[X_REG].wrapping_sub(1);
        CPU::set_nz_reg(&mut self.status, self.mem[X_REG]);
    }

    fn emu_dey(&mut self) {
        self.mem[Y_REG] = self.mem[Y_REG].wrapping_sub(1);
        CPU::set_nz_reg(&mut self.status, self.mem[Y_REG]);
    }

    fn emu_inc(&mut self) {
        let target = self.get_eff_add();
        self.mem[target] = self.mem[target].wrapping_sub(1);
        CPU::set_nz_reg(&mut self.status, self.mem[target]);
    }

    fn emu_inx(&mut self) {
        self.mem[X_REG] = self.mem[X_REG].wrapping_add(1);
        CPU::set_nz_reg(&mut self.status, self.mem[X_REG]);
    }

    fn emu_iny(&mut self) {
        self.mem[Y_REG] = self.mem[Y_REG].wrapping_add(1);
        CPU::set_nz_reg(&mut self.status, self.mem[Y_REG]);
    }

    // addition and subtraction
    fn emu_adc(&mut self) {
        //fix me check adc and sbc for correctness
        let target = self.get_eff_add();
        let r = self.mem[A_REG]
            .overflowing_add(self.mem[target] + if self.check_status(Flag::C) { 1 } else { 0 });
        // set carry if wraps
        if r.1 {
            self.status |= Flag::C as u8
        } else {
            self.status &= !(Flag::C as u8)
        }
        // set overflow
        if (self.mem[A_REG] ^ r.0) & (self.mem[target] ^ r.0) & 0x80 != 0 {
            // if (r.0 ^ self.mem[A_REG]) & 0x80 != 0 {
            self.status |= Flag::V as u8
        } else {
            self.status &= !(Flag::V as u8)
        }
        self.mem[A_REG] = r.0;
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_sbc(&mut self) {
        let target = self.get_eff_add();
        let r = self.mem[A_REG]
            .overflowing_sub(self.mem[target] + if self.check_status(Flag::C) { 0 } else { 1 });
        // set carry if wraps
        if r.1 {
            self.status |= Flag::C as u8
        } else {
            self.status &= !(Flag::C as u8)
        }
        // set overflow
        if (self.mem[A_REG] ^ r.0) & ((self.mem[target] ^ 0xff) ^ r.0) & 0x80 != 0 {
            self.status |= Flag::V as u8
        } else {
            self.status &= !(Flag::V as u8)
        }
        self.mem[A_REG] = r.0;
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    // comparison operations
    fn emu_cmp(&mut self) {
        let r: i8 = self.mem[A_REG] as i8 - self.mem[self.get_eff_add()] as i8;
        CPU::set_nz_reg(&mut self.status, r as u8);
        if r >= 0 {
            self.status |= Flag::C as u8
        } else {
            self.status &= !(Flag::C as u8)
        }
    }

    fn emu_cpx(&mut self) {
        let r: i8 = self.mem[X_REG] as i8 - self.mem[self.get_eff_add()] as i8;
        CPU::set_nz_reg(&mut self.status, r as u8);
        if r >= 0 {
            self.status |= Flag::C as u8
        } else {
            self.status &= !(Flag::C as u8)
        }
    }

    fn emu_cpy(&mut self) {
        let r: i8 = self.mem[Y_REG] as i8 - self.mem[self.get_eff_add()] as i8;
        CPU::set_nz_reg(&mut self.status, r as u8);
        if r >= 0 {
            self.status |= Flag::C as u8
        } else {
            self.status &= !(Flag::C as u8)
        }
    }

    // stack operations
    fn emu_pha(&mut self) {
        self.mem[0x100 + self.sp as usize] = self.mem[A_REG];
        self.sp -= 1;
    }

    fn emu_php(&mut self) {
        self.mem[0x100 + self.sp as usize] = self.status;
        self.sp -= 1;
    }

    fn emu_pla(&mut self) {
        self.sp += 1;
        self.mem[A_REG] = self.mem[0x100 + self.sp as usize];
        // let r = self.mem[A_REG];
        CPU::set_nz_reg(&mut self.status, self.mem[A_REG]);
    }

    fn emu_plp(&mut self) {
        self.sp += 1;
        self.status = self.mem[0x100 + self.sp as usize];
    }

    fn emu_tsx(&mut self) {
        self.mem[X_REG] = self.sp;
        CPU::set_nz_reg(&mut self.status, self.mem[X_REG]);
    }

    fn emu_txs(&mut self) {
        self.mem[X_REG] = self.sp;
    }

    // status register operations
    fn emu_clc(&mut self) {
        self.status &= !(Flag::C as u8);
    }

    fn emu_cld(&mut self) {
        self.status &= !(Flag::D as u8);
    }

    fn emu_cli(&mut self) {
        self.status &= !(Flag::I as u8);
    }

    fn emu_clv(&mut self) {
        self.status &= !(Flag::V as u8);
    }

    fn emu_sec(&mut self) {
        self.status |= Flag::C as u8;
    }

    fn emu_sed(&mut self) {
        self.status |= Flag::D as u8;
    }

    fn emu_sei(&mut self) {
        self.status |= Flag::I as u8;
    }

    // interrupt related
    fn emu_rti(&mut self) {
        self.status = self.pop_8();
        self.pc = self.pop_16();
    }

    fn emu_brk(&mut self) {
        // let pc = self.pc as u16;
        // let st = self.status;
        self.push_16(self.pc);
        self.push_8(self.status);
        self.pc = self.get_mem_16(0xfffe);
        self.status |= Flag::B as u8;
    }

    // nope
    fn emu_nop(&mut self) {}
}

impl CPU {
    // formatting for printing an instruction
    pub fn get_format(m: Mode, addr: usize, ops: &[u8]) -> String {
        match m {
            Imm => format!("#${:>02X}", ops[0]),
            Zpg => format!("${:>02X}", ops[0]),
            Zpx => format!("${:>02X},X", ops[0]),
            Zpy => format!("${:>02X},Y", ops[0]),
            Abs => format!("${:>02X}{:>02X}", ops[1], ops[0]),
            Abx => format!("${:>02X}{:>02X},X", ops[1], ops[0]),
            Aby => format!("${:>02X}{:>02X},Y", ops[1], ops[0]),
            Ind => format!("(${:>02X}{:>02X})", ops[1], ops[0]),
            Inx => format!("(${:>02X},X)", ops[0]),
            Iny => format!("(${:>02X}),Y", ops[0]),
            Acc => "A".to_string(),
            Rel => format!(
                "${:>04X}",
                (ops[0] as i8 as isize + addr as isize + 2) as usize
            ),
            Imp => "".to_string(),
            Unk => "---".to_string(),
        }
    }

    #[inline(always)]
    fn set_nz_reg(status: &mut u8, r: u8) {
        if r == 0 {
            *status |= Flag::Z as u8;
            *status &= !(Flag::N as u8);
        } else {
            *status &= !(Flag::Z as u8);
            *status = *status & 0x7f | (r & 0x80);
        }
    }
}

#[derive(Copy, Clone)]
pub struct Instruction {
    pub opcode: i32,      // the opcode
    pub ef: fn(&mut CPU), // the thing that gets the result
    pub step: u16,        // how much to move the pc after this instruction
    pub ops: i32,
    pub mode: Mode,
    pub isbr: bool,
    pub mnemonic: &'static str,
}

// use cpu65::Modes::*;
#[cfg_attr(rustfmt, rustfmt_skip)]
pub const INSTRUCTIONS: [Instruction; 256] = [
Instruction { opcode: 0x00, ef: CPU::emu_brk, step: 0, ops: 0, mode: Imp, isbr: false, mnemonic: "BRK", },
Instruction { opcode: 0x01, ef: CPU::emu_ora, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x02, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x03, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x04, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x05, ef: CPU::emu_ora, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x06, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ASL", },
Instruction { opcode: 0x07, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x08, ef: CPU::emu_php, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PHP", },
Instruction { opcode: 0x09, ef: CPU::emu_ora, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x0a, ef: CPU::emu_asl, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ASL", },
Instruction { opcode: 0x0b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x0c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x0d, ef: CPU::emu_ora, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x0e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ASL", },
Instruction { opcode: 0x0f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x10, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BPL", },
Instruction { opcode: 0x11, ef: CPU::emu_ora, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x12, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x13, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x14, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x15, ef: CPU::emu_ora, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x16, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ASL", },
Instruction { opcode: 0x17, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x18, ef: CPU::emu_clc, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLC", },
Instruction { opcode: 0x19, ef: CPU::emu_ora, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x1a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x1b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x1c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x1d, ef: CPU::emu_ora, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ORA", },
Instruction { opcode: 0x1e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ASL", },
Instruction { opcode: 0x1f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x20, ef: CPU::emu_jsr, step: 0, ops: 2, mode: Abs, isbr: true,  mnemonic: "JSR", },
Instruction { opcode: 0x21, ef: CPU::emu_and, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x22, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x23, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x24, ef: CPU::emu_bit, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "BIT", },
Instruction { opcode: 0x25, ef: CPU::emu_and, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x26, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ROL", },
Instruction { opcode: 0x27, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x28, ef: CPU::emu_plp, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PLP", },
Instruction { opcode: 0x29, ef: CPU::emu_and, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x2a, ef: CPU::emu_rol, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ROL", },
Instruction { opcode: 0x2b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x2c, ef: CPU::emu_bit, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "BIT", },
Instruction { opcode: 0x2d, ef: CPU::emu_and, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x2e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ROL", },
Instruction { opcode: 0x2f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x30, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BMI", },
Instruction { opcode: 0x31, ef: CPU::emu_and, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x32, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x33, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x34, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x35, ef: CPU::emu_and, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x36, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ROL", },
Instruction { opcode: 0x37, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x38, ef: CPU::emu_sec, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SEC", },
Instruction { opcode: 0x39, ef: CPU::emu_and, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x3a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x3b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x3c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x3d, ef: CPU::emu_and, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "AND", },
Instruction { opcode: 0x3e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ROL", },
Instruction { opcode: 0x3f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x40, ef: CPU::emu_rti, step: 0, ops: 0, mode: Imp, isbr: false, mnemonic: "RTI", },
Instruction { opcode: 0x41, ef: CPU::emu_eor, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x42, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x43, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x44, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x45, ef: CPU::emu_eor, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x46, ef: CPU::emu_lsr, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LSR", },
Instruction { opcode: 0x47, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x48, ef: CPU::emu_pha, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PHA", },
Instruction { opcode: 0x49, ef: CPU::emu_eor, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x4a, ef: CPU::emu_lsr, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "LSR", },
Instruction { opcode: 0x4b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x4c, ef: CPU::emu_jmp, step: 0, ops: 2, mode: Abs, isbr: true,  mnemonic: "JMP", },
Instruction { opcode: 0x4d, ef: CPU::emu_eor, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x4e, ef: CPU::emu_lsr, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LSR", },
Instruction { opcode: 0x4f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x50, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BVC", },
Instruction { opcode: 0x51, ef: CPU::emu_eor, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x52, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x53, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x54, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x55, ef: CPU::emu_eor, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x56, ef: CPU::emu_lsr, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LSR", },
Instruction { opcode: 0x57, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x58, ef: CPU::emu_cli, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLI", },
Instruction { opcode: 0x59, ef: CPU::emu_eor, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x5a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x5b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x5c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x5d, ef: CPU::emu_eor, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "EOR", },
Instruction { opcode: 0x5e, ef: CPU::emu_lsr, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LSR", },
Instruction { opcode: 0x5f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x60, ef: CPU::emu_rts, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "RTS", },
Instruction { opcode: 0x61, ef: CPU::emu_adc, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x62, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x63, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x64, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x65, ef: CPU::emu_adc, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x66, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ROR", },
Instruction { opcode: 0x67, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x68, ef: CPU::emu_pla, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PLA", },
Instruction { opcode: 0x69, ef: CPU::emu_adc, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x6a, ef: CPU::emu_ror, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ROR", },
Instruction { opcode: 0x6b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x6c, ef: CPU::emu_err, step: 0, ops: 2, mode: Ind, isbr: true,  mnemonic: "JMP", },
Instruction { opcode: 0x6d, ef: CPU::emu_adc, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x6e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ROR", },
Instruction { opcode: 0x6f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x70, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BVS", },
Instruction { opcode: 0x71, ef: CPU::emu_adc, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x72, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x73, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x74, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x75, ef: CPU::emu_adc, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x76, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ROR", },
Instruction { opcode: 0x77, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x78, ef: CPU::emu_sei, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SEI", },
Instruction { opcode: 0x79, ef: CPU::emu_adc, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x7a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x7b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x7c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x7d, ef: CPU::emu_adc, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ADC", },
Instruction { opcode: 0x7e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ROR", },
Instruction { opcode: 0x7f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x80, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x81, ef: CPU::emu_sta, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x82, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x83, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x84, ef: CPU::emu_sty, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STY", },
Instruction { opcode: 0x85, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x86, ef: CPU::emu_stx, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STX", },
Instruction { opcode: 0x87, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x88, ef: CPU::emu_dey, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "DEY", },
Instruction { opcode: 0x89, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x8a, ef: CPU::emu_txa, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TXA", },
Instruction { opcode: 0x8b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x8c, ef: CPU::emu_sty, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STY", },
Instruction { opcode: 0x8d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x8e, ef: CPU::emu_stx, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STX", },
Instruction { opcode: 0x8f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x90, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BCC", },
Instruction { opcode: 0x91, ef: CPU::emu_sta, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x92, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x93, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x94, ef: CPU::emu_sty, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "STY", },
Instruction { opcode: 0x95, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x96, ef: CPU::emu_stx, step: 2, ops: 1, mode: Zpy, isbr: false, mnemonic: "STX", },
Instruction { opcode: 0x97, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x98, ef: CPU::emu_tya, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TYA", },
Instruction { opcode: 0x99, ef: CPU::emu_sta, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x9a, ef: CPU::emu_txs, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TXS", },
Instruction { opcode: 0x9b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x9c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x9d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "STA", },
Instruction { opcode: 0x9e, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0x9f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xa0, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDY", },
Instruction { opcode: 0xa1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xa2, ef: CPU::emu_ldx, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDX", },
Instruction { opcode: 0xa3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xa4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDY", },
Instruction { opcode: 0xa5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xa6, ef: CPU::emu_ldx, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDX", },
Instruction { opcode: 0xa7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xa8, ef: CPU::emu_tay, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TAY", },
Instruction { opcode: 0xa9, ef: CPU::emu_lda, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xaa, ef: CPU::emu_tax, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TAX", },
Instruction { opcode: 0xab, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xac, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDY", },
Instruction { opcode: 0xad, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xae, ef: CPU::emu_ldx, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDX", },
Instruction { opcode: 0xaf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xb0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BCS", },
Instruction { opcode: 0xb1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xb2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xb3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xb4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LDY", },
Instruction { opcode: 0xb5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xb6, ef: CPU::emu_ldx, step: 2, ops: 1, mode: Zpy, isbr: false, mnemonic: "LDX", },
Instruction { opcode: 0xb7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xb8, ef: CPU::emu_clv, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLV", },
Instruction { opcode: 0xb9, ef: CPU::emu_lda, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xba, ef: CPU::emu_tsx, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TSX", },
Instruction { opcode: 0xbb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xbc, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LDY", },
Instruction { opcode: 0xbd, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LDA", },
Instruction { opcode: 0xbe, ef: CPU::emu_ldx, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "LDX", },
Instruction { opcode: 0xbf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xc0, ef: CPU::emu_cpy, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CPY", },
Instruction { opcode: 0xc1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xc2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xc3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xc4, ef: CPU::emu_cpy, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CPY", },
Instruction { opcode: 0xc5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xc6, ef: CPU::emu_dec, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "DEC", },
Instruction { opcode: 0xc7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xc8, ef: CPU::emu_iny, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "INY", },
Instruction { opcode: 0xc9, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xca, ef: CPU::emu_dex, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "DEX", },
Instruction { opcode: 0xcb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xcc, ef: CPU::emu_cpy, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CPY", },
Instruction { opcode: 0xcd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xce, ef: CPU::emu_dec, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "DEC", },
Instruction { opcode: 0xcf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xd0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BNE", },
Instruction { opcode: 0xd1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xd2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xd3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xd4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xd5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xd6, ef: CPU::emu_dec, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "DEC", },
Instruction { opcode: 0xd7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xd8, ef: CPU::emu_cld, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLD", },
Instruction { opcode: 0xd9, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xda, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xdb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xdc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xdd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "CMP", },
Instruction { opcode: 0xde, ef: CPU::emu_dec, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "DEC", },
Instruction { opcode: 0xdf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xe0, ef: CPU::emu_cpx, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CPX", },
Instruction { opcode: 0xe1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xe2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xe3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xe4, ef: CPU::emu_cpx, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CPX", },
Instruction { opcode: 0xe5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xe6, ef: CPU::emu_inc, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "INC", },
Instruction { opcode: 0xe7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xe8, ef: CPU::emu_inx, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "INX", },
Instruction { opcode: 0xe9, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xea, ef: CPU::emu_nop, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "NOP", },
Instruction { opcode: 0xeb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xec, ef: CPU::emu_cpx, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CPX", },
Instruction { opcode: 0xed, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xee, ef: CPU::emu_inc, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "INC", },
Instruction { opcode: 0xef, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xf0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true,  mnemonic: "BEQ", },
Instruction { opcode: 0xf1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xf2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xf3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xf4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xf5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xf6, ef: CPU::emu_inc, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "INC", },
Instruction { opcode: 0xf7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xf8, ef: CPU::emu_sed, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SED", },
Instruction { opcode: 0xf9, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xfa, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xfb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xfc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Instruction { opcode: 0xfd, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "SBC", },
Instruction { opcode: 0xfe, ef: CPU::emu_inc, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "INC", },
Instruction { opcode: 0xff, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
];
