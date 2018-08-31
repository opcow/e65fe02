// use crate::cpu65::emu::EMU_FUNCS;
//use crate::cpu65::emu::EMU_FUNCS;
use crate::cpu65::Modes::*;
use crate::cpu65::Op::*;
use std::collections::HashMap;
use std::sync::Mutex;

// #[macro_use]
// extern crate lazy_static;

lazy_static! {
    static ref BRAMAP: Mutex<HashMap<usize, String>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };

    // static ref HASHMAP: Mutex<HashMap<u32, &'static str>> = {
    //     let mut m = HashMap::new();
    //     m.insert(0, "foo");
    //     m.insert(1, "bar");
    //     m.insert(2, "baz");
    //     Mutex::new(m)
    // };
}

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

const MAX_ADD: usize = 0xffff;
const MEM_SIZE: usize = MAX_ADD + 6; // registers stored in array
const A_REG: usize = 0x10000;
const X_REG: usize = 0x10001;
const Y_REG: usize = 0x10002;

pub enum Op {
    Op16(u16),
    Op8(u8),
    Noop,
}

pub struct TraceData {
    a: u8,
    x: u8,
    y: u8,
    pc: usize,
    st: u8,
    sp: u8,
    oc: i32,
    opl: u8,
    oph: u8,
    op: Op,
    opstr: String,
    mode: String,
    instruction: String,
    status: String,
    label: String,
    has_first_pass: bool,
}

impl TraceData {
    fn new() -> TraceData {
        TraceData {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            st: 0,
            sp: 0,
            oc: 0,
            opl: 0,
            oph: 0,
            op: Op::Noop,
            opstr: String::from("$00"),
            mode: String::from("ERR"),
            instruction: String::from("ERR"),
            status: String::from("--------"),
            label: String::from(""),
            has_first_pass: false,
        }
    }
}

struct Segment {
    start: usize,
    end:   usize,
}

//#[derive(Debug)]
#[derive(Copy, Clone)]
pub enum Modes {
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
    // Tst(fn(&mut CPU) -> &mut u8),
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
    segs:   Vec<Segment>,
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
            segs:   Vec::new(),
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

    #[inline(always)]
    fn get_op_add(&self) -> usize {
        (self.mem[self.pc as usize + 1] as usize) | (self.mem[self.pc as usize + 2] as usize) << 8
    }
    #[inline(always)]
    fn get_op_16(&self) -> u16 {
        u16::from(self.mem[self.pc as usize + 1]) | u16::from(self.mem[self.pc as usize + 2]) << 8
    }
    #[inline(always)]
    fn get_br_addr(&self, pc: isize) -> usize {
        (self.mem[pc as usize + 1] as i8 as isize + pc as isize + 2) as usize
    }
    #[inline(always)]
    fn get_op_u8(&self) -> u8 {
        self.mem[self.pc as usize + 1]
    }
    #[inline(always)]
    fn get_mem_16(&self, a: usize) -> u16 {
        // let m16 = |m: &[u8], i: usize| (m[i] as usize) & (m[i + 1] as usize) << 8;
        u16::from(self.mem[a]) & u16::from(self.mem[a + 1]) << 8
    }

    //#[inline(always)]
    fn get_mem_usize(&self, a: usize) -> usize {
        // let m16 = |m: &[u8], i: usize| (m[i] as usize) & (m[i + 1] as usize) << 8;
        self.mem[a] as usize | (self.mem[a + 1] as usize) << 8
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
            self.segs.push(Segment {
                start: start_add,
                end:   end_add,
            });
            self.mem[start_add..=end_add].clone_from_slice(&buf[seg_beg..seg_end]);
            offset = seg_end;
        }
        true
    }

    fn pass1(&self) {
        let mut pc: usize;
        let mut oc: &Opcode;
        let mut dest: usize;
        let mut b = BRAMAP.lock().unwrap();

        for seg in &self.segs {
            pc = seg.start;
            while pc <= seg.end {
                oc = &OPCODES[self.mem[pc] as usize];
                if oc.isbr {
                    match oc.mode {
                        Rel => {
                            dest = self.get_br_addr(pc as isize);
                            b.insert(dest, format!("LOC{:04X}", dest));
                        }
                        _ => {
                            dest = self.get_mem_usize(pc + 1);
                            b.insert(dest, format!("LOC{:04X}", dest));
                        }
                    }
                }
                pc += oc.ops as usize + 1;
            }
        }
    }

    pub fn disasm(&self) {
        let mut pc: usize;
        let mut oc: &Opcode;
        let mut dest: usize;
        let defstr = String::from("");

        println!(";;; begin disassembley ;;;\n");
        println!("    PROCESSOR 6502");
        println!("    LIST ON\n");

        let b = BRAMAP.lock().unwrap();

        for seg in &self.segs {
            println!("        .ORG ${:04X}", seg.start);
            pc = seg.start;
            while pc <= seg.end {
                oc = &OPCODES[self.mem[pc] as usize];
                let l = b.get(&pc).unwrap_or(&defstr);
                if oc.isbr {
                    match oc.mode {
                        Rel => dest = self.get_br_addr(pc as isize),
                        _ => dest = self.get_mem_usize(pc + 1),
                    }
                    println!("{:7}   {} LOC{:04X}", l, oc.mnemonic, dest);
                } else {
                    println!(
                        "{:7}   {} {}",
                        l,
                        oc.mnemonic,
                        get_format(
                            oc.mode,
                            pc,
                            &self.mem[(pc as usize + 1)..=(pc as usize + 2)],
                        )
                    );
                }
                pc += oc.ops as usize + 1;
            }
            print!("\n");
        }
    }

    pub fn trace(&mut self, start: u16, count: u32) {
        let mut td = TraceData::new();
        let defstr = String::from("");

        if !td.has_first_pass {
            self.pass1()
        }
        let b = BRAMAP.lock().unwrap();

        print!(";;;;;; begin trace ;;;;;;\n\n");

        use std::{thread, time};
        let delay = time::Duration::from_millis(200);

        self.set_pc(start);
        for i in 1..=count {
            self.trace_step(&mut td);
            if i != 2 && td.pc == 0x8001 {
                break;
            }

            let l = b.get(&td.pc).unwrap_or(&defstr);

            match td.op {
            Op16(_o) => println!(
                "{:8} {:04}:{:>04X} {} {:<10}{:>02X} {:>02X} {:>02X}  |{}| A={:>02X} X={:>02X} Y={:>02X}",
                l, i, td.pc, td.instruction, td.opstr, td.oc, td.opl, td.oph, td.status, td.a, td.x, td.y
            ),
            _ => println!(
                "{:8} {:04}:{:>04X} {} {:<10}{:>02X} {:>02X}     |{}| A={:>02X} X={:>02X} Y={:>02X}",
                l, i, td.pc, td.instruction, td.opstr, td.oc, td.opl, td.status, td.a, td.x, td.y
            ),
        };
            thread::sleep(delay);
        }
    }

    pub fn trace_step(&mut self, td: &mut TraceData) {
        let s1 = ['-', '-', '-', '-', '-', '-', '-', '-'];
        let s2 = ['N', 'V', 'U', 'B', 'D', 'I', 'Z', 'C'];

        let oc = &OPCODES[self.mem[self.pc as usize] as usize];

        td.pc = self.pc as usize;
        td.st = self.status;
        td.a = self.mem[A_REG];
        td.x = self.mem[X_REG];
        td.y = self.mem[Y_REG];

        let mut bit = 1 << 7;
        td.status.clear();
        for i in 0..8 {
            if td.st & bit != 0 {
                td.status.push(s2[i]);
            } else {
                td.status.push(s1[i]);
            }
            bit >>= 1;
        }
        td.oc = oc.code;
        td.opl = self.mem[self.pc as usize + 1];
        td.oph = self.mem[self.pc as usize + 2];
        match oc.ops {
            2 => td.op = Op::Op16(self.get_op_16()),
            1 => td.op = Op::Op8(self.get_op_u8()),
            _ => td.op = Op::Noop,
        };

        td.opstr = get_format(
            oc.mode,
            self.pc as usize,
            &self.mem[(self.pc as usize + 1)..=(self.pc as usize + 2)],
        );

        td.mode = String::from(match oc.mode {
            Imm => "IMM",
            Zpg => "ZPG",
            Abs => "ABS",
            Inx => "INX",
            Iny => "INY",
            Zpx => "ZPX",
            Zpy => "ZPY",
            Abx => "ABX",
            Aby => "ABY",
            Acc => "ACC",
            _ => "ERR",
        });

        td.instruction = String::from(oc.mnemonic);

        (oc.ef)(self); // call emu fn
        self.pc += oc.step;
    }

    pub fn step(&mut self) {
        let oc = &OPCODES[self.mem[self.pc as usize] as usize];
        (oc.ef)(self); // call emu fn
        self.pc += oc.step;
    }

    fn get_eff_add(&self) -> usize {
        // fix me zero page wrapping
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

    fn emu_sed(&mut self) {
        self.status |= 1 << (Status::D as u8);
    }

    fn emu_clc(&mut self) {
        self.status &= !(1 << (Status::C as u8));
    }

    fn emu_sec(&mut self) {
        self.status |= 1 << (Status::C as u8);
    }

    fn emu_clv(&mut self) {
        self.status &= !(1 << (Status::V as u8));
    }

    fn emu_sev(&mut self) {
        self.status |= 1 << (Status::V as u8);
    }

    fn emu_cli(&mut self) {
        self.status &= !(1 << (Status::I as u8));
    }

    fn emu_sei(&mut self) {
        self.status |= 1 << (Status::I as u8);
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
        self.status |= *addr >> 7;
        *addr <<= 1;
    }

    fn emu_ror(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr & 1; // bit 0 goes into carry bit
        *addr >>= 1;
        *addr |= self.status & ((Status::C as u8) << 7); // carry goes into bit 7
    }

    fn emu_rol(&mut self) {
        let addr = &mut self.mem[self.get_eff_add()];
        self.status |= *addr >> 7; // bit 0 goes into carry bit
        *addr <<= 1;
        *addr |= self.status & (Status::C as u8); // carry goes into bit 0
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
    }
}

#[derive(Copy, Clone)]
pub struct Opcode {
    pub code:     i32,          // the opcode
    pub ef:       fn(&mut CPU), // the thing that gets the result
    pub step:     u16,
    pub ops:      i32,
    pub mode:     Modes,
    pub isbr:     bool,
    pub mnemonic: &'static str,
}

fn get_format(m: Modes, addr: usize, ops: &[u8]) -> String {
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

// use cpu65::Modes::*;
#[cfg_attr(rustfmt, rustfmt_skip)]
pub const OPCODES: [Opcode; 256] = [
Opcode { code: 0x00, ef: CPU::emu_brk, step: 0, ops: 0, mode: Imp, isbr: false, mnemonic: "BRK", },
Opcode { code: 0x01, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x02, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x03, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x04, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x05, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x06, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ASL", },
Opcode { code: 0x07, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x08, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PHP", },
Opcode { code: 0x09, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x0a, ef: CPU::emu_asl, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ASL", },
Opcode { code: 0x0b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x0c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x0d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x0e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ASL", },
Opcode { code: 0x0f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x10, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BPL", },
Opcode { code: 0x11, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x12, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x13, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x14, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x15, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x16, ef: CPU::emu_asl, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ASL", },
Opcode { code: 0x17, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x18, ef: CPU::emu_clc, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLC", },
Opcode { code: 0x19, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x1a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x1b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x1c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x1d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ORA", },
Opcode { code: 0x1e, ef: CPU::emu_asl, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ASL", },
Opcode { code: 0x1f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x20, ef: CPU::emu_jsr, step: 0, ops: 2, mode: Abs, isbr: true, mnemonic: "JSR", },
Opcode { code: 0x21, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "AND", },
Opcode { code: 0x22, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x23, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x24, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "BIT", },
Opcode { code: 0x25, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "AND", },
Opcode { code: 0x26, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ROL", },
Opcode { code: 0x27, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x28, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PLP", },
Opcode { code: 0x29, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "AND", },
Opcode { code: 0x2a, ef: CPU::emu_rol, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ROL", },
Opcode { code: 0x2b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x2c, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "BIT", },
Opcode { code: 0x2d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "AND", },
Opcode { code: 0x2e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ROL", },
Opcode { code: 0x2f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x30, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BMI", },
Opcode { code: 0x31, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "AND", },
Opcode { code: 0x32, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x33, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x34, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x35, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "AND", },
Opcode { code: 0x36, ef: CPU::emu_rol, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ROL", },
Opcode { code: 0x37, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x38, ef: CPU::emu_sec, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SEC", },
Opcode { code: 0x39, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "AND", },
Opcode { code: 0x3a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x3b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x3c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x3d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "AND", },
Opcode { code: 0x3e, ef: CPU::emu_rol, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ROL", },
Opcode { code: 0x3f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x40, ef: CPU::emu_rti, step: 0, ops: 0, mode: Imp, isbr: false, mnemonic: "RTI", },
Opcode { code: 0x41, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x42, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x43, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x44, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x45, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x46, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LSR", },
Opcode { code: 0x47, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x48, ef: CPU::emu_pha, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PHA", },
Opcode { code: 0x49, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x4a, ef: CPU::emu_err, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "LSR", },
Opcode { code: 0x4b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x4c, ef: CPU::emu_err, step: 0, ops: 2, mode: Abs, isbr: true, mnemonic: "JMP", },
Opcode { code: 0x4d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x4e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LSR", },
Opcode { code: 0x4f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x50, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BVC", },
Opcode { code: 0x51, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x52, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x53, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x54, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x55, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x56, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LSR", },
Opcode { code: 0x57, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x58, ef: CPU::emu_cli, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLI", },
Opcode { code: 0x59, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x5a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x5b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x5c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x5d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "EOR", },
Opcode { code: 0x5e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LSR", },
Opcode { code: 0x5f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x60, ef: CPU::emu_rts, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "RTS", },
Opcode { code: 0x61, ef: CPU::emu_err, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x62, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x63, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x64, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x65, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x66, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "ROR", },
Opcode { code: 0x67, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x68, ef: CPU::emu_pla, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "PLA", },
Opcode { code: 0x69, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x6a, ef: CPU::emu_ror, step: 1, ops: 0, mode: Acc, isbr: false, mnemonic: "ROR", },
Opcode { code: 0x6b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x6c, ef: CPU::emu_err, step: 0, ops: 2, mode: Ind, isbr: true, mnemonic: "JMP", },
Opcode { code: 0x6d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x6e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "ROR", },
Opcode { code: 0x6f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x70, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BVS", },
Opcode { code: 0x71, ef: CPU::emu_err, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x72, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x73, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x74, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x75, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x76, ef: CPU::emu_ror, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "ROR", },
Opcode { code: 0x77, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x78, ef: CPU::emu_sei, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SEI", },
Opcode { code: 0x79, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x7a, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x7b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x7c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x7d, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ADC", },
Opcode { code: 0x7e, ef: CPU::emu_ror, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "ROR", },
Opcode { code: 0x7f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x80, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x81, ef: CPU::emu_sta, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "STA", },
Opcode { code: 0x82, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x83, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x84, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STY", },
Opcode { code: 0x85, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STA", },
Opcode { code: 0x86, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "STX", },
Opcode { code: 0x87, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x88, ef: CPU::emu_dey, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "DEY", },
Opcode { code: 0x89, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x8a, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TXA", },
Opcode { code: 0x8b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x8c, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STY", },
Opcode { code: 0x8d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STA", },
Opcode { code: 0x8e, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "STX", },
Opcode { code: 0x8f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x90, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BCC", },
Opcode { code: 0x91, ef: CPU::emu_sta, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "STA", },
Opcode { code: 0x92, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x93, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x94, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "STY", },
Opcode { code: 0x95, ef: CPU::emu_sta, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "STA", },
Opcode { code: 0x96, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpy, isbr: false, mnemonic: "STX", },
Opcode { code: 0x97, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x98, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TYA", },
Opcode { code: 0x99, ef: CPU::emu_sta, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "STA", },
Opcode { code: 0x9a, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TXS", },
Opcode { code: 0x9b, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x9c, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x9d, ef: CPU::emu_sta, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "STA", },
Opcode { code: 0x9e, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0x9f, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xa0, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDY", },
Opcode { code: 0xa1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xa2, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDX", },
Opcode { code: 0xa3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xa4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDY", },
Opcode { code: 0xa5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xa6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "LDX", },
Opcode { code: 0xa7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xa8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TAY", },
Opcode { code: 0xa9, ef: CPU::emu_lda, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xaa, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TAX", },
Opcode { code: 0xab, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xac, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDY", },
Opcode { code: 0xad, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xae, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "LDX", },
Opcode { code: 0xaf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xb0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BCS", },
Opcode { code: 0xb1, ef: CPU::emu_lda, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xb2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xb3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xb4, ef: CPU::emu_ldy, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LDY", },
Opcode { code: 0xb5, ef: CPU::emu_lda, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xb6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpy, isbr: false, mnemonic: "LDX", },
Opcode { code: 0xb7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xb8, ef: CPU::emu_clv, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLV", },
Opcode { code: 0xb9, ef: CPU::emu_lda, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xba, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "TSX", },
Opcode { code: 0xbb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xbc, ef: CPU::emu_ldy, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LDY", },
Opcode { code: 0xbd, ef: CPU::emu_lda, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "LDA", },
Opcode { code: 0xbe, ef: CPU::emu_err, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "LDX", },
Opcode { code: 0xbf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xc0, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CPY", },
Opcode { code: 0xc1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xc2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xc3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xc4, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CPY", },
Opcode { code: 0xc5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xc6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "DEC", },
Opcode { code: 0xc7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xc8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "INY", },
Opcode { code: 0xc9, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xca, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "DEX", },
Opcode { code: 0xcb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xcc, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CPY", },
Opcode { code: 0xcd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xce, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "DEC", },
Opcode { code: 0xcf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xd0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BNE", },
Opcode { code: 0xd1, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xd2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xd3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xd4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xd5, ef: CPU::emu_cmp, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xd6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "DEC", },
Opcode { code: 0xd7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xd8, ef: CPU::emu_cld, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "CLD", },
Opcode { code: 0xd9, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xda, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xdb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xdc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xdd, ef: CPU::emu_cmp, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "CMP", },
Opcode { code: 0xde, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "DEC", },
Opcode { code: 0xdf, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xe0, ef: CPU::emu_err, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "CPX", },
Opcode { code: 0xe1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Inx, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xe2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xe3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xe4, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "CPX", },
Opcode { code: 0xe5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xe6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpg, isbr: false, mnemonic: "INC", },
Opcode { code: 0xe7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xe8, ef: CPU::emu_err, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "INX", },
Opcode { code: 0xe9, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Imm, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xea, ef: CPU::emu_nop, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "NOP", },
Opcode { code: 0xeb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xec, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "CPX", },
Opcode { code: 0xed, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xee, ef: CPU::emu_err, step: 3, ops: 2, mode: Abs, isbr: false, mnemonic: "INC", },
Opcode { code: 0xef, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xf0, ef: CPU::emu_bra, step: 0, ops: 1, mode: Rel, isbr: true, mnemonic: "BEQ", },
Opcode { code: 0xf1, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Iny, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xf2, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xf3, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xf4, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xf5, ef: CPU::emu_sbc, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xf6, ef: CPU::emu_err, step: 2, ops: 1, mode: Zpx, isbr: false, mnemonic: "INC", },
Opcode { code: 0xf7, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xf8, ef: CPU::emu_sed, step: 1, ops: 0, mode: Imp, isbr: false, mnemonic: "SED", },
Opcode { code: 0xf9, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Aby, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xfa, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xfb, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xfc, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
Opcode { code: 0xfd, ef: CPU::emu_sbc, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "SBC", },
Opcode { code: 0xfe, ef: CPU::emu_err, step: 3, ops: 2, mode: Abx, isbr: false, mnemonic: "INC", },
Opcode { code: 0xff, ef: CPU::emu_err, step: 0, ops: 0, mode: Unk, isbr: false, mnemonic: "---", },
];
