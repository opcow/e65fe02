use crate::cpu65::CPU;

pub const EMU_FUNCS: [fn(&mut CPU); 256] = [
    CPU::emu_brk,      // 0x00: BRK
    CPU::emu_not_impl, // 0x01: ORA
    CPU::emu_not_impl, // 0x02:
    CPU::emu_not_impl, // 0x03:
    CPU::emu_not_impl, // 0x04:
    CPU::emu_not_impl, // 0x05: ORA
    CPU::emu_not_impl, // 0x06: ASL
    CPU::emu_not_impl, // 0x07:
    CPU::emu_not_impl, // 0x08: PHP
    CPU::emu_not_impl, // 0x09: ORA
    CPU::emu_not_impl, // 0x0a: ASL
    CPU::emu_not_impl, // 0x0b:
    CPU::emu_not_impl, // 0x0c:
    CPU::emu_not_impl, // 0x0d: ORA
    CPU::emu_not_impl, // 0x0e: ASL
    CPU::emu_not_impl, // 0x0f:
    CPU::emu_bra,      // 0x10: BPL
    CPU::emu_not_impl, // 0x11: ORA
    CPU::emu_not_impl, // 0x12:
    CPU::emu_not_impl, // 0x13:
    CPU::emu_not_impl, // 0x14:
    CPU::emu_not_impl, // 0x15: ORA
    CPU::emu_not_impl, // 0x16: ASL
    CPU::emu_not_impl, // 0x17:
    CPU::emu_clc,      // 0x18: CLC
    CPU::emu_not_impl, // 0x19: ORA
    CPU::emu_not_impl, // 0x1a:
    CPU::emu_not_impl, // 0x1b:
    CPU::emu_not_impl, // 0x1c:
    CPU::emu_not_impl, // 0x1d: ORA
    CPU::emu_not_impl, // 0x1e: ASL
    CPU::emu_not_impl, // 0x1f:
    CPU::emu_jsr,      // 0x20: JSR
    CPU::emu_not_impl, // 0x21: AND
    CPU::emu_not_impl, // 0x22:
    CPU::emu_not_impl, // 0x23:
    CPU::emu_not_impl, // 0x24: BIT
    CPU::emu_not_impl, // 0x25: AND
    CPU::emu_rol,      // 0x26: ROL
    CPU::emu_not_impl, // 0x27:
    CPU::emu_not_impl, // 0x28: PLP
    CPU::emu_not_impl, // 0x29: AND
    CPU::emu_rol,      // 0x2a: ROL
    CPU::emu_not_impl, // 0x2b:
    CPU::emu_not_impl, // 0x2c: BIT
    CPU::emu_not_impl, // 0x2d: AND
    CPU::emu_rol,      // 0x2e: ROL
    CPU::emu_not_impl, // 0x2f:
    CPU::emu_bra,      // 0x30: BMI
    CPU::emu_not_impl, // 0x31: AND
    CPU::emu_not_impl, // 0x32:
    CPU::emu_not_impl, // 0x33:
    CPU::emu_not_impl, // 0x34:
    CPU::emu_not_impl, // 0x35: AND
    CPU::emu_rol,      // 0x36: ROL
    CPU::emu_not_impl, // 0x37:
    CPU::emu_not_impl, // 0x38: SEC
    CPU::emu_not_impl, // 0x39: AND
    CPU::emu_not_impl, // 0x3a:
    CPU::emu_not_impl, // 0x3b:
    CPU::emu_not_impl, // 0x3c:
    CPU::emu_not_impl, // 0x3d: AND
    CPU::emu_rol,      // 0x3e: ROL
    CPU::emu_not_impl, // 0x3f:
    CPU::emu_not_impl, // 0x40: RTI
    CPU::emu_not_impl, // 0x41: EOR
    CPU::emu_not_impl, // 0x42:
    CPU::emu_not_impl, // 0x43:
    CPU::emu_not_impl, // 0x44:
    CPU::emu_not_impl, // 0x45: EOR
    CPU::emu_not_impl, // 0x46: LSR
    CPU::emu_not_impl, // 0x47:
    CPU::emu_not_impl, // 0x48: PHA
    CPU::emu_not_impl, // 0x49: EOR
    CPU::emu_not_impl, // 0x4a: LSR
    CPU::emu_not_impl, // 0x4b:
    CPU::emu_not_impl, // 0x4c: JMP
    CPU::emu_not_impl, // 0x4d: EOR
    CPU::emu_not_impl, // 0x4e: LSR
    CPU::emu_not_impl, // 0x4f:
    CPU::emu_bra,      // 0x50: BVC
    CPU::emu_not_impl, // 0x51: EOR
    CPU::emu_not_impl, // 0x52:
    CPU::emu_not_impl, // 0x53:
    CPU::emu_not_impl, // 0x54:
    CPU::emu_not_impl, // 0x55: EOR
    CPU::emu_not_impl, // 0x56: LSR
    CPU::emu_not_impl, // 0x57:
    CPU::emu_not_impl, // 0x58: CLI
    CPU::emu_not_impl, // 0x59: EOR
    CPU::emu_not_impl, // 0x5a:
    CPU::emu_not_impl, // 0x5b:
    CPU::emu_not_impl, // 0x5c:
    CPU::emu_not_impl, // 0x5d: EOR
    CPU::emu_not_impl, // 0x5e: LSR
    CPU::emu_not_impl, // 0x5f:
    CPU::emu_not_impl, // 0x60: RTS
    CPU::emu_not_impl, // 0x61: ADC
    CPU::emu_not_impl, // 0x62:
    CPU::emu_not_impl, // 0x63:
    CPU::emu_not_impl, // 0x64:
    CPU::emu_not_impl, // 0x65: ADC
    CPU::emu_ror,      // 0x66: ROR
    CPU::emu_not_impl, // 0x67:
    CPU::emu_not_impl, // 0x68: PLA
    CPU::emu_not_impl, // 0x69: ADC
    CPU::emu_ror,      // 0x6a: ROR
    CPU::emu_not_impl, // 0x6b:
    CPU::emu_not_impl, // 0x6c: JMP
    CPU::emu_not_impl, // 0x6d: ADC
    CPU::emu_ror,      // 0x6e: ROR
    CPU::emu_not_impl, // 0x6f:
    CPU::emu_bra,      // 0x70: BVS
    CPU::emu_not_impl, // 0x71: ADC
    CPU::emu_not_impl, // 0x72:
    CPU::emu_not_impl, // 0x73:
    CPU::emu_not_impl, // 0x74:
    CPU::emu_not_impl, // 0x75: ADC
    CPU::emu_ror,      // 0x76: ROR
    CPU::emu_not_impl, // 0x77:
    CPU::emu_not_impl, // 0x78: SEI
    CPU::emu_not_impl, // 0x79: ADC
    CPU::emu_not_impl, // 0x7a:
    CPU::emu_not_impl, // 0x7b:
    CPU::emu_not_impl, // 0x7c:
    CPU::emu_not_impl, // 0x7d: ADC
    CPU::emu_ror,      // 0x7e: ROR
    CPU::emu_not_impl, // 0x7f:
    CPU::emu_not_impl, // 0x80:
    CPU::emu_sta,      // 0x81: STA
    CPU::emu_not_impl, // 0x82:
    CPU::emu_not_impl, // 0x83:
    CPU::emu_not_impl, // 0x84: STY
    CPU::emu_sta,      // 0x85: STA
    CPU::emu_not_impl, // 0x86: STX
    CPU::emu_not_impl, // 0x87:
    CPU::emu_not_impl, // 0x88: DEY
    CPU::emu_not_impl, // 0x89:
    CPU::emu_not_impl, // 0x8a: TXA
    CPU::emu_not_impl, // 0x8b:
    CPU::emu_not_impl, // 0x8c: STY
    CPU::emu_sta,      // 0x8d: STA
    CPU::emu_not_impl, // 0x8e: STX
    CPU::emu_not_impl, // 0x8f:
    CPU::emu_bra,      // 0x90: BCC
    CPU::emu_sta,      // 0x91: STA
    CPU::emu_not_impl, // 0x92:
    CPU::emu_not_impl, // 0x93:
    CPU::emu_not_impl, // 0x94: STY
    CPU::emu_sta,      // 0x95: STA
    CPU::emu_not_impl, // 0x96: STX
    CPU::emu_not_impl, // 0x97:
    CPU::emu_not_impl, // 0x98: TYA
    CPU::emu_sta,      // 0x99: STA
    CPU::emu_not_impl, // 0x9a: TXS
    CPU::emu_not_impl, // 0x9b:
    CPU::emu_not_impl, // 0x9c:
    CPU::emu_sta,      // 0x9d: STA
    CPU::emu_not_impl, // 0x9e:
    CPU::emu_not_impl, // 0x9f:
    CPU::emu_ldy,      // 0xa0: LDY
    CPU::emu_not_impl, // 0xa1: LDA
    CPU::emu_not_impl, // 0xa2: LDX
    CPU::emu_not_impl, // 0xa3:
    CPU::emu_ldy,      // 0xa4: LDY
    CPU::emu_not_impl, // 0xa5: LDA
    CPU::emu_not_impl, // 0xa6: LDX
    CPU::emu_not_impl, // 0xa7:
    CPU::emu_not_impl, // 0xa8: TAY
    CPU::emu_not_impl, // 0xa9: LDA
    CPU::emu_not_impl, // 0xaa: TAX
    CPU::emu_not_impl, // 0xab:
    CPU::emu_ldy,      // 0xac: LDY
    CPU::emu_not_impl, // 0xad: LDA
    CPU::emu_not_impl, // 0xae: LDX
    CPU::emu_not_impl, // 0xaf:
    CPU::emu_bra,      // 0xb0: BCS
    CPU::emu_not_impl, // 0xb1: LDA
    CPU::emu_not_impl, // 0xb2:
    CPU::emu_not_impl, // 0xb3:
    CPU::emu_ldy,      // 0xb4: LDY
    CPU::emu_not_impl, // 0xb5: LDA
    CPU::emu_not_impl, // 0xb6: LDX
    CPU::emu_not_impl, // 0xb7:
    CPU::emu_not_impl, // 0xb8: CLV
    CPU::emu_not_impl, // 0xb9: LDA
    CPU::emu_not_impl, // 0xba: TSX
    CPU::emu_not_impl, // 0xbb:
    CPU::emu_ldy,      // 0xbc: LDY
    CPU::emu_not_impl, // 0xbd: LDA
    CPU::emu_not_impl, // 0xbe: LDX
    CPU::emu_not_impl, // 0xbf:
    CPU::emu_not_impl, // 0xc0: CPY
    CPU::emu_not_impl, // 0xc1: CMP
    CPU::emu_not_impl, // 0xc2:
    CPU::emu_not_impl, // 0xc3:
    CPU::emu_not_impl, // 0xc4: CPY
    CPU::emu_not_impl, // 0xc5: CMP
    CPU::emu_not_impl, // 0xc6: DEC
    CPU::emu_not_impl, // 0xc7:
    CPU::emu_not_impl, // 0xc8: INY
    CPU::emu_not_impl, // 0xc9: CMP
    CPU::emu_not_impl, // 0xca: DEX
    CPU::emu_not_impl, // 0xcb:
    CPU::emu_not_impl, // 0xcc: CPY
    CPU::emu_not_impl, // 0xcd: CMP
    CPU::emu_not_impl, // 0xce: DEC
    CPU::emu_not_impl, // 0xcf:
    CPU::emu_bra,      // 0xd0: BNE
    CPU::emu_not_impl, // 0xd1: CMP
    CPU::emu_not_impl, // 0xd2:
    CPU::emu_not_impl, // 0xd3:
    CPU::emu_not_impl, // 0xd4:
    CPU::emu_not_impl, // 0xd5: CMP
    CPU::emu_not_impl, // 0xd6: DEC
    CPU::emu_not_impl, // 0xd7:
    CPU::emu_cld,      // 0xd8: CLD
    CPU::emu_not_impl, // 0xd9: CMP
    CPU::emu_not_impl, // 0xda:
    CPU::emu_not_impl, // 0xdb:
    CPU::emu_not_impl, // 0xdc:
    CPU::emu_not_impl, // 0xdd: CMP
    CPU::emu_not_impl, // 0xde: DEC
    CPU::emu_not_impl, // 0xdf:
    CPU::emu_not_impl, // 0xe0: CPX
    CPU::emu_not_impl, // 0xe1: SBC
    CPU::emu_not_impl, // 0xe2:
    CPU::emu_not_impl, // 0xe3:
    CPU::emu_not_impl, // 0xe4: CPX
    CPU::emu_not_impl, // 0xe5: SBC
    CPU::emu_not_impl, // 0xe6: INC
    CPU::emu_not_impl, // 0xe7:
    CPU::emu_not_impl, // 0xe8: INX
    CPU::emu_not_impl, // 0xe9: SBC
    CPU::emu_not_impl, // 0xea: NOP
    CPU::emu_not_impl, // 0xeb:
    CPU::emu_not_impl, // 0xec: CPX
    CPU::emu_not_impl, // 0xed: SBC
    CPU::emu_not_impl, // 0xee: INC
    CPU::emu_not_impl, // 0xef:
    CPU::emu_bra,      // 0xf0: BEQ
    CPU::emu_not_impl, // 0xf1: SBC
    CPU::emu_not_impl, // 0xf2:
    CPU::emu_not_impl, // 0xf3:
    CPU::emu_not_impl, // 0xf4:
    CPU::emu_not_impl, // 0xf5: SBC
    CPU::emu_not_impl, // 0xf6: INC
    CPU::emu_not_impl, // 0xf7:
    CPU::emu_not_impl, // 0xf8: SED
    CPU::emu_not_impl, // 0xf9: SBC
    CPU::emu_not_impl, // 0xfa:
    CPU::emu_not_impl, // 0xfb:
    CPU::emu_not_impl, // 0xfc:
    CPU::emu_not_impl, // 0xfd: SBC
    CPU::emu_not_impl, // 0xfe: INC
    CPU::emu_not_impl, // 0xff:
];
