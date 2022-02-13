use crate::mapper::Mapper;
use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
    intrinsics::transmute,
};

pub fn reset_address(buf: &(impl Mapper + ?Sized)) -> u16 {
    buf.read_address(0xfffc)
}

pub fn nmi_address(buf: &(impl Mapper + ?Sized)) -> u16 {
    buf.read_address(0xfffa)
}

pub fn irq_address(buf: &(impl Mapper + ?Sized)) -> u16 {
    buf.read_address(0xfffe)
}

#[derive(Clone, Copy, Debug)]
pub enum Opcode {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    SLP,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    SWA,
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug)]
pub enum Instruction {
    Accumulator {
        loc: u16,
        opcode: Opcode,
    },
    Absolute {
        loc: u16,
        opcode: Opcode,
        addr: u16,
    },
    AbsoluteX {
        loc: u16,
        opcode: Opcode,
        addr: u16,
    },
    AbsoluteY {
        loc: u16,
        opcode: Opcode,
        addr: u16,
    },
    Immediate {
        loc: u16,
        opcode: Opcode,
        val: u8,
    },
    Implied {
        loc: u16,
        opcode: Opcode,
    },
    Indirect {
        loc: u16,
        opcode: Opcode,
        addr: u16,
    },
    IndirectX {
        loc: u16,
        opcode: Opcode,
        offset: u8,
    },
    IndirectY {
        loc: u16,
        opcode: Opcode,
        offset: u8,
    },
    Relative {
        loc: u16,
        opcode: Opcode,
        offset: i8,
    },
    Zeropage {
        loc: u16,
        opcode: Opcode,
        addr: u8,
    },
    ZeropageX {
        loc: u16,
        opcode: Opcode,
        addr: u8,
    },
    ZeropageY {
        loc: u16,
        opcode: Opcode,
        addr: u8,
    },
    Switch {
        loc: u16,
        opcode: Opcode,
        targets: Vec<u16>,
    },
    Unknown {
        loc: u16,
        opcode: u8,
    },
}

impl Instruction {
    pub fn read(buf: &dyn Mapper, offset: u16) -> Self {
        let read_absolute = |opcode| Instruction::Absolute {
            loc: offset,
            opcode,
            addr: buf.read_address(offset + 1),
        };
        let read_absolutex = |opcode| Instruction::AbsoluteX {
            loc: offset,
            opcode,
            addr: buf.read_address(offset + 1),
        };
        let read_absolutey = |opcode| Instruction::AbsoluteY {
            loc: offset,
            opcode,
            addr: buf.read_address(offset + 1),
        };
        let read_accumulator = |opcode| Instruction::Accumulator {
            loc: offset,
            opcode,
        };
        let read_immediate = |opcode| Instruction::Immediate {
            loc: offset,
            opcode,
            val: buf.read_static(offset + 1),
        };
        let read_implied = |opcode| Instruction::Implied {
            loc: offset,
            opcode,
        };
        let read_indirect = |opcode| Instruction::Indirect {
            loc: offset,
            opcode,
            addr: buf.read_address(offset + 1),
        };
        let read_indirectx = |opcode| Instruction::IndirectX {
            loc: offset,
            opcode,
            offset: buf.read_static(offset + 1),
        };
        let read_indirecty = |opcode| Instruction::IndirectY {
            loc: offset,
            opcode,
            offset: buf.read_static(offset + 1),
        };
        let read_relative = |opcode| Instruction::Relative {
            loc: offset,
            opcode,
            offset: unsafe { transmute(buf.read_static(offset + 1)) },
        };
        let read_zeropage = |opcode| Instruction::Zeropage {
            loc: offset,
            opcode,
            addr: buf.read_static(offset + 1),
        };
        let read_zeropagex = |opcode| Instruction::ZeropageX {
            loc: offset,
            opcode,
            addr: buf.read_static(offset + 1),
        };
        let read_zeropagey = |opcode| Instruction::ZeropageY {
            loc: offset,
            opcode,
            addr: buf.read_static(offset + 1),
        };

        match buf.read_static(offset) {
            0x00 => read_implied(Opcode::BRK),
            0x01 => read_indirectx(Opcode::ORA),
            0x05 => read_zeropage(Opcode::ORA),
            0x06 => read_zeropage(Opcode::ASL),
            0x08 => read_implied(Opcode::PHP),
            0x09 => read_immediate(Opcode::ORA),
            0x0a => read_accumulator(Opcode::ASL),
            0x0d => read_absolute(Opcode::ORA),
            0x0e => read_absolute(Opcode::ASL),
            0x10 => read_relative(Opcode::BPL),
            0x11 => read_indirecty(Opcode::ORA),
            0x15 => read_zeropagex(Opcode::ORA),
            0x16 => read_zeropagex(Opcode::ASL),
            0x18 => read_implied(Opcode::CLC),
            0x19 => read_absolutey(Opcode::ORA),
            0x1d => read_absolutex(Opcode::ORA),
            0x1e => read_absolutex(Opcode::ASL),
            0x20 => read_absolute(Opcode::JSR),
            0x21 => read_indirectx(Opcode::AND),
            0x24 => read_zeropage(Opcode::BIT),
            0x25 => read_zeropage(Opcode::AND),
            0x26 => read_zeropage(Opcode::ROL),
            0x28 => read_implied(Opcode::PLP),
            0x29 => read_immediate(Opcode::AND),
            0x2a => read_accumulator(Opcode::ROL),
            0x2c => read_absolute(Opcode::BIT),
            0x2d => read_absolute(Opcode::AND),
            0x2e => read_absolute(Opcode::ROL),
            0x30 => read_relative(Opcode::BMI),
            0x31 => read_indirecty(Opcode::AND),
            0x35 => read_zeropagex(Opcode::AND),
            0x36 => read_zeropagex(Opcode::ROL),
            0x38 => read_implied(Opcode::SEC),
            0x39 => read_absolutey(Opcode::AND),
            0x3d => read_absolutex(Opcode::AND),
            0x3e => read_absolutex(Opcode::ROL),
            0x40 => read_implied(Opcode::RTI),
            0x41 => read_indirectx(Opcode::EOR),
            0x45 => read_zeropage(Opcode::EOR),
            0x46 => read_zeropage(Opcode::LSR),
            0x48 => read_implied(Opcode::PHA),
            0x49 => read_immediate(Opcode::EOR),
            0x4a => read_accumulator(Opcode::LSR),
            0x4c => read_absolute(Opcode::JMP),
            0x4d => read_absolute(Opcode::EOR),
            0x4e => read_absolute(Opcode::LSR),
            0x50 => read_relative(Opcode::BVC),
            0x51 => read_indirecty(Opcode::EOR),
            0x55 => read_zeropagex(Opcode::EOR),
            0x56 => read_zeropagex(Opcode::LSR),
            0x58 => read_implied(Opcode::CLI),
            0x59 => read_absolutey(Opcode::EOR),
            0x5d => read_absolutex(Opcode::EOR),
            0x5e => read_absolutex(Opcode::LSR),
            0x60 => read_implied(Opcode::RTS),
            0x61 => read_indirectx(Opcode::ADC),
            0x65 => read_zeropage(Opcode::ADC),
            0x66 => read_zeropage(Opcode::ROR),
            0x68 => read_implied(Opcode::PLA),
            0x69 => read_immediate(Opcode::ADC),
            0x6a => read_accumulator(Opcode::ROR),
            0x6c => read_indirect(Opcode::JMP),
            0x6d => read_absolute(Opcode::ADC),
            0x6e => read_absolute(Opcode::ROR),
            0x70 => read_relative(Opcode::BVS),
            0x71 => read_indirecty(Opcode::ADC),
            0x75 => read_zeropagex(Opcode::ADC),
            0x76 => read_zeropagex(Opcode::ROR),
            0x78 => read_implied(Opcode::SEI),
            0x79 => read_absolutey(Opcode::ADC),
            0x7d => read_absolutex(Opcode::ADC),
            0x7e => read_absolutex(Opcode::ROR),
            0x81 => read_indirectx(Opcode::STA),
            0x84 => read_zeropage(Opcode::STY),
            0x85 => read_zeropage(Opcode::STA),
            0x86 => read_zeropage(Opcode::STX),
            0x88 => read_implied(Opcode::DEY),
            0x8a => read_implied(Opcode::TXA),
            0x8c => read_absolute(Opcode::STY),
            0x8d => read_absolute(Opcode::STA),
            0x8e => read_absolute(Opcode::STX),
            0x90 => read_relative(Opcode::BCC),
            0x91 => read_indirecty(Opcode::STA),
            0x94 => read_zeropagex(Opcode::STY),
            0x95 => read_zeropagex(Opcode::STA),
            0x96 => read_zeropagey(Opcode::STX),
            0x98 => read_implied(Opcode::TYA),
            0x99 => read_absolutey(Opcode::STA),
            0x9a => read_implied(Opcode::TXS),
            0x9d => read_absolutex(Opcode::STA),
            0xa0 => read_immediate(Opcode::LDY),
            0xa1 => read_indirectx(Opcode::LDA),
            0xa2 => read_immediate(Opcode::LDX),
            0xa4 => read_zeropage(Opcode::LDY),
            0xa5 => read_zeropage(Opcode::LDA),
            0xa6 => read_zeropage(Opcode::LDX),
            0xa8 => read_implied(Opcode::TAY),
            0xa9 => read_immediate(Opcode::LDA),
            0xaa => read_implied(Opcode::TAX),
            0xac => read_absolute(Opcode::LDY),
            0xad => read_absolute(Opcode::LDA),
            0xae => read_absolute(Opcode::LDX),
            0xb0 => read_relative(Opcode::BCS),
            0xb1 => read_indirecty(Opcode::LDA),
            0xb4 => read_zeropagex(Opcode::LDY),
            0xb5 => read_zeropagex(Opcode::LDA),
            0xb6 => read_zeropagey(Opcode::LDX),
            0xb8 => read_implied(Opcode::CLV),
            0xb9 => read_absolutey(Opcode::LDA),
            0xba => read_implied(Opcode::TSX),
            0xbc => read_absolutex(Opcode::LDY),
            0xbd => read_absolutex(Opcode::LDA),
            0xbe => read_absolutey(Opcode::LDX),
            0xc0 => read_immediate(Opcode::CPY),
            0xc1 => read_indirectx(Opcode::CMP),
            0xc4 => read_zeropage(Opcode::CPY),
            0xc5 => read_zeropage(Opcode::CMP),
            0xc6 => read_zeropage(Opcode::DEC),
            0xc8 => read_implied(Opcode::INY),
            0xc9 => read_immediate(Opcode::CMP),
            0xca => read_implied(Opcode::DEX),
            0xcc => read_absolute(Opcode::CPY),
            0xcd => read_absolute(Opcode::CMP),
            0xce => read_absolute(Opcode::DEC),
            0xd0 => read_relative(Opcode::BNE),
            0xd1 => read_indirecty(Opcode::CMP),
            0xd5 => read_zeropagex(Opcode::CMP),
            0xd6 => read_zeropagex(Opcode::DEC),
            0xd8 => read_implied(Opcode::CLD),
            0xd9 => read_absolutey(Opcode::CMP),
            0xdd => read_absolutex(Opcode::CMP),
            0xde => read_absolutex(Opcode::DEC),
            0xe0 => read_immediate(Opcode::CPX),
            0xe1 => read_indirecty(Opcode::SBC),
            0xe4 => read_zeropage(Opcode::CPX),
            0xe5 => read_zeropage(Opcode::SBC),
            0xe6 => read_zeropage(Opcode::INC),
            0xe8 => read_implied(Opcode::INX),
            0xe9 => read_immediate(Opcode::SBC),
            0xea => read_implied(Opcode::NOP),
            0xec => read_absolute(Opcode::CPX),
            0xed => read_absolute(Opcode::SBC),
            0xee => read_absolute(Opcode::INC),
            0xf0 => read_relative(Opcode::BEQ),
            0xf1 => read_indirecty(Opcode::SBC),
            0xf5 => read_zeropagex(Opcode::SBC),
            0xf6 => read_zeropagex(Opcode::INC),
            0xf8 => read_implied(Opcode::SED),
            0xf9 => read_absolutey(Opcode::SBC),
            0xfd => read_absolutex(Opcode::SBC),
            0xfe => read_absolutex(Opcode::INC),
            op => Instruction::Unknown {
                loc: offset,
                opcode: op,
            },
        }
    }

    pub fn offset(&self) -> u16 {
        match self {
            Instruction::Absolute { loc, .. } => *loc,
            Instruction::AbsoluteX { loc, .. } => *loc,
            Instruction::AbsoluteY { loc, .. } => *loc,
            Instruction::Accumulator { loc, .. } => *loc,
            Instruction::Immediate { loc, .. } => *loc,
            Instruction::Implied { loc, .. } => *loc,
            Instruction::Indirect { loc, .. } => *loc,
            Instruction::IndirectX { loc, .. } => *loc,
            Instruction::IndirectY { loc, .. } => *loc,
            Instruction::Relative { loc, .. } => *loc,
            Instruction::Zeropage { loc, .. } => *loc,
            Instruction::ZeropageX { loc, .. } => *loc,
            Instruction::ZeropageY { loc, .. } => *loc,
            Instruction::Switch { loc, .. } => *loc,
            Instruction::Unknown { loc, .. } => *loc,
        }
    }

    pub fn bin_length(&self) -> u16 {
        match self {
            Instruction::Absolute { .. } => 3,
            Instruction::AbsoluteX { .. } => 3,
            Instruction::AbsoluteY { .. } => 3,
            Instruction::Accumulator { .. } => 1,
            Instruction::Immediate { .. } => 2,
            Instruction::Implied { .. } => 1,
            Instruction::Indirect { .. } => 3,
            Instruction::IndirectX { .. } => 2,
            Instruction::IndirectY { .. } => 2,
            Instruction::Relative { .. } => 2,
            Instruction::Zeropage { .. } => 2,
            Instruction::ZeropageX { .. } => 2,
            Instruction::ZeropageY { .. } => 2,
            Instruction::Switch { targets, .. } => (targets.len() as u16) + 3,
            Instruction::Unknown { .. } => 1,
        }
    }

    pub fn next_addr(&self) -> u16 {
        self.offset() + self.bin_length()
    }

    pub fn branch_addr(&self) -> Option<u16> {
        match self {
            Instruction::Relative { loc, offset, .. } => {
                Some(((*loc as i32) + 2 + (*offset as i32)) as u16)
            }
            _ => None,
        }
    }

    pub fn following_addrs(&self) -> Vec<u16> {
        match self {
            Instruction::Absolute {
                opcode: Opcode::JMP,
                addr,
                ..
            } => vec![*addr],
            Instruction::Implied {
                opcode: Opcode::RTI,
                ..
            } => vec![],
            Instruction::Implied {
                opcode: Opcode::RTS,
                ..
            } => vec![],
            Instruction::Implied {
                opcode: Opcode::SLP,
                ..
            } => vec![],
            Instruction::Indirect {
                opcode: Opcode::JMP,
                ..
            } => vec![],
            i @ Instruction::Relative { loc, offset, .. } => {
                vec![i.next_addr(), ((*loc as i32) + 2 + (*offset as i32)) as u16]
            }
            Instruction::Switch { targets, .. } => targets.clone(),
            Instruction::Unknown { .. } => vec![],
            i => vec![i.next_addr()],
        }
    }

    pub fn call_target(&self) -> Option<u16> {
        match self {
            Instruction::Absolute {
                opcode: Opcode::JSR,
                addr,
                ..
            } => Some(*addr),
            _ => None,
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Absolute { opcode, addr, .. } => write!(f, "{} {:04x}", opcode, addr),
            Instruction::AbsoluteX { opcode, addr, .. } => write!(f, "{} {:04x},X", opcode, addr),
            Instruction::AbsoluteY { opcode, addr, .. } => write!(f, "{} {:04x},Y", opcode, addr),
            Instruction::Accumulator { opcode, .. } => write!(f, "{} A", opcode),
            Instruction::Immediate { opcode, val, .. } => write!(f, "{} #${:02x}", opcode, val),
            Instruction::Implied { opcode, .. } => write!(f, "{}", opcode),
            Instruction::Indirect { opcode, addr, .. } => write!(f, "{} (${:04x})", opcode, addr),
            Instruction::IndirectX { opcode, offset, .. } => {
                write!(f, "{} (${:02x},X)", opcode, offset)
            }
            Instruction::IndirectY { opcode, offset, .. } => {
                write!(f, "{} (${:02x}),Y", opcode, offset)
            }
            Instruction::Relative {
                loc,
                opcode,
                offset,
                ..
            } => write!(
                f,
                "{} ({:04x})",
                opcode,
                ((*loc as i32) + 2 + (*offset as i32)) as u16
            ),
            Instruction::Unknown { opcode, .. } => write!(f, "!Unknown {:02x}", opcode),
            Instruction::Zeropage { opcode, addr, .. } => write!(f, "{} ${:02x}", opcode, addr),
            Instruction::ZeropageX { opcode, addr, .. } => write!(f, "{} ${:02x},X", opcode, addr),
            Instruction::ZeropageY { opcode, addr, .. } => write!(f, "{} ${:02x},Y", opcode, addr),
            Instruction::Switch {
                opcode, targets, ..
            } => {
                write!(f, "{} [", opcode)?;
                let mut first = true;
                for target in targets {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:04x}", target)?;
                }
                write!(f, "]")
            }
        }
    }
}

fn reachable<A, B, F>(roots: &[A], f: F) -> BTreeMap<A, B>
where
    A: Clone + Ord,
    F: Fn(A) -> (B, Vec<A>),
{
    let mut result = BTreeMap::new();
    let mut inner_roots = vec![];
    inner_roots.extend(roots.iter().cloned());
    while !inner_roots.is_empty() {
        let root = inner_roots.pop().unwrap();
        if !result.contains_key(&root) {
            let (val, step_roots) = f(root.clone());
            result.insert(root.clone(), val);
            inner_roots.extend(step_roots);
        }
    }
    result
}

pub type Instructions = BTreeMap<u16, Instruction>;

pub type Functions = BTreeMap<u16, Instructions>;

pub fn function_body_with_parser(read: impl Fn(u16) -> Instruction, root: u16) -> Instructions {
    reachable(&[root], |addr| {
        let inst = read(addr);
        let following_addrs = inst.following_addrs();
        (inst, following_addrs)
    })
}

pub fn function_bodies_with_parser(read: impl Fn(u16) -> Instruction, roots: &[u16]) -> Functions {
    reachable(roots, |root| {
        let func = function_body_with_parser(&read, root);
        let call_targets = func.values().filter_map(Instruction::call_target).collect();
        (func, call_targets)
    })
}
