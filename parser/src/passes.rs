use std::mem::swap;

use crate::{
    assembly::{
        function_bodies_with_parser, irq_address, nmi_address, reset_address, Functions,
        Instruction, Instructions, Opcode,
    },
    mapper::Mapper,
};

pub trait Parser {
    fn read_instruction(&self, buf: &dyn Mapper, addr: u16) -> Instruction;
    fn post_process(&self, roots: &[u16], functions: &mut Functions);

    fn parse_roots(&self, buf: &dyn Mapper, roots: &[u16]) -> Functions {
        let mut funcs = function_bodies_with_parser(|addr| self.read_instruction(buf, addr), roots);
        self.post_process(roots, &mut funcs);
        funcs
    }

    fn parse(&self, buf: &dyn Mapper) -> Functions {
        self.parse_roots(
            buf,
            &[reset_address(buf), nmi_address(buf), irq_address(buf)],
        )
    }
}

pub struct BaseParser;

impl Parser for BaseParser {
    fn read_instruction(&self, buf: &dyn Mapper, addr: u16) -> Instruction {
        Instruction::read(buf, addr)
    }

    fn post_process(&self, _roots: &[u16], _functions: &mut Functions) {}
}

pub struct SelfLoopPass {
    inner: Box<dyn Parser>,
}

impl SelfLoopPass {
    pub fn new(inner: Box<dyn Parser>) -> Self {
        Self { inner }
    }

    pub fn with_inner(inner: impl Parser + 'static) -> Self {
        Self::new(Box::new(inner))
    }
}

impl Parser for SelfLoopPass {
    fn read_instruction(&self, buf: &dyn Mapper, addr: u16) -> Instruction {
        match self.inner.read_instruction(buf, addr) {
            Instruction::Absolute {
                loc,
                opcode: Opcode::JMP,
                addr,
            } if loc == addr => Instruction::Implied {
                loc,
                opcode: Opcode::SLP,
            },
            i => i,
        }
    }

    fn post_process(&self, roots: &[u16], functions: &mut Functions) {
        self.inner.post_process(roots, functions);
    }
}

pub struct SmbSwitchPass {
    inner: Box<dyn Parser>,
}

impl SmbSwitchPass {
    pub fn new(inner: Box<dyn Parser>) -> Self {
        Self { inner }
    }

    pub fn with_inner(inner: impl Parser + 'static) -> Self {
        Self::new(Box::new(inner))
    }
}

impl Parser for SmbSwitchPass {
    fn read_instruction(&self, buf: &dyn Mapper, addr: u16) -> Instruction {
        match self.inner.read_instruction(buf, addr) {
            Instruction::Absolute {
                opcode: Opcode::JSR,
                addr: 0x8e04,
                ..
            } => {
                let mut targets = vec![];
                for i in 0.. {
                    let target = buf.read_address(addr + 3 + (2 * i));
                    if target < 0x8000 {
                        break;
                    }
                    targets.push(target);
                }
                Instruction::Switch {
                    loc: addr,
                    opcode: Opcode::SWA,
                    targets,
                }
            }
            i => i,
        }
    }

    fn post_process(&self, roots: &[u16], functions: &mut Functions) {
        self.inner.post_process(roots, functions);
        let flattened: Instructions = functions
            .iter()
            .flat_map(|(_, insts)| insts.values())
            .map(|inst| {
                if let Instruction::Switch {
                    loc,
                    opcode,
                    targets,
                    ..
                } = inst
                {
                    let new_targets = targets
                        .iter()
                        .cloned()
                        .take_while(|i| is_code(functions, *i))
                        .collect();
                    (
                        *loc,
                        Instruction::Switch {
                            loc: *loc,
                            opcode: *opcode,
                            targets: new_targets,
                        },
                    )
                } else {
                    (inst.offset(), inst.clone())
                }
            })
            .collect();
        let mut trimmed_funcs = function_bodies_with_parser(
            |addr| {
                flattened
                    .get(&addr)
                    .cloned()
                    .unwrap_or(Instruction::Unknown {
                        loc: addr,
                        opcode: 0,
                    })
            },
            roots,
        );
        self.inner.post_process(roots, &mut trimmed_funcs);
        swap(functions, &mut trimmed_funcs);
    }
}

fn is_code(funcs: &Functions, addr: u16) -> bool {
    funcs
        .values()
        .filter(|insts| {
            insts
                .range(0..=addr)
                .next_back()
                .filter(|(_, floor)| addr < floor.next_addr())
                .is_some()
        })
        .next()
        .is_some()
}
