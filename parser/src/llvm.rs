use std::collections::BTreeMap;

use inkwell::{module::{Module, Linkage}, context::Context, values::{FunctionValue, BasicValueEnum, PointerValue, IntValue}, attributes::{AttributeLoc, Attribute}, AddressSpace, builder::Builder, basic_block::BasicBlock, IntPredicate, types::{StructType, BasicType}};

use crate::{assembly::{Instruction, Opcode}};

const INACCESSIBLE_MEM_ONLY: u32 = 9;
const NO_ALIAS: u32 = 16;
const NO_CAPTURE: u32 = 18;
const NO_UNWIND: u32 = 30;
const READ_ONLY: u32 = 38;
const SRET: u32 = 55;
const WILL_RETURN: u32 = 59;

pub struct Compiler<'a, 'ctx> {
  context: &'ctx Context,
  builder: Builder<'ctx>,
  module: &'a Module<'ctx>,
}

impl <'a, 'ctx> Compiler<'a, 'ctx> {
  pub fn new(context: &'ctx Context, module: &'a Module<'ctx>) -> Self {
    let result = Self {
      context,
      builder: context.create_builder(),
      module,
    };
    result.add_read_mem_decl();
    result.add_write_mem_decl();
    result.add_uadd_carry_decl();
    result.add_sadd_carry_decl();
    result.add_usub_carry_decl();
    result.add_ssub_carry_decl();
    let sp = result.module.add_global(result.context.i8_type(), None, "sp");
    sp.set_initializer(&result.context.i8_type().const_zero());
    sp.set_linkage(Linkage::Private);
    result
  }

  fn add_read_mem_decl(&self) {
    let fn_type = self.context.i8_type().fn_type(&[self.context.i16_type().into()], false);
    let result = self.module.add_function("readMem", fn_type, None);
    let read_only = self.context.create_enum_attribute(READ_ONLY, 0);
    let inaccessible_mem_only = self.context.create_enum_attribute(INACCESSIBLE_MEM_ONLY, 0);
    let no_unwind = self.context.create_enum_attribute(NO_UNWIND, 0);
    let will_return = self.context.create_enum_attribute(WILL_RETURN, 0);
    // result.add_attribute(AttributeLoc::Function, read_only);
    result.add_attribute(AttributeLoc::Function, inaccessible_mem_only);
    result.add_attribute(AttributeLoc::Function, no_unwind);
    result.add_attribute(AttributeLoc::Function, will_return);
  }
  
  fn add_write_mem_decl(&self) {
    let fn_type = self.context.void_type().fn_type(&[self.context.i16_type().into(), self.context.i8_type().into()], false);
    let result = self.module.add_function("writeMem", fn_type, None);
    let inaccessible_mem_only = self.context.create_enum_attribute(INACCESSIBLE_MEM_ONLY, 0);
    let no_unwind = self.context.create_enum_attribute(NO_UNWIND, 0);
    let will_return = self.context.create_enum_attribute(WILL_RETURN, 0);
    result.add_attribute(AttributeLoc::Function, inaccessible_mem_only);
    result.add_attribute(AttributeLoc::Function, no_unwind);
    result.add_attribute(AttributeLoc::Function, will_return);
  }
  
  fn add_uadd_carry_decl(&self) {
    let i8_ty = self.context.i8_type().into();
    let i1_ty = self.context.bool_type().into();
    let result_type = self.context.struct_type(&[i8_ty, i1_ty], false);
    let fn_type = result_type.fn_type(&[i8_ty.into(), i8_ty.into()], false);
    self.module.add_function("llvm.uadd.with.overflow.i8", fn_type, None);
  }
  
  fn add_sadd_carry_decl(&self) {
    let i8_ty = self.context.i8_type().into();
    let i1_ty = self.context.bool_type().into();
    let result_type = self.context.struct_type(&[i8_ty, i1_ty], false);
    let fn_type = result_type.fn_type(&[i8_ty.into(), i8_ty.into()], false);
    self.module.add_function("llvm.sadd.with.overflow.i8", fn_type, None);
  }
  
  fn add_usub_carry_decl(&self) {
    let i8_ty = self.context.i8_type().into();
    let i1_ty = self.context.bool_type().into();
    let result_type = self.context.struct_type(&[i8_ty, i1_ty], false);
    let fn_type = result_type.fn_type(&[i8_ty.into(), i8_ty.into()], false);
    self.module.add_function("llvm.usub.with.overflow.i8", fn_type, None);
  }
  
  fn add_ssub_carry_decl(&self) {
    let i8_ty = self.context.i8_type().into();
    let i1_ty = self.context.bool_type().into();
    let result_type = self.context.struct_type(&[i8_ty, i1_ty], false);
    let fn_type = result_type.fn_type(&[i8_ty.into(), i8_ty.into()], false);
    self.module.add_function("llvm.ssub.with.overflow.i8", fn_type, None);
  }

  pub fn declare_func(&mut self, addr: u16) {
    let i8_type = self.context.i8_type().into();
    let i1_type = self.context.bool_type().into();
    let ret_ty = function_return_type(self.context);
    let fn_type = ret_ty.fn_type(&[
      i8_type, // A
      i8_type, // X
      i8_type, // Y
      i1_type, // N
      i1_type, // Z
      i1_type, // V
      i1_type, // C
      ], false);
    self.module.add_function(&function_name(addr), fn_type, Some(Linkage::Private));
  }

  pub fn define_func(&mut self, addr: u16, body: &BTreeMap<u16, Instruction>) {
    let func = self.module.get_function(&function_name(addr)).expect("Unknown function");
    let mut function_compiler = FunctionCompiler::new(self, func);

    for addr in body.keys() {
      function_compiler.declare_instruction(*addr);
    }

    for inst in body.values() {
      function_compiler.define_instruction(inst);
    }

    function_compiler.set_first_instruction(addr);
  }

  pub fn define_entry(&mut self, name: &str, addr: u16) {
    let fn_type = self.context.void_type().fn_type(&[], false);
    let i8_zero = self.context.i8_type().const_zero();
    let i1_zero = self.context.bool_type().const_zero();
    let function = self.module.add_function(name, fn_type, None);
    let entry = self.context.append_basic_block(function, "entry");
    self.builder.position_at_end(entry);
    let inner = self.module.get_function(&function_name(addr)).unwrap();
    self.builder.build_call(inner, &[i8_zero.into(), i8_zero.into(), i8_zero.into(), i1_zero.into(),
      i1_zero.into(), i1_zero.into(), i1_zero.into()], "res");
    self.builder.build_return(None);
  }
}

fn function_name(addr: u16) -> String {
  format!("func_{:04x}", addr)
}

fn function_return_type(context: &Context) -> StructType {
  let i8_ty = context.i8_type().into();
  let i1_ty = context.bool_type().into();
  let i16_ty = context.i16_type().into();
  context.struct_type(&[i8_ty, i8_ty, i8_ty, i1_ty, i1_ty, i1_ty, i1_ty, i8_ty, i16_ty], false)
}

struct FunctionCompiler<'a, 'b, 'ctx> {
  compiler: &'a Compiler<'b, 'ctx>,
  context: &'ctx Context,
  builder: &'a Builder<'ctx>,
  function: FunctionValue<'ctx>,
  blocks: BTreeMap<u16, BasicBlock<'ctx>>,
  reg_a: PointerValue<'ctx>,
  reg_x: PointerValue<'ctx>,
  reg_y: PointerValue<'ctx>,
  reg_n: PointerValue<'ctx>,
  reg_z: PointerValue<'ctx>,
  reg_v: PointerValue<'ctx>,
  reg_c: PointerValue<'ctx>,
}

impl <'a, 'b, 'ctx> FunctionCompiler<'a, 'b, 'ctx> {
  fn new(compiler: &'a Compiler<'b, 'ctx>, function: FunctionValue<'ctx>) -> Self {
    let entry = compiler.context.append_basic_block(function, "entry");
    compiler.builder.position_at_end(entry);

    let reg_a = compiler.builder.build_alloca(compiler.context.i8_type(), "regA");
    let reg_x = compiler.builder.build_alloca(compiler.context.i8_type(), "regX");
    let reg_y = compiler.builder.build_alloca(compiler.context.i8_type(), "regY");
    let reg_n = compiler.builder.build_alloca(compiler.context.bool_type(), "regN");
    let reg_z = compiler.builder.build_alloca(compiler.context.bool_type(), "regZ");
    let reg_v = compiler.builder.build_alloca(compiler.context.bool_type(), "regV");
    let reg_c = compiler.builder.build_alloca(compiler.context.bool_type(), "regC");
    compiler.builder.build_store(reg_a, function.get_nth_param(0).unwrap().into_int_value());
    compiler.builder.build_store(reg_x, function.get_nth_param(1).unwrap().into_int_value());
    compiler.builder.build_store(reg_y, function.get_nth_param(2).unwrap().into_int_value());
    compiler.builder.build_store(reg_n, function.get_nth_param(3).unwrap().into_int_value());
    compiler.builder.build_store(reg_z, function.get_nth_param(4).unwrap().into_int_value());
    compiler.builder.build_store(reg_v, function.get_nth_param(5).unwrap().into_int_value());
    compiler.builder.build_store(reg_c, function.get_nth_param(6).unwrap().into_int_value());

    Self {
      compiler,
      context: compiler.context,
      builder: &compiler.builder,
      function,
      blocks: BTreeMap::new(),
      reg_a,
      reg_x,
      reg_y,
      reg_n,
      reg_z,
      reg_v,
      reg_c,
    }
  }

  fn declare_instruction(&mut self, addr: u16) {
    self.blocks.insert(addr, self.compiler.context.append_basic_block(self.function, &format!("{:04x}", addr)));
  }

  fn define_instruction(&mut self, inst: &Instruction) {
    let block = *self.blocks.get(&inst.offset()).unwrap();
    self.builder.position_at_end(block);

    self.write_instruction(inst);

    if block.get_terminator().is_none() {
      if let Some(following) = self.blocks.get(&inst.next_addr()) {
        self.builder.build_unconditional_branch(*following);
      } else {
        let ret = function_return_type(self.context).const_zero();
        self.builder.build_return(Some(&ret));
      }
    }
  }

  fn write_instruction(&self, inst: &Instruction) {
    match inst {
      Instruction::Absolute { opcode: Opcode::ADC, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.adc(arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::AND, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.and(arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::ASL, addr, .. } => {
        let arg = self.absolute_value(*addr);
        let val = self.asl(arg);
        self.write_mem(self.context.i16_type().const_int(*addr as u64, false), val);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::BIT, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.bit(arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::CMP, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.compare(self.reg_a, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::CPX, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.compare(self.reg_x, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::CPY, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.compare(self.reg_y, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::DEC, addr, .. } => {
        self.decrement(self.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::EOR, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.eor(arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::INC, addr, .. } => {
        self.increment(self.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::JMP, addr, .. } => {
        self.builder.build_unconditional_branch(*self.blocks.get(addr).unwrap());
      }
      Instruction::Absolute { loc, opcode: Opcode::JSR, addr, ..} => {
        self.incr_clk(6);
        self.push(self.context.i8_type().const_int((*loc & 0xFF) as u64, false));
        self.push(self.context.i8_type().const_int(((*loc >> 8) & 0xFF) as u64, false));
        let a = self.builder.build_load(self.reg_a, "a");
        let x = self.builder.build_load(self.reg_x, "x");
        let y = self.builder.build_load(self.reg_y, "y");
        let n = self.builder.build_load(self.reg_n, "n");
        let z = self.builder.build_load(self.reg_z, "z");
        let v = self.builder.build_load(self.reg_v, "v");
        let c = self.builder.build_load(self.reg_c, "c");
        let function = self.compiler.module.get_function(&function_name(*addr)).unwrap();
        let res = self.builder.build_call(function, &[a.into(), x.into(), y.into(), n.into(), z.into(), v.into(), c.into()], "res").try_as_basic_value().unwrap_left().into_struct_value();
        let a = self.builder.build_extract_value(res, 0, "new_a").unwrap();
        self.builder.build_store(self.reg_a, a);
        let x = self.builder.build_extract_value(res, 1, "new_x").unwrap();
        self.builder.build_store(self.reg_x, x);
        let y = self.builder.build_extract_value(res, 2, "new_y").unwrap();
        self.builder.build_store(self.reg_y, y);
        let n = self.builder.build_extract_value(res, 3, "new_n").unwrap();
        self.builder.build_store(self.reg_n, n);
        let z = self.builder.build_extract_value(res, 4, "new_z").unwrap();
        self.builder.build_store(self.reg_z, z);
        let v = self.builder.build_extract_value(res, 5, "new_v").unwrap();
        self.builder.build_store(self.reg_v, v);
        let c = self.builder.build_extract_value(res, 6, "new_c").unwrap();
        self.builder.build_store(self.reg_c, c);
      }
      Instruction::Absolute { opcode: Opcode::LDA, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.load(self.reg_a, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::LDX, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.load(self.reg_x, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::LDY, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.load(self.reg_y, arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::LSR, addr, .. } => {
        let arg = self.absolute_value(*addr);
        let res = self.lsr(arg);
        self.write_mem(self.context.i16_type().const_int(*addr as u64, false), res);
        self.incr_clk(6);
      }
      Instruction::Absolute { opcode: Opcode::ORA, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.ora(arg);
      }
      Instruction::Absolute { opcode: Opcode::ROL, addr, .. } => {
        let arg = self.absolute_value(*addr);
        let res = self.rol(arg);
        self.write_mem(self.context.i16_type().const_int(*addr as u64, false), res);
        self.incr_clk(6);
      }
      Instruction::Absolute { opcode: Opcode::ROR, addr, .. } => {
        let arg = self.absolute_value(*addr);
        let res = self.ror(arg);
        self.write_mem(self.context.i16_type().const_int(*addr as u64, false), res);
        self.incr_clk(6);
      }
      Instruction::Absolute { opcode: Opcode::SBC, addr, .. } => {
        let arg = self.absolute_value(*addr);
        self.sbc(arg);
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.compiler.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::STX, addr, .. } => {
        self.store(self.reg_x, self.compiler.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      }
      Instruction::Absolute { opcode: Opcode::STY, addr, .. } => {
        self.store(self.reg_y, self.compiler.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      }
      Instruction::AbsoluteX { opcode: Opcode::ADC, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.adc(arg);
        self.incr_clk(4);
      }
      Instruction::AbsoluteX { opcode: Opcode::AND, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.and(arg);
        self.incr_clk(4);
      }
      Instruction::AbsoluteX { opcode: Opcode::ASL, addr, .. } => {
        let val = self.absolute_x_value(*addr);
        let val = self.asl(val);
        self.write_mem(self.absolute_x_addr(*addr), val);
        self.incr_clk(7);
      }
      Instruction::AbsoluteX { opcode: Opcode::CMP, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.compare(self.reg_a, arg);
      }
      Instruction::AbsoluteX { opcode: Opcode::DEC, addr, .. } => {
        self.decrement(self.absolute_x_addr(*addr));
      }
      Instruction::AbsoluteX { opcode: Opcode::EOR, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.eor(arg);
      }
      Instruction::AbsoluteX { opcode: Opcode::INC, addr, .. } => {
        self.increment(self.absolute_x_addr(*addr));
      }
      Instruction::AbsoluteX { opcode: Opcode::LDA, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.load(self.reg_a, arg);
      }
      Instruction::AbsoluteX { opcode: Opcode::LDY, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.load(self.reg_y, arg);
      }
      Instruction::AbsoluteX { opcode: Opcode::LSR, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        let res = self.lsr(arg);
        self.write_mem(self.absolute_x_addr(*addr), res);
      }
      Instruction::AbsoluteX { opcode: Opcode::ORA, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.ora(arg);
      }
      Instruction::AbsoluteX { opcode: Opcode::ROL, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        let res = self.rol(arg);
        self.write_mem(self.absolute_x_addr(*addr), res);
        self.incr_clk(6);
      }
      Instruction::AbsoluteX { opcode: Opcode::ROR, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        let res = self.ror(arg);
        self.write_mem(self.absolute_x_addr(*addr), res);
        self.incr_clk(6);
      }
      Instruction::AbsoluteX { opcode: Opcode::SBC, addr, .. } => {
        let arg = self.absolute_x_value(*addr);
        self.sbc(arg);
        self.incr_clk(4);
      }
      Instruction::AbsoluteX { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.absolute_x_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::AbsoluteY { opcode: Opcode::ADC, addr, .. } => {
        let addr = self.absolute_y_value(*addr);
        self.adc(addr);
        self.incr_clk(4);
      }
      Instruction::AbsoluteY { opcode: Opcode::AND, addr, .. } => {
        let addr = self.absolute_y_value(*addr);
        self.and(addr);
        self.incr_clk(4);
      }
      Instruction::AbsoluteY { opcode: Opcode::CMP, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.compare(self.reg_a, arg);
      }
      Instruction::AbsoluteY { opcode: Opcode::EOR, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.eor(arg);
      }
      Instruction::AbsoluteY { opcode: Opcode::LDA, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.load(self.reg_a, arg);
      }
      Instruction::AbsoluteY { opcode: Opcode::LDX, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.load(self.reg_x, arg);
      }
      Instruction::AbsoluteY { opcode: Opcode::ORA, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.ora(arg);
      }
      Instruction::AbsoluteY { opcode: Opcode::SBC, addr, .. } => {
        let arg = self.absolute_y_value(*addr);
        self.sbc(arg);
        self.incr_clk(4);
      }
      Instruction::AbsoluteY { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.absolute_y_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::Accumulator { opcode: Opcode::ASL, .. } => {
        let val = self.builder.build_load(self.reg_a, "a").into_int_value();
        let val = self.asl(val);
        self.builder.build_store(self.reg_a, val);
        self.incr_clk(7);
      }
      Instruction::Accumulator { opcode: Opcode::LSR, .. } => {
        let val = self.builder.build_load(self.reg_a, "a").into_int_value();
        let val = self.lsr(val);
        self.builder.build_store(self.reg_a, val);
        self.incr_clk(7);
      }
      Instruction::Accumulator { opcode: Opcode::ROL, .. } => {
        let val = self.builder.build_load(self.reg_a, "a").into_int_value();
        let val = self.rol(val);
        self.builder.build_store(self.reg_a, val);
        self.incr_clk(7);
      }
      Instruction::Accumulator { opcode: Opcode::ROR, .. } => {
        let val = self.builder.build_load(self.reg_a, "a").into_int_value();
        let val = self.ror(val);
        self.builder.build_store(self.reg_a, val);
        self.incr_clk(7);
      }
      Instruction::Immediate { opcode: Opcode::ADC, val, .. } => {
        self.adc(self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::AND, val, .. } => {
        self.and(self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::CMP, val, .. } => {
        self.compare(self.reg_a, self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::CPX, val, .. } => {
        self.compare(self.reg_x, self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::CPY, val, .. } => {
        self.compare(self.reg_y, self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::EOR, val, .. } => {
        self.eor(self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::LDA, val, .. } => {
        self.load(self.reg_a, self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(2);
      }
      Instruction::Immediate { opcode: Opcode::LDX, val, .. } => {
        self.load(self.reg_x, self.context.i8_type().const_int(*val as u64, false));
      }
      Instruction::Immediate { opcode: Opcode::LDY, val, .. } => {
        self.load(self.reg_y, self.context.i8_type().const_int(*val as u64, false));
      }
      Instruction::Immediate { opcode: Opcode::ORA, val, .. } => {
        self.ora(self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Immediate { opcode: Opcode::SBC, val, .. } => {
        self.sbc(self.context.i8_type().const_int(*val as u64, false));
        self.incr_clk(4);
      }
      Instruction::Implied { opcode: Opcode::CLC, .. } => {
        self.builder.build_store(self.reg_c, self.context.bool_type().const_zero());
      }
      Instruction::Implied { opcode: Opcode::CLV, .. } => {
        self.builder.build_store(self.reg_v, self.context.bool_type().const_zero());
      }
      Instruction::Implied { opcode: Opcode::DEX, .. } => {
        self.decrement_reg(self.reg_x);
      }
      Instruction::Implied { opcode: Opcode::DEY, .. } => {
        self.decrement_reg(self.reg_y);
      }
      Instruction::Implied { opcode: Opcode::INX, .. } => {
        self.increment_reg(self.reg_x);
      }
      Instruction::Implied { opcode: Opcode::INY, .. } => {
        self.increment_reg(self.reg_y);
      }
      Instruction::Implied { opcode: Opcode::PHA, .. } => {
        let a = self.builder.build_load(self.reg_a, "a").into_int_value();
        self.push(a);
      }
      Instruction::Implied { opcode: Opcode::PLA, .. } => {
        let new_a = self.pop();
        self.builder.build_store(self.reg_a, new_a);
      }
      Instruction::Implied { opcode: Opcode::RTS, .. } => {
        self.pop();
        self.pop();
        let ret = function_return_type(self.context).const_zero();
        let a = self.builder.build_load(self.reg_a, "a");
        let ret = self.builder.build_insert_value(ret, a, 0, "ret").unwrap();
        let x = self.builder.build_load(self.reg_x, "x");
        let ret = self.builder.build_insert_value(ret, x, 1, "ret").unwrap();
        let y = self.builder.build_load(self.reg_y, "y");
        let ret = self.builder.build_insert_value(ret, y, 2, "ret").unwrap();
        let n = self.builder.build_load(self.reg_n, "n");
        let ret = self.builder.build_insert_value(ret, n, 3, "ret").unwrap();
        let z = self.builder.build_load(self.reg_z, "z");
        let ret = self.builder.build_insert_value(ret, z, 4, "ret").unwrap();
        let v = self.builder.build_load(self.reg_v, "v");
        let ret = self.builder.build_insert_value(ret, v, 5, "ret").unwrap();
        let c = self.builder.build_load(self.reg_c, "c");
        let ret = self.builder.build_insert_value(ret, c, 6, "ret").unwrap();
        self.builder.build_return(Some(&ret));
      }
      Instruction::Implied { opcode: Opcode::SEC, .. } => {
        self.builder.build_store(self.reg_c, self.context.bool_type().const_all_ones());
      }
      Instruction::Implied { opcode: Opcode::TAX, .. } => {
        self.transfer(self.reg_a, self.reg_x);
      }
      Instruction::Implied { opcode: Opcode::TAY, .. } => {
        self.transfer(self.reg_a, self.reg_y);
      }
      Instruction::Implied { opcode: Opcode::TXA, .. } => {
        self.transfer(self.reg_x, self.reg_a);
      }
      Instruction::Implied { opcode: Opcode::TYA, .. } => {
        self.transfer(self.reg_y, self.reg_a);
      }
      Instruction::IndirectX { opcode: Opcode::ADC, offset, .. } => {
        self.adc(self.indirect_x_value(*offset));
      }
      Instruction::IndirectX { opcode: Opcode::AND, offset, .. } => {
        self.and(self.indirect_x_value(*offset));
      }
      Instruction::IndirectX { opcode: Opcode::EOR, offset, .. } => {
        self.eor(self.indirect_x_value(*offset));
      }
      Instruction::IndirectX { opcode: Opcode::LDA, offset, .. } => {
        self.load(self.reg_a, self.indirect_x_value(*offset));
      }
      Instruction::IndirectX { opcode: Opcode::ORA, offset, .. } => {
        self.ora(self.indirect_x_value(*offset));
      }
      Instruction::IndirectX { opcode: Opcode::SBC, offset, .. } => {
        self.sbc(self.indirect_x_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::ADC, offset, .. } => {
        self.adc(self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::AND, offset, .. } => {
        self.and(self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::EOR, offset, .. } => {
        self.eor(self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::LDA, offset, .. } => {
        self.load(self.reg_a, self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::ORA, offset, .. } => {
        self.ora(self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::SBC, offset, .. } => {
        self.sbc(self.indirect_y_value(*offset));
      }
      Instruction::IndirectY { opcode: Opcode::STA, offset, .. } => {
        self.store(self.reg_a, self.indirect_y_addr(*offset));
      }
      inst@Instruction::Relative { opcode: Opcode::BCC, .. } => {
        let c = self.builder.build_load(self.reg_c, "c").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(c, next, branch);
      }
      inst@Instruction::Relative { opcode: Opcode::BCS, .. } => {
        let c = self.builder.build_load(self.reg_c, "c").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(c, branch, next);
      }
      inst@Instruction::Relative { opcode: Opcode::BEQ, .. } => {
        let z = self.builder.build_load(self.reg_z, "z").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(z, branch, next);
      }
      inst@Instruction::Relative { opcode: Opcode::BMI, .. } => {
        let n = self.builder.build_load(self.reg_n, "n").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(n, branch, next);
      }
      inst@Instruction::Relative { opcode: Opcode::BNE, .. } => {
        let z = self.builder.build_load(self.reg_z, "z").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(z, next, branch);
      }
      inst@Instruction::Relative { opcode: Opcode::BPL, .. } => {
        let n = self.builder.build_load(self.reg_n, "n").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(n, next, branch);
      }
      inst@Instruction::Relative { opcode: Opcode::BVC, .. } => {
        let v = self.builder.build_load(self.reg_v, "v").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(v, next, branch);
      }
      inst@Instruction::Relative { opcode: Opcode::BVS, .. } => {
        let v = self.builder.build_load(self.reg_v, "v").into_int_value();
        let next = self.blocks[&inst.next_addr()];
        let branch = self.blocks[&inst.branch_addr().unwrap()];
        self.builder.build_conditional_branch(v, branch, next);
      }
      Instruction::Switch { opcode: Opcode::SWA, targets, .. } => {
        let a = self.builder.build_load(self.reg_a, "a").into_int_value();
        self.builder.build_switch(a, *self.blocks.get(targets.get(0).unwrap()).unwrap(),
          &targets.iter()
            .enumerate()
            .map(|(i, tgt)| (self.context.i8_type().const_int(i as u64, false), *self.blocks.get(tgt).unwrap()))
            .collect::<Vec<_>>());
      }
      Instruction::Zeropage { opcode: Opcode::ADC, addr, .. } => {
        let arg = self.zeropage_value(*addr);
        self.adc(arg);
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::AND, addr, .. } => {
        let arg = self.zeropage_value(*addr);
        self.and(arg);
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::ASL, addr, .. } => {
        let arg = self.zeropage_value(*addr);
        let val = self.asl(arg);
        self.write_mem(self.zeropage_addr(*addr), val);
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::BIT, addr, .. } => {
        let arg = self.zeropage_value(*addr);
        self.bit(arg);
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::CMP, addr, .. } => {
        let arg = self.zeropage_value(*addr);
        self.compare(self.reg_a, arg);
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::DEC, addr, .. } => {
        self.decrement(self.zeropage_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::EOR, addr, .. } => {
        self.eor(self.zeropage_value(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::INC, addr, .. } => {
        self.increment(self.zeropage_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::LDA, addr, .. } => {
        self.load(self.reg_a, self.zeropage_value(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::LDX, addr, .. } => {
        self.load(self.reg_x, self.zeropage_value(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::LDY, addr, .. } => {
        self.load(self.reg_y, self.zeropage_value(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::LSR, addr, .. } => {
        let res = self.lsr(self.zeropage_value(*addr));
        self.write_mem(self.zeropage_addr(*addr), res);
        self.incr_clk(6);
      }
      Instruction::Zeropage { opcode: Opcode::ORA, addr, .. } => {
        self.ora(self.zeropage_value(*addr));
      }
      Instruction::Zeropage { opcode: Opcode::SBC, addr, .. } => {
        self.sbc(self.zeropage_value(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.zeropage_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::STX, addr, .. } => {
        self.store(self.reg_x, self.zeropage_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::Zeropage { opcode: Opcode::STY, addr, .. } => {
        self.store(self.reg_y, self.zeropage_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::ADC, addr, .. } => {
        self.adc(self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::AND, addr, .. } => {
        self.and(self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::ASL, addr, .. } => {
        let val = self.asl(self.zeropage_x_value(*addr));
        self.write_mem(self.zeropage_x_addr(*addr), val);
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::DEC, addr, .. } => {
        self.decrement(self.zeropage_x_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::EOR, addr, .. } => {
        self.eor(self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::INC, addr, .. } => {
        self.increment(self.zeropage_x_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::LDA, addr, .. } => {
        self.load(self.reg_a, self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::LDY, addr, .. } => {
        self.load(self.reg_y, self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::LSR, addr, .. } => {
        let res = self.lsr(self.zeropage_x_value(*addr));
        self.write_mem(self.zeropage_addr(*addr), res);
        self.incr_clk(6);
      }
      Instruction::ZeropageX { opcode: Opcode::ORA, addr, .. } => {
        self.ora(self.zeropage_x_value(*addr));
      }
      Instruction::ZeropageX { opcode: Opcode::SBC, addr, .. } => {
        self.sbc(self.zeropage_x_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageX { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.zeropage_x_addr(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageY { opcode: Opcode::LDX, addr, .. } => {
        self.load(self.reg_x, self.zeropage_y_value(*addr));
        self.incr_clk(4);
      }
      Instruction::ZeropageY { opcode: Opcode::STX, addr, .. } => {
        self.store(self.reg_x, self.zeropage_y_addr(*addr));
        self.incr_clk(4);
      }
      _ => {}
    }
  }

  fn absolute_value(&self, addr: u16) -> IntValue<'ctx> {
    let read_mem = self.compiler.module.get_function("readMem").unwrap();
    self.builder.build_call(read_mem,
      &[self.context.i16_type().const_int(addr as u64, false).into()], "absVal")
      .try_as_basic_value().unwrap_left().into_int_value()
  }

  fn set_n_z(&self, val: IntValue) {
    self.set_n(val);
    self.set_z(val);
  }

  fn set_n(&self, val: IntValue) {
    let n = self.builder.build_int_compare(IntPredicate::UGT, val, self.context.i8_type().const_int(0x7f, false), "n");
    self.builder.build_store(self.reg_n, n);
  }

  fn set_z(&self, val: IntValue) {
    let z = self.builder.build_int_compare(IntPredicate::EQ, val, self.context.i8_type().const_int(0, false), "z");
    self.builder.build_store(self.reg_z, z);
  }

  fn adc(&self, arg: IntValue<'ctx>) {
    let c = self.builder.build_load(self.reg_c, "c").into_int_value();
    let c_ext = self.builder.build_int_z_extend(c, self.context.i8_type(), "c_ext");
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let (a2, c2, v2) = self.add_carry(a, c_ext);
    let (a3, c3, v3) = self.add_carry(a2, arg);
    self.builder.build_store(self.reg_a, a3);
    self.set_n_z(a3);
    let c_new = self.builder.build_or(c2, c3, "c_new");
    self.builder.build_store(self.reg_c, c_new);
    let v_new = self.builder.build_or(v2, v3, "v_new");
    self.builder.build_store(self.reg_v, v_new);
  }

  fn add_carry(&self, arg1: IntValue<'ctx>, arg2: IntValue<'ctx>) -> (IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let uadd = self.compiler.module.get_function("llvm.uadd.with.overflow.i8").unwrap();
    let sadd = self.compiler.module.get_function("llvm.sadd.with.overflow.i8").unwrap();
    let res_u = self.compiler.builder.build_call(uadd, &[arg1.into(), arg2.into()], "res_u").try_as_basic_value().unwrap_left().into_struct_value();
    let res_s = self.compiler.builder.build_call(sadd, &[arg1.into(), arg2.into()], "res_s").try_as_basic_value().unwrap_left().into_struct_value();
    let res = self.compiler.builder.build_extract_value(res_u, 0, "res").unwrap().into_int_value();
    let c = self.compiler.builder.build_extract_value(res_u, 1, "c").unwrap().into_int_value();
    let v = self.compiler.builder.build_extract_value(res_s, 1, "v").unwrap().into_int_value();
    (res, c, v)
  }

  fn sub_carry(&self, arg1: IntValue<'ctx>, arg2: IntValue<'ctx>) -> (IntValue<'ctx>, IntValue<'ctx>, IntValue<'ctx>) {
    let usub = self.compiler.module.get_function("llvm.usub.with.overflow.i8").unwrap();
    let ssub = self.compiler.module.get_function("llvm.ssub.with.overflow.i8").unwrap();
    let res_u = self.compiler.builder.build_call(usub, &[arg1.into(), arg2.into()], "res_u").try_as_basic_value().unwrap_left().into_struct_value();
    let res_s = self.compiler.builder.build_call(ssub, &[arg1.into(), arg2.into()], "res_s").try_as_basic_value().unwrap_left().into_struct_value();
    let res = self.compiler.builder.build_extract_value(res_u, 0, "res").unwrap().into_int_value();
    let c = self.compiler.builder.build_extract_value(res_u, 1, "c").unwrap().into_int_value();
    let v = self.compiler.builder.build_extract_value(res_s, 1, "v").unwrap().into_int_value();
    (res, c, v)
  }

  fn and(&self, arg: IntValue) {
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let result = self.builder.build_and(arg, a, "res");
    self.builder.build_store(self.reg_a, result);
    self.set_n_z(result);
  }

  fn asl(&self, arg: IntValue<'ctx>) -> IntValue<'ctx> {
    let new_c1 = self.builder.build_right_shift(arg, self.context.i8_type().const_int(7, false), false, "new_c1");
    let new_c2 = self.builder.build_int_truncate(new_c1, self.context.bool_type(), "new_c2");
    let new_val = self.builder.build_left_shift(arg, self.context.i8_type().const_int(1, false), "new_val");
    self.builder.build_store(self.reg_c, new_c2);
    self.set_n_z(new_val);
    new_val
  }

  fn bit(&self, arg: IntValue) {
    let i8_ty = self.compiler.context.i8_type();
    let i1_ty = self.compiler.context.bool_type();
    let new_n = self.builder.build_right_shift(arg, i8_ty.const_int(7, false), false, "new_n");
    let new_n_trunc = self.builder.build_int_truncate(new_n, i1_ty, "new_n_trunc");
    self.builder.build_store(self.reg_n, new_n_trunc);
    let new_v = self.builder.build_right_shift(arg, i8_ty.const_int(6, false), false, "new_v");
    let new_v_trunc = self.builder.build_int_truncate(new_v, i1_ty, "new_v_trunc");
    self.builder.build_store(self.reg_v, new_v_trunc);
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let res = self.builder.build_and(a, arg, "res");
    self.builder.build_store(self.reg_a, res);
    self.set_z(res);
  }

  fn compare(&self, reg: PointerValue, arg: IntValue) {
    let reg_val = self.builder.build_load(reg, "reg_val").into_int_value();
    let res = self.builder.build_int_sub(reg_val, arg, "res");
    self.set_n_z(res);
    let new_c = self.builder.build_int_compare(IntPredicate::UGE, reg_val, arg, "new_c");
    self.builder.build_store(self.reg_c, new_c);
  }

  fn decrement(&self, addr: IntValue<'ctx>) {
    let val = self.read_mem(addr);
    let val_new = self.builder.build_int_sub(val, self.context.i8_type().const_int(1, false), "val_new");
    self.set_n_z(val_new);
    self.write_mem(addr, val_new);
  }

  fn decrement_reg(&self, reg: PointerValue) {
    let val = self.builder.build_load(reg, "val").into_int_value();
    let val_new = self.builder.build_int_sub(val, self.context.i8_type().const_int(1, false), "val_new");
    self.set_n_z(val_new);
    self.builder.build_store(reg, val_new);
  }

  fn eor(&self, arg: IntValue) {
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let res = self.builder.build_xor(a, arg, "res");
    self.builder.build_store(self.reg_a, res);
    self.set_n_z(res);
  }

  fn increment(&self, addr: IntValue<'ctx>) {
    let val = self.read_mem(addr);
    let val_new = self.builder.build_int_add(val, self.context.i8_type().const_int(1, false), "val_new");
    self.set_n_z(val_new);
    self.write_mem(addr, val_new);
  }

  fn increment_reg(&self, reg: PointerValue) {
    let val = self.builder.build_load(reg, "val").into_int_value();
    let val_new = self.builder.build_int_add(val, self.context.i8_type().const_int(1, false), "val_new");
    self.set_n_z(val_new);
    self.builder.build_store(reg, val_new);
  }

  fn load(&self, reg: PointerValue, val: IntValue) {
    self.builder.build_store(reg, val);
    self.set_n_z(val);
  }

  fn lsr(&self, val: IntValue<'ctx>) -> IntValue<'ctx> {
    let new_c = self.builder.build_int_truncate(val, self.context.bool_type(), "new_c");
    self.builder.build_store(self.reg_c, new_c);
    let new_val = self.builder.build_right_shift(val, self.context.i8_type().const_int(1, false), false, "new_val");
    self.set_n_z(new_val);
    new_val
  }

  fn ora(&self, val: IntValue) {
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let new_a = self.builder.build_or(a, val, "new_a");
    self.builder.build_store(self.reg_a, new_a);
    self.set_n_z(new_a);
  }

  fn push(&self, val: IntValue) {
    let reg_s = self.compiler.module.get_global("sp").unwrap().as_pointer_value();
    let s = self.builder.build_load(reg_s, "s").into_int_value();
    let big_s = self.builder.build_int_z_extend(s, self.context.i16_type(), "big_s");
    let mem_loc = self.builder.build_int_add(big_s, self.context.i16_type().const_int(0x100, false), "mem_loc");
    self.write_mem(mem_loc, val);
    let new_s = self.builder.build_int_add(s, self.context.i8_type().const_int(1, false), "new_s");
    self.builder.build_store(reg_s, new_s);
  }

  fn pop(&self) -> IntValue {
    let reg_s = self.compiler.module.get_global("sp").unwrap().as_pointer_value();
    let s = self.builder.build_load(reg_s, "s").into_int_value();
    let new_s = self.builder.build_int_sub(s, self.context.i8_type().const_int(1, false), "new_s");
    let big_s = self.builder.build_int_z_extend(new_s, self.context.i16_type(), "big_s");
    self.builder.build_store(reg_s, new_s);
    self.read_mem(big_s)
  }

  fn read_mem(&self, addr: IntValue<'ctx>) -> IntValue<'ctx> {
    let read_mem = self.compiler.module.get_function("readMem").unwrap();
    self.builder.build_call(read_mem, &[addr.into()], "res").try_as_basic_value().unwrap_left().into_int_value()
  }

  fn rol(&self, val: IntValue<'ctx>) -> IntValue<'ctx> {
    let c = self.builder.build_load(self.reg_c, "c").into_int_value();
    let new_c1 = self.builder.build_right_shift(val, self.context.i8_type().const_int(7, false), false, "new_c1");
    let new_c2 = self.builder.build_int_truncate(new_c1, self.context.bool_type(), "new_c2");
    let c_ext = self.builder.build_int_s_extend(c, self.context.i8_type(), "c_ext");
    let new_val1 = self.builder.build_left_shift(val, self.context.i8_type().const_int(1, false), "new_val1");
    let new_val = self.builder.build_or(new_val1, c_ext, "new_val");
    self.builder.build_store(self.reg_c, new_c2);
    self.set_n_z(new_val);
    new_val
  }

  fn ror(&self, val: IntValue<'ctx>) -> IntValue<'ctx> {
    let c = self.builder.build_load(self.reg_c, "c").into_int_value();
    let new_c = self.builder.build_int_truncate(val, self.context.bool_type(), "new_c");
    let c_ext = self.builder.build_int_s_extend(c, self.context.i8_type(), "c_ext");
    let c_ext2 = self.builder.build_and(c_ext, self.context.i8_type().const_int(0x80, false), "c_ext2");
    let new_val1 = self.builder.build_right_shift(val, self.context.i8_type().const_int(1, false), false, "new_val1");
    let new_val = self.builder.build_or(new_val1, c_ext2, "new_val");
    self.builder.build_store(self.reg_c, new_c);
    self.set_n_z(new_val);
    new_val
  }

  fn sbc(&self, val: IntValue<'ctx>) {
    let c = self.builder.build_load(self.reg_c, "c").into_int_value();
    let c_ext = self.builder.build_int_z_extend(c, self.context.i8_type(), "c_ext");
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let (a2, c2, v2) = self.sub_carry(a, c_ext);
    let (a3, c3, v3) = self.sub_carry(a2, val);
    self.builder.build_store(self.reg_a, a3);
    self.set_n_z(a3);
    let c_new = self.builder.build_or(c2, c3, "c_new");
    self.builder.build_store(self.reg_c, c_new);
    let v_new = self.builder.build_or(v2, v3, "c_new");
    self.builder.build_store(self.reg_v, v_new);
  }

  fn store(&self, reg: PointerValue, addr: IntValue) {
    let reg_val = self.builder.build_load(reg, "reg_val").into_int_value();
    self.write_mem(addr, reg_val);
  }

  fn transfer(&self, source: PointerValue, target: PointerValue) {
    let val = self.builder.build_load(source, "val").into_int_value();
    self.builder.build_store(target, val);
    self.set_n_z(val);
  }

  fn write_mem(&self, addr: IntValue, val: IntValue) {
    let write_mem = self.compiler.module.get_function("writeMem").unwrap();
    self.builder.build_call(write_mem, &[addr.into(), val.into()], "res");
  }

  fn incr_clk(&self, cycles: u64) {
    /*let clk = self.builder.build_load(self.reg_clk, "clk").into_int_value();
    let clk = self.builder.build_int_add(clk, self.context.i16_type().const_int(cycles, false), "newClk");
    self.builder.build_store(self.reg_clk, clk);*/
  }

  fn set_first_instruction(&self, addr: u16) {
    self.builder.position_at_end(self.function.get_first_basic_block().unwrap());
    self.builder.build_unconditional_branch(*self.blocks.get(&addr).unwrap());
  }

  fn absolute_x_addr(&self, addr: u16) -> IntValue<'ctx> {
    let x = self.builder.build_load(self.reg_x, "x").into_int_value();
    let x_ext = self.builder.build_int_z_extend(x, self.context.i16_type(), "x_ext");
    self.builder.build_int_add(x_ext, self.context.i16_type().const_int(addr as u64, false), "addr")
  }

  fn absolute_x_value(&self, addr: u16) -> IntValue<'ctx> {
    self.read_mem(self.absolute_x_addr(addr))
  }

  fn absolute_y_addr(&self, addr: u16) -> IntValue<'ctx> {
    let y = self.builder.build_load(self.reg_y, "y").into_int_value();
    let y_ext = self.builder.build_int_z_extend(y, self.context.i16_type(), "y_ext");
    self.builder.build_int_add(y_ext, self.context.i16_type().const_int(addr as u64, false), "addr")
  }

  fn absolute_y_value(&self, addr: u16) -> IntValue<'ctx> {
    self.read_mem(self.absolute_y_addr(addr))
  }

  fn concat(&self, low: IntValue<'ctx>, high: IntValue<'ctx>) -> IntValue<'ctx> {
    let low_ext = self.builder.build_int_z_extend(low, self.context.i16_type(), "low_ext");
    let high_ext = self.builder.build_int_z_extend(high, self.context.i16_type(), "high_ext");
    let high_shift = self.builder.build_left_shift(high_ext, self.context.i16_type().const_int(8, false), "high_shift");
    self.builder.build_or(low_ext, high_shift, "concat")
  }

  fn indirect_x_addr(&self, offset: u8) -> IntValue<'ctx> {
    let x = self.builder.build_load(self.reg_x, "x").into_int_value();
    let addr_src = self.builder.build_int_add(x, self.context.i8_type().const_int(offset as u64, false), "addr_src");
    let addr_low = self.builder.build_int_z_extend(addr_src, self.context.i16_type(), "addr_low");
    let low = self.read_mem(addr_low);
    let addr_high = self.builder.build_int_add(addr_low, self.context.i16_type().const_int(1, false), "addr_high");
    let high = self.read_mem(addr_high);
    self.concat(low, high)
  }

  fn indirect_x_value(&self, offset: u8) -> IntValue<'ctx> {
    self.read_mem(self.indirect_x_addr(offset))
  }

  fn indirect_y_addr(&self, offset: u8) -> IntValue<'ctx> {
    let addr_low = self.context.i16_type().const_int(offset as u64, false);
    let low = self.read_mem(addr_low);
    let addr_high = self.builder.build_int_add(addr_low, self.context.i16_type().const_int(1, false), "addr_high");
    let high = self.read_mem(addr_high);
    let addr_base = self.concat(low, high);
    let y = self.builder.build_load(self.reg_y, "y").into_int_value();
    let y_ext = self.builder.build_int_z_extend(y, self.context.i16_type(), "y_ext");
    self.builder.build_int_add(addr_base, y_ext, "addr")
  }

  fn indirect_y_value(&self, offset: u8) -> IntValue<'ctx> {
    self.read_mem(self.indirect_y_addr(offset))
  }

  fn zeropage_addr(&self, offset: u8) -> IntValue<'ctx> {
    self.builder.build_int_z_extend(self.context.i8_type().const_int(offset as u64, false), self.context.i16_type(), "addr")
  }

  fn zeropage_value(&self, offset: u8) -> IntValue <'ctx> {
    self.read_mem(self.zeropage_addr(offset))
  }

  fn zeropage_x_addr(&self, offset: u8) -> IntValue<'ctx> {
    let x = self.builder.build_load(self.reg_x, "x").into_int_value();
    let x_sum = self.builder.build_int_add(x, self.context.i8_type().const_int(offset as u64, false), "x_sum");
    self.builder.build_int_z_extend(x_sum, self.context.i16_type(), "addr")
  }

  fn zeropage_x_value(&self, offset: u8) -> IntValue<'ctx> {
    self.read_mem(self.zeropage_x_addr(offset))
  }

  fn zeropage_y_addr(&self, offset: u8) -> IntValue<'ctx> {
    let y = self.builder.build_load(self.reg_y, "y").into_int_value();
    let y_sum = self.builder.build_int_add(y, self.context.i8_type().const_int(offset as u64, false), "y_sum");
    self.builder.build_int_z_extend(y_sum, self.context.i16_type(), "addr")
  }

  fn zeropage_y_value(&self, offset: u8) -> IntValue<'ctx> {
    self.read_mem(self.zeropage_y_addr(offset))
  }
}
