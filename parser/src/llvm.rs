use std::collections::BTreeMap;

use inkwell::{module::{Module, Linkage}, context::Context, values::{FunctionValue, BasicValueEnum, PointerValue, IntValue}, attributes::{AttributeLoc, Attribute}, AddressSpace, builder::Builder, basic_block::BasicBlock, IntPredicate, types::StructType};

use crate::{assembly::{Instruction, Opcode}};

const INACCESSIBLE_MEM_ONLY: u32 = 9;
const NO_ALIAS: u32 = 16;
const NO_CAPTURE: u32 = 18;
const NO_UNWIND: u32 = 30;
const READ_ONLY: u32 = 38;
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
    result
  }

  fn add_read_mem_decl(&self) {
    let fn_type = self.context.i8_type().fn_type(&[self.context.i16_type().into()], false);
    let result = self.module.add_function("readMem", fn_type, None);
    let read_only = self.context.create_enum_attribute(READ_ONLY, 0);
    let inaccessible_mem_only = self.context.create_enum_attribute(INACCESSIBLE_MEM_ONLY, 0);
    let no_unwind = self.context.create_enum_attribute(NO_UNWIND, 0);
    let will_return = self.context.create_enum_attribute(WILL_RETURN, 0);
    result.add_attribute(AttributeLoc::Function, read_only);
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

  pub fn declare_func(&mut self, addr: u16) {
    let i8_reg_type = self.context.i8_type().ptr_type(AddressSpace::Generic).into();
    let i1_reg_type = self.context.bool_type().ptr_type(AddressSpace::Generic).into();
    let i16_reg_type = self.context.i16_type().ptr_type(AddressSpace::Generic).into();
    let fn_type = self.context.void_type().fn_type(&[
      i8_reg_type, // A
      i8_reg_type, // X
      i8_reg_type, // Y
      i1_reg_type, // N
      i1_reg_type, // Z
      i1_reg_type, // V
      i1_reg_type, // C
      i8_reg_type, // S
      i16_reg_type, // Clk
      ], false);
    let func = self.module.add_function(&function_name(addr), fn_type, Some(Linkage::Private));
    let no_alias = self.context.create_enum_attribute(NO_ALIAS, 0);
    let no_capture = self.context.create_enum_attribute(NO_CAPTURE, 0);
    for i in 0..=8 {
      func.add_attribute(AttributeLoc::Param(i), no_alias);
      func.add_attribute(AttributeLoc::Param(i), no_capture);
    }
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
    let function = self.module.add_function(name, fn_type, None);
    let entry = self.context.append_basic_block(function, "entry");
    self.builder.position_at_end(entry);
    let reg_a = self.builder.build_alloca(self.context.i8_type(), "regA");
    let reg_x = self.builder.build_alloca(self.context.i8_type(), "regX");
    let reg_y = self.builder.build_alloca(self.context.i8_type(), "regY");
    let reg_n = self.builder.build_alloca(self.context.bool_type(), "regN");
    let reg_z = self.builder.build_alloca(self.context.bool_type(), "regZ");
    let reg_v = self.builder.build_alloca(self.context.bool_type(), "regV");
    let reg_c = self.builder.build_alloca(self.context.bool_type(), "regC");
    let reg_s = self.builder.build_alloca(self.context.i8_type(), "regS");
    let reg_clk = self.builder.build_alloca(self.context.i16_type(), "clk");
    self.builder.build_store(reg_s, self.context.i8_type().const_int(0, false));
    self.builder.build_store(reg_clk, self.context.i16_type().const_int(0, false));
    let inner = self.module.get_function(&function_name(addr)).unwrap();
    self.builder.build_call(inner, &[reg_a.into(), reg_x.into(), reg_y.into(), reg_n.into(),
      reg_z.into(), reg_v.into(), reg_c.into(), reg_s.into(), reg_clk.into()], "res");
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
  reg_s: PointerValue<'ctx>,
  reg_clk: PointerValue<'ctx>,
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
    let reg_s = compiler.builder.build_alloca(compiler.context.i8_type(), "regS");
    let reg_clk = compiler.builder.build_alloca(compiler.context.i16_type(), "clk");
    let reg_a_arg = compiler.builder.build_load(function.get_nth_param(0).unwrap().into_pointer_value(), "regA");
    compiler.builder.build_store(reg_a, reg_a_arg);
    let reg_x_arg = compiler.builder.build_load(function.get_nth_param(1).unwrap().into_pointer_value(), "regX");
    compiler.builder.build_store(reg_x, reg_x_arg);
    let reg_y_arg = compiler.builder.build_load(function.get_nth_param(2).unwrap().into_pointer_value(), "regY");
    compiler.builder.build_store(reg_y, reg_y_arg);
    let reg_n_arg = compiler.builder.build_load(function.get_nth_param(3).unwrap().into_pointer_value(), "regN");
    compiler.builder.build_store(reg_n, reg_n_arg);
    let reg_z_arg = compiler.builder.build_load(function.get_nth_param(4).unwrap().into_pointer_value(), "regZ");
    compiler.builder.build_store(reg_z, reg_z_arg);
    let reg_v_arg = compiler.builder.build_load(function.get_nth_param(5).unwrap().into_pointer_value(), "regV");
    compiler.builder.build_store(reg_v, reg_v_arg);
    let reg_c_arg = compiler.builder.build_load(function.get_nth_param(6).unwrap().into_pointer_value(), "regC");
    compiler.builder.build_store(reg_c, reg_c_arg);
    let reg_s_arg = compiler.builder.build_load(function.get_nth_param(7).unwrap().into_pointer_value(), "regS");
    compiler.builder.build_store(reg_s, reg_s_arg);
    let reg_clk_arg = compiler.builder.build_load(function.get_nth_param(8).unwrap().into_pointer_value(), "regClk");
    compiler.builder.build_store(reg_clk, reg_clk_arg);

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
      reg_s,
      reg_clk,
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
        self.builder.build_return(None);
      }
    }
  }

  fn write_instruction(&self, inst: &Instruction) {
    match inst {
      Instruction::Absolute { opcode: Opcode::ADC, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.adc(arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::AND, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.and(arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::ASL, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.asl(arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::BIT, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.bit(arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::CMP, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.compare(self.reg_a, arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::CPX, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.compare(self.reg_x, arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::CPY, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.compare(self.reg_y, arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::DEC, addr, .. } => {
        self.decrement(self.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::EOR, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.eor(arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::INC, addr, .. } => {
        self.increment(self.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::STA, addr, .. } => {
        self.store(self.reg_a, self.compiler.context.i16_type().const_int(*addr as u64, false));
        self.incr_clk(6);
      },
      Instruction::Absolute { opcode: Opcode::JMP, addr, .. } => {
        self.builder.build_unconditional_branch(*self.blocks.get(addr).unwrap());
      },
      Instruction::Absolute { loc, opcode: Opcode::JSR, addr, ..} => {
        self.incr_clk(6);
        self.push(self.context.i8_type().const_int((*loc & 0xFF) as u64, false));
        self.push(self.context.i8_type().const_int(((*loc >> 8) & 0xFF) as u64, false));
        let a = self.builder.build_load(self.reg_a, "a");
        self.builder.build_store(self.function.get_nth_param(0).unwrap().into_pointer_value(), a);
        let x = self.builder.build_load(self.reg_x, "x");
        self.builder.build_store(self.function.get_nth_param(1).unwrap().into_pointer_value(), x);
        let y = self.builder.build_load(self.reg_y, "y");
        self.builder.build_store(self.function.get_nth_param(2).unwrap().into_pointer_value(), y);
        let n = self.builder.build_load(self.reg_n, "n");
        self.builder.build_store(self.function.get_nth_param(3).unwrap().into_pointer_value(), n);
        let z = self.builder.build_load(self.reg_z, "z");
        self.builder.build_store(self.function.get_nth_param(4).unwrap().into_pointer_value(), z);
        let v = self.builder.build_load(self.reg_v, "v");
        self.builder.build_store(self.function.get_nth_param(5).unwrap().into_pointer_value(), v);
        let c = self.builder.build_load(self.reg_c, "c");
        self.builder.build_store(self.function.get_nth_param(6).unwrap().into_pointer_value(), c);
        let s = self.builder.build_load(self.reg_s, "s");
        self.builder.build_store(self.function.get_nth_param(7).unwrap().into_pointer_value(), s);
        let clk = self.builder.build_load(self.reg_clk, "clk");
        self.builder.build_store(self.function.get_nth_param(8).unwrap().into_pointer_value(), clk);
        let function = self.compiler.module.get_function(&function_name(*addr)).unwrap();
        self.builder.build_call(function, &self.function.get_params().into_iter().map(|p| p.into()).collect::<Vec<_>>(), "res");
        let a = self.builder.build_load(self.function.get_nth_param(0).unwrap().into_pointer_value(), "a");
        self.builder.build_store(self.reg_a, a);
        let x = self.builder.build_load(self.function.get_nth_param(1).unwrap().into_pointer_value(), "x");
        self.builder.build_store(self.reg_x, x);
        let y = self.builder.build_load(self.function.get_nth_param(2).unwrap().into_pointer_value(), "y");
        self.builder.build_store(self.reg_y, y);
        let n = self.builder.build_load(self.function.get_nth_param(3).unwrap().into_pointer_value(), "n");
        self.builder.build_store(self.reg_n, n);
        let z = self.builder.build_load(self.function.get_nth_param(4).unwrap().into_pointer_value(), "z");
        self.builder.build_store(self.reg_z, z);
        let v = self.builder.build_load(self.function.get_nth_param(5).unwrap().into_pointer_value(), "v");
        self.builder.build_store(self.reg_v, v);
        let c = self.builder.build_load(self.function.get_nth_param(6).unwrap().into_pointer_value(), "c");
        self.builder.build_store(self.reg_c, c);
        let s = self.builder.build_load(self.function.get_nth_param(7).unwrap().into_pointer_value(), "s");
        self.builder.build_store(self.reg_s, s);
        let clk = self.builder.build_load(self.function.get_nth_param(8).unwrap().into_pointer_value(), "clk");
        self.builder.build_store(self.reg_clk, clk);
      },
      Instruction::Absolute { opcode: Opcode::LDA, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.load(self.reg_a, arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::LDX, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.load(self.reg_x, arg);
        self.incr_clk(4);
      },
      Instruction::Absolute { opcode: Opcode::LDY, addr, .. } => {
        let arg = self.absolute_value(*addr).into_int_value();
        self.load(self.reg_y, arg);
        self.incr_clk(4);
      },
      _ => {}
    }
  }

  fn absolute_value(&self, addr: u16) -> BasicValueEnum<'ctx> {
    let read_mem = self.compiler.module.get_function("readMem").unwrap();
    self.builder.build_call(read_mem,
      &[self.context.i16_type().const_int(addr as u64, false).into()], "absVal")
      .try_as_basic_value().unwrap_left()
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

  fn and(&self, arg: IntValue) {
    let a = self.builder.build_load(self.reg_a, "a").into_int_value();
    let result = self.builder.build_and(arg, a, "res");
    self.builder.build_store(self.reg_a, result);
    self.set_n_z(result);
  }

  fn asl(&self, arg: IntValue) {
    let new_c1 = self.builder.build_right_shift(arg, self.context.i8_type().const_int(7, false), false, "new_c1");
    let new_c2 = self.builder.build_int_truncate(new_c1, self.context.bool_type(), "new_c2");
    let new_val = self.builder.build_left_shift(arg, self.context.i8_type().const_int(1, false), "new_val");
    self.builder.build_store(self.reg_c, new_c2);
    self.set_n_z(new_val);
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
    self.write_mem(addr, val_new);
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
    self.write_mem(addr, val_new);
  }

  fn load(&self, reg: PointerValue, val: IntValue) {
    self.builder.build_store(reg, val);
    self.set_n_z(val);
  }

  fn push(&self, val: IntValue) {
    let s = self.builder.build_load(self.reg_s, "s").into_int_value();
    let big_s = self.builder.build_int_z_extend(s, self.context.i16_type(), "big_s");
    let mem_loc = self.builder.build_int_add(big_s, self.context.i16_type().const_int(0x100, false), "mem_loc");
    self.write_mem(mem_loc, val);
    let new_s = self.builder.build_int_add(s, self.context.i8_type().const_int(1, false), "new_s");
    self.builder.build_store(self.reg_s, new_s);
  }

  fn read_mem(&self, addr: IntValue<'ctx>) -> IntValue<'ctx> {
    let read_mem = self.compiler.module.get_function("readMem").unwrap();
    self.builder.build_call(read_mem, &[addr.into()], "res").try_as_basic_value().unwrap_left().into_int_value()
  }

  fn store(&self, reg: PointerValue, addr: IntValue) {
    let reg_val = self.builder.build_load(reg, "reg_val").into_int_value();
    self.write_mem(addr, reg_val);
  }

  fn write_mem(&self, addr: IntValue, val: IntValue) {
    let write_mem = self.compiler.module.get_function("writeMem").unwrap();
    self.builder.build_call(write_mem, &[addr.into(), val.into()], "res");
  }

  fn incr_clk(&self, cycles: u64) {
    let clk = self.builder.build_load(self.reg_clk, "clk").into_int_value();
    let clk = self.builder.build_int_add(clk, self.context.i16_type().const_int(cycles, false), "newClk");
    self.builder.build_store(self.reg_clk, clk);
  }

  fn set_first_instruction(&self, addr: u16) {
    self.builder.position_at_end(self.function.get_first_basic_block().unwrap());
    self.builder.build_unconditional_branch(*self.blocks.get(&addr).unwrap());
  }
}