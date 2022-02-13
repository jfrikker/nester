use inkwell::{context::Context, module::{Module, Linkage}, attributes::AttributeLoc, IntPredicate};
use nom::{IResult, bytes::complete::take};

use crate::{llvm::{INACCESSIBLE_MEM_ONLY, NO_UNWIND, WILL_RETURN}, mapper::{Mapper, add_rom, add_ram}};

pub struct Mapper0<'a> {
  prg_rom: &'a [u8],
  chr_rom: &'a [u8],
}

impl <'a> Mapper0<'a> {
  pub fn read(contents: &'a [u8]) -> IResult<&'a[u8], Self> {
    let (contents, _) = take(4usize)(contents)?;
    let (contents, prg_size) = nom::number::complete::u8(contents)?;
    let (contents, chr_size) = nom::number::complete::u8(contents)?;
    let (contents, _) = take(10usize)(contents)?;
    let (contents, prg_rom) = take((prg_size as usize) * 16384)(contents)?;
    let (contents, chr_rom) = take((chr_size as usize) * 8192)(contents)?;
    Ok((contents,
      Self {
      prg_rom,
      chr_rom
    }))
  }

  fn add_read_mem_decl<'ctx>(&self, context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let i8_ty = context.i8_type();
    let i16_ty = context.i16_type();
    let fn_type = i8_ty.fn_type(&[i16_ty.into()], false);
    let read_mem_external = module.add_function("readMemExternal", fn_type, None);
    let inaccessible_mem_only = context.create_enum_attribute(INACCESSIBLE_MEM_ONLY, 0);
    let no_unwind = context.create_enum_attribute(NO_UNWIND, 0);
    let will_return = context.create_enum_attribute(WILL_RETURN, 0);
    read_mem_external.add_attribute(AttributeLoc::Function, inaccessible_mem_only);
    read_mem_external.add_attribute(AttributeLoc::Function, no_unwind);
    read_mem_external.add_attribute(AttributeLoc::Function, will_return);

    let read_mem = module.add_function("readMem", fn_type, None);
    read_mem.set_linkage(Linkage::Private);
    let entry = context.append_basic_block(read_mem, "entry");
    let if_low = context.append_basic_block(read_mem, "if_low");
    let else_low = context.append_basic_block(read_mem, "else_low");
    let if_external = context.append_basic_block(read_mem, "if_external");
    let else_external = context.append_basic_block(read_mem, "else_external");
    let if_high = context.append_basic_block(read_mem, "if_high");
    let else_high = context.append_basic_block(read_mem, "else_high");

    let addr = read_mem.get_nth_param(0).unwrap().into_int_value();
    builder.position_at_end(entry);
    let cond = builder.build_int_compare(IntPredicate::ULT, addr, i16_ty.const_int(0x2000, false), "cond");
    builder.build_conditional_branch(cond, if_low, else_low);

    builder.position_at_end(if_low);
    let addr2 = builder.build_int_unsigned_rem(addr, i16_ty.const_int(0x800, false), "addr");
    let ptr = unsafe {
      builder.build_gep(module.get_global("lowMem").unwrap().as_pointer_value(), &[i16_ty.const_zero(), addr2], "ptr")
    };
    let val = builder.build_load(ptr, "val");
    builder.build_return(Some(&val));

    builder.position_at_end(else_low);
    let cond = builder.build_int_compare(IntPredicate::ULT, addr, i16_ty.const_int(0x4020, false), "cond");
    builder.build_conditional_branch(cond, if_external, else_external);

    builder.position_at_end(if_external);
    let val = builder.build_call(read_mem_external, &[addr.into()], "val").try_as_basic_value().unwrap_left();
    builder.build_return(Some(&val));

    builder.position_at_end(else_external);
    let cond = builder.build_int_compare(IntPredicate::ULT, addr, i16_ty.const_int(0x8000, false), "cond");
    builder.build_conditional_branch(cond, if_high, else_high);

    builder.position_at_end(if_high);
    let addr2 = builder.build_int_sub(addr, i16_ty.const_int(0x4000, false), "addr");
    let ptr = unsafe {
      builder.build_gep(module.get_global("highMem").unwrap().as_pointer_value(), &[i16_ty.const_zero(), addr2], "ptr")
    };
    let val = builder.build_load(ptr, "val");
    builder.build_return(Some(&val));

    builder.position_at_end(else_high);
    let addr2 = builder.build_int_sub(addr, i16_ty.const_int(0x8000, false), "addr");
    let ptr = unsafe {
      builder.build_gep(module.get_global("prgRom").unwrap().as_pointer_value(), &[i16_ty.const_zero(), addr2], "ptr")
    };
    let val = builder.build_load(ptr, "val");
    builder.build_return(Some(&val));
  }
  
  fn add_write_mem_decl<'ctx>(&self, context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let i8_ty = context.i8_type();
    let i16_ty = context.i16_type();
    let fn_type = context.void_type().fn_type(&[i16_ty.into(), i8_ty.into()], false);
    let write_mem_external = module.add_function("writeMemExternal", fn_type, None);
    let inaccessible_mem_only = context.create_enum_attribute(INACCESSIBLE_MEM_ONLY, 0);
    let no_unwind = context.create_enum_attribute(NO_UNWIND, 0);
    let will_return = context.create_enum_attribute(WILL_RETURN, 0);
    write_mem_external.add_attribute(AttributeLoc::Function, inaccessible_mem_only);
    write_mem_external.add_attribute(AttributeLoc::Function, no_unwind);
    write_mem_external.add_attribute(AttributeLoc::Function, will_return);

    let write_mem = module.add_function("writeMem", fn_type, None);
    write_mem.set_linkage(Linkage::Private);
    let entry = context.append_basic_block(write_mem, "entry");
    let if_low = context.append_basic_block(write_mem, "if_low");
    let else_low = context.append_basic_block(write_mem, "else_low");
    let if_external = context.append_basic_block(write_mem, "if_external");
    let else_external = context.append_basic_block(write_mem, "else_external");

    let addr = write_mem.get_nth_param(0).unwrap().into_int_value();
    let val = write_mem.get_nth_param(1).unwrap().into_int_value();
    builder.position_at_end(entry);
    let cond = builder.build_int_compare(IntPredicate::ULT, addr, i16_ty.const_int(0x2000, false), "cond");
    builder.build_conditional_branch(cond, if_low, else_low);

    builder.position_at_end(if_low);
    let addr2 = builder.build_int_unsigned_rem(addr, i16_ty.const_int(0x800, false), "addr");
    let ptr = unsafe {
      builder.build_gep(module.get_global("lowMem").unwrap().as_pointer_value(), &[i16_ty.const_zero(), addr2], "ptr")
    };
    builder.build_store(ptr, val);
    builder.build_return(None);

    builder.position_at_end(else_low);
    let cond = builder.build_int_compare(IntPredicate::ULT, addr, i16_ty.const_int(0x4020, false), "cond");
    builder.build_conditional_branch(cond, if_external, else_external);

    builder.position_at_end(if_external);
    builder.build_call(write_mem_external, &[addr.into(), val.into()], "val");
    builder.build_return(None);

    builder.position_at_end(else_external);
    let addr2 = builder.build_int_sub(addr, i16_ty.const_int(0x4000, false), "addr");
    let ptr = unsafe {
      builder.build_gep(module.get_global("highMem").unwrap().as_pointer_value(), &[i16_ty.const_zero(), addr2], "ptr")
    };
    builder.build_store(ptr, val);
    builder.build_return(None);
  }
}

impl <'a> Mapper for Mapper0<'a> {
    fn read_static(&self, addr: u16) -> u8 {
      let addr = addr as usize;
      let start = 0xFFFF - (self.prg_rom.len() as usize) + 1;
      if addr < start {
        0
      } else {
        self.prg_rom[addr - start]
      }
    }

    fn add_globals<'ctx>(&self, context: &'ctx Context, module: &Module<'ctx>) {
      add_rom(self.prg_rom, "prgRom", context, module);
      add_ram(0x800, "lowMem", context, module);
      add_ram(0x4000, "highMem", context, module);

      self.add_read_mem_decl(context, module);
      self.add_write_mem_decl(context, module);
    }
}