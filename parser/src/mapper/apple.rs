use inkwell::{
    attributes::AttributeLoc,
    context::Context,
    module::{Linkage, Module},
    IntPredicate,
};
use nom::{bytes::complete::take, IResult};

use super::Mapper;
use crate::llvm::{INACCESSIBLE_MEM_ONLY, NO_UNWIND, WILL_RETURN};

const MONITOR: &[u8] = include_bytes!("appleMon.bin");

pub struct Apple<'a> {
    offset: u16,
    rom: &'a [u8],
}

impl<'a> Apple<'a> {
    pub fn read(contents: &'a [u8]) -> IResult<&'a [u8], Self> {
        let (contents, offset) =
            nom::number::complete::u16(nom::number::Endianness::Big)(contents)?;
        let (contents, length) =
            nom::number::complete::u16(nom::number::Endianness::Big)(contents)?;
        let (contents, rom) = take(length as usize)(contents)?;
        Ok((contents, Self { offset, rom }))
    }
}

impl<'a> Mapper for Apple<'a> {
    fn read_static(&self, addr: u16) -> u8 {
        let addr = addr as usize;
        let offset = self.offset as usize;
        match addr {
            0xffff => (self.offset >> 8) as u8,
            0xfffe => self.offset as u8,
            0xfffd => (self.offset >> 8) as u8,
            0xfffc => self.offset as u8,
            0xfffb => (self.offset >> 8) as u8,
            0xfffa => self.offset as u8,
            i if i >= offset && i < offset + self.rom.len() => self.rom[i - offset],
            i if i >= 0xff00 && i < 0xff00 + MONITOR.len() => MONITOR[i - 0xff00],
            _ => 0,
        }
    }

    fn add_globals<'ctx>(
        &self,
        context: &'ctx inkwell::context::Context,
        module: &inkwell::module::Module<'ctx>,
    ) {
        let glob = module.add_global(context.i8_type().array_type(0x10000), None, "mem");
        glob.set_initializer(
            &context.i8_type().const_array(
                &(0..self.offset)
                    .into_iter()
                    .map(|_| context.i8_type().const_zero())
                    .chain(
                        self.rom
                            .iter()
                            .map(|i| context.i8_type().const_int(*i as u64, false)),
                    )
                    .chain(
                        ((self.offset as usize + self.rom.len())..0x10000)
                            .into_iter()
                            .map(|_| context.i8_type().const_zero()),
                    )
                    .collect::<Vec<_>>(),
            ),
        );
        glob.set_linkage(Linkage::Private);

        add_read_mem_decl(context, module);
        add_write_mem_decl(context, module);
    }
}

fn add_read_mem_decl<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let i8_ty = context.i8_type();
    let i16_ty = context.i16_type();
    let i64_ty = context.i64_type();
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

    let addr = read_mem.get_nth_param(0).unwrap().into_int_value();
    builder.position_at_end(entry);
    let addr_ext = builder.build_int_z_extend(addr, i64_ty, "addr_ext");
    let cond = builder.build_int_compare(
        IntPredicate::ULT,
        addr,
        i16_ty.const_int(0xd012, false),
        "cond",
    );
    builder.build_conditional_branch(cond, if_low, else_low);

    builder.position_at_end(if_low);
    let ptr = unsafe {
        builder.build_gep(
            module.get_global("mem").unwrap().as_pointer_value(),
            &[i64_ty.const_zero(), addr_ext],
            "ptr",
        )
    };
    let val = builder.build_load(ptr, "val");
    builder.build_return(Some(&val));

    builder.position_at_end(else_low);
    let cond = builder.build_int_compare(
        IntPredicate::ULT,
        addr,
        i16_ty.const_int(0xd013, false),
        "cond",
    );
    builder.build_conditional_branch(cond, if_external, else_external);

    builder.position_at_end(if_external);
    let val = builder
        .build_call(read_mem_external, &[addr.into()], "val")
        .try_as_basic_value()
        .unwrap_left();
    builder.build_return(Some(&val));

    builder.position_at_end(else_external);
    let ptr = unsafe {
        builder.build_gep(
            module.get_global("mem").unwrap().as_pointer_value(),
            &[i64_ty.const_zero(), addr_ext],
            "ptr",
        )
    };
    let val = builder.build_load(ptr, "val");
    builder.build_return(Some(&val));
}

fn add_write_mem_decl<'ctx>(context: &'ctx Context, module: &Module<'ctx>) {
    let builder = context.create_builder();
    let i8_ty = context.i8_type();
    let i16_ty = context.i16_type();
    let i64_ty = context.i64_type();
    let fn_type = context
        .void_type()
        .fn_type(&[i16_ty.into(), i8_ty.into()], false);
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
    let addr_ext = builder.build_int_z_extend(addr, i64_ty, "addr_ext");
    let cond = builder.build_int_compare(
        IntPredicate::ULT,
        addr,
        i16_ty.const_int(0xd012, false),
        "cond",
    );
    builder.build_conditional_branch(cond, if_low, else_low);

    builder.position_at_end(if_low);
    let ptr = unsafe {
        builder.build_gep(
            module.get_global("mem").unwrap().as_pointer_value(),
            &[i64_ty.const_zero(), addr_ext],
            "ptr",
        )
    };
    builder.build_store(ptr, val);
    builder.build_return(None);

    builder.position_at_end(else_low);
    let cond = builder.build_int_compare(
        IntPredicate::ULT,
        addr,
        i16_ty.const_int(0xd013, false),
        "cond",
    );
    builder.build_conditional_branch(cond, if_external, else_external);

    builder.position_at_end(if_external);
    builder.build_call(write_mem_external, &[addr.into(), val.into()], "val");
    builder.build_return(None);

    builder.position_at_end(else_external);
    let ptr = unsafe {
        builder.build_gep(
            module.get_global("mem").unwrap().as_pointer_value(),
            &[i64_ty.const_zero(), addr_ext],
            "ptr",
        )
    };
    builder.build_store(ptr, val);
    builder.build_return(None);
}
