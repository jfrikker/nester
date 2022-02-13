use inkwell::{context::Context, module::{Module, Linkage}};

pub trait Mapper {
  fn read_static(&self, addr: u16) -> u8;

  fn read_address(&self, addr: u16) -> u16 {
    let low = self.read_static(addr);
    let high = self.read_static(addr + 1);
    ((high as u16) << 8) + (low as u16)
  }

  fn add_globals<'ctx>(&self, context: &'ctx Context, module: &Module<'ctx>);
}

pub fn add_rom<'ctx>(rom: &[u8], name: &str, context: &'ctx Context, module: &Module<'ctx>) {
  let glob = module.add_global(context.i8_type().array_type(0x8000), None, name);
  glob.set_linkage(Linkage::Private);
  glob.set_constant(true);
  glob.set_initializer(&context.i8_type().const_array(&rom.iter()
    .map(|b| context.i8_type().const_int(*b as u64, false))
    .collect::<Vec<_>>()));
}

pub fn add_ram<'ctx>(len: usize, name: &str, context: &'ctx Context, module: &Module<'ctx>) {
  let glob = module.add_global(context.i8_type().array_type(len as u32), None, name);
  glob.set_initializer(&context.i8_type().const_array(&(0..len).into_iter()
    .map(|_| context.i8_type().const_zero())
    .collect::<Vec<_>>()));
  glob.set_linkage(Linkage::Private)
}