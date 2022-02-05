use std::{path::PathBuf, fs::File, io::{Read, Write, BufWriter}, process::exit};

use assembly::{nmi_address, irq_address};
use clap::StructOpt;
use clap_derive::{Parser, Subcommand};
use inkwell::context::Context;
use llvm::Compiler;
use mapper::Mapper0;
use passes::{SelfLoopPass, SmbSwitchPass, BaseParser, Parser};

use crate::assembly::reset_address;

mod assembly;
mod llvm;
mod mapper;
mod passes;

#[derive(Parser)]
struct Args {
  #[clap(subcommand)]
  command: Command,
}

#[derive(Subcommand)]
enum Command {
  Disassemble {
    #[clap(name = "FILE", parse(from_os_str))]
    input: PathBuf,

    #[clap(name = "OUTPUT", parse(from_os_str))]
    output: PathBuf,
  },
  Llvm {
    #[clap(name = "FILE", parse(from_os_str))]
    input: PathBuf,

    #[clap(name = "OUTPUT", parse(from_os_str))]
    output: PathBuf,
  }
}

fn main() {
  let args = Args::parse();

  let res = match args.command {
    Command::Disassemble{input, output} => disassemble(input, output),
    Command::Llvm{input, output} => llvm(input, output),
  };

  if let Err(e) = res {
    eprint!("Error: {:?}", e);
    exit(1);
  }
}

fn disassemble(input: PathBuf, output: PathBuf) -> anyhow::Result<()> {
  let mut buf = vec!();
  File::open(input)?.read_to_end(&mut buf)?;
  let (_, mapper) = Mapper0::read(&buf).map_err(|_| anyhow::anyhow!("Failed to read file"))?;

  let mut output = BufWriter::new(File::create(output)?);
  let prg_rom = mapper.prg_rom();
  writeln!(output, "; Reset: {:04x}", reset_address(&prg_rom))?;
  writeln!(output, "; NMI: {:04x}", nmi_address(&prg_rom))?;
  writeln!(output, "; IRQ: {:04x}", irq_address(&prg_rom))?;

  let parser = SelfLoopPass::with_inner(SmbSwitchPass::with_inner(BaseParser));
  let functions = parser.parse(&prg_rom);
  for (offset, body) in functions {
    writeln!(output, "")?;
    writeln!(output, "{:04x}:", offset)?;
    for (offset, inst) in body {
      writeln!(output, "{:04x}: {}", offset, inst)?;
    }
  }
  Ok(())
}

fn llvm(input: PathBuf, output: PathBuf) -> anyhow::Result<()> {
  let mut buf = vec!();
  File::open(input)?.read_to_end(&mut buf)?;
  let (_, mapper) = Mapper0::read(&buf).map_err(|_| anyhow::anyhow!("Failed to read file"))?;

  let prg_rom = mapper.prg_rom();
  let parser = SelfLoopPass::with_inner(SmbSwitchPass::with_inner(BaseParser));
  let functions = parser.parse(&prg_rom);

  let context = Context::create();
  let module = context.create_module("nes");
  let mut compiler = Compiler::new(&context, &module);

  for addr in functions.keys() {
    compiler.declare_func(*addr);
  }

  for (addr, body) in functions.iter() {
    compiler.define_func(*addr, body);
  }

  compiler.define_entry("reset", reset_address(&prg_rom));
  compiler.define_entry("nmi", nmi_address(&prg_rom));

  module.verify().map_err(|s| anyhow::anyhow!("Resulting module is invalid: {}", s))?;
  module.write_bitcode_to_path(&output);
  Ok(())
}