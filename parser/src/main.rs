use std::{
    fs::File,
    io::{BufWriter, Read, Write},
    path::PathBuf,
    process::exit,
};

use assembly::{irq_address, nmi_address};
use clap::StructOpt;
use clap_derive::{ArgEnum, Parser, Subcommand};
use inkwell::{context::Context, passes::PassManager};
use llvm::Compiler;
use mapper::{apple::Apple, mapper0::Mapper0, Mapper};
use passes::{BaseParser, Parser, SelfLoopPass, SmbSwitchPass};

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
        #[clap(arg_enum, short = 'm', default_value = "mapper0")]
        mapper: MapperId,

        #[clap(name = "FILE", parse(from_os_str))]
        input: PathBuf,

        #[clap(name = "OUTPUT", parse(from_os_str))]
        output: PathBuf,
    },
    Llvm {
        #[clap(arg_enum, short = 'm', default_value = "mapper0")]
        mapper: MapperId,

        #[clap(short = 'O')]
        optimize: bool,

        #[clap(name = "FILE", parse(from_os_str))]
        input: PathBuf,

        #[clap(name = "OUTPUT", parse(from_os_str))]
        output: PathBuf,
    },
}

#[derive(ArgEnum, Clone)]
enum MapperId {
    Apple,
    Mapper0,
}

fn main() {
    let args = Args::parse();

    let res = match args.command {
        Command::Disassemble {
            input,
            output,
            mapper,
        } => disassemble(input, output, mapper),
        Command::Llvm {
            input,
            output,
            mapper,
            optimize,
        } => llvm(input, output, mapper, optimize),
    };

    if let Err(e) = res {
        eprint!("Error: {:?}", e);
        exit(1);
    }
}

fn disassemble(input: PathBuf, output: PathBuf, mapper_id: MapperId) -> anyhow::Result<()> {
    let mut buf = vec![];
    File::open(input)?.read_to_end(&mut buf)?;
    let mapper = read_mapper(mapper_id, &buf)?;

    let mut output = BufWriter::new(File::create(output)?);
    writeln!(output, "; Reset: {:04x}", reset_address(mapper.as_ref()))?;
    writeln!(output, "; NMI: {:04x}", nmi_address(mapper.as_ref()))?;
    writeln!(output, "; IRQ: {:04x}", irq_address(mapper.as_ref()))?;

    let parser = SelfLoopPass::with_inner(SmbSwitchPass::with_inner(BaseParser));
    let functions = parser.parse(mapper.as_ref());
    for (offset, body) in functions {
        writeln!(output)?;
        writeln!(output, "{:04x}:", offset)?;
        for (offset, inst) in body {
            writeln!(output, "{:04x}: {}", offset, inst)?;
        }
    }
    Ok(())
}

fn llvm(
    input: PathBuf,
    output: PathBuf,
    mapper_id: MapperId,
    optimize: bool,
) -> anyhow::Result<()> {
    let mut buf = vec![];
    File::open(input)?.read_to_end(&mut buf)?;
    let mapper = read_mapper(mapper_id, &buf)?;

    let parser = SelfLoopPass::with_inner(SmbSwitchPass::with_inner(BaseParser));
    let functions = parser.parse(mapper.as_ref());

    let context = Context::create();
    let module = context.create_module("nes");
    mapper.add_globals(&context, &module);
    let mut compiler = Compiler::new(&context, &module);

    for addr in functions.keys() {
        compiler.declare_func(*addr);
    }

    for (addr, body) in functions.iter() {
        compiler.define_func(*addr, body);
    }

    compiler.define_entry("reset", reset_address(mapper.as_ref()));
    compiler.define_entry("nmi", nmi_address(mapper.as_ref()));

    module
        .verify()
        .map_err(|s| anyhow::anyhow!("Resulting module is invalid: {}", s))?;

    if optimize {
        let pm = PassManager::create(&());
        pm.add_promote_memory_to_register_pass(); // eliminate stack vars
        pm.add_cfg_simplification_pass(); // Remove single-predecessor blocks
        pm.add_bit_tracking_dce_pass(); // eliminate unused instructions
        pm.add_dead_arg_elimination_pass(); // eliminate arguments we don't use
        pm.add_reassociate_pass();
        pm.add_instruction_combining_pass(); // eliminate redundant extract / inserts from previous pass
        pm.add_scalar_repl_aggregates_pass_ssa();
        pm.add_dead_store_elimination_pass();
        pm.add_loop_idiom_pass();
        pm.add_loop_rotate_pass();
        pm.add_ind_var_simplify_pass();
        pm.add_loop_deletion_pass();
        pm.add_licm_pass();
        pm.add_loop_unroll_pass();
        pm.add_cfg_simplification_pass();
        pm.add_cfg_simplification_pass();
        pm.add_instruction_combining_pass();

        pm.add_function_inlining_pass();
        pm.add_global_dce_pass();
        pm.add_cfg_simplification_pass();
        pm.add_instruction_combining_pass(); // eliminate redundant extract / inserts from previous pass
        pm.add_cfg_simplification_pass();
        pm.add_bit_tracking_dce_pass(); // eliminate unused instructions
        pm.add_dead_arg_elimination_pass(); // eliminate arguments we don't use
        pm.add_bit_tracking_dce_pass(); // eliminate unused instructions
        pm.add_cfg_simplification_pass();
        pm.run_on(&module);
    }

    module.write_bitcode_to_path(&output);
    Ok(())
}

fn read_mapper<'a>(mapper_id: MapperId, file: &'a [u8]) -> anyhow::Result<Box<dyn Mapper + 'a>> {
    let result: Box<dyn Mapper + 'a> = match mapper_id {
        MapperId::Apple => Box::new(
            Apple::read(file)
                .map_err(|_| anyhow::anyhow!("Failed to read file"))?
                .1,
        ),
        MapperId::Mapper0 => Box::new(
            Mapper0::read(file)
                .map_err(|_| anyhow::anyhow!("Failed to read file"))?
                .1,
        ),
    };
    Ok(result)
}
