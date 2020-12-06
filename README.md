# nester

This is just a fun project to re-compile popular NES games for modern architectures. It's maybe 20% complete at this
point.

## Moving Parts

The `parser` directory contains the `nester` executable. This is responsible for disassembling a game ROM into 6502 instructions, then generating LLVM IR from the output.

The `runtime` directory will eventually be an emulator for the non-cpu portion of the NES (graphics hardware, audio hardware, etc.)

The `games` directory will contain makefiles for transpiling individual game ROMs. Unfortunately, making a completely general-purpose disassembler is very difficult (due mostly to the "indirect jump" instruction). For now at least, a minimal amount of customization will be required per ROM.

Actual game ROMs aren't present in this repository for copyright reasons.

## Usage
1. Install both stack, and the llvm command-line tools.
2. Build the `nester` and `runtime` binaries: `stack install`. 
3. Copy a game ROM to the appropriate directory under `games` (so far only Super Mario Bros. is supported).
4. Run `make` in the game directory to build the binary, or `make game.as` to output the disassembly. Note that the `opt` command may take up to a minute to optimize the IR.

## Special Instructions

A few extra instructions have been added to the 6502 instruction set. Per-ROM customizations mostly focus on recognizing and generating these instructions.

### Unknown

This represents an unknown opcode. This can appear even in "correct" disassemblies. For example, this sequence:

```
f3ad: LDX #$08
f3af: BNE (f38d)
f3b1: !Unknown 9f
```

In this case, the conditional BNE is actually an unconditional jump, probably implemented this way to save a cycle. The decompiler can't detect that though, and assumes exection may continue past the branch.

### SLP

This instruction represents a busy-wait, usually at the end of the reset handler while waiting for an interrupt. When generating LLVM, this instruction generates a call to the sleep callback, rather than blocking the thread.

### SWA

This instruction represents a switch. Switches are the primary use-case of the indirect jump instruction. For example (from https://gist.githubusercontent.com/1wErt3r/4048722/raw/59e88c0028a58c6d7b9156749230ccac647bc7d4/SMBDIS.ASM):

```
  lda OperMode
  jsr JumpEngine

  .dw TitleScreenMode
  .dw GameMode
  .dw VictoryMode
  .dw GameOverMode

JumpEngine:
       asl
       tay
       pla
       sta $04
       pla
       sta $05
       iny
       lda ($04),y
       sta $06
       iny
       lda ($04),y
       sta $07
       jmp ($06)
```

This code uses some stack magic to implement a switch based on the value of register A. At the moment, a special pass is required to identify this pattern, and generate an SWA instruction. In this case, we'd generate something like:

```
SWA [8231, aedc, 838b, 9218]
```