use std::ops::Index;

use nom::{IResult, bytes::complete::take};

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

  pub fn prg_rom(&self) -> Mapper0PrgRom<'a> {
    Mapper0PrgRom {
      prg_rom: self.prg_rom
    }
  }
}

pub struct Mapper0PrgRom<'a> {
  prg_rom: &'a [u8],
}

impl <'a> Mapper0PrgRom<'a> {
  pub fn as_bytes(&self) -> &[u8] {
    self.prg_rom
  }
}

impl <'a> Index<u16> for Mapper0PrgRom<'a> {
  type Output = u8;

  fn index(&self, index: u16) -> &Self::Output {
    let index = index as usize;
    let start = 0xFFFF - (self.prg_rom.len() as usize) + 1;
    if index < start {
      &0
    } else {
      &self.prg_rom[index - start]
    }
  }
}