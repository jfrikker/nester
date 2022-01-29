#include <stdint.h>
#include <stdio.h>

extern uint8_t* prgRom;
uint8_t lowMem[0x800];
uint8_t mem[0x10000];

void reset();

void externalWrite(uint16_t addr, uint8_t val) {
  putchar(1);
  putchar((uint8_t)addr);
  putchar((uint8_t)(addr >> 8));
  putchar(val);
}

__attribute__((always_inline))
void writeMem(uint16_t addr, uint8_t val) {
  #ifdef DEBUG_MEM
  printf("Writing %04X: %02X\n", addr, val);
  #endif
  if (addr < 0x2000) {
    mem[addr % 0x800] = val;
  } else if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
    externalWrite(addr, val);
  } else if (addr < 0x4020) {
    externalWrite(addr, val);
  } else if (addr < 0x8000) {
    mem[addr] = val;
  }
}

uint8_t externalRead(uint16_t addr) {
  putchar(0);
  putchar((uint8_t)addr);
  putchar((uint8_t)(addr >> 8));
  fflush(stdout);
  return getchar();
}

__attribute__((always_inline))
uint8_t readMem(uint16_t addr) {
  #ifdef DEBUG_MEM
  printf("Reading %04X\n", addr);
  #endif
  if (addr < 0x2000) {
    return mem[addr % 0x800];
  } else if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
    return externalRead(addr);
  } else if (addr < 0x4020) {
    return externalRead(addr);
  } else if (addr < 0x8000) {
    return mem[addr];
  }
  return prgRom[addr - 0x8000];
}

int main() {
  reset();
  return 0;
}