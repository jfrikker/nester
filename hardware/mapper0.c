#include <stdint.h>
#include <stdio.h>

extern uint8_t* prgRom;
uint8_t lowMem[0x800];
uint8_t cartMem[0x4000];

void reset();

void externalWrite(uint16_t addr, uint8_t val) {
  putchar(0);
  putchar((uint8_t)addr);
  putchar((uint8_t)(addr >> 8));
  putchar(val);
}

void writeMem(uint16_t addr, uint8_t val, uint16_t *clock) {
  if (addr < 0x2000) {
    lowMem[addr % 0x800] = val;
  } else if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
    externalWrite(addr, val);
  } else if (addr < 0x4020) {
    externalWrite(addr, val);
  } else if (addr < 0x8000) {
    cartMem[addr - 0x4000] = val;
  }
}

uint8_t externalRead(uint16_t addr) {
  putchar(1);
  putchar((uint8_t)addr);
  putchar((uint8_t)(addr >> 8));
  fflush(stdout);
  return getchar();
}

uint8_t readMem(uint16_t addr, uint16_t *clock) {
  if (addr < 0x2000) {
    return lowMem[addr % 0x800];
  } else if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
    return externalRead(addr);
  } else if (addr < 0x4020) {
    return externalRead(addr);
  } else if (addr < 0x8000) {
    return cartMem[addr - 0x4000];
  }
  return prgRom[addr - 0x8000];
}

int main() {
  reset();
  return 0;
}