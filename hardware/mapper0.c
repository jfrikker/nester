#include <stdint.h>
#include <unistd.h>

extern uint8_t* prgRom;
uint8_t lowMem[0x800];
uint8_t mem[0x10000];

void reset();

 __attribute__((noinline))
void externalWrite(uint16_t addr, uint8_t val) {
  uint8_t buf[] = {1, (uint8_t)addr, (uint8_t)(addr >> 8), val};
  write(1, buf, 4);
}

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

 __attribute__((noinline))
uint8_t externalRead(uint16_t addr) {
  uint8_t buf[] = {0, (uint8_t)addr, (uint8_t)(addr >> 8)};
  write(1, buf, 3);
  read(0, buf, 1);
  return buf[0];
}

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