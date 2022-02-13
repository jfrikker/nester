#include <stdint.h>
#include <unistd.h>

extern uint8_t chrRom[8192];

void reset();

void externalWrite(uint16_t addr, uint8_t val) {
  uint8_t buf[] = {1, (uint8_t)addr, (uint8_t)(addr >> 8), val};
  write(1, buf, 4);
}

void writeMemExternal(uint16_t addr, uint8_t val) {
  #ifdef DEBUG_MEM
  printf("Writing %04X: %02X\n", addr, val);
  #endif
  if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
  }
  externalWrite(addr, val);
}

uint8_t externalRead(uint16_t addr) {
  uint8_t buf[] = {0, (uint8_t)addr, (uint8_t)(addr >> 8)};
  write(1, buf, 3);
  read(0, buf, 1);
  return buf[0];
}

uint8_t readMemExternal(uint16_t addr) {
  #ifdef DEBUG_MEM
  printf("Reading %04X\n", addr);
  #endif
  if (addr < 0x4000) {
    addr = 0x2000 + (addr % 0x100);
  }
  return externalRead(addr);
}

int main() {
  externalWrite(0x2006, 0x00);
  externalWrite(0x2006, 0x20);
  for (int i = 0; i < 0x2000; i++) {
    externalWrite(0x2007, chrRom[i]);
  }
  reset();
  return 0;
}