#include <stdint.h>
#include <stdio.h>
#include <string.h>

extern uint8_t rom[];
extern uint16_t romOffset;
extern uint16_t romSize;
uint8_t mem[0x10000];

void reset();

uint8_t charCount = 0;

void print(uint8_t c) {
  c = c & 0x7f;

  if (c == 0x0d) {
    c = '\n';
  }

  if (c == '\n') {
    putchar(c);
    charCount = 0;
  } else if (charCount >= 40) {
    putchar('\n');
    putchar(c);
    charCount = 1;
  } else {
    putchar(c);
    charCount++;
  }
}

void writeMem(uint16_t addr, uint8_t val, uint16_t *clock) {
  #ifdef DEBUG_MEM
  printf("Writing %04X: %02X\n", addr, val);
  #endif
  if (addr == 0xd012) {
    print(val);
  } else {
    mem[addr] = val;
  }
}

uint8_t readMem(uint16_t addr, uint16_t *clock) {
  #ifdef DEBUG_MEM
  printf("Reading %04X\n", addr);
  #endif
  if (addr == 0xd012) {
    return 0;
  }
  return mem[addr];
}

int main() {
  memcpy(mem + romOffset, rom, romSize);
  reset();
  return 0;
}