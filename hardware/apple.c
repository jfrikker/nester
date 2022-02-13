#include <stdint.h>
#include <stdio.h>
#include <string.h>

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

void writeMemExternal(uint16_t addr, uint8_t val) {
  #ifdef DEBUG_MEM
  printf("Writing %04X: %02X\n", addr, val);
  #endif
  if (addr == 0xd012) {
    print(val);
  }
}

uint8_t readMemExternal(uint16_t addr) {
  #ifdef DEBUG_MEM
  printf("Reading %04X\n", addr);
  #endif
  if (addr == 0xd012) {
    return 0;
  }
  return 0;
}

int main() {
  reset();
  return 0;
}