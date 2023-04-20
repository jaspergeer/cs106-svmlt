// Encoding functions for instructions

// This code is all special cases of the `Bitpack` module that
// students implement in CS 40.  Nothing to see here.  Move along.

#include <assert.h>

#include "iformat.h"

#define BYTE(X) (assert(((X) & 0xff) == (X)))

Instruction eR3   (unsigned opcode, unsigned x, unsigned y, unsigned z) {
  BYTE(x);
  BYTE(y);
  BYTE(z);
  BYTE(opcode);
  return opcode << 24 | x << 16 | y << 8 | z;
}

Instruction eR2   (unsigned opcode, unsigned x, unsigned y) {
  return eR3(opcode, x, y, 0);
}

Instruction eR1   (unsigned opcode, unsigned x) {
  return eR3(opcode, x, 0, 0);
}

Instruction eR1U8  (unsigned opcode, unsigned x, unsigned y) {
  return eR3(opcode, x, y, 0);
}

Instruction eR0   (unsigned opcode) {
  return eR3(opcode, 0, 0, 0);
}

Instruction eR2U8 (unsigned opcode, unsigned x, unsigned y, unsigned z) {
  return eR3(opcode, x, y, z);
}

Instruction eR1U16(unsigned opcode, unsigned x, unsigned yz) {
  BYTE(x);
  assert(yz == (yz & 0xffff));
  BYTE(opcode);
  return opcode << 24 | x << 16 | yz;
}

Instruction eR0I24(unsigned opcode, int32_t xyz) {
  assert((xyz << 8) >> 8 == xyz);
  BYTE(opcode);
  return (opcode << 24) | ((unsigned) (xyz << 8)) >> 8;
}
