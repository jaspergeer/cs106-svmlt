//// Parsers for loading virtual object code.

// In module 2, you add parsers `parseR1LIT` and `parseR1GLOBAL` to
// this file.  The other parsers may serve as examples you can build on.

#include <assert.h>

#include "iformat.h"
#include "iparsers.h"
#include "vmstate.h"


#define SEE(R) do { if ((R) > *maxreg) *maxreg = (R); } while(0)

Instruction parseR3(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t regZ = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY); SEE(regZ);
  return eR3(opcode, regX, regY, regZ);
}

Instruction parseR2(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR2(opcode, regX, regY);
}

Instruction parseR1(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  return eR1(opcode, regX);
}

Instruction parseR0(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  assert(operands == NULL);
  return eR0(opcode);
}

Instruction parseR1U16(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint32_t immediate = tokens_get_int(&operands, NULL);
  assert(operands == NULL);
  assert(immediate == (uint16_t) immediate);
  SEE(regX);
  return eR1U16(opcode, regX, immediate);
}

Instruction parseR2U8(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t k    = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR3(opcode, regX, regY, k);
}

Instruction parseR0I24(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  (void)vm;
  (void)maxreg;
  int32_t immediate = tokens_get_int(&operands, NULL);
  assert(immediate == ((immediate << 8) >> 8));
  assert(operands == NULL);
  return eR0I24(opcode, immediate);
}


static Name truename, falsename, nilname, emptyname, stringname;

static void initnames(void) {
  if (truename == NULL) {
    truename     = strtoname("true");
    falsename    = strtoname("false");
    nilname      = strtoname("nil");
    emptyname    = strtoname("emptylist");
    stringname   = strtoname("string");
  }
}

Instruction parseR1LIT(VMState vm, Opcode opcode, Tokens operands, unsigned *maxreg) {
  initnames(); // before comparing names, you must call this function
  (void)vm; (void)opcode; (void)operands; (void)maxreg; // replace with real code!
  assert(0);
}
