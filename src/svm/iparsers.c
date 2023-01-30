//// Parsers for loading virtual object code.

// In module 2, you add parsers `parseR1LIT` and `parseR1GLOBAL` to
// this file.  The other parsers may serve as examples you can build on.

#include <assert.h>

#include "iformat.h"
#include "iparsers.h"
#include "vmstate.h"
#include "string.h"


#define SEE(R) do { if ((R) > *maxreg) *maxreg = (R); } while(0)

Instruction parseR3(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t regZ = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY); SEE(regZ);
  return eR3(opcode, regX, regY, regZ);
}

Instruction parseR2(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR2(opcode, regX, regY);
}

Instruction parseR1(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX);
  return eR1(opcode, regX);
}

Instruction parseR0(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  (void)maxreg;
  assert(operands == NULL);
  return eR0(opcode);
}

Instruction parseR1U16(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  (void)maxreg;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint32_t immediate = tokens_get_int(&operands, NULL);
  assert(operands == NULL);
  assert(immediate == (uint16_t)immediate);
  SEE(regX);
  return eR1U16(opcode, regX, immediate);
}

Instruction parseR2U8(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void)vm;
  uint8_t regX = tokens_get_byte(&operands, NULL);
  uint8_t regY = tokens_get_byte(&operands, NULL);
  uint8_t k = tokens_get_byte(&operands, NULL);
  assert(operands == NULL);
  SEE(regX); SEE(regY);
  return eR3(opcode, regX, regY, k);
}

Instruction parseR0I24(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
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
    truename = strtoname("true");
    falsename = strtoname("false");
    nilname = strtoname("nil");
    emptyname = strtoname("emptylist");
    stringname = strtoname("string");
  }
}

static Value get_literal(Tokens* litp, const char* input);

// <opcode> <register> <literal>
Instruction parseR1LIT(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  (void) maxreg;

  initnames(); // before comparing names, you must call this function

  // <register>
  uint8_t regX = tokens_get_byte(&operands, NULL);

  // <literal>
  Value literal = get_literal(&operands, NULL);

  assert(operands == NULL);
  SEE(regX); // probably prevents register overflow
  return eR1U16(opcode, regX, literal_slot(vm, literal));
}

Instruction parseR1GLO(VMState vm, Opcode opcode, Tokens operands, unsigned* maxreg) {
  // <register>
  uint8_t regX = tokens_get_byte(&operands, NULL);

  Name n = tokens_get_name(&operands, NULL);
  
  assert(operands == NULL);
  Value name = mkStringValue(Vmstring_newc(nametostr(n)));
  SEE(regX); // probably prevents register overflow
  return eR1U16(opcode, regX, global_slot(vm, name));
}


static Value get_string_literal(Tokens* strp, const char* input);

// <literal> ::=
static Value get_literal(Tokens* litp, const char* input) {
  initnames();

  switch(first_token_type(*litp)) {
    case TNAME:
      {
        Name tok_name = tokens_get_name(litp, input);

        // true | false
        if (tok_name == falsename)
          return mkBooleanValue(false);
        if (tok_name == truename)
          return mkBooleanValue(true);

        // | emptylist
        if (tok_name == emptyname)
          return emptylistValue;

        // | nil
        if (tok_name == nilname)
          return nilValue;

        // | string ...
        if (tok_name == stringname)
          return get_string_literal(litp, input);
        break;
      }
    // | <number>
    case TDOUBLE:
      return mkNumberValue(tokens_get_signed_number(litp, input));
    case TU32:
      return mkNumberValue((Number_T) tokens_get_int(litp, input));
  }

  assert(0);
}

// string <length> { <byte> }
static Value get_string_literal(Tokens* strp, const char* input) {
  // <length>
  uint32_t len = tokens_get_int(strp, input);

  // { <byte> }
  StringBuffer buf = Vmstring_buffer(len);
  for (uint32_t i = 0; i < len; ++i) {
    Vmstring_putc(buf, tokens_get_byte(strp, input));
  }

  return mkStringValue(Vmstring_of_buffer(&buf));
}
