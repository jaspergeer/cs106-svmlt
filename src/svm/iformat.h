////// formatting of the instruction word; encoding and decoding

// Module 1: Use the decoding functions.
// Module 2: Use the encoding functions.
// They are all special cases of the `Bitpack` interface that students
// implement in CS 40.

#ifndef IFORMAT_INCLUDED
#define IFORMAT_INCLUDED

#include <inttypes.h>
#include <stdio.h>

#include "opcode.h"

typedef uint32_t Instruction;

// decoding functions (observers for instructions)

static inline Opcode   opcode(Instruction);
static inline uint8_t  uX(Instruction);
static inline uint8_t  uY(Instruction);
static inline uint8_t  uZ(Instruction);
static inline uint16_t uYZ(Instruction);
static inline int32_t  iXYZ(Instruction);

// encoding functions (creators for instructions)

Instruction eR0   (Opcode opcode);
Instruction eR1   (Opcode opcode, unsigned x);
Instruction eR2   (Opcode opcode, unsigned x, unsigned y);
Instruction eR3   (Opcode opcode, unsigned x, unsigned y, unsigned z);
Instruction eR1U8 (Opcode opcode, unsigned x, unsigned y);
Instruction eR2U8 (Opcode opcode, unsigned x, unsigned y, unsigned z);
Instruction eR1U16(Opcode opcode, unsigned x, unsigned yz);
Instruction eR0I24(Opcode opcode, int32_t xyz);


//////// Implementations.  Do not inspect beyond this point.

static inline Opcode   opcode(Instruction i) { return (Opcode) (i >> 24); }
static inline uint8_t  uX(Instruction i)     { return (i << 8) >> 24; }
static inline uint8_t  uY(Instruction i)     { return (i << 16) >> 24; }
static inline uint8_t  uZ(Instruction i)     { return (i & 0xff); }
static inline uint16_t uYZ(Instruction i)    { return (i & 0xffff); }
static inline int32_t  iXYZ(Instruction i)   { return ((int32_t) i << 8) >> 8; }

#endif
