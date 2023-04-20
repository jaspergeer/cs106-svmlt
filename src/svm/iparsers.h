//// Parsers for loading virtual object code.

// From module 2 onward, you'll need to know how these work so that
// you can associate an appropriate parser with each opcode in your
// instruction table (file instructions.c).  There is one parser for
// each encoding function in header file "iformat.h", plus one for
// parsing an instruction that includes a literal (which eventually
// gets encoded in format R1U16).

// In module 2, you'll implement functions `parseR1LIT` and `parseR1GLO`
// in file iparsers.c.

#ifndef IPARSERS_INCLUDED
#define IPARSERS_INCLUDED

#include <inttypes.h>
#include <stdio.h>

#include "iformat.h"
#include "tokens.h"

typedef struct VMState *VMState;

typedef Instruction (*InstructionParser)(VMState, Opcode, Tokens, unsigned *maxregp);
  // Consume all the tokens (which represent operands), 
  // update *maxregp with the largest register seen,
  // and return the encoded instruction.  If the opcode
  // calls for a literal, ensures the literal appears in the VM's pool.


Instruction parseR3   (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR2   (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1   (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR0   (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1U8 (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR2U8 (VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1U16(VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR0I24(VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1LIT(VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseU8LIT(VMState, Opcode, Tokens, unsigned *maxregp);
Instruction parseR1GLO(VMState, Opcode, Tokens, unsigned *maxregp);

#endif
