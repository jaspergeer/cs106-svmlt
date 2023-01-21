// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented 
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

typedef enum opcode { 
                      Halt, // R0
                      Print, // R1
                      Check, Expect, // R1LIT
                      Add,
                      SetZero,
                      AsBool,
                      Not,
                      Unimp, // stand-in for opcodes not yet implemented
} Opcode;

#endif
