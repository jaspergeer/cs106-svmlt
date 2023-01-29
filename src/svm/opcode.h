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
    CondMove, // R3
    Jump, // R0I24

    // Load/Store
    LoadLiteral, // R1U16
    LoadGlobal, // R1U16
    StoreGlobal, // R1U16

    // Check-Expect
    Check, Expect, // R1LIT

    // Arithmetic
    Add, Sub, Mul, Div, // R3

    // Boolean Logic
    Truth, Not, And, Or, Xor, // R3

    // Comparison
    Cmp, Gt, Lt, Ge, Le, // R3
    
    Unimp, // stand-in for opcodes not yet implemented
} Opcode;

#endif
