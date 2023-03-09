// Defines all the opcodes used in the VM

// When you're thinking about new instructions, define them here first.
// It's OK for an opcode to be defined here even if it is not implemented 
// anywhere.  But if you want to *run* an instruction (module 1) or *load*
// an instruction (module 2), the opcode has to be defined here first.

#ifndef OPCODE_INCLUDED
#define OPCODE_INCLUDED

<<<<<<< HEAD
typedef enum opcode {
    Return,

    Zero, // R1
    Hash, // R2
    Copy, // R2
    Err, // R1

    // Printing
    Print, // R1
    Println,
    Printu,

    // Dynamic Loading
    PipeOpen, // R1LIT
    DynLoad, // R3

    // Branching
    CondSkip, // R3
    Jump, // R0I24

    // Function Calls
    Call,
    TailCall,

    // Load/Store
    LoadLiteral, // R1U16
    GetGlobal, // R1U16
    SetGlobal, // R1U16

    // Check-Expect
    Check, Expect, // R1LIT

    // Arithmetic
    Add, Sub, Mul, Div, Mod, Idiv, // R3

    // Boolean Logic
    Truth, Not, And, Or, Xor, // R3

    // Comparison
    Cmp, Gt, Lt, Ge, Le, // R3

    // S-Expressions
    Cons, Car, Cdr,
    
    // Type Predicates
    IsFunc, IsPair, IsSym, IsNum, IsBool, IsNull, IsNil,

    Unimp, // stand-in for opcodes not yet implemented
    Unimp2, // stand-in for opcodes not yet implemented
=======
#include <stdbool.h>

typedef enum opcode { 
                      Halt, // R0
                      Print, // R1
                      Println, // R1
                      LoadLiteral, // R1LIT
                      Check, Expect, // R1LIT
                      Unimp, // stand-in for opcodes not yet implemented
                      Unimp2, // stand-in for opcodes not yet implemented
>>>>>>> refs/remotes/origin/main
} Opcode;

bool isgetglobal(Opcode code); // update this for your SVM, in instructions.c

#endif
