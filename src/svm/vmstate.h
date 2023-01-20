// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <stdint.h>

#include "value.h"

#define LITERAL_POOL_SIZE 128
#define GLOBAL_VARS_SIZE 128
#define NUM_REGISTERS 256

typedef struct VMState *VMState;

struct VMState {
    // registers
    Value *registers[NUM_REGISTERS];

    // literal pool
    Value literals[LITERAL_POOL_SIZE];
    int literals_count;

    // global variable table
    Value *globals[GLOBAL_VARS_SIZE];
 
    // instruction stream
    uint32_t *code;

    // program counter
    uint32_t *pc;
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState *sp);  // deallocate

int literal_slot(VMState state, Value literal);
  // return index of literal in `literals`, adding if needed
  // (at need, can be postponed to module 2)

Value literal_value(VMState state, unsigned index);
  // Return the value at the given index. *Not* intended 
  // for use in `vmrun`, in which you don't want to pay the 
  // overhead of a function call.

int literal_count(VMState state);
  // Returns N, the number of index values for which it
  // is ok to call `literal_value` (range 0 to N-1)

#endif /* VMSTATE_INCLUDED */
