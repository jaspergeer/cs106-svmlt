// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <stdint.h>
#include "value.h"
#include "vtable.h"

#define LITERAL_INIT_LENGTH 128

#include "value.h"

#define NUM_REGISTERS 256

typedef struct VMState* VMState;

struct VMState {
  // registers
  Value registers[NUM_REGISTERS];

  // literal pool
  Value* literals;
  uint32_t curLiteralSize;
  uint32_t maxLiteralSize;

  // global variable table
  VTable_T globals;

  // instruction stream
  uint32_t* code;

  // program counter
  uint32_t* pc;
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState* sp);  // deallocate

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
