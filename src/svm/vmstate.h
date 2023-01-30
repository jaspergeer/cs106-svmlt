// State of a VM, and functions to allocate, deallocate, add a literal

// This one's the essential part of module 1.
// You'll define the key representation, `struct VMState`,
// and you'll use it in your `vmrun` function.

#ifndef VMSTATE_INCLUDED
#define VMSTATE_INCLUDED

#include <stdint.h>
#include "stable.h"
#include "name.h"
#include "value.h"
#include "vtable.h"

#define TINY_VM

#ifdef TINY_VM
#define LITERALS_SIZE 16
#define GLOBALS_SIZE 16
#else
#define LITERALS_SIZE 256
#define GLOBALS_SIZE 256
#endif

#include "value.h"

#define NUM_REGISTERS 256

typedef struct VMState* VMState;

struct VMState {
  // registers
  Value registers[NUM_REGISTERS];

  // literal pool
  Value literals[LITERALS_SIZE];
  int num_literals;

  // global variable table
  Value globals[GLOBALS_SIZE];
  int num_globals;

  // program counter
  uint32_t pc; // assumes that the first instruction is at address 0x0
};

VMState newstate(void);       // allocate and initialize (to empty)
void freestatep(VMState* sp);  // deallocate

int literal_slot(VMState state, Value literal);
  // return any index of literal in `literals`, adding if needed

int global_slot(VMState state, Value name);
  // return the unique index of `name` in `globals`, adding if needed.
  // The `name` parameter must be a VM string or the result is
  // a checked run-time error.


// The last three functions are used only for disassembly.

Value literal_value(VMState state, unsigned index);
  // Return the value at the given index. *Not* intended 
  // for use in `vmrun`, in which you don't want to pay the 
  // overhead of a function call.

int literal_count(VMState state);
  // Returns N, the number of index values for which it
  // is ok to call `literal_value` (range 0 to N-1)

const char *global_name(VMState state, unsigned index);
  // Return the name of the global at the given index.

#endif /* VMSTATE_INCLUDED */
