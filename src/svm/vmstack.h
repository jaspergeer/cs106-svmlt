#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

typedef struct {
  uint32_t pc;                // 
  Value *dest_reg;            // the destination register
  Value *reg0;                // start of the register window (reg0 in callee space)
  struct VMFunction *fun;     // the function being called
  struct VMBlock *suspended;  // block that stores extra argument
} Activation;

#endif
