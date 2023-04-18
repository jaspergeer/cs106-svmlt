#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

typedef struct {
  uint32_t pc;
  Value *dest_reg;
  Value *reg0;
  struct VMFunction *fun;
  struct VMBlock *suspended;
} Activation;

#endif
