#ifndef VMSTACK_INCLUDED
#define VMSTACK_INCLUDED

#include "value.h"

typedef struct {
  Instruction *stream_ptr;
  Value *dest_reg;
  Value *reg0;
} Activation;

#endif
