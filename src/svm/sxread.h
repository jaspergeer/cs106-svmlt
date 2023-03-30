#ifndef SXREAD_INCLUDED
#define SXREAD_INCLUDED

#include "value.h"

typedef struct Sxstream *Sxstream;

typedef enum Prompts { NOT_PROMPTING, PROMPTING } Prompts;

extern Value cons(Value car, Value cdr);
static inline Value car(Value xs);
static inline Value cdr(Value xs);
extern Value reverse(Value xs);

////////////////////////////////////////////////////////////////

static inline Value car(Value xs) {
  assert(xs.tag == ConsCell);
  return xs.block->slots[0];
}

static inline Value cdr(Value xs) {
  assert(xs.tag == ConsCell);
  return xs.block->slots[1];
}



#endif
