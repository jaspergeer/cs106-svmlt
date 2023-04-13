#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#include "vstack.h"

#include "value.h"

#include "gcdebug.h"

#define T VStack_T

struct T {
  int population;
  int nslots;
  Value *slots;
};

static void expand(T stack) {
  assert(stack);
  int nslots = 2 * stack->nslots;
  Value *slots = realloc(stack->slots, nslots * sizeof(*slots));
  assert(slots);
  stack->nslots = nslots;
  stack->slots  = slots;
}

T VStack_new(void) {
  int nslots = 40; // winging it
  T p = malloc(sizeof(*p));
  assert(p);
  p->slots = malloc(nslots * sizeof (*p->slots));
  assert(p->slots);
  p->nslots = nslots;
  p->population = 0;
  return p;
}

void VStack_free(T p) {
  assert(p);
  free(p->slots);
  free(p);
}

bool VStack_isempty(T stack) {
  assert(stack);
  gcprintf("Stack has %d elements\n", stack->population);
  return stack->population == 0;
}

Value VStack_pop(T stack) {
  assert(stack);
  assert(stack->population > 0);
  return stack->slots[--stack->population];
}

void VStack_push(T stack, Value v) {
  if (stack->population == stack->nslots)
    expand(stack);
  assert(stack->population < stack->nslots);
  stack->slots[stack->population++] = v;
}
