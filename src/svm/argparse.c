#include "argparse.h"
#include "vmheap.h"
#include "vmsizes.h"
#include "vmstring.h"

static Value argvalue(const char *s) {
  return mkStringValue(Vmstring_newc(s)); // TODO: replace with S-expression reader
}

static Value cons(Value car, Value cdr) {
  VMNEW(struct VMBlock *, block,  vmsize_cons());
  block->nslots = 2;
  block->slots[0] = car;
  block->slots[1] = cdr;
  return mkConsValue(block);
}

extern Value arglist(int count, char **elements) {
  if (count > 0)
    return cons(argvalue(*elements), arglist(count - 1, elements + 1));
  else
    return emptylistValue;
}

