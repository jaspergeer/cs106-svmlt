// Use this to index global variables by name.

#ifndef STABLE_INCLUDED
#define STABLE_INCLUDED

#include <inttypes.h>

#define T STable_T

typedef struct T *T;
extern T     STable_new(void);
  // allocate a new table from garbage-collected memory, so the heap
  // must be initialized

extern void            STable_put(T stable, const char *key, unsigned payload);
extern const unsigned *STable_get(T stable, const char *key);
  // return null pointer if absent

void STable_free(T *tableptr);

#undef T
#endif
