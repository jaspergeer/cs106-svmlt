#ifndef LPOOL_H
#define LPOOL_H

#include <stdint.h>
#include "value.h"
#include "vtable.h"

typedef struct LPool* LPool;

struct LPool {
  VTable_T keys;
  Value *literals;
  uint16_t nlits;
  size_t litcap;
};

LPool LPool_new(void);
Value LPool_get(LPool pool, uint16_t key);
uint16_t LPool_put(LPool pool, Value v);
void LPool_free(LPool *pool);
int LPool_nlits(LPool pool);

#endif
