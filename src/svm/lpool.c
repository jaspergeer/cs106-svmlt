#include "lpool.h"
#include "vtable.h"
#include "print.h"
#include <stdlib.h>

#define INIT_LITERALS_SIZE 1024

#define L LPool_T

struct L {
  VTable_T keys;
  Value *literals;
  uint16_t nlits;
  size_t litcap;
};

L LPool_new(void) {
  L pool = malloc(sizeof(struct L));
  pool->keys = VTable_new(42);
  pool->literals = calloc(INIT_LITERALS_SIZE, sizeof(Value));

  pool->litcap = INIT_LITERALS_SIZE;

  // nil value at position 0
  pool->nlits = 1;
  return pool;
}

Value LPool_get(L pool, uint16_t key) {
  if (key == 0)
    return nilValue;
  return pool->literals[key];
}

uint16_t LPool_put(L pool, Value v) {
  if (isNil(v))
    return 0;
  Value key = VTable_get(pool->keys, v);
  if (!isNil(key)) {
    return (uint16_t) key.n;
  }
  VTable_put(pool->keys, v, mkNumberValue(pool->nlits));
  if (pool->nlits == pool->litcap) {
    if (pool->nlits == UINT16_MAX) {
      print("literals capacity exceeded\n");
      return 0;
    }
    pool->litcap = pool->litcap * 2 + 1 % UINT16_MAX;;
    pool->literals = realloc(pool->literals, pool->litcap);
  }
  pool->literals[pool->nlits] = v;
  return pool->nlits++;
}

void LPool_free(L *pool) {
  free((*pool)->literals);
  free(*pool);
}

int LPool_nlits(L pool) {
  return pool->nlits;
}

Value *LPool_getlits(L pool) {
  return pool->literals;
}
