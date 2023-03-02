#ifndef LPOOL_H
#define LPOOL_H

#include <stdint.h>
#include "value.h"

#define L LPool_T

typedef struct L *L;

L LPool_new(void);
Value LPool_get(L pool, uint16_t key);
uint16_t LPool_put(L pool, Value v);
void LPool_free(L *pool);
int LPool_nlits(L pool);

#endif
