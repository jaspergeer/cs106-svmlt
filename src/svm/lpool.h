#ifndef LPOOL_H
#define LPOOL_H

#include <stdint.h>
#include "value.h"

#define T LPool_T

typedef struct T *T;

T LPool_new();
Value LPool_get(T pool, uint16_t key);
uint16_t LPool_put(T pool, Value v);
void LPool_free(T *pool);
int LPool_nlits(T pool);

#endif
