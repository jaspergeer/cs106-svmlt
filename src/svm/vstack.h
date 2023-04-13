// Value stack, managed with malloc

#ifndef VSTACK_INCLUDED
#define VSTACK_INCLUDED

#include "value.h"

#define T VStack_T
typedef struct T *T;
extern T      VStack_new(void);
extern void   VStack_free(T);
extern bool   VStack_isempty(T seq);
extern Value  VStack_pop(T seq); // if empty, checked run-time error
extern void   VStack_push(T seq, Value v);
   
#undef T
#endif
