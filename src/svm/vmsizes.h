#ifndef VMSIZES_INCLUDED
#define VMSIZES_INCLUDED

#include <stdlib.h>

#include "value.h"

static inline size_t vmsize_fun(int n_instructions);
static inline size_t vmsize_fun_payload(struct VMFunction *fun);

static inline size_t vmsize_cons(void);

static inline size_t vmsize_closure(int nslots);
static inline size_t vmsize_closure_payload(struct VMClosure *hof);

static inline size_t vmsize_string(size_t nchars);
static inline size_t vmsize_string_payload(struct VMString *s);

static inline size_t vmsize_block(int nslots);
static inline size_t vmsize_block_payload(struct VMBlock *s);






/////////////////////////////////////////////////////////

static inline size_t vmsize_fun(int n_instructions) {
  struct VMFunction *fun;
  return sizeof (*fun) + n_instructions * sizeof(fun->instructions[0]);
}
static inline size_t vmsize_fun_payload(struct VMFunction *fun) {
  return vmsize_fun(fun->size);
}

static inline size_t vmsize_closure(int nslots) {
  struct VMClosure *hof;
  return sizeof(*hof) + nslots * sizeof(hof->captured[0]);
}
static inline size_t vmsize_closure_payload(struct VMClosure *hof) {
  return vmsize_closure(hof->nslots);
}

static inline size_t vmsize_string(size_t nchars) {
  // size of a string object of the given length
  struct VMString *s;
  return sizeof(*s) + (nchars + 1) * sizeof(char);
  // accounts for the hidden terminating '\0'
}
static inline size_t vmsize_string_payload(struct VMString *s) {
  return vmsize_string(s->length);
}

static inline size_t vmsize_block(int nslots) {
  struct VMBlock *block;
  return sizeof(*block) + nslots * sizeof(*block->slots);
}
static inline size_t vmsize_block_payload(struct VMBlock *b) {
  return vmsize_block(b->nslots);
}

static inline size_t vmsize_cons(void) {
  return vmsize_block(2);
}



  


#endif /* VMSIZES_INCLUDED */
