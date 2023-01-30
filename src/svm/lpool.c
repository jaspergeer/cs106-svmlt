#include "lpool.h"
#include <stdlib.h>

#ifdef TINY_LPOOL
#define INIT_LITERALS_SIZE 8
#define HASHES_SIZE 8
#else
#define INIT_LITERALS_SIZE 8
#define HASHES_SIZE 1024
#endif

#define T LPool_T

typedef struct link {
    struct link *next;
    uint32_t hash;
    uint16_t key;
} *Link;

struct T {
  uint16_t size;
  uint16_t nlits;

  Value *literals;

  Link *hashes;
};

T LPool_new() {
  T pool = malloc(sizeof(struct T));
  pool->hashes = calloc(HASHES_SIZE, sizeof(Link));
  pool->literals = calloc(INIT_LITERALS_SIZE, sizeof(Value));
  pool->size = INIT_LITERALS_SIZE;
  pool->nlits = 0;
  return pool;
}

Value LPool_get(T pool, uint16_t key) {
  return pool->literals[key];
}

uint16_t LPool_put(T pool, Value v) {
  uint32_t vhash = hashvalue(v);

  Link *l = pool->hashes + vhash % HASHES_SIZE;

  for (Link curr = *l; curr != NULL; curr = curr->next) {
    if (curr->hash == vhash)
      return curr->key;
    l = &curr;
  }

  Link new_link = malloc(sizeof(struct link));
  new_link->hash = vhash;
  new_link->next = 0;
  new_link->key = pool->nlits;

  if (pool->nlits == pool->size) {
    pool->literals = realloc(pool->literals, pool->size * 2 + 1);
    pool->size = (pool->size * 2 + 1) % UINT16_MAX;
  }

  pool->literals[pool->nlits] = v;
  *l = new_link;
  return pool->nlits++;
}

void LPool_free(T *pool) {
  for (int i = 0; i < HASHES_SIZE; ++i) {
    Link curr = (*pool)->hashes[i];
    if ((*pool)->hashes[i] != NULL) {
      while (curr != NULL) {
        Link last = curr;
        curr = curr->next;
        free(last);
      }
    }
  }
  free((*pool)->literals);
  free(*pool);
}

int LPool_nlits(T pool) {
  return pool->nlits;
}


