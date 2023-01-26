// Parsing for SVMDEBUG string

// Perhaps interesting as an example of low-level, character-by-character
// parsing, but otherwise not.
//
// You'll be able to tell this code was written by a functional programmer.

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "svmdebug.h"

typedef struct cacheline {
  char *key;
  char *value;
  struct cacheline *next;
} *Cacheline;

static const char *static_true = "true";

static Cacheline cacheline(char *key, char *value, Cacheline rest) {
  Cacheline c = malloc(sizeof *c);
  assert(c);
  c->key = key;
  c->value = value;
  c->next = rest;
  return c;
}

static void cachefree(Cacheline c) {
  if (c) {
    cachefree(c->next);
    free(c->key);
    free(c->value);
    free(c);
  }
}

char *newstring(const char *start, const char *limit) {
  char *p = malloc(limit - start + 1);
  assert(p);
  memcpy(p, start, limit - start);
  p[limit - start] = '\0';
  return p;
}

static Cacheline parse(const char *start) {
  if (*start == '\0')
    return NULL;
  else {
    assert(*start != ','); // to hell with ill-formed input
    const char *limit = start + 1;
    while (*limit && *limit != '=' && *limit != ',')
      limit++;
    if (*limit == ',') // implicit value true, parse rest of string
      return cacheline(newstring(start, limit),
                       newstring(static_true, static_true + 4),
                       parse(limit + 1));
    else if (*limit == '\0') // implicit "true", end of string
      return cacheline(newstring(start, limit),
                       newstring(static_true, static_true + 4),
                       NULL);
    else { // explicit value
      assert(*limit == '=');
      const char *valstart = limit + 1;
      const char *vl = valstart; // value limit
      int openbraces = 0; // net number of open braces between valstart and vl
      while (*vl && !(*vl == ',' && openbraces == 0)) {
        if (*vl == '}') {
          assert(openbraces > 0);
          openbraces--;
        } else if (*vl == '{') {
          openbraces++;
        }
        vl++;
      }
      assert(openbraces == 0); // if not, premature end to string
      const char *rest = *vl == ',' ? vl + 1 : vl;
      if (*valstart == '{') {
        valstart++;
        vl--;
        assert(*vl == '}');
      }
      return cacheline(newstring(start, limit),
                       newstring(valstart, vl),
                       parse(rest));
    }
  }
}
    


static bool parsed;

static struct cacheline *cache;
  // non-NULL if ever parsed anything

void svmdebug_finalize(void) {
  cachefree(cache);
}

static const char *find(const char *key) {
  for (Cacheline c = cache; c; c = c-> next)
    if (!strcmp(key, c->key))
      return c->value;
  return NULL;
}


const char *svmdebug_value(const char *key) {
  if (!parsed) {
    parsed = true;
    const char *debug = getenv("SVMDEBUG");
    if (debug)
      cache = parse(debug);
  }

  return find(key);
}

#include <stdio.h>

void svdump(void) {
  (void)svmdebug_value("test");
  for (Cacheline c = cache; c; c = c-> next) {
    printf("%s = {%s}\n", c->key, c->value);
  }
}
  
