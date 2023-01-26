// Efficient interning of strings to Names.

// For the SVM project, this is probably overkill.  If you like data structures,
// read the paper cited below.

/* 
Adapted from code described in "Ternary Search Trees" by Jon
Bentley and Robert Sedgewick in the April, 1998, Dr. Dobb's Journal.
*/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "name.h"
#include "stable.h"

typedef struct Name *Tptr;
typedef struct Name {
  char splitchar;
  Tptr lokid, eqkid, hikid;
} Tnode;
static Tptr root;

static inline const char *copy(const char *name, int len) {
  char *s = malloc(len + 1);
  assert(s);
  strncpy(s, name, len);
  s[len] = 0;
  return s;
}

static inline Tptr init(Tptr p, char c) {
  if (p == NULL) {
    p = malloc(sizeof(*p));
    assert(p);
    p->splitchar = c;
    p->lokid = p->eqkid = p->hikid = NULL;
  }
  return p;
}


static Tptr insertl(Tptr *np, const char *name, int len) {
    // given pointer to root
    // returns pointer to node whose eqkid contains payload
    const char *s = name;
    Tptr thisnode = NULL;
    while (len >= 0) {
      char c = len == 0 ? '\0' : *s;
      assert (len == 0 || c != 0);  // needed to overload eqkid
      *np = init(*np, c);
      Tptr p = *np;
      // now: p points to a node that is ready to consume a character
      if (c == p->splitchar) {
        np = &p->eqkid;
        thisnode = p;
        s++, len--;
      } else if (c < p->splitchar) {
        np = &p->lokid;
      } else {
        np = &p->hikid;
      }
    }
    assert(np == &thisnode->eqkid);
    return thisnode;
}

static void cleanup(Tptr p) {
  if (p) {
    cleanup(p->lokid);
    cleanup(p->hikid);
    if (p->splitchar != '\0')
      cleanup(p->eqkid);
    else
      free(p->eqkid);
    free(p);
  }
}

void name_cleanup(void) {
  cleanup(root);
  root = NULL;
}

const char* nametostr(Name np) {
  return (const char *)np->eqkid;
}

Name strtoname(const char *s) {
    return strtonamel(s, strlen(s));
}

Name strtonamel(const char *s, int length) {
  Tptr thisnode = insertl(&root, s, length);
  assert(thisnode);
  Tptr *np = &thisnode->eqkid;
  if (*np == NULL)
      *np = (Tptr) copy(s, length);

  return thisnode;
}

////////////////////////////////////////////////////////////////

struct STable_T {
  Tptr root;
};

STable_T STable_new(void) {
  STable_T table = malloc(sizeof *table);
  assert(table);
  table->root = NULL;
  return table;
}

extern void STable_put(STable_T stable, const char *key, unsigned payload) {
  assert(stable);
  Tptr thisnode = insertl(&stable->root, key, strlen(key));
  if (thisnode->eqkid == NULL) {
    thisnode->eqkid = malloc(sizeof payload);
    assert(thisnode->eqkid);
  }
  unsigned *p = (unsigned *)thisnode->eqkid;
  assert(p);
  *p = payload;
}

extern const unsigned *STable_get   (STable_T stable, const char *key) {
  assert(stable);
  Tptr thisnode = insertl(&stable->root, key, strlen(key));
  return (unsigned *) thisnode->eqkid;
}

void STable_free(STable_T *tableptr) {
  cleanup((*tableptr)->root);
  free(*tableptr);
  *tableptr = NULL;
}
