//// Tokenization of virtual object code

// There's nothing here you need to understand.

// Part One contains a ton of code that is mostly boilerplate;
// it implements a classic algebraic data type embedded in a list.
//
// Part Two is the mildly interesting bit: it tokenizes an input line,
// and it contains a lot of gnarly code for careful string-to-number
// conversion.  It's gnarly because the standard C library is uncivilized.

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdnoreturn.h>

#include <stdio.h>
#include <inttypes.h>

#include "tokens.h"

//////////////// Part One: token list as an abstract data type


//////// representation

struct tokens { // cons cell for linked list of tokens
  enum tokentype type;
  union {
    Name name;
    uint32_t n;
    double x;
  };
  struct tokens *next;
};

//////// creators

static Tokens mkInt(uint32_t n) {
  Tokens t = malloc(sizeof(*t));
  assert(t);
  t->type = TU32;
  t->n = n;
  return t;
}

static Tokens mkDouble(double x) {
  Tokens t = malloc(sizeof(*t));
  assert(t);
  t->type = TDOUBLE;
  t->x = x;
  return t;
}

static Tokens mkName(Name n) {
  Tokens t = malloc(sizeof(*t));
  assert(t);
  t->type = TNAME;
  t->name = n;
  return t;
}

//////// destructor

void free_tokens(Tokens *tp) {
  if (*tp != NULL) {
    free_tokens(&(*tp)->next);
    free(*tp);
    *tp = NULL;
  }
}

//////// observers

enum tokentype first_token_type(Tokens ts) {
  assert(ts);
  return ts->type;
}

void print_tokens(FILE *fp, Tokens toks) {
  if (toks) {
    switch (toks->type) {
    case TNAME: fprintf(fp, "name:%s", nametostr(toks->name)); break;
    case TU32:  fprintf(fp, "int:%" PRIu32, toks->n); break;
    case TDOUBLE: fprintf(fp, "double:%g", toks->x); break;
    }
    fprintf(fp, " -> ");
    print_tokens(fp, toks->next);
  } else {
    fprintf(fp, "NULL");
  }
}

//////// error checking and validation (used by mutators)

static inline Tokens validate(const char *what, Tokens *p, const char *original) {
  assert(p);
  Tokens ts = *p;
  if (ts == NULL) {
    fprintf(stderr, "asked for %s, but there are no tokens left", what);
    if (original)
      fprintf(stderr, " (original input was \"%s\")", original);
    fprintf(stderr, "\n");
    assert(0);
  }
  return ts;
}

static const char* got(Tokens ts) {
  assert(ts);
  switch (ts->type) {
  case TU32:    return "an integer"; 
  case TDOUBLE: return "a floating-point number"; 
  case TNAME:   return "a name"; 
  default: assert(0); return "this can't happen";
  }
}

noreturn static void invalid(const char *asked, const char *got, const char *original) {
  assert(asked);
  assert(got);
  fprintf(stderr, "asked for %s, but the next token is %s", asked, got);
  if (original)
    fprintf(stderr, " (original input was \"%s\")", original);
  fprintf(stderr, "\n");
  assert(0);
  exit(1);
}


//////// mutators

uint32_t tokens_get_int   (Tokens *p, const char *original) {
  Tokens ts = validate("an int", p, original);
  if (ts->type == TU32) {
    *p = ts->next;
    return ts->n;
  } else {
    invalid("an int", got(ts), original);
  }
}
  
uint8_t  tokens_get_byte  (Tokens *p, const char *original) {
  Tokens ts = validate("a byte", p, original);
  if (ts->type == TU32) {
    uint8_t b = (uint8_t) ts->n;
    if ((uint32_t) b == ts->n) {
      *p = ts->next;
      return b;
    } else {
      invalid("a byte", "too big to fit in 8 bits", original);
    }
  } else {
    invalid("a byte", got(ts), original);
  }
}

Name     tokens_get_name  (Tokens *p, const char *original) {
  Tokens ts = validate("a name", p, original);
  if (ts->type == TNAME) {
    *p = ts->next;
    return ts->name;
  } else {
    invalid("a name", got(ts), original);
  }
}
double   tokens_get_signed_number(Tokens *p, const char *original) {
  Tokens ts = validate("a number", p, original);
  switch (ts->type) {
  case TU32:    *p = ts->next; return (double) (int32_t) ts->n;
  case TDOUBLE: *p = ts->next; return ts->x;
  case TNAME:   invalid("a number", "a name", original);
  default: assert(0); return 0.0;
  }
}



//////////////// Part Two: tokenization of strings

static int isdelim(char c) {
    return isspace((unsigned char)c) || c == '\0';
}

Tokens tokens(const char *s) {
    const char *p, *q;
    Tokens head;

    while (*s && isspace((unsigned char)*s))
      s++;
    p = s;                            /* remember starting position */
    for (q = p; !isdelim(*q); q++)    /* scan to next delimiter */
      ;
    char *t;                        // first nondigit in s
    errno = 0;
    long long l = strtoll(s, &t, 0);      // value of digits in s, if any
    if (t == q && t > p) {          // the token is all digits
        if (errno == ERANGE) {
            assert(0); // overflow
        } else if (l >= INT32_MIN && l <= UINT32_MAX) {
            head = mkInt ((uint32_t) l);
        } else {
            head = mkDouble((double) l);
        }
    } else {
      char *tx;
      double x = strtod(s, &tx);
      if (tx == q && tx > p) {
        head = mkDouble (x);
      } else if (q > p) {
        head = mkName(strtonamel(p, q - p));
      } else {
        return NULL;
      }
    }
    head->next = tokens(q);
    return head;
}
