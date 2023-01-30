//// List of tokens, to represent a line of virtual object code (.vo file)

// This data structure and its functions are essential for module 2:
// You'll need them to parse parts of virtual object code.
// Examples of their use can be found in file iparsers.c.

// The abstraction is designed so that you can see only the first token.  
//
// To consume the first token and move on to the next, you call one of
// the "`tokens_get`" functions.  If you need to make a decision based
// on the form of the first token, call `first_token_type`.

#ifndef TOKENS_H
#define TOKENS_H

#include <stdint.h>
#include <stdio.h>

#include "name.h"

//// create tokens by tokenizing a line; free their memory

typedef struct tokens *Tokens; // linked list of tokens, NULL if empty

Tokens tokens(const char *line); // result of tokenizing an input line
void free_tokens(Tokens *pp);   // Hanson style


//// observe first token, or print all tokens

enum tokentype { TNAME, TU32, TDOUBLE }; // name, unsigned 32-bit int, or double
enum tokentype first_token_type(Tokens);
  // given a non-NULL pointer, returns the type of the first token

void print_tokens(FILE *fp, Tokens ts); // OK even if NULL

//// mutate a list of tokens (while getting the value of the first)

// Each function pulls a value from a token list, which it shifts to
// the next token.  Pulling the wrong type is a checked run-time error.
//
// N.B. Each function takes the *address* of the tokens list, 
// not the list itself.  Address `p` must not be NULL, but
// safe to call if tokens list `*p` is NULL (will always report error).

// CAUTION: The functions overwrite a `Tokens` pointer.  Before using
// any of the functions, you will generally have to make a copy of the
// inital `Tokens` pointer so you can eventually call `free_tokens`.

uint32_t tokens_get_int   (Tokens *p, const char *original);
uint8_t  tokens_get_byte  (Tokens *p, const char *original);
Name     tokens_get_name  (Tokens *p, const char *original);
double   tokens_get_signed_number(Tokens *p, const char *original); 
              // get_signed_number also works on int32


#endif
