//// Strings with constant-time equality test (pointer equality).

// You need to understand this for module 2, so you can identify
// names in the loader.

// This abstraction is variously called "atom", "name", "symbol",
// and probably other things.  The idea is you take a string
// and convert it to a pointer such that two strings are
// equal if and only if they convert to the *same* pointer.
// In algebraic laws,
//
//   strtoname(s) == strtoname(t)  if and only if   strcmp(s,t) == 0


#ifndef NAME_INCLUDED
#define NAME_INCLUDED

typedef struct Name *Name;

const char* nametostr(Name np);
Name strtoname(const char *s);
Name strtonamel(const char *s, int length);  // may not contain an embedded \0

void name_cleanup(void); // return all memory (make Valgrind happy)

#endif
