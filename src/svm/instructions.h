// Promised list of all loadable instructions.

// Every instruction needs an entry in the table defined here.
// See files itable.h and instructions.c.

#ifndef INSTRUCTIONS_INCLUDED
#define INSTRUCTIONS_INCLUDED


#include "itable.h"

extern struct instruction_info instructions[];
extern int number_of_instructions;


#endif
