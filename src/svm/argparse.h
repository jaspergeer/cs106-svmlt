#ifndef ARGPARSE_INCLUDED
#define ARGPARSE_INCLUDED

#include "value.h"

extern Value arglist(int argc, char **argv);
  // return a list of S-expressions, one for each argument
  // (OK for argc to be negative, in which case list is empty

#endif
