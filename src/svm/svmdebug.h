// Parses the SVMDEBUG environment variable

// Optional, but useful for debugging.
//
// If you want to add "printf debugging" or other styles
// of debugging to your VM code, including the `idump`
// function from file disasm.h, you'll want to use the
// SVMDEBUG environment variable with this interface.

#ifndef SVMDEBUG_INCLUDED
#define SVMDEBUG_INCLUDED

const char *svmdebug_value(const char *key);
  // query the SVMDEBUG variable and report back the value
  // associated with the given field

// Rules for SVMDEBUG:
//
//   - It is a comma-separated list of assignments.
//
//   - Each assignment has the form key=value.
//     The key may not contain spaces, = signs, brackets, or commas.
//
//   - The =value is optional; value defaults to "true".
//
//   - The value must contain balanced brackets.
//
//   - The value can be escaped with curly brackets.  The outermost
//     brackets are removed.  This is necessary only if the value
//     contains a comma.
//
// Example command-line usage: turn on 'decode' debugging, and on halt
// dump the first 5 registers:
//
//    env SVMDEBUG="decode,halt-dump=5" svm test.vo
//
// (Your SVM will have to have code that calls `svmdebug_value("decode")`
// and `svmdebug_value("halt-dump")`, and it will have to do something
// in response to the values returned.)

void svmdebug_finalize(void);
  // if `svmdebug_value` was ever called, call this to free memory

#endif
