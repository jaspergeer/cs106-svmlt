// Virtual-machine strings

// They have a different representation from C strings.  You won't
// need this file until you want to implement instructions that
// allocate strings, or if you reach a stage where `AS_CSTRING` no
// longer meets your needs.

// The main idea here is to make it very efficient to use short
// strings as keys in hash tables.  That's important to us because
// that's how we handle global variables.

#ifndef VMSTRING_INCLUDED
#define VMSTRING_INCLUDED

#include <stddef.h>
#include <stdint.h>

#include "gcmeta.h"
// subject to change

typedef struct VMString {
  // that part of a string that is represented on the heap
  GCMETA(VMString)
  size_t length; // number of bytes not counting a secret trailing '\0'
  uint32_t hash; // if is zero and string is long, not hashed yet
  struct VMString *next_interned; // for interning table
  char bytes[];
} *VMString;

// invariant: an extra byte is allocated, and s->bytes[s->length] == '\0'

static inline size_t Vmstring_objsize(size_t len) {
  // size of a string object of the given length
  return sizeof(struct VMString) + (len + 1) * sizeof(char);
  // accounts for the hidden terminating '\0'
}

//// MEMORY MANAGEMENT: Memory allocated by functions in this interface
//// is allocated on the VM heap and is meant to be garbage collected.

//// initialization and finalization

extern void Vmstring_init(void);   // call before any other function in this interface
extern void Vmstring_finish(void); // recover for valgrind


//// string creation

// Strings can be created in two ways: from an existing sequence of bytes,
// or by accumulating bytes into a buffer.


VMString Vmstring_new(const char *p, size_t length);
  // Create string from sequence of bytes, which may include zeros.
  // Interned only if short.

VMString Vmstring_newc(const char *s);
  // Create string from a null-terminated C string.
  // Interned only if short.

VMString Vmstring_newlong(const char *p, size_t len);
  // Create sequence of bytes; force a long string.  


typedef struct StringBuffer *StringBuffer;

StringBuffer Vmstring_buffer(size_t length);
  // allocate a new, empty buffer

void Vmstring_putc(StringBuffer p, char c);
  // add a character to a buffer

void Vmstring_puts(StringBuffer p, VMString s);
  // add all characters of `s` to buffer

VMString Vmstring_of_buffer(StringBuffer *bp);
  // interns the contents of the buffer and frees its memory


//// garbage collection

void VMString_drop_dead_strings(void);


//// hashing

uint32_t Vmstring_hashbytes(const char *s, size_t len);
uint32_t Vmstring_hashlong(VMString s); // done on demand for long string?
static inline uint32_t Vmstring_hash(VMString s);


///////////////////////////// implementation below ////////////////

extern uint32_t Vmstring_hash_slow(VMString s);

static inline uint32_t Vmstring_hash(VMString s) {
  if (s->hash)
    return s->hash;
  else
    return Vmstring_hash_slow(s);
}



#endif
