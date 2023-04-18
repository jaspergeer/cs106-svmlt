// The virtual-machine heap, eventually to be garbage-collected.

// Module 1 onward: Use for any data structure that is allocated by
// a VM instruction: all those structures are eventually supposed to be 
// garbage collected.  

// This interface is to be used directly to allocate blocks,
// functions, and closures.  It is also used indirectly by functions
// the create VMString, VTable_T, and VSeq_T.

#ifndef VMHEAP_INCLUDED
#define VMHEAP_INCLUDED

#include <stddef.h>
#include <stdbool.h>

#include "vmstate.h"

//// initialization and finalization

extern void heap_init(void);
extern void heap_shutdown(void);
  // returns all memory to the OS (to make valgrind happy)


//// Allocators, which never return NULL.

extern void *vmalloc_raw(size_t);
extern void *vmcalloc_raw(size_t, size_t);

#define VMNEW(TYPE,  P, N) TYPE P = vmalloc_raw(N); GCINIT(*P)
#define VMNEWC(TYPE, P, N) TYPE P = vmcalloc_raw(1, N); GCINIT(*P)

//// Module 11: GC interface

extern bool gc_needed;
  // signal from the heap that a GC is needed.  VM main loop will
  // call GC only in a safe state.

typedef struct VMState *VMState;
extern void gc(VMState vm);
  // recover all heap-allocated payloads reachable from this state,
  // and return any unused memory to the heap

extern void heapsearch(const char *what, void *p); 
  // For debugging: print a message to stderr saying
  // what part of the heap `p` is found in (if any).
  // The string `what` identifies `p` in the message.


extern bool vmalloc_islarge(size_t);
  // tells string client when not to intern

#endif /* VMHEAP_INCLUDED */
