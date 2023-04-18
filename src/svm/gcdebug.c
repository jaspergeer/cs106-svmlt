#include <assert.h>

#include "gcdebug.h"
#include "svmdebug.h"
#include "print.h"

#ifndef NOVALGRIND
  #include <valgrind/memcheck.h>
#else

  #define VALGRIND_CREATE_BLOCK(p, n, s)     ((void)(p),(void)(n),(void)(s))
  #define VALGRIND_CREATE_MEMPOOL(p, n, z)   ((void)(p),(void)(n),(void)(z))
  #define VALGRIND_MAKE_MEM_DEFINED_IF_ADDRESSABLE(p, n) \
                                             ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_DEFINED(p, n)    ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_UNDEFINED(p, n)  ((void)(p),(void)(n))
  #define VALGRIND_MAKE_MEM_NOACCESS(p, n)   ((void)(p),(void)(n))
  #define VALGRIND_MEMPOOL_ALLOC(p1, p2, n)  ((void)(p1),(void)(p2),(void)(n))
  #define VALGRIND_MEMPOOL_FREE(p1, p2)      ((void)(p1),(void)(p2))

#endif

static int gc_pool_object;
static void *gc_pool = &gc_pool_object;  /* valgrind needs this */

static const char *debug;

void gc_debug_init(void) {
  debug = svmdebug_value("gc");
  VALGRIND_CREATE_MEMPOOL(gc_pool, 0, 0);
}

void gc_debug_make_undefined(void *mem, size_t nbytes) {
  VALGRIND_MAKE_MEM_UNDEFINED(mem, nbytes);
}

void gc_debug_invalidate(void *mem, size_t nbytes) {
  VALGRIND_MAKE_MEM_NOACCESS(mem, nbytes);
}

void gc_debug_notify_allocated(void *page, size_t bytes) {
  (void)page;
  (void)bytes;
}

void gc_debug_notify_free(void *page, size_t bytes) {
  (void)page;
  (void)bytes;
}



void gc_debug_post_acquire(void *mem, size_t nbytes) {
  gcprintf("ACQUIRE %zu at %p\n", nbytes, mem);
  VALGRIND_CREATE_BLOCK(mem, nbytes, "managed page");
  VALGRIND_MEMPOOL_ALLOC(gc_pool, mem, nbytes); // new, makes undefined
//  VALGRIND_MAKE_MEM_NOACCESS(mem, nbytes); // new out
}

void gc_debug_pre_release(void *mem, size_t nbytes) {
  gcprintf("RELEASE %zu at %p\n", nbytes, mem);
  VALGRIND_MAKE_MEM_NOACCESS(mem, nbytes);
}

void gc_debug_pre_allocate(void *payload, size_t nbytes) {
    gcprintf("ALLOC %zu at %p (next is %p)\n", nbytes, payload, (char *)payload + nbytes);
//    VALGRIND_MEMPOOL_ALLOC(gc_pool, payload, nbytes);  // new out
    VALGRIND_MAKE_MEM_UNDEFINED(payload, nbytes);
}

void gc_debug_post_reclaim_page(void *page, size_t bytes) {
    gcprintf("FREE PAGE (%zu bytes) at %p [to %p]\n", bytes, page, (char *)page + bytes);
    VALGRIND_MEMPOOL_FREE(gc_pool, page);
}


void gcprint(const char *fmt, ...) {
  if (debug) {
    va_list_box box;
    Printbuf buf = printbuf();

    assert(fmt);
    va_start(box.ap, fmt);
    vbprint(buf, fmt, &box);
    va_end(box.ap);
    fwritebuf(buf, stderr);
    fflush(stderr);
    freebuf(&buf);
  }
}

void gcprintf(const char *fmt, ...) {
  if (debug) {
    va_list args;

    assert(fmt);
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
    fflush(stderr);
  }
}
