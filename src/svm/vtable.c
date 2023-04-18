// Implementation of hash table, modified from Hanson CII

// Will have to be looked at in module 12, but not until then.

#include <assert.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>

#include "gcmeta.h"
#include "vtable.h"
#include "vmheap.h"

#include "print.h"

#define T VTable_T
struct T {
        GCMETA(T)
        int size;  // number of buckets
	int length; // population
	unsigned timestamp;
	struct binding {
		struct binding *link;
		Value key;
		Value value;
	} **buckets;
};

void *firstbucket(T table) {
  void *p = table->buckets[0];
  if (p)
    fprintf(stderr, "First bucket is at %p\n", (void*)&table->buckets[0]);
  return p;
}

void *firstbucketaddr(T table) {
  return &table->buckets[0];
}

extern T* VTable_forwarded_ptr(T vtable) {
  assert(vtable);
  return &vtable->forwarded;
}

size_t VTable_size(int nbuckets) {
  T table;
  return sizeof (*table) + nbuckets * sizeof (table->buckets[0]);
}

T VTable_new(int hint) {
	int i;
	static int primes[] = { 5, 5, 11, 23, 47, 97, 197, 397, 509, 1021, 2053, 4093,
		8191, 16381, 32771, 65521, INT_MAX };
           // smaller sizes borrowed from Lua 3.0
	assert(hint >= 0);
	for (i = 1; primes[i] < hint; i++)
		;
        VMNEW(T, table, VTable_size(primes[i-1]));
	table->size = primes[i-1];
	table->buckets = (struct binding **)(table + 1);
	for (i = 0; i < table->size; i++)
		table->buckets[i] = NULL;
	table->length = 0;
	table->timestamp = 0;
	return table;
}

Value VTable_get(T table, Value key) {
	int i;
	struct binding *p;
	assert(table);
        uint32_t h = hashvalue(key);
	i = h % table->size;
	for (p = table->buckets[i]; p; p = p->link)
		if (identical(key, p->key))
			break;
                else if (0) {
                  fprintf(stderr, "Tags %d and %d: ", key.tag, p->key.tag);
                  fprintf(stderr, "Payloads %p and %p\n", (void*)key.s, (void*)p->key.s);
                  heapsearch("parameter's payload", key.s);
                  heapsearch("stored key's payload", p->key.s);
                  
                  fprint(stderr, "Key %v not identical to stored key %v", key, p->key);
                }
        if (0 && p == NULL) {
                for (int i = 0; i < table->size; i++)
                        if (table->buckets[i])
                                fprint(stderr, "bucket %d has key %v\n",
                                       i, table->buckets[i]->key);
        }
	return p ? p->value : nilValue;
}

void VTable_put(T table, Value key, Value value) {
	int i;
	struct binding *p;
	assert(table);
        if (value.tag == Nil) {
                VTable_remove(table, key);
        } else {
                i = hashvalue(key)%table->size;
                for (p = table->buckets[i]; p; p = p->link)
                        if (identical(key, p->key))
                                break;
                if (p == NULL) {
                        p = vmalloc_raw(sizeof(*p));
                        p->key = key;
                        p->link = table->buckets[i];
                        table->buckets[i] = p;
                        table->length++;
                }
                p->value = value;
                table->timestamp++;
        }
}
int VTable_length(T table) {
	assert(table);
	return table->length;
}

void VTable_internal_values(T table, void visit(Value *vp)) {
  assert(table);
  assert(visit);
//  fprintf(stderr, "table has %d buckets holding %d elements\n", table->size, table->length);
  for (int i = 0; i < table->size; i++)
    for (struct binding *p = table->buckets[i]; p; p = p->link) {
//      fprint(stderr, "visiting internal pair { %v |--> %v }\n", p->key, p->value);
//      fprintf(stderr, "before visit, internal key payload is %p\n", (void*)p->key.s);
      visit(&p->key);
      visit(&p->value);
//      fprintf(stderr, "after visit, internal key payload is %p\n", (void*)p->key.s);
    }
}

static struct binding *copy_chain(struct binding *p) {
  if (p == NULL) {
    return p;
  } else {
    struct binding *copy = vmalloc_raw(sizeof(*p));
//    fprint(stderr, "coping pair { %v |--> %v }\n", p->key, p->value);
    copy->key = p->key;
    copy->value = p->value;
    copy->link = copy_chain(p->link);
    return copy;
  } 
}

T VTable_copy(T old) {
  assert(old);
  VMNEW(T, new, VTable_size(old->size));
  memcpy(new, old, sizeof(*new));
  new->buckets = (struct binding **)(new + 1);
  for (int i = 0; i < old->size; i++)
    new->buckets[i] = copy_chain(old->buckets[i]);
  return new;
}

void VTable_remove(T table, Value key) {
	int i;
	struct binding **pp;
	assert(table);
	table->timestamp++;
	i = hashvalue(key)%table->size;
	for (pp = &table->buckets[i]; *pp; pp = &(*pp)->link)
		if (identical(key, (*pp)->key)) {
			struct binding *p = *pp;
			// Value value = p->value;
			*pp = p->link;
			// FREE(p);
			table->length--;
		}
}

//  void VTable_free(T *table) {
//  	assert(table && *table);
//  	if ((*table)->length > 0) {
//  		int i;
//  		struct binding *p, *q;
//  		for (i = 0; i < (*table)->size; i++)
//  			for (p = (*table)->buckets[i]; p; p = q) {
//  				q = p->link;
//  				FREE(p);
//  			}
//  	}
//  	FREE(*table);
//  }

