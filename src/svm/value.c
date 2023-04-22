// Values and functions promised in value.h

// If you want to see how the various forms of equality test work,
// this file might be worth poking at.  Otherwise it does what it 
// says on the tin.

#include <assert.h>

#include "value.h"
#include "vmstring.h"

Value nilValue;

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

Value emptylistValue = { Emptylist };

bool identical(Value v1, Value v2) { // object identity, needed for hashing!
  if (v1.tag != v2.tag)
    return false;
  else
    switch (v1.tag) {
    case Nil: return true;
    case Boolean: return v1.b == v2.b;
    case Number:  return v1.n == v2.n;
    case Emptylist: return true;
    case String:  return v1.s == v2.s;  // all strings assumed interned
    case Table:   return v1.table == v2.table;  // object identity
    case Seq:     return v1.seq == v2.seq;  // object identity from here on out
    case ConsCell:return v1.block == v2.block;
    case Block:   return v1.block == v2.block;
    case VMFunction: return v1.f == v2.f;
    case CFunction: return  v1.cf == v2.cf;
    case VMClosure: return  v1.hof == v2.hof;
    case LightUserdata: return v1.p == v2.p;
    default:  assert(0); 
    }
}


// Except for strings, numbers, and booleans, all values of
// the same type hash to the same slot.  You are warned.

// hash values from /dev/urandom

#define NILHASH   0x6f909293
#define NUMHASH   0xaea24ac5
#define TRUEHASH  0x14db0c2b 
#define FALSEHASH 0x3f6d7a1c
#define EMPTYHASH 0x2a15a97c
#define TABHASH   0x00259d4c
#define SEQHASH   0xc060fa09
#define CONSHASH  0xa8ee9d58
#define BLOCKHASH 0x798eedfc
#define FUNHASH   0x5a13b92c
#define CFUNHASH  0x3c059f75
#define CLOHASH   0xc940c06a

uint32_t hashvalue(Value v) {
    switch (v.tag) {
    case Nil:           return NILHASH;
    case Boolean:       return v.b ? TRUEHASH : FALSEHASH;
    case Number:        return NUMHASH ^ (uint32_t) v.n;
    case Emptylist:     return EMPTYHASH;
    case String:        return Vmstring_hash(v.s);
    case Table:         return TABHASH;
    case Seq:           return SEQHASH;
    case ConsCell:      return CONSHASH;
    case Block:         return BLOCKHASH;
    case VMFunction:    return FUNHASH;
    case CFunction:     return CFUNHASH;
    case VMClosure:     return CLOHASH;
    case LightUserdata: return (uint32_t) ((uintptr_t) v.p >> 3);
    default:  assert(0); 
    }

}


//// the vscheme = primitive.  

bool eqvalue(Value v1, Value v2) { // Will not work for hashing!
  if (v1.tag != v2.tag)
    return false;
  else
    switch (v1.tag) {
    case Nil: return true;
    case Boolean: return v1.b == v2.b;
    case Number:  return v1.n == v2.n;
    case String:  return v1.s == v2.s;  // all strings assumed interned
    case Table:   return v1.table == v2.table;  // object identity
    case Seq:     return v1.seq == v2.seq;  // object identity
    case ConsCell: return false;
    case Block:    return false;
    case Emptylist: return true;
    case VMFunction: return false;
    case CFunction: return false;
    case VMClosure: return false;
    case LightUserdata: return false;
    default:  assert(0); return false; // not implemented yet
    }
}

//// the test used in uscheme check-expect

bool eqtests(Value v1, Value v2) { // will not work for hashing!
  if (v1.tag == v2.tag && v1.tag == ConsCell) {
    return eqtests(v1.block->slots[0], v2.block->slots[0]) &&
           eqtests(v1.block->slots[1], v2.block->slots[1]);
  } else if (v1.tag == v2.tag && v1.tag == Block && v1.block->nslots == v2.block->nslots) {
    if (v1.block == v2.block)
      return true;
    for (int i = 0; i < v1.block->nslots; i++)
      if (!eqtests(v1.block->slots[i], v2.block->slots[i]))
        return false;
    return true;
  } else {
    return eqvalue(v1, v2);
  }
}


/////////////////////////


const char *tagnames[] = {
   "nil",
   "a boolean", "a number", "a string", "the empty list", "a cons cell",
   "a VM function", "a closure",
   "a record", "a sequence", "a table", 
   "a C function", "a pointer",
};
                          
