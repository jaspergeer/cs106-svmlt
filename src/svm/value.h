// Representation of all VM values.

// This is the core of the VM, used heavily from module 1 on.

#ifndef VALUE_INCLUDED
#define VALUE_INCLUDED

#include <stdint.h>
#include <stdbool.h>

#include "iformat.h"

//// Each value is identified by one of the following tags ////

typedef enum { Nil = 0,   // vScheme value not found in uScheme
                          // (it's the initial value of an otherwise 
                          // undefined variable)

               Boolean,   // vScheme S-expressions
               Number,
               String,
               Emptylist,
               ConsCell,   // represented as Block (for now)

               VMFunction, // first-order function (sequence of instructions)
               VMClosure,  // higher-order function


               ///// stretch goals for more interesting languages

               Block,      // block of values of fixed size: record, etc
               Seq,        // variable-size sequence in the style of Hanson/Icon/CLU
               Table,      // hash table


               ///// potential support for writing primitives/libraries in C

               CFunction, 
               LightUserdata,   // pointer to data on the C heap
} VTag;

extern const char *tagnames[];  // map VTag to string


////////////////  representation of values

typedef double Number_T; // representation of a VM number

typedef struct Value {
  VTag tag;
  union {  // payloads for everything except Nil and EmptyList
    bool b;
    Number_T n;
    struct VMString *s;
    struct VMBlock *block;  // also ConsCell
    struct VMFunction *f;
    struct VMClosure *hof;
    struct VSeq_T *seq;
    struct VTable_T *table;
    struct CFunction *cf;
    void *p;               // light userdata == external memory 
  };
} Value;  // Note!  Not a pointer type!

////////////////////////////////////////////////////////////////

//////   creation/introduction for values

extern struct Value nilValue;       // initialized statically
extern struct Value emptylistValue; // initialized statically

// remaining creators are embedding functions

static inline Value mkNumberValue(Number_T n);
static inline Value mkStringValue(struct VMString *s);
static inline Value mkBooleanValue(bool b);
static inline Value mkVMFunctionValue(struct VMFunction *f);
static inline Value mkClosureValue(struct VMClosure *cl);
static inline Value mkBlockValue(struct VMBlock *bl);
static inline Value mkConsValue(struct VMBlock *bl);
static inline Value mkTableValue(struct VTable_T *table);

////// observation/elimination for values

// projection functions (to be called via macros below)

typedef struct VMState *VMState;  // for stack trace if projection fails

static inline struct VMBlock   *asBlock_  (VMState, Value, const char *file, int line);
static inline struct VMBlock   *asCons_   (VMState, Value, const char *file, int line);
static inline const char       *asCString_(VMState, Value, const char *file, int line);
static inline Number_T          asNumber_ (VMState, Value, const char *file, int line);
static inline struct VMClosure *asClosure_(VMState, Value, const char *file, int line);
static inline struct VMFunction *asVMFunction_ 
                                          (VMState, Value, const char *file, int line);
static inline struct VMString *asVMString_(VMState, Value, const char *file, int line);

static inline bool getTruth(VMState, Value v, const char *file, int line);

#define AS_BLOCK(VM, V)      asBlock_     ((VM), (V), __FILE__, __LINE__)
#define AS_CONS_CELL(VM, V)  asCons_      ((VM), (V), __FILE__, __LINE__)
#define AS_CSTRING(VM, V)    asCString_   ((VM), (V), __FILE__, __LINE__)
#define AS_NUMBER(VM, V)     asNumber_    ((VM), (V), __FILE__, __LINE__)
#define AS_CLOSURE(VM, V)    asClosure_   ((VM), (V), __FILE__, __LINE__)
#define AS_VMFUNCTION(VM, V) asVMFunction_((VM), (V), __FILE__, __LINE__)
#define AS_VMSTRING(VM, V)   asVMString_  ((VM), (V), __FILE__, __LINE__)
#define AS_BOOLEAN(VM, V)    asBoolean_    ((VM), (V), __FILE__, __LINE__)

#define GET_TRUTH(VM, V)    getTruth    ((VM), (V), __FILE__, __LINE__)

// additional observers for values

extern uint32_t hashvalue(Value v);
extern bool eqvalue  (Value v1, Value v2); // vscheme = primitive
extern bool identical(Value v1, Value v2); // for hashing
extern bool eqtests  (Value v1, Value v2); // for check-expect

////////////////////////////////////////////////////////////////
//
// representations

// when you're ready to do cons,car,cdr (from module 1 onward):

struct VMBlock {
  int nslots;
  struct Value slots[];
};


// when you implement first-order functions (module 7 and onward):

struct VMFunction {
  // Note: as yet, lacks GC support
  int arity; // number of args expected
  int size;  // number of instructions
  int nregs; // one more than the number of highest register read or written
  Instruction instructions[];
};

// when you implement higher-order functions (module 10 and onward):

struct VMClosure {
  struct VMFunction *f;
  struct VMClosure *base;
  int arity;
  int nslots;
  struct Value captured[];
};


// stretch goals:

typedef int (*CFunction_Code)(VMState state, void *env, Value *reg0, int nactuals);
    // result == number of things returned

struct CFunction { // a closure
  void *env; // environment
  CFunction_Code code;
  int arity;        // number of arguments expected
  int nregs; // one more than the number of highest register read or written
  const char *name; // for diagnostics
};


////////////////////////////////////////////////////////////////
// no user-serviceable parts past this point

#include <stdnoreturn.h>

#include "vmstring.h"

#ifndef GCVALIDATE
#define GCVALIDATE(E) (E) // in module 11, will replace with actual validation
#endif

extern _Noreturn void typeerror(VMState state, const char *expected, Value got,
                                const char *file, int line);


static inline struct VMBlock *asBlock_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != Block)
    typeerror(vm, "a block", v, file, line);
  return GCVALIDATE(v.block);
}
static inline struct VMBlock *asCons_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != ConsCell)
    typeerror(vm, "a cons cell", v, file, line);
  return GCVALIDATE(v.block);
}
static inline const char *asCString_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != String)
    typeerror(vm, "a string", v, file, line);
  return GCVALIDATE(v.s)->bytes;
}
static inline Number_T asNumber_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != Number)
    typeerror(vm, "a number", v, file, line);
  return v.n;
}
static inline struct VMClosure *asClosure_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != VMClosure)
    typeerror(vm, "a closure", v, file, line);
  return GCVALIDATE(v.hof);
}
static inline struct VMFunction *asVMFunction_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != VMFunction)
    typeerror(vm, "a VM function", v, file, line);
  return GCVALIDATE(v.f);
}
static inline struct VMString *asVMString_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != String)
    typeerror(vm, "a string", v, file, line);
  return GCVALIDATE(v.s);
}
static inline bool asBoolean_(VMState vm, Value v, const char *file, int line) {
  if (v.tag != Boolean)
    typeerror(vm, "a boolean", v, file, line);
  return v.b;
}

static inline bool getTruth(VMState vm, Value v, const char *file, int line) {
    switch (v.tag)
    {
    case Boolean:
      return v.b;
    case Number:
      return v.n != 0;
    // can add more cases for truthiness here
    default:
      typeerror(vm, "only number can be casted", v, file, line);      
    }
}

////////////////////////////////////////////////////////////////


static inline Value mkNumberValue(Number_T n) {
    Value val;
    val.tag = Number;
    val.n = n;
    return val;
}

static inline Value mkStringValue(struct VMString *s) {
    Value val;
    val.tag = String;
    val.s = s;
    return val;
}

static inline Value mkBooleanValue(bool b) {
  Value val;
  val.tag = Boolean;
  val.b = b;
  return val;
}

static inline Value mkVMFunctionValue(struct VMFunction *f) {
  Value val;
  val.tag = VMFunction;
  val.f = f;
  return val;
}

static inline Value mkClosureValue(struct VMClosure *cl) {
  Value val;
  val.tag = VMClosure;
  val.hof = cl;
  return val;
}

static inline Value mkBlockValue(struct VMBlock *bl) {
  Value val;
  val.tag = Block;
  val.block = bl;
  return val;
}

static inline Value mkConsValue(struct VMBlock *bl) {
  Value val;
  val.tag = ConsCell;
  val.block = bl;
  return val;
}

static inline Value mkTableValue(struct VTable_T *t) {
  Value val;
  val.tag = Table;
  val.table = t;
  return val;
}

static inline bool isFunction(Value v) {
  return v.tag == VMFunction || v.tag == VMClosure;
}

static inline bool isPair(Value v) {
  return v.tag == ConsCell;
}

static inline bool isSymbol(Value v) {
  return v.tag == String;
}

static inline bool isNumber(Value v) {
  return v.tag == Number;
}

static inline bool isBoolean(Value v) {
  return v.tag == Boolean;
}

static inline bool isNull(Value v) {
  return v.tag == Emptylist;
}

static inline bool isNil(Value v) {
  return eqvalue(v, nilValue);
}

static inline bool isVMClosure(Value v) {
  return v.tag == VMClosure;
}

static inline bool isVMFunction(Value v) {
  return v.tag == VMFunction;
}

#endif
