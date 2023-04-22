#include <stdint.h>

#define XNOR ==
#define BRANCH(c,th,el) if (c) { goto th; } { goto el; }

// A reference is implemented as a simple array of uint64_t.
// Index 0      : Reference count
// Index 1      : Length of Data (in uint64_ts)
// Index 2      : Tag
// Index 3,4,...: Data
//
// The tag is a 64-bit bitmask identifying whether each index of the data is a
// pointer.
// A closure is implemented as a reference where the item at index 3 is a
// function pointer.
typedef uint64_t Ref;

#define REFCOUNT(v) v[0]
#define REFLENGTH(v) v[1]
#define REFTAG(v) v[2]
#define REFDATA(v) (v+3)

#define MKREF(v,f) mk_ref(f, (uint64_t)v)
#define DEREF(v,t) ((t)deref(v))
#define SET(u,v) set_ref(u, (uint64_t)v)
#define MKCL(x,len,tag,...) { uint64_t data[] = { __VA_ARGS__ }; x = mk_closure(len, tag, data); }
#define CALLCL(t,cl,v) ((t (*)(Ref *, uint64_t))REFDATA(cl)[0])(REFDATA(cl)+1,v)
#define INCREF(v) inc_ref(v)
#define DECREF(v) dec_ref(v)

Ref *mk_closure(uint64_t len, uint64_t tag, uint64_t *data);
Ref *alloc_closure(uint64_t len, uint64_t tag);
Ref *mk_ref(int is_ref, uint64_t value);
uint64_t deref(Ref *v);
void set_ref(Ref *r, uint64_t new_v);

// Reference counting garbage collection utilities
void inc_ref(Ref *r);
void dec_ref(Ref *r);

// Exit hook for the runtime if it needs to do anything before exit (e.g.
// profiling)
void pre_exit();