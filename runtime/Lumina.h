#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

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
#define REFCOUNT(v) v[0]
#define REFLENGTH(v) v[1]
#define REFTAG(v) v[2]
#define REFDATA(v) (v+3)
#define REFALLOC(i) calloc(3+(i), sizeof(Ref))
typedef uint64_t Ref;

#define MKREF(v,f) mk_ref(f, (uint64_t)v)
#define DEREF(v,t) ((t)deref(v))
#define SET(u,v) set_ref(u, (uint64_t)v)
#define MKCL(x,len,tag,...) { uint64_t data[] = { __VA_ARGS__ }; x = mk_closure(len, tag, data); }
#define CALLCL(t,cl,v) ((t (*)(Ref *, uint64_t))cl[3])(cl+4,v)
#define INCREF(v) inc_ref(v)
#define DECREF(v) dec_ref(v)

Ref *mk_closure(uint64_t len, uint64_t tag, uint64_t *data);
Ref *mk_ref(int is_ref, uint64_t value);
uint64_t deref(Ref *v);
void set_ref(Ref *r, uint64_t new_v);

// Reference counting garbage collection utilities
void init_closure(Ref *r);
void inc_ref(Ref *r);
void dec_ref(Ref *r);