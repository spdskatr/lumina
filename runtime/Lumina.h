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
typedef uint64_t Ref;

#define MKREF(v,f) mk_ref(f, (uint64_t)v)
#define DEREF(v,t) ((t)deref(v))
#define SET(u,v) set_ref(u, (uint64_t)v)
#define MKCL(x,len,tag,...) { uint64_t data[] = { __VA_ARGS__ }; x = alloc_closure(len,tag); memcpy(x+3, data, len * sizeof(uint64_t)); init_closure(x); }
#define CALLCL(t,cl,v) ((t (*)(Ref *, uint64_t))cl[3])(cl+4,v)
#define INCREF(v) inc_ref(v)
#define DECREF(v) dec_ref(v)

Ref *alloc_closure(uint64_t len, uint64_t tag);
Ref *mk_ref(int is_ref, uint64_t value);
uint64_t deref(Ref *v);
void set_ref(Ref *r, uint64_t new_v);

void init_closure(Ref *r);
void inc_ref(Ref *r);
void dec_ref(Ref *r);