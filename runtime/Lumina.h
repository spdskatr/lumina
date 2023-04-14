#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define XNOR ==
#define BRANCH(c,th,el) if (c) { goto th; } { goto el; }

typedef uint64_t Ref;
// A reference is implemented as a simple array of uint64_t.
// Index 0      : Tag
// Index 1      : Length of Data (in uint64_ts)
// Index 2,3,...: Data
//
// The tag follows OCaml convention.
// The last bit of the tag tells you whether value should be interpreted as a
// pointer (0 = true, 1 = false), the remaining bits count the number of
// references

#define MKREF(v,f) _mk_ref(f, (uint64_t)v)
#define DEREF(v,t) ((t)_deref(v))
#define SET(u,v) _set_ref(u,(uint64_t)v);

Ref *_mk_ref(int is_closure, uint64_t value);
Ref _deref(Ref *v);
void _set_ref(Ref *r, uint64_t new_v);