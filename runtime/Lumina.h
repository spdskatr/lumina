#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define XNOR ==
#define BRANCH(c,th,el) if (c) { goto th; } { goto el; }

typedef enum reftag : unsigned int {
    T_INT = 0,
    T_REF = 1,
    T_CLOSURE = 2
} reftag_t;

typedef struct ref {
    // OCaml convention. The last bit of the tag tells you whether value should
    // be interpreted as a pointer (0 = true, 1 = false), the remaining bits
    // count the number of references
    uint64_t tag;
    uint64_t value;
} Ref;

#define MKREF(v,f) _mk_ref(f, (uint64_t)v)
#define DEREF(v,t) ((t)v->value)
#define SET(u,v) _set_ref(u,(uint64_t)v);

Ref *_mk_ref(int is_closure, uint64_t value);
void _set_ref(Ref *r, uint64_t new_v);