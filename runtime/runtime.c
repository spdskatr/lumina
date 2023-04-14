#include "Lumina.h"

Ref *_mk_ref(int is_closure, uint64_t value) {
    Ref *r = malloc(sizeof(Ref));
    r->tag = 2 | (is_closure & 1);
    r->value = value;
    return r;
}

void _set_ref(Ref *ref, uint64_t value) {
    // TODO: Decrement reference count for previous reference
    ref->value = value;
}