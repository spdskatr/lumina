#include "Lumina.h"

Ref *_mk_ref(int is_closure, uint64_t value) {
    Ref *r = calloc(3, sizeof(Ref));
    r[0] = 2 | (is_closure & 1);
    r[1] = 1;
    r[2] = value;
    return r;
}

Ref _deref(Ref *ref) {
    return ref[2];
}

void _set_ref(Ref *ref, uint64_t value) {
    // TODO: Decrement reference count for previous reference
    // And increment reference count for new reference
    ref[2] = value;
}