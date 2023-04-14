#include "Lumina.h"

#ifdef _LUMINA_GC
#define gclog(...) fprintf(stderr, __VA_ARGS__)
#else
#define gclog(...)
#endif

Ref *alloc_closure(uint64_t len, uint64_t tag) {
    Ref *r = calloc(3 + len, sizeof(Ref));
    gclog("Created closure %p\n", r);
    r[0] = 1;
    r[1] = len;
    r[2] = tag;
    return r;
}

Ref *mk_ref(int is_ref, uint64_t value) {
    Ref *r = calloc(4, sizeof(Ref));
    gclog("Created reference %p\n", r);
    r[0] = 1;
    r[1] = 1;
    if (is_ref) {
        r[2] = 1;
        // Increment reference count of value since it's about to get stored
        inc_ref((Ref *)value);
    }
    r[3] = value;
    return r;
}

uint64_t deref(Ref *ref) {
    if (ref[2] & 1) {
        // Increment reference count of value since it's about to get returned
        inc_ref((Ref *)ref[3]);
    }
    return ref[3];
}

void set_ref(Ref *ref, uint64_t value) {
    if (ref[2] & 1) {
        // Decrement reference count for previous reference
        dec_ref((Ref *)ref[3]);
        // And increment reference count for new reference
        inc_ref((Ref *)value);
    }
    ref[3] = value;
}

void init_closure(Ref *ref) {
    // Increment the references of the pointers as needed
    uint64_t flags = ref[2];
    while (flags) {
        int pos = ffsl(flags) - 1;
        inc_ref((Ref *)ref[3+pos]);
        flags ^= (1<<pos);
    }
}

void inc_ref(Ref *ref) {
    ref[0]++;
    gclog("Reference at %p now %ld\n", ref, ref[0]);
}

void dec_ref(Ref *ref) {
    ref[0]--;
    gclog("Reference at %p now %ld\n", ref, ref[0]);
    if (ref[0] == 0) {
        gclog("Deallocating %p\n", ref);
        // Recursively decrement reference counts for child references
        uint64_t flags = ref[2];
        while (flags) {
            int pos = ffsl(flags) - 1;
            dec_ref((Ref *)ref[3+pos]);
            flags ^= (1<<pos);
        }
        // Deallocate
        free(ref);
    }
}