#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "Lumina.h"

// Can enable debug logging of the garbage collecor with -D_LUMINA_GC
#ifdef _LUMINA_GC
#define gclog(...) fprintf(stderr, __VA_ARGS__)
#else
#define gclog(...)
#endif

// Can track the number of allocated references (and crash if there are
// allocated references at exit) with -D_LUMINA_TRACKREF
#ifdef _LUMINA_TRACKREF
int refcount = 0;
#define REFALLOC(i) (refcount++,calloc(3+(i), sizeof(Ref)))
#define REFFREE(r) (refcount--,free(r))
#else
#define REFALLOC(i) calloc(3+(i), sizeof(Ref))
#define REFFREE(r) free(r)
#endif

void init_closure(Ref *r);

Ref *alloc_closure(uint64_t len, uint64_t tag) {
    Ref *r = REFALLOC(len);
    gclog("Created closure %p\n", r);
    REFCOUNT(r) = 1;
    REFLENGTH(r) = len;
    REFTAG(r) = tag;
    return r;
}

Ref *mk_closure(uint64_t len, uint64_t tag, uint64_t *data) {
    Ref *r = alloc_closure(len, tag);
    memcpy(REFDATA(r), data, len * sizeof(uint64_t));
    // Initialise reference counts
    init_closure(r);
    return r;
}

Ref *mk_ref(int is_ref, uint64_t value) {
    Ref *r = REFALLOC(1);
    gclog("Created reference %p\n", r);
    REFCOUNT(r) = 1;
    REFLENGTH(r) = 1;
    if (is_ref) {
        REFTAG(r) = 1;
        // Increment reference count of value since it's about to get stored
        inc_ref((Ref *)value);
    }
    REFDATA(r)[0] = value;
    return r;
}

uint64_t deref(Ref *r) {
    if (REFTAG(r) & 1) {
        // Increment reference count of value since it's about to get returned
        inc_ref((Ref *)REFDATA(r)[0]);
    }
    return REFDATA(r)[0];
}

void set_ref(Ref *r, uint64_t value) {
    if (REFTAG(r) & 1) {
        // Increment reference count for new reference
        inc_ref((Ref *)value);
        // Decrement reference count for previous reference
        dec_ref((Ref *)REFDATA(r)[0]);
    }
    REFDATA(r)[0] = value;
}

void init_closure(Ref *r) {
    // Increment the references of the pointers as needed
    uint64_t flags = REFTAG(r);
    while (flags) {
        int pos = ffsl(flags) - 1;
        inc_ref((Ref *)REFDATA(r)[pos]);
        flags ^= (1<<pos);
    }
}

void inc_ref(Ref *r) {
    REFCOUNT(r)++;
    gclog("Reference at %p now %ld\n", r, REFCOUNT(r));
}

void dec_ref(Ref *r) {
    REFCOUNT(r)--;
    gclog("Reference at %p now %ld\n", r, REFCOUNT(r));
    if (REFCOUNT(r) == 0) {
        gclog("Deallocating %p\n", r);
        // Recursively decrement reference counts for child references
        uint64_t flags = REFTAG(r);
        while (flags) {
            int pos = ffsl(flags) - 1;
            dec_ref((Ref *)REFDATA(r)[pos]);
            flags ^= (1<<pos);
        }
        // Deallocate
        REFFREE(r);
    }
}

void pre_exit() {
#ifdef _LUMINA_TRACKREF
    if (refcount != 0) {
        fprintf(stderr, "Number of active references at program exit was nonzero (%d). Did we memory leak?\n", refcount);
        exit(1);
    }
#endif
}