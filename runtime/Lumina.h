#include <stdio.h>
#include <stdbool.h>

#define XNOR ==

#define BRANCH(c,th,el) if (c) { goto th; } { goto el; }

#define ENTRY(f) int main() { printf("%lld\n", f(0)); return 0; }