#include <stdio.h>
#include <stdbool.h>

#define XNOR ==

#define BRANCH(c,th,el) if (c) { goto th; } { goto el; }