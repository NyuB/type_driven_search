#include "fs.h"
#include <stdio.h>

void mv(const char* source, const char* target) {
    rename(source, target);
}