#include "fs.h"
#include <stdio.h>

int mv(const char* source, const char* target) {
    return rename(source, target);
}