#include "fs.h"
#include <stdio.h>
#include <sys/stat.h>


void mv(const char* source, const char* target) {
    rename(source, target);
    chmod(target, 0777);
}