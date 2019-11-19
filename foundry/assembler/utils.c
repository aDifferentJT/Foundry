#include "utils.h"

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

// Return the position of x in the list that is the remaining arguments
int string_in(char* x, ...) {
  int res = 0;
  va_list ys;
  va_start(ys, x);
  for (int i = 1;; i++) {
    char* y = va_arg(ys, char*); // NOLINT
    if (y == NULL) {
      break;
    }
    if (strcmp(x, y) == 0) {
      res = i;
      break;
    }
  }
  va_end(ys);
  return res;
}

char* strdup(const char* src) {
  char* dest = malloc(sizeof(char) * (strlen(src) + 1));
  strcpy(dest, src); // NOLINT
  return dest;
}

