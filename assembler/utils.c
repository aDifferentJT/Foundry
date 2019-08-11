#include "utils.h"

#include <stdarg.h>
#include <string.h>

int string_in(char* x, ...) {
  int res = 0;
  va_list ys;
  va_start(ys, x);
  for (int i = 1;; i++) {
    char* y;
    if ((y = va_arg(ys, char*))) {
      if (strcmp(x, y) == 0) {
        res = i;
        break;
      }
    } else {
      break;
    }
  }
  va_end(ys);
  return res;
}

