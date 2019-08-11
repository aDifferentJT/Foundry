#ifndef Utils_h
#define Utils_h

int string_in(char* x, ...);

#define copy_list(x, n, t, ...) do { t _ ## x [] = { __VA_ARGS__ }; memcpy(x, _ ## x, n * sizeof(t)); } while(0)

#endif

