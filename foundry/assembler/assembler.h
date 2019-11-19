#ifndef Assembler_h
#define Assembler_h

#include "plugin.h"

void parseIntArg(arg_t* arg, size_t width, char* str, char** saveptr);
void parseRegArg(arg_t* arg, size_t width, char* str, char** saveptr);
void encArg(arg_t arg, bit_t* dest, void* data);

#endif

