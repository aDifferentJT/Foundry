// The header defining the interface for the machine generated part of the assembler

#ifndef Plugin_h
#define Plugin_h

#include <stdio.h>

typedef struct {
  enum {
    Arg_Literal,
    Arg_Label,
    Arg_Reg,
  } type;
  union {
    int literal;
    char* label;
    char* reg;
  } value;
  size_t width;
} arg_t;

typedef struct {
  char* name;
  size_t arg_count;
  arg_t* args;
  size_t width;
} inst_t;

typedef enum {
  Zero,
  One,
} bit_t;

typedef enum {
  Inst_Ok,
  Inst_Not_Exist,
  Inst_Wrong_Args,
} inst_error_t;

inst_error_t parseInst(char* str, inst_t* inst);

typedef enum {
  Reg_Ok,
  Reg_Not_Exist,
  Reg_Enc_Wrong_Size,
} reg_error_t;

reg_error_t encReg(char* reg, bit_t* dest, int width);

void encInst(inst_t inst, bit_t* dest, void* data);

#endif

