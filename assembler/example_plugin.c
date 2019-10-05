// Handwritten example for machine generated plugin to the assembler

#include "assembler.h"
#include "plugin.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

inst_error_t parseInst(char* str, inst_t* inst) {
  char* tok = strtok_r(str, " ", &str);
  inst->name = strdup(tok);
  inst->width = 8;
  if (strcmp(tok, "halt") == 0) {
    inst->arg_count = 0;
    inst->args = NULL;
  } else if (string_in(tok, "ldi", "add", "sub", "ldm", "stm", NULL)) {
    inst->arg_count = 1;
    inst->args = malloc(sizeof(arg_t));
    inst->args[0].type = Arg_Literal;
    inst->args[0].width = 4;
    char* tok = strtok_r(str, " ", &str);
    char* p;
    inst->args[0].value.literal = strtol(tok, &p, 0);
    if (*p != '\0') {
      fprintf(stderr, "%s instruction takes an Int argument\n", tok);
      return Inst_Wrong_Args;
    }
  } else if (strcmp(tok, "jp") == 0 || strcmp(tok, "jpz") == 0) {
    inst->arg_count = 1;
    inst->args = malloc(sizeof(arg_t));
    inst->args[0].type = Arg_Literal;
    inst->args[0].width = 4;
    char* tok = strtok_r(str, " ", &str);
    char* p;
    inst->args[0].value.literal = strtol(tok, &p, 0);
    if (*p != '\0') {
      inst->args[0].type = Arg_Label;
      inst->args[0].value.label = strdup(tok);
    }
  } else {
    fprintf(stderr, "instruction %s not recognised\n", tok);
    return Inst_Not_Exist;
  }
  return Inst_Ok;
}

reg_error_t encReg(char* reg, bit_t* dest, int width) {
  return Reg_Not_Exist;
}

void encInst(inst_t inst, bit_t* dest, void* data) {
#define Z Zero
#define O One
  int i;
  if (strcmp(inst.name, "halt") == 0) {
    copy_list(dest, 8, bit_t, Z,Z,Z,Z,Z,Z,Z,Z);
  } else if ((i = string_in(inst.name, "ldi", "add", "sub", "ldm", "stm", "jp", "jpz", NULL))) {
    switch (i) {
      case 1: // ldi
          copy_list(dest, 4, bit_t, Z,Z,Z,O);
          break;
      case 2: // add
          copy_list(dest, 4, bit_t, Z,Z,O,Z);
          break;
      case 3: // sub
          copy_list(dest, 4, bit_t, Z,Z,O,O);
          break;
      case 4: // ldm
          copy_list(dest, 4, bit_t, Z,O,Z,Z);
          break;
      case 5: // stm
          copy_list(dest, 4, bit_t, Z,O,Z,O);
          break;
      case 6: // jp
          copy_list(dest, 4, bit_t, Z,O,O,Z);
          break;
      case 7: // jpz
          copy_list(dest, 4, bit_t, Z,O,O,O);
          break;
      default:
        break;
    }
    encArg(inst.args[0], dest + 4, data);
  }
#undef Z
#undef O
}

