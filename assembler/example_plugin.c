// Handwritten example for machine generated plugin to the assembler

#include "assembler.h"
#include "plugin.h"
#include "utils.h"

#include <stdlib.h>
#include <string.h>

inst_error_t parseInst(char* str, inst_t* inst) {
  char* instName = strtok_r(str, " ", &str);
  inst->name = strdup(instName);
  inst->width = 8;
  if (strcmp(instName, "halt") == 0) {
    inst->arg_count = 0;
    inst->args = NULL;
  } else if (string_in(instName, "ldi", "add", "sub", "ldm", "stm", "jp", "jpz", NULL)) {
    inst->arg_count = 1;
    inst->args = malloc(sizeof(arg_t));
    parseIntArg(&inst->args[0], 4, str, &str);
  } else {
    fprintf(stderr, "instruction %s not recognised\n", instName);
    return Inst_Not_Exist;
  }
  return Inst_Ok;
}

reg_error_t encReg(char* reg, bit_t* dest, int width) {
  return Reg_Not_Exist;
}

bit_t* encInst(inst_t inst, void* data) {
#define Z Zero
#define O One
  bit_t* dest = malloc(inst.width * sizeof(bit_t));
  if (strcmp(inst.name, "halt") == 0) {
    copy_list(dest, 8, bit_t, Z,Z,Z,Z,Z,Z,Z,Z);
  } else {
    switch (string_in(inst.name, "ldi", "add", "sub", "ldm", "stm", "jp", "jpz", NULL)) {
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
  return dest;
#undef Z
#undef O
}

