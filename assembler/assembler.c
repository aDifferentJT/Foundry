// Handwritten simple C assembler

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "assembler.h"
#include "plugin.h"

#define LINE_LEN 256

typedef struct {
  char* name;
  int loc;
} label_t;

typedef struct _stmts_t {
  enum {
    Stmt_Label,
    Stmt_Inst,
  } type;
  union {
    label_t label;
    inst_t inst;
  } value;
  struct _stmts_t* next;
} stmts_t;

int findLabel(char* name, stmts_t* stmts) {
  while (stmts != NULL) {
    if (stmts->type == Stmt_Label && strcmp(name, stmts->value.label.name) == 0) {
      return stmts->value.label.loc;
    } else {
      stmts = stmts->next;
    }
  }
  return -1;
}

int encInt(int n, bit_t* dest, int width) {
  for (int i = 0; i < width; ++i) {
    dest[i] = n & 1 ? One : Zero;
    n = n >> 1;
  }
  return n;
}

void encArg(arg_t arg, bit_t* dest, void* data) {
  stmts_t* stmts = data;
  switch (arg.type) {
    case Arg_Literal:
      if (encInt(arg.value.literal, dest, arg.width)) {
        fprintf(stderr, "Literal argument %d doesn't fit\n", arg.value.literal);
      }
      break;
    case Arg_Label:
      {
        int loc = findLabel(arg.value.label, stmts);
        if (loc < 0) {
          fprintf(stderr, "Label %s not found\n", arg.value.label);
        } else {
          if (encInt(loc, dest, arg.width)) {
            fprintf(stderr, "Label argument %s doesn't fit, perhaps your program is larger than the address space\n", arg.value.label);
          }
        }
        break;
      }
    case Arg_Reg:
      switch (encReg(arg.value.reg, dest, arg.width)) {
        case Reg_Ok:
          break;
        case Reg_Not_Exist:
          fprintf(stderr, "Unknown register %s\n", arg.value.reg);
          break;
        case Reg_Enc_Wrong_Size:
          fprintf(stderr, "Register argument %s doesn't fit\n", arg.value.reg);
          break;
      }
      break;
  }
}

char* stripComment(char* str) {
  char* lastSemi = NULL;
  char* end = str;
  while (*(end + 1) != '\0') {
    if (*end == ';') {
      lastSemi = end;
    }
    end += 1;
  }
  if (lastSemi) {
    *lastSemi = '\0';
  }
  return str;
}

#define isWhitespace(x) *x == ' ' || *x == '\t' || *x == '\n' || *x == '\r'
char* stripWhitespace(char* str) {
  while (isWhitespace(str)) {
    str += 1;
  }
  char* lastSpace = NULL;
  char* end = str;
  while (*end != '\0') {
    if (isWhitespace(end)) {
      lastSpace = end;
      while (isWhitespace(end)) {
        end += 1;
      }
    } else {
      end += 1;
    }
  }
  if (lastSpace) {
    *lastSpace = '\0';
  }
  return str;
}
#undef isWhitespace

stmts_t** parseStmt(char* str, stmts_t** dest, int* currentPos) {
  stmts_t* stmt = malloc(sizeof(stmts_t));
  str = stripComment(str);
  str = stripWhitespace(str);
  size_t strLen = strlen(str);
  if (str[strLen - 1] == ':') {
    stmt->type = Stmt_Label;
    char* name = malloc(strLen);
    strncpy(name, str, strLen - 1);
    name[strLen - 1] = '\0';
    stmt->value.label.name = name;
    stmt->value.label.loc = *currentPos;
  } else {
    stmt->type = Stmt_Inst;
    parseInst(str, &stmt->value.inst);
    // *currentPos += stmt->value.inst.width; // Not this, this is bits
    *currentPos += 1;
  }
  stmt->next = NULL;
  *dest = stmt;
  return &stmt->next;
}

stmts_t* parseFile(FILE* f) {
  stmts_t* stmts = NULL;
  stmts_t** stmtsEnd = NULL;
  int currentPos = 0;
  char line[LINE_LEN];
  if (fgets(line, LINE_LEN, f)) {
    stmtsEnd = parseStmt(line, &stmts, &currentPos);
    while (fgets(line, LINE_LEN, f)) {
      stmtsEnd = parseStmt(line, stmtsEnd, &currentPos);
    }
  }
  return stmts;
}

void outputStmts(stmts_t* stmts, FILE* f) {
  stmts_t* stmt = stmts;
  while (stmt) {
    switch (stmt->type) {
      case Stmt_Label:
        break;
      case Stmt_Inst: {
                        bit_t* bs = malloc(stmt->value.inst.width * sizeof(bit_t));
                        encInst(stmt->value.inst, bs, stmts);
                        for (int i = 0; i < stmt->value.inst.width; i++) {
                          switch (bs[i]) {
                            case Zero:
                              fprintf(f, "0");
                              break;
                            case One:
                              fprintf(f, "1");
                              break;
                          }
                        }
                        fprintf(f, "\n");
                      }
    }
    stmt = stmt->next;
  }
}

int main(int argc, char* argv[]) {
  FILE* fIn = NULL;
  FILE* fOut = NULL;

  if (argc != 3) {
    fprintf(stderr, "Wrong number of command line arguments, USAGE %s <input> <output>\n", argv[0]);
    goto error;
  }
  fIn = fopen(argv[1], "r");
  if (fIn == NULL) {
    fprintf(stderr, "Input file %s does not exist\n", argv[1]);
    goto error;
  }
  fOut = strcmp(argv[2], "--") ? fopen(argv[2], "w") : stdout;
  if (fOut == NULL) {
    fprintf(stderr, "Output file %s does not exist\n", argv[2]);
    goto error;
  }

  stmts_t* stmts = parseFile(fIn);
  outputStmts(stmts, fOut);
  free(stmts);

  fclose(fIn);
  fclose(fOut);
  return 0;

error:
  if (fIn != NULL) { fclose(fIn); }
  if (fOut != NULL) { fclose(fOut); }
  return 1;
}

