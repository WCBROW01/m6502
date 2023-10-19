#ifndef M6502_INSTRUCTIONS
#define M6502_INSTRUCTIONS

#include <stdint.h>

#include "instructions.h"

// struct containing function call and address mode for each opcode
typedef void (*inst_func)(CPU *, addr_mode);

typedef struct {
    inst_func func;
    addr_mode mode;
} opcode_t;

// table containing opcode information
extern const opcode_t OPCODE_TABLE[256];

#endif
