#pragma once

enum InsnTypes {
    R_TYPE,
    I_TYPE,
    J_TYPE,
    COP0,
    COP1,
};

enum ParamTypes {
    PARAM_NONE,
    PARAM_SWAP_RS_IMM,
    PARAM_BITSHIFT,
    PARAM_FLOAT_RT,
    PARAM_SWAP_RS_RT,
    PARAM_JAL,
    PARAM_JUMP,
    PARAM_JR,
    PARAM_LUI,
    PARAM_MULT_MOVE,
    PARAM_TRAP,
    PARAM_EMUX,
    PARAM_SYSCALL,
};

typedef struct PACKED {
    u16 rd        : 5;
    u16 shift_amt : 5;
    u16 function  : 6;
} RTypeData;

typedef struct PACKED {
    u16 opcode : 6;
    u16 rs     : 5;
    u16 rt     : 5;
    union {
        RTypeData rdata;
        u16 immediate;
    };
} Insn;

typedef struct PACKED {
    u16 opcode : 6;
    u16 fmt    : 5;
    u16 ft     : 5;
    u16 fs     : 5;
    u16 fd     : 5;
    u16 func   : 6;
} CzInsn;

typedef struct PACKED {
    u16 regimm : 6;
    u16 rs     : 5;
    u16 sub    : 5;
    u16 offset;
} BranchInsn;

typedef union {
    Insn i;
    CzInsn f;
    BranchInsn b;
    u32  d;
} InsnData;

typedef struct PACKED {
    u32 type;
    u32 arbitraryParam;
    u16 opcode   : 6;
    u16 function : 6;
    u8 name[10];
} InsnTemplate;

typedef struct PACKED {
    u32 type;
    u32 arbitraryParam;
    u16 function : 6;
    u8 name[10];
} COPzInsnTemplate;

#define OP_COP0 0b010000
#define OP_COP1 0b010001
#define OP_BRANCH 0b000001 // technically "REGIMM"

extern char *insn_disasm(InsnData *insn);
extern u8 insn_is_jal(Insn *i);
extern u8 insn_is_jalr(Insn *i);

