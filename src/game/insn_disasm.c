#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"

enum InsnTypes {
    R_TYPE,
    I_TYPE,
    J_TYPE,
    // BRANCH,
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
};

extern far char *parse_map(u32 pc);
static char insn_as_string[100];

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

InsnTemplate insn_db[] = {
    // arithmetic
    {R_TYPE, PARAM_NONE, 0, 0b100000, "ADD"},
    {R_TYPE, PARAM_NONE, 0, 0b100001, "ADDU"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b001000, 0, "ADDI"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b001001, 0, "ADDIU"},
    {R_TYPE, PARAM_NONE, 0, 0b100010, "SUB"},
    {R_TYPE, PARAM_NONE, 0, 0b100011, "SUBU"},
    {R_TYPE, PARAM_NONE, 0, 0b011000, "MULT"},
    {R_TYPE, PARAM_NONE, 0, 0b011001, "MULTU"},
    {R_TYPE, PARAM_NONE, 0, 0b011010, "DIV"},
    {R_TYPE, PARAM_NONE, 0, 0b011011, "DIVU"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010000, "MFHI"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010001, "MTHI"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010010, "MFLO"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010011, "MTLO"},

    {R_TYPE, PARAM_NONE, 0, 0b101010, "SLT"},
    {R_TYPE, PARAM_NONE, 0, 0b101011, "SLTU"},

    {I_TYPE, PARAM_NONE, 0b001010, 0, "SLTI"},
    {I_TYPE, PARAM_NONE, 0b001011, 0, "SLTIU"},


    // bitwise ops
    {R_TYPE, PARAM_NONE, 0, 0b100100, "AND"},
    {I_TYPE, PARAM_NONE, 0b001100, 0, "ANDI"},
    {R_TYPE, PARAM_NONE, 0, 0b100101, "OR"},
    {I_TYPE, PARAM_NONE, 0b001101, 0, "ORI"},
    {R_TYPE, PARAM_NONE, 0, 0b100110, "XOR"},
    {I_TYPE, PARAM_NONE, 0b001110, 0, "XORI"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b100110, "SLL"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000100, "SLLV"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b000010, "SRL"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000110, "SRLV"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b000011, "SRA"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000111, "SRAV"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b100111, "NOR"},

    // load/store
    {I_TYPE, PARAM_LUI,  0b001111, 0, "LUI"},
    {I_TYPE, PARAM_NONE, 0b100000, 0, "LB"},
    {I_TYPE, PARAM_NONE, 0b100100, 0, "LBU"},
    {I_TYPE, PARAM_NONE, 0b101000, 0, "SB"},
    {I_TYPE, PARAM_NONE, 0b100001, 0, "LH"},
    {I_TYPE, PARAM_NONE, 0b100101, 0, "LHU"},
    {I_TYPE, PARAM_NONE, 0b101001, 0, "SH"},
    {I_TYPE, PARAM_NONE, 0b100011, 0, "LW"},
    {I_TYPE, PARAM_NONE, 0b101011, 0, "SW"},
    {I_TYPE, PARAM_NONE, 0b110111, 0, "LD"},
    {I_TYPE, PARAM_NONE, 0b111111, 0, "SD"},
    {I_TYPE, PARAM_FLOAT_RT, 0b110001, 0, "LWC1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b111001, 0, "SWC1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b110101, 0, "LDC1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b111101, 0, "SDC1"},
    // unaligned
    {I_TYPE, PARAM_NONE, 0b100010, 0, "LWL"},
    {I_TYPE, PARAM_NONE, 0b100110, 0, "LWR"},
    {I_TYPE, PARAM_NONE, 0b101010, 0, "SWL"},
    {I_TYPE, PARAM_NONE, 0b101110, 0, "SWR"},
    // atomics
    {I_TYPE, PARAM_NONE, 0b110000, 0, "LL"},
    {I_TYPE, PARAM_NONE, 0b111000, 0, "SC"},
    {I_TYPE, PARAM_NONE, 0b111100, 0, "SCD"},

    // branches
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000100, 0, "BEQ"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010100, 0, "BEQL"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000101, 0, "BNE"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010101, 0, "BNEL"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000111, 0, "BGTZ"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010111, 0, "BGTZL"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000110, 0, "BLEZ"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010110, 0, "BLEZL"},
    {R_TYPE, PARAM_NONE, 0, 0b001001, "JALR"},
    {R_TYPE, PARAM_NONE, 0, 0b001000, "JR"},
    {R_TYPE, PARAM_TRAP, 0, 0b110100, "TEQ"},
    {R_TYPE, PARAM_EMUX, 0, 0b110110, "TNE"},

    // jal (special)
    {J_TYPE, PARAM_JAL, 0b000011, 0, "JAL"},
    {J_TYPE, PARAM_JUMP, 0b000010, 0, "J"},

    // instructions involving doubles (deprioritized on the list)
    {R_TYPE, PARAM_NONE, 0, 0b101101, "DADDU"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b011000, 0, "DADDI"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b011001, 0, "DADDIU"},
    {R_TYPE, PARAM_NONE, 0, 0b101110, "DSUB"},
    {R_TYPE, PARAM_NONE, 0, 0b101111, "DSUBU"},
    {R_TYPE, PARAM_NONE, 0, 0b011101, "DMULTU"},
    {R_TYPE, PARAM_NONE, 0, 0b011110, "DDIV"},
    {R_TYPE, PARAM_NONE, 0, 0b011111, "DDIVU"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010100, "DSLLV"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111100, "DSLL32"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111110, "DSRL32"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010110, "DSRLV"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111111, "DSRA32"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010111, "DSRAV"},
};


char registerMaps[][4] = {
    "$R0",
    "$AT",
    "$V0", "$V1",
    "$A0", "$A1", "$A2", "$A3",
    "$T0", "$T1", "$T2", "$T3", "$T4", "$T5", "$T6", "$T7",
    "$S0", "$S1", "$S2", "$S3", "$S4", "$S5", "$S6", "$S7",
    "$T8", "$T9",
    "$K0", "$K1",
    "$GP", "$SP", "$FP", "$RA",
};

char registerMapFloat[][5] = {
    "$F0", "$F1", "$F2", "$F3",
    "$F4", "$F5", "$F6", "$F7",
    "$F8", "$F9", "$F10", "$F11",
    "$F12", "$F13", "$F14", "$F15",
    "$F16", "$F17", "$F18", "$F19",
    "$F20", "$F21", "$F22", "$F23",
    "$F24", "$F25", "$F26", "$F27",
    "$F28", "$F29", "$F30", "$F31",
};

char *c0_insn_disasm(UNUSED InsnData insn, UNUSED u32 isPC) {
    return "COP0 UNIMPL";
}

char *c1_insn_disasm(UNUSED InsnData insn, UNUSED u32 isPC) {
    // char *strp = &insn_as_string[0];
    // for (int i = 0; i < ARRAY_COUNT(insn_as_string); i++) insn_as_string[i] = 0;
    return "COP1 UNIMPL";
}

char *branch_insn_disasm(InsnData insn, u32 isPC) {
    static char *insn_names[] = {
        [0b00001] = "BGEZ",
        [0b00011] = "BGEZL",
        [0b10001] = "BGEZAL",
        [0b10011] = "BGEZALL",
        [0b00000] = "BLTZ",
        [0b00010] = "BLTZL",
        [0b10000] = "BLTZAL",
        [0b10010] = "BLTZALL",
    };
    char *strp = &insn_as_string[0];
    char *rs = registerMaps[insn.b.rs];
    u16 offset = insn.b.offset;

    for (int i = 0; i < ARRAY_COUNT(insn_as_string); i++) insn_as_string[i] = 0;

    sprintf(strp, "%-8s %s %04X %s", insn_names[insn.b.sub], rs, offset, isPC ? "<-- CRASH" : "");

    return insn_as_string;
}

char *insn_disasm(InsnData insn, u32 isPC) {
    char *strp = &insn_as_string[0];
    int successful_print = 0;
    u32 target;

    if (insn.d == 0) { // trivial case
        if (isPC) {
            return "nop <-- CRASH";
        } else {
            return "nop";
        }
    }

    if (insn.i.opcode == OP_BRANCH) {
        return branch_insn_disasm(insn, isPC);
    }
    if (insn.i.opcode == OP_COP0) {
        return c0_insn_disasm(insn, isPC);
    }
    if (insn.i.opcode == OP_COP1) {
        return c1_insn_disasm(insn, isPC);
    }

    for (int i = 0; i < ARRAY_COUNT(insn_as_string); i++) insn_as_string[i] = 0;

    for (int i = 0; i < ARRAY_COUNT(insn_db); i++) {
        if (insn.i.opcode != 0 && insn.i.opcode == insn_db[i].opcode) {
            switch (insn_db[i].arbitraryParam) {
                case PARAM_SWAP_RS_IMM:
                    strp += sprintf(strp, "%-8s %s %s  %04X", insn_db[i].name,
                                               registerMaps[insn.i.rt],
                                               registerMaps[insn.i.rs],
                                               insn.i.immediate
                    ); break;
                case PARAM_LUI:
                    strp += sprintf(strp, "%-8s %s %04X", insn_db[i].name,
                                               registerMaps[insn.i.rt],
                                               insn.i.immediate
                    ); break;
                    break;
                case PARAM_JAL:
                    target = 0x80000000 | ((insn.d & 0x1FFFFFF) * 4);
                    if ((u32)parse_map != MAP_PARSER_ADDRESS) {
                        strp += sprintf(strp, "%-8s %s", insn_db[i].name,
                                                         parse_map(target)
                        );
                    } else {
                        strp += sprintf(strp, "%-8s %08X", insn_db[i].name,
                                                           target
                        );
                    }
                    break;
                case PARAM_JUMP:
                    target = 0x80000000 | (insn.d & 0x0FFFFFFF);
                    strp += sprintf(strp, "%-8s %08X", insn_db[i].name,
                                                       target
                    );
                    break;
                case PARAM_FLOAT_RT:
                    strp += sprintf(strp, "%-8s %s, %04X (%s)", insn_db[i].name,
                                               registerMapFloat[insn.i.rt],
                                               insn.i.immediate,
                                               registerMaps[insn.i.rs]
                    ); break;
                case PARAM_NONE:
                    strp += sprintf(strp, "%-8s %s %04X (%s)", insn_db[i].name,
                                                   registerMaps[insn.i.rt],
                                                   insn.i.immediate,
                                                   registerMaps[insn.i.rs]
                    ); break;

            }
            successful_print = 1;
            break;
        } else if (insn.i.rdata.function != 0 && insn.i.rdata.function == insn_db[i].function) {
            switch (insn_db[i].arbitraryParam) {
                case PARAM_BITSHIFT:
                    strp += sprintf(strp, "%-8s %s %s %04X", insn_db[i].name,
                                                       registerMaps[insn.i.rdata.rd],
                                                       registerMaps[insn.i.rt],
                                                       insn.i.rdata.shift_amt
                        );
                    break;
                case PARAM_SWAP_RS_RT:
                    strp += sprintf(strp, "%-8s %s %s %s", insn_db[i].name,
                                                       registerMaps[insn.i.rdata.rd],
                                                       registerMaps[insn.i.rt],
                                                       registerMaps[insn.i.rs]
                        );
                    break;
                case PARAM_MULT_MOVE:
                    strp += sprintf(strp, "%-8s %s", insn_db[i].name,
                                                       registerMaps[insn.i.rdata.rd]
                        );
                    break;
                case PARAM_EMUX:
                    target = (insn.d >> 6) & 0x3FF;
                    if (insn.i.rs == insn.i.rt) {
                        strp += sprintf(strp, "emux %s 0x%02X",
                                                       registerMaps[insn.i.rs],
                                                       target
                        );
                    } else {
                        strp += sprintf(strp, "%-8s %s %s", insn_db[i].name,
                                                       registerMaps[insn.i.rs],
                                                       registerMaps[insn.i.rt]
                        );
                    }
                    break;
                case PARAM_TRAP:
                    strp += sprintf(strp, "%-8s %s %s", insn_db[i].name,
                                                       registerMaps[insn.i.rs],
                                                       registerMaps[insn.i.rt]
                    );
                    break;
                case PARAM_NONE:
                    strp += sprintf(strp, "%-8s %s %s %s", insn_db[i].name,
                                                       registerMaps[insn.i.rdata.rd],
                                                       registerMaps[insn.i.rs],
                                                       registerMaps[insn.i.rt]
                        );
                    break;

            }
            successful_print = 1;
            break;
        }
    }
    if (successful_print == 0) {
        strp += sprintf(strp, "unimpl %08X", insn.d);
    }

    if (isPC) {
        sprintf(strp, " <-- CRASH");
    }

    return insn_as_string;
}
