#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"

enum InsnTypes {
    R_TYPE,
    I_TYPE,
    J_TYPE,
};

enum ParamTypes {
    PARAM_NONE,
    PARAM_SWAP_RS_IMM,
    PARAM_JAL,
    PARAM_JR,
    PARAM_LUI,
};

extern far char *parse_map(u32 pc);
static char insn_as_string[100];

typedef struct __attribute__((packed)) {
    u16 rd        : 5;
    u16 shift_amt : 5;
    u16 function  : 6;
} RTypeData;

typedef struct __attribute__((packed)) {
    u16 opcode : 6;
    u16 rs     : 5;
    u16 rt     : 5;
    union {
        RTypeData rdata;
        u16 immediate;
    };
} Insn;

typedef union {
    Insn i;
    u32  d;
} InsnData;

typedef struct __attribute__((packed)) {
    u32 type;
    u32 arbitraryParam;
    u16 opcode   : 6;
    u16 function : 6;
    u8 name[10];
} InsnTemplate;


InsnTemplate insn_db[] = {
    {R_TYPE, PARAM_NONE, 0, 0b100000, "ADD"},
    {R_TYPE, PARAM_NONE, 0, 0b100001, "ADDU"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b001001, 0, "ADDIU"},
    {R_TYPE, PARAM_NONE, 0, 0b100100, "AND"},
    {R_TYPE, PARAM_NONE, 0, 0b011010, "DIV"},
    {R_TYPE, PARAM_NONE, 0, 0b011011, "DIVU"},
    {R_TYPE, PARAM_NONE, 0, 0b001000, "JR"},
 
    {I_TYPE, PARAM_NONE, 0b101000, 0, "SB"},
    {I_TYPE, PARAM_NONE, 0b100000, 0, "LB"},
    {I_TYPE, PARAM_NONE, 0b100100, 0, "LBU"},
    {I_TYPE, PARAM_NONE, 0b101001, 0, "SH"},
    {I_TYPE, PARAM_NONE, 0b100001, 0, "LH"},
    {I_TYPE, PARAM_NONE, 0b100101, 0, "LHU"},
    {I_TYPE, PARAM_NONE, 0b101011, 0, "SW"},
    {I_TYPE, PARAM_NONE, 0b100011, 0, "LW"},
    {I_TYPE, PARAM_LUI,  0b001111, 0, "LUI"},

    // branches
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000100, 0, "BEQ"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000101, 0, "BNE"},
    {R_TYPE, PARAM_NONE, 0, 0b110100, "TEQ"},
    {R_TYPE, PARAM_NONE, 0, 0b001001, "JALR"},

    // jal (special)
    {J_TYPE, PARAM_JAL, 0b000011, 0, "JAL"}
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

char *insn_disasm(InsnData insn, u32 isPC) {
    char *strp = &insn_as_string[0];
    int successful_print = 0;
    u32 target;

    if (insn.d == 0) { // trivial case
        if (isPC) {
            return "NOP <-- CRASH";
        } else {
            return "NOP";
        }
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
                strp += sprintf(strp, "%-8s %s %s %s", insn_db[i].name,
                                                   registerMaps[insn.i.rdata.rd],
                                                   registerMaps[insn.i.rs],
                                                   registerMaps[insn.i.rt]
                        );
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
