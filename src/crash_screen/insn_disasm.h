#pragma once

#include <ultra64.h>

#include "types.h"

//! TODO:
// Toggle showing function names (like stack trace)
// Show addresses for each row?
// Individual address/instruction select
// Jump to address through branch/jal/etc.
// Pseudo-instructions: LA, LI, MOVE, BEQZ, BNEZ, BEQZL, BNEZL, etc.
// Verify insn_db ordering

enum ParamTypes {
    PARAM_N,
    PARAM_SYS,
    PARAM_SYN,
    PARAM_S,   // rs
    PARAM_T,   // rt
    PARAM_D,   // rd
    PARAM_ST,  // rs, rt
    PARAM_ST2, // rs, rt
    PARAM_DS,  // rd, rs
    PARAM_TD,  // rt, rd
    PARAM_SD,  // rs, rd
    PARAM_STD, // rs, rd, rt
    PARAM_DST, // rd, rs, rt
    PARAM_DTS, // rd, rt, rs
    PARAM_DTA, // rd, rt, shift
    PARAM_SI,  // rs, 0xI
    PARAM_TI,  // rt, 0xI
    PARAM_STI, // rs, rt, 0xI
    PARAM_TSI, // rt, rs, 0xI
    PARAM_TIS, // rt, 0xI(rs)
    PARAM_SO,  // rs, offset/func
    PARAM_STO, // rs, rt, offset
    PARAM_B,   // offset
    PARAM_J,   // func
    PARAM_FIS, // ft, 0xI(rs)
    PARAM_TFS, // rt, fs
    PARAM_FF,  // fd, fs
    PARAM_FFF, // fd, fs, ft
    PARAM_CON, // fs, ft
    PARAM_BC1, // offset
    PARAM_UNK, // unimpl
};

typedef struct __attribute__((packed)) {
    /*0x00*/ u16 rd        : 5; // fs
    /*0x00*/ u16 sa        : 5; // fd
    /*0x01*/ u16 function  : 6;
} RTypeData; /*0x02*/

typedef struct __attribute__((packed)) {
    /*0x00*/ u16 opcode : 6;
    /*0x00*/ u16 rs     : 5; // fr
    /*0x01*/ u16 rt     : 5; // ft
    /*0x02*/ union {
                 RTypeData rdata;
                 u16 immediate;
             };
} Insn; /*0x04*/

typedef union {
    Insn i;
    u32  d;
} InsnData; /*0x04*/

typedef struct __attribute__((packed)) {
    /*0x00*/ InsnData i;
    /*0x04*/ u16 paramType;
    /*0x06*/ char name[10];
} InsnTemplate; /*0x10*/


#define INSN_OFFSET(addr, offset) ((addr) + (sizeof(InsnData) * (s16)(offset)))


s32 is_branch(InsnData insn);
char *insn_disasm(InsnData insn, u32 isPC);
