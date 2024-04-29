#pragma once


#include "insn_disasm.h"
#include "registers.h"


/**
 * How to find instructions:
 *
 * - If first 4 bits (cop_opcode) == 0100 (COP_OPCODE):
 *  - The next 2 bits (cop_num) are coprocessor number, so use insn_db_cop_lists
 *  - compare the next 2 bits (cop_subtype):
 *   - if 0:
 *    - compare the next 3 bits (fmt) to the list
 *   - otherwise, if 1:
 *    - skip the next 3 bits (fmt) and compare the 5 bits after that (ft) to the list
 *   - otherwise, if 2:
 *    - compare the last 6 bits (func) to the list
 *   - otherwise, if 3:
 *    - invalid/unknown instruction.
 * - otherwise, check first 6 bits (opcode)
 *  - if 0 (OPC_SPECIAL):
 *   - compare the last 6 bits (func) to insn_db_spec
 *  - otherwise, if 1 (OPC_REGIMM):
 *   - skip the next 5 bits and compare the 5 bits after that (regimm)
 *  - otherwise, compare the first 6 bits to insn_db_standard
 */


// MIPS III Instructions:
// https://n64brew.dev/wiki/MIPS_III_instructions
// https://hack64.net/docs/VR43XX.pdf

// Opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_standard[] = { // INSN_TYPE_OPCODE
    // OPC_SPECIAL (insn_db_spec) // 0: OPC_SPECIAL, INSN_TYPE_FUNC
    // OPC_REGIMM  (insn_db_regi) // 1: OPC_REGIMM,  INSN_TYPE_REGIMM
    INSNI(OPC_J,      "J      ",        IFMT_J,   0         ), //  2: Jump.
    INSNI(OPC_JAL,    "JAL    ",        IFMT_J,   0         ), //  3: Jump and Link.
    INSNI(OPC_BEQ,    "BEQ    ",        IFMT_stB, 0         ), //  4: Branch on Equal.
    INSNI(OPC_BNE,    "BNE    ",        IFMT_stB, 0         ), //  5: Branch on Not Equal.
    INSNI(OPC_BLEZ,   "BLEZ   ",        IFMT_sB,  0         ), //  6: Branch on Less Than or Equal to Zero.
    INSNI(OPC_BGTZ,   "BGTZ   ",        IFMT_sB,  0         ), //  7: Branch on Greater Than Zero.
    INSNI(OPC_ADDI,   "ADDI   ",        IFMT_tsI, 1         ), //  8: Add Immediate Word.
    INSNI(OPC_ADDIU,  "ADDIU  ",        IFMT_tsI, 1         ), //  9: Add Immediate Unsigned Word.
    INSNI(OPC_SLTI,   "SLTI   ",        IFMT_tsI, 1         ), // 10: Set on Less Than Immediate.
    INSNI(OPC_SLTIU,  "SLTIU  ",        IFMT_tsI, 1         ), // 11: Set on Less Than Immediate Unsigned.
    INSNI(OPC_ANDI,   "ANDI   ",        IFMT_tsI, 1         ), // 12: And Immediate.
    INSNI(OPC_ORI,    "ORI    ",        IFMT_tsI, 1         ), // 13: Or Immediate.
    INSNI(OPC_XORI,   "XORI   ",        IFMT_tsI, 1         ), // 14: Exclusive Or Immediate.
    INSNI(OPC_LUI,    "LUI    ",        IFMT_tI,  1         ), // 15: Load Upper Immediate.
    // OPC_COP0 (insn_db_cop0) // 16: Coprocessor-0 (System Control Coprocessor).
    // OPC_COP1 (insn_db_cop1) // 17: Coprocessor-1 (Floating-Point Unit).
    // OPC_COP2 (insn_db_cop2) // 18: Coprocessor-2 (Reality Co-Processor Vector Unit).
    // OPC_COP3 (insn_db_cop3) // 19: Coprocessor-3 (CP3).
    INSNI(OPC_BEQL,   "BEQL   ",        IFMT_stB, 0         ), // 20: Branch on Equal Likely.
    INSNI(OPC_BNEL,   "BNEL   ",        IFMT_stB, 0         ), // 21: Branch on Not Equal Likely.
    INSNI(OPC_BLEZL,  "BLEZL  ",        IFMT_sB,  0         ), // 22: Branch on Less Than or Equal to Zero Likely.
    INSNI(OPC_BGTZL,  "BGTZL  ",        IFMT_sB,  0         ), // 23: Branch on Greater Than Zero Likely.
    INSNI(OPC_DADDI,  "DADDI  ",        IFMT_tsI, 1         ), // 24: Doubleword Add Immediate.
    INSNI(OPC_DADDIU, "DADDIU ",        IFMT_tsI, 1         ), // 25: Doubleword Add Immediate Unsigned.
    INSNI(OPC_LDL,    "LDL    ",        IFMT_to,  1         ), // 26: Load Doubleword Left.
    INSNI(OPC_LDR,    "LDR    ",        IFMT_to,  1         ), // 27: Load Doubleword Right.
    INSNI(OPC_LB,     "LB     ",        IFMT_to,  1         ), // 32: Load Byte.
    INSNI(OPC_LH,     "LH     ",        IFMT_to,  1         ), // 33: Load Halfword.
    INSNI(OPC_LWL,    "LWL    ",        IFMT_to,  1         ), // 34: Load Word Left.
    INSNI(OPC_LW,     "LW     ",        IFMT_to,  1         ), // 35: Load Word.
    INSNI(OPC_LBU,    "LBU    ",        IFMT_to,  1         ), // 36: Load Byte Unsigned.
    INSNI(OPC_LHU,    "LHU    ",        IFMT_to,  1         ), // 37: Load Halfword Unsigned.
    INSNI(OPC_LWR,    "LWR    ",        IFMT_to,  1         ), // 38: Load Word Right.
    INSNI(OPC_LWU,    "LWU    ",        IFMT_to,  1         ), // 39: Load Word Unsigned.
    INSNI(OPC_SB,     "SB     ",        IFMT_to,  0         ), // 40: Store Byte.
    INSNI(OPC_SH,     "SH     ",        IFMT_to,  0         ), // 41: Store Halfword.
    INSNI(OPC_SWL,    "SWL    ",        IFMT_to,  0         ), // 42: Store Word Left.
    INSNI(OPC_SW,     "SW     ",        IFMT_to,  0         ), // 43: Store Word.
    INSNI(OPC_SDL,    "SDL    ",        IFMT_to,  0         ), // 44: Store Doubleword Left.
    INSNI(OPC_SDR,    "SDR    ",        IFMT_to,  0         ), // 45: Store Doubleword Right.
    INSNI(OPC_SWR,    "SWR    ",        IFMT_to,  0         ), // 46: Store Word Right.
    INSNI(OPC_CACHE,  "CACHE  ",        IFMT_to,  0         ), // 47: https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html.
    INSNI(OPC_LL,     "LL     ",        IFMT_to,  1         ), // 48: Load Linked Word.
    INSNF(OPC_LWC1,   "LWC1   ", FALSE, IFMT_To,  0, 0, 1, 1), // 49: Load Word to Coprocessor-1 (Floating-Point Unit).
    INSNI(OPC_LWC2,   "LWC2   ",        IFMT_to,  0         ), // 50: Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSNI(OPC_LWC3,   "LWC3   ",        IFMT_to,  0         ), // 51: Load Word to Coprocessor-3 (COP3).
    INSNI(OPC_LLD ,   "LLD    ",        IFMT_to,  1         ), // 52: Load Linked Doubleword.
    INSNF(OPC_LDC1,   "LDC1   ", FALSE, IFMT_To,  1, 0, 1, 1), // 53: Load Doubleword to Coprocessor-1 (Floating-Point Unit).
    INSNI(OPC_LDC2,   "LDC2   ",        IFMT_to,  1         ), // 54: Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSNI(OPC_LD,     "LD     ",        IFMT_to,  1         ), // 55: Load Doubleword.
    INSNI(OPC_SC,     "SC     ",        IFMT_to,  0         ), // 56: Store Conditional Word.
    INSNF(OPC_SWC1,   "SWC1   ", FALSE, IFMT_To,  0, 0, 1, 1), // 57: Store Word to Coprocessor-1 (Floating-Point Unit).
    INSNI(OPC_SWC2,   "SWC2   ",        IFMT_to,  0         ), // 58: Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSNI(OPC_SWC3,   "SWC3   ",        IFMT_to,  0         ), // 59: Store Word to Coprocessor-3 (COP3).
    INSNI(OPC_SCD,    "SCD    ",        IFMT_to,  0         ), // 60: Store Conditional Doubleword.
    INSNF(OPC_SDC1,   "SDC1   ", FALSE, IFMT_To,  0, 0, 1, 1), // 61: Store Doubleword to Coprocessor-1 (Floating-Point Unit).
    INSNI(OPC_SDC2,   "SDC2   ",        IFMT_to,  0         ), // 62: Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSNI(OPC_SD,     "SD     ",        IFMT_to,  0         ), // 63: Store Doubleword.
    INSN_END(), // NULL terminator.
};

// Special opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_spec[] = { // OPC_SPECIAL, INSN_TYPE_FUNC
    INSNI(OPS_SLL,     "SLL    ", IFMT_dta, 1), //  0: Shift Word Left Logical.
    INSNI(OPS_SRL,     "SRL    ", IFMT_dta, 1), //  2: Shift Word Right Logical.
    INSNI(OPS_SRA,     "SRA    ", IFMT_dta, 1), //  3: Shift Word Right Arithmetic.
    INSNI(OPS_SLLV,    "SLLV   ", IFMT_dts, 1), //  4: Shift Word Left Logical Variable.
    INSNI(OPS_SRLV,    "SRLV   ", IFMT_dts, 1), //  6: Shift Word Right Logical Variable.
    INSNI(OPS_SRAV,    "SRAV   ", IFMT_dts, 1), //  7: Shift Word Right Arithmetic Variable.
    INSNI(OPS_JR,      "JR     ", IFMT_s,   0), //  8: Jump Register.
    INSNI(OPS_JALR,    "JALR   ", IFMT_ds,  0), //  9: Jump and Link Register.
    INSNI(OPS_SYSCALL, "SYSCALL", IFMT_E,   0), // 12: System Call (assert).
    INSNI(OPS_BREAK,   "BREAK  ", IFMT_E,   0), // 13: Breakpoint.
    INSNI(OPS_SYNC,    "SYNC   ", IFMT_NOP, 0), // 15: Synchronize Shared Memory.
    INSNI(OPS_MFHI,    "MFHI   ", IFMT_d,   6), // 16: Move From HI.
    INSNI(OPS_MTHI,    "MTHI   ", IFMT_s,   6), // 17: Move To HI.
    INSNI(OPS_MFLO,    "MFLO   ", IFMT_d,   6), // 18: Move From LO.
    INSNI(OPS_MTLO,    "MTLO   ", IFMT_s,   6), // 19: Move To LO.
    INSNI(OPS_DSLLV,   "DSLLV  ", IFMT_dts, 1), // 20: Doubleword Shift Left Logical Variable.
    INSNI(OPS_DSRLV,   "DSRLV  ", IFMT_dts, 1), // 22: Doubleword Shift Right Logical Variable.
    INSNI(OPS_DSRAV,   "DSRAV  ", IFMT_dts, 1), // 23: Doubleword Shift Right Arithmetic Variable.
    INSNI(OPS_MULT,    "MULT   ", IFMT_st,  6), // 24: Multiply Word (5cyc).
    INSNI(OPS_MULTU,   "MULTU  ", IFMT_st,  6), // 25: Multiply Unsigned Word (5cyc).
    INSNI(OPS_DIV,     "DIV    ", IFMT_st,  6), // 26: Divide Word (37cyc).
    INSNI(OPS_DIVU,    "DIVU   ", IFMT_st,  6), // 27: Divide Unsigned Word (37cyc).
    INSNI(OPS_DMULT,   "DMULT  ", IFMT_st,  6), // 28: Doubleword Multiply (8cyc).
    INSNI(OPS_DMULTU,  "DMULTU ", IFMT_st,  6), // 29: Doubleword Multiply Unsigned (8cyc).
    INSNI(OPS_DDIV,    "DDIV   ", IFMT_st,  6), // 30: Doubleword Divide (69cyc).
    INSNI(OPS_DDIVU,   "DDIVU  ", IFMT_st,  6), // 31: Doubleword Divide Unsigned (69cyc).
    INSNI(OPS_ADD,     "ADD    ", IFMT_dst, 1), // 32: Add Word.
    INSNI(OPS_ADDU,    "ADDU   ", IFMT_dst, 1), // 33: Add Unsigned Word.
    INSNI(OPS_SUB,     "SUB    ", IFMT_dst, 1), // 34: Subtract Word.
    INSNI(OPS_SUBU,    "SUBU   ", IFMT_dst, 1), // 35: Subtract Unsigned Word.
    INSNI(OPS_AND,     "AND    ", IFMT_dst, 1), // 36: And.
    INSNI(OPS_OR,      "OR     ", IFMT_dst, 1), // 37: Or.
    INSNI(OPS_XOR,     "XOR    ", IFMT_dst, 1), // 38: Exclusive Or.
    INSNI(OPS_NOR,     "NOR    ", IFMT_dst, 1), // 39: Nor.
    INSNI(OPS_SLT,     "SLT    ", IFMT_dst, 1), // 42: Set on Less Than.
    INSNI(OPS_SLTU,    "SLTU   ", IFMT_dst, 1), // 43: Set on Less Than Unsigned.
    INSNI(OPS_DADD,    "DADD   ", IFMT_dst, 1), // 44: Doubleword Add.
    INSNI(OPS_DADDU,   "DADDU  ", IFMT_dst, 1), // 45: Doubleword Add Unsigned.
    INSNI(OPS_DSUB,    "DSUB   ", IFMT_dst, 1), // 46: Doubleword Subtract.
    INSNI(OPS_DSUBU,   "DSUBU  ", IFMT_dst, 1), // 47: Doubleword Subtract Unsigned.
    INSNI(OPS_TGE,     "TGE    ", IFMT_ste, 0), // 48: Trap if Greater Than or Equal.
    INSNI(OPS_TGEU,    "TGEU   ", IFMT_ste, 0), // 49: Trap if Greater Than or Equal Unsigned.
    INSNI(OPS_TLT,     "TLT    ", IFMT_ste, 0), // 50: Trap if Less Than.
    INSNI(OPS_TLTU,    "TLTU   ", IFMT_ste, 0), // 51: Trap if Less Than Unsigned.
    INSNI(OPS_TEQ,     "TEQ    ", IFMT_ste, 0), // 52: Trap if Equal.
    INSNI(OPS_TNE,     "TNE    ", IFMT_ste, 0), // 54: Trap if Not Equal.
    INSNI(OPS_DSLL,    "DSLL   ", IFMT_dta, 1), // 56: Doubleword Shift Left Logical.
    INSNI(OPS_DSRL,    "DSRL   ", IFMT_dta, 1), // 58: Doubleword Shift Right Logical.
    INSNI(OPS_DSRA,    "DSRA   ", IFMT_dta, 1), // 59: Doubleword Shift Right Arithmetic.
    INSNI(OPS_DSLL32,  "DSLL32 ", IFMT_dta, 1), // 60: Doubleword Shift Left Logical + 32.
    INSNI(OPS_DSRL32,  "DSRL32 ", IFMT_dta, 1), // 62: Doubleword Shift Right Logical + 32.
    INSNI(OPS_DSRA32,  "DSRA32 ", IFMT_dta, 1), // 63: Doubleword Shift Right Arithmetic + 32.
    INSN_END(), // NULL terminator.
};

// Register opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_regi[] = { // OPC_REGIMM, INSN_TYPE_REGIMM
    INSNI(OPR_BLTZ,    "BLTZ   ", IFMT_sB, 0), //  0: Branch on Less Than Zero.
    INSNI(OPR_BGEZ,    "BGEZ   ", IFMT_sB, 0), //  1: Branch on Greater Than or Equal to Zero.
    INSNI(OPR_BLTZL,   "BLTZL  ", IFMT_sB, 0), //  2: Branch on Less Than Zero Likely.
    INSNI(OPR_BGEZL,   "BGEZL  ", IFMT_sB, 0), //  3: Branch on Greater Than or Equal to Zero Likely.
    INSNI(OPR_BLTZAL,  "BLTZAL ", IFMT_sB, 7), // 16: Branch on Less Than Zero and Link.
    INSNI(OPR_BGEZAL,  "BGEZAL ", IFMT_sB, 7), // 17: Branch on Greater Than or Equal to Zero and Link.
    INSNI(OPR_BLTZALL, "BLTZALL", IFMT_sB, 7), // 18: Branch on Less Than Zero and Link Likely.
    INSNI(OPR_BGEZALL, "BGEZALL", IFMT_sB, 7), // 19: Branch on Greater Than or Equal to Zero and Link Likely.

    INSNI(OPR_TGEI,    "TGEI   ", IFMT_sI, 0), //  8: Trap if Greater Than or Equal Immediate.
    INSNI(OPR_TGEIU,   "TGEIU  ", IFMT_sI, 0), //  9: Trap if Greater Than or Equal Unsigned Immediate.
    INSNI(OPR_TLTI,    "TLTI   ", IFMT_sI, 0), // 10: Trap if Less Than Immediate.
    INSNI(OPR_TLTIU,   "TLTIU  ", IFMT_sI, 0), // 11: Trap if Less Than Unsigned Immediate.
    INSNI(OPR_TEQI,    "TEQI   ", IFMT_sI, 0), // 12: Trap if Equal Immediate.
    INSNI(OPR_TNEI,    "TNEI   ", IFMT_sI, 0), // 14: Trap if Not Equal Immediate.
    INSN_END(), // NULL terminator.
};

// Coprocessor-0 (System Control Coprocessor):
ALIGNED32 static const InsnTemplate insn_db_cop0_sub00[] = { // OPC_COP0, INSN_TYPE_COP_FMT
    INSNI(COP0_MF,  "MFC0   ", IFMT_t0, 1), //  0: Move from System Control Coprocessor.
    INSNI(COP0_DMF, "DMFC0  ", IFMT_t0, 1), //  1: Doubleword Move from System Control Coprocessor.
    INSNI(COP0_MT,  "MTC0   ", IFMT_t0, 2), //  4: Move to System Control Coprocessor.
    INSNI(COP0_DMT, "DMTC0  ", IFMT_t0, 2), //  5: Doubleword Move to System Control Coprocessor.
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop0_sub10[] = { // OPC_COP0, INSN_TYPE_FUNC
    //! TODO: These use the COP0 TLB Index register as the input/output.
    INSNI(OPC_COP0_TLBP,  "TLBP   ", IFMT_NOP, 0), //  8: Searches for a TLB entry that matches the EntryHi register.
    INSNI(OPC_COP0_TLBR,  "TLBR   ", IFMT_NOP, 0), //  1: Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
    INSNI(OPC_COP0_TLBWI, "TLBWI  ", IFMT_NOP, 0), //  2: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register.
    INSNI(OPC_COP0_TLBWR, "TLBWR  ", IFMT_NOP, 0), //  6: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register.
    INSNI(OPC_COP0_ERET,  "ERET   ", IFMT_NOP, 0), // 24: Return from interrupt, exception, or error exception.
    INSN_END(), // NULL terminator.
};

// Coprocessor-1 (Floating-Point Unit):
ALIGNED32 static const InsnTemplate insn_db_cop1_sub00[] = { // OPC_COP1, INSN_TYPE_COP_FMT
    INSNF(COP1_FMT_SINGLE, "MFC1   ", FALSE, IFMT_tS, 1, 1, 0, 1), //  0: Move Word From Floating-Point.
    INSNF(COP1_FMT_DOUBLE, "DMFC1  ", FALSE, IFMT_tS, 1, 1, 0, 1), //  1: Doubleword Move From Floating-Point.
    INSNF(COP1_FMT_WORD,   "MTC1   ", FALSE, IFMT_tS, 2, 0, 1, 2), //  4: Move Word To Floating-Point.
    INSNF(COP1_FMT_LONG,   "DMTC1  ", FALSE, IFMT_tS, 2, 0, 1, 2), //  5: Doubleword Move To Floating-Point.
    INSNI(COP1_FMT_CTL_F,  "CFC1   ",        IFMT_tS, 1         ), //  2: Move Control Word From Floating-Point.
    INSNI(COP1_FMT_CTL_T,  "CTC1   ",        IFMT_tS, 2         ), //  6: Move Control Word To Floating-Point.
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub01[] = { // OPC_COP1, INSN_TYPE_REGIMM
    INSNF(OPT_COP1_BC1F,  "BC1F   ", FALSE, IFMT_B, 0, 1, 1, 0), //  0: Branch on FP False (1cyc*).
    INSNF(OPT_COP1_BC1T,  "BC1T   ", FALSE, IFMT_B, 0, 1, 1, 0), //  1: Branch on FP True (1cyc*).
    INSNF(OPT_COP1_BC1FL, "BC1FL  ", FALSE, IFMT_B, 0, 1, 1, 0), //  2: Branch on FP False Likely (1cyc*).
    INSNF(OPT_COP1_BC1TL, "BC1TL  ", FALSE, IFMT_B, 0, 1, 1, 0), //  3: Branch on FP True Likely (1cyc*).
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub10[] = { // OPC_COP1, INSN_TYPE_FUNC
    INSNF(OPS_ADD_F,     "ADD    ", TRUE, IFMT_DST, 1, 1, 1, 0), //  0: ADD.[FMT]     Floating-Point Add (3cyc).
    INSNF(OPS_SUB_F,     "SUB    ", TRUE, IFMT_DST, 1, 1, 1, 0), //  1: SUB.[FMT]     Floating-Point Subtract (3cyc).
    INSNF(OPS_MUL_F,     "MUL    ", TRUE, IFMT_DST, 1, 1, 1, 0), //  2: MUL.[FMT]     Floating-Point Multiply (S:5cyc; D:8cyc).
    INSNF(OPS_DIV_F,     "DIV    ", TRUE, IFMT_DST, 1, 1, 1, 0), //  3: DIV.[FMT]     Floating-Point Divide (S:29cyc; D:58cyc).
    INSNF(OPS_SQRT_F,    "SQRT   ", TRUE, IFMT_DS,  1, 1, 1, 0), //  4: SQRT.[FMT]    Floating-Point Square Root (S:29cyc; D:58cyc).
    INSNF(OPS_ABS_F,     "ABS    ", TRUE, IFMT_DS,  1, 1, 1, 0), //  5: ABS.[FMT]     Floating-Point Absolute Value (1cyc).
    INSNF(OPS_MOV_F,     "MOV    ", TRUE, IFMT_DS,  1, 1, 1, 0), //  6: MOV.[FMT]     Floating-Point Move (1cyc).
    INSNF(OPS_NEG_F,     "NEG    ", TRUE, IFMT_DS,  1, 1, 1, 0), //  7: NEG.[FMT]     Floating-Point Negate (1cyc).
    INSNF(OPS_ROUND_L_F, "ROUND.L", TRUE, IFMT_DS,  1, 0, 1, 2), //  8: ROUND.L.[FMT] Floating-Point Round to Long Fixed-Point (5cyc).
    INSNF(OPS_TRUNC_L_F, "TRUNC.L", TRUE, IFMT_DS,  1, 0, 1, 2), //  9: TRUNC.L.[FMT] Floating-Point Truncate to Long Fixed-Point (5cyc).
    INSNF(OPS_CEIL_L_F,  "CEIL.L ", TRUE, IFMT_DS,  1, 0, 1, 2), // 10: CEIL.L.[FMT]  Floating-Point Ceiling to Long Fixed-Point (5cyc).
    INSNF(OPS_FLOOR_L_F, "FLOOR.L", TRUE, IFMT_DS,  1, 0, 1, 2), // 11: FLOOR.L.[FMT] Floating-Point Floor to Long Fixed-Point (5cyc).
    INSNF(OPS_ROUND_W_F, "ROUND.W", TRUE, IFMT_DS,  1, 0, 1, 2), // 12: ROUND.W.[FMT] Floating-Point Round to Word Fixed-Point (5cyc).
    INSNF(OPS_TRUNC_W_F, "TRUNC.W", TRUE, IFMT_DS,  1, 0, 1, 2), // 13: TRUNC.W.[FMT] Floating-Point Truncate to Word Fixed-Point (5cyc).
    INSNF(OPS_CEIL_W_F,  "CEIL.W ", TRUE, IFMT_DS,  1, 0, 1, 2), // 14: CEIL.W.[FMT]  Floating-Point Ceiling to Word Fixed-Point (5cyc).
    INSNF(OPS_FLOOR_W_F, "FLOOR.W", TRUE, IFMT_DS,  1, 0, 1, 2), // 15: FLOOR.W.[FMT] Floating-Point Floor to Word Fixed-Point (5cyc).

    INSNF(OPS_CVT_S_F,   "CVT.S  ", TRUE, IFMT_DS,  1, 1, 0, 2), // 32: CVT.S.[FMT]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc).
    INSNF(OPS_CVT_D_F,   "CVT.D  ", TRUE, IFMT_DS,  1, 1, 0, 2), // 33: CVT.D.[FMT]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc).
    INSNF(OPS_CVT_W_F,   "CVT.W  ", TRUE, IFMT_DS,  1, 0, 1, 2), // 36: CVT.W.[FMT]   Floating-Point Convert to Word Fixed-Point (5cyc).
    INSNF(OPS_CVT_L_F,   "CVT.L  ", TRUE, IFMT_DS,  1, 0, 1, 2), // 37: CVT.L.[FMT]   Floating-Point Convert to Long Fixed-Point (5cyc).

    INSNF(OPS_C_F,       "C.F    ", TRUE, IFMT_ST,  0, 1, 1, 0), // 48: C.F.[FMT]     Floating-Point Compare (False) (1cyc).
    INSNF(OPS_C_UN,      "C.UN   ", TRUE, IFMT_ST,  0, 1, 1, 0), // 49: C.UN.[FMT]    Floating-Point Compare (Unordered) (1cyc).
    INSNF(OPS_C_EQ,      "C.EQ   ", TRUE, IFMT_ST,  0, 1, 1, 0), // 50: C.EQ.[FMT]    Floating-point Compare (Equal) (1cyc).
    INSNF(OPS_C_UEQ,     "C.UEQ  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 51: C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc).
    INSNF(OPS_C_OLT,     "C.OLT  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 52: C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc).
    INSNF(OPS_C_ULT,     "C.ULT  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 53: C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc).
    INSNF(OPS_C_OLE,     "C.OLE  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 54: C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc).
    INSNF(OPS_C_ULE,     "C.ULE  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 55: C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc).
    INSNF(OPS_C_SF,      "C.SF   ", TRUE, IFMT_ST,  0, 1, 1, 0), // 56: C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc).
    INSNF(OPS_C_NGLE,    "C.NGLE ", TRUE, IFMT_ST,  0, 1, 1, 0), // 57: C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc).
    INSNF(OPS_C_SEQ,     "C.SEQ  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 58: C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc).
    INSNF(OPS_C_NGL,     "C.NGL  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 59: C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc).
    INSNF(OPS_C_LT,      "C.LT   ", TRUE, IFMT_ST,  0, 1, 1, 0), // 60: C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc).
    INSNF(OPS_C_NGE,     "C.NGE  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 61: C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc).
    INSNF(OPS_C_LE,      "C.LE   ", TRUE, IFMT_ST,  0, 1, 1, 0), // 62: C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc).
    INSNF(OPS_C_NGT,     "C.NGT  ", TRUE, IFMT_ST,  0, 1, 1, 0), // 63: C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc).
    INSN_END(), // NULL terminator.
};

// Coprocessor subtype lists.
static const InsnTemplate* insn_db_cop_lists[][0b11 + 1] = {
    [COP0] = { [INSN_TYPE_COP_FMT] = insn_db_cop0_sub00, [INSN_TYPE_REGIMM] = NULL,               [INSN_TYPE_FUNC] = insn_db_cop0_sub10, [INSN_TYPE_UNKNOWN] = NULL, }, // Coprocessor-0 (System Control Coprocessor).
    [COP1] = { [INSN_TYPE_COP_FMT] = insn_db_cop1_sub00, [INSN_TYPE_REGIMM] = insn_db_cop1_sub01, [INSN_TYPE_FUNC] = insn_db_cop1_sub10, [INSN_TYPE_UNKNOWN] = NULL, }, // Coprocessor-1 (Floating-Point Unit).
    [COP2] = { [INSN_TYPE_COP_FMT] = NULL,               [INSN_TYPE_REGIMM] = NULL,               [INSN_TYPE_FUNC] = NULL,               [INSN_TYPE_UNKNOWN] = NULL, }, // Coprocessor-2 (Reality Co-Processor Vector Unit).
    [COP3] = { [INSN_TYPE_COP_FMT] = NULL,               [INSN_TYPE_REGIMM] = NULL,               [INSN_TYPE_FUNC] = NULL,               [INSN_TYPE_UNKNOWN] = NULL, }, // Coprocessor-3 (CP3).
};

// Single-line pseudo-instructions.
ALIGNED32 static const InsnTemplate insn_db_pseudo[] = {
    [PSEUDO_NOP  ] = INSNI(OPS_SLL,   "NOP    ", IFMT_NOP, 0), // NOP (pseudo of SLL).
    [PSEUDO_MOVET] = INSNI(OPS_ADD,   "MOVE   ", IFMT_dt,  1), // Move (pseudo of ADD and OR).
    [PSEUDO_MOVES] = INSNI(OPS_ADD,   "MOVE   ", IFMT_ds,  1), // Move (pseudo of ADD).
    [PSEUDO_B    ] = INSNI(OPC_BEQ,   "B      ", IFMT_B,   0), // Branch (pseudo of BEQ).
    [PSEUDO_BEQZ ] = INSNI(OPC_BEQ,   "BEQZ   ", IFMT_sB,  0), // Branch on Equal to Zero (pseudo of BEQ).
    [PSEUDO_BNEZ ] = INSNI(OPC_BNE,   "BNEZ   ", IFMT_sB,  0), // Branch on Not Equal to Zero (pseudo of BNE).
    [PSEUDO_LI   ] = INSNI(OPC_ADDI,  "LI     ", IFMT_tI,  1), // Load Immediate (pseudo of ADDI and ADDIU).
    [PSEUDO_SUBI ] = INSNI(OPC_ADDI,  "SUBI   ", IFMT_tsi, 1), // Subtract Immediate Word (pseudo of ADDI).
    [PSEUDO_BEQZL] = INSNI(OPC_BEQL,  "BEQZL  ", IFMT_sB,  0), // Branch on Equal to Zero Likely (pseudo of BEQL).
    [PSEUDO_BNEZL] = INSNI(OPC_BNEL,  "BNEZL  ", IFMT_sB,  0), // Branch on Not Equal to Zero Likely (pseudo of BNEL).
    [PSEUDO_DSUBI] = INSNI(OPC_DADDI, "DSUBI  ", IFMT_tsi, 1), // Doubleword Subtract Immediate (pseudo of DADDI).
};
