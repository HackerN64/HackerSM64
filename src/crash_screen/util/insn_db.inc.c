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
    INSN_DB(OPC_J,      "J      ", FALSE, IFMT_J,   0, PSI_J    ), //  2: Jump.
    INSN_DB(OPC_JAL,    "JAL    ", FALSE, IFMT_J,   0, PSI_JAL  ), //  3: Jump and Link.
    INSN_DB(OPC_BEQ,    "BEQ    ", FALSE, IFMT_stB, 0, PSI_BEQ  ), //  4: Branch on Equal.
    INSN_DB(OPC_BNE,    "BNE    ", FALSE, IFMT_stB, 0, PSI_BNE  ), //  5: Branch on Not Equal.
    INSN_DB(OPC_BLEZ,   "BLEZ   ", FALSE, IFMT_sB,  0, PSI_BLEZ ), //  6: Branch on Less Than or Equal to Zero.
    INSN_DB(OPC_BGTZ,   "BGTZ   ", FALSE, IFMT_sB,  0, PSI_BGTZ ), //  7: Branch on Greater Than Zero.
    INSN_DB(OPC_ADDI,   "ADDI   ", FALSE, IFMT_tsI, 1, PSI_ADDI ), //  8: Add Immediate Word.
    INSN_DB(OPC_ADDIU,  "ADDIU  ", FALSE, IFMT_tsI, 1, PSI_ADDI ), //  9: Add Immediate Unsigned Word.
    INSN_DB(OPC_SLTI,   "SLTI   ", FALSE, IFMT_tsI, 1, PSI_SLTI ), // 10: Set on Less Than Immediate.
    INSN_DB(OPC_SLTIU,  "SLTIU  ", FALSE, IFMT_tsI, 1, PSI_SLTI ), // 11: Set on Less Than Immediate Unsigned.
    INSN_DB(OPC_ANDI,   "ANDI   ", FALSE, IFMT_tsI, 1, PSI_ANDI ), // 12: And Immediate.
    INSN_DB(OPC_ORI,    "ORI    ", FALSE, IFMT_tsI, 1, PSI_ORI  ), // 13: Or Immediate.
    INSN_DB(OPC_XORI,   "XORI   ", FALSE, IFMT_tsI, 1, PSI_XORI ), // 14: Exclusive Or Immediate.
    INSN_DB(OPC_LUI,    "LUI    ", FALSE, IFMT_tI,  1, PSI_LUI  ), // 15: Load Upper Immediate.
    // OPC_COP0 (insn_db_cop0) // 16: Coprocessor-0 (System Control Coprocessor).
    // OPC_COP1 (insn_db_cop1) // 17: Coprocessor-1 (Floating-Point Unit).
    // OPC_COP2 (insn_db_cop2) // 18: Coprocessor-2 (Reality Co-Processor Vector Unit).
    // OPC_COP3 (insn_db_cop3) // 19: Coprocessor-3 (CP3).
    INSN_DB(OPC_BEQL,   "BEQL   ", FALSE, IFMT_stB, 0, PSI_BEQ  ), // 20: Branch on Equal Likely.
    INSN_DB(OPC_BNEL,   "BNEL   ", FALSE, IFMT_stB, 0, PSI_BNE  ), // 21: Branch on Not Equal Likely.
    INSN_DB(OPC_BLEZL,  "BLEZL  ", FALSE, IFMT_sB,  0, PSI_BLEZ ), // 22: Branch on Less Than or Equal to Zero Likely.
    INSN_DB(OPC_BGTZL,  "BGTZL  ", FALSE, IFMT_sB,  0, PSI_BGTZ ), // 23: Branch on Greater Than Zero Likely.
    INSN_DB(OPC_DADDI,  "DADDI  ", FALSE, IFMT_tsI, 1, PSI_ADDI ), // 24: Doubleword Add Immediate.
    INSN_DB(OPC_DADDIU, "DADDIU ", FALSE, IFMT_tsI, 1, PSI_ADDI ), // 25: Doubleword Add Immediate Unsigned.
    INSN_DB(OPC_LDL,    "LDL    ", FALSE, IFMT_to,  1, PSI_L_L  ), // 26: Load Doubleword Left.
    INSN_DB(OPC_LDR,    "LDR    ", FALSE, IFMT_to,  1, PSI_L_R  ), // 27: Load Doubleword Right.
    INSN_DB(OPC_LB,     "LB     ", FALSE, IFMT_to,  1, PSI_L    ), // 32: Load Byte.
    INSN_DB(OPC_LH,     "LH     ", FALSE, IFMT_to,  1, PSI_L    ), // 33: Load Halfword.
    INSN_DB(OPC_LWL,    "LWL    ", FALSE, IFMT_to,  1, PSI_L_L  ), // 34: Load Word Left.
    INSN_DB(OPC_LW,     "LW     ", FALSE, IFMT_to,  1, PSI_L    ), // 35: Load Word.
    INSN_DB(OPC_LBU,    "LBU    ", FALSE, IFMT_to,  1, PSI_L    ), // 36: Load Byte Unsigned.
    INSN_DB(OPC_LHU,    "LHU    ", FALSE, IFMT_to,  1, PSI_L    ), // 37: Load Halfword Unsigned.
    INSN_DB(OPC_LWR,    "LWR    ", FALSE, IFMT_to,  1, PSI_L_R  ), // 38: Load Word Right.
    INSN_DB(OPC_LWU,    "LWU    ", FALSE, IFMT_to,  1, PSI_L    ), // 39: Load Word Unsigned.
    INSN_DB(OPC_SB,     "SB     ", FALSE, IFMT_to,  0, PSI_S    ), // 40: Store Byte.
    INSN_DB(OPC_SH,     "SH     ", FALSE, IFMT_to,  0, PSI_S    ), // 41: Store Halfword.
    INSN_DB(OPC_SWL,    "SWL    ", FALSE, IFMT_to,  0, PSI_S_L  ), // 42: Store Word Left.
    INSN_DB(OPC_SW,     "SW     ", FALSE, IFMT_to,  0, PSI_S    ), // 43: Store Word.
    INSN_DB(OPC_SDL,    "SDL    ", FALSE, IFMT_to,  0, PSI_S_L  ), // 44: Store Doubleword Left.
    INSN_DB(OPC_SDR,    "SDR    ", FALSE, IFMT_to,  0, PSI_S_R  ), // 45: Store Doubleword Right.
    INSN_DB(OPC_SWR,    "SWR    ", FALSE, IFMT_to,  0, PSI_S_R  ), // 46: Store Word Right.
    INSN_DB(OPC_CACHE,  "CACHE  ", FALSE, IFMT_to,  0, PSI_CACHE), // 47: https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html.
    INSN_EX(OPC_LL,     "LL     ", FALSE, IFMT_to,  1, PSI_LL,    EXR_LL, EXR_00, 0b10), // 48: Load Linked Word.
    INSN_DB(OPC_LWC1,   "LWC1   ", FALSE, IFMT_To,  1, PSI_LC1  ), // 49: Load Word to Coprocessor-1 (Floating-Point Unit).
    INSN_DB(OPC_LWC2,   "LWC2   ", FALSE, IFMT_to,  1, PSI_L    ), // 50: Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSN_DB(OPC_LWC3,   "LWC3   ", FALSE, IFMT_to,  1, PSI_L    ), // 51: Load Word to Coprocessor-3 (COP3).
    INSN_EX(OPC_LLD ,   "LLD    ", FALSE, IFMT_to,  1, PSI_LL,    EXR_LL, EXR_00, 0b10), // 52: Load Linked Doubleword.
    INSN_DB(OPC_LDC1,   "LDC1   ", FALSE, IFMT_To,  1, PSI_LC1  ), // 53: Load Doubleword to Coprocessor-1 (Floating-Point Unit).
    INSN_DB(OPC_LDC2,   "LDC2   ", FALSE, IFMT_to,  1, PSI_L    ), // 54: Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSN_DB(OPC_LD,     "LD     ", FALSE, IFMT_to,  1, PSI_L    ), // 55: Load Doubleword.
    INSN_DB(OPC_SC,     "SC     ", FALSE, IFMT_to,  0, PSI_SC   ), // 56: Store Conditional Word.
    INSN_DB(OPC_SWC1,   "SWC1   ", FALSE, IFMT_To,  0, PSI_SC1  ), // 57: Store Word to Coprocessor-1 (Floating-Point Unit).
    INSN_DB(OPC_SWC2,   "SWC2   ", FALSE, IFMT_to,  0, PSI_S    ), // 58: Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSN_DB(OPC_SWC3,   "SWC3   ", FALSE, IFMT_to,  0, PSI_S    ), // 59: Store Word to Coprocessor-3 (COP3).
    INSN_EX(OPC_SCD,    "SCD    ", FALSE, IFMT_to,  0, PSI_SC,    EXR_LL, EXR_00, 0b00), // 60: Store Conditional Doubleword.
    INSN_DB(OPC_SDC1,   "SDC1   ", FALSE, IFMT_To,  0, PSI_SC1  ), // 61: Store Doubleword to Coprocessor-1 (Floating-Point Unit).
    INSN_DB(OPC_SDC2,   "SDC2   ", FALSE, IFMT_to,  0, PSI_S    ), // 62: Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    INSN_EX(OPC_SD,     "SD     ", FALSE, IFMT_to,  0, PSI_S,     EXR_LL, EXR_00, 0b00), // 63: Store Doubleword.
    INSN_END(), // NULL terminator.
};

// Special opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_spec[] = { // OPC_SPECIAL, INSN_TYPE_FUNC
    INSN_DB(OPS_SLL,     "SLL    ", FALSE, IFMT_dta, 1, PSI_SLI    ), //  0: Shift Word Left Logical.
    INSN_DB(OPS_SRL,     "SRL    ", FALSE, IFMT_dta, 1, PSI_SRI    ), //  2: Shift Word Right Logical.
    INSN_DB(OPS_SRA,     "SRA    ", FALSE, IFMT_dta, 1, PSI_SRI    ), //  3: Shift Word Right Arithmetic.
    INSN_DB(OPS_SLLV,    "SLLV   ", FALSE, IFMT_dts, 1, PSI_SLV    ), //  4: Shift Word Left Logical Variable.
    INSN_DB(OPS_SRLV,    "SRLV   ", FALSE, IFMT_dts, 1, PSI_SRV    ), //  6: Shift Word Right Logical Variable.
    INSN_DB(OPS_SRAV,    "SRAV   ", FALSE, IFMT_dts, 1, PSI_SRV    ), //  7: Shift Word Right Arithmetic Variable.
    INSN_DB(OPS_JR,      "JR     ", FALSE, IFMT_s,   0, PSI_JR     ), //  8: Jump Register.
    INSN_DB(OPS_JALR,    "JALR   ", FALSE, IFMT_ds,  1, PSI_JALR   ), //  9: Jump and Link Register.
    INSN_DB(OPS_SYSCALL, "SYSCALL", FALSE, IFMT_E,   0, PSI_BREAK  ), // 12: System Call (assert).
    INSN_DB(OPS_BREAK,   "BREAK  ", FALSE, IFMT_E,   0, PSI_BREAK  ), // 13: Breakpoint.
    INSN_DB(OPS_SYNC,    "SYNC   ", FALSE, IFMT_NOP, 0, PSI_FUNC   ), // 15: Synchronize Shared Memory.
    INSN_EX(OPS_MFHI,    "MFHI   ", FALSE, IFMT_d,   0, PSI_MFHI,    EXR_HI, EXR_00, 0b00), // 16: Move From HI.
    INSN_EX(OPS_MTHI,    "MTHI   ", FALSE, IFMT_s,   0, PSI_MTHI,    EXR_HI, EXR_00, 0b11), // 17: Move To HI.
    INSN_EX(OPS_MFLO,    "MFLO   ", FALSE, IFMT_d,   0, PSI_MFLO,    EXR_LO, EXR_00, 0b00), // 18: Move From LO.
    INSN_EX(OPS_MTLO,    "MTLO   ", FALSE, IFMT_s,   0, PSI_MTLO,    EXR_LO, EXR_00, 0b11), // 19: Move To LO.
    INSN_DB(OPS_DSLLV,   "DSLLV  ", FALSE, IFMT_dts, 1, PSI_SLV    ), // 20: Doubleword Shift Left Logical Variable.
    INSN_DB(OPS_DSRLV,   "DSRLV  ", FALSE, IFMT_dts, 1, PSI_SRV    ), // 22: Doubleword Shift Right Logical Variable.
    INSN_DB(OPS_DSRAV,   "DSRAV  ", FALSE, IFMT_dts, 1, PSI_SRV    ), // 23: Doubleword Shift Right Arithmetic Variable.
    INSN_EX(OPS_MULT,    "MULT   ", FALSE, IFMT_st,  0, PSI_MULT,    EXR_HI, EXR_LO, 0b11), // 24: Multiply Word (5cyc).
    INSN_EX(OPS_MULTU,   "MULTU  ", FALSE, IFMT_st,  0, PSI_MULT,    EXR_HI, EXR_LO, 0b11), // 25: Multiply Unsigned Word (5cyc).
    INSN_EX(OPS_DIV,     "DIV    ", FALSE, IFMT_st,  0, PSI_DIV,     EXR_HI, EXR_LO, 0b11), // 26: Divide Word (37cyc).
    INSN_EX(OPS_DIVU,    "DIVU   ", FALSE, IFMT_st,  0, PSI_DIV,     EXR_HI, EXR_LO, 0b11), // 27: Divide Unsigned Word (37cyc).
    INSN_EX(OPS_DMULT,   "DMULT  ", FALSE, IFMT_st,  0, PSI_MULT,    EXR_HI, EXR_LO, 0b11), // 28: Doubleword Multiply (8cyc).
    INSN_EX(OPS_DMULTU,  "DMULTU ", FALSE, IFMT_st,  0, PSI_MULT,    EXR_HI, EXR_LO, 0b11), // 29: Doubleword Multiply Unsigned (8cyc).
    INSN_EX(OPS_DDIV,    "DDIV   ", FALSE, IFMT_st,  0, PSI_DIV,     EXR_HI, EXR_LO, 0b11), // 30: Doubleword Divide (69cyc).
    INSN_EX(OPS_DDIVU,   "DDIVU  ", FALSE, IFMT_st,  0, PSI_DIV,     EXR_HI, EXR_LO, 0b11), // 31: Doubleword Divide Unsigned (69cyc).
    INSN_DB(OPS_ADD,     "ADD    ", FALSE, IFMT_dst, 1, PSI_ADD    ), // 32: Add Word.
    INSN_DB(OPS_ADDU,    "ADDU   ", FALSE, IFMT_dst, 1, PSI_ADD    ), // 33: Add Unsigned Word.
    INSN_DB(OPS_SUB,     "SUB    ", FALSE, IFMT_dst, 1, PSI_SUB    ), // 34: Subtract Word.
    INSN_DB(OPS_SUBU,    "SUBU   ", FALSE, IFMT_dst, 1, PSI_SUB    ), // 35: Subtract Unsigned Word.
    INSN_DB(OPS_AND,     "AND    ", FALSE, IFMT_dst, 1, PSI_AND    ), // 36: And.
    INSN_DB(OPS_OR,      "OR     ", FALSE, IFMT_dst, 1, PSI_OR     ), // 37: Or.
    INSN_DB(OPS_XOR,     "XOR    ", FALSE, IFMT_dst, 1, PSI_XOR    ), // 38: Exclusive Or.
    INSN_DB(OPS_NOR,     "NOR    ", FALSE, IFMT_dst, 1, PSI_NOR    ), // 39: Nor.
    INSN_DB(OPS_SLT,     "SLT    ", FALSE, IFMT_dst, 1, PSI_SLT    ), // 42: Set on Less Than.
    INSN_DB(OPS_SLTU,    "SLTU   ", FALSE, IFMT_dst, 1, PSI_SLT    ), // 43: Set on Less Than Unsigned.
    INSN_DB(OPS_DADD,    "DADD   ", FALSE, IFMT_dst, 1, PSI_ADD    ), // 44: Doubleword Add.
    INSN_DB(OPS_DADDU,   "DADDU  ", FALSE, IFMT_dst, 1, PSI_ADD    ), // 45: Doubleword Add Unsigned.
    INSN_DB(OPS_DSUB,    "DSUB   ", FALSE, IFMT_dst, 1, PSI_SUB    ), // 46: Doubleword Subtract.
    INSN_DB(OPS_DSUBU,   "DSUBU  ", FALSE, IFMT_dst, 1, PSI_SUB    ), // 47: Doubleword Subtract Unsigned.
    INSN_DB(OPS_TGE,     "TGE    ", FALSE, IFMT_ste, 0, PSI_TGE    ), // 48: Trap if Greater Than or Equal.
    INSN_DB(OPS_TGEU,    "TGEU   ", FALSE, IFMT_ste, 0, PSI_TGE    ), // 49: Trap if Greater Than or Equal Unsigned.
    INSN_DB(OPS_TLT,     "TLT    ", FALSE, IFMT_ste, 0, PSI_TLT    ), // 50: Trap if Less Than.
    INSN_DB(OPS_TLTU,    "TLTU   ", FALSE, IFMT_ste, 0, PSI_TLT    ), // 51: Trap if Less Than Unsigned.
    INSN_DB(OPS_TEQ,     "TEQ    ", FALSE, IFMT_ste, 0, PSI_TEQ    ), // 52: Trap if Equal.
    INSN_DB(OPS_TNE,     "TNE    ", FALSE, IFMT_ste, 0, PSI_TEQ    ), // 54: Trap if Not Equal.
    INSN_DB(OPS_DSLL,    "DSLL   ", FALSE, IFMT_dta, 1, PSI_SLI    ), // 56: Doubleword Shift Left Logical.
    INSN_DB(OPS_DSRL,    "DSRL   ", FALSE, IFMT_dta, 1, PSI_SRI    ), // 58: Doubleword Shift Right Logical.
    INSN_DB(OPS_DSRA,    "DSRA   ", FALSE, IFMT_dta, 1, PSI_SRI    ), // 59: Doubleword Shift Right Arithmetic.
    INSN_DB(OPS_DSLL32,  "DSLL32 ", FALSE, IFMT_dta, 1, PSI_DSLI32 ), // 60: Doubleword Shift Left Logical + 32.
    INSN_DB(OPS_DSRL32,  "DSRL32 ", FALSE, IFMT_dta, 1, PSI_DSRI32 ), // 62: Doubleword Shift Right Logical + 32.
    INSN_DB(OPS_DSRA32,  "DSRA32 ", FALSE, IFMT_dta, 1, PSI_DSRI32 ), // 63: Doubleword Shift Right Arithmetic + 32.
    INSN_END(), // NULL terminator.
};

// Register opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_regi[] = { // OPC_REGIMM, INSN_TYPE_REGIMM
    INSN_DB(OPR_BLTZ,    "BLTZ   ", FALSE, IFMT_sB, 0, PSI_BLTZ ), //  0: Branch on Less Than Zero.
    INSN_DB(OPR_BGEZ,    "BGEZ   ", FALSE, IFMT_sB, 0, PSI_BGEZ ), //  1: Branch on Greater Than or Equal to Zero.
    INSN_DB(OPR_BLTZL,   "BLTZL  ", FALSE, IFMT_sB, 0, PSI_BLTZ ), //  2: Branch on Less Than Zero Likely.
    INSN_DB(OPR_BGEZL,   "BGEZL  ", FALSE, IFMT_sB, 0, PSI_BGEZ ), //  3: Branch on Greater Than or Equal to Zero Likely.
    INSN_EX(OPR_BLTZAL,  "BLTZAL ", FALSE, IFMT_sB, 0, PSI_BLTZAL, EXR_LL, EXR_00, 0b00), // 16: Branch on Less Than Zero and Link.
    INSN_EX(OPR_BGEZAL,  "BGEZAL ", FALSE, IFMT_sB, 0, PSI_BGEZAL, EXR_LL, EXR_00, 0b00), // 17: Branch on Greater Than or Equal to Zero and Link.
    INSN_EX(OPR_BLTZALL, "BLTZALL", FALSE, IFMT_sB, 0, PSI_BLTZAL, EXR_LL, EXR_00, 0b00), // 18: Branch on Less Than Zero and Link Likely.
    INSN_EX(OPR_BGEZALL, "BGEZALL", FALSE, IFMT_sB, 0, PSI_BGEZAL, EXR_LL, EXR_00, 0b00), // 19: Branch on Greater Than or Equal to Zero and Link Likely.

    INSN_DB(OPR_TGEI,    "TGEI   ", FALSE, IFMT_sI, 0, PSI_TGEI), //  8: Trap if Greater Than or Equal Immediate.
    INSN_DB(OPR_TGEIU,   "TGEIU  ", FALSE, IFMT_sI, 0, PSI_TGEI), //  9: Trap if Greater Than or Equal Unsigned Immediate.
    INSN_DB(OPR_TLTI,    "TLTI   ", FALSE, IFMT_sI, 0, PSI_TLTI), // 10: Trap if Less Than Immediate.
    INSN_DB(OPR_TLTIU,   "TLTIU  ", FALSE, IFMT_sI, 0, PSI_TLTI), // 11: Trap if Less Than Unsigned Immediate.
    INSN_DB(OPR_TEQI,    "TEQI   ", FALSE, IFMT_sI, 0, PSI_TEQI), // 12: Trap if Equal Immediate.
    INSN_DB(OPR_TNEI,    "TNEI   ", FALSE, IFMT_sI, 0, PSI_TNEI), // 14: Trap if Not Equal Immediate.
    INSN_END(), // NULL terminator.
};

// Coprocessor-0 (System Control Coprocessor):
ALIGNED32 static const InsnTemplate insn_db_cop0_sub00[] = { // OPC_COP0, INSN_TYPE_COP_FMT
    INSN_DB(COP0_MF,  "MFC0   ", FALSE, IFMT_tdCP0, 1, PSI_MFC0), //  0: Move from System Control Coprocessor.
    INSN_DB(COP0_DMF, "DMFC0  ", FALSE, IFMT_tdCP0, 1, PSI_MFC0), //  1: Doubleword Move from System Control Coprocessor.
    INSN_DB(COP0_MT,  "MTC0   ", FALSE, IFMT_tdCP0, 2, PSI_MTC0), //  4: Move to System Control Coprocessor.
    INSN_DB(COP0_DMT, "DMTC0  ", FALSE, IFMT_tdCP0, 2, PSI_MTC0), //  5: Doubleword Move to System Control Coprocessor.
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop0_sub10[] = { // OPC_COP0, INSN_TYPE_FUNC
    INSN_EX(OPC_COP0_TLBP,  "TLBP   ", FALSE, IFMT_NOP, 0, PSI_FUNC, EXR_EN, EXR_00, 0b00), //  8: Searches for a TLB entry that matches the EntryHi register.
    INSN_EX(OPC_COP0_TLBR,  "TLBR   ", FALSE, IFMT_NOP, 0, PSI_FUNC, EXR_EN, EXR_IX, 0b10), //  1: Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
    INSN_EX(OPC_COP0_TLBWI, "TLBWI  ", FALSE, IFMT_NOP, 0, PSI_FUNC, EXR_EN, EXR_IX, 0b00), //  2: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register.
    INSN_EX(OPC_COP0_TLBWR, "TLBWR  ", FALSE, IFMT_NOP, 0, PSI_FUNC, EXR_EN, EXR_RD, 0b00), //  6: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register.
    INSN_DB(OPC_COP0_ERET,  "ERET   ", FALSE, IFMT_NOP, 0, PSI_FUNC), // 24: Return from interrupt, exception, or error exception.
    INSN_END(), // NULL terminator.
};

// Coprocessor-1 (Floating-Point Unit):
ALIGNED32 static const InsnTemplate insn_db_cop1_sub00[] = { // OPC_COP1, INSN_TYPE_COP_FMT
    INSN_DB(COP1_FMT_SINGLE, "MFC1   ", FALSE, IFMT_tSCP1, 1, PSI_MFC1), //  0: Move Word From Floating-Point.
    INSN_DB(COP1_FMT_DOUBLE, "DMFC1  ", FALSE, IFMT_tSCP1, 1, PSI_MFC1), //  1: Doubleword Move From Floating-Point.
    INSN_DB(COP1_FMT_WORD,   "MTC1   ", FALSE, IFMT_tSCP1, 2, PSI_MTC1), //  4: Move Word To Floating-Point.
    INSN_DB(COP1_FMT_LONG,   "DMTC1  ", FALSE, IFMT_tSCP1, 2, PSI_MTC1), //  5: Doubleword Move To Floating-Point.
    INSN_DB(COP1_FMT_CTL_F,  "CFC1   ", FALSE, IFMT_tSFCR, 1, PSI_CFC1), //  2: Move Control Word From Floating-Point.
    INSN_DB(COP1_FMT_CTL_T,  "CTC1   ", FALSE, IFMT_tSFCR, 2, PSI_CTC1), //  6: Move Control Word To Floating-Point.
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub01[] = { // OPC_COP1, INSN_TYPE_REGIMM
    INSN_EX(OPT_COP1_BC1F,  "BC1F   ", FALSE, IFMT_B, 0, PSI_BC1F, EXR_FP, EXR_00, 0b00), //  0: Branch on FP False (1cyc*).
    INSN_EX(OPT_COP1_BC1T,  "BC1T   ", FALSE, IFMT_B, 0, PSI_BC1T, EXR_FP, EXR_00, 0b00), //  1: Branch on FP True (1cyc*).
    INSN_EX(OPT_COP1_BC1FL, "BC1FL  ", FALSE, IFMT_B, 0, PSI_BC1F, EXR_FP, EXR_00, 0b00), //  2: Branch on FP False Likely (1cyc*).
    INSN_EX(OPT_COP1_BC1TL, "BC1TL  ", FALSE, IFMT_B, 0, PSI_BC1T, EXR_FP, EXR_00, 0b00), //  3: Branch on FP True Likely (1cyc*).
    INSN_END(), // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub10[] = { // OPC_COP1, INSN_TYPE_FUNC
    INSN_DB(OPS_ADD_F,     "ADD    ", TRUE, IFMT_DST,   1, PSI_ADDF), //  0: ADD.[fmt]     Floating-Point Add (3cyc).
    INSN_DB(OPS_SUB_F,     "SUB    ", TRUE, IFMT_DST,   1, PSI_SUBF), //  1: SUB.[fmt]     Floating-Point Subtract (3cyc).
    INSN_DB(OPS_MUL_F,     "MUL    ", TRUE, IFMT_DST,   1, PSI_MULF), //  2: MUL.[fmt]     Floating-Point Multiply (S:5cyc; D:8cyc).
    INSN_DB(OPS_DIV_F,     "DIV    ", TRUE, IFMT_DST,   1, PSI_DIVF), //  3: DIV.[fmt]     Floating-Point Divide (S:29cyc; D:58cyc).
    INSN_DB(OPS_SQRT_F,    "SQRT   ", TRUE, IFMT_DS_XX, 1, PSI_CVTF), //  4: SQRT.[fmt]    Floating-Point Square Root (S:29cyc; D:58cyc).
    INSN_DB(OPS_ABS_F,     "ABS    ", TRUE, IFMT_DS_XX, 1, PSI_CVTF), //  5: ABS.[fmt]     Floating-Point Absolute Value (1cyc).
    INSN_DB(OPS_MOV_F,     "MOV    ", TRUE, IFMT_DS_XX, 1, PSI_MOVF), //  6: MOV.[fmt]     Floating-Point Move (1cyc).
    INSN_DB(OPS_NEG_F,     "NEG    ", TRUE, IFMT_DS_XX, 1, PSI_NEGF), //  7: NEG.[fmt]     Floating-Point Negate (1cyc).
    INSN_DB(OPS_ROUND_L_F, "ROUND.L", TRUE, IFMT_DS_IX, 1, PSI_CVTF), //  8: ROUND.L.[fmt] Floating-Point Round to Long Fixed-Point (5cyc).
    INSN_DB(OPS_TRUNC_L_F, "TRUNC.L", TRUE, IFMT_DS_IX, 1, PSI_CVTF), //  9: TRUNC.L.[fmt] Floating-Point Truncate to Long Fixed-Point (5cyc).
    INSN_DB(OPS_CEIL_L_F,  "CEIL.L ", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 10: CEIL.L.[fmt]  Floating-Point Ceiling to Long Fixed-Point (5cyc).
    INSN_DB(OPS_FLOOR_L_F, "FLOOR.L", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 11: FLOOR.L.[fmt] Floating-Point Floor to Long Fixed-Point (5cyc).
    INSN_DB(OPS_ROUND_W_F, "ROUND.W", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 12: ROUND.W.[fmt] Floating-Point Round to Word Fixed-Point (5cyc).
    INSN_DB(OPS_TRUNC_W_F, "TRUNC.W", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 13: TRUNC.W.[fmt] Floating-Point Truncate to Word Fixed-Point (5cyc).
    INSN_DB(OPS_CEIL_W_F,  "CEIL.W ", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 14: CEIL.W.[fmt]  Floating-Point Ceiling to Word Fixed-Point (5cyc).
    INSN_DB(OPS_FLOOR_W_F, "FLOOR.W", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 15: FLOOR.W.[fmt] Floating-Point Floor to Word Fixed-Point (5cyc).

    INSN_DB(OPS_CVT_S_F,   "CVT.S  ", TRUE, IFMT_DS_FX, 1, PSI_CVTF), // 32: CVT.S.[fmt]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc).
    INSN_DB(OPS_CVT_D_F,   "CVT.D  ", TRUE, IFMT_DS_FX, 1, PSI_CVTF), // 33: CVT.D.[fmt]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc).
    INSN_DB(OPS_CVT_W_F,   "CVT.W  ", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 36: CVT.W.[fmt]   Floating-Point Convert to Word Fixed-Point (5cyc).
    INSN_DB(OPS_CVT_L_F,   "CVT.L  ", TRUE, IFMT_DS_IX, 1, PSI_CVTF), // 37: CVT.L.[fmt]   Floating-Point Convert to Long Fixed-Point (5cyc).

    INSN_EX(OPS_C_F,       "C.F    ", TRUE, IFMT_ST,    0, PSI_C_F,    EXR_FP, EXR_00, 0b10), // 48: C.F.[fmt]     Floating-Point Compare (False) (1cyc).
    INSN_EX(OPS_C_UN,      "C.UN   ", TRUE, IFMT_ST,    0, PSI_C_UN,   EXR_FP, EXR_00, 0b10), // 49: C.UN.[fmt]    Floating-Point Compare (Unordered) (1cyc).
    INSN_EX(OPS_C_EQ,      "C.EQ   ", TRUE, IFMT_ST,    0, PSI_C_EQ,   EXR_FP, EXR_00, 0b10), // 50: C.EQ.[fmt]    Floating-point Compare (Equal) (1cyc).
    INSN_EX(OPS_C_UEQ,     "C.UEQ  ", TRUE, IFMT_ST,    0, PSI_C_UEQ,  EXR_FP, EXR_00, 0b10), // 51: C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc).
    INSN_EX(OPS_C_OLT,     "C.OLT  ", TRUE, IFMT_ST,    0, PSI_C_OLT,  EXR_FP, EXR_00, 0b10), // 52: C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc).
    INSN_EX(OPS_C_ULT,     "C.ULT  ", TRUE, IFMT_ST,    0, PSI_C_ULT,  EXR_FP, EXR_00, 0b10), // 53: C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc).
    INSN_EX(OPS_C_OLE,     "C.OLE  ", TRUE, IFMT_ST,    0, PSI_C_OLE,  EXR_FP, EXR_00, 0b10), // 54: C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc).
    INSN_EX(OPS_C_ULE,     "C.ULE  ", TRUE, IFMT_ST,    0, PSI_C_ULE,  EXR_FP, EXR_00, 0b10), // 55: C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc).
    INSN_EX(OPS_C_SF,      "C.SF   ", TRUE, IFMT_ST,    0, PSI_C_SF,   EXR_FP, EXR_00, 0b10), // 56: C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc).
    INSN_EX(OPS_C_NGLE,    "C.NGLE ", TRUE, IFMT_ST,    0, PSI_C_NGLE, EXR_FP, EXR_00, 0b10), // 57: C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc).
    INSN_EX(OPS_C_SEQ,     "C.SEQ  ", TRUE, IFMT_ST,    0, PSI_C_SEQ,  EXR_FP, EXR_00, 0b10), // 58: C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc).
    INSN_EX(OPS_C_NGL,     "C.NGL  ", TRUE, IFMT_ST,    0, PSI_C_NGL,  EXR_FP, EXR_00, 0b10), // 59: C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc).
    INSN_EX(OPS_C_LT,      "C.LT   ", TRUE, IFMT_ST,    0, PSI_C_LT,   EXR_FP, EXR_00, 0b10), // 60: C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc).
    INSN_EX(OPS_C_NGE,     "C.NGE  ", TRUE, IFMT_ST,    0, PSI_C_NGE,  EXR_FP, EXR_00, 0b10), // 61: C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc).
    INSN_EX(OPS_C_LE,      "C.LE   ", TRUE, IFMT_ST,    0, PSI_C_LE,   EXR_FP, EXR_00, 0b10), // 62: C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc).
    INSN_EX(OPS_C_NGT,     "C.NGT  ", TRUE, IFMT_ST,    0, PSI_C_NGT,  EXR_FP, EXR_00, 0b10), // 63: C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc).
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
    [PSEUDO_NOP   ] = INSN_DB(OPS_SLL,    "NOP    ", FALSE, IFMT_NOP, 0, PSI_NOP  ), // NOP (pseudo of SLL).
    [PSEUDO_MOVET ] = INSN_DB(OPS_ADD,    "MOVE   ", FALSE, IFMT_dt,  1, PSI_MOVET), // Move (pseudo of ADD and OR).
    [PSEUDO_MOVES ] = INSN_DB(OPS_ADD,    "MOVE   ", FALSE, IFMT_ds,  1, PSI_MOVES), // Move (pseudo of ADD).
    [PSEUDO_B     ] = INSN_DB(OPC_BEQ,    "B      ", FALSE, IFMT_B,   0, PSI_B    ), // Branch (pseudo of BEQ).
    [PSEUDO_BEQZ  ] = INSN_DB(OPC_BEQ,    "BEQZ   ", FALSE, IFMT_sB,  0, PSI_BEQ  ), // Branch on Equal to Zero (pseudo of BEQ).
    [PSEUDO_BNEZ  ] = INSN_DB(OPC_BNE,    "BNEZ   ", FALSE, IFMT_sB,  0, PSI_BNE  ), // Branch on Not Equal to Zero (pseudo of BNE).
    [PSEUDO_LI    ] = INSN_DB(OPC_ADDI,   "LI     ", FALSE, IFMT_tI,  1, PSI_LI   ), // Load Immediate (pseudo of ADDI and ADDIU).
    [PSEUDO_SUBI  ] = INSN_DB(OPC_ADDI,   "SUBI   ", FALSE, IFMT_tsI, 1, PSI_SUBI ), // Word Subtract Immediate (pseudo of ADDI).
    [PSEUDO_SUBIU ] = INSN_DB(OPC_ADDIU,  "SUBIU  ", FALSE, IFMT_tsI, 1, PSI_SUBI ), // Word Subtract Immediate Unsigned (pseudo of ADDIU).
    [PSEUDO_BEQZL ] = INSN_DB(OPC_BEQL,   "BEQZL  ", FALSE, IFMT_sB,  0, PSI_BEQ  ), // Branch on Equal to Zero Likely (pseudo of BEQL).
    [PSEUDO_BNEZL ] = INSN_DB(OPC_BNEL,   "BNEZL  ", FALSE, IFMT_sB,  0, PSI_BNE  ), // Branch on Not Equal to Zero Likely (pseudo of BNEL).
    [PSEUDO_DSUBI ] = INSN_DB(OPC_DADDI,  "DSUBI  ", FALSE, IFMT_tsI, 1, PSI_SUBI ), // Doubleword Subtract Immediate (pseudo of DADDI).
    [PSEUDO_DSUBIU] = INSN_DB(OPC_DADDIU, "DSUBIU ", FALSE, IFMT_tsI, 1, PSI_SUBI ), // Doubleword Subtract Immediate Unsigned (pseudo of DADDIU).
};


#define asm_v(...) asm volatile(__VA_ARGS__)
#define asm_v_b(_insnStr, _label) asm volatile goto(_insnStr" %l0"::::_label)
// Inline an instruction that does nothing but prints a hex value from 0x0-0x3FF (0-1023).
#define ASMCOMMENT(exc10); asm volatile("teq $0,$ra,%0"::"i"(exc10));

// A function which ncludes every instruction at least once:
void mips_III_insn_test(f32 a, f32 b, f32 c) {
    insn_test:
// insn_db_standard
    asm_v("nop"); // nop
    asm_v("j   0"); // j
    asm_v("jal 0"); // jal
    asm_v_b("beq  $t0,$t1,", insn_test); // beq
    asm_v_b("bne  $t0,$t1,", insn_test); // bne
    asm_v_b("blez $t0,",     insn_test); // blez
    asm_v_b("bgtz $t0,",     insn_test); // bgtz
    asm volatile("addi  $t0,$t1,0x1234"); // addi
    asm volatile("addiu $t0,$t1,0x1234"); // addiu
    asm volatile("slti  $t0,$t1,0x1234"); // slti
    asm volatile("sltiu $t0,$t1,0x1234"); // sltiu
    asm volatile("andi  $t0,$t1,0x1234"); // andi
    asm volatile("ori   $t0,$t1,0x1234"); // ori
    asm volatile("xori  $t0,$t1,0x1234"); // xori
    asm volatile("lui   $t0,0x1234"); // lui
    // cp0
    // cp1
    // cp2
    // cp3
    asm_v_b("beql  $t0,$t1,", insn_test); // beql
    asm_v_b("bnel  $t0,$t1,", insn_test); // bnel
    asm_v_b("blezl $t0,",     insn_test); // blezl
    asm_v_b("bgtzl $t0,",     insn_test); // bgtzl
    asm volatile("daddi  $t0,$t1,0x1234" ); // daddi
    asm volatile("daddiu $t0,$t1,0x1234" ); // daddiu
    asm volatile("ldl    $t0,0x1234($t1)"); // ldl
    asm volatile("ldr    $t0,0x1234($t1)"); // ldr
    asm volatile("lb     $t0,0x1234($t1)"); // lb
    asm volatile("lh     $t0,0x1234($t1)"); // lh
    asm volatile("lwl    $t0,0x1234($t1)"); // lwl
    asm volatile("lw     $t0,0x1234($t1)"); // lw
    asm volatile("lbu    $t0,0x1234($t1)"); // lbu
    asm volatile("lhu    $t0,0x1234($t1)"); // lhu
    asm volatile("lwr    $t0,0x1234($t1)"); // lwr
    asm volatile("lwu    $t0,0x1234($t1)"); // lwu
    asm volatile("sb     $t0,0x1234($t1)"); // sb
    asm volatile("sh     $t0,0x1234($t1)"); // sh
    asm volatile("swl    $t0,0x1234($t1)"); // swl
    asm volatile("sw     $t0,0x1234($t1)"); // sw
    asm volatile("sdl    $t0,0x1234($t1)"); // sdl
    asm volatile("sdr    $t0,0x1234($t1)"); // sdr
    asm volatile("cache  0x00,0x1234($t1)"); // cache
    asm volatile("cache  0x01,0x1234($t1)"); // cache
    // asm volatile("cache  0x02,0x1234($t1)"); // cache
    // asm volatile("cache  0x03,0x1234($t1)"); // cache
    asm volatile("cache  0x04,0x1234($t1)"); // cache
    asm volatile("cache  0x05,0x1234($t1)"); // cache
    // asm volatile("cache  0x06,0x1234($t1)"); // cache
    // asm volatile("cache  0x07,0x1234($t1)"); // cache
    asm volatile("cache  0x08,0x1234($t1)"); // cache
    asm volatile("cache  0x09,0x1234($t1)"); // cache
    // asm volatile("cache  0x0A,0x1234($t1)"); // cache
    // asm volatile("cache  0x0B,0x1234($t1)"); // cache
    // asm volatile("cache  0x0C,0x1234($t1)"); // cache
    // asm volatile("cache  0x0D,0x1234($t1)"); // cache
    // asm volatile("cache  0x0E,0x1234($t1)"); // cache
    // asm volatile("cache  0x0F,0x1234($t1)"); // cache
    asm volatile("cache  0x10,0x1234($t1)"); // cache
    asm volatile("cache  0x11,0x1234($t1)"); // cache
    // asm volatile("cache  0x12,0x1234($t1)"); // cache
    // asm volatile("cache  0x13,0x1234($t1)"); // cache
    asm volatile("cache  0x14,0x1234($t1)"); // cache
    asm volatile("cache  0x15,0x1234($t1)"); // cache
    // asm volatile("cache  0x16,0x1234($t1)"); // cache
    // asm volatile("cache  0x17,0x1234($t1)"); // cache
    asm volatile("cache  0x18,0x1234($t1)"); // cache
    asm volatile("cache  0x19,0x1234($t1)"); // cache
    // asm volatile("cache  0x1A,0x1234($t1)"); // cache
    // asm volatile("cache  0x1B,0x1234($t1)"); // cache
    // asm volatile("cache  0x1C,0x1234($t1)"); // cache
    // asm volatile("cache  0x1D,0x1234($t1)"); // cache
    // asm volatile("cache  0x1E,0x1234($t1)"); // cache
    // asm volatile("cache  0x1F,0x1234($t1)"); // cache
    asm volatile("ll     $t0,0x1234($t1)"); // ll
    asm volatile("lwc1   $0,0x1234($t1)"); // lwc1
    asm volatile("lwc2   $0,0x1234($t1)"); // lwc2
    // asm volatile("lwc3   $0,0x1234($t1)"); // lwc3
    asm volatile("lld    $t0,0x1234($t1)"); // lld
    asm volatile("ldc1   $0,0x1234($t1)"); // ldc1
    asm volatile("ldc2   $0,0x1234($t1)"); // ldc2
    asm volatile("ld     $t0,0x1234($t1)"); // ld
    asm volatile("sc     $t0,0x1234($t1)"); // sc
    asm volatile("swc1   $0,0x1234($t1)"); // swc1
    asm volatile("swc2   $0,0x1234($t1)"); // swc2
    // asm volatile("swc3   $0,0x1234($t1)"); // swc3
    asm volatile("scd    $t0,0x1234($t1)"); // scd
    asm volatile("sdc1   $0,0x1234($t1)"); // sdc1
    asm volatile("sdc2   $0,0x1234($t1)"); // sdc2
    asm volatile("sd     $t0,0x1234($t1)"); // sd
    asm_v("nop"); // nop
// insn_db_spec
    asm volatile("sll     $t0,$t1,0x10" ); // sll
    asm volatile("srl     $t0,$t1,0x10" ); // srl
    asm volatile("sra     $t0,$t1,0x10" ); // sra
    asm volatile("sll     $t0,$t1,$t2"  ); // sllv
    asm volatile("srl     $t0,$t1,$t2"  ); // srlv
    asm volatile("sra     $t0,$t1,$t2"  ); // srav
    asm volatile("jr      $t0"          ); // jr
    asm volatile("jalr    $t0"          ); // jalr
    asm volatile("syscall 1"            ); // syscall
    asm volatile("break   1,2"          ); // break
    asm volatile("sync"                 ); // sync
    asm volatile("mfhi    $t0"          ); // mfhi
    asm volatile("mthi    $t0"          ); // mthi
    asm volatile("mflo    $t0"          ); // mflo
    asm volatile("mtlo    $t0"          ); // mtlo
    asm volatile("dsll    $t0,$t1,$t2"  ); // dsllv
    asm volatile("dsrl    $t0,$t1,$t2"  ); // dsrlv
    asm volatile("dsra    $t0,$t1,$t2"  ); // dsrav
    ASMCOMMENT(0x3F0);
    asm volatile("mult    $t0,$t1"      ); // mult
    ASMCOMMENT(0x3F1);
    asm volatile("multu   $t0,$t1"      ); // multu
    ASMCOMMENT(0x3F2);
    asm volatile("div     $t0,$t1"      ); // div
    ASMCOMMENT(0x3F3);
    asm volatile("divu    $t0,$t1"      ); // divu
    ASMCOMMENT(0x3F4);
    asm volatile("dmult   $t0,$t1"      ); // dmult
    ASMCOMMENT(0x3F5);
    asm volatile("dmultu  $t0,$t1"      ); // dmultu
    ASMCOMMENT(0x3F6);
    asm volatile("ddiv    $t0,$t1"      ); // ddiv
    ASMCOMMENT(0x3F7);
    asm volatile("ddivu   $t0,$t1"      ); // ddivu
    ASMCOMMENT(0x3F8);
    asm volatile("add     $t0,$t1,$t2"  ); // add
    asm volatile("addu    $t0,$t1,$t2"  ); // addu
    asm volatile("sub     $t0,$t1,$t2"  ); // sub
    asm volatile("subu    $t0,$t1,$t2"  ); // subu
    asm volatile("and     $t0,$t1,$t2"  ); // and
    asm volatile("or      $t0,$t1,$t2"  ); // or
    asm volatile("xor     $t0,$t1,$t2"  ); // xor
    asm volatile("nor     $t0,$t1,$t2"  ); // nor
    asm volatile("slt     $t0,$t1,$t2"  ); // slt
    asm volatile("sltu    $t0,$t1,$t2"  ); // sltu
    asm volatile("dadd    $t0,$t1,$t2"  ); // dadd
    asm volatile("daddu   $t0,$t1,$t2"  ); // daddu
    asm volatile("dsub    $t0,$t1,$t2"  ); // dsub
    asm volatile("dsubu   $t0,$t1,$t2"  ); // dsubu
    asm volatile("tge     $t0,$t1,0x123"); // tge
    asm volatile("tgeu    $t0,$t1,0x123"); // tgeu
    asm volatile("tlt     $t0,$t1,0x123"); // tlt
    asm volatile("tltu    $t0,$t1,0x123"); // tltu
    asm volatile("teq     $t0,$t1,0x123"); // teq
    asm volatile("tne     $t0,$t1,0x123"); // tne
    asm volatile("dsll    $t0,$t1,0x10" ); // dsll
    asm volatile("dsrl    $t0,$t1,0x10" ); // dsrl
    asm volatile("dsra    $t0,$t1,0x10" ); // dsra
    asm volatile("dsll32  $t0,$t1,0x10" ); // dsll32
    asm volatile("dsrl32  $t0,$t1,0x10" ); // dsrl32
    asm volatile("dsra32  $t0,$t1,0x10" ); // dsra32
    asm_v("nop"); // nop
// insn_db_regi
    asm_v_b("bltz    $t0,", insn_test); // bltz
    asm_v_b("bgez    $t0,", insn_test); // bgez
    asm_v_b("bltzl   $t0,", insn_test); // bltzl
    asm_v_b("bgezl   $t0,", insn_test); // bgezl
    asm_v_b("bltzal  $t0,", insn_test); // bltzal
    asm_v_b("bgezal  $t0,", insn_test); // bgezal
    asm_v_b("bltzall $t0,", insn_test); // bltzall
    asm_v_b("bgezall $t0,", insn_test); // bgezall
    asm volatile("tgei    $t0,0x1234"); // tgei
    asm volatile("tgeiu   $t0,0x1234"); // tgeiu
    asm volatile("tlti    $t0,0x1234"); // tlti
    asm volatile("tltiu   $t0,0x1234"); // tltiu
    asm volatile("teqi    $t0,0x1234"); // teqi
    asm volatile("tnei    $t0,0x1234"); // tnei
    asm_v("nop"); // nop
// insn_db_cop0_sub00
    asm volatile("mfc0    $t0,$0"); // mfc0
    asm volatile("dmfc0   $t0,$1"); // dmfc0
    asm volatile("mtc0    $t0,$2"); // mtc0
    asm volatile("dmtc0   $t0,$3"); // dmtc0
    asm_v("nop"); // nop
// insn_db_cop0_sub10
    asm volatile("tlbp "); // tlbp
    asm volatile("tlbr "); // tlbr
    asm volatile("tlbwi"); // tlbwi
    asm volatile("tlbwr"); // tlbwr
    asm volatile("eret "); // eret
    asm_v("nop"); // nop
// insn_db_cop1_sub00
    asm volatile("mfc1  $t0, $0"); // mfc1
    asm volatile("dmfc1 $t0, $0"); // dmfc1
    asm volatile("mtc1  $t0, $0"); // mtc1
    asm volatile("dmtc1 $t0, $0"); // dmtc1
    asm volatile("cfc1  $t0,$31"); // cfc1
    asm volatile("ctc1  $t0,$31"); // ctc1
    asm_v("nop"); // nop
// insn_db_cop1_sub01
    asm_v_b("bc1f",  insn_test);  // bc1f
    asm_v_b("bc1t",  insn_test);  // bc1t
    asm_v_b("bc1fl", insn_test);  // bc1fl
    asm_v_b("bc1tl", insn_test);  // bc1tl
    asm_v("nop"); // nop
// insn_db_cop1_sub10
    asm volatile("add.s     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // add.s
    asm volatile("add.d     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // add.d
    asm volatile("sub.s     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // sub.s
    asm volatile("sub.d     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // sub.d
    asm volatile("mul.s     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // mul.s
    asm volatile("mul.d     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // mul.d
    asm volatile("div.s     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // div.s
    asm volatile("div.d     %0,%1,%2":"=f"(a):"f"(b),"f"(c)); // div.d
    asm volatile("sqrt.s    %0,%1":"=f"(a):"f"(b)); // sqrt.s
    asm volatile("sqrt.d    %0,%1":"=f"(a):"f"(b)); // sqrt.d
    asm volatile("abs.s     %0,%1":"=f"(a):"f"(b)); // abs.s
    asm volatile("abs.d     %0,%1":"=f"(a):"f"(b)); // abs.d
    asm volatile("mov.s     %0,%1":"=f"(a):"f"(b)); // mov.s
    asm volatile("mov.d     %0,%1":"=f"(a):"f"(b)); // mov.d
    asm volatile("neg.s     %0,%1":"=f"(a):"f"(b)); // neg.s
    asm volatile("neg.d     %0,%1":"=f"(a):"f"(b)); // neg.d

    asm volatile("round.l.s %0,%1":"=f"(a):"f"(b)); // round.l.s
    asm volatile("round.l.d %0,%1":"=f"(a):"f"(b)); // round.l.d
    asm volatile("trunc.l.s %0,%1":"=f"(a):"f"(b)); // trunc.l.s
    asm volatile("trunc.l.d %0,%1":"=f"(a):"f"(b)); // trunc.l.d
    asm volatile("ceil.l.s  %0,%1":"=f"(a):"f"(b)); // ceil.l.s
    asm volatile("ceil.l.d  %0,%1":"=f"(a):"f"(b)); // ceil.l.d
    asm volatile("floor.l.s %0,%1":"=f"(a):"f"(b)); // floor.l.s
    asm volatile("floor.l.d %0,%1":"=f"(a):"f"(b)); // floor.l.d

    asm volatile("round.w.s %0,%1":"=f"(a):"f"(b)); // round.w.s
    asm volatile("round.w.d %0,%1":"=f"(a):"f"(b)); // round.w.d
    asm volatile("trunc.w.s %0,%1":"=f"(a):"f"(b)); // trunc.w.s
    asm volatile("trunc.w.d %0,%1":"=f"(a):"f"(b)); // trunc.w.d
    asm volatile("ceil.w.s  %0,%1":"=f"(a):"f"(b)); // ceil.w.s
    asm volatile("ceil.w.d  %0,%1":"=f"(a):"f"(b)); // ceil.w.d
    asm volatile("floor.w.s %0,%1":"=f"(a):"f"(b)); // floor.w.s
    asm volatile("floor.w.d %0,%1":"=f"(a):"f"(b)); // floor.w.d

    // asm volatile("cvt.s.s   %0,%1":"=f"(a):"f"(b)); // cvt.s.s
    asm volatile("cvt.s.d   %0,%1":"=f"(a):"f"(b)); // cvt.s.d
    asm volatile("cvt.s.w   %0,%1":"=f"(a):"f"(b)); // cvt.s.w
    asm volatile("cvt.s.l   %0,%1":"=f"(a):"f"(b)); // cvt.s.l

    asm volatile("cvt.d.s   %0,%1":"=f"(a):"f"(b)); // cvt.d.s
    // asm volatile("cvt.d.d   %0,%1":"=f"(a):"f"(b)); // cvt.d.d
    asm volatile("cvt.d.w   %0,%1":"=f"(a):"f"(b)); // cvt.d.w
    asm volatile("cvt.d.l   %0,%1":"=f"(a):"f"(b)); // cvt.d.l

    asm volatile("cvt.w.s   %0,%1":"=f"(a):"f"(b)); // cvt.w.s
    asm volatile("cvt.w.d   %0,%1":"=f"(a):"f"(b)); // cvt.w.d
    // asm volatile("cvt.w.w   %0,%1":"=f"(a):"f"(b)); // cvt.w.w
    // asm volatile("cvt.w.l   %0,%1":"=f"(a):"f"(b)); // cvt.w.l

    asm volatile("cvt.l.s   %0,%1":"=f"(a):"f"(b)); // cvt.l.s
    asm volatile("cvt.l.d   %0,%1":"=f"(a):"f"(b)); // cvt.l.d
    // asm volatile("cvt.l.w   %0,%1":"=f"(a):"f"(b)); // cvt.l.w
    // asm volatile("cvt.l.;   %0,%1":"=f"(a):"f"(b)); // cvt.l.l

    asm volatile("c.f.s    %0,%1":"=f"(a):"f"(b)); // c.f.s
    asm volatile("c.f.d    %0,%1":"=f"(a):"f"(b)); // c.f.d
    asm volatile("c.un.s   %0,%1":"=f"(a):"f"(b)); // c.un.s
    asm volatile("c.un.d   %0,%1":"=f"(a):"f"(b)); // c.un.d
    asm volatile("c.eq.s   %0,%1":"=f"(a):"f"(b)); // c.eq.s
    asm volatile("c.eq.d   %0,%1":"=f"(a):"f"(b)); // c.eq.d
    asm volatile("c.ueq.s  %0,%1":"=f"(a):"f"(b)); // c.ueq.s
    asm volatile("c.ueq.d  %0,%1":"=f"(a):"f"(b)); // c.ueq.d
    asm volatile("c.olt.s  %0,%1":"=f"(a):"f"(b)); // c.olt.s
    asm volatile("c.olt.d  %0,%1":"=f"(a):"f"(b)); // c.olt.d
    asm volatile("c.ult.s  %0,%1":"=f"(a):"f"(b)); // c.ult.s
    asm volatile("c.ult.d  %0,%1":"=f"(a):"f"(b)); // c.ult.d
    asm volatile("c.ole.s  %0,%1":"=f"(a):"f"(b)); // c.ole.s
    asm volatile("c.ole.d  %0,%1":"=f"(a):"f"(b)); // c.ole.d
    asm volatile("c.ule.s  %0,%1":"=f"(a):"f"(b)); // c.ule.s
    asm volatile("c.ule.d  %0,%1":"=f"(a):"f"(b)); // c.ule.d
    asm volatile("c.sf.s   %0,%1":"=f"(a):"f"(b)); // c.sf.s
    asm volatile("c.sf.d   %0,%1":"=f"(a):"f"(b)); // c.sf.d
    asm volatile("c.ngle.s %0,%1":"=f"(a):"f"(b)); // c.ngle.s
    asm volatile("c.ngle.d %0,%1":"=f"(a):"f"(b)); // c.ngle.d
    asm volatile("c.seq.s  %0,%1":"=f"(a):"f"(b)); // c.seq.s
    asm volatile("c.seq.d  %0,%1":"=f"(a):"f"(b)); // c.seq.d
    asm volatile("c.ngl.s  %0,%1":"=f"(a):"f"(b)); // c.ngl.s
    asm volatile("c.ngl.d  %0,%1":"=f"(a):"f"(b)); // c.ngl.d
    asm volatile("c.lt.s   %0,%1":"=f"(a):"f"(b)); // c.lt.s
    asm volatile("c.lt.d   %0,%1":"=f"(a):"f"(b)); // c.lt.d
    asm volatile("c.nge.s  %0,%1":"=f"(a):"f"(b)); // c.nge.s
    asm volatile("c.nge.d  %0,%1":"=f"(a):"f"(b)); // c.nge.d
    asm volatile("c.le.s   %0,%1":"=f"(a):"f"(b)); // c.le.s
    asm volatile("c.le.d   %0,%1":"=f"(a):"f"(b)); // c.le.d
    asm volatile("c.ngt.s  %0,%1":"=f"(a):"f"(b)); // c.ngt.s
    asm volatile("c.ngt.d  %0,%1":"=f"(a):"f"(b)); // c.ngt.d
    asm_v("nop"); // nop
// insn_db_pseudo
    asm volatile("add $t0,$0,$t2"); // moveT
    asm volatile("add $t0,$t1,$0"); // moveS
    asm volatile("or  $t0,$t1,$0"); // moveS
    asm_v_b("beq $0,$0,",   insn_test); // b
    asm_v_b("beq $t0,$0,",  insn_test); // beqz
    asm_v_b("bne $t0,$0,",  insn_test); // bnez
    asm volatile("addi  $t0,$0,0x1234"); // li
    asm volatile("addiu $t0,$0,0x1234"); // li
    asm volatile("addi  $t0,$t1,-1"); // subi
    asm_v_b("beql $t0,$0,", insn_test); // beqzl
    asm_v_b("bnel $t0,$0,", insn_test); // bnezl
    asm volatile("daddi $t0,$t1,-1"); // dsubi
    asm_v("nop"); // nop
};

volatile Address gMipsIIITest = (volatile Address)mips_III_insn_test;
