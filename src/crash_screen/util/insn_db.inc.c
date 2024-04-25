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
    { .opcode = OPC_J          , .name = "J"      , .fmt = "\'J"  , .out = 0, }, //  2: Jump.
    { .opcode = OPC_JAL        , .name = "JAL"    , .fmt = "\'J"  , .out = 0, }, //  3: Jump and Link.
    { .opcode = OPC_BEQ        , .name = "BEQ"    , .fmt = "\'stB", .out = 0, }, //  4: Branch on Equal.
    { .opcode = OPC_BNE        , .name = "BNE"    , .fmt = "\'stB", .out = 0, }, //  5: Branch on Not Equal.
    { .opcode = OPC_BLEZ       , .name = "BLEZ"   , .fmt = "\'sB" , .out = 0, }, //  6: Branch on Less Than or Equal to Zero.
    { .opcode = OPC_BGTZ       , .name = "BGTZ"   , .fmt = "\'sB" , .out = 0, }, //  7: Branch on Greater Than Zero.
    { .opcode = OPC_ADDI       , .name = "ADDI"   , .fmt = "\'tsI", .out = 1, }, //  8: Add Immediate Word.
    { .opcode = OPC_ADDIU      , .name = "ADDIU"  , .fmt = "\'tsI", .out = 1, }, //  9: Add Immediate Unsigned Word.
    { .opcode = OPC_SLTI       , .name = "SLTI"   , .fmt = "\'tsI", .out = 1, }, // 10: Set on Less Than Immediate.
    { .opcode = OPC_SLTIU      , .name = "SLTIU"  , .fmt = "\'tsI", .out = 1, }, // 11: Set on Less Than Immediate Unsigned.
    { .opcode = OPC_ANDI       , .name = "ANDI"   , .fmt = "\'tsI", .out = 1, }, // 12: And Immediate.
    { .opcode = OPC_ORI        , .name = "ORI"    , .fmt = "\'tsI", .out = 1, }, // 13: Or Immediate.
    { .opcode = OPC_XORI       , .name = "XORI"   , .fmt = "\'tsI", .out = 1, }, // 14: Exclusive Or Immediate.
    { .opcode = OPC_LUI        , .name = "LUI"    , .fmt = "\'tI" , .out = 1, }, // 15: Load Upper Immediate.
    // OPC_COP0 (insn_db_cop0) // 16: Coprocessor-0 (System Control Coprocessor).
    // OPC_COP1 (insn_db_cop1) // 17: Coprocessor-1 (Floating-Point Unit).
    // OPC_COP2 (insn_db_cop2) // 18: Coprocessor-2 (Reality Co-Processor Vector Unit).
    // OPC_COP3 (insn_db_cop3) // 19: Coprocessor-3 (CP3).
    { .opcode = OPC_BEQL       , .name = "BEQL"   , .fmt = "\'stB", .out = 0, }, // 20: Branch on Equal Likely.
    { .opcode = OPC_BNEL       , .name = "BNEL"   , .fmt = "\'stB", .out = 0, }, // 21: Branch on Not Equal Likely.
    { .opcode = OPC_BLEZL      , .name = "BLEZL"  , .fmt = "\'sB" , .out = 0, }, // 22: Branch on Less Than or Equal to Zero Likely.
    { .opcode = OPC_BGTZL      , .name = "BGTZL"  , .fmt = "\'sB" , .out = 0, }, // 23: Branch on Greater Than Zero Likely.
    { .opcode = OPC_DADDI      , .name = "DADDI"  , .fmt = "\'tsI", .out = 1, }, // 24: Doubleword Add Immediate.
    { .opcode = OPC_DADDIU     , .name = "DADDIU" , .fmt = "\'tsI", .out = 1, }, // 25: Doubleword Add Immediate Unsigned.
    { .opcode = OPC_LDL        , .name = "LDL"    , .fmt = "\'to(", .out = 1, }, // 26: Load Doubleword Left.
    { .opcode = OPC_LDR        , .name = "LDR"    , .fmt = "\'to(", .out = 1, }, // 27: Load Doubleword Right.
    { .opcode = OPC_LB         , .name = "LB"     , .fmt = "\'to(", .out = 1, }, // 32: Load Byte.
    { .opcode = OPC_LH         , .name = "LH"     , .fmt = "\'to(", .out = 1, }, // 33: Load Halfword.
    { .opcode = OPC_LWL        , .name = "LWL"    , .fmt = "\'to(", .out = 1, }, // 34: Load Word Left.
    { .opcode = OPC_LW         , .name = "LW"     , .fmt = "\'to(", .out = 1, }, // 35: Load Word.
    { .opcode = OPC_LBU        , .name = "LBU"    , .fmt = "\'to(", .out = 1, }, // 36: Load Byte Unsigned.
    { .opcode = OPC_LHU        , .name = "LHU"    , .fmt = "\'to(", .out = 1, }, // 37: Load Halfword Unsigned.
    { .opcode = OPC_LWR        , .name = "LWR"    , .fmt = "\'to(", .out = 1, }, // 38: Load Word Right.
    { .opcode = OPC_LWU        , .name = "LWU"    , .fmt = "\'to(", .out = 1, }, // 39: Load Word Unsigned.
    { .opcode = OPC_SB         , .name = "SB"     , .fmt = "\'to(", .out = 0, }, // 40: Store Byte.
    { .opcode = OPC_SH         , .name = "SH"     , .fmt = "\'to(", .out = 0, }, // 41: Store Halfword.
    { .opcode = OPC_SWL        , .name = "SWL"    , .fmt = "\'to(", .out = 0, }, // 42: Store Word Left.
    { .opcode = OPC_SW         , .name = "SW"     , .fmt = "\'to(", .out = 0, }, // 43: Store Word.
    { .opcode = OPC_SDL        , .name = "SDL"    , .fmt = "\'to(", .out = 0, }, // 44: Store Doubleword Left.
    { .opcode = OPC_SDR        , .name = "SDR"    , .fmt = "\'to(", .out = 0, }, // 45: Store Doubleword Right.
    { .opcode = OPC_SWR        , .name = "SWR"    , .fmt = "\'to(", .out = 0, }, // 46: Store Word Right.
    { .opcode = OPC_CACHE      , .name = "CACHE"  , .fmt = "\'to(", .out = 0, }, // 47: https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html.
    { .opcode = OPC_LL         , .name = "LL"     , .fmt = "\'to(", .out = 1, }, // 48: Load Linked Word.
    { .opcode = OPC_LWC1       , .name = "LWC1"   , .fmt = "\'To(", .out = 0, .f1 = 0, .f2 = 1, .f2i = 1, }, // 49: Load Word to Coprocessor-1 (Floating-Point Unit).
    { .opcode = OPC_LWC2       , .name = "LWC2"   , .fmt = "\'to(", .out = 0, }, // 50: Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .opcode = OPC_LWC3       , .name = "LWC3"   , .fmt = "\'to(", .out = 0, }, // 51: Load Word to Coprocessor-3 (COP3).
    { .opcode = OPC_LLD        , .name = "LLD"    , .fmt = "\'to(", .out = 1, }, // 52: Load Linked Doubleword.
    { .opcode = OPC_LDC1       , .name = "LDC1"   , .fmt = "\'To(", .out = 1, .f1 = 0, .f2 = 1, .f2i = 1, }, // 53: Load Doubleword to Coprocessor-1 (Floating-Point Unit).
    { .opcode = OPC_LDC2       , .name = "LDC2"   , .fmt = "\'to(", .out = 1, }, // 54: Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .opcode = OPC_LD         , .name = "LD"     , .fmt = "\'to(", .out = 1, }, // 55: Load Doubleword.
    { .opcode = OPC_SC         , .name = "SC"     , .fmt = "\'to(", .out = 0, }, // 56: Store Conditional Word.
    { .opcode = OPC_SWC1       , .name = "SWC1"   , .fmt = "\'To(", .out = 0, .f1 = 0, .f2 = 1, .f2i = 1, }, // 57: Store Word to Coprocessor-1 (Floating-Point Unit).
    { .opcode = OPC_SWC2       , .name = "SWC2"   , .fmt = "\'to(", .out = 0, }, // 58: Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .opcode = OPC_SWC3       , .name = "SWC3"   , .fmt = "\'to(", .out = 0, }, // 59: Store Word to Coprocessor-3 (COP3).
    { .opcode = OPC_SCD        , .name = "SCD"    , .fmt = "\'to(", .out = 0, }, // 60: Store Conditional Doubleword.
    { .opcode = OPC_SDC1       , .name = "SDC1"   , .fmt = "\'To(", .out = 0, .f1 = 0, .f2 = 1, .f2i = 1, }, // 61: Store Doubleword to Coprocessor-1 (Floating-Point Unit).
    { .opcode = OPC_SDC2       , .name = "SDC2"   , .fmt = "\'to(", .out = 0, }, // 62: Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .opcode = OPC_SD         , .name = "SD"     , .fmt = "\'to(", .out = 0, }, // 63: Store Doubleword.
    {}, // NULL terminator.
};

// Special opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_spec[] = { // OPC_SPECIAL, INSN_TYPE_FUNC
    { .opcode = OPS_SLL        , .name = "SLL"    , .fmt = "\'dta", .out = 1, }, //  0: Shift Word Left Logical.
    { .opcode = OPS_SRL        , .name = "SRL"    , .fmt = "\'dta", .out = 1, }, //  2: Shift Word Right Logical.
    { .opcode = OPS_SRA        , .name = "SRA"    , .fmt = "\'dta", .out = 1, }, //  3: Shift Word Right Arithmetic.
    { .opcode = OPS_SLLV       , .name = "SLLV"   , .fmt = "\'dts", .out = 1, }, //  4: Shift Word Left Logical Variable.
    { .opcode = OPS_SRLV       , .name = "SRLV"   , .fmt = "\'dts", .out = 1, }, //  6: Shift Word Right Logical Variable.
    { .opcode = OPS_SRAV       , .name = "SRAV"   , .fmt = "\'dts", .out = 1, }, //  7: Shift Word Right Arithmetic Variable.
    { .opcode = OPS_JR         , .name = "JR"     , .fmt = "\'s"  , .out = 0, }, //  8: Jump Register.
    { .opcode = OPS_JALR       , .name = "JALR"   , .fmt = "\'ds" , .out = 0, }, //  9: Jump and Link Register.
    { .opcode = OPS_SYSCALL    , .name = "SYSCALL", .fmt = "\'E"  , .out = 0, }, // 12: System Call (assert).
    { .opcode = OPS_BREAK      , .name = "BREAK"  , .fmt = "\'E"  , .out = 0, }, // 13: Breakpoint.
    { .opcode = OPS_SYNC       , .name = "SYNC"   , .fmt = "\'"   , .out = 0, }, // 15: Synchronize Shared Memory.
    { .opcode = OPS_MFHI       , .name = "MFHI"   , .fmt = "\'d"  , .out = 6, }, // 16: Move From HI.
    { .opcode = OPS_MTHI       , .name = "MTHI"   , .fmt = "\'s"  , .out = 6, }, // 17: Move To HI.
    { .opcode = OPS_MFLO       , .name = "MFLO"   , .fmt = "\'d"  , .out = 6, }, // 18: Move From LO.
    { .opcode = OPS_MTLO       , .name = "MTLO"   , .fmt = "\'s"  , .out = 6, }, // 19: Move To LO.
    { .opcode = OPS_DSLLV      , .name = "DSLLV"  , .fmt = "\'dts", .out = 1, }, // 20: Doubleword Shift Left Logical Variable.
    { .opcode = OPS_DSRLV      , .name = "DSRLV"  , .fmt = "\'dts", .out = 1, }, // 22: Doubleword Shift Right Logical Variable.
    { .opcode = OPS_DSRAV      , .name = "DSRAV"  , .fmt = "\'dts", .out = 1, }, // 23: Doubleword Shift Right Arithmetic Variable.
    { .opcode = OPS_MULT       , .name = "MULT"   , .fmt = "\'st" , .out = 6, }, // 24: Multiply Word (5cyc).
    { .opcode = OPS_MULTU      , .name = "MULTU"  , .fmt = "\'st" , .out = 6, }, // 25: Multiply Unsigned Word (5cyc).
    { .opcode = OPS_DIV        , .name = "DIV"    , .fmt = "\'st" , .out = 6, }, // 26: Divide Word (37cyc).
    { .opcode = OPS_DIVU       , .name = "DIVU"   , .fmt = "\'st" , .out = 6, }, // 27: Divide Unsigned Word (37cyc).
    { .opcode = OPS_DMULT      , .name = "DMULT"  , .fmt = "\'st" , .out = 6, }, // 28: Doubleword Multiply (8cyc).
    { .opcode = OPS_DMULTU     , .name = "DMULTU" , .fmt = "\'st" , .out = 6, }, // 29: Doubleword Multiply Unsigned (8cyc).
    { .opcode = OPS_DDIV       , .name = "DDIV"   , .fmt = "\'st" , .out = 6, }, // 30: Doubleword Divide (69cyc).
    { .opcode = OPS_DDIVU      , .name = "DDIVU"  , .fmt = "\'st" , .out = 6, }, // 31: Doubleword Divide Unsigned (69cyc).
    { .opcode = OPS_ADD        , .name = "ADD"    , .fmt = "\'dst", .out = 1, }, // 32: Add Word.
    { .opcode = OPS_ADDU       , .name = "ADDU"   , .fmt = "\'dst", .out = 1, }, // 33: Add Unsigned Word.
    { .opcode = OPS_SUB        , .name = "SUB"    , .fmt = "\'dst", .out = 1, }, // 34: Subtract Word.
    { .opcode = OPS_SUBU       , .name = "SUBU"   , .fmt = "\'dst", .out = 1, }, // 35: Subtract Unsigned Word.
    { .opcode = OPS_AND        , .name = "AND"    , .fmt = "\'dst", .out = 1, }, // 36: And.
    { .opcode = OPS_OR         , .name = "OR"     , .fmt = "\'dst", .out = 1, }, // 37: Or.
    { .opcode = OPS_XOR        , .name = "XOR"    , .fmt = "\'dst", .out = 1, }, // 38: Exclusive Or.
    { .opcode = OPS_NOR        , .name = "NOR"    , .fmt = "\'dst", .out = 1, }, // 39: Nor.
    { .opcode = OPS_SLT        , .name = "SLT"    , .fmt = "\'dst", .out = 1, }, // 42: Set on Less Than.
    { .opcode = OPS_SLTU       , .name = "SLTU"   , .fmt = "\'dst", .out = 1, }, // 43: Set on Less Than Unsigned.
    { .opcode = OPS_DADD       , .name = "DADD"   , .fmt = "\'dst", .out = 1, }, // 44: Doubleword Add.
    { .opcode = OPS_DADDU      , .name = "DADDU"  , .fmt = "\'dst", .out = 1, }, // 45: Doubleword Add Unsigned.
    { .opcode = OPS_DSUB       , .name = "DSUB"   , .fmt = "\'dst", .out = 1, }, // 46: Doubleword Subtract.
    { .opcode = OPS_DSUBU      , .name = "DSUBU"  , .fmt = "\'dst", .out = 1, }, // 47: Doubleword Subtract Unsigned.
    { .opcode = OPS_TGE        , .name = "TGE"    , .fmt = "\'ste", .out = 0, }, // 48: Trap if Greater Than or Equal.
    { .opcode = OPS_TGEU       , .name = "TGEU"   , .fmt = "\'ste", .out = 0, }, // 49: Trap if Greater Than or Equal Unsigned.
    { .opcode = OPS_TLT        , .name = "TLT"    , .fmt = "\'ste", .out = 0, }, // 50: Trap if Less Than.
    { .opcode = OPS_TLTU       , .name = "TLTU"   , .fmt = "\'ste", .out = 0, }, // 51: Trap if Less Than Unsigned.
    { .opcode = OPS_TEQ        , .name = "TEQ"    , .fmt = "\'ste", .out = 0, }, // 52: Trap if Equal.
    { .opcode = OPS_TNE        , .name = "TNE"    , .fmt = "\'ste", .out = 0, }, // 54: Trap if Not Equal.
    { .opcode = OPS_DSLL       , .name = "DSLL"   , .fmt = "\'dta", .out = 1, }, // 56: Doubleword Shift Left Logical.
    { .opcode = OPS_DSRL       , .name = "DSRL"   , .fmt = "\'dta", .out = 1, }, // 58: Doubleword Shift Right Logical.
    { .opcode = OPS_DSRA       , .name = "DSRA"   , .fmt = "\'dta", .out = 1, }, // 59: Doubleword Shift Right Arithmetic.
    { .opcode = OPS_DSLL32     , .name = "DSLL32" , .fmt = "\'dta", .out = 1, }, // 60: Doubleword Shift Left Logical + 32.
    { .opcode = OPS_DSRL32     , .name = "DSRL32" , .fmt = "\'dta", .out = 1, }, // 62: Doubleword Shift Right Logical + 32.
    { .opcode = OPS_DSRA32     , .name = "DSRA32" , .fmt = "\'dta", .out = 1, }, // 63: Doubleword Shift Right Arithmetic + 32.
    {}, // NULL terminator.
};

// Register opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_regi[] = { // OPC_REGIMM, INSN_TYPE_REGIMM
    { .opcode = OPR_BLTZ       , .name = "BLTZ"   , .fmt = "\'sB" , .out = 0, }, //  0: Branch on Less Than Zero.
    { .opcode = OPR_BGEZ       , .name = "BGEZ"   , .fmt = "\'sB" , .out = 0, }, //  1: Branch on Greater Than or Equal to Zero.
    { .opcode = OPR_BLTZL      , .name = "BLTZL"  , .fmt = "\'sB" , .out = 0, }, //  2: Branch on Less Than Zero Likely.
    { .opcode = OPR_BGEZL      , .name = "BGEZL"  , .fmt = "\'sB" , .out = 0, }, //  3: Branch on Greater Than or Equal to Zero Likely.
    { .opcode = OPR_BLTZAL     , .name = "BLTZAL" , .fmt = "\'sB" , .out = 7, }, // 16: Branch on Less Than Zero and Link.
    { .opcode = OPR_BGEZAL     , .name = "BGEZAL" , .fmt = "\'sB" , .out = 7, }, // 17: Branch on Greater Than or Equal to Zero and Link.
    { .opcode = OPR_BLTZALL    , .name = "BLTZALL", .fmt = "\'sB" , .out = 7, }, // 18: Branch on Less Than Zero and Link Likely.
    { .opcode = OPR_BGEZALL    , .name = "BGEZALL", .fmt = "\'sB" , .out = 7, }, // 19: Branch on Greater Than or Equal to Zero and Link Likely.

    { .opcode = OPR_TGEI       , .name = "TGEI"   , .fmt = "\'sI" , .out = 0, }, //  8: Trap if Greater Than or Equal Immediate.
    { .opcode = OPR_TGEIU      , .name = "TGEIU"  , .fmt = "\'sI" , .out = 0, }, //  9: Trap if Greater Than or Equal Unsigned Immediate.
    { .opcode = OPR_TLTI       , .name = "TLTI"   , .fmt = "\'sI" , .out = 0, }, // 10: Trap if Less Than Immediate.
    { .opcode = OPR_TLTIU      , .name = "TLTIU"  , .fmt = "\'sI" , .out = 0, }, // 11: Trap if Less Than Unsigned Immediate.
    { .opcode = OPR_TEQI       , .name = "TEQI"   , .fmt = "\'sI" , .out = 0, }, // 12: Trap if Equal Immediate.
    { .opcode = OPR_TNEI       , .name = "TNEI"   , .fmt = "\'sI" , .out = 0, }, // 14: Trap if Not Equal Immediate.
    {}, // NULL terminator.
};

// Coprocessor-0 (System Control Coprocessor):
ALIGNED32 static const InsnTemplate insn_db_cop0_sub00[] = { // OPC_COP0, INSN_TYPE_COP_FMT
    { .opcode = COP0_MF        , .name = "MFC0"   , .fmt = "\'t0" , .out = 1, }, //  0: Move from System Control Coprocessor.
    { .opcode = COP0_DMF       , .name = "DMFC0"  , .fmt = "\'t0" , .out = 1, }, //  1: Doubleword Move from System Control Coprocessor.
    { .opcode = COP0_MT        , .name = "MTC0"   , .fmt = "\'t0" , .out = 2, }, //  4: Move to System Control Coprocessor.
    { .opcode = COP0_DMT       , .name = "DMTC0"  , .fmt = "\'t0" , .out = 2, }, //  5: Doubleword Move to System Control Coprocessor.
    {}, // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop0_sub10[] = { // OPC_COP0, INSN_TYPE_FUNC
    //! TODO: These use the COP0 TLB Index register as the input/output.
    { .opcode = OPC_COP0_TLBP  , .name = "TLBP"   , .fmt = "\'"   , .out = 0, }, //  8: Searches for a TLB entry that matches the EntryHi register.
    { .opcode = OPC_COP0_TLBR  , .name = "TLBR"   , .fmt = "\'"   , .out = 0, }, //  1: Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
    { .opcode = OPC_COP0_TLBWI , .name = "TLBWI"  , .fmt = "\'"   , .out = 0, }, //  2: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register.
    { .opcode = OPC_COP0_TLBWR , .name = "TLBWR"  , .fmt = "\'"   , .out = 0, }, //  6: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register.
    { .opcode = OPC_COP0_ERET  , .name = "ERET"   , .fmt = "\'"   , .out = 0, }, // 24: Return from interrupt, exception, or error exception.
    {}, // NULL terminator.
};

// Coprocessor-1 (Floating-Point Unit):
ALIGNED32 static const InsnTemplate insn_db_cop1_sub00[] = { // OPC_COP1, INSN_TYPE_COP_FMT
    { .opcode = COP1_FMT_SINGLE, .name = "MFC1"   , .fmt = "\'tS" , .out = 1, .f1 = 1, .f2 = 0, .f2i = 1, }, //  0: Move Word From Floating-Point.
    { .opcode = COP1_FMT_DOUBLE, .name = "DMFC1"  , .fmt = "\'tS" , .out = 1, .f1 = 1, .f2 = 0, .f2i = 1, }, //  1: Doubleword Move From Floating-Point.
    { .opcode = COP1_FMT_WORD  , .name = "MTC1"   , .fmt = "\'tS" , .out = 2, .f1 = 0, .f2 = 1, .f2i = 2, }, //  4: Move Word To Floating-Point.
    { .opcode = COP1_FMT_LONG  , .name = "DMTC1"  , .fmt = "\'tS" , .out = 2, .f1 = 0, .f2 = 1, .f2i = 2, }, //  5: Doubleword Move To Floating-Point.
    { .opcode = COP1_FMT_CTL_F , .name = "CFC1"   , .fmt = "\'tS" , .out = 1, .f1 = 1, .f2 = 0, .f2i = 1, }, //  2: Move Control Word From Floating-Point.
    { .opcode = COP1_FMT_CTL_T , .name = "CTC1"   , .fmt = "\'tS" , .out = 2, .f1 = 0, .f2 = 1, .f2i = 2, }, //  6: Move Control Word To Floating-Point.
    {}, // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub01[] = { // OPC_COP1, INSN_TYPE_REGIMM
    { .opcode = OPT_COP1_BC1F  , .name = "BC1F"   , .fmt = "\'B"  , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, //  0: Branch on FP False (1cyc*).
    { .opcode = OPT_COP1_BC1T  , .name = "BC1T"   , .fmt = "\'B"  , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, //  1: Branch on FP True (1cyc*).
    { .opcode = OPT_COP1_BC1FL , .name = "BC1FL"  , .fmt = "\'B"  , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, //  2: Branch on FP False Likely (1cyc*).
    { .opcode = OPT_COP1_BC1TL , .name = "BC1TL"  , .fmt = "\'B"  , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, //  3: Branch on FP True Likely (1cyc*).
    {}, // NULL terminator.
};
ALIGNED32 static const InsnTemplate insn_db_cop1_sub10[] = { // OPC_COP1, INSN_TYPE_FUNC
    { .opcode = OPS_ADD_F      , .name = "ADD"    , .fmt = "\"DST", .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  0: ADD.[FMT]     Floating-Point Add (3cyc).
    { .opcode = OPS_SUB_F      , .name = "SUB"    , .fmt = "\"DST", .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  1: SUB.[FMT]     Floating-Point Subtract (3cyc).
    { .opcode = OPS_MUL_F      , .name = "MUL"    , .fmt = "\"DST", .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  2: MUL.[FMT]     Floating-Point Multiply (S:5cyc; D:8cyc).
    { .opcode = OPS_DIV_F      , .name = "DIV"    , .fmt = "\"DST", .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  3: DIV.[FMT]     Floating-Point Divide (S:29cyc; D:58cyc).
    { .opcode = OPS_SQRT_F     , .name = "SQRT"   , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  4: SQRT.[FMT]    Floating-Point Square Root (S:29cyc; D:58cyc).
    { .opcode = OPS_ABS_F      , .name = "ABS"    , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  5: ABS.[FMT]     Floating-Point Absolute Value (1cyc).
    { .opcode = OPS_MOV_F      , .name = "MOV"    , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  6: MOV.[FMT]     Floating-Point Move (1cyc).
    { .opcode = OPS_NEG_F      , .name = "NEG"    , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 1, .f2i = 0, }, //  7: NEG.[FMT]     Floating-Point Negate (1cyc).
    { .opcode = OPS_ROUND_L_F  , .name = "ROUND.L", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, //  8: ROUND.L.[FMT] Floating-Point Round to Long Fixed-Point (5cyc).
    { .opcode = OPS_TRUNC_L_F  , .name = "TRUNC.L", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, //  9: TRUNC.L.[FMT] Floating-Point Truncate to Long Fixed-Point (5cyc).
    { .opcode = OPS_CEIL_L_F   , .name = "CEIL.L" , .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 10: CEIL.L.[FMT]  Floating-Point Ceiling to Long Fixed-Point (5cyc).
    { .opcode = OPS_FLOOR_L_F  , .name = "FLOOR.L", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 11: FLOOR.L.[FMT] Floating-Point Floor to Long Fixed-Point (5cyc).
    { .opcode = OPS_ROUND_W_F  , .name = "ROUND.W", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 12: ROUND.W.[FMT] Floating-Point Round to Word Fixed-Point (5cyc).
    { .opcode = OPS_TRUNC_W_F  , .name = "TRUNC.W", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 13: TRUNC.W.[FMT] Floating-Point Truncate to Word Fixed-Point (5cyc).
    { .opcode = OPS_CEIL_W_F   , .name = "CEIL.W" , .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 14: CEIL.W.[FMT]  Floating-Point Ceiling to Word Fixed-Point (5cyc).
    { .opcode = OPS_FLOOR_W_F  , .name = "FLOOR.W", .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 15: FLOOR.W.[FMT] Floating-Point Floor to Word Fixed-Point (5cyc).

    { .opcode = OPS_CVT_S_F    , .name = "CVT.S"  , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 0, .f2i = 2, }, // 32: CVT.S.[FMT]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc).
    { .opcode = OPS_CVT_D_F    , .name = "CVT.D"  , .fmt = "\"DS" , .out = 1, .f1 = 1, .f2 = 0, .f2i = 2, }, // 33: CVT.D.[FMT]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc).
    { .opcode = OPS_CVT_W_F    , .name = "CVT.W"  , .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 36: CVT.W.[FMT]   Floating-Point Convert to Word Fixed-Point (5cyc).
    { .opcode = OPS_CVT_L_F    , .name = "CVT.L"  , .fmt = "\"DS" , .out = 1, .f1 = 0, .f2 = 1, .f2i = 2, }, // 37: CVT.L.[FMT]   Floating-Point Convert to Long Fixed-Point (5cyc).

    { .opcode = OPS_C_F        , .name = "C.F"    , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 48: C.F.[FMT]     Floating-Point Compare (False) (1cyc).
    { .opcode = OPS_C_UN       , .name = "C.UN"   , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 49: C.UN.[FMT]    Floating-Point Compare (Unordered) (1cyc).
    { .opcode = OPS_C_EQ       , .name = "C.EQ"   , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 50: C.EQ.[FMT]    Floating-point Compare (Equal) (1cyc).
    { .opcode = OPS_C_UEQ      , .name = "C.UEQ"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 51: C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc).
    { .opcode = OPS_C_OLT      , .name = "C.OLT"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 52: C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc).
    { .opcode = OPS_C_ULT      , .name = "C.ULT"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 53: C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc).
    { .opcode = OPS_C_OLE      , .name = "C.OLE"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 54: C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc).
    { .opcode = OPS_C_ULE      , .name = "C.ULE"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 55: C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc).
    { .opcode = OPS_C_SF       , .name = "C.SF"   , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 56: C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc).
    { .opcode = OPS_C_NGLE     , .name = "C.NGLE" , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 57: C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc).
    { .opcode = OPS_C_SEQ      , .name = "C.SEQ"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 58: C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc).
    { .opcode = OPS_C_NGL      , .name = "C.NGL"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 59: C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc).
    { .opcode = OPS_C_LT       , .name = "C.LT"   , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 60: C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc).
    { .opcode = OPS_C_NGE      , .name = "C.NGE"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 61: C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc).
    { .opcode = OPS_C_LE       , .name = "C.LE"   , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 62: C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc).
    { .opcode = OPS_C_NGT      , .name = "C.NGT"  , .fmt = "\"ST" , .out = 0, .f1 = 1, .f2 = 1, .f2i = 0, }, // 63: C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc).
    {}, // NULL terminator.
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
    [PSEUDO_NOP  ] = { .opcode = OPS_SLL  , .name = "NOP"  , .fmt = "_"    , .out = 0, }, // NOP (pseudo of SLL).
    [PSEUDO_MOVET] = { .opcode = OPS_ADD  , .name = "MOVE" , .fmt = "\'dt" , .out = 1, }, // Move (pseudo of ADD and OR).
    [PSEUDO_MOVES] = { .opcode = OPS_ADD  , .name = "MOVE" , .fmt = "\'ds" , .out = 1, }, // Move (pseudo of ADD).
    [PSEUDO_B    ] = { .opcode = OPC_BEQ  , .name = "B"    , .fmt = "\'B"  , .out = 0, }, // Branch (pseudo of BEQ).
    [PSEUDO_BEQZ ] = { .opcode = OPC_BEQ  , .name = "BEQZ" , .fmt = "\'sB" , .out = 0, }, // Branch on Equal to Zero (pseudo of BEQ).
    [PSEUDO_BNEZ ] = { .opcode = OPC_BNE  , .name = "BNEZ" , .fmt = "\'sB" , .out = 0, }, // Branch on Not Equal to Zero (pseudo of BNE).
    [PSEUDO_LI   ] = { .opcode = OPC_ADDI , .name = "LI"   , .fmt = "\'tI" , .out = 1, }, // Load Immediate (pseudo of ADDI and ADDIU).
    [PSEUDO_SUBI ] = { .opcode = OPC_ADDI , .name = "SUBI" , .fmt = "\'tsi", .out = 1, }, // Subtract Immediate Word (pseudo of ADDI).
    [PSEUDO_BEQZL] = { .opcode = OPC_BEQL , .name = "BEQZL", .fmt = "\'sB" , .out = 0, }, // Branch on Equal to Zero Likely (pseudo of BEQL).
    [PSEUDO_BNEZL] = { .opcode = OPC_BNEL , .name = "BNEZL", .fmt = "\'sB" , .out = 0, }, // Branch on Not Equal to Zero Likely (pseudo of BNEL).
    [PSEUDO_DSUBI] = { .opcode = OPC_DADDI, .name = "DSUBI", .fmt = "\'tsi", .out = 1, }, // Doubleword Subtract Immediate (pseudo of DADDI).
};
