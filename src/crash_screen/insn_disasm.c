#include <PR/ultratypes.h>
#include <string.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "color_presets.h"
#include "crash_screen.h"
#include "insn_disasm.h"
#include "engine/math_util.h"


/**
 * How to find instructions:
 * 
 * - If first 4 bits (cop_opcode) == 0100 (COP_OPCODE):
 *  - The next 2 bits (cop_num) are coprocessor number, so use insn_db_cop(z)
 *  - If the next bit (cop_bit) is set:
 *   - Compare the last 6 bits (func) to the list
 *  - Otherwise:
 *   - If the bit after is set:
 *    - Compare the next 3 bits (fmt) to the list
 *   - Otherwise:
 *    - skip the next 3 bits (fmt) and compare the 5 bits after that (ft) to the list
 * - otherwise, check first 6 bits (opcode)
 *  - if 0 (OPC_SPECIAL):
 *   - compare the last 6 bits (func) to insn_db_spec
 *  - otherwise, if 1 (OPC_REGIMM):
 *   - skip the next 5 bits and compare the 5 bits after that (regimm)
 *  - otherwise, compare the first 6 bits to insn_db
 */


// MIPS III Instructions:

// Opcode instructions:
ALIGNED32 static const InsnTemplate insn_db[] = {
    // OPC_SPECIAL (insn_db_spec)
    // OPC_REGIMM (insn_db_regi)
    { .name = "J"     , .fmt = "\'J"  , .opcode = OPC_J      }, // Jump.
    { .name = "JAL"   , .fmt = "\'J"  , .opcode = OPC_JAL    }, // Jump and Link.
    { .name = "BEQ"   , .fmt = "\'stB", .opcode = OPC_BEQ    }, // Branch on Equal.
    { .name = "BNE"   , .fmt = "\'stB", .opcode = OPC_BNE    }, // Branch on Not Equal.
    { .name = "BLEZ"  , .fmt = "\'sB" , .opcode = OPC_BLEZ   }, // Branch on Less Than or Equal to Zero.
    { .name = "BGTZ"  , .fmt = "\'sB" , .opcode = OPC_BGTZ   }, // Branch on Greater Than Zero.
    { .name = "ADDI"  , .fmt = "\'tsI", .opcode = OPC_ADDI   }, // Add Immediate Word.
    { .name = "ADDIU" , .fmt = "\'tsI", .opcode = OPC_ADDIU  }, // Add Immediate Unsigned Word.
    { .name = "SLTI"  , .fmt = "\'tsI", .opcode = OPC_SLTI   }, // Set on Less Than Immediate.
    { .name = "SLTIU" , .fmt = "\'tsI", .opcode = OPC_SLTIU  }, // Set on Less Than Immediate Unsigned.
    { .name = "ANDI"  , .fmt = "\'tsI", .opcode = OPC_ANDI   }, // And Immediate.
    { .name = "ORI"   , .fmt = "\'tsI", .opcode = OPC_ORI    }, // Or Immediate.
    { .name = "XORI"  , .fmt = "\'tsI", .opcode = OPC_XORI   }, // Exclusive Or Immediate.
    { .name = "LUI"   , .fmt = "\'tI" , .opcode = OPC_LUI    }, // Load Upper Immediate.
    // OPC_COP0 (insn_db_cop0)
    // OPC_COP1 (insn_db_cop1)
    // OPC_COP2 (insn_db_cop2)
    // OPC_COP3 (insn_db_cop3)
    { .name = "BEQL"  , .fmt = "\'stB", .opcode = OPC_BEQL   }, // Branch on Equal Likely.
    { .name = "BNEL"  , .fmt = "\'stB", .opcode = OPC_BNEL   }, // Branch on Not Equal Likely.
    { .name = "BLEZL" , .fmt = "\'sB" , .opcode = OPC_BLEZL  }, // Branch on Less Than or Equal to Zero Likely.
    { .name = "BGTZL" , .fmt = "\'sB" , .opcode = OPC_BGTZL  }, // Branch on Greater Than Zero Likely.
    { .name = "DADDI" , .fmt = "\'tsI", .opcode = OPC_DADDI  }, // Doubleword Add Immediate.
    { .name = "DADDIU", .fmt = "\'tsI", .opcode = OPC_DADDIU }, // Doubleword Add Immediate Unsigned.
    { .name = "LDL"   , .fmt = "\'tI(", .opcode = OPC_LDL    }, // Load Doubleword Left.
    { .name = "LDR"   , .fmt = "\'tI(", .opcode = OPC_LDR    }, // Load Doubleword Right.
    { .name = "LB"    , .fmt = "\'tI(", .opcode = OPC_LB     }, // Load Byte.
    { .name = "LH"    , .fmt = "\'tI(", .opcode = OPC_LH     }, // Load Halfword.
    { .name = "LWL"   , .fmt = "\'tI(", .opcode = OPC_LWL    }, // Load Word Left.
    { .name = "LW"    , .fmt = "\'tI(", .opcode = OPC_LW     }, // Load Word.
    { .name = "LBU"   , .fmt = "\'tI(", .opcode = OPC_LBU    }, // Load Byte Unsigned.
    { .name = "LHU"   , .fmt = "\'tI(", .opcode = OPC_LHU    }, // Load Halfword Unsigned.
    { .name = "LWR"   , .fmt = "\'tI(", .opcode = OPC_LWR    }, // Load Word Right.
    { .name = "LWU"   , .fmt = "\'tI(", .opcode = OPC_LWU    }, // Load Word Unsigned.
    { .name = "SB"    , .fmt = "\'tI(", .opcode = OPC_SB     }, // Store Byte.
    { .name = "SH"    , .fmt = "\'tI(", .opcode = OPC_SH     }, // Store Halfword.
    { .name = "SWL"   , .fmt = "\'tI(", .opcode = OPC_SWL    }, // Store Word Left.
    { .name = "SW"    , .fmt = "\'tI(", .opcode = OPC_SW     }, // Store Word.
    { .name = "SDL"   , .fmt = "\'tI(", .opcode = OPC_SDL    }, // Store Doubleword Left.
    { .name = "SDR"   , .fmt = "\'tI(", .opcode = OPC_SDR    }, // Store Doubleword Right.
    { .name = "SWR"   , .fmt = "\'tI(", .opcode = OPC_SWR    }, // Store Word Right.
    { .name = "CACHE" , .fmt = "\'tI(", .opcode = OPC_CACHE  }, // https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html.
    { .name = "LL"    , .fmt = "\'tI(", .opcode = OPC_LL     }, // Load Linked Word.
    { .name = "LWC1"  , .fmt = "\'TI(", .opcode = OPC_LWC1   }, // Load Word to Coprocessor-1 (Floating-Point Unit).
    { .name = "LWC2"  , .fmt = "\'TI(", .opcode = OPC_LWC2   }, // Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .name = "LWC3"  , .fmt = "\'TI(", .opcode = OPC_LWC3   }, // Load Word to Coprocessor-3 (COP3).
    { .name = "LLD"   , .fmt = "\'tI(", .opcode = OPC_LLD    }, // Load Linked Doubleword.
    { .name = "LDC1"  , .fmt = "\'tI(", .opcode = OPC_LDC1   }, // Load Doubleword to Coprocessor-1 (Floating-Point Unit).
    { .name = "LDC2"  , .fmt = "\'tI(", .opcode = OPC_LDC2   }, // Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .name = "LD"    , .fmt = "\'tI(", .opcode = OPC_LD     }, // Load Doubleword.
    { .name = "SC"    , .fmt = "\'tI(", .opcode = OPC_SC     }, // Store Conditional Word.
    { .name = "SWC1"  , .fmt = "\'TI(", .opcode = OPC_SWC1   }, // Store Word to Coprocessor-1 (Floating-Point Unit).
    { .name = "SWC2"  , .fmt = "\'TI(", .opcode = OPC_SWC2   }, // Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .name = "SWC3"  , .fmt = "\'TI(", .opcode = OPC_SWC3   }, // Store Word to Coprocessor-3 (COP3).
    { .name = "SCD"   , .fmt = "\'tI(", .opcode = OPC_SCD    }, // Store Conditional Doubleword.
    { .name = "SDC1"  , .fmt = "\'tI(", .opcode = OPC_SDC1   }, // Store Doubleword to Coprocessor-1 (Floating-Point Unit).
    { .name = "SDC2"  , .fmt = "\'tI(", .opcode = OPC_SDC2   }, // Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    { .name = "SD"    , .fmt = "\'tI(", .opcode = OPC_SD     }, // Store Doubleword.
    { .name = "." },
};

// Special opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_spec[] = { // OPC_SPECIAL
    { .name = "SLL"    , .fmt =  "\'dta", .opcode = OPS_SLL     }, // Shift Word Left Logical.
    { .name = "SRL"    , .fmt =  "\'dta", .opcode = OPS_SRL     }, // Shift Word Right Logical.
    { .name = "SRA"    , .fmt =  "\'dta", .opcode = OPS_SRA     }, // Shift Word Right Arithmetic.
    { .name = "SLLV"   , .fmt =  "\'dts", .opcode = OPS_SLLV    }, // Shift Word Left Logical Variable.
    { .name = "SRLV"   , .fmt =  "\'dts", .opcode = OPS_SRLV    }, // Shift Word Right Logical Variable.
    { .name = "SRAV"   , .fmt =  "\'dts", .opcode = OPS_SRAV    }, // Shift Word Right Arithmetic Variable.
    { .name = "JR"     , .fmt =  "\'s"  , .opcode = OPS_JR      }, // Jump Register.
    { .name = "JALR"   , .fmt =  "\'ds" , .opcode = OPS_JALR    }, // Jump and Link Register.
    { .name = "SYSCALL", .fmt =  "\'"   , .opcode = OPS_SYSCALL }, // System Call (assert).
    { .name = "BREAK"  , .fmt =  "\'"   , .opcode = OPS_BREAK   }, // Breakpoint.
    { .name = "SYNC"   , .fmt =  "\'"   , .opcode = OPS_SYNC    }, // Synchronize Shared Memory.
    { .name = "MFHI"   , .fmt =  "\'d"  , .opcode = OPS_MFHI    }, // Move From HI.
    { .name = "MTHI"   , .fmt =  "\'s"  , .opcode = OPS_MTHI    }, // Move To HI.
    { .name = "MFLO"   , .fmt =  "\'d"  , .opcode = OPS_MFLO    }, // Move From LO.
    { .name = "MTLO"   , .fmt =  "\'s"  , .opcode = OPS_MTLO    }, // Move To LO.
    { .name = "DSLLV"  , .fmt =  "\'dts", .opcode = OPS_DSLLV   }, // Doubleword Shift Left Logical Variable.
    { .name = "DSRLV"  , .fmt =  "\'dts", .opcode = OPS_DSRLV   }, // Doubleword Shift Right Logical Variable.
    { .name = "DSRAV"  , .fmt =  "\'dts", .opcode = OPS_DSRAV   }, // Doubleword Shift Right Arithmetic Variable.
    { .name = "MULT"   , .fmt =  "\'st" , .opcode = OPS_MULT    }, // Multiply Word (5cyc).
    { .name = "MULTU"  , .fmt =  "\'st" , .opcode = OPS_MULTU   }, // Multiply Unsigned Word (5cyc).
    { .name = "DIV"    , .fmt =  "\'st" , .opcode = OPS_DIV     }, // Divide Word (37cyc).
    { .name = "DIVU"   , .fmt =  "\'st" , .opcode = OPS_DIVU    }, // Divide Unsigned Word (37cyc).
    { .name = "DMULT"  , .fmt =  "\'st" , .opcode = OPS_DMULT   }, // Doubleword Multiply (8cyc).
    { .name = "DMULTU" , .fmt =  "\'st" , .opcode = OPS_DMULTU  }, // Doubleword Multiply Unsigned (8cyc).
    { .name = "DDIV"   , .fmt =  "\'st" , .opcode = OPS_DDIV    }, // Doubleword Divide (69cyc).
    { .name = "DDIVU"  , .fmt =  "\'st" , .opcode = OPS_DDIVU   }, // Doubleword Divide Unsigned (69cyc).
    { .name = "ADD"    , .fmt =  "\'dst", .opcode = OPS_ADD     }, // Add Word.
    { .name = "ADDU"   , .fmt =  "\'dst", .opcode = OPS_ADDU    }, // Add Unsigned Word.
    { .name = "SUB"    , .fmt =  "\'dst", .opcode = OPS_SUB     }, // Subtract Word.
    { .name = "SUBU"   , .fmt =  "\'dst", .opcode = OPS_SUBU    }, // Subtract Unsigned Word.
    { .name = "AND"    , .fmt =  "\'dst", .opcode = OPS_AND     }, // And.
    { .name = "OR"     , .fmt =  "\'dst", .opcode = OPS_OR      }, // Or.
    { .name = "XOR"    , .fmt =  "\'dst", .opcode = OPS_XOR     }, // Exclusive Or.
    { .name = "NOR"    , .fmt =  "\'dst", .opcode = OPS_NOR     }, // Nor.
    { .name = "SLT"    , .fmt =  "\'dst", .opcode = OPS_SLT     }, // Set on Less Than.
    { .name = "SLTU"   , .fmt =  "\'dst", .opcode = OPS_SLTU    }, // Set on Less Than Unsigned.
    { .name = "DADD"   , .fmt =  "\'dst", .opcode = OPS_DADD    }, // Doubleword Add.
    { .name = "DADDU"  , .fmt =  "\'dst", .opcode = OPS_DADDU   }, // Doubleword Add Unsigned.
    { .name = "DSUB"   , .fmt =  "\'dst", .opcode = OPS_DSUB    }, // Doubleword Subtract.
    { .name = "DSUBU"  , .fmt =  "\'dst", .opcode = OPS_DSUBU   }, // Doubleword Subtract Unsigned.
    { .name = "TGE"    , .fmt =  "\'st" , .opcode = OPS_TGE     }, // Trap if Greater Than or Equal.
    { .name = "TGEU"   , .fmt =  "\'st" , .opcode = OPS_TGEU    }, // Trap if Greater Than or Equal Unsigned.
    { .name = "TLT"    , .fmt =  "\'st" , .opcode = OPS_TLT     }, // Trap if Less Than.
    { .name = "TLTU"   , .fmt =  "\'st" , .opcode = OPS_TLTU    }, // Trap if Less Than Unsigned.
    { .name = "TEQ"    , .fmt =  "\'st" , .opcode = OPS_TEQ     }, // Trap if Equal.
    { .name = "TNE"    , .fmt =  "\'st" , .opcode = OPS_TNE     }, // Trap if Not Equal.
    { .name = "DSLL"   , .fmt =  "\'dta", .opcode = OPS_DSLL    }, // Doubleword Shift Left Logical.
    { .name = "DSRL"   , .fmt =  "\'dta", .opcode = OPS_DSRL    }, // Doubleword Shift Right Logical.
    { .name = "DSRA"   , .fmt =  "\'dta", .opcode = OPS_DSRA    }, // Doubleword Shift Right Arithmetic.
    { .name = "DSLL32" , .fmt =  "\'dta", .opcode = OPS_DSLL32  }, // Doubleword Shift Left Logical + 32.
    { .name = "DSRL32" , .fmt =  "\'dta", .opcode = OPS_DSRL32  }, // Doubleword Shift Right Logical + 32.
    { .name = "DSRA32" , .fmt =  "\'dta", .opcode = OPS_DSRA32  }, // Doubleword Shift Right Arithmetic + 32.
    { .name = "." },
};

// Register opcode instructions:
ALIGNED32 static const InsnTemplate insn_db_regi[] = { // OPC_REGIMM
    { .name = "BLTZ"   , .fmt = "\'sB", .opcode = OPR_BLTZ    }, // Branch on Less Than Zero.
    { .name = "BGEZ"   , .fmt = "\'sB", .opcode = OPR_BGEZ    }, // Branch on Greater Than or Equal to Zero.
    { .name = "BLTZL"  , .fmt = "\'sB", .opcode = OPR_BLTZL   }, // Branch on Less Than Zero Likely.
    { .name = "BGEZL"  , .fmt = "\'sB", .opcode = OPR_BGEZL   }, // Branch on Greater Than or Equal to Zero Likely.
    { .name = "BLTZAL" , .fmt = "\'sB", .opcode = OPR_BLTZAL  }, // Branch on Less Than Zero and Link.
    { .name = "BGEZAL" , .fmt = "\'sB", .opcode = OPR_BGEZAL  }, // Branch on Greater Than or Equal to Zero and Link.
    { .name = "BLTZALL", .fmt = "\'sB", .opcode = OPR_BLTZALL }, // Branch on Less Than Zero and Link Likely.
    { .name = "BGEZALL", .fmt = "\'sB", .opcode = OPR_BGEZALL }, // Branch on Greater Than or Equal to Zero and Link Likely.
    { .name = "TGEI"   , .fmt = "\'sI", .opcode = OPR_TGEI    }, // Trap if Greater Than or Equal Immediate.
    { .name = "TGEIU"  , .fmt = "\'sI", .opcode = OPR_TGEIU   }, // Trap if Greater Than or Equal Unsigned Immediate.
    { .name = "TLTI"   , .fmt = "\'sI", .opcode = OPR_TLTI    }, // Trap if Less Than Immediate.
    { .name = "TLTIU"  , .fmt = "\'sI", .opcode = OPR_TLTIU   }, // Trap if Less Than Unsigned Immediate.
    { .name = "TEQI"   , .fmt = "\'sI", .opcode = OPR_TEQI    }, // Trap if Equal Immediate.
    { .name = "TNEI"   , .fmt = "\'sI", .opcode = OPR_TNEI    }, // Trap if Not Equal Immediate.
    { .name = "." },
};

// Coprocessor-0 (System Control Coprocessor):
ALIGNED32 static const InsnTemplate insn_db_cop0[] = {
    /* sub:00 */ { .name = "MFC0" , .fmt = "\'t0", .opcode = COP0_MF        }, // Move from System Control Coprocessor.
    /* sub:00 */ { .name = "DMFC0", .fmt = "\'t0", .opcode = COP0_DMF       }, // Doubleword Move from System Control Coprocessor.
    /* sub:00 */ { .name = "MTC0" , .fmt = "\'t0", .opcode = COP0_MT        }, // Move to System Control Coprocessor.
    /* sub:00 */ { .name = "DMTC0", .fmt = "\'t0", .opcode = COP0_DMT       }, // Doubleword Move to System Control Coprocessor.
    /* sub:10 */ { .name = "TLBP" , .fmt = "\'"  , .opcode = OPC_COP0_TLBP  }, // Searches for a TLB entry that matches the EntryHi register.
    /* sub:10 */ { .name = "TLBR" , .fmt = "\'"  , .opcode = OPC_COP0_TLBR  }, // Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
    /* sub:10 */ { .name = "TLBWI", .fmt = "\'"  , .opcode = OPC_COP0_TLBWI }, // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register.
    /* sub:10 */ { .name = "TLBWR", .fmt = "\'"  , .opcode = OPC_COP0_TLBWR }, // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register.
    /* sub:10 */ { .name = "ERET" , .fmt = "\'"  , .opcode = OPC_COP0_ERET  }, // Return from interrupt, exception, or error exception.
    { .name = "." },
};

// Coprocessor-1 (Floating-Point Unit):
ALIGNED32 static const InsnTemplate insn_db_cop1[] = {
    /* sub:00 */ { .name = "MFC1"   , .fmt = "\'tS" , .opcode = COP1_FMT_SINGLE }, // Move Word From Floating-Point.
    /* sub:00 */ { .name = "DMFC1"  , .fmt = "\'tS" , .opcode = COP1_FMT_DOUBLE }, // Doubleword Move From Floating-Point.
    /* sub:00 */ { .name = "MTC1"   , .fmt = "\'tS" , .opcode = COP1_FMT_WORD   }, // Move Word To Floating-Point.
    /* sub:00 */ { .name = "DMTC1"  , .fmt = "\'tS" , .opcode = COP1_FMT_LONG   }, // Doubleword Move To Floating-Point.
    /* sub:00 */ { .name = "CFC1"   , .fmt = "\'tS" , .opcode = COP1_FMT_CTL_F  }, // Move Control Word From Floating-Point.
    /* sub:00 */ { .name = "CTC1"   , .fmt = "\'tS" , .opcode = COP1_FMT_CTL_T  }, // Move Control Word To Floating-Point.
    /* sub:01 */ { .name = "BC1F"   , .fmt = "\'B"  , .opcode = OPT_COP1_BC1F   }, // Branch on FP False (1cyc*).
    /* sub:01 */ { .name = "BC1T"   , .fmt = "\'B"  , .opcode = OPT_COP1_BC1T   }, // Branch on FP True (1cyc*).
    /* sub:01 */ { .name = "BC1FL"  , .fmt = "\'B"  , .opcode = OPT_COP1_BC1FL  }, // Branch on FP False Likely (1cyc*).
    /* sub:01 */ { .name = "BC1TL"  , .fmt = "\'B"  , .opcode = OPT_COP1_BC1TL  }, // Branch on FP True Likely (1cyc*).
    /* sub:10 */ { .name = "ADD"    , .fmt = "\"DST", .opcode = OPS_ADD_F       }, // ADD.[FMT]     Floating-Point Add (3cyc).
    /* sub:10 */ { .name = "SUB"    , .fmt = "\"DST", .opcode = OPS_SUB_F       }, // SUB.[FMT]     Floating-Point Subtract (3cyc).
    /* sub:10 */ { .name = "MUL"    , .fmt = "\"DST", .opcode = OPS_MUL_F       }, // MUL.[FMT]     Floating-Point Multiply (S:5cyc; D:8cyc).
    /* sub:10 */ { .name = "DIV"    , .fmt = "\"DST", .opcode = OPS_DIV_F       }, // DIV.[FMT]     Floating-Point Divide (S:29cyc; D:58cyc).
    /* sub:10 */ { .name = "SQRT"   , .fmt = "\"DS" , .opcode = OPS_SQRT_F      }, // SQRT.[FMT]    Floating-Point Square Root (S:29cyc; D:58cyc).
    /* sub:10 */ { .name = "ABS"    , .fmt = "\"DS" , .opcode = OPS_ABS_F       }, // ABS.[FMT]     Floating-Point Absolute Value (1cyc).
    /* sub:10 */ { .name = "MOV"    , .fmt = "\"DS" , .opcode = OPS_MOV_F       }, // MOV.[FMT]     Floating-Point Move (1cyc).
    /* sub:10 */ { .name = "NEG"    , .fmt = "\"DS" , .opcode = OPS_NEG_F       }, // NEG.[FMT]     Floating-Point Negate (1cyc).
    /* sub:10 */ { .name = "ROUND.L", .fmt = "\"DS" , .opcode = OPS_ROUND_L_F   }, // ROUND.L.[FMT] Floating-Point Round to Long Fixed-Point (5cyc).
    /* sub:10 */ { .name = "TRUNC.L", .fmt = "\"DS" , .opcode = OPS_TRUNC_L_F   }, // TRUNC.L.[FMT] Floating-Point Truncate to Long Fixed-Point (5cyc).
    /* sub:10 */ { .name = "CEIL.L" , .fmt = "\"DS" , .opcode = OPS_CEIL_L_F    }, // CEIL.L.[FMT]  Floating-Point Ceiling to Long Fixed-Point (5cyc).
    /* sub:10 */ { .name = "FLOOR.L", .fmt = "\"DS" , .opcode = OPS_FLOOR_L_F   }, // FLOOR.L.[FMT] Floating-Point Floor to Long Fixed-Point (5cyc).
    /* sub:10 */ { .name = "ROUND.W", .fmt = "\"DS" , .opcode = OPS_ROUND_W_F   }, // ROUND.W.[FMT] Floating-Point Round to Word Fixed-Point (5cyc).
    /* sub:10 */ { .name = "TRUNC.W", .fmt = "\"DS" , .opcode = OPS_TRUNC_W_F   }, // TRUNC.W.[FMT] Floating-Point Truncate to Word Fixed-Point (5cyc).
    /* sub:10 */ { .name = "CEIL.W" , .fmt = "\"DS" , .opcode = OPS_CEIL_W_F    }, // CEIL.W.[FMT]  Floating-Point Ceiling to Word Fixed-Point (5cyc).
    /* sub:10 */ { .name = "FLOOR.W", .fmt = "\"DS" , .opcode = OPS_FLOOR_W_F   }, // FLOOR.W.[FMT] Floating-Point Floor to Word Fixed-Point (5cyc).
    /* sub:10 */ { .name = "CVT.S"  , .fmt = "\"DS" , .opcode = OPS_CVT_S_F     }, // CVT.S.[FMT]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc).
    /* sub:10 */ { .name = "CVT.D"  , .fmt = "\"DS" , .opcode = OPS_CVT_D_F     }, // CVT.D.[FMT]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc).
    /* sub:10 */ { .name = "CVT.W"  , .fmt = "\"DS" , .opcode = OPS_CVT_W_F     }, // CVT.W.[FMT]   Floating-Point Convert to Word Fixed-Point (5cyc).
    /* sub:10 */ { .name = "CVT.L"  , .fmt = "\"DS" , .opcode = OPS_CVT_L_F     }, // CVT.L.[FMT]   Floating-Point Convert to Long Fixed-Point (5cyc).
    /* sub:10 */ { .name = "C.F"    , .fmt = "\"ST" , .opcode = OPS_C_F         }, // C.F.[FMT]     Floating-Point Compare (False) (1cyc).
    /* sub:10 */ { .name = "C.UN"   , .fmt = "\"ST" , .opcode = OPS_C_UN        }, // C.UN.[FMT]    Floating-Point Compare (Unordered) (1cyc).
    /* sub:10 */ { .name = "C.EQ"   , .fmt = "\"ST" , .opcode = OPS_C_EQ        }, // C.EQ.[FMT]    Floating-point Compare (Equal) (1cyc).
    /* sub:10 */ { .name = "C.UEQ"  , .fmt = "\"ST" , .opcode = OPS_C_UEQ       }, // C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc).
    /* sub:10 */ { .name = "C.OLT"  , .fmt = "\"ST" , .opcode = OPS_C_OLT       }, // C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc).
    /* sub:10 */ { .name = "C.ULT"  , .fmt = "\"ST" , .opcode = OPS_C_ULT       }, // C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc).
    /* sub:10 */ { .name = "C.OLE"  , .fmt = "\"ST" , .opcode = OPS_C_OLE       }, // C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc).
    /* sub:10 */ { .name = "C.ULE"  , .fmt = "\"ST" , .opcode = OPS_C_ULE       }, // C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc).
    /* sub:10 */ { .name = "C.SF"   , .fmt = "\"ST" , .opcode = OPS_C_SF        }, // C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc).
    /* sub:10 */ { .name = "C.NGLE" , .fmt = "\"ST" , .opcode = OPS_C_NGLE      }, // C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc).
    /* sub:10 */ { .name = "C.SEQ"  , .fmt = "\"ST" , .opcode = OPS_C_SEQ       }, // C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc).
    /* sub:10 */ { .name = "C.NGL"  , .fmt = "\"ST" , .opcode = OPS_C_NGL       }, // C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc).
    /* sub:10 */ { .name = "C.LT"   , .fmt = "\"ST" , .opcode = OPS_C_LT        }, // C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc).
    /* sub:10 */ { .name = "C.NGE"  , .fmt = "\"ST" , .opcode = OPS_C_NGE       }, // C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc).
    /* sub:10 */ { .name = "C.LE"   , .fmt = "\"ST" , .opcode = OPS_C_LE        }, // C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc).
    /* sub:10 */ { .name = "C.NGT"  , .fmt = "\"ST" , .opcode = OPS_C_NGT       }, // C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc).
    { .name = "." },
};

// Coprocessor-2 (Reality Co-Processor Vector Unit):
ALIGNED32 static const InsnTemplate insn_db_cop2[] = {
    { .name = "." },
};

// Coprocessor-3 (CP3):
ALIGNED32 static const InsnTemplate insn_db_cop3[] = {
    { .name = "." },
};

static const InsnTemplate* insn_db_cop_lists[] = {
    [COP0] = insn_db_cop0,
    [COP1] = insn_db_cop1,
    [COP2] = insn_db_cop2,
    [COP3] = insn_db_cop3,
};

// Pseudo-instructions
ALIGNED32 static const InsnTemplate insn_db_pseudo[] = {
    [PSEUDO_NOP  ] = { .name = "NOP"  , .fmt = "_"    , .opcode = OPS_SLL   }, // NOP (pseudo of SLL).
    [PSEUDO_MOVET] = { .name = "MOVE" , .fmt = "\'dt" , .opcode = OPS_ADD   }, // Move (pseudo of ADD and OR).
    [PSEUDO_MOVES] = { .name = "MOVE" , .fmt = "\'ds" , .opcode = OPS_ADD   }, // Move (pseudo of ADD).
    [PSEUDO_B    ] = { .name = "B"    , .fmt = "\'B"  , .opcode = OPC_BEQ   }, // Branch (pseudo of BEQ).
    [PSEUDO_BEQZ ] = { .name = "BEQZ" , .fmt = "\'sB" , .opcode = OPC_BEQ   }, // Branch on Equal to Zero (pseudo of BEQ).
    [PSEUDO_BNEZ ] = { .name = "BNEZ" , .fmt = "\'sB" , .opcode = OPC_BNE   }, // Branch on Not Equal to Zero (pseudo of BNE).
    [PSEUDO_LI   ] = { .name = "LI"   , .fmt = "\'tI" , .opcode = OPC_ADDI  }, // Load Immediate (pseudo of ADDI and ADDIU).
    [PSEUDO_SUBI ] = { .name = "SUBI" , .fmt = "\'tsi", .opcode = OPC_ADDI  }, // Subtract Immediate Word (pseudo of ADDI).
    [PSEUDO_BEQZL] = { .name = "BEQZL", .fmt = "\'sB" , .opcode = OPC_BEQL  }, // Branch on Equal to Zero Likely (pseudo of BEQL).
    [PSEUDO_BNEZL] = { .name = "BNEZL", .fmt = "\'sB" , .opcode = OPC_BNEL  }, // Branch on Not Equal to Zero Likely (pseudo of BNEL).
    [PSEUDO_DSUBI] = { .name = "DSUBI", .fmt = "\'tsi", .opcode = OPC_DADDI }, // Doubleword Subtract Immediate (pseudo of DADDI).
};

static _Bool check_pseudo_insn(const InsnTemplate** type, enum PseudoInsns id, _Bool cond) {
    if (cond) {
        *type = &insn_db_pseudo[id];
        return TRUE;
    }

    return FALSE;
}

static _Bool check_pseudo_instructions(const InsnTemplate** type, InsnData insn) {
    // NOP (trivial case)
    if (check_pseudo_insn(type, PSEUDO_NOP, (insn.raw == 0))) return TRUE;

    // There are no known one-line Coprocessor pseudoinstructions.
    if (insn.cop_opcode == COP_OPCODE) {
        return FALSE;
    }

    switch (insn.opcode) {
        case OPC_SPECIAL:
            switch (insn.func) {
                case OPS_ADD:
                    if (check_pseudo_insn(type, PSEUDO_MOVES, (insn.rt == 0))) return TRUE;
                    if (check_pseudo_insn(type, PSEUDO_MOVET, (insn.rs == 0))) return TRUE;
                    break;
                case OPS_OR:
                    if (check_pseudo_insn(type, PSEUDO_MOVES, (insn.rt == 0))) return TRUE;
            }
            break;
        case OPC_BEQ:
            if (check_pseudo_insn(type, PSEUDO_B,     (insn.rs == insn.rt))) return TRUE;
            if (check_pseudo_insn(type, PSEUDO_BEQZ,  (insn.rt == 0))) return TRUE;
            break;
        case OPC_BNE:
            if (check_pseudo_insn(type, PSEUDO_BNEZ,  (insn.rt == 0))) return TRUE;
            break;
        case OPC_ADDI:
            if (check_pseudo_insn(type, PSEUDO_LI,    (insn.rs == 0))) return TRUE;
            if (check_pseudo_insn(type, PSEUDO_SUBI,  ((s16)insn.immediate < 0))) return TRUE;
            break;
        case OPC_ADDIU:
            if (check_pseudo_insn(type, PSEUDO_LI,    (insn.rs == 0))) return TRUE;
            break;
        case OPC_BEQL:
            if (check_pseudo_insn(type, PSEUDO_BEQZL, (insn.rt == 0))) return TRUE;
            break;
        case OPC_BNEL:
            if (check_pseudo_insn(type, PSEUDO_BNEZL, (insn.rt == 0))) return TRUE;
            break;
        case OPC_DADDI:
            if (check_pseudo_insn(type, PSEUDO_DSUBI, ((s16)insn.immediate < 0))) return TRUE;
            break;
    }

    return FALSE;
}

static enum InsnType get_insn_type_and_list(InsnData insn, const InsnTemplate** checkInsn) {
    enum InsnType insnType = INSN_TYPE_OPCODE;
    *checkInsn = insn_db;
    
    if (insn.cop_opcode == COP_OPCODE) { // COPz
        if (insn.cop_num < ARRAY_COUNT(insn_db_cop_lists)) {
            *checkInsn = insn_db_cop_lists[insn.cop_num];
            switch (insn.cop_subtype) {
                case 0b00: insnType = INSN_TYPE_COP_FMT; break;
                case 0b01: insnType = INSN_TYPE_REGIMM;  break;
                default:   insnType = INSN_TYPE_FUNC;    break;
            }
        } else {
            return INSN_TYPE_ERROR;
        }
    } else {
        switch (insn.opcode) {
            case OPC_SPECIAL:
                *checkInsn = insn_db_spec;
                insnType = INSN_TYPE_FUNC;
                break;
            case OPC_REGIMM:
                *checkInsn = insn_db_regi;
                insnType = INSN_TYPE_REGIMM;
                break;
        }
    }

    return insnType;
}

const InsnTemplate* get_insn(InsnData insn) { //! TODO: Optimize this
    const InsnTemplate* checkInsn = NULL;

    if (check_pseudo_instructions(&checkInsn, insn)) {
        return checkInsn;
    }

    enum InsnType insnType = get_insn_type_and_list(insn, &checkInsn);

    if (insnType == INSN_TYPE_ERROR) {
        return NULL;
    }

    u8 check = 0;

    while (checkInsn->name[0] != '.') {
        switch (insnType) {
            default:
            case INSN_TYPE_OPCODE:  check = insn.opcode; break; // First 6 bits
            case INSN_TYPE_FUNC:    check = insn.func;   break; // Last 6 bits
            case INSN_TYPE_REGIMM:  check = insn.regimm; break; // The 5 bits after the first 11
            case INSN_TYPE_COP_FMT: check = insn.fmt;    break; // The 3 bits after the first 8
        }

        if (check == checkInsn->opcode) {
            return checkInsn;
        }

        checkInsn++;
    }

    return NULL;
}

// Registers
static const char sCPURegisterNames[][3] = {
    "R0",                                           // $zero.
    "AT",                                           // Assembler temporary value.
    "V0", "V1",                                     // Subroutine return value.
    "A0", "A1", "A2", "A3",                         // Subroutine arguments.
    "T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", // Temporary values.
    "S0", "S1", "S2", "S3", "S4", "S5", "S6", "S7", // Saved values.
    "T8", "T9",                                     // Temporary values.
    "K0", "K1",                                     // Reserved by kernel.
    "GP",                                           // Global pointer.
    "SP",                                           // Stack pointer.
    "FP",                                           // Saved value or frame pointer.
    "RA",                                           // Return address.
}; //! TODO: Combine this with sRegNames

static const char sCOP0RegisterNames[][9] = {
    [ 0] = "Index",                         // Programmable pointer into TLB array.
    [ 1] = "Random",                        // Pseudorandom pointer into TLB array (read only).
    [ 2] = "EntryLo0",                      // Low half of TLB entry for even virtual addresses (VPN).
    [ 3] = "EntryLo1",                      // Low half of TLB entry for odd virtual addresses (VPN).
    [ 4] = "Context",                       // Pointer to kernel virtual page table entry (PTE) in 32-bit mode.
    [ 5] = "PageMask",                      // Page size specification.
    [ 6] = "Wired",                         // Number of wired TLB entries.
    [ 7] = "7",                             // Reserved for future use.
    [ 8] = "BadVAddr",                      // Display of virtual address that occurred an error last.
    [ 9] = "Count",                         // Timer Count.
    [10] = "EntryHi",                       // High half of TLB entry (including ASID).
    [11] = "Compare",                       // Timer Compare Value.
    [12] = "Status",                        // Operation status setting.
    [13] = "Cause",                         // Display of cause of last exception.
    [14] = "EPC",                           // Exception Program Counter.
    [15] = "PRId",                          // Processor Revision Identifier.
    [16] = "Config",                        // Memory system mode setting.
    [17] = "LLAddr",                        // Load Linked instruction address display.
    [18] = "WatchLo",                       // Memory reference trap address low bits.
    [19] = "WatchHi",                       // Memory reference trap address high bits.
    [20] = "XContext",                      // Pointer to Kernel virtual PTE table in 64-bit mode.
    [21] = "21", "22", "23", "24", "25",    // Reserved for future use.
    [26] = "ParityErr",                     // Cache parity bits.
    [27] = "CacheErr",                      // Cache Error and Status register.
    [28] = "TagLo",                         // Cache Tag register low.
    [29] = "TagHi",                         // Cache Tag register high.
    [30] = "ErrorEPC",                      // Error Exception Program Counter.
    [31] = "31",                            // Reserved for future use.
};

// // FPU Registers
// static const char sCOP1RegisterNames[][4] = {
//     "F00", "F02",                               // Subroutine return value.
//     "F04", "F06", "F08", "F10",                 // Temporary values.
//     "F12", "F14",                               // Subroutine arguments.
//     "F16", "F18",                               // Temporary values.
//     "F20", "F22", "F24", "F26", "F28", "F30",   // Saved Values.
// };

// Checks an instruction for its branch offset. Returns 0 if there is no branch.
s16 check_for_branch_offset(InsnData insn) {
    const InsnTemplate* info = get_insn(insn);

    if (info == NULL) {
        return 0x0000;
    }

    for (int i = 0; i < 4; i++) {
        if (info->fmt[i] == CHAR_P_BRANCH) {
            return insn.offset;
        }
    }

    return 0x0000;
}

uintptr_t get_branch_target_from_addr(uintptr_t addr) {
    if (!is_in_code_segment(addr)) {
        return addr;
    }

    InsnData insn = { .raw = *(uintptr_t*)addr };

    if (insn.opcode == OPC_J || insn.opcode == OPC_JAL) {
        return PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(uintptr_t));
    }

    s16 branchOffset = check_for_branch_offset(insn);

    if (branchOffset) {
        return (addr + (((s16)branchOffset + 1) * sizeof(uintptr_t)));
    }

    return addr;
}

static char cop1_fmt_to_char(InsnData insn) {
    switch (insn.fmt) {
        case COP1_FMT_SINGLE: return 'S'; // Single
        case COP1_FMT_DOUBLE: return 'D'; // Double
        case COP1_FMT_WORD:   return 'W'; // Word
        case COP1_FMT_LONG:   return 'L'; // Long
    }

    return 'X'; // Unknown
}

// Print color code if the color has changed.
static void check_color_change(char** strp, RGBA32* oldColor, RGBA32 newColor) {
    if (*oldColor != newColor) {
        *oldColor = newColor;
        *strp += sprintf(*strp, STR_COLOR_PREFIX, newColor);
    }
}

static char insn_as_string[CHAR_BUFFER_SIZE] = "";

#define STR_INSN_NAME_BASE      "%s"
#define STR_FORMAT              "%c"
#define STR_CONDITION           "%s"

#define STR_INSN_NAME           "%-"TO_STRING2(INSN_NAME_DISPLAY_WIDTH)"s"
#define STR_INSN_NAME_FORMAT    STR_INSN_NAME_BASE"."STR_FORMAT

#define STR_IREG            "%s"                           // Register
#define STR_IMMEDIATE       STR_HEX_PREFIX STR_HEX_HALFWORD // 0xI
#define STR_OFFSET          "%c"STR_IMMEDIATE               // ±Offset
#define STR_FUNCTION        STR_HEX_PREFIX STR_HEX_WORD     // Function address
#define STR_IREG_BASE       "("STR_IREG")"                  // Base register
#define STR_FREG            "F%02d"                         // Float Register

char* insn_disasm(InsnData insn, const char** fname, _Bool showDestNames) {
    char* strp = &insn_as_string[0];
    _Bool unimpl = FALSE;
    char insn_name[INSN_NAME_DISPLAY_WIDTH] = "";

    bzero(insn_as_string, sizeof(insn_as_string));

    const InsnTemplate* info = get_insn(insn);

    if (info != NULL) {
        RGBA32 color = COLOR_RGBA32_NONE;

        for (u8 cmdIndex = 0; cmdIndex < sizeof(u32); cmdIndex++) {
            char curCmd = info->fmt[cmdIndex];

            if (unimpl || curCmd == CHAR_P_NULL) {
                break;
            }

            //! TODO: commas between registers (check next byte?)
            switch (curCmd) {
                case CHAR_P_NOP:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_NOP);
                    strp += sprintf(strp, info->name);
                    return insn_as_string;
                case CHAR_P_NAME:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_INSN);
                    strp += sprintf(strp, STR_INSN_NAME, info->name);
                    break;
                case CHAR_P_NAMEF:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_INSN);
                    bzero(insn_name, sizeof(insn_name));
                    sprintf(insn_name, STR_INSN_NAME_FORMAT, info->name, cop1_fmt_to_char(insn));
                    strp += sprintf(strp, STR_INSN_NAME, insn_name);
                    break;
                case CHAR_P_RS:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sCPURegisterNames[insn.rs]);
                    break;
                case CHAR_P_RT:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sCPURegisterNames[insn.rt]);
                    break;
                case CHAR_P_RD:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sCPURegisterNames[insn.rd]);
                    break;
                case CHAR_P_IMM:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, insn.immediate);
                    break;
                case CHAR_P_NIMM:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, -(s16)insn.immediate);
                    break;
                case CHAR_P_SHIFT:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, insn.sa);
                    break;
                case CHAR_P_BASE:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_BASE_REG);
                    strp += sprintf(strp, STR_IREG_BASE, sCPURegisterNames[insn.base]);
                    break;
                case CHAR_P_BRANCH:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_FUNCTION_NAME_2);
                    s16 branchOffset = (insn.offset + 1);
                    strp += sprintf(strp, STR_OFFSET, ((branchOffset < 0x0000) ? '-' : '+'), abss(branchOffset)); //! TODO: Use '%+' format specifier if possible.
                    break;
                case CHAR_P_COP0D:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sCOP0RegisterNames[insn.rd]);
                    break;
                case CHAR_P_FT:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_FREG" ", insn.ft);
                    break;
                case CHAR_P_FS:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_FREG" ", insn.fs);
                    break;
                case CHAR_P_FD:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_FREG" ", insn.fd);
                    break;
                case CHAR_P_FUNC:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_FUNCTION_NAME);
                    uintptr_t target = PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(uintptr_t));
#ifdef INCLUDE_DEBUG_MAP
                    if (showDestNames) {
                        *fname = parse_map_exact(target);

                        if (*fname != NULL) {
                            break;
                        }
                    }
#endif
                    strp += sprintf(strp, STR_FUNCTION, target);
                    break;
                default:
                    unimpl = TRUE;
                    break;
            }
        }
    } else {
        unimpl = TRUE;
    }

    if (unimpl) { //! TODO: binary mode for these
        strp += sprintf(strp, ("unimpl "STR_HEX_WORD), insn.raw);
    }

    return insn_as_string;
}
