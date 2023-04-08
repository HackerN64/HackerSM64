#include <PR/ultratypes.h>
#include <string.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "color_presets.h"
#include "crash_screen.h"
#include "crash_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
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
 *  - if 0 (OPC_SPEC):
 *   - compare the last 6 bits (func) to insn_db_spec
 *  - otherwise, if 1 (OPC_REGI):
 *   - skip the next 5 bits and compare the 5 bits after that (regimm)
 *  - otherwise, compare the first 6 bits to insn_db
 */


// MIPS III Instructions

// Opcode instructions
ALIGNED32 static const InsnTemplate insn_db[] = {
    // OPC_SPEC (insn_db_spec)
    // OPC_REGI (insn_db_regi)
    INSN_ID_1(OPC_J     , 0, 0, 0, 0, 0, "\'J"  , "J"      ) // Jump
    INSN_ID_1(OPC_JAL   , 0, 0, 0, 0, 0, "\'J"  , "JAL"    ) // Jump and Link
    INSN_ID_1(OPC_BEQ   , 0, 0, 0, 0, 0, "\'stB", "BEQ"    ) // Branch on Equal
    INSN_ID_1(OPC_BNE   , 0, 0, 0, 0, 0, "\'stB", "BNE"    ) // Branch on Not Equal
    INSN_ID_1(OPC_BLEZ  , 0, 0, 0, 0, 0, "\'sB" , "BLEZ"   ) // Branch on Less Than or Equal to Zero
    INSN_ID_1(OPC_BGTZ  , 0, 0, 0, 0, 0, "\'sB" , "BGTZ"   ) // Branch on Greater Than Zero
    INSN_ID_1(OPC_ADDI  , 0, 0, 0, 0, 0, "\'tsI", "ADDI"   ) // Add Immediate Word
    INSN_ID_1(OPC_ADDIU , 0, 0, 0, 0, 0, "\'tsI", "ADDIU"  ) // Add Immediate Unsigned Word
    INSN_ID_1(OPC_SLTI  , 0, 0, 0, 0, 0, "\'tsI", "SLTI"   ) // Set on Less Than Immediate
    INSN_ID_1(OPC_SLTIU , 0, 0, 0, 0, 0, "\'tsI", "SLTIU"  ) // Set on Less Than Immediate Unsigned
    INSN_ID_1(OPC_ANDI  , 0, 0, 0, 0, 0, "\'tsI", "ANDI"   ) // And Immediate
    INSN_ID_1(OPC_ORI   , 0, 0, 0, 0, 0, "\'tsI", "ORI"    ) // Or Immediate
    INSN_ID_1(OPC_XORI  , 0, 0, 0, 0, 0, "\'tsI", "XORI"   ) // Exclusive Or Immediate
    INSN_ID_1(OPC_LUI   , 0, 0, 0, 0, 0, "\'tI" , "LUI"    ) // Load Upper Immediate
    // OPC_COP0 (insn_db_cop0)
    // OPC_COP1 (insn_db_cop1)
    // OPC_COP2 (insn_db_cop2)
    // OPC_COP3 (insn_db_cop3)
    INSN_ID_1(OPC_BEQL  , 0, 0, 0, 0, 0, "\'stB", "BEQL"   ) // Branch on Equal Likely
    INSN_ID_1(OPC_BNEL  , 0, 0, 0, 0, 0, "\'stB", "BNEL"   ) // Branch on Not Equal Likely
    INSN_ID_1(OPC_BLEZL , 0, 0, 0, 0, 0, "\'sB" , "BLEZL"  ) // Branch on Less Than or Equal to Zero Likely
    INSN_ID_1(OPC_BGTZL , 0, 0, 0, 0, 0, "\'sB" , "BGTZL"  ) // Branch on Greater Than Zero Likely
    INSN_ID_0(OPC_DADDI , 0, 0, 0, 0, 0, "\'tsI", "DADDI"  ) // Doubleword Add Immediate
    INSN_ID_0(OPC_DADDIU, 0, 0, 0, 0, 0, "\'tsI", "DADDIU" ) // Doubleword Add Immediate Unsigned
    INSN_ID_0(OPC_LDL   , 0, 0, 0, 0, 0, "\'tI(", "LDL"    ) // Load Doubleword Left
    INSN_ID_0(OPC_LDR   , 0, 0, 0, 0, 0, "\'tI(", "LDR"    ) // Load Doubleword Right
    INSN_ID_1(OPC_LB    , 0, 0, 0, 0, 0, "\'tI(", "LB"     ) // Load Byte
    INSN_ID_1(OPC_LH    , 0, 0, 0, 0, 0, "\'tI(", "LH"     ) // Load Halfword
    INSN_ID_1(OPC_LWL   , 0, 0, 0, 0, 0, "\'tI(", "LWL"    ) // Load Word Left
    INSN_ID_1(OPC_LW    , 0, 0, 0, 0, 0, "\'tI(", "LW"     ) // Load Word
    INSN_ID_1(OPC_LBU   , 0, 0, 0, 0, 0, "\'tI(", "LBU"    ) // Load Byte Unsigned
    INSN_ID_1(OPC_LHU   , 0, 0, 0, 0, 0, "\'tI(", "LHU"    ) // Load Halfword Unsigned
    INSN_ID_1(OPC_LWR   , 0, 0, 0, 0, 0, "\'tI(", "LWR"    ) // Load Word Right
    INSN_ID_0(OPC_LWU   , 0, 0, 0, 0, 0, "\'tI(", "LWU"    ) // Load Word Unsigned
    INSN_ID_1(OPC_SB    , 0, 0, 0, 0, 0, "\'tI(", "SB"     ) // Store Byte
    INSN_ID_1(OPC_SH    , 0, 0, 0, 0, 0, "\'tI(", "SH"     ) // Store Halfword
    INSN_ID_0(OPC_SWL   , 0, 0, 0, 0, 0, "\'tI(", "SWL"    ) // Store Word Left
    INSN_ID_1(OPC_SW    , 0, 0, 0, 0, 0, "\'tI(", "SW"     ) // Store Word
    INSN_ID_0(OPC_SDL   , 0, 0, 0, 0, 0, "\'tI(", "SDL"    ) // Store Doubleword Left
    INSN_ID_0(OPC_SDR   , 0, 0, 0, 0, 0, "\'tI(", "SDR"    ) // Store Doubleword Right
    INSN_ID_0(OPC_SWR   , 0, 0, 0, 0, 0, "\'tI(", "SWR"    ) // Store Word Right
    INSN_ID_0(OPC_CACHE , 0, 0, 0, 0, 0, "\'tI(", "CACHE"  ) // https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html
    INSN_ID_0(OPC_LL    , 0, 0, 0, 0, 0, "\'tI(", "LL"     ) // Load Linked Word
    INSN_ID_1(OPC_LWC1  , 0, 0, 0, 0, 0, "\'TI(", "LWC1"   ) // Load Word to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_LWC2  , 0, 0, 0, 0, 0, "\'TI(", "LWC2"   ) // Load Word to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_LWC3  , 0, 0, 0, 0, 0, "\'TI(", "LWC3"   ) // Load Word to Coprocessor-3 (COP3)
    INSN_ID_0(OPC_LLD   , 0, 0, 0, 0, 0, "\'tI(", "LLD"    ) // Load Linked Doubleword
    INSN_ID_1(OPC_LDC1  , 0, 0, 0, 0, 0, "\'tI(", "LDC1"   ) // Load Doubleword to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_LDC2  , 0, 0, 0, 0, 0, "\'tI(", "LDC2"   ) // Load Doubleword to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_LD    , 0, 0, 0, 0, 0, "\'tI(", "LD"     ) // Load Doubleword
    INSN_ID_0(OPC_SC    , 0, 0, 0, 0, 0, "\'tI(", "SC"     ) // Store Conditional Word
    INSN_ID_1(OPC_SWC1  , 0, 0, 0, 0, 0, "\'TI(", "SWC1"   ) // Store Word to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_SWC2  , 0, 0, 0, 0, 0, "\'TI(", "SWC2"   ) // Store Word to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_SWC3  , 0, 0, 0, 0, 0, "\'TI(", "SWC3"   ) // Store Word to Coprocessor-3 (COP3)
    INSN_ID_0(OPC_SCD   , 0, 0, 0, 0, 0, "\'tI(", "SCD"    ) // Store Conditional Doubleword
    INSN_ID_1(OPC_SDC1  , 0, 0, 0, 0, 0, "\'tI(", "SDC1"   ) // Store Doubleword to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_SDC2  , 0, 0, 0, 0, 0, "\'tI(", "SDC2"   ) // Store Doubleword to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_SD    , 0, 0, 0, 0, 0, "\'tI(", "SD"     ) // Store Doubleword
    INSN_DB_END
};

// Special opcode instructions
ALIGNED32 static const InsnTemplate insn_db_spec[] = {
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SLL    , "\'dta"  , "SLL"    ) // Shift Word Left Logical
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SRL    , "\'dta"  , "SRL"    ) // Shift Word Right Logical
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SRA    , "\'dta"  , "SRA"    ) // Shift Word Right Arithmetic
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SLLV   , "\'dts"  , "SLLV"   ) // Shift Word Left Logical Variable
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SRLV   , "\'dts"  , "SRLV"   ) // Shift Word Right Logical Variable
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SRAV   , "\'dts"  , "SRAV"   ) // Shift Word Right Arithmetic Variable
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_JR     , "\'s"    , "JR"     ) // Jump Register
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_JALR   , "\'ds"   , "JALR"   ) // Jump and Link Register
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SYSCALL, "\'"     , "SYSCALL") // System Call (assert)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_BREAK  , "\'"     , "BREAK"  ) // Breakpoint
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_SYNC   , "\'"     , "SYNC"   ) // Synchronize Shared Memory
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MFHI   , "\'d"    , "MFHI"   ) // Move From HI
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MTHI   , "\'s"    , "MTHI"   ) // Move To HI
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MFLO   , "\'d"    , "MFLO"   ) // Move From LO
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MTLO   , "\'s"    , "MTLO"   ) // Move To LO
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSLLV  , "\'dts"  , "DSLLV"  ) // Doubleword Shift Left Logical Variable
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRLV  , "\'dts"  , "DSRLV"  ) // Doubleword Shift Right Logical Variable
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRAV  , "\'dts"  , "DSRAV"  ) // Doubleword Shift Right Arithmetic Variable
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MULT   , "\'st"   , "MULT"   ) // Multiply Word (5cyc)
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_MULTU  , "\'st"   , "MULTU"  ) // Multiply Unsigned Word (5cyc)
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_DIV    , "\'st"   , "DIV"    ) // Divide Word (37cyc)
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_DIVU   , "\'st"   , "DIVU"   ) // Divide Unsigned Word (37cyc)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DMULT  , "\'st"   , "DMULT"  ) // Doubleword Multiply (8cyc)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DMULTU , "\'st"   , "DMULTU" ) // Doubleword Multiply Unsigned (8cyc)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DDIV   , "\'st"   , "DDIV"   ) // Doubleword Divide (69cyc)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DDIVU  , "\'st"   , "DDIVU"  ) // Doubleword Divide Unsigned (69cyc)
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_ADD    , "\'dst"  , "ADD"    ) // Add Word
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_ADDU   , "\'dst"  , "ADDU"   ) // Add Unsigned Word
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SUB    , "\'dst"  , "SUB"    ) // Subtract Word
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SUBU   , "\'dst"  , "SUBU"   ) // Subtract Unsigned Word
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_AND    , "\'dst"  , "AND"    ) // And
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_OR     , "\'dst"  , "OR"     ) // Or
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_XOR    , "\'dst"  , "XOR"    ) // Exclusive Or
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_NOR    , "\'dst"  , "NOR"    ) // Nor
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SLT    , "\'dst"  , "SLT"    ) // Set on Less Than
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_SLTU   , "\'dst"  , "SLTU"   ) // Set on Less Than Unsigned
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DADD   , "\'dst"  , "DADD"   ) // Doubleword Add
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DADDU  , "\'dst"  , "DADDU"  ) // Doubleword Add Unsigned
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSUB   , "\'dst"  , "DSUB"   ) // Doubleword Subtract
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSUBU  , "\'dst"  , "DSUBU"  ) // Doubleword Subtract Unsigned
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_TGE    , "\'st"   , "TGE"    ) // Trap if Greater Than or Equal
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_TGEU   , "\'st"   , "TGEU"   ) // Trap if Greater Than or Equal Unsigned
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_TLT    , "\'st"   , "TLT"    ) // Trap if Less Than
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_TLTU   , "\'st"   , "TLTU"   ) // Trap if Less Than Unsigned
    INSN_ID_1(OPC_SPEC, 0, 0, 0, 0, OPS_TEQ    , "\'st"   , "TEQ"    ) // Trap if Equal
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_TNE    , "\'st"   , "TNE"    ) // Trap if Not Equal
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSLL   , "\'dta"  , "DSLL"   ) // Doubleword Shift Left Logical
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRL   , "\'dta"  , "DSRL"   ) // Doubleword Shift Right Logical
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRA   , "\'dta"  , "DSRA"   ) // Doubleword Shift Right Arithmetic
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSLL32 , "\'dta"  , "DSLL32" ) // Doubleword Shift Left Logical + 32
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRL32 , "\'dta"  , "DSRL32" ) // Doubleword Shift Right Logical + 32
    INSN_ID_0(OPC_SPEC, 0, 0, 0, 0, OPS_DSRA32 , "\'dta"  , "DSRA32" ) // Doubleword Shift Right Arithmetic + 32
    INSN_DB_END
};

// Register opcode instructions
ALIGNED32 static const InsnTemplate insn_db_regi[] = {
    INSN_ID_1(OPC_REGI, 0, OPR_BLTZ   , 0, 0, 0, "\'sB"  ,  "BLTZ"   ) // Branch on Less Than Zero
    INSN_ID_1(OPC_REGI, 0, OPR_BGEZ   , 0, 0, 0, "\'sB"  ,  "BGEZ"   ) // Branch on Greater Than or Equal to Zero
    INSN_ID_1(OPC_REGI, 0, OPR_BLTZL  , 0, 0, 0, "\'sB"  ,  "BLTZL"  ) // Branch on Less Than Zero Likely
    INSN_ID_1(OPC_REGI, 0, OPR_BGEZL  , 0, 0, 0, "\'sB"  ,  "BGEZL"  ) // Branch on Greater Than or Equal to Zero Likely
    INSN_ID_0(OPC_REGI, 0, OPR_BLTZAL , 0, 0, 0, "\'sB"  ,  "BLTZAL" ) // Branch on Less Than Zero and Link
    INSN_ID_0(OPC_REGI, 0, OPR_BGEZAL , 0, 0, 0, "\'sB"  ,  "BGEZAL" ) // Branch on Greater Than or Equal to Zero and Link
    INSN_ID_0(OPC_REGI, 0, OPR_BLTZALL, 0, 0, 0, "\'sB"  ,  "BLTZALL") // Branch on Less Than Zero and Link Likely
    INSN_ID_0(OPC_REGI, 0, OPR_BGEZALL, 0, 0, 0, "\'sB"  ,  "BGEZALL") // Branch on Greater Than or Equal to Zero and Link Likely
    INSN_ID_0(OPC_REGI, 0, OPR_TGEI   , 0, 0, 0, "\'sI"  ,  "TGEI"   ) // Trap if Greater Than or Equal Immediate
    INSN_ID_0(OPC_REGI, 0, OPR_TGEIU  , 0, 0, 0, "\'sI"  ,  "TGEIU"  ) // Trap if Greater Than or Equal Unsigned Immediate
    INSN_ID_0(OPC_REGI, 0, OPR_TLTI   , 0, 0, 0, "\'sI"  ,  "TLTI"   ) // Trap if Less Than Immediate
    INSN_ID_0(OPC_REGI, 0, OPR_TLTIU  , 0, 0, 0, "\'sI"  ,  "TLTIU"  ) // Trap if Less Than Unsigned Immediate
    INSN_ID_0(OPC_REGI, 0, OPR_TEQI   , 0, 0, 0, "\'sI"  ,  "TEQI"   ) // Trap if Equal Immediate
    INSN_ID_0(OPC_REGI, 0, OPR_TNEI   , 0, 0, 0, "\'sI"  ,  "TNEI"   ) // Trap if Not Equal Immediate
    INSN_DB_END
};

// Coprocessor-0 (System Control Coprocessor)
ALIGNED32 static const InsnTemplate insn_db_cop0[] = {
    INSN_ID_0(OPC_COP0, COP0_MF , 0, 0, 0, 0             , "\'td", "MFC0"   ) // Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0, COP0_DMF, 0, 0, 0, 0             , "\'td", "DMFC0"  ) // Doubleword Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0, COP0_MT , 0, 0, 0, 0             , "\'td", "MTC0"   ) // Move to System Control Coprocessor
    INSN_ID_0(OPC_COP0, COP0_DMT, 0, 0, 0, 0             , "\'td", "DMTC0"  ) // Doubleword Move to System Control Coprocessor
    INSN_ID_0(OPC_COP0, 0b10000 , 0, 0, 0, OPC_COP0_TLBP , "\'"  , "TLBP"   ) // Searches for a TLB entry that matches the EntryHi register
    INSN_ID_0(OPC_COP0, 0b10000 , 0, 0, 0, OPC_COP0_TLBR , "\'"  , "TLBR"   ) // Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0, 0b10000 , 0, 0, 0, OPC_COP0_TLBWI, "\'"  , "TLBWI"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0, 0b10000 , 0, 0, 0, OPC_COP0_TLBWR, "\'"  , "TLBWR"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register
    INSN_ID_0(OPC_COP0, 0b10000 , 0, 0, 0, OPC_COP0_ERET , "\'"  , "ERET"   ) // Return from interrupt, exception, or error exception
    INSN_DB_END
};

// Coprocessor-1 (Floating-Point Unit)
ALIGNED32 static const InsnTemplate insn_db_cop1[] = {
    INSN_ID_1(OPC_COP1, COP1_FMT_SINGLE, 0             , 0, 0, 0            , "\'tS" , "MFC1"   ) // Move Word From Floating-Point
    INSN_ID_0(OPC_COP1, COP1_FMT_DOUBLE, 0             , 0, 0, 0            , "\'tS" , "DMFC1"  ) // Doubleword Move From Floating-Point
    INSN_ID_1(OPC_COP1, COP1_FMT_WORD  , 0             , 0, 0, 0            , "\'tS" , "MTC1"   ) // Move Word To Floating-Point
    INSN_ID_0(OPC_COP1, COP1_FMT_LONG  , 0             , 0, 0, 0            , "\'tS" , "DMTC1"  ) // Doubleword Move To Floating-Point
    INSN_ID_0(OPC_COP1, COP1_FMT_CTL_F , 0             , 0, 0, 0            , "\'tS" , "CFC1"   ) // Move Control Word From Floating-Point
    INSN_ID_0(OPC_COP1, COP1_FMT_CTL_T , 0             , 0, 0, 0            , "\'tS" , "CTC1"   ) // Move Control Word To Floating-Point
    INSN_ID_1(OPC_COP1, 0b01000        , OPT_COP1_BC1F , 0, 0, 0            , "\'B"  , "BC1F"   ) // Branch on FP False (1cyc*)
    INSN_ID_1(OPC_COP1, 0b01000        , OPT_COP1_BC1T , 0, 0, 0            , "\'B"  , "BC1T"   ) // Branch on FP True (1cyc*)
    INSN_ID_1(OPC_COP1, 0b01000        , OPT_COP1_BC1FL, 0, 0, 0            , "\'B"  , "BC1FL"  ) // Branch on FP False Likely (1cyc*)
    INSN_ID_1(OPC_COP1, 0b01000        , OPT_COP1_BC1TL, 0, 0, 0            , "\'B"  , "BC1TL"  ) // Branch on FP True Likely (1cyc*)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_ADD_F    , "\"DST", "ADD"    ) // ADD.[FMT]     Floating-Point Add (3cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_SUB_F    , "\"DST", "SUB"    ) // SUB.[FMT]     Floating-Point Subtract (3cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_MUL_F    , "\"DST", "MUL"    ) // MUL.[FMT]     Floating-Point Multiply (S:5cyc; D:8cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_DIV_F    , "\"DST", "DIV"    ) // DIV.[FMT]     Floating-Point Divide (S:29cyc; D:58cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_SQRT_F   , "\"DS" , "SQRT"   ) // SQRT.[FMT]    Floating-Point Square Root (S:29cyc; D:58cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_ABS_F    , "\"DS" , "ABS"    ) // ABS.[FMT]     Floating-Point Absolute Value (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_MOV_F    , "\"DS" , "MOV"    ) // MOV.[FMT]     Floating-Point Move (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_NEG_F    , "\"DS" , "NEG"    ) // NEG.[FMT]     Floating-Point Negate (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_ROUND_L_F, "\"DS" , "ROUND.L") // ROUND.L.[FMT] Floating-Point Round to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_TRUNC_L_F, "\"DS" , "TRUNC.L") // TRUNC.L.[FMT] Floating-Point Truncate to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CEIL_L_F , "\"DS" , "CEIL.L" ) // CEIL.L.[FMT]  Floating-Point Ceiling to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_FLOOR_L_F, "\"DS" , "FLOOR.L") // FLOOR.L.[FMT] Floating-Point Floor to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_ROUND_W_F, "\"DS" , "ROUND.W") // ROUND.W.[FMT] Floating-Point Round to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_TRUNC_W_F, "\"DS" , "TRUNC.W") // TRUNC.W.[FMT] Floating-Point Truncate to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CEIL_W_F , "\"DS" , "CEIL.W" ) // CEIL.W.[FMT]  Floating-Point Ceiling to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_FLOOR_W_F, "\"DS" , "FLOOR.W") // FLOOR.W.[FMT] Floating-Point Floor to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CVT_S_F  , "\"DS" , "CVT.S"  ) // CVT.S.[FMT]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CVT_D_F  , "\"DS" , "CVT.D"  ) // CVT.D.[FMT]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CVT_W_F  , "\"DS" , "CVT.W"  ) // CVT.W.[FMT]   Floating-Point Convert to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_CVT_L_F  , "\"DS" , "CVT.L"  ) // CVT.L.[FMT]   Floating-Point Convert to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_F      , "\"ST" , "C.F"    ) // C.F.[FMT]     Floating-Point Compare (False) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_UN     , "\"ST" , "C.UN"   ) // C.UN.[FMT]    Floating-Point Compare (Unordered) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_EQ     , "\"ST" , "C.EQ"   ) // C.EQ.[FMT]    Floating-point Compare (Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_UEQ    , "\"ST" , "C.UEQ"  ) // C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_OLT    , "\"ST" , "C.OLT"  ) // C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_ULT    , "\"ST" , "C.ULT"  ) // C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_OLE    , "\"ST" , "C.OLE"  ) // C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_ULE    , "\"ST" , "C.ULE"  ) // C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_SF     , "\"ST" , "C.SF"   ) // C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_NGLE   , "\"ST" , "C.NGLE" ) // C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_SEQ    , "\"ST" , "C.SEQ"  ) // C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_NGL    , "\"ST" , "C.NGL"  ) // C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_LT     , "\"ST" , "C.LT"   ) // C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_NGE    , "\"ST" , "C.NGE"  ) // C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_LE     , "\"ST" , "C.LE"   ) // C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1, 0b10000        , 0             , 0, 0, OPS_C_NGT    , "\"ST" , "C.NGT"  ) // C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc)
    INSN_DB_END
};

// Coprocessor-2 (Reality Co-Processor)
ALIGNED32 static const InsnTemplate insn_db_cop2[] = {
    INSN_DB_END
};

// Coprocessor-3 (CP3)
ALIGNED32 static const InsnTemplate insn_db_cop3[] = {
    INSN_DB_END
};

static const InsnTemplate* insn_db_cop_lists[] = {
    [COP0] = insn_db_cop0,
    [COP1] = insn_db_cop1,
    [COP2] = insn_db_cop2,
    [COP3] = insn_db_cop3,
};

// Pseudo-instructions
ALIGNED32 static const InsnTemplate insn_db_pseudo[] = {
    [PSEUDO_NOP  ] = INSN_ID_1(OPC_SPEC , 0, 0, 0, 0, OPS_SLL, "0"    , "NOP"    ) // NOP (pseudo of SLL)
    [PSEUDO_MOVET] = INSN_ID_0(OPC_SPEC , 0, 0, 0, 0, OPS_ADD, "\'dt" , "MOVE"   ) // Move (pseudo of ADD and OR)
    [PSEUDO_MOVES] = INSN_ID_0(OPC_SPEC , 0, 0, 0, 0, OPS_ADD, "\'ds" , "MOVE"   ) // Move (pseudo of ADD)
    [PSEUDO_B    ] = INSN_ID_1(OPC_BEQ  , 0, 0, 0, 0, 0      , "\'B"  , "B"      ) // Branch (pseudo of BEQ)
    [PSEUDO_BEQZ ] = INSN_ID_1(OPC_BEQ  , 0, 0, 0, 0, 0      , "\'sB" , "BEQZ"   ) // Branch on Equal to Zero (pseudo of BEQ)
    [PSEUDO_BNEZ ] = INSN_ID_1(OPC_BNE  , 0, 0, 0, 0, 0      , "\'sB" , "BNEZ"   ) // Branch on Not Equal to Zero (pseudo of BNE)
    [PSEUDO_SUBI ] = INSN_ID_1(OPC_ADDI , 0, 0, 0, 0, 0      , "\'tsi", "SUBI"   ) // Add Immediate Word
    [PSEUDO_BEQZL] = INSN_ID_1(OPC_BEQL , 0, 0, 0, 0, 0      , "\'sB" , "BEQZL"  ) // Branch on Equal to Zero Likely (pseudo of BEQL)
    [PSEUDO_BNEZL] = INSN_ID_1(OPC_BNEL , 0, 0, 0, 0, 0      , "\'sB" , "BNEZL"  ) // Branch on Not Equal to Zero Likely (pseudo of BNEL)
    [PSEUDO_DSUBI] = INSN_ID_0(OPC_DADDI, 0, 0, 0, 0, 0      , "\'tsi", "DSUBI"  ) // Doubleword Add Immediate
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
        case OPC_SPEC:
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
            if (check_pseudo_insn(type, PSEUDO_BEQZ,  (insn.rt == 0      ))) return TRUE;
            break;
        case OPC_BNE:
            if (check_pseudo_insn(type, PSEUDO_BNEZ,  (insn.rt == 0      ))) return TRUE;
            break;
        case OPC_ADDI:
            if (check_pseudo_insn(type, PSEUDO_SUBI,  ((s16)insn.immediate < 0))) return TRUE;
            break;
        case OPC_BEQL:
            if (check_pseudo_insn(type, PSEUDO_BEQZL, (insn.rt == 0      ))) return TRUE;
            break;
        case OPC_BNEL:
            if (check_pseudo_insn(type, PSEUDO_BNEZL, (insn.rt == 0      ))) return TRUE;
            break;
        case OPC_DADDI:
            if (check_pseudo_insn(type, PSEUDO_DSUBI, ((s16)insn.immediate < 0))) return TRUE;
            break;
    }

    return FALSE;
}

static enum InsnType get_insn_list(InsnData insn, const InsnTemplate** checkInsn) {
    enum InsnType insnType = INSN_TYPE_OPCODE;
    
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
            case OPC_SPEC:
                *checkInsn = insn_db_spec;
                insnType = INSN_TYPE_FUNC;
                break;
            case OPC_REGI:
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
    } else {
        checkInsn = insn_db;
        enum InsnType insnType = get_insn_list(insn, &checkInsn);
        _Bool check = FALSE;

        if (insnType != INSN_TYPE_ERROR) {
            while (checkInsn->insn.raw != (u32)-1) {
                switch (insnType) {
                    default:
                    case INSN_TYPE_OPCODE:  check = (insn.opcode == checkInsn->insn.opcode); break; // First 6 bits
                    case INSN_TYPE_FUNC:    check = (insn.func   == checkInsn->insn.func  ); break; // Last 6 bits
                    case INSN_TYPE_REGIMM:  check = (insn.regimm == checkInsn->insn.regimm); break; // The 5 bits after the first 11
                    case INSN_TYPE_COP_FMT: check = (insn.fmt    == checkInsn->insn.fmt   ); break; // The 3 bits after the first 8
                }

                if (check) {
                    return checkInsn;
                }

                checkInsn++;
            }
        }
    }

    return NULL;
}

// Registers
static const char sRegisterNames[][3] = {
    "R0",                                           // $zero
    "AT",                                           // Assembler temporary value
    "V0", "V1",                                     // Subroutine return value
    "A0", "A1", "A2", "A3",                         // Subroutine arguments
    "T0", "T1", "T2", "T3", "T4", "T5", "T6", "T7", // Temporary values
    "S0", "S1", "S2", "S3", "S4", "S5", "S6", "S7", // Saved values
    "T8", "T9",                                     // Temporary values
    "K0", "K1",                                     // Reserved by kernel
    "GP",                                           // Global pointer
    "SP",                                           // Stack pointer
    "FP",                                           // Saved value or frame pointer
    "RA",                                           // Return address
}; //! TODO: Combine this with sRegNames

// // FPU Registers
// static const char sFloatRegisterNames[][3] = {
//     "00", "02",                                     // Subroutine return value
//     "04", "06", "08", "10",                         // Temporary values
//     "12", "14",                                     // Subroutine arguments
//     "16", "18",                                     // Temporary values
//     "20", "22", "24", "26", "28", "30",             // Saved Values
// };

s16 check_for_branch_offset(InsnData insn) {
    const InsnTemplate* type = get_insn(insn);

    if (type != NULL) {
        for (int i = 0; i < 4; i++) {
            if (type->format[i] == CHAR_P_BRANCH) {
                return insn.offset;
            }
        }
    }

    return 0;
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
#define STR_IREG_OFFSET     "("STR_IREG")"                  // Offset register
#define STR_FREG            "F%02d"                         // Float Register

char* insn_disasm(InsnData insn, const char** fname, _Bool showDestNames) {
    char* strp = &insn_as_string[0];
    _Bool unimpl = FALSE;
    char insn_name[INSN_NAME_DISPLAY_WIDTH] = "";

    bzero(insn_as_string, sizeof(insn_as_string));

    const InsnTemplate* type = get_insn(insn);

    if (type != NULL) {
        RGBA32 color = COLOR_RGBA32_NONE;

        for (u8 cmdIndex = 0; cmdIndex < sizeof(u32); cmdIndex++) {
            char curCmd = type->format[cmdIndex];

            if (unimpl || curCmd == CHAR_P_NULL) {
                break;
            }

            //! TODO: commas between registers (check next byte?)
            switch (curCmd) {
                case CHAR_P_NOP:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_NOP);
                    strp += sprintf(strp, type->name);
                    return insn_as_string;
                case CHAR_P_NAME:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_INSN);
                    strp += sprintf(strp, STR_INSN_NAME, type->name);
                    break;
                case CHAR_P_NAMEF:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_INSN);
                    bzero(insn_name, sizeof(insn_name));
                    sprintf(insn_name, STR_INSN_NAME_FORMAT, type->name, cop1_fmt_to_char(insn));
                    strp += sprintf(strp, STR_INSN_NAME, insn_name);
                    break;
                case CHAR_P_RS:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sRegisterNames[insn.rs]);
                    break;
                case CHAR_P_RT:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sRegisterNames[insn.rt]);
                    break;
                case CHAR_P_RD:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_REG);
                    strp += sprintf(strp, STR_IREG" ", sRegisterNames[insn.rd]);
                    break;
                case CHAR_P_IMM:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, insn.immediate);
                    break;
                case CHAR_P_NIMM:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, (s16)-insn.immediate);
                    break;
                case CHAR_P_SHIFT:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_IMMEDIATE);
                    strp += sprintf(strp, STR_IMMEDIATE, insn.sa);
                    break;
                case CHAR_P_BASE:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_DISASM_BASE_REG);
                    strp += sprintf(strp, STR_IREG_OFFSET, sRegisterNames[insn.base]);
                    break;
                case CHAR_P_BRANCH:
                    check_color_change(&strp, &color, COLOR_RGBA32_CRASH_FUNCTION_NAME_2);
                    s16 branchOffset = (insn.offset + 1);
                    strp += sprintf(strp, STR_OFFSET, ((branchOffset < 0) ? '-' : '+'), abss(branchOffset)); //! TODO: Use '+' format specifier if possible.
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
