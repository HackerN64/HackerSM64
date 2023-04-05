#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "color_presets.h"
#include "crash_screen.h"
#include "crash_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "engine/math_util.h"


// MIPS III Instructions
//! TODO: Fix pseudoinstructions

// Opcode instructions
ALIGNED32 static const InsnTemplate insn_db[] = {
    // INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0,             0, PARAM_NOP, "SPECIAL") //! check insn_db_spec
    // INSN_ID_0(OPC_REGI  ,       0, 0b00000,       0,       0,             0, PARAM_NOP, "REGI"   ) //! check insn_db_regi
    INSN_ID_1(OPC_J     ,       0,       0,       0,       0,             0, PARAM_J  , "J"      ) // Jump
    INSN_ID_1(OPC_JAL   ,       0,       0,       0,       0,             0, PARAM_J  , "JAL"    ) // Jump and Link
    //! TODO: INSN_ID_1(OPC_BEQ   , 0b00000, 0b00000,       0,       0,             0, PARAM_B  , "B"      ) // Branch (pseudo of BEQ)
    //! TODO: INSN_ID_1(OPC_BEQ   , 0b00000,       0,       0,       0,             0, PARAM_SB , "BEQZ"   ) // Branch on Equal to Zero (pseudo of BEQ)
    INSN_ID_1(OPC_BEQ   ,       0,       0,       0,       0,             0, PARAM_STB, "BEQ"    ) // Branch on Equal
    //! TODO: INSN_ID_1(OPC_BNE   , 0b00000,       0,       0,       0,             0, PARAM_SB , "BNEZ"   ) // Branch on Not Equal to Zero (pseudo of BNE)
    INSN_ID_1(OPC_BNE   ,       0,       0,       0,       0,             0, PARAM_STB, "BNE"    ) // Branch on Not Equal
    INSN_ID_1(OPC_BLEZ  ,       0, 0b00000,       0,       0,             0, PARAM_SB , "BLEZ"   ) // Branch on Less Than or Equal to Zero
    INSN_ID_1(OPC_BGTZ  ,       0, 0b00000,       0,       0,             0, PARAM_SB , "BGTZ"   ) // Branch on Greater Than Zero
    INSN_ID_1(OPC_ADDI  ,       0,       0,       0,       0,             0, PARAM_TSI, "ADDI"   ) // Add Immediate Word
    INSN_ID_1(OPC_ADDIU ,       0,       0,       0,       0,             0, PARAM_TSI, "ADDIU"  ) // Add Immediate Unsigned Word
    INSN_ID_1(OPC_SLTI  ,       0,       0,       0,       0,             0, PARAM_TSI, "SLTI"   ) // Set on Less Than Immediate
    INSN_ID_1(OPC_SLTIU ,       0,       0,       0,       0,             0, PARAM_TSI, "SLTIU"  ) // Set on Less Than Immediate Unsigned
    INSN_ID_1(OPC_ANDI  ,       0,       0,       0,       0,             0, PARAM_TSI, "ANDI"   ) // And Immediate
    INSN_ID_1(OPC_ORI   ,       0,       0,       0,       0,             0, PARAM_TSI, "ORI"    ) // Or Immediate
    INSN_ID_1(OPC_XORI  ,       0,       0,       0,       0,             0, PARAM_TSI, "XORI"   ) // Exclusive Or Immediate
    INSN_ID_1(OPC_LUI   ,       0,       0,       0,       0,             0, PARAM_TI , "LUI"    ) // Load Upper Immediate
    // INSN_ID_0(OPC_COP0  ,       0,       0,       0,       0,             0, PARAM_N  , "COP0"   ) //! Coprocessor-0 Operation (System Control Coprocessor)
    // INSN_ID_0(OPC_COP1  ,       0,       0,       0,       0,             0, PARAM_N  , "COP1"   ) //! Coprocessor-1 Operation (Floating-Point Unit)
    // INSN_ID_0(OPC_COP2  ,       0,       0,       0,       0,             0, PARAM_N  , "COP2"   ) //! Coprocessor-2 Operation (Reality Co-Processor)
    // INSN_ID_0(OPC_COP3  ,       0,       0,       0,       0,             0, PARAM_N  , "COP3"   ) //! Coprocessor-3 Operation (CP3)
    //! TODO: INSN_ID_1(OPC_BEQL  , 0b00000,       0,       0,       0,             0, PARAM_SB , "BEQZL"  ) // Branch on Equal to Zero Likely (pseudo of BEQL)
    INSN_ID_1(OPC_BEQL  ,       0,       0,       0,       0,             0, PARAM_STB, "BEQL"   ) // Branch on Equal Likely
    //! TODO: INSN_ID_1(OPC_BNEL  , 0b00000,       0,       0,       0,             0, PARAM_SB , "BNEZL"  ) // Branch on Not Equal to Zero Likely (pseudo of BNEL)
    INSN_ID_1(OPC_BNEL  ,       0,       0,       0,       0,             0, PARAM_STB, "BNEL"   ) // Branch on Not Equal Likely
    INSN_ID_1(OPC_BLEZL ,       0, 0b00000,       0,       0,             0, PARAM_SB , "BLEZL"  ) // Branch on Less Than or Equal to Zero Likely
    INSN_ID_1(OPC_BGTZL ,       0, 0b00000,       0,       0,             0, PARAM_SB , "BGTZL"  ) // Branch on Greater Than Zero Likely
    INSN_ID_0(OPC_DADDI ,       0,       0,       0,       0,             0, PARAM_TSI, "DADDI"  ) // Doubleword Add Immediate
    INSN_ID_0(OPC_DADDIU,       0,       0,       0,       0,             0, PARAM_TSI, "DADDIU" ) // Doubleword Add Immediate Unsigned
    INSN_ID_0(OPC_LDL   ,       0,       0,       0,       0,             0, PARAM_TOS, "LDL"    ) // Load Doubleword Left
    INSN_ID_0(OPC_LDR   ,       0,       0,       0,       0,             0, PARAM_TOS, "LDR"    ) // Load Doubleword Right
    INSN_ID_1(OPC_LB    ,       0,       0,       0,       0,             0, PARAM_TOS, "LB"     ) // Load Byte
    INSN_ID_1(OPC_LH    ,       0,       0,       0,       0,             0, PARAM_TOS, "LH"     ) // Load Halfword
    INSN_ID_1(OPC_LWL   ,       0,       0,       0,       0,             0, PARAM_TOS, "LWL"    ) // Load Word Left
    INSN_ID_1(OPC_LW    ,       0,       0,       0,       0,             0, PARAM_TOS, "LW"     ) // Load Word
    INSN_ID_1(OPC_LBU   ,       0,       0,       0,       0,             0, PARAM_TOS, "LBU"    ) // Load Byte Unsigned
    INSN_ID_1(OPC_LHU   ,       0,       0,       0,       0,             0, PARAM_TOS, "LHU"    ) // Load Halfword Unsigned
    INSN_ID_1(OPC_LWR   ,       0,       0,       0,       0,             0, PARAM_TOS, "LWR"    ) // Load Word Right
    INSN_ID_0(OPC_LWU   ,       0,       0,       0,       0,             0, PARAM_TOS, "LWU"    ) // Load Word Unsigned
    INSN_ID_1(OPC_SB    ,       0,       0,       0,       0,             0, PARAM_TOS, "SB"     ) // Store Byte
    INSN_ID_1(OPC_SH    ,       0,       0,       0,       0,             0, PARAM_TOS, "SH"     ) // Store Halfword
    INSN_ID_0(OPC_SWL   ,       0,       0,       0,       0,             0, PARAM_TOS, "SWL"    ) // Store Word Left
    INSN_ID_1(OPC_SW    ,       0,       0,       0,       0,             0, PARAM_TOS, "SW"     ) // Store Word
    INSN_ID_0(OPC_SDL   ,       0,       0,       0,       0,             0, PARAM_TOS, "SDL"    ) // Store Doubleword Left
    INSN_ID_0(OPC_SDR   ,       0,       0,       0,       0,             0, PARAM_TOS, "SDR"    ) // Store Doubleword Right
    INSN_ID_0(OPC_SWR   ,       0,       0,       0,       0,             0, PARAM_TOS, "SWR"    ) // Store Word Right
    INSN_ID_0(OPC_CACHE ,       0,       0,       0,       0,             0, PARAM_TOS, "CACHE"  ) // https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html
    INSN_ID_0(OPC_LL    ,       0,       0,       0,       0,             0, PARAM_TOS, "LL"     ) // Load Linked Word
    INSN_ID_1(OPC_LWC1  ,       0,       0,       0,       0,             0, PARAM_FOS, "LWC1"   ) // Load Word to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_LWC2  ,       0,       0,       0,       0,             0, PARAM_FOS, "LWC2"   ) // Load Word to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_LWC3  ,       0,       0,       0,       0,             0, PARAM_FOS, "LWC3"   ) // Load Word to Coprocessor-3 (COP3)
    INSN_ID_0(OPC_LLD   ,       0,       0,       0,       0,             0, PARAM_TOS, "LLD"    ) // Load Linked Doubleword
    INSN_ID_1(OPC_LDC1  ,       0,       0,       0,       0,             0, PARAM_TOS, "LDC1"   ) // Load Doubleword to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_LDC2  ,       0,       0,       0,       0,             0, PARAM_TOS, "LDC2"   ) // Load Doubleword to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_LD    ,       0,       0,       0,       0,             0, PARAM_TOS, "LD"     ) // Load Doubleword
    INSN_ID_0(OPC_SC    ,       0,       0,       0,       0,             0, PARAM_TOS, "SC"     ) // Store Conditional Word
    INSN_ID_1(OPC_SWC1  ,       0,       0,       0,       0,             0, PARAM_FOS, "SWC1"   ) // Store Word to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_SWC2  ,       0,       0,       0,       0,             0, PARAM_FOS, "SWC2"   ) // Store Word to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_SWC3  ,       0,       0,       0,       0,             0, PARAM_FOS, "SWC3"   ) // Store Word to Coprocessor-3 (COP3)
    INSN_ID_0(OPC_SCD   ,       0,       0,       0,       0,             0, PARAM_TOS, "SCD"    ) // Store Conditional Doubleword
    INSN_ID_1(OPC_SDC1  ,       0,       0,       0,       0,             0, PARAM_TOS, "SDC1"   ) // Store Doubleword to Coprocessor-1 (Floating-Point Unit)
    INSN_ID_0(OPC_SDC2  ,       0,       0,       0,       0,             0, PARAM_TOS, "SDC2"   ) // Store Doubleword to Coprocessor-2 (Reality Co-Processor)
    INSN_ID_0(OPC_SD    ,       0,       0,       0,       0,             0, PARAM_TOS, "SD"     ) // Store Doubleword
    INSN_DB_END
};

// Special opcode instructions
ALIGNED32 static const InsnTemplate insn_db_spec[] = { // PARAM_NOP, PARAM_TD, PARAM_SD, PARAM_DST, PARAM_DTA, PARAM_DTS
    INSN_ID_1(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_SLL      , PARAM_DTA, "SLL"    ) // Shift Word Left Logical
    INSN_ID_1(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_SRL      , PARAM_DTA, "SRL"    ) // Shift Word Right Logical
    INSN_ID_1(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_SRA      , PARAM_DTA, "SRA"    ) // Shift Word Right Arithmetic
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SLLV     , PARAM_DTS, "SLLV"   ) // Shift Word Left Logical Variable
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SRLV     , PARAM_DTS, "SRLV"   ) // Shift Word Right Logical Variable
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SRAV     , PARAM_DTS, "SRAV"   ) // Shift Word Right Arithmetic Variable
    INSN_ID_1(OPC_SPEC  ,       0, 0b00000, 0b00000, 0b00000, OPS_JR       , PARAM_S  , "JR"     ) // Jump Register
    INSN_ID_1(OPC_SPEC  ,       0, 0b00000,       0, 0b00000, OPS_JALR     , PARAM_DS , "JALR"   ) // Jump and Link Register
    INSN_ID_1(OPC_SPEC  , 0b00000, 0b00000, 0b00000, 0b00000, OPS_SYSCALL  , PARAM_SYS, "SYSCALL") // System Call (assert)
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_BREAK    , PARAM_SYS, "BREAK"  ) // Breakpoint
    INSN_ID_0(OPC_SPEC  , 0b00000, 0b00000, 0b00000,       0, OPS_SYNC     , PARAM_SYN, "SYNC"   ) // Synchronize Shared Memory
    INSN_ID_1(OPC_SPEC  , 0b00000, 0b00000,       0, 0b00000, OPS_MFHI     , PARAM_D  , "MFHI"   ) // Move From HI
    INSN_ID_1(OPC_SPEC  ,       0, 0b00000, 0b00000, 0b00000, OPS_MTHI     , PARAM_S  , "MTHI"   ) // Move To HI
    INSN_ID_1(OPC_SPEC  , 0b00000, 0b00000,       0, 0b00000, OPS_MFLO     , PARAM_D  , "MFLO"   ) // Move From LO
    INSN_ID_1(OPC_SPEC  ,       0, 0b00000, 0b00000, 0b00000, OPS_MTLO     , PARAM_S  , "MTLO"   ) // Move To LO
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DSLLV    , PARAM_DTS, "DSLLV"  ) // Doubleword Shift Left Logical Variable
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DSRLV    , PARAM_DTS, "DSRLV"  ) // Doubleword Shift Right Logical Variable
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DSRAV    , PARAM_DTS, "DSRAV"  ) // Doubleword Shift Right Arithmetic Variable
    INSN_ID_1(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_MULT     , PARAM_ST , "MULT"   ) // Multiply Word (5cyc)
    INSN_ID_1(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_MULTU    , PARAM_ST , "MULTU"  ) // Multiply Unsigned Word (5cyc)
    INSN_ID_1(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DIV      , PARAM_ST , "DIV"    ) // Divide Word (37cyc)
    INSN_ID_1(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DIVU     , PARAM_ST , "DIVU"   ) // Divide Unsigned Word (37cyc)
    INSN_ID_0(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DMULT    , PARAM_ST , "DMULT"  ) // Doubleword Multiply (8cyc)
    INSN_ID_0(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DMULTU   , PARAM_ST , "DMULTU" ) // Doubleword Multiply Unsigned (8cyc)
    INSN_ID_0(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DDIV     , PARAM_ST , "DDIV"   ) // Doubleword Divide (69cyc)
    INSN_ID_0(OPC_SPEC  ,       0,       0, 0b00000, 0b00000, OPS_DDIVU    , PARAM_ST , "DDIVU"  ) // Doubleword Divide Unsigned (69cyc)
    //! TODO: INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0, 0b00000, OPS_ADD      , PARAM_TD , "MOVE"   ) // Move (pseudo of ADD)
    //! TODO: INSN_ID_0(OPC_SPEC  ,       0, 0b00000,       0, 0b00000, OPS_ADD      , PARAM_SD , "MOVE"   ) // Move (pseudo of ADD)
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_ADD      , PARAM_DST, "ADD"    ) // Add Word
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_ADDU     , PARAM_DST, "ADDU"   ) // Add Unsigned Word
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SUB      , PARAM_DST, "SUB"    ) // Subtract Word
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SUBU     , PARAM_DST, "SUBU"   ) // Subtract Unsigned Word
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_AND      , PARAM_DST, "AND"    ) // And
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_OR       , PARAM_DST, "OR"     ) // Or
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_XOR      , PARAM_DST, "XOR"    ) // Exclusive Or
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_NOR      , PARAM_DST, "NOR"    ) // Nor
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SLT      , PARAM_DST, "SLT"    ) // Set on Less Than
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_SLTU     , PARAM_DST, "SLTU"   ) // Set on Less Than Unsigned
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DADD     , PARAM_DST, "DADD"   ) // Doubleword Add
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DADDU    , PARAM_DST, "DADDU"  ) // Doubleword Add Unsigned
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DSUB     , PARAM_DST, "DSUB"   ) // Doubleword Subtract
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0, 0b00000, OPS_DSUBU    , PARAM_DST, "DSUBU"  ) // Doubleword Subtract Unsigned
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_TGE      , PARAM_ST2, "TGE"    ) // Trap if Greater Than or Equal
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_TGEU     , PARAM_ST2, "TGEU"   ) // Trap if Greater Than or Equal Unsigned
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_TLT      , PARAM_ST2, "TLT"    ) // Trap if Less Than
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_TLTU     , PARAM_ST2, "TLTU"   ) // Trap if Less Than Unsigned
    INSN_ID_1(OPC_SPEC  ,       0,       0,       0,       0, OPS_TEQ      , PARAM_ST2, "TEQ"    ) // Trap if Equal
    INSN_ID_0(OPC_SPEC  ,       0,       0,       0,       0, OPS_TNE      , PARAM_ST2, "TNE"    ) // Trap if Not Equal
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSLL     , PARAM_DTA, "DSLL"   ) // Doubleword Shift Left Logical
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSRL     , PARAM_DTA, "DSRL"   ) // Doubleword Shift Right Logical
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSRA     , PARAM_DTA, "DSRA"   ) // Doubleword Shift Right Arithmetic
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSLL32   , PARAM_DTA, "DSLL32" ) // Doubleword Shift Left Logical + 32
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSRL32   , PARAM_DTA, "DSRL32" ) // Doubleword Shift Right Logical + 32
    INSN_ID_0(OPC_SPEC  , 0b00000,       0,       0,       0, OPS_DSRA32   , PARAM_DTA, "DSRA32" ) // Doubleword Shift Right Arithmetic + 32
    INSN_DB_END
};

// Register opcode instructions
ALIGNED32 static const InsnTemplate insn_db_regi[] = {
    INSN_ID_1(OPC_REGI  ,       0, OPR_BLTZ   ,       0,       0,             0, PARAM_SB,  "BLTZ"   ) // Branch on Less Than Zero
    INSN_ID_1(OPC_REGI  ,       0, OPR_BGEZ   ,       0,       0,             0, PARAM_SB,  "BGEZ"   ) // Branch on Greater Than or Equal to Zero
    INSN_ID_1(OPC_REGI  ,       0, OPR_BLTZL  ,       0,       0,             0, PARAM_SB,  "BLTZL"  ) // Branch on Less Than Zero Likely
    INSN_ID_1(OPC_REGI  ,       0, OPR_BGEZL  ,       0,       0,             0, PARAM_SB,  "BGEZL"  ) // Branch on Greater Than or Equal to Zero Likely
    INSN_ID_0(OPC_REGI  ,       0, OPR_BLTZAL ,       0,       0,             0, PARAM_SB,  "BLTZAL" ) // Branch on Less Than Zero and Link
    INSN_ID_0(OPC_REGI  ,       0, OPR_BGEZAL ,       0,       0,             0, PARAM_SB,  "BGEZAL" ) // Branch on Greater Than or Equal to Zero and Link
    INSN_ID_0(OPC_REGI  ,       0, OPR_BLTZALL,       0,       0,             0, PARAM_SB,  "BLTZALL") // Branch on Less Than Zero and Link Likely
    INSN_ID_0(OPC_REGI  ,       0, OPR_BGEZALL,       0,       0,             0, PARAM_SB,  "BGEZALL") // Branch on Greater Than or Equal to Zero and Link Likely
    INSN_ID_0(OPC_REGI  ,       0, OPR_TGEI   ,       0,       0,             0, PARAM_SI,  "TGEI"   ) // Trap if Greater Than or Equal Immediate
    INSN_ID_0(OPC_REGI  ,       0, OPR_TGEIU  ,       0,       0,             0, PARAM_SI,  "TGEIU"  ) // Trap if Greater Than or Equal Unsigned Immediate
    INSN_ID_0(OPC_REGI  ,       0, OPR_TLTI   ,       0,       0,             0, PARAM_SI,  "TLTI"   ) // Trap if Less Than Immediate
    INSN_ID_0(OPC_REGI  ,       0, OPR_TLTIU  ,       0,       0,             0, PARAM_SI,  "TLTIU"  ) // Trap if Less Than Unsigned Immediate
    INSN_ID_0(OPC_REGI  ,       0, OPR_TEQI   ,       0,       0,             0, PARAM_SI,  "TEQI"   ) // Trap if Equal Immediate
    INSN_ID_0(OPC_REGI  ,       0, OPR_TNEI   ,       0,       0,             0, PARAM_SI,  "TNEI"   ) // Trap if Not Equal Immediate
    INSN_DB_END
};

// Coprocessor-0 (System Control Coprocessor)
ALIGNED32 static const InsnTemplate insn_db_cop0[] = {
    INSN_ID_0(OPC_COP0  , COP0_MF ,       0,       0, 0b00000,       0b000000, PARAM_TD , "MFC0"   ) // Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0  , COP0_DMF,       0,       0, 0b00000,       0b000000, PARAM_TD , "DMFC0"  ) // Doubleword Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0  , COP0_MT ,       0,       0, 0b00000,       0b000000, PARAM_TD , "MTC0"   ) // Move to System Control Coprocessor
    INSN_ID_0(OPC_COP0  , COP0_DMT,       0,       0, 0b00000,       0b000000, PARAM_TD , "DMTC0"  ) // Doubleword Move to System Control Coprocessor
    INSN_ID_0(OPC_COP0  ,  0b10000, 0b00000, 0b00000, 0b00000, OPC_COP0_TLBP , PARAM_SYS, "TLBP"   ) // Searches for a TLB entry that matches the EntryHi register
    INSN_ID_0(OPC_COP0  ,  0b10000, 0b00000, 0b00000, 0b00000, OPC_COP0_TLBR , PARAM_SYS, "TLBR"   ) // Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0  ,  0b10000, 0b00000, 0b00000, 0b00000, OPC_COP0_TLBWI, PARAM_SYS, "TLBWI"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0  ,  0b10000, 0b00000, 0b00000, 0b00000, OPC_COP0_TLBWR, PARAM_SYS, "TLBWR"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register
    INSN_ID_0(OPC_COP0  ,  0b10000, 0b00000, 0b00000, 0b00000, OPC_COP0_ERET , PARAM_SYS, "ERET"   ) // Return from interrupt, exception, or error exception
    INSN_DB_END
};

// Coprocessor-1 (Floating-Point Unit)
ALIGNED32 static const InsnTemplate insn_db_cop1[] = { // Floating point
    INSN_ID_1(OPC_COP1  , COP1_FMT_SINGLE,              0,       0, 0b00000,      0b000000, PARAM_TFS, "MFC1"   ) // Move Word From Floating-Point
    INSN_ID_0(OPC_COP1  , COP1_FMT_DOUBLE,              0,       0, 0b00000,      0b000000, PARAM_TFS, "DMFC1"  ) // Doubleword Move From Floating-Point
    INSN_ID_1(OPC_COP1  , COP1_FMT_WORD  ,              0,       0, 0b00000,      0b000000, PARAM_TFS, "MTC1"   ) // Move Word To Floating-Point
    INSN_ID_0(OPC_COP1  , COP1_FMT_LONG  ,              0,       0, 0b00000,      0b000000, PARAM_TFS, "DMTC1"  ) // Doubleword Move To Floating-Point
    INSN_ID_0(OPC_COP1  , COP1_FMT_CTL_F ,              0,       0, 0b00000,      0b000000, PARAM_TFS, "CFC1"   ) // Move Control Word From Floating-Point
    INSN_ID_0(OPC_COP1  , COP1_FMT_CTL_T ,              0,       0, 0b00000,      0b000000, PARAM_TFS, "CTC1"   ) // Move Control Word To Floating-Point
    INSN_ID_1(OPC_COP1  , 0b01000        , OPT_COP1_BC1F ,       0,       0,             0, PARAM_BC1, "BC1F"   ) // Branch on FP False (1cyc*)
    INSN_ID_1(OPC_COP1  , 0b01000        , OPT_COP1_BC1T ,       0,       0,             0, PARAM_BC1, "BC1T"   ) // Branch on FP True (1cyc*)
    INSN_ID_1(OPC_COP1  , 0b01000        , OPT_COP1_BC1FL,       0,       0,             0, PARAM_BC1, "BC1FL"  ) // Branch on FP False Likely (1cyc*)
    INSN_ID_1(OPC_COP1  , 0b01000        , OPT_COP1_BC1TL,       0,       0,             0, PARAM_BC1, "BC1TL"  ) // Branch on FP True Likely (1cyc*)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0,       0, OPS_ADD_F    , PARAM_FFF, "ADD"    ) // ADD.[FMT]     Floating-Point Add (3cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0,       0, OPS_SUB_F    , PARAM_FFF, "SUB"    ) // SUB.[FMT]     Floating-Point Subtract (3cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0,       0, OPS_MUL_F    , PARAM_FFF, "MUL"    ) // MUL.[FMT]     Floating-Point Multiply (S:5cyc; D:8cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0,       0, OPS_DIV_F    , PARAM_FFF, "DIV"    ) // DIV.[FMT]     Floating-Point Divide (S:29cyc; D:58cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_SQRT_F   , PARAM_FF , "SQRT"   ) // SQRT.[FMT]    Floating-Point Square Root (S:29cyc; D:58cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_ABS_F    , PARAM_FF , "ABS"    ) // ABS.[FMT]     Floating-Point Absolute Value (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_MOV_F    , PARAM_FF , "MOV"    ) // MOV.[FMT]     Floating-Point Move (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_NEG_F    , PARAM_FF , "NEG"    ) // NEG.[FMT]     Floating-Point Negate (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_ROUND_L_F, PARAM_FF , "ROUND.L") // ROUND.L.[FMT] Floating-Point Round to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_TRUNC_L_F, PARAM_FF , "TRUNC.L") // TRUNC.L.[FMT] Floating-Point Truncate to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CEIL_L_F , PARAM_FF , "CEIL.L" ) // CEIL.L.[FMT]  Floating-Point Ceiling to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_FLOOR_L_F, PARAM_FF , "FLOOR.L") // FLOOR.L.[FMT] Floating-Point Floor to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_ROUND_W_F, PARAM_FF , "ROUND.W") // ROUND.W.[FMT] Floating-Point Round to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_TRUNC_W_F, PARAM_FF , "TRUNC.W") // TRUNC.W.[FMT] Floating-Point Truncate to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CEIL_W_F , PARAM_FF , "CEIL.W" ) // CEIL.W.[FMT]  Floating-Point Ceiling to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_FLOOR_W_F, PARAM_FF , "FLOOR.W") // FLOOR.W.[FMT] Floating-Point Floor to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CVT_S_F  , PARAM_FF , "CVT.S"  ) // CVT.S.[FMT]   Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CVT_D_F  , PARAM_FF , "CVT.D"  ) // CVT.D.[FMT]   Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CVT_W_F  , PARAM_FF , "CVT.W"  ) // CVT.W.[FMT]   Floating-Point Convert to Word Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,        0b00000,       0,       0, OPS_CVT_L_F  , PARAM_FF , "CVT.L"  ) // CVT.L.[FMT]   Floating-Point Convert to Long Fixed-Point (5cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_F      , PARAM_CON, "C.F"    ) // C.F.[FMT]     Floating-Point Compare (False) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_UN     , PARAM_CON, "C.UN"   ) // C.UN.[FMT]    Floating-Point Compare (Unordered) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_EQ     , PARAM_CON, "C.EQ"   ) // C.EQ.[FMT]    Floating-point Compare (Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_UEQ    , PARAM_CON, "C.UEQ"  ) // C.UEQ.[fmt]   Floating-point Compare (Unordered or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_OLT    , PARAM_CON, "C.OLT"  ) // C.OLT.[fmt]   Floating-point Compare (Ordered Less Than) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_ULT    , PARAM_CON, "C.ULT"  ) // C.ULT.[fmt]   Floating-point Compare (Unordered or Less Than) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_OLE    , PARAM_CON, "C.OLE"  ) // C.OLE.[fmt]   Floating-point Compare (Ordered or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_ULE    , PARAM_CON, "C.ULE"  ) // C.ULE.[fmt]   Floating-point Compare (Unordered or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_SF     , PARAM_CON, "C.SF"   ) // C.SF.[fmt]    Floating-point Compare (Signaling False) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_NGLE   , PARAM_CON, "C.NGLE" ) // C.NGLE.[fmt]  Floating-point Compare (Not Greater or Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_SEQ    , PARAM_CON, "C.SEQ"  ) // C.SEQ.[fmt]   Floating-point Compare (Signalling Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_NGL    , PARAM_CON, "C.NGL"  ) // C.NGL.[fmt]   Floating-point Compare (Not Greater or Less Than) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_LT     , PARAM_CON, "C.LT"   ) // C.LT.[fmt]    Floating-point Compare (Less Than) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_NGE    , PARAM_CON, "C.NGE"  ) // C.NGE.[fmt]   Floating-point Compare (Not Greater Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_LE     , PARAM_CON, "C.LE"   ) // C.LE.[fmt]    Floating-point Compare (Less Than or Equal) (1cyc)
    INSN_ID_1(OPC_COP1  , 0b10000        ,              0,       0, 0b00000, OPS_C_NGT    , PARAM_CON, "C.NGT"  ) // C.NGT.[fmt]   Floating-point Compare (Not Greater Than) (1cyc)
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

static const InsnTemplate* insn_db_copz[] = {
    [COP0] = insn_db_cop0,
    [COP1] = insn_db_cop1,
    [COP2] = insn_db_cop2,
    [COP3] = insn_db_cop3,
};

#define STR_NOP             "NOP"
#define STR_UNIMPLEMENTED   "unimpl"

#define STR_INSN_NAME_BASE      "%s"
#define STR_FORMAT              "%c"
#define STR_CONDITION           "%s"

#define STR_INSN_NAME           "%-"TO_STRING2(INSN_NAME_DISPLAY_WIDTH)"s"
#define STR_INSN_NAME_FORMAT    STR_INSN_NAME_BASE"."STR_FORMAT

#define STR_IREG            "%s"                            // Register
#define STR_IMMEDIATE       STR_HEX_PREFIX STR_HEX_HALFWORD // 0xI
#define STR_OFFSET          "%c"STR_IMMEDIATE               // +-Offset
#define STR_FUNCTION        STR_HEX_PREFIX STR_HEX_WORD     // Function address
#define STR_IREG_OFFSET     "("STR_IREG")"                  // Offset register
#define STR_FREG            "F%02d"                         // Float Register

//! TODO: Move this data into insn_db arrays to save memory, replacing paramType
ALIGNED4 static const u8 insn_print_formats[][4] = {
    [PARAM_N  ] = { P_NAME,     0,          0,          0,          }, //
    [PARAM_S  ] = { P_NAME,     P_RS,       0,          0,          }, // rs
    [PARAM_T  ] = { P_NAME,     P_RT,       0,          0,          }, // rt
    [PARAM_D  ] = { P_NAME,     P_RD,       0,          0,          }, // rd
    [PARAM_ST ] = { P_NAME,     P_RS,       P_RT,       0,          }, // rs, rt
    [PARAM_DS ] = { P_NAME,     P_RD,       P_RS,       0,          }, // rd, rs
    [PARAM_TD ] = { P_NAME,     P_RT,       P_RD,       0,          }, // rt, rd
    [PARAM_SD ] = { P_NAME,     P_RS,       P_RD,       0,          }, // rs, rd
    [PARAM_STD] = { P_NAME,     P_RS,       P_RT,       P_RD,       }, // rs, rt, rd
    [PARAM_SDT] = { P_NAME,     P_RS,       P_RD,       P_RT,       }, // rs, rd, rt
    [PARAM_DST] = { P_NAME,     P_RD,       P_RS,       P_RT,       }, // rd, rs, rt
    [PARAM_DTS] = { P_NAME,     P_RD,       P_RT,       P_RS,       }, // rd, rt, rs
    [PARAM_DTA] = { P_NAME,     P_RD,       P_RT,       P_SHIFT,    }, // rd, rt, sa
    [PARAM_SI ] = { P_NAME,     P_RS,       P_IMM,      0,          }, // rs, 0xI
    [PARAM_TI ] = { P_NAME,     P_RT,       P_IMM,      0,          }, // rt, 0xI
    [PARAM_STI] = { P_NAME,     P_RS,       P_RT,       P_IMM       }, // rs, rt, 0xI
    [PARAM_TSI] = { P_NAME,     P_RT,       P_RS,       P_IMM       }, // rt, rs, 0xI
    [PARAM_TOS] = { P_NAME,     P_RT,       P_IMM,      P_BASE,     }, // rt, offset(rs)
    [PARAM_SB ] = { P_NAME,     P_RS,       P_BRANCH,   0,          }, // rs, offset
    [PARAM_STB] = { P_NAME,     P_RS,       P_RT,       P_BRANCH,   }, // rs, rt, offset
    [PARAM_B  ] = { P_NAME,     P_BRANCH,   0,          0,          }, // offset
    [PARAM_J  ] = { P_NAME,     P_FUNC,     0,          0,          }, // function address
    [PARAM_FOS] = { P_NAME,     P_FT,       P_IMM,      P_BASE,     }, // ft, offset(rs)
    [PARAM_TFS] = { P_NAME,     P_RT,       P_FS,       0,          }, // rt, fs
    [PARAM_FF ] = { P_NAME_FMT, P_FD,       P_FS,       0,          }, // fd, fs
    [PARAM_FFF] = { P_NAME_FMT, P_FD,       P_FS,       P_FT,       }, // fd, fs, ft
    [PARAM_CON] = { P_NAME_FMT, P_FS,       P_FT,       0,          }, // fs, ft
};

ALIGNED8 static const InsnPrintFormatData insn_print_format_data[] = {
    [P_NULL     ] = { .color = COLOR_RGBA32_NONE,                  .print = ""                      },
    [P_NAME     ] = { .color = COLOR_RGBA32_CRASH_DISASM_INSN,     .print = ""STR_INSN_NAME         },
    [P_NAME_FMT ] = { .color = COLOR_RGBA32_CRASH_DISASM_INSN,     .print = ""STR_INSN_NAME_FORMAT  },
    [P_RS       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_IREG             },
    [P_RT       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_IREG             },
    [P_RD       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_IREG             },
    [P_SHIFT    ] = { .color = COLOR_RGBA32_CRASH_IMMEDIATE,       .print = " "STR_IMMEDIATE        },
    [P_BASE     ] = { .color = COLOR_RGBA32_CRASH_DISASM_BASE_REG, .print = ""STR_IREG_OFFSET       },
    [P_IMM      ] = { .color = COLOR_RGBA32_CRASH_IMMEDIATE,       .print = " "STR_IMMEDIATE        },
    [P_BRANCH   ] = { .color = COLOR_RGBA32_CRASH_FUNCTION_NAME_2, .print = " "STR_OFFSET           },
    [P_FT       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_FREG             },
    [P_FS       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_FREG             },
    [P_FD       ] = { .color = COLOR_RGBA32_CRASH_DISASM_REG,      .print = " "STR_FREG             },
    [P_FUNC     ] = { .color = COLOR_RGBA32_CRASH_FUNCTION_NAME,   .print = " "STR_FUNCTION         },
};

static enum InsnType get_insn_type(InsnData insn, const InsnTemplate** checkInsn) {
    enum InsnType insnType = INSN_TYPE_OPCODE;
    
    if (insn.cop_opcode == COP_OPCODE) { // COPz
        if (insn.cop_num < ARRAY_COUNT(insn_db_copz)) {
            *checkInsn = insn_db_copz[insn.cop_num];
            insnType = insn.cop_bit ? INSN_TYPE_FUNC : INSN_TYPE_COP;
            // if cop1 && insn.CO, has .fmt
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
    if (insn.raw != 0) {
        const InsnTemplate* checkInsn = insn_db;
        enum InsnType insnType = get_insn_type(insn, &checkInsn);
        _Bool check = FALSE;

        if (insnType != INSN_TYPE_ERROR) {
            while (checkInsn->paramType != PARAM_END) {
                switch (insnType) {
                    default:
                    case INSN_TYPE_OPCODE: check = (insn.opcode  == checkInsn->insn.opcode ); break; // First 6 bits
                    case INSN_TYPE_FUNC:   check = (insn.func    == checkInsn->insn.func   ); break; // Last 6 bits
                    case INSN_TYPE_REGIMM: check = (insn.regimm  == checkInsn->insn.regimm ); break; // The 5 bits after the first 11
                    case INSN_TYPE_COP:    check = (insn.fmt     == checkInsn->insn.fmt    ); break; // The 4 bits after the first 7
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
        for (u8 cmdIndex = 0; cmdIndex < ARRAY_COUNT(insn_print_formats[0]); cmdIndex++) {
            u8 curCmd = insn_print_formats[type->paramType][cmdIndex];
            if (curCmd == P_BRANCH) {
                return insn.offset;
            }
        }
    }

    return 0;
}

uintptr_t get_branch_target_from_addr(uintptr_t addr) {
    uintptr_t* addrptr = (uintptr_t*)addr;
    if (addrptr == NULL) {
        return addr;
    }
    InsnData insn = { .raw = *addrptr };

    if (insn.opcode == OPC_J || insn.opcode == OPC_JAL) {
        return PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(uintptr_t));
    }

    s16 branchOffset = check_for_branch_offset(insn);

    if (branchOffset) {
        return (addr + (((s16)branchOffset + 1) * sizeof(uintptr_t)));
    }

    return addr;
}

static char insn_as_string[CHAR_BUFFER_SIZE] = "";

static char cop1_fmt_to_char(InsnData insn) {
    switch (insn.fmt & ~COP1_FMT) {
        case COP1_FMT_SINGLE: return 'S'; // Single
        case COP1_FMT_DOUBLE: return 'D'; // Double
        case COP1_FMT_WORD:   return 'W'; // Word
        case COP1_FMT_LONG:   return 'L'; // Long
    }

    return 'X'; // Unknown
}

char* insn_disasm(InsnData insn, const char** fname, _Bool showDestNames) {
    char* strp = &insn_as_string[0];
    _Bool unimpl = FALSE;
    char insn_name[sizeof((InsnTemplate*)0)->name + 2] = "";

    bzero(insn_as_string, sizeof(insn_as_string));

    if (insn.raw == 0) { // Trivial case.
        strp += sprintf(strp, (STR_COLOR_PREFIX STR_NOP), COLOR_RGBA32_CRASH_DISASM_NOP); // Technically a pseudo-instruction of SLL.

        return insn_as_string;
    }

    const InsnTemplate* type = get_insn(insn);

    if (type != NULL) {
        RGBA32 color = COLOR_RGBA32_NONE;

        for (u8 cmdIndex = 0; cmdIndex < ARRAY_COUNT(insn_print_formats[0]); cmdIndex++) {
            enum ParamParts curCmd = insn_print_formats[type->paramType][cmdIndex];
            const InsnPrintFormatData* data = &insn_print_format_data[curCmd];

            if (curCmd == P_NULL) {
                break;
            }

            if (color != data->color) {
                color = data->color;
                strp += sprintf(strp, "@%08X", color);
            }

            switch (curCmd) {
                case P_NAME:
                    strp += sprintf(strp, data->print, type->name);
                    break;
                case P_NAME_FMT:
                    bzero(insn_name, sizeof(insn_name));
                    sprintf(insn_name, data->print, type->name, cop1_fmt_to_char(insn));
                    strp += sprintf(strp, insn_print_format_data[P_NAME].print, insn_name);
                    break;
                case P_RS:
                    strp += sprintf(strp, data->print, sRegisterNames[insn.rs]);
                    break;
                case P_RT:
                    strp += sprintf(strp, data->print, sRegisterNames[insn.rt]);
                    break;
                case P_RD:
                    strp += sprintf(strp, data->print, sRegisterNames[insn.rd]);
                    break;
                case P_SHIFT:
                    strp += sprintf(strp, data->print, insn.sa);
                    break;
                case P_BASE:
                    strp += sprintf(strp, data->print, sRegisterNames[insn.base]);
                    break;
                case P_IMM:
                    strp += sprintf(strp, data->print, insn.immediate);
                    break;
                case P_BRANCH:;
                    s16 branchOffset = (insn.offset + 1);
                    strp += sprintf(strp, data->print, ((branchOffset < 0) ? '-' : '+'), abss(branchOffset));
                    break;
                case P_FT:
                    strp += sprintf(strp, data->print, insn.ft);
                    break;
                case P_FS:
                    strp += sprintf(strp, data->print, insn.fs);
                    break;
                case P_FD:
                    strp += sprintf(strp, data->print, insn.fd);
                    break;
                case P_FUNC:;
                    uintptr_t target = PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(uintptr_t));
#ifdef INCLUDE_DEBUG_MAP
                    if (showDestNames) {
                        *fname = parse_map_exact(target);

                        if (*fname != NULL) {
                            break;
                        }
                    }
#endif
                    strp += sprintf(strp, data->print, target);
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
        strp += sprintf(strp, (STR_UNIMPLEMENTED" "STR_HEX_WORD), insn.raw);
    }

    return insn_as_string;
}
