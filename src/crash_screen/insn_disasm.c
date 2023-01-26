#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "color_presets.h"
#include "crash_screen.h"
#include "crash_screen_print.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "engine/math_util.h"


// MIPS III Instructions
ALIGNED32 static const InsnTemplate insn_db[] = {
    INSN_ID_1(OPC_SPEC, 0b00000, 0b00000, 0b00000, 0b00000, 0b000000, PARAM_NOP, "NOP"    ) // NOP
//                COP1,     fmt,      rt,      fs,      fd,      MOV
    // Floating point
    // branch
    INSN_ID_1(OPC_COP1, 0b01000, 0b00000,       0,       0,        0, PARAM_BC1, "BC1F"   ) // Branch on FP False
    INSN_ID_1(OPC_COP1, 0b01000, 0b00001,       0,       0,        0, PARAM_BC1, "BC1T"   ) // Branch on FP True
    INSN_ID_1(OPC_COP1, 0b01000, 0b00010,       0,       0,        0, PARAM_BC1, "BC1FL"  ) // Branch on FP False Likely
    INSN_ID_1(OPC_COP1, 0b01000, 0b00011,       0,       0,        0, PARAM_BC1, "BC1TL"  ) // Branch on FP True Likely
    // arithmetic
    INSN_ID_1(OPC_COP1,       0,       0,       0,       0, 0b000000, PARAM_FFF, "ADD"    ) // ADD.fmt Floating-Point Add
    INSN_ID_1(OPC_COP1,       0,       0,       0,       0, 0b000001, PARAM_FFF, "SUB"    ) // SUB.fmt Floating-Point Subtract
    INSN_ID_1(OPC_COP1,       0,       0,       0,       0, 0b000010, PARAM_FFF, "MUL"    ) // MUL.fmt Floating-Point Multiply
    INSN_ID_1(OPC_COP1,       0,       0,       0,       0, 0b000011, PARAM_FFF, "DIV"    ) // DIV.fmt Floating-Point Divide
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b000100, PARAM_FF,  "SQRT"   ) // SQRT.fmt Floating-Point Square Root
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b000101, PARAM_FF,  "ABS"    ) // ABS.fmt Floating-Point Absolute Value
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b000110, PARAM_FF,  "MOV"    ) // MOV.fmt Floating-Point Move
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b000111, PARAM_FF,  "NEG"    ) // NEG.fmt Floating-Point Negate
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001000, PARAM_FF,  "ROUND.L") // ROUND.L.fmt Floating-Point Round to Long Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001001, PARAM_FF,  "TRUNC.L") // TRUNC.L.fmt Floating-Point Truncate to Long Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001010, PARAM_FF,  "CEIL.L" ) // CEIL.L.fmt Floating-Point Ceiling to Long Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001011, PARAM_FF,  "FLOOR.L") // FLOOR.L.fmt Floating-Point Floor to Long Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001100, PARAM_FF,  "ROUND.W") // ROUND.W.fmt Floating-Point Round to Word Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001101, PARAM_FF,  "TRUNC.W") // TRUNC.W.fmt Floating-Point Truncate to Word Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001110, PARAM_FF,  "CEIL.W" ) // CEIL.W.fmt Floating-Point Ceiling to Word Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b001111, PARAM_FF,  "FLOOR.W") // FLOOR.W.fmt Floating-Point Floor to Word Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b100000, PARAM_FF,  "CVT.S"  ) // CVT.S.fmt Floating-Point Convert to Single Floating-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b100001, PARAM_FF,  "CVT.D"  ) // CVT.D.fmt Floating-Point Convert to Double Floating-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b100100, PARAM_FF,  "CVT.W"  ) // CVT.W.fmt Floating-Point Convert to Word Fixed-Point
    INSN_ID_1(OPC_COP1,       0, 0b00000,       0,       0, 0b100101, PARAM_FF,  "CVT.L"  ) // CVT.L.fmt Floating-Point Convert to Long Fixed-Point
    INSN_ID_1(OPC_COP1,       0,       0,       0, 0b00000, 0b110000, PARAM_CON, "C"      ) // C.cond.fmt Floating-Point Compare
    // memory access
    INSN_ID_1(OPC_COP1, 0b00000,       0,       0, 0b00000, 0b000000, PARAM_TFS, "MFC1"   ) // Move Word From Floating-Point
    INSN_ID_0(OPC_COP1, 0b00001,       0,       0, 0b00000, 0b000000, PARAM_TFS, "DMFC1"  ) // Doubleword Move From Floating-Point
    INSN_ID_1(OPC_COP1, 0b00100,       0,       0, 0b00000, 0b000000, PARAM_TFS, "MTC1"   ) // Move Word To Floating-Point
    INSN_ID_0(OPC_COP1, 0b00101,       0,       0, 0b00000, 0b000000, PARAM_TFS, "DMTC1"  ) // Doubleword Move To Floating-Point
    INSN_ID_0(OPC_COP1, 0b00010,       0,       0, 0b00000, 0b000000, PARAM_TFS, "CFC1"   ) // Move Control Word From Floating-Point
    INSN_ID_0(OPC_COP1, 0b00110,       0,       0, 0b00000, 0b000000, PARAM_TFS, "CTC1"   ) // Move Control Word To Floating-Point
//              opcode,      rs,      rt,      rd,      sa, function
    // Arithmetic
    // add
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0, 0b00000, 0b100000, PARAM_TD,  "MOVE"   ) // Move (pseudo of ADD)
    INSN_ID_0(OPC_SPEC,       0, 0b00000,       0, 0b00000, 0b100000, PARAM_SD,  "MOVE"   ) // Move (pseudo of ADD)
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b100000, PARAM_DST, "ADD"    ) // Add Word
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100001, PARAM_DST, "ADDU"   ) // Add Unsigned Word
    INSN_ID_0(0b001000,       0,       0,       0,       0,        0, PARAM_TSI, "ADDI"   ) // Add Immediate Word
    INSN_ID_1(0b001001,       0,       0,       0,       0,        0, PARAM_TSI, "ADDIU"  ) // Add Immediate Unsigned Word
    // sub
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100010, PARAM_DST, "SUB"    ) // Subtract Word
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100011, PARAM_DST, "SUBU"   ) // Subtract Unsigned Word
    // and
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100100, PARAM_DST, "AND"    ) // And
    INSN_ID_1(0b001100,       0,       0,       0,       0,        0, PARAM_TSI, "ANDI"   ) // And Immediate
    // or
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100101, PARAM_DST, "OR"     ) // Or
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100110, PARAM_DST, "XOR"    ) // Exclusive Or
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b100111, PARAM_DST, "NOR"    ) // Nor
    INSN_ID_1(0b001101,       0,       0,       0,       0,        0, PARAM_TSI, "ORI"    ) // Or Immediate
    INSN_ID_1(0b001110,       0,       0,       0,       0,        0, PARAM_TSI, "XORI"   ) // Exclusive Or Immediate
    // lui
    INSN_ID_1(0b001111,       0,       0,       0,       0,        0, PARAM_TI,  "LUI"    ) // Load Upper Immediate
    // set
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b101010, PARAM_DST, "SLT"    ) // Set on Less Than
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b101011, PARAM_DST, "SLTU"   ) // Set on Less Than Unsigned
    INSN_ID_1(0b001010,       0,       0,       0,       0,        0, PARAM_TSI, "SLTI"   ) // Set on Less Than Immediate
    INSN_ID_1(0b001011,       0,       0,       0,       0,        0, PARAM_TSI, "SLTIU"  ) // Set on Less Than Immediate Unsigned
    // doubleword add
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b101100, PARAM_DST, "DADD"   ) // Doubleword Add
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b101101, PARAM_DST, "DADDU"  ) // Doubleword Add Unsigned
    INSN_ID_0(0b011000,       0,       0,       0,       0,        0, PARAM_TSI, "DADDI"  ) // Doubleword Add Immediate
    INSN_ID_0(0b011001,       0,       0,       0,       0,        0, PARAM_TSI, "DADDIU" ) // Doubleword Add Immediate Unsigned
    // dooubleword sub
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b101110, PARAM_DST, "DSUB"   ) // Doubleword Subtract
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b101111, PARAM_DST, "DSUBU"  ) // Doubleword Subtract Unsigned

    // shift
    INSN_ID_1(OPC_SPEC, 0b00000,       0,       0,       0, 0b000000, PARAM_DTA, "SLL"    ) // Shift Word Left Logical
    INSN_ID_1(OPC_SPEC, 0b00000,       0,       0,       0, 0b000010, PARAM_DTA, "SRL"    ) // Shift Word Right Logical
    INSN_ID_1(OPC_SPEC, 0b00000,       0,       0,       0, 0b000011, PARAM_DTA, "SRA"    ) // Shift Word Right Arithmetic
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b000100, PARAM_DTS, "SLLV"   ) // Shift Word Left Logical Variable
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b000110, PARAM_DTS, "SRLV"   ) // Shift Word Right Logical Variable
    INSN_ID_1(OPC_SPEC,       0,       0,       0, 0b00000, 0b000111, PARAM_DTS, "SRAV"   ) // Shift Word Right Arithmetic Variable
    // doubleword shift
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111000, PARAM_DTA, "DSLL"   ) // Doubleword Shift Left Logical
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111010, PARAM_DTA, "DSRL"   ) // Doubleword Shift Right Logical
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111011, PARAM_DTA, "DSRA"   ) // Doubleword Shift Right Arithmetic
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111100, PARAM_DTA, "DSLL32" ) // Doubleword Shift Left Logical + 32
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111110, PARAM_DTA, "DSRL32" ) // Doubleword Shift Right Logical + 32
    INSN_ID_0(OPC_SPEC, 0b00000,       0,       0,       0, 0b111111, PARAM_DTA, "DSRA32" ) // Doubleword Shift Right Arithmetic + 32
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b010100, PARAM_DTS, "DSLLV"  ) // Doubleword Shift Left Logical Variable
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b010110, PARAM_DTS, "DSRLV"  ) // Doubleword Shift Right Logical Variable
    INSN_ID_0(OPC_SPEC,       0,       0,       0, 0b00000, 0b010111, PARAM_DTS, "DSRAV"  ) // Doubleword Shift Right Arithmetic Variable

    // Multiply
    // move hi/lo
    INSN_ID_1(OPC_SPEC, 0b00000, 0b00000,       0, 0b00000, 0b010000, PARAM_D,   "MFHI"   ) // Move From HI
    INSN_ID_1(OPC_SPEC,       0, 0b00000, 0b00000, 0b00000, 0b010001, PARAM_S,   "MTHI"   ) // Move To HI
    INSN_ID_1(OPC_SPEC, 0b00000, 0b00000,       0, 0b00000, 0b010010, PARAM_D,   "MFLO"   ) // Move From LO
    INSN_ID_1(OPC_SPEC,       0, 0b00000, 0b00000, 0b00000, 0b010011, PARAM_S,   "MTLO"   ) // Move To LO
    // mult
    INSN_ID_1(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011000, PARAM_ST,  "MULT"   ) // Multiply Word
    INSN_ID_1(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011001, PARAM_ST,  "MULTU"  ) // Multiply Unsigned Word
    // div
    INSN_ID_1(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011010, PARAM_ST,  "DIV"    ) // Divide Word
    INSN_ID_1(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011011, PARAM_ST,  "DIVU"   ) // Divide Unsigned Word
    // doubleword mult
    INSN_ID_0(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011100, PARAM_ST,  "DMULT"  ) // Doubleword Multiply
    INSN_ID_0(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011101, PARAM_ST,  "DMULTU" ) // Doubleword Multiply Unsigned
    // doubleword div
    INSN_ID_0(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011110, PARAM_ST,  "DDIV"   ) // Doubleword Divide
    INSN_ID_0(OPC_SPEC,       0,       0, 0b00000, 0b00000, 0b011111, PARAM_ST,  "DDIVU"  ) // Doubleword Divide Unsigned

    // branch
    INSN_ID_1(OPC_REGI,       0, 0b00000,       0,       0,        0, PARAM_SO,  "BLTZ"   ) // Branch on Less Than Zero
    INSN_ID_1(OPC_REGI,       0, 0b00001,       0,       0,        0, PARAM_SO,  "BGEZ"   ) // Branch on Greater Than or Equal to Zero
    INSN_ID_1(OPC_REGI,       0, 0b00010,       0,       0,        0, PARAM_SO,  "BLTZL"  ) // Branch on Less Than Zero Likely
    INSN_ID_1(OPC_REGI,       0, 0b00011,       0,       0,        0, PARAM_SO,  "BGEZL"  ) // Branch on Greater Than or Equal to Zero Likely
    INSN_ID_0(OPC_REGI,       0, 0b10000,       0,       0,        0, PARAM_SO,  "BLTZAL" ) // Branch on Less Than Zero and Link
    INSN_ID_0(OPC_REGI,       0, 0b10001,       0,       0,        0, PARAM_SO,  "BGEZAL" ) // Branch on Greater Than or Equal to Zero and Link
    INSN_ID_0(OPC_REGI,       0, 0b10010,       0,       0,        0, PARAM_SO,  "BLTZALL") // Branch on Less Than Zero and Link Likely
    INSN_ID_0(OPC_REGI,       0, 0b10011,       0,       0,        0, PARAM_SO,  "BGEZALL") // Branch on Greater Than or Equal to Zero and Link Likely
    INSN_ID_1(0b000100, 0b00000, 0b00000,       0,       0,        0, PARAM_O,   "B"      ) // Branch (pseudo of BEQ)
    INSN_ID_1(0b000100, 0b00000,       0,       0,       0,        0, PARAM_SO,  "BEQZ"   ) // Branch on Equal to Zero (pseudo of BEQ)
    INSN_ID_1(0b000100,       0,       0,       0,       0,        0, PARAM_STO, "BEQ"    ) // Branch on Equal
    INSN_ID_1(0b000101, 0b00000,       0,       0,       0,        0, PARAM_SO,  "BNEZ"   ) // Branch on Not Equal to Zero (pseudo of BNE)
    INSN_ID_1(0b000101,       0,       0,       0,       0,        0, PARAM_STO, "BNE"    ) // Branch on Not Equal
    INSN_ID_1(0b000110,       0, 0b00000,       0,       0,        0, PARAM_SO,  "BLEZ"   ) // Branch on Less Than or Equal to Zero
    INSN_ID_1(0b000111,       0, 0b00000,       0,       0,        0, PARAM_SO,  "BGTZ"   ) // Branch on Greater Than Zero
    INSN_ID_1(0b010100, 0b00000,       0,       0,       0,        0, PARAM_SO,  "BEQZL"  ) // Branch on Equal to Zero Likely (pseudo of BEQZL)
    INSN_ID_1(0b010100,       0,       0,       0,       0,        0, PARAM_STO, "BEQL"   ) // Branch on Equal Likely
    INSN_ID_1(0b010101, 0b00000,       0,       0,       0,        0, PARAM_SO,  "BNEZL"  ) // Branch on Not Equal to Zero Likely (pseudo of BNEL)
    INSN_ID_1(0b010101,       0,       0,       0,       0,        0, PARAM_STO, "BNEL"   ) // Branch on Not Equal Likely
    INSN_ID_1(0b010110,       0, 0b00000,       0,       0,        0, PARAM_SO,  "BLEZL"  ) // Branch on Less Than or Equal to Zero Likely
    INSN_ID_1(0b010111,       0, 0b00000,       0,       0,        0, PARAM_SO,  "BGTZL"  ) // Branch on Greater Than Zero Likely
    // return
    INSN_ID_0(OPC_COP0, 0b10000, 0b00000, 0b00000, 0b00000, 0b011000, PARAM_SYS, "ERET"   ) // Return from interrupt, exception, or error exception
    // TLB
    INSN_ID_0(OPC_COP0, 0b10000, 0b00000, 0b00000, 0b00000, 0b001000, PARAM_SYS, "TLBP"   ) // Searches for a TLB entry that matches the EntryHi register
    INSN_ID_0(OPC_COP0, 0b10000, 0b00000, 0b00000, 0b00000, 0b000001, PARAM_SYS, "TLBR"   ) // Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0, 0b10000, 0b00000, 0b00000, 0b00000, 0b000010, PARAM_SYS, "TLBWI"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
    INSN_ID_0(OPC_COP0, 0b10000, 0b00000, 0b00000, 0b00000, 0b000110, PARAM_SYS, "TLBWR"  ) // Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register
    // jal (special)
    INSN_ID_1(0b000010,       0,       0,       0,       0,        0, PARAM_J,   "J"      ) // Jump
    INSN_ID_1(0b000011,       0,       0,       0,       0,        0, PARAM_J,   "JAL"    ) // Jump and Link
    INSN_ID_1(OPC_SPEC,       0, 0b00000, 0b00000, 0b00000, 0b001000, PARAM_S,   "JR"     ) // Jump Register
    INSN_ID_1(OPC_SPEC,       0, 0b00000,       0, 0b00000, 0b001001, PARAM_DS,  "JALR"   ) // Jump and Link Register
    // move
    INSN_ID_0(OPC_COP0, 0b00000,       0,       0, 0b00000, 0b000000, PARAM_TD,  "MFC0"   ) // Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0, 0b00001,       0,       0, 0b00000, 0b000000, PARAM_TD,  "DMFC0"  ) // Doubleword Move from System Control Coprocessor
    INSN_ID_0(OPC_COP0, 0b00100,       0,       0, 0b00000, 0b000000, PARAM_TD,  "MTC0"   ) // Move to System Control Coprocessor
    INSN_ID_0(OPC_COP0, 0b00101,       0,       0, 0b00000, 0b000000, PARAM_TD,  "DMTC0"  ) // Doubleword Move to System Control Coprocessor
    // break
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b001101, PARAM_SYS, "BREAK"  ) // Breakpoint
    // system call (assert)
    INSN_ID_1(OPC_SPEC, 0b00000, 0b00000, 0b00000, 0b00000, 0b001100, PARAM_SYS, "SYSCALL") // System Call
    // sync
    INSN_ID_0(OPC_SPEC, 0b00000, 0b00000, 0b00000,       0, 0b001111, PARAM_SYN, "SYNC"   ) // Synchronize Shared Memory
    // cache
    INSN_ID_0(0b101111,       0,       0,       0,       0,        0, PARAM_TOS, "CACHE"  ) // https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html
    // trap
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b110000, PARAM_ST2, "TGE"    ) // Trap if Greater Than or Equal
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b110001, PARAM_ST2, "TGEU"   ) // Trap if Greater Than or Equal Unsigned
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b110010, PARAM_ST2, "TLT"    ) // Trap if Less Than
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b110011, PARAM_ST2, "TLTU"   ) // Trap if Less Than Unsigned
    INSN_ID_1(OPC_SPEC,       0,       0,       0,       0, 0b110100, PARAM_ST2, "TEQ"    ) // Trap if Equal
    INSN_ID_0(OPC_SPEC,       0,       0,       0,       0, 0b110110, PARAM_ST2, "TNE"    ) // Trap if Not Equal
    INSN_ID_0(OPC_REGI,       0, 0b01000,       0,       0,        0, PARAM_SI,  "TGEI"   ) // Trap if Greater Than or Equal Immediate
    INSN_ID_0(OPC_REGI,       0, 0b01001,       0,       0,        0, PARAM_SI,  "TGEIU"  ) // Trap if Greater Than or Equal Unsigned Immediate
    INSN_ID_0(OPC_REGI,       0, 0b01010,       0,       0,        0, PARAM_SI,  "TLTI"   ) // Trap if Less Than Immediate
    INSN_ID_0(OPC_REGI,       0, 0b01011,       0,       0,        0, PARAM_SI,  "TLTIU"  ) // Trap if Less Than Unsigned Immediate
    INSN_ID_0(OPC_REGI,       0, 0b01100,       0,       0,        0, PARAM_SI,  "TEQI"   ) // Trap if Equal Immediate
    INSN_ID_0(OPC_REGI,       0, 0b01110,       0,       0,        0, PARAM_SI,  "TNEI"   ) // Trap if Not Equal Immediate

    // Memory Access
    INSN_ID_0(0b011010,       0,       0,       0,       0,        0, PARAM_TOS, "LDL"    ) // Load Doubleword Left
    INSN_ID_0(0b011011,       0,       0,       0,       0,        0, PARAM_TOS, "LDR"    ) // Load Doubleword Right
    INSN_ID_1(0b100000,       0,       0,       0,       0,        0, PARAM_TOS, "LB"     ) // Load Byte
    INSN_ID_1(0b100001,       0,       0,       0,       0,        0, PARAM_TOS, "LH"     ) // Load Halfword
    INSN_ID_1(0b100010,       0,       0,       0,       0,        0, PARAM_TOS, "LWL"    ) // Load Word Left
    INSN_ID_1(0b100011,       0,       0,       0,       0,        0, PARAM_TOS, "LW"     ) // Load Word
    INSN_ID_1(0b100100,       0,       0,       0,       0,        0, PARAM_TOS, "LBU"    ) // Load Byte Unsigned
    INSN_ID_1(0b100101,       0,       0,       0,       0,        0, PARAM_TOS, "LHU"    ) // Load Halfword Unsigned
    INSN_ID_1(0b100110,       0,       0,       0,       0,        0, PARAM_TOS, "LWR"    ) // Load Word Right
    INSN_ID_0(0b100111,       0,       0,       0,       0,        0, PARAM_TOS, "LWU"    ) // Load Word Unsigned
    INSN_ID_1(0b101000,       0,       0,       0,       0,        0, PARAM_TOS, "SB"     ) // Store Byte
    INSN_ID_1(0b101001,       0,       0,       0,       0,        0, PARAM_TOS, "SH"     ) // Store Halfword
    INSN_ID_0(0b101010,       0,       0,       0,       0,        0, PARAM_TOS, "SWL"    ) // Store Word Left
    INSN_ID_1(0b101011,       0,       0,       0,       0,        0, PARAM_TOS, "SW"     ) // Store Word
    INSN_ID_0(0b101100,       0,       0,       0,       0,        0, PARAM_TOS, "SDL"    ) // Store Doubleword Left
    INSN_ID_0(0b101101,       0,       0,       0,       0,        0, PARAM_TOS, "SDR"    ) // Store Doubleword Right
    INSN_ID_0(0b101110,       0,       0,       0,       0,        0, PARAM_TOS, "SWR"    ) // Store Word Right
    INSN_ID_0(0b110000,       0,       0,       0,       0,        0, PARAM_TOS, "LL"     ) // Load Linked Word
    INSN_ID_1(0b110001,       0,       0,       0,       0,        0, PARAM_FOS, "LWC1"   ) // Load Word to Floating-Point
    INSN_ID_0(0b110010,       0,       0,       0,       0,        0, PARAM_FOS, "LWC2"   ) // Load Word to RCP
    INSN_ID_0(0b110011,       0,       0,       0,       0,        0, PARAM_FOS, "LWC3"   ) // Load Word to Coprocessor-3
    INSN_ID_0(0b110100,       0,       0,       0,       0,        0, PARAM_TOS, "LLD"    ) // Load Linked Doubleword
    INSN_ID_1(0b110101,       0,       0,       0,       0,        0, PARAM_TOS, "LDC1"   ) // Load Doubleword to Floating-Point
    INSN_ID_0(0b110110,       0,       0,       0,       0,        0, PARAM_TOS, "LDC2"   ) // Load Doubleword to RCP
    INSN_ID_0(0b110111,       0,       0,       0,       0,        0, PARAM_TOS, "LD"     ) // Load Doubleword
    INSN_ID_0(0b111000,       0,       0,       0,       0,        0, PARAM_TOS, "SC"     ) // Store Conditional Word
    INSN_ID_1(0b111001,       0,       0,       0,       0,        0, PARAM_FOS, "SWC1"   ) // Store Word to Floating-Point
    INSN_ID_0(0b111010,       0,       0,       0,       0,        0, PARAM_FOS, "SWC2"   ) // Store Word from RCP
    INSN_ID_0(0b111011,       0,       0,       0,       0,        0, PARAM_FOS, "SWC3"   ) // Store Word from Coprocessor-3
    INSN_ID_0(0b111100,       0,       0,       0,       0,        0, PARAM_TOS, "SCD"    ) // Store Conditional Doubleword
    INSN_ID_1(0b111101,       0,       0,       0,       0,        0, PARAM_TOS, "SDC1"   ) // Store Doubleword to Floating-Point
    INSN_ID_0(0b111110,       0,       0,       0,       0,        0, PARAM_TOS, "SDC2"   ) // Store Doubleword from RCP
    INSN_ID_0(0b111111,       0,       0,       0,       0,        0, PARAM_TOS, "SD"     ) // Store Doubleword
    // Coprocessor
    INSN_ID_0(OPC_COP0,       0,       0,       0,       0,        0, PARAM_N,   "COP0"   ) // Coprocessor-0 Operation (System Control Coprocessor)
    INSN_ID_0(OPC_COP1,       0,       0,       0,       0,        0, PARAM_N,   "COP1"   ) // Coprocessor-1 Operation (Floating-Point Unit)
    INSN_ID_0(OPC_COP2,       0,       0,       0,       0,        0, PARAM_N,   "COP2"   ) // Coprocessor-2 Operation (Reality Co-Processor)
    INSN_ID_0(OPC_COP3,       0,       0,       0,       0,        0, PARAM_N,   "COP3"   ) // Coprocessor-3 Operation (CP3)
};

static const InsnData insn_masks[] = {
//                       opcode,      rs,      rt,      rd,      sa, function
    [PARAM_NOP] = {.i={0b111111, 0b11111, 0b11111, 0b11111, 0b11111, 0b111111}}, // NOP
    [PARAM_N  ] = {.i={0b111111, 0b11111, 0b11111, 0b11111,       0, 0b111111}}, //
    [PARAM_SYS] = {.i={0b111111,       0,       0,       0,       0, 0b111111}}, //
    [PARAM_SYN] = {.i={0b111111, 0b11111, 0b11111, 0b11111, 0b11111, 0b111111}}, //
    [PARAM_S  ] = {.i={0b111111,       0, 0b11111, 0b11111, 0b11111, 0b111111}}, // rs
    [PARAM_T  ] = {.i={0b111111, 0b11111,       0, 0b11111, 0b11111, 0b111111}}, // rt
    [PARAM_D  ] = {.i={0b111111, 0b11111, 0b11111,       0, 0b11111, 0b111111}}, // rd
    [PARAM_ST ] = {.i={0b111111,       0,       0, 0b11111, 0b11111, 0b111111}}, // rs, rt
    [PARAM_ST2] = {.i={0b111111,       0,       0,       0,       0, 0b111111}}, // rs, rt
    [PARAM_DS ] = {.i={0b111111,       0, 0b11111,       0, 0b11111, 0b111111}}, // rd, rs
    [PARAM_TD ] = {.i={0b111111, 0b11111,       0,       0, 0b11111, 0b111111}}, // rt, rd
    [PARAM_SD ] = {.i={0b111111,       0, 0b11111,       0, 0b11111, 0b111111}}, // rs, rd
    [PARAM_STD] = {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}}, // rs, rt, rd
    [PARAM_SDT] = {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}}, // rs, rd, rt
    [PARAM_DST] = {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}}, // rd, rs, rt
    [PARAM_DTS] = {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}}, // rd, rt, rs
    [PARAM_DTA] = {.i={0b111111, 0b11111,       0,       0,       0, 0b111111}}, // rd, rt, shift
    [PARAM_SI ] = {.i={0b111111,       0, 0b11111,       0,       0,        0}}, // rs, 0xI
    [PARAM_TI ] = {.i={0b111111, 0b11111,       0,       0,       0,        0}}, // rt, 0xI
    [PARAM_STI] = {.i={0b111111,       0,       0,       0,       0,        0}}, // rs, rt, 0xI
    [PARAM_TSI] = {.i={0b111111,       0,       0,       0,       0,        0}}, // rt, rs, 0xI
    [PARAM_TOS] = {.i={0b111111,       0,       0,       0,       0,        0}}, // rt, 0xI(rs)
    [PARAM_SO ] = {.i={0b111111,       0, 0b11111,       0,       0,        0}}, // rs, offset
    [PARAM_STO] = {.i={0b111111,       0,       0,       0,       0,        0}}, // rs, rt, offset
    [PARAM_O  ] = {.i={0b111111, 0b11111, 0b11111,       0,       0,        0}}, // offset
    [PARAM_J  ] = {.i={0b111111,       0,       0,       0,       0,        0}}, // func
//                         COP1,    fmt,       rt,      fs,      fd,      MOV
    [PARAM_FOS] = {.i={0b111111,       0,       0,       0,       0,        0}}, // ft, 0xI(rs)
    [PARAM_TFS] = {.i={0b111111, 0b11111,       0,       0, 0b11111, 0b111111}}, // rt, fs
    [PARAM_FF ] = {.i={0b111111,       0, 0b11111,       0,       0, 0b111111}}, // fd, fs
    [PARAM_FFF] = {.i={0b111111,       0,       0,       0,       0, 0b111111}}, // fd, fs, ft
    [PARAM_CON] = {.i={0b111111,       0,       0,       0, 0b00011, 0b110000}}, // fs, ft
    [PARAM_BC1] = {.i={0b111111, 0b11111, 0b00011,       0,       0,        0}}, // offset
};

// C.cond.fmt
static const char sFPUconditions[][5] = {
    "F",    // False
    "UN",   // Unordered
    "EQ",   // Equal
    "UEQ",  // Unordered or Equal
    "OLT",  // Ordered Less Than
    "ULT",  // Unordered or Less Than
    "OLE",  // Ordered or Less Than or Equal
    "ULE",  // Unordered or Less Than or Equal
    "SF",   // Signaling False
    "NGLE", // Not Greater or Less Than or Equal
    "SEQ",  // Signalling Equal
    "NGL",  // Not Greater or Less Than
    "LT",   // Less Than
    "NGE",  // Not Greater Than or Equal
    "LE",   // Less Than or Equal
    "NGT"   // Not Greater Than
};

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
};

// // FPU Registers
// static const char sFloatRegisterNames[][3] = {
//     "00", "02",                                     // Subroutine return value
//     "04", "06", "08", "10",                         // Temporary values
//     "12", "14",                                     // Subroutine arguments
//     "16", "18",                                     // Temporary values
//     "20", "22", "24", "26", "28", "30",             // Saved Values
// };

// Print colors
static const RGBA32 sDisasmColors[] = {
    [DISASM_COLOR_NOP      ] = COLOR_RGBA32_CRASH_DISASM_NOP,
    [DISASM_COLOR_INSN     ] = COLOR_RGBA32_CRASH_DISASM_INST,
    [DISASM_COLOR_REG      ] = COLOR_RGBA32_CRASH_DISASM_REG,
    [DISASM_COLOR_REG_2    ] = COLOR_RGBA32_CRASH_DISASM_REG_2,
    [DISASM_COLOR_IMMEDIATE] = COLOR_RGBA32_CRASH_IMMEDIATE,
    [DISASM_COLOR_ADDRESS  ] = COLOR_RGBA32_CRASH_FUNCTION_NAME,
    [DISASM_COLOR_OFFSET   ] = COLOR_RGBA32_CRASH_FUNCTION_NAME_2,
};

static char insn_as_string[CHAR_BUFFER_SIZE];

static char fmt_to_char(InsnData insn) {
    switch (insn.i.rs) {
        case FMT_SINGLE:     return 'S'; // Single
        case FMT_DOUBLEWORD: return 'D'; // Doubleword
        case FMT_WORD:       return 'W'; // Word
        case FMT_LONG:       return 'L'; // Long
    }

    return 'X'; // Unknown
}

const InsnTemplate *get_insn_type(InsnData insn) {
    if (insn.d != 0) {
        const InsnTemplate *checkInsn = insn_db;
        for (s32 i = 0; i < ARRAY_COUNT(insn_db); i++) {
            if ((insn.d & insn_masks[checkInsn->paramType].d) == checkInsn->i.d) {
                return checkInsn;
            }
            checkInsn++;
        }
    }

    return NULL;
}

s32 get_branch_offset(InsnData insn) {
    const InsnTemplate *type = get_insn_type(insn);

    if (type) {
        switch (type->paramType) {
            case PARAM_SO:
            case PARAM_STO:
            case PARAM_O:
            case PARAM_BC1:
                return (s16)insn.i.immediate;
        }
    }

    return 0;
}

uintptr_t get_branch_target_from_addr(uintptr_t addr) {
    InsnData insn;
    insn.d = *(uintptr_t *)addr;
    const InsnTemplate *type = get_insn_type(insn);

    if (type) {
        switch (type->paramType) {
            case PARAM_SO:
            case PARAM_STO:
            case PARAM_O:
            case PARAM_BC1:
                addr = (addr + (((s16)insn.i.immediate + 1) * DISASM_STEP));
                break;
            case PARAM_J:
                addr = (0x80000000 | ((insn.d & BITMASK(25)) * DISASM_STEP));
        }
    }

    return addr;
}

char *insn_disasm(InsnData insn, u32 isPC) {
    const InsnTemplate *type = NULL;
    char *strp = &insn_as_string[0];
    uintptr_t target;
    s16 branchOffset;
#ifdef INCLUDE_DEBUG_MAP
    char *fname = NULL;
#endif
    char insn_name[10];

    bzero(insn_as_string, sizeof(insn_as_string));
    bzero(insn_name, sizeof(insn_name));

    if (insn.d == 0) { // trivial case
        strp += sprintf(strp, "@%08XNOP", sDisasmColors[DISASM_COLOR_NOP]);
    } else {
        type = get_insn_type(insn);
        if (type) {
            switch (type->paramType) {
                case PARAM_N:
                case PARAM_SYS:
                case PARAM_SYN:
                    strp += sprintf(strp, "@%08X%-6s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name
                    );
                    break;
                case PARAM_S:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rs]
                    );
                    break;
                case PARAM_T:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rt]
                    );
                    break;
                case PARAM_D:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rdata.rd]
                    );
                    break;
                case PARAM_ST:
                case PARAM_ST2:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rs],
                                                          sRegisterNames[insn.i.rt]
                    );
                    break;
                case PARAM_DS:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rdata.rd],
                                                          sRegisterNames[insn.i.rs]
                    );
                    break;
                case PARAM_TD:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rt],
                                                          sRegisterNames[insn.i.rdata.rd]
                    );
                    break;
                case PARAM_SD:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rs],
                                                          sRegisterNames[insn.i.rdata.rd]
                    );
                    break;
                case PARAM_STD:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rs],
                                                          sRegisterNames[insn.i.rt],
                                                          sRegisterNames[insn.i.rdata.rd]
                    );
                    break;
                case PARAM_SDT:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rs],
                                                          sRegisterNames[insn.i.rdata.rd],
                                                          sRegisterNames[insn.i.rt]
                    );
                    break;
                case PARAM_DST:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rdata.rd],
                                                          sRegisterNames[insn.i.rs],
                                                          sRegisterNames[insn.i.rt]
                    );
                    break;
                case PARAM_DTS:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rdata.rd],
                                                          sRegisterNames[insn.i.rt],
                                                          sRegisterNames[insn.i.rs]
                    );
                    break;
                case PARAM_DTA:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rdata.rd],
                                                               sRegisterNames[insn.i.rt],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.rdata.sa
                    );
                    break;
                case PARAM_SI:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rs],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                    );
                    break;
                case PARAM_TI:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rt],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                    );
                    break;
                case PARAM_STI:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rs],
                                                               sRegisterNames[insn.i.rt],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                    );
                    break;
                case PARAM_TSI:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rt],
                                                               sRegisterNames[insn.i.rs],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                    );
                    break;
                case PARAM_TOS:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X@%08X(%s)",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], sRegisterNames[insn.i.rt],
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate,
                        sDisasmColors[DISASM_COLOR_REG_2    ], sRegisterNames[insn.i.rs]
                    );
                    break;
                case PARAM_SO:
                    branchOffset = (1 + insn.i.immediate);
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X%s0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                        sDisasmColors[DISASM_COLOR_REG   ], sRegisterNames[insn.i.rs],
                        sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), abss(branchOffset)
                    );
                    break;
                case PARAM_STO:
                    branchOffset = (1 + insn.i.immediate);
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X%s0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                        sDisasmColors[DISASM_COLOR_REG   ], sRegisterNames[insn.i.rs],
                                                            sRegisterNames[insn.i.rt],
                        sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), abss(branchOffset)
                    );
                    break;
                case PARAM_O:
                case PARAM_BC1:
                    branchOffset = (1 + insn.i.immediate);
                    strp += sprintf(strp, "@%08X%-6s @%08X%s0x%04X",
                        sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                        sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), abss(branchOffset)
                    );
                    break;
                case PARAM_J:
                    target = (0x80000000 | ((insn.d & BITMASK(25)) * DISASM_STEP));
                    strp += sprintf(strp, "@%08X%-6s @%08X0x%08X",
                        sDisasmColors[DISASM_COLOR_INSN   ], type->name,
                        sDisasmColors[DISASM_COLOR_ADDRESS], target
                    );
    #ifdef INCLUDE_DEBUG_MAP
                    //! TODO: use sShowRamAsAscii to toggle whether to show hex address or name?
                    fname = parse_map_exact(target);
                    if (fname != NULL) {
                        strp += sprintf(strp, " ^%d%s", 26, fname);
                    }
    #endif
                    break;
                case PARAM_FOS:
                    strp += sprintf(strp, "@%08X%-6s @%08XF%d, @%08X0x%04X@%08X(%s)",
                        sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                        sDisasmColors[DISASM_COLOR_REG      ], insn.i.rt,
                        sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate,
                        sDisasmColors[DISASM_COLOR_REG_2    ], sRegisterNames[insn.i.rs]
                    );
                    break;
                case PARAM_TFS:
                    strp += sprintf(strp, "@%08X%-6s @%08X%s, F%02d",
                        sDisasmColors[DISASM_COLOR_INSN], type->name,
                        sDisasmColors[DISASM_COLOR_REG ], sRegisterNames[insn.i.rt],
                                                          insn.i.rdata.rd // fs
                    );
                    break;
                case PARAM_FF:
                    sprintf(insn_name, "%s.%c", type->name, fmt_to_char(insn));
                    strp += sprintf(strp, "@%08X%-6s @%08XF%02d, F%02d",
                        sDisasmColors[DISASM_COLOR_INSN], insn_name,
                        sDisasmColors[DISASM_COLOR_REG ], insn.i.rdata.sa, // fd
                                                          insn.i.rdata.rd  // fs
                    );
                    break;
                case PARAM_FFF:
                    sprintf(insn_name, "%s.%c", type->name, fmt_to_char(insn));
                    strp += sprintf(strp, "@%08X%-6s @%08XF%d, F%02d, F%02d",
                        sDisasmColors[DISASM_COLOR_INSN], insn_name,
                        sDisasmColors[DISASM_COLOR_REG ], insn.i.rdata.sa, // fd
                                                          insn.i.rdata.rd, // fs
                                                          insn.i.rt        // ft
                    );
                    break;
                case PARAM_CON:
                    sprintf(insn_name, "%s.%s.%c", type->name, sFPUconditions[insn.i.rdata.function & BITMASK(4)], fmt_to_char(insn));
                    strp += sprintf(strp, "@%08X%-6s @%08XF%02d, F%02d",
                        sDisasmColors[DISASM_COLOR_INSN], insn_name,
                        sDisasmColors[DISASM_COLOR_REG ], insn.i.rdata.rd, // fd
                                                          insn.i.rt        // ft
                    );
                    break;
                case PARAM_UNK:
                default:
                    strp += sprintf(strp, "unimpl 0x%08X", insn.d);
                    break;
            }
        } else {
            strp += sprintf(strp, "unimpl 0x%08X", insn.d);
        }
    }

    if (isPC) {
        sprintf(strp, " @%08X<-- CRASH", COLOR_RGBA32_CRASH_AT);
    }

    return insn_as_string;
}
