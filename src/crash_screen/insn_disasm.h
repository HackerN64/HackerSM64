#pragma once

#include <ultra64.h>

#include "types.h"


#define INSN_NAME_DISPLAY_WIDTH 10

#define CHAR_P_NOP      '_'     // "NOP"
#define CHAR_P_NAME     '\''    // "[name]%-6"
#define CHAR_P_NAMEF    '\"'    // "[name].[fmt]%-6"
#define CHAR_P_RS       's'     // "[rs reg]"
#define CHAR_P_RT       't'     // "[rt reg]"
#define CHAR_P_RD       'd'     // "[rd reg]"
#define CHAR_P_IMM      'I'     // "0x[last 16 bits]"
#define CHAR_P_NIMM     'i'     // "[±]0x[[last 16 bits]]" (for SUBI pseudoinstruction)
#define CHAR_P_SHIFT    'a'     // "[shift]""
#define CHAR_P_BASE     '('     // "([rs reg])"
#define CHAR_P_BRANCH   'B'     // "[±]0x[last 16 bits]" + draw branch arrow
#define CHAR_P_FT       'T'     // "F[ft reg]"
#define CHAR_P_FS       'S'     // "F[fs reg]"
#define CHAR_P_FD       'D'     // "F[fd reg]"
#define CHAR_P_FUNC     'J'     // "[function address]" or parse map
#define CHAR_P_COP0D    '0'     // "[rd reg]" COP0 Special register

enum Coprocessors {
    CPU = -1,
    COP0, // Coprocessor-0 (System Control Coprocessor)
    COP1, // Coprocessor-1 (Floating-Point Unit)
    COP2, // Coprocessor-2 (Reality Co-Processor Vector Unit)
    COP3, // Coprocessor-3 (CP3)
};

#define COP_FROM    (0 * BIT(2)) // 0b00000 (move from)
#define COP_TO      (1 * BIT(2)) // 0b00100 (move to)
#define FMT_FLT     COP_FROM // 0b00000 (floating-point)
#define FMT_FIX     COP_TO   // 0b00100 (fixed-point)
#define FMT_CTL     BIT(1)

#define FMT_32b     (0 * BIT(0)) // 0b00000 (32-bit)
#define FMT_64b     (1 * BIT(0)) // 0b00001 (64-bit)

// fmt_to_char
enum COP1Formats {
    COP1_FMT_SINGLE = (FMT_FLT | FMT_32b), // 0b00000 (16): 32-bit floating-point
    COP1_FMT_DOUBLE = (FMT_FLT | FMT_64b), // 0b00001 (17): 64-bit floating-point
    COP1_FMT_WORD   = (FMT_FIX | FMT_32b), // 0b00100 (20): 32-bit fixed-point
    COP1_FMT_LONG   = (FMT_FIX | FMT_64b), // 0b00101 (21): 64-bit fixed-point
    COP1_FMT_CTL_F  = (COP_FROM | FMT_CTL | FMT_32b), // 0b00010  (2):
    COP1_FMT_CTL_T  = (COP_TO   | FMT_CTL | FMT_32b), // 0b00110  (6):
};

#define FMT_SIGNED   (0 * BIT(0))
#define FMT_UNSIGNED (1 * BIT(0))

#define COP_OPCODE 0b0100

// Regular opcodes
enum InsnOpCodes {
    OPC_SPECIAL = 0b000000, //  0: Use function (last 6 bits) as opcode
    OPC_REGIMM  = 0b000001, //  1: Use rt (skip 5 bits after opcode and the 5 bits after that) as opcode

    OPC_J       = 0b000010, //  2: Jump
    OPC_JAL     = 0b000011, //  3: Jump and Link
    OPC_BEQ     = 0b000100, //  4: Branch on Equal
    OPC_BNE     = 0b000101, //  5: Branch on Not Equal
    OPC_BLEZ    = 0b000110, //  6: Branch on Less Than or Equal to Zero
    OPC_BGTZ    = 0b000111, //  7: Branch on Greater Than Zero

    OPC_ADDI    = 0b001000, //  8: Add Immediate Word
    OPC_ADDIU   = 0b001001, //  9: Add Immediate Unsigned Word
    OPC_SLTI    = 0b001010, // 10: Set on Less Than Immediate
    OPC_SLTIU   = 0b001011, // 11: Set on Less Than Immediate Unsigned
    OPC_ANDI    = 0b001100, // 12: And Immediate
    OPC_ORI     = 0b001101, // 13: Or Immediate
    OPC_XORI    = 0b001110, // 14: Exclusive Or Immediate
    OPC_LUI     = 0b001111, // 15: Load Upper Immediate

#define B_OPC_COP (COP_OPCODE << 2) // 0b010000
    OPC_COP0    = (B_OPC_COP | COP0), // 0b010000 (16): Coprocessor-0 (System Control Coprocessor)
    OPC_COP1    = (B_OPC_COP | COP1), // 0b010001 (17): Coprocessor-1 (Floating-Point Unit)
    OPC_COP2    = (B_OPC_COP | COP2), // 0b010010 (18): Coprocessor-2 (Reality Co-Processor Vector Unit)
    OPC_COP3    = (B_OPC_COP | COP3), // 0b010011 (19): Coprocessor-3 (CP3)

    OPC_BEQL    = 0b010100, // 20: Branch on Equal Likely
    OPC_BNEL    = 0b010101, // 21: Branch on Not Equal Likely
    OPC_BLEZL   = 0b010110, // 22: Branch on Less Than or Equal to Zero Likely
    OPC_BGTZL   = 0b010111, // 23: Branch on Greater Than Zero Likely

    OPC_DADDI   = 0b011000, // 24: Doubleword Add Immediate
    OPC_DADDIU  = 0b011001, // 25: Doubleword Add Immediate Unsigned

    OPC_LDL     = 0b011010, // 26: Load Doubleword Left
    OPC_LDR     = 0b011011, // 27: Load Doubleword Right

    //! TODO: OPC_UNK_28  = 0b011100, // 28:
    //! TODO: OPC_UNK_29  = 0b011101, // 29:
    //! TODO: OPC_UNK_30  = 0b011110, // 30:
    //! TODO: OPC_UNK_31  = 0b011111, // 31:

    OPC_LB      = 0b100000, // 32: Load Byte
    OPC_LH      = 0b100001, // 33: Load Halfword
    OPC_LWL     = 0b100010, // 34: Load Word Left
    OPC_LW      = 0b100011, // 35: Load Word
    OPC_LBU     = 0b100100, // 36: Load Byte Unsigned
    OPC_LHU     = 0b100101, // 37: Load Halfword Unsigned
    OPC_LWR     = 0b100110, // 38: Load Word Right
    OPC_LWU     = 0b100111, // 39: Load Word Unsigned

    OPC_SB      = 0b101000, // 40: Store Byte
    OPC_SH      = 0b101001, // 41: Store Halfword
    OPC_SWL     = 0b101010, // 42: Store Word Left
    OPC_SW      = 0b101011, // 43: Store Word
    OPC_SDL     = 0b101100, // 44: Store Doubleword Left
    OPC_SDR     = 0b101101, // 45: Store Doubleword Right
    OPC_SWR     = 0b101110, // 46: Store Word Right

    OPC_CACHE   = 0b101111, // 47: https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html

    OPC_LL      = 0b110000, // 48: Load Linked Word
    B_OPC_LWC   = 0b100000,
    OPC_LWC1    = (B_OPC_LWC | OPC_COP1), // 0b110001 (49): Load Word to Coprocessor-1 (Floating-Point Unit)
    OPC_LWC2    = (B_OPC_LWC | OPC_COP2), // 0b110010 (50): Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit)
    OPC_LWC3    = (B_OPC_LWC | OPC_COP3), // 0b110011 (51): Load Word to Coprocessor-3 (COP3)
    OPC_LLD     = 0b110100, // 52: Load Linked Doubleword
#define B_OPC_LDC 0b100100
    OPC_LDC1    = (B_OPC_LDC | OPC_COP1), // 0b110101 (53): Load Doubleword to Coprocessor-1 (Floating-Point Unit)
    OPC_LDC2    = (B_OPC_LDC | OPC_COP2), // 0b110110 (54): Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit)
    OPC_LD      = 0b110111, // 55: Load Doubleword

    OPC_SC      = 0b111000, // 56: Store Conditional Word
#define B_OPC_SWC 0b101000
    OPC_SWC1    = (B_OPC_SWC | OPC_COP1), // 0b111001 (57): Store Word to Coprocessor-1 (Floating-Point Unit)
    OPC_SWC2    = (B_OPC_SWC | OPC_COP2), // 0b111010 (58): Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit)
    OPC_SWC3    = (B_OPC_SWC | OPC_COP3), // 0b111011 (59): Store Word to Coprocessor-3 (COP3)
    OPC_SCD     = 0b111100, // 60: Store Conditional Doubleword
#define B_OPC_SDC 0b101100
    OPC_SDC1    = (B_OPC_SDC | OPC_COP1), // 0b111101 (61): Store Doubleword to Coprocessor-1 (Floating-Point Unit)
    OPC_SDC2    = (B_OPC_SDC | OPC_COP2), // 0b111110 (62): Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit)
    OPC_SD      = 0b111111, // 63: Store Doubleword
};

enum COP0BranchOpCodes {
    OPT_COP1_BC1F  = (0b00000 | FALSE), // 0b00000  (0):
    OPT_COP1_BC1T  = (0b00000 | TRUE ), // 0b00001  (1):
#define B_OPT_COP1_LIKELY 0b00010
    OPT_COP1_BC1FL = (B_OPT_COP1_LIKELY | FALSE), // 0b00010  (2):
    OPT_COP1_BC1TL = (B_OPT_COP1_LIKELY | TRUE ), // 0b00011  (3):
};

enum COP0MoveFormats {
    COP0_MF  = (COP_FROM | FMT_32b), // 0b00000 (16): 32-bit floating-point
    COP0_DMF = (COP_FROM | FMT_64b), // 0b00001 (17): 64-bit floating-point
    COP0_MT  = (COP_TO   | FMT_32b), // 0b00100 (20): 32-bit fixed-point
    COP0_DMT = (COP_TO   | FMT_64b), // 0b00101 (21): 64-bit fixed-point
};

enum COP0SpecialOpCodes {
    // OPC_COP0_MOVE  = 0b000000, //  0:

    OPC_COP0_TLBP  = 0b001000, //  8: Searches for a TLB entry that matches the EntryHi register
    OPC_COP0_TLBR  = 0b000001, //  1: Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register
    OPC_COP0_TLBWI = 0b000010, //  2: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register
    OPC_COP0_TLBWR = 0b000110, //  6: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register

    OPC_COP0_ERET  = 0b011000, // 24: Return from interrupt, exception, or error exception
};

enum COP1Funcs {
    COP1_ROUND,
    COP1_TRUNC,
    COP1_CEIL,
    COP1_FLOOR,
};

enum COP1FPUConditions {
    COP0_COND_F,    // False.
    COP0_COND_UN,   // Unordered.
    COP0_COND_EQ,   // Equal.
    COP0_COND_UEQ,  // Unordered or Equal.
    COP0_COND_OLT,  // Ordered Less Than.
    COP0_COND_ULT,  // Unordered or Less Than.
    COP0_COND_OLE,  // Ordered or Less Than or Equal.
    COP0_COND_ULE,  // Unordered or Less Than or Equal.
    COP0_COND_SF,   // Signaling False.
    COP0_COND_NGLE, // Not Greater or Less Than or Equal.
    COP0_COND_SEQ,  // Signalling Equal.
    COP0_COND_NGL,  // Not Greater or Less Than.
    COP0_COND_LT,   // Less Than.
    COP0_COND_NGE,  // Not Greater Than or Equal.
    COP0_COND_LE,   // Less Than or Equal.
    COP0_COND_NGT,  // Not Greater Than.
};

// Special opcodes
enum COP1SpecialOpCodes {
    OPS_ADD_F       = 0b000000, //  0: ADD.[fmt] Floating-Point Add (3cyc)
    OPS_SUB_F       = 0b000001, //  1: SUB.[fmt] Floating-Point Subtract (3cyc)
    OPS_MUL_F       = 0b000010, //  2: MUL.[fmt] Floating-Point Multiply (S:5cyc; D:8cyc)
    OPS_DIV_F       = 0b000011, //  3: DIV.[fmt] Floating-Point Divide (S:29cyc; D:58cyc)

    OPS_SQRT_F      = 0b000100, //  4: SQRT.[fmt] Floating-Point Square Root (S:29cyc; D:58cyc)
    OPS_ABS_F       = 0b000101, //  5: ABS.[fmt]  Floating-Point Absolute Value (1cyc)
    OPS_MOV_F       = 0b000110, //  6: MOV.[fmt]  Floating-Point Move (1cyc)
    OPS_NEG_F       = 0b000111, //  7: NEG.[fmt]  Floating-Point Negate (1cyc)

#define B_OPS_TOFIXED_L 0b001000
    OPS_ROUND_L_F   = (B_OPS_TOFIXED_L | COP1_ROUND), // 0b001000 ( 8): ROUND.L.[fmt] Floating-Point Round to Long Fixed-Point (5cyc)
    OPS_TRUNC_L_F   = (B_OPS_TOFIXED_L | COP1_TRUNC), // 0b001001 ( 9): TRUNC.L.[fmt] Floating-Point Truncate to Long Fixed-Point (5cyc)
    OPS_CEIL_L_F    = (B_OPS_TOFIXED_L | COP1_CEIL ), // 0b001010 (10): CEIL.L.[fmt]  Floating-Point Ceiling to Long Fixed-Point (5cyc)
    OPS_FLOOR_L_F   = (B_OPS_TOFIXED_L | COP1_FLOOR), // 0b001011 (11): FLOOR.L.[fmt] Floating-Point Floor to Long Fixed-Point (5cyc)

#define B_OPS_TOFIXED_W 0b001100
    OPS_ROUND_W_F   = (B_OPS_TOFIXED_W | COP1_ROUND), // 0b001100 (12): ROUND.W.[fmt] Floating-Point Round to Word Fixed-Point (5cyc)
    OPS_TRUNC_W_F   = (B_OPS_TOFIXED_W | COP1_TRUNC), // 0b001101 (13): TRUNC.W.[fmt] Floating-Point Truncate to Word Fixed-Point (5cyc)
    OPS_CEIL_W_F    = (B_OPS_TOFIXED_W | COP1_CEIL ), // 0b001110 (14): CEIL.W.[fmt]  Floating-Point Ceiling to Word Fixed-Point (5cyc)
    OPS_FLOOR_W_F   = (B_OPS_TOFIXED_W | COP1_FLOOR), // 0b001111 (15): FLOOR.W.[fmt] Floating-Point Floor to Word Fixed-Point (5cyc)

#define B_OPS_CVT_F_F 0b100000
    OPS_CVT_S_F     = (B_OPS_CVT_F_F | COP1_FMT_SINGLE), // 0b100000 (32):  CVT.S.[fmt] Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc)
    OPS_CVT_D_F     = (B_OPS_CVT_F_F | COP1_FMT_DOUBLE), // 0b100001 (33):  CVT.D.[fmt] Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc)
    OPS_CVT_W_F     = (B_OPS_CVT_F_F | COP1_FMT_WORD  ), // 0b100100 (36):  CVT.W.[fmt] Floating-Point Convert to Word Fixed-Point (5cyc)
    OPS_CVT_L_F     = (B_OPS_CVT_F_F | COP1_FMT_LONG  ), // 0b100101 (37):  CVT.L.[fmt] Floating-Point Convert to Long Fixed-Point (5cyc)

#define B_OPS_C_F 0b110000 // Last 4 bits are format
    OPS_C_F         = (B_OPS_C_F | COP0_COND_F   ), // 0b110000 (48): C.F.[fmt]    Floating-Point Compare (False) (1cyc)
    OPS_C_UN        = (B_OPS_C_F | COP0_COND_UN  ), // 0b110001 (49): C.UN.[fmt]   Floating-Point Compare (Unordered) (1cyc)
    OPS_C_EQ        = (B_OPS_C_F | COP0_COND_EQ  ), // 0b110010 (50): C.EQ.[fmt]   Floating-point Compare (Equal) (1cyc)
    OPS_C_UEQ       = (B_OPS_C_F | COP0_COND_UEQ ), // 0b110011 (51): C.UEQ.[fmt]  Floating-point Compare (Unordered or Equal) (1cyc)
    OPS_C_OLT       = (B_OPS_C_F | COP0_COND_OLT ), // 0b110100 (52): C.OLT.[fmt]  Floating-point Compare (Ordered Less Than) (1cyc)
    OPS_C_ULT       = (B_OPS_C_F | COP0_COND_ULT ), // 0b110101 (53): C.ULT.[fmt]  Floating-point Compare (Unordered or Less Than) (1cyc)
    OPS_C_OLE       = (B_OPS_C_F | COP0_COND_OLE ), // 0b110110 (54): C.OLE.[fmt]  Floating-point Compare (Ordered or Less Than or Equal) (1cyc)
    OPS_C_ULE       = (B_OPS_C_F | COP0_COND_ULE ), // 0b110111 (55): C.ULE.[fmt]  Floating-point Compare (Unordered or Less Than or Equal) (1cyc)
    OPS_C_SF        = (B_OPS_C_F | COP0_COND_SF  ), // 0b111000 (56): C.SF.[fmt]   Floating-point Compare (Signaling False) (1cyc)
    OPS_C_NGLE      = (B_OPS_C_F | COP0_COND_NGLE), // 0b111001 (57): C.NGLE.[fmt] Floating-point Compare (Not Greater or Less Than or Equal) (1cyc)
    OPS_C_SEQ       = (B_OPS_C_F | COP0_COND_SEQ ), // 0b111010 (58): C.SEQ.[fmt]  Floating-point Compare (Signalling Equal) (1cyc)
    OPS_C_NGL       = (B_OPS_C_F | COP0_COND_NGL ), // 0b111011 (59): C.NGL.[fmt]  Floating-point Compare (Not Greater or Less Than) (1cyc)
    OPS_C_LT        = (B_OPS_C_F | COP0_COND_LT  ), // 0b111100 (60): C.LT.[fmt]   Floating-point Compare (Less Than) (1cyc)
    OPS_C_NGE       = (B_OPS_C_F | COP0_COND_NGE ), // 0b111101 (61): C.NGE.[fmt]  Floating-point Compare (Not Greater Than or Equal) (1cyc)
    OPS_C_LE        = (B_OPS_C_F | COP0_COND_LE  ), // 0b111110 (62): C.LE.[fmt]   Floating-point Compare (Less Than or Equal) (1cyc)
    OPS_C_NGT       = (B_OPS_C_F | COP0_COND_NGT ), // 0b111111 (63): C.NGT.[fmt]  Floating-point Compare (Not Greater Than) (1cyc)
};

// enum COP2SpecialOpCodes {

// };
// enum COP3SpecialOpCodes {

// };

enum SpecialOpCodes {
    OPS_NOP     = 0b000000, //  0: NOP (pseudo of SLL)

    OPS_SLL     = 0b000000, //  0: Shift Word Left Logical
    //! TODO: OPS_SLA     = 0b000001, //  1: Shift Word Left Arithmetic
    OPS_SRL     = 0b000010, //  2: Shift Word Right Logical
    OPS_SRA     = 0b000011, //  3: Shift Word Right Arithmetic
    OPS_SLLV    = 0b000100, //  4: Shift Word Left Logical Variable
    //! TODO: OPS_SLAV    = 0b000101, //  5: Shift Word Left Arithmetic Variable
    OPS_SRLV    = 0b000110, //  6: Shift Word Right Logical Variable
    OPS_SRAV    = 0b000111, //  7: Shift Word Right Arithmetic Variable

    OPS_JR      = 0b001000, //  8: Jump Register
    OPS_JALR    = 0b001001, //  9: Jump and Link Register
    //! TODO: OPS_UNK_10  = 0b001010, // 10:
    //! TODO: OPS_UNK_11  = 0b001011, // 11:
    OPS_SYSCALL = 0b001100, // 12: System Call (assert)
    OPS_BREAK   = 0b001101, // 13: Breakpoint
    // OPS_UNK_14  = 0b001110, // 14:
    OPS_SYNC    = 0b001111, // 15: Synchronize Shared Memory

    OPS_MFHI    = 0b010000, // 16: Move From HI
    OPS_MTHI    = 0b010001, // 17: Move To HI
    OPS_MFLO    = 0b010010, // 18: Move From LO
    OPS_MTLO    = 0b010011, // 19: Move To LO

    OPS_DSLLV   = 0b010100, // 20: Doubleword Shift Left Logical Variable
    //! TODO: OPS_DSLAV   = 0b010101, // 21: Doubleword Shift Left Arithmetic Variable
    OPS_DSRLV   = 0b010110, // 22: Doubleword Shift Right Logical Variable
    OPS_DSRAV   = 0b010111, // 23: Doubleword Shift Right Arithmetic Variable

    OPS_MULT    = 0b011000, // 24: Multiply Word (5cyc)
    OPS_MULTU   = 0b011001, // 25: Multiply Unsigned Word (5cyc)
    OPS_DIV     = 0b011010, // 26: Divide Word (37cyc)
    OPS_DIVU    = 0b011011, // 27: Divide Unsigned Word (37cyc)
    OPS_DMULT   = 0b011100, // 28: Doubleword Multiply (8cyc)
    OPS_DMULTU  = 0b011101, // 29: Doubleword Multiply Unsigned (8cyc)
    OPS_DDIV    = 0b011110, // 30: Doubleword Divide (69cyc)
    OPS_DDIVU   = 0b011111, // 31: Doubleword Divide Unsigned (69cyc)

    OPS_ADD     = 0b100000, // 32: Add Word
    OPS_ADDU    = 0b100001, // 33: Add Unsigned Word
    OPS_SUB     = 0b100010, // 34: Subtract Word
    OPS_SUBU    = 0b100011, // 35: Subtract Unsigned Word
    OPS_AND     = 0b100100, // 36: And
    OPS_OR      = 0b100101, // 37: Or
    OPS_XOR     = 0b100110, // 38: Exclusive Or
    OPS_NOR     = 0b100111, // 39: Nor
    //! TODO: OPS_UNK_40  = 0b101000, // 40:
    //! TODO: OPS_UNK_41  = 0b101001, // 41:
    OPS_SLT     = 0b101010, // 42: Set on Less Than
    OPS_SLTU    = 0b101011, // 43: Set on Less Than Unsigned
    OPS_DADD    = 0b101100, // 44: Doubleword Add
    OPS_DADDU   = 0b101101, // 45: Doubleword Add Unsigned
    OPS_DSUB    = 0b101110, // 46: Doubleword Subtract
    OPS_DSUBU   = 0b101111, // 47: Doubleword Subtract Unsigned

    OPS_TGE     = 0b110000, // 48: Trap if Greater Than or Equal
    OPS_TGEU    = 0b110001, // 49: Trap if Greater Than or Equal Unsigned
    OPS_TLT     = 0b110010, // 50: Trap if Less Than
    OPS_TLTU    = 0b110011, // 51: Trap if Less Than Unsigned
    OPS_TEQ     = 0b110100, // 52: Trap if Equal
    //! TODO: OPS_UNK_53  = 0b110101, // 53:
    OPS_TNE     = 0b110110, // 54: Trap if Not Equal
    //! TODO: OPS_UNK_55  = 0b110111, // 55:
    OPS_DSLL    = 0b111000, // 56: Doubleword Shift Left Logical
    //! TODO: OPS_DSLA    = 0b111001, // 57: Doubleword Shift Left Arithmetic
    OPS_DSRL    = 0b111010, // 58: Doubleword Shift Right Logical
    OPS_DSRA    = 0b111011, // 59: Doubleword Shift Right Arithmetic
    OPS_DSLL32  = 0b111100, // 60: Doubleword Shift Left Logical + 32
    //! TODO: OPS_DSLA32  = 0b111101, // 61: Doubleword Shift Left Arithmetic + 32
    OPS_DSRL32  = 0b111110, // 62: Doubleword Shift Right Logical + 32
    OPS_DSRA32  = 0b111111, // 63: Doubleword Shift Right Arithmetic + 32
};

enum RegImmOpCodes {
    OPR_BLTZ    = 0b00000, //  0: Branch on Less Than Zero
    OPR_BGEZ    = 0b00001, //  1: Branch on Greater Than or Equal to Zero
    OPR_BLTZL   = 0b00010, //  2: Branch on Less Than Zero Likely
    OPR_BGEZL   = 0b00011, //  3: Branch on Greater Than or Equal to Zero Likely

#define B_OPR_AND_LINK 0b10000
    OPR_BLTZAL  = (B_OPR_AND_LINK | OPR_BLTZ ), // 0b10000 (16): Branch on Less Than Zero and Link
    OPR_BGEZAL  = (B_OPR_AND_LINK | OPR_BGEZ ), // 0b10001 (17): Branch on Greater Than or Equal to Zero and Link
    OPR_BLTZALL = (B_OPR_AND_LINK | OPR_BLTZL), // 0b10010 (18): Branch on Less Than Zero and Link Likely
    OPR_BGEZALL = (B_OPR_AND_LINK | OPR_BGEZL), // 0b10011 (19): Branch on Greater Than or Equal to Zero and Link Likely

    OPR_TGEI    = 0b01000, //  8: Trap if Greater Than or Equal Immediate
    OPR_TGEIU   = 0b01001, //  9: Trap if Greater Than or Equal Unsigned Immediate
    OPR_TLTI    = 0b01010, // 10: Trap if Less Than Immediate
    OPR_TLTIU   = 0b01011, // 11: Trap if Less Than Unsigned Immediate
    OPR_TEQI    = 0b01100, // 12: Trap if Equal Immediate
    //! TODO: OPR_UNK_13  = 0b01101, // 13:
    OPR_TNEI    = 0b01110, // 14: Trap if Not Equal Immediate
};

enum PseudoInsns {
    PSEUDO_NOP,
    PSEUDO_MOVET,
    PSEUDO_MOVES,
    PSEUDO_B,
    PSEUDO_BEQZ,
    PSEUDO_BNEZ,
    PSEUDO_LI,
    PSEUDO_SUBI,
    PSEUDO_BEQZL,
    PSEUDO_BNEZL,
    PSEUDO_DSUBI,
};

enum InsnType {
    INSN_TYPE_ERROR = -1,
    INSN_TYPE_OPCODE,
    INSN_TYPE_FUNC,
    INSN_TYPE_REGIMM,
    INSN_TYPE_COP_FMT,
};

// Instruction data
//! TODO: Clean this up if it's possible to make the structs not overwrite each other.
typedef union {
    struct PACKED {
        /*0x00*/ Word opcode :  6;
        /*0x00*/ Word rs     :  5; // aka base
        /*0x00*/ Word rt     :  5; // aka regimm
        /*0x00*/ Word rd     :  5;
        /*0x00*/ Word sa     :  5;
        /*0x00*/ Word func   :  6;
    };
    struct PACKED {
        /*0x00*/ Word        :  6;
        /*0x00*/ Word base   :  5;
        /*0x00*/ Word        :  5;
        /*0x00*/ Word offset : 16; // aka immediate
    };
    struct PACKED {
        /*0x00*/ Word           : 11;
        /*0x00*/ Word regimm    :  5; // aka rt
        /*0x00*/ Word immediate : 16; // aka offset
    };
    struct PACKED {
        /*0x00*/ Word    : 11;
        /*0x00*/ Word ft :  5; // aka: branch condition
        /*0x00*/ Word fs :  5;
        /*0x00*/ Word fd :  5;
        /*0x00*/ Word    :  6;
    };
    struct PACKED {
        /*0x00*/ Word cop_opcode  :  4;
        /*0x00*/ Word cop_num     :  2;
        /*0x00*/ Word cop_subtype :  2;
        /*0x00*/ Word fmt         :  3;
        /*0x00*/ Word cop_bcond   :  5; // 0b00010 = likely, 0b00001 = true
        /*0x00*/ Word             : 10;
        /*0x00*/ Word FC          :  2;
        /*0x00*/ Word cond        :  4;
    };
    struct PACKED {
        /*0x00*/ Word             :  6;
        /*0x00*/ Word instr_index : 26;
    };
    Word raw;
} InsnData; /*0x04*/

// Instruction database format
typedef struct PACKED {
    /*0x00*/ char name[8];
    /*0x04*/ char fmt[4];
    /*0x08*/ u8 pad[3];
    /*0x0F*/ u8 opcode;
} InsnTemplate; /*0x10*/

typedef struct PACKED {
    /*0x00*/ InsnData mask; // A bitmask for the bits used to identify the instruction. Anything not in the mask is params.
    /*0x04*/ const char* paramStr;
} InsnParamType; /*0x08*/


#define DISASM_STEP (ssize_t)sizeof(InsnData)

#define INSN_OFFSET(addr, offset) ((addr) + (sizeof(InsnData) * (s16)(offset)))


extern _Bool gEnablePseudoinstructions;


s16 check_for_branch_offset(InsnData insn);
Address get_branch_target_from_addr(Address addr);
char* insn_disasm(InsnData insn, const char** fname, _Bool showDestNames);
