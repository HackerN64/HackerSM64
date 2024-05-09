#pragma once

#include <ultra64.h>

#include "types.h"

#include "registers.h"
#include "crash_screen/cs_print.h"


#define INSN_NAME_DISPLAY_WIDTH 10


typedef enum PACKED MIPSParamFmts {
    IFMT_NOP,

    IFMT_s,     // rs
    // IFMT_t,     // rt // unused
    IFMT_d,     // rd
    IFMT_ds,    // rd rs
    IFMT_st,    // rs rt
    IFMT_dt,    // rd rt // for pseudos
    IFMT_dst,   // rd rs rt
    IFMT_dts,   // rd rt rs

    IFMT_tdCP0, // rt rd[cp0]
    IFMT_tSCP1, // rt fs[cp1]
    IFMT_tSFCR, // rt fs[fcr]

    IFMT_DS_XX, // fd fs // allflt
    IFMT_DS_IX, // 
    IFMT_DS_FX, // 
    IFMT_ST,    // fs ft
    IFMT_DST,   // fd fs ft

    IFMT_tsI,   // rt rs immediate
    IFMT_tsN,   // rt rs abs(immediate) // for pseudos
    IFMT_tI,    // rt immediate    
    IFMT_sI,    // rs immediate

    IFMT_to,    // rt offset(base)
    IFMT_To,    // ft offset(base)

    IFMT_dta,   // rd rt shift
    IFMT_ste,   // rs rt exc10
    IFMT_E,     // exc20

    IFMT_stB,   // rs rt branch
    IFMT_sB,    // rs branch
    IFMT_B,     // branch
    // IFMT_B_FP,

    IFMT_J,     // jump
} MIPSParamFmts;

// Lower 4 bits:
typedef enum PACKED MIPSParamID {
    MP_NOP,     // "NOP".
    MP_RS,      // "[rs reg]".
    MP_RT,      // "[rt reg]".
    MP_RD,      // "[rd reg]".
    MP_RDCP0,   // "[rd reg]" COP0 register.
    MP_FT,      // "F[ft reg]".
    MP_FS,      // "F[fs reg]".
    MP_FD,      // "F[fd reg]".
    MP_FSFCR,   // "[fs reg]" FCR register.
    MP_IMM,     // "0x[last 16 bits]".
    MP_SHIFT,   // "[shift]".
    MP_OFF,     // "0x[last 16 bits]([rs reg])".
    MP_B,       // "[±]0x[last 16 bits]" + draw branch arrow.
    MP_JUMP,    // "[function address]" or parse map.
    MP_EXC10,   // "[0xXXX]" 10-bit data for the exception handler.
    MP_EXC20,   // "[0xXXXXX]" 20-bit data for the exception handler.
} MIPSParamID;


#define COP_FROM    (0 * BIT(2)) // 0b00000 (move from).
#define COP_TO      (1 * BIT(2)) // 0b00100 (move to).
#define FMT_FLT     COP_FROM // 0b00000 (floating-point).
#define FMT_FIX     COP_TO   // 0b00100 (fixed-point).
#define FMT_CTL     BIT(1)

#define FMT_32b     (0 * BIT(0)) // 0b00000 (32-bit).
#define FMT_64b     (1 * BIT(0)) // 0b00001 (64-bit).

// Used for fmt_to_char.
typedef enum CP1Formats {
    COP1_FMT_SINGLE = (FMT_FLT | FMT_32b), // 0b00000 (16): 32-bit floating-point.
    COP1_FMT_DOUBLE = (FMT_FLT | FMT_64b), // 0b00001 (17): 64-bit floating-point.
    COP1_FMT_WORD   = (FMT_FIX | FMT_32b), // 0b00100 (20): 32-bit fixed-point.
    COP1_FMT_LONG   = (FMT_FIX | FMT_64b), // 0b00101 (21): 64-bit fixed-point.
    COP1_FMT_CTL_F  = (COP_FROM | FMT_CTL | FMT_32b), // 0b00010  (2): Control Word From.
    COP1_FMT_CTL_T  = (COP_TO   | FMT_CTL | FMT_32b), // 0b00110  (6): Control Word To.
} CP1Formats;

#define FMT_SIGNED   (0 * BIT(0))
#define FMT_UNSIGNED (1 * BIT(0))

#define COP_OPCODE 0b0100

// insn_db_standard opcodes.
typedef enum InsnDB_base_Opcodes {
    OPC_SPECIAL = 0b000000, //  0: Use function (last 6 bits) as opcode.
    OPC_REGIMM  = 0b000001, //  1: Use rt (skip 5 bits after opcode and the 5 bits after that) as opcode.

    OPC_J       = 0b000010, //  2: Jump.
    OPC_JAL     = 0b000011, //  3: Jump and Link.
    OPC_BEQ     = 0b000100, //  4: Branch on Equal.
    OPC_BNE     = 0b000101, //  5: Branch on Not Equal.
    OPC_BLEZ    = 0b000110, //  6: Branch on Less Than or Equal to Zero.
    OPC_BGTZ    = 0b000111, //  7: Branch on Greater Than Zero.

    OPC_ADDI    = 0b001000, //  8: Add Immediate Word.
    OPC_ADDIU   = 0b001001, //  9: Add Immediate Unsigned Word.
    OPC_SLTI    = 0b001010, // 10: Set on Less Than Immediate.
    OPC_SLTIU   = 0b001011, // 11: Set on Less Than Immediate Unsigned.
    OPC_ANDI    = 0b001100, // 12: And Immediate.
    OPC_ORI     = 0b001101, // 13: Or Immediate.
    OPC_XORI    = 0b001110, // 14: Exclusive Or Immediate.
    OPC_LUI     = 0b001111, // 15: Load Upper Immediate.

#define B_OPC_COP (COP_OPCODE << 2) // 0b010000
    OPC_COP0    = (B_OPC_COP | COP0), // 0b010000 (16): Coprocessor-0 (System Control Coprocessor).
    OPC_COP1    = (B_OPC_COP | COP1), // 0b010001 (17): Coprocessor-1 (Floating-Point Unit).
    OPC_COP2    = (B_OPC_COP | COP2), // 0b010010 (18): Coprocessor-2 (Reality Co-Processor Vector Unit).
    OPC_COP3    = (B_OPC_COP | COP3), // 0b010011 (19): Coprocessor-3 (CP3).

    OPC_BEQL    = 0b010100, // 20: Branch on Equal Likely.
    OPC_BNEL    = 0b010101, // 21: Branch on Not Equal Likely.
    OPC_BLEZL   = 0b010110, // 22: Branch on Less Than or Equal to Zero Likely.
    OPC_BGTZL   = 0b010111, // 23: Branch on Greater Than Zero Likely.

    OPC_DADDI   = 0b011000, // 24: Doubleword Add Immediate.
    OPC_DADDIU  = 0b011001, // 25: Doubleword Add Immediate Unsigned.

    OPC_LDL     = 0b011010, // 26: Load Doubleword Left.
    OPC_LDR     = 0b011011, // 27: Load Doubleword Right.

    // OPC_UNK_28  = 0b011100, // 28:
    // OPC_UNK_29  = 0b011101, // 29:
    // OPC_UNK_30  = 0b011110, // 30:
    // OPC_UNK_31  = 0b011111, // 31:

    OPC_LB      = 0b100000, // 32: Load Byte.
    OPC_LH      = 0b100001, // 33: Load Halfword.
    OPC_LWL     = 0b100010, // 34: Load Word Left.
    OPC_LW      = 0b100011, // 35: Load Word.
    OPC_LBU     = 0b100100, // 36: Load Byte Unsigned.
    OPC_LHU     = 0b100101, // 37: Load Halfword Unsigned.
    OPC_LWR     = 0b100110, // 38: Load Word Right.
    OPC_LWU     = 0b100111, // 39: Load Word Unsigned.

    OPC_SB      = 0b101000, // 40: Store Byte.
    OPC_SH      = 0b101001, // 41: Store Halfword.
    OPC_SWL     = 0b101010, // 42: Store Word Left.
    OPC_SW      = 0b101011, // 43: Store Word.
    OPC_SDL     = 0b101100, // 44: Store Doubleword Left.
    OPC_SDR     = 0b101101, // 45: Store Doubleword Right.
    OPC_SWR     = 0b101110, // 46: Store Word Right.

    OPC_CACHE   = 0b101111, // 47: https://techpubs.jurassic.nl/manuals/hdwr/developer/R10K_UM/sgi_html/t5.Ver.2.0.book_301.html.

    OPC_LL      = 0b110000, // 48: Load Linked Word.
    B_OPC_LWC   = 0b100000,
    OPC_LWC1    = (B_OPC_LWC | OPC_COP1), // 0b110001 (49): Load Word to Coprocessor-1 (Floating-Point Unit).
    OPC_LWC2    = (B_OPC_LWC | OPC_COP2), // 0b110010 (50): Load Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    OPC_LWC3    = (B_OPC_LWC | OPC_COP3), // 0b110011 (51): Load Word to Coprocessor-3 (COP3).
    OPC_LLD     = 0b110100, // 52: Load Linked Doubleword.
#define B_OPC_LDC 0b100100
    OPC_LDC1    = (B_OPC_LDC | OPC_COP1), // 0b110101 (53): Load Doubleword to Coprocessor-1 (Floating-Point Unit).
    OPC_LDC2    = (B_OPC_LDC | OPC_COP2), // 0b110110 (54): Load Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    OPC_LD      = 0b110111, // 55: Load Doubleword.

    OPC_SC      = 0b111000, // 56: Store Conditional Word.
#define B_OPC_SWC 0b101000
    OPC_SWC1    = (B_OPC_SWC | OPC_COP1), // 0b111001 (57): Store Word to Coprocessor-1 (Floating-Point Unit).
    OPC_SWC2    = (B_OPC_SWC | OPC_COP2), // 0b111010 (58): Store Word to Coprocessor-2 (Reality Co-Processor Vector Unit).
    OPC_SWC3    = (B_OPC_SWC | OPC_COP3), // 0b111011 (59): Store Word to Coprocessor-3 (COP3).
    OPC_SCD     = 0b111100, // 60: Store Conditional Doubleword.
#define B_OPC_SDC 0b101100
    OPC_SDC1    = (B_OPC_SDC | OPC_COP1), // 0b111101 (61): Store Doubleword to Coprocessor-1 (Floating-Point Unit).
    OPC_SDC2    = (B_OPC_SDC | OPC_COP2), // 0b111110 (62): Store Doubleword to Coprocessor-2 (Reality Co-Processor Vector Unit).
    OPC_SD      = 0b111111, // 63: Store Doubleword.
} InsnDB_base_Opcodes;

// insn_db_spec opcodes.
typedef enum InsnDB_spec_Opcodes {
    OPS_NOP     = 0b000000, //  0: NOP (pseudo of SLL).

    OPS_SLL     = 0b000000, //  0: Shift Word Left Logical.
    // OPS_SLA     = 0b000001, //  1: Shift Word Left Arithmetic.
    OPS_SRL     = 0b000010, //  2: Shift Word Right Logical.
    OPS_SRA     = 0b000011, //  3: Shift Word Right Arithmetic.
    OPS_SLLV    = 0b000100, //  4: Shift Word Left Logical Variable.
    // OPS_SLAV    = 0b000101, //  5: Shift Word Left Arithmetic Variable.
    OPS_SRLV    = 0b000110, //  6: Shift Word Right Logical Variable.
    OPS_SRAV    = 0b000111, //  7: Shift Word Right Arithmetic Variable.

    OPS_JR      = 0b001000, //  8: Jump Register.
    OPS_JALR    = 0b001001, //  9: Jump and Link Register.
    // OPS_UNK_10  = 0b001010, // 10:
    // OPS_UNK_11  = 0b001011, // 11:
    OPS_SYSCALL = 0b001100, // 12: System Call (assert).
    OPS_BREAK   = 0b001101, // 13: Breakpoint.
    // OPS_UNK_14  = 0b001110, // 14:
    OPS_SYNC    = 0b001111, // 15: Synchronize Shared Memory.

    OPS_MFHI    = 0b010000, // 16: Move From HI.
    OPS_MTHI    = 0b010001, // 17: Move To HI.
    OPS_MFLO    = 0b010010, // 18: Move From LO.
    OPS_MTLO    = 0b010011, // 19: Move To LO.

    OPS_DSLLV   = 0b010100, // 20: Doubleword Shift Left Logical Variable.
    // OPS_DSLAV   = 0b010101, // 21: Doubleword Shift Left Arithmetic Variable.
    OPS_DSRLV   = 0b010110, // 22: Doubleword Shift Right Logical Variable.
    OPS_DSRAV   = 0b010111, // 23: Doubleword Shift Right Arithmetic Variable.

    OPS_MULT    = 0b011000, // 24: Multiply Word (5cyc).
    OPS_MULTU   = 0b011001, // 25: Multiply Unsigned Word (5cyc).
    OPS_DIV     = 0b011010, // 26: Divide Word (37cyc).
    OPS_DIVU    = 0b011011, // 27: Divide Unsigned Word (37cyc).
    OPS_DMULT   = 0b011100, // 28: Doubleword Multiply (8cyc).
    OPS_DMULTU  = 0b011101, // 29: Doubleword Multiply Unsigned (8cyc).
    OPS_DDIV    = 0b011110, // 30: Doubleword Divide (69cyc).
    OPS_DDIVU   = 0b011111, // 31: Doubleword Divide Unsigned (69cyc).

    OPS_ADD     = 0b100000, // 32: Add Word.
    OPS_ADDU    = 0b100001, // 33: Add Unsigned Word.
    OPS_SUB     = 0b100010, // 34: Subtract Word.
    OPS_SUBU    = 0b100011, // 35: Subtract Unsigned Word.
    OPS_AND     = 0b100100, // 36: And.
    OPS_OR      = 0b100101, // 37: Or.
    OPS_XOR     = 0b100110, // 38: Exclusive Or.
    OPS_NOR     = 0b100111, // 39: Nor.
    // OPS_UNK_40  = 0b101000, // 40:
    // OPS_UNK_41  = 0b101001, // 41:
    OPS_SLT     = 0b101010, // 42: Set on Less Than.
    OPS_SLTU    = 0b101011, // 43: Set on Less Than Unsigned.
    OPS_DADD    = 0b101100, // 44: Doubleword Add.
    OPS_DADDU   = 0b101101, // 45: Doubleword Add Unsigned.
    OPS_DSUB    = 0b101110, // 46: Doubleword Subtract.
    OPS_DSUBU   = 0b101111, // 47: Doubleword Subtract Unsigned.

    OPS_TGE     = 0b110000, // 48: Trap if Greater Than or Equal.
    OPS_TGEU    = 0b110001, // 49: Trap if Greater Than or Equal Unsigned.
    OPS_TLT     = 0b110010, // 50: Trap if Less Than.
    OPS_TLTU    = 0b110011, // 51: Trap if Less Than Unsigned.
    OPS_TEQ     = 0b110100, // 52: Trap if Equal.
    // OPS_UNK_53  = 0b110101, // 53:
    OPS_TNE     = 0b110110, // 54: Trap if Not Equal.
    // OPS_UNK_55  = 0b110111, // 55:
    OPS_DSLL    = 0b111000, // 56: Doubleword Shift Left Logical.
    // OPS_DSLA    = 0b111001, // 57: Doubleword Shift Left Arithmetic.
    OPS_DSRL    = 0b111010, // 58: Doubleword Shift Right Logical.
    OPS_DSRA    = 0b111011, // 59: Doubleword Shift Right Arithmetic.
    OPS_DSLL32  = 0b111100, // 60: Doubleword Shift Left Logical + 32.
    // OPS_DSLA32  = 0b111101, // 61: Doubleword Shift Left Arithmetic + 32.
    OPS_DSRL32  = 0b111110, // 62: Doubleword Shift Right Logical + 32.
    OPS_DSRA32  = 0b111111, // 63: Doubleword Shift Right Arithmetic + 32.
} InsnDB_spec_Opcodes;

// insn_db_regi opcodes.
typedef enum InsnDB_regi_Opcodes {
    OPR_BLTZ    = 0b00000, //  0: Branch on Less Than Zero.
    OPR_BGEZ    = 0b00001, //  1: Branch on Greater Than or Equal to Zero.
    OPR_BLTZL   = 0b00010, //  2: Branch on Less Than Zero Likely.
    OPR_BGEZL   = 0b00011, //  3: Branch on Greater Than or Equal to Zero Likely.

#define B_OPR_AND_LINK 0b10000
    OPR_BLTZAL  = (B_OPR_AND_LINK | OPR_BLTZ ), // 0b10000 (16): Branch on Less Than Zero and Link.
    OPR_BGEZAL  = (B_OPR_AND_LINK | OPR_BGEZ ), // 0b10001 (17): Branch on Greater Than or Equal to Zero and Link.
    OPR_BLTZALL = (B_OPR_AND_LINK | OPR_BLTZL), // 0b10010 (18): Branch on Less Than Zero and Link Likely.
    OPR_BGEZALL = (B_OPR_AND_LINK | OPR_BGEZL), // 0b10011 (19): Branch on Greater Than or Equal to Zero and Link Likely.

    OPR_TGEI    = 0b01000, //  8: Trap if Greater Than or Equal Immediate.
    OPR_TGEIU   = 0b01001, //  9: Trap if Greater Than or Equal Unsigned Immediate.
    OPR_TLTI    = 0b01010, // 10: Trap if Less Than Immediate.
    OPR_TLTIU   = 0b01011, // 11: Trap if Less Than Unsigned Immediate.
    OPR_TEQI    = 0b01100, // 12: Trap if Equal Immediate.
    // OPR_UNK_13  = 0b01101, // 13:
    OPR_TNEI    = 0b01110, // 14: Trap if Not Equal Immediate.
} InsnDB_regi_Opcodes;

// Coprocessor-0 (System Control Coprocessor) move formats.
typedef enum COP0MoveFormats {
    COP0_MF  = (COP_FROM | FMT_32b), // 0b00000 (16): 32-bit floating-point.
    COP0_DMF = (COP_FROM | FMT_64b), // 0b00001 (17): 64-bit floating-point.
    COP0_MT  = (COP_TO   | FMT_32b), // 0b00100 (20): 32-bit fixed-point.
    COP0_DMT = (COP_TO   | FMT_64b), // 0b00101 (21): 64-bit fixed-point.
} COP0MoveFormats;

// insn_db_cop0_sub10 opcodes.
typedef enum InsnDB_COP0_sub10_Opcodes {
    // OPC_COP0_MOVE  = 0b000000, //  0:

    OPC_COP0_TLBP  = 0b001000, //  8: Searches for a TLB entry that matches the EntryHi register.
    OPC_COP0_TLBR  = 0b000001, //  1: Loads EntryHi and EntryLo registers with the TLB entry pointed at by the Index register.
    OPC_COP0_TLBWI = 0b000010, //  2: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Index register.
    OPC_COP0_TLBWR = 0b000110, //  6: Stores the contents of EntryHi and EntryLo registers into the TLB entry pointed at by the Random register.

    OPC_COP0_ERET  = 0b011000, // 24: Return from interrupt, exception, or error exception.
} InsnDB_COP0_sub10_Opcodes;

// insn_db_cop1_sub01 opcodes.
typedef enum InsnDB_COP1_sub01_Opcodes {
    OPT_COP1_BC1F  = (0b00000 | FALSE), // 0b00000  (0): Branch on FP False (1cyc*).
    OPT_COP1_BC1T  = (0b00000 | TRUE ), // 0b00001  (1): Branch on FP True (1cyc*).
#define B_OPT_COP1_LIKELY 0b00010
    OPT_COP1_BC1FL = (B_OPT_COP1_LIKELY | FALSE), // 0b00010  (2): Branch on FP False Likely (1cyc*).
    OPT_COP1_BC1TL = (B_OPT_COP1_LIKELY | TRUE ), // 0b00011  (3): Branch on FP True Likely (1cyc*).
} InsnDB_COP1_sub01_Opcodes;

// Coprocessor-1 (Floating-Point Unit) functions.
typedef enum COP1Funcs {
    COP1_ROUND,
    COP1_TRUNC,
    COP1_CEIL,
    COP1_FLOOR,
} COP1Funcs;

// Coprocessor-1 (Floating-Point Unit) conditions.
typedef enum COP1FPUConditions {
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
} COP1FPUConditions;

// insn_db_cop1_sub10 opcodes.
typedef enum InsnDB_COP1_sub10_Opcodes {
    OPS_ADD_F       = 0b000000, //  0: ADD.[fmt] Floating-Point Add (3cyc).
    OPS_SUB_F       = 0b000001, //  1: SUB.[fmt] Floating-Point Subtract (3cyc).
    OPS_MUL_F       = 0b000010, //  2: MUL.[fmt] Floating-Point Multiply (S:5cyc; D:8cyc).
    OPS_DIV_F       = 0b000011, //  3: DIV.[fmt] Floating-Point Divide (S:29cyc; D:58cyc).

    OPS_SQRT_F      = 0b000100, //  4: SQRT.[fmt] Floating-Point Square Root (S:29cyc; D:58cyc).
    OPS_ABS_F       = 0b000101, //  5: ABS.[fmt]  Floating-Point Absolute Value (1cyc).
    OPS_MOV_F       = 0b000110, //  6: MOV.[fmt]  Floating-Point Move (1cyc).
    OPS_NEG_F       = 0b000111, //  7: NEG.[fmt]  Floating-Point Negate (1cyc).

#define B_OPS_TOFIXED_L 0b001000
    OPS_ROUND_L_F   = (B_OPS_TOFIXED_L | COP1_ROUND), // 0b001000 ( 8): ROUND.L.[fmt] Floating-Point Round to Long Fixed-Point (5cyc).
    OPS_TRUNC_L_F   = (B_OPS_TOFIXED_L | COP1_TRUNC), // 0b001001 ( 9): TRUNC.L.[fmt] Floating-Point Truncate to Long Fixed-Point (5cyc).
    OPS_CEIL_L_F    = (B_OPS_TOFIXED_L | COP1_CEIL ), // 0b001010 (10): CEIL.L.[fmt]  Floating-Point Ceiling to Long Fixed-Point (5cyc).
    OPS_FLOOR_L_F   = (B_OPS_TOFIXED_L | COP1_FLOOR), // 0b001011 (11): FLOOR.L.[fmt] Floating-Point Floor to Long Fixed-Point (5cyc).

#define B_OPS_TOFIXED_W 0b001100
    OPS_ROUND_W_F   = (B_OPS_TOFIXED_W | COP1_ROUND), // 0b001100 (12): ROUND.W.[fmt] Floating-Point Round to Word Fixed-Point (5cyc).
    OPS_TRUNC_W_F   = (B_OPS_TOFIXED_W | COP1_TRUNC), // 0b001101 (13): TRUNC.W.[fmt] Floating-Point Truncate to Word Fixed-Point (5cyc).
    OPS_CEIL_W_F    = (B_OPS_TOFIXED_W | COP1_CEIL ), // 0b001110 (14): CEIL.W.[fmt]  Floating-Point Ceiling to Word Fixed-Point (5cyc).
    OPS_FLOOR_W_F   = (B_OPS_TOFIXED_W | COP1_FLOOR), // 0b001111 (15): FLOOR.W.[fmt] Floating-Point Floor to Word Fixed-Point (5cyc).

#define B_OPS_CVT_F_F 0b100000
    OPS_CVT_S_F     = (B_OPS_CVT_F_F | COP1_FMT_SINGLE), // 0b100000 (32):  CVT.S.[fmt] Floating-Point Convert to Single Floating-Point (D:2cyc; W:5cyc; L:5cyc).
    OPS_CVT_D_F     = (B_OPS_CVT_F_F | COP1_FMT_DOUBLE), // 0b100001 (33):  CVT.D.[fmt] Floating-Point Convert to Double Floating-Point (S:1cyc; W:5cyc; L:5cyc).
    OPS_CVT_W_F     = (B_OPS_CVT_F_F | COP1_FMT_WORD  ), // 0b100100 (36):  CVT.W.[fmt] Floating-Point Convert to Word Fixed-Point (5cyc).
    OPS_CVT_L_F     = (B_OPS_CVT_F_F | COP1_FMT_LONG  ), // 0b100101 (37):  CVT.L.[fmt] Floating-Point Convert to Long Fixed-Point (5cyc).

#define B_OPS_C_F 0b110000 // Last 4 bits are format
    OPS_C_F         = (B_OPS_C_F | COP0_COND_F   ), // 0b110000 (48): C.F.[fmt]    Floating-Point Compare (False) (1cyc).
    OPS_C_UN        = (B_OPS_C_F | COP0_COND_UN  ), // 0b110001 (49): C.UN.[fmt]   Floating-Point Compare (Unordered) (1cyc).
    OPS_C_EQ        = (B_OPS_C_F | COP0_COND_EQ  ), // 0b110010 (50): C.EQ.[fmt]   Floating-point Compare (Equal) (1cyc).
    OPS_C_UEQ       = (B_OPS_C_F | COP0_COND_UEQ ), // 0b110011 (51): C.UEQ.[fmt]  Floating-point Compare (Unordered or Equal) (1cyc).
    OPS_C_OLT       = (B_OPS_C_F | COP0_COND_OLT ), // 0b110100 (52): C.OLT.[fmt]  Floating-point Compare (Ordered Less Than) (1cyc).
    OPS_C_ULT       = (B_OPS_C_F | COP0_COND_ULT ), // 0b110101 (53): C.ULT.[fmt]  Floating-point Compare (Unordered or Less Than) (1cyc).
    OPS_C_OLE       = (B_OPS_C_F | COP0_COND_OLE ), // 0b110110 (54): C.OLE.[fmt]  Floating-point Compare (Ordered or Less Than or Equal) (1cyc).
    OPS_C_ULE       = (B_OPS_C_F | COP0_COND_ULE ), // 0b110111 (55): C.ULE.[fmt]  Floating-point Compare (Unordered or Less Than or Equal) (1cyc).
    OPS_C_SF        = (B_OPS_C_F | COP0_COND_SF  ), // 0b111000 (56): C.SF.[fmt]   Floating-point Compare (Signaling False) (1cyc).
    OPS_C_NGLE      = (B_OPS_C_F | COP0_COND_NGLE), // 0b111001 (57): C.NGLE.[fmt] Floating-point Compare (Not Greater or Less Than or Equal) (1cyc).
    OPS_C_SEQ       = (B_OPS_C_F | COP0_COND_SEQ ), // 0b111010 (58): C.SEQ.[fmt]  Floating-point Compare (Signalling Equal) (1cyc).
    OPS_C_NGL       = (B_OPS_C_F | COP0_COND_NGL ), // 0b111011 (59): C.NGL.[fmt]  Floating-point Compare (Not Greater or Less Than) (1cyc).
    OPS_C_LT        = (B_OPS_C_F | COP0_COND_LT  ), // 0b111100 (60): C.LT.[fmt]   Floating-point Compare (Less Than) (1cyc).
    OPS_C_NGE       = (B_OPS_C_F | COP0_COND_NGE ), // 0b111101 (61): C.NGE.[fmt]  Floating-point Compare (Not Greater Than or Equal) (1cyc).
    OPS_C_LE        = (B_OPS_C_F | COP0_COND_LE  ), // 0b111110 (62): C.LE.[fmt]   Floating-point Compare (Less Than or Equal) (1cyc).
    OPS_C_NGT       = (B_OPS_C_F | COP0_COND_NGT ), // 0b111111 (63): C.NGT.[fmt]  Floating-point Compare (Not Greater Than) (1cyc).
} InsnDB_COP1_sub10_Opcodes;

// Pseudo-instruction IDs for insn_db_pseudo.
typedef enum PseudoInsns {
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
} PseudoInsns;

// Types of instructions.
// For COPz instructions, equivalent to cop_subtype.
typedef enum InsnType {
    INSN_TYPE_COP_FMT = 0b00, // Use 'fmt' as opcode.
    INSN_TYPE_REGIMM  = 0b01, // Use 'regimm' as opcode.
    INSN_TYPE_FUNC    = 0b10, // Use 'func' as opcode.
    INSN_TYPE_UNKNOWN = 0b11, // Unknown.
    INSN_TYPE_OPCODE, // Use 'opcode' as opcode.
} InsnType;

// Instruction data
//! TODO: Clean this up if it's possible to make the structs not overwrite each other.
typedef union InsnData {
    struct PACKED {
        /*0x00*/ Word opcode        :  6; // opcode.
        /*0x00*/ Word rs            :  5; // AKA: base.
        /*0x00*/ Word rt            :  5; // AKA: regimm.
        /*0x00*/ Word rd            :  5;
        /*0x00*/ Word sa            :  5;
        /*0x00*/ Word func          :  6; // INSN_TYPE_FUNC opcode.
    };
    struct PACKED {
        /*0x00*/ Word               :  6;
        /*0x00*/ Word base          :  5;
        /*0x00*/ Word               :  5;
        /*0x00*/ Word offset        : 16; // AKA: immediate.
    };
    struct PACKED {
        /*0x00*/ Word               : 11;
        /*0x00*/ Word regimm        :  5; // INSN_TYPE_REGIMM opcode. AKA: rt.
        /*0x00*/ Word immediate     : 16; // AKA: offset.
    };
    struct PACKED {
        /*0x00*/ Word               : 11;
        /*0x00*/ Word ft            :  5; // AKA: branch condition.
        /*0x00*/ Word fs            :  5;
        /*0x00*/ Word fd            :  5;
        /*0x00*/ Word               :  6;
    };
    struct PACKED {
        /*0x00*/ Word cop_opcode    :  4;
        /*0x00*/ Word cop_num       :  2; // Coprocessor number.
        /*0x00*/ Word cop_subtype   :  2;
        /*0x00*/ Word fmt           :  3; // INSN_TYPE_COP_FMT opcode.
        /*0x00*/ Word cop_bcond     :  5; // 0b00010 = likely, 0b00001 = true.
        /*0x00*/ Word               : 10;
        /*0x00*/ Word FC            :  2;
        /*0x00*/ Word cond          :  4;
    };
    struct PACKED {
        /*0x00*/ Word               :  6;
        /*0x00*/ Word instr_index   : 26;
    };
    struct PACKED {
        /*0x00*/ Word               : 16;
        /*0x00*/ Word code10        : 10; // 10-bit data for exception handler.
        /*0x00*/ Word               :  6;
    };
    struct PACKED {
        /*0x00*/ Word               :  6;
        /*0x00*/ Word code20        : 20; // 20-bit data for exception handler.
        /*0x00*/ Word               :  6;
    };
    Word raw;
} InsnData; /*0x04*/


// All chars that can appear in an instruction name, packed by '_PL()'.
#define INSN_ALPHABET_STR_0 "\0."
#define INSN_ALPHABET_STR_N "0123" // Numbers 4-9 do not appear in amy instruction names, but all other alphanumeric chars do.
#define INSN_ALPHABET_STR_A "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

// Pack single char (see insn_alphabet and above defines for format). Can be used by the preprocessor.
#define _PL(_c) ( \
    ((_c) == '.') \
    ? 1 \
    : ( \
        ((_c) >= '0' && (_c) <= '3') \
        ? (((_c) - '0') + STRLEN(INSN_ALPHABET_STR_0)) \
        : ( \
            IS_UPPERCASE(_c) \
            ? (((_c) - 'A') + STRLEN(INSN_ALPHABET_STR_0 INSN_ALPHABET_STR_N)) \
            : 0 \
        ) \
    ) \
)
// Unpack single char.
#define _UL(_arr, _c) (_arr)[_c]

#define PCKCSZ 5 // Num bits in packed char.

// Pack and shift a single char.
#define PACK_CHR(_str, _idx)    ((u64)(_PL((_str)[_idx]) & BITMASK(PCKCSZ)) << (PCKCSZ * (_idx)))
// Unpack and shift a single char.
#define UNPACK_CHR(_src, _idx)  _UL(insn_alphabet, (unsigned char)(((_src) >> (PCKCSZ * (_idx))) & BITMASK(PCKCSZ)))

// Pack a string of 7 chars.
#define PACK_STR7(_str) ( \
    PACK_CHR(_str, 0) | \
    PACK_CHR(_str, 1) | \
    PACK_CHR(_str, 2) | \
    PACK_CHR(_str, 3) | \
    PACK_CHR(_str, 4) | \
    PACK_CHR(_str, 5) | \
    PACK_CHR(_str, 6)   \
)
// Unpack a string of 7 (+ null terminator) chars.
#define UNPACK_STR7(_src) { \
    UNPACK_CHR((_src), 0), \
    UNPACK_CHR((_src), 1), \
    UNPACK_CHR((_src), 2), \
    UNPACK_CHR((_src), 3), \
    UNPACK_CHR((_src), 4), \
    UNPACK_CHR((_src), 5), \
    UNPACK_CHR((_src), 6), \
    '\0', \
}

typedef enum PACKED DisasmExtraPrintedRegisters {
    EXR_00,
    EXR_HI,
    EXR_LO,
    EXR_LL,
    EXR_EN,
    EXR_IX,
    EXR_RD,
    EXR_FP,
    NUM_EXR_TYPES,
} DisasmExtraPrintedRegisters;

// Instruction database format.
typedef union InsnTemplate {
    struct PACKED {
        /*0x00*/ struct PACKED {
            /*00*/ u64 name   : 35; // (PCKCSZ * 7) Packed name (7 5-bit chars). 
            /*35*/ u16 params :  5; // enum MIPSParamFmts.
            /*40*/ u16 output :  2; // [0=none,1=p1,2=p2,3=p3]
            /*42*/ u16 ex1    :  3; // 1st extra register slot (enum DisasmExtraPrintedRegisters).
            /*45*/ u16 ex2    :  3; // 2nd extra register slot (enum DisasmExtraPrintedRegisters).
        }; /*0x06*/
        /*0x06*/ struct PACKED {
            /*00*/ u8 exo    :  2; // Mask for extra registers [0b0=input,0b1=output].
            /*02*/ u8 opcode :  6; // Opcode to compare to.
        }; /*0x01*/
        /*0x07*/ struct PACKED {
            /*00*/ u8 hasFmt  : 1; // Whether the name has a format suffix.
            /*01*/ u8 pseudoC : 7; // enum MIPS_C_Pseudocodes //! TODO:
        };
    };
    u64 raw;
    u64 raw64[1];
    u32 raw32[2];
    u16 raw16[4];
    u8  raw8[8];
} InsnTemplate; /*0x08*/
#define SIZEOF_INSN_TEMPLATE sizeof(InsnTemplate)

#define INSN_DB_IMPL(_opcode, _name, _hasFmt, _params, _output, _ex1, _ex2, _exo) { \
    .opcode = _opcode,          \
    .name   = PACK_STR7(_name), \
    .hasFmt = _hasFmt,          \
    .params = _params,          \
    .output = _output,          \
    .ex1    = _ex1,             \
    .ex2    = _ex2,             \
    .exo    = _exo,             \
}

#define INSN_DB(_opcode, _name, _hasFmt, _params, _output) \
    INSN_DB_IMPL(_opcode, _name, _hasFmt, _params, _output, EXR_00, EXR_00, 0b00)
#define INSN_EX(_opcode, _name, _hasFmt, _params, _output, _ex1, _ex2, _exo) \
    INSN_DB_IMPL(_opcode, _name, _hasFmt, _params, _output, _ex1, _ex2, _exo)
#define INSN_END() {}


typedef enum PACKED RegFmts {
    MP_FMT_I = REG_VAL_TYPE_INT,
    MP_FMT_F = REG_VAL_TYPE_FLOAT,
    MP_FMT_O,
} RegFmts;

typedef union InsnParam {
    struct PACKED {
        const u8 id  : 4; // enum MIPSParamID
        const u8 fmt : 2; // [0=int,1=flt,2=overwrite,3=?]
        const u8     : 2; //! TODO:
    };
    u8 raw;
} InsnParam;
#define SIZEOF_PARAM sizeof(InsnParam)


#define INSN_OFFSET_FROM_ADDR(_addr, _insnOffset) ((Address)(_addr) + (sizeof(InsnData) * (s16)(_insnOffset)))
//! TODO: Are any of these subject to change?
#define ADDR_INSN_ASSERT                    INSN_OFFSET_FROM_ADDR(__n64Assert,   8) // 9th instruction in __n64Assert (the 'syscall').
#define ADDR_INSN_WAITING_FOR_MESG          INSN_OFFSET_FROM_ADDR(osRecvMesg,   26) // 27th instruction in osRecvMesg.
#define ADDR_INSN_STRLEN_DEREFERENCE_ARG    INSN_OFFSET_FROM_ADDR(strlen,        0) // 1st instruction in strlen (dereference arg).


#define REG_BUFFER_SIZE (3 + 3) // 3 registers from instruction + up to 3 extra registers.
extern RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
extern int gSavedRegBufSize;

s16 insn_check_for_branch_offset(InsnData insn);
Address get_insn_branch_target_from_addr(Address addr);
char* cs_insn_to_string(Address addr, InsnData insn, const char** fname, _Bool formatting);
