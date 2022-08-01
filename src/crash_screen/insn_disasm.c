#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "color_presets.h"
#include "crash_screen.h"
#include "insn_disasm.h"
#include "map_parser.h"
#include "engine/math_util.h"

// MIPS III Instructions
static const InsnTemplate insn_db[] = {
//            COP1,    fmt,       rt,      fs,      fd,      MOV
    // Floating point
    // branch
    {{.i={0b010001, 0b01000, 0b00000,       0,       0,        0}}, PARAM_BC1, "BC1F"   },
    {{.i={0b010001, 0b01000, 0b00001,       0,       0,        0}}, PARAM_BC1, "BC1T"   },
    {{.i={0b010001, 0b01000, 0b00010,       0,       0,        0}}, PARAM_BC1, "BC1FL"  },
    {{.i={0b010001, 0b01000, 0b00011,       0,       0,        0}}, PARAM_BC1, "BC1TL"  },
    // memory access
    {{.i={0b010001, 0b00000,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "MFC1"   },
    // {{.i={0b010001, 0b00001,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "DMFC1"  },
    {{.i={0b010001, 0b00100,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "MTC1"   },
    // {{.i={0b010001, 0b00101,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "DMTC1"  },
    // {{.i={0b010001, 0b00010,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "CFC1"   },
    // {{.i={0b010001, 0b00110,       0,       0, 0b00000, 0b000000}}, PARAM_TFS, "CTC1"   },
    // arithmetic
    {{.i={0b010001,       0,       0,       0,       0, 0b000000}}, PARAM_FFF, "ADD"    },
    {{.i={0b010001,       0,       0,       0,       0, 0b000001}}, PARAM_FFF, "SUB"    },
    {{.i={0b010001,       0,       0,       0,       0, 0b000010}}, PARAM_FFF, "MUL"    },
    {{.i={0b010001,       0,       0,       0,       0, 0b000011}}, PARAM_FFF, "DIV"    },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b000100}}, PARAM_FF,  "SQRT"   },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b000101}}, PARAM_FF,  "ABS"    },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b000110}}, PARAM_FF,  "MOV"    },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b000111}}, PARAM_FF,  "NEG"    },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001000}}, PARAM_FF,  "ROUND.L"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001001}}, PARAM_FF,  "TRUNC.L"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001010}}, PARAM_FF,  "CEIL.L" },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001011}}, PARAM_FF,  "FLOOR.L"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001100}}, PARAM_FF,  "ROUND.W"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001101}}, PARAM_FF,  "TRUNC.W"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001110}}, PARAM_FF,  "CEIL.W" },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b001111}}, PARAM_FF,  "FLOOR.W"},
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b100000}}, PARAM_FF,  "CVT.S"  },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b100001}}, PARAM_FF,  "CVT.D"  },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b100100}}, PARAM_FF,  "CVT.W"  },
    {{.i={0b010001,       0, 0b00000,       0,       0, 0b100101}}, PARAM_FF,  "CVT.L"  },
    {{.i={0b010001,       0,       0,       0, 0b00000, 0b110000}}, PARAM_CON, "C"      },
//          opcode,      rs,      rt,      rd,      sa,  function
    // Arithmetic
    // add
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b100000}}, PARAM_DST, "ADD"    },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100001}}, PARAM_DST, "ADDU"   },
    // {{.i={0b001000,       0,       0,       0,       0,        0}}, PARAM_TSI, "ADDI"   },
    {{.i={0b001001,       0,       0,       0,       0,        0}}, PARAM_TSI, "ADDIU"  },
    // sub
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100010}}, PARAM_DST, "SUB"    },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100011}}, PARAM_DST, "SUBU"   },
    // and
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100100}}, PARAM_DST, "AND"    },
    {{.i={0b001100,       0,       0,       0,       0,        0}}, PARAM_TSI, "ANDI"   },
    // or
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100101}}, PARAM_DST, "OR"     },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100110}}, PARAM_DST, "XOR"    },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b100111}}, PARAM_DST, "NOR"    },
    {{.i={0b001101,       0,       0,       0,       0,        0}}, PARAM_TSI, "ORI"    },
    {{.i={0b001110,       0,       0,       0,       0,        0}}, PARAM_TSI, "XORI"   },
    // lui
    {{.i={0b001111,       0,       0,       0,       0,        0}}, PARAM_TI,  "LUI"    },
    // set
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b101010}}, PARAM_DST, "SLT"    },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b101011}}, PARAM_DST, "SLTU"   },
    {{.i={0b001010,       0,       0,       0,       0,        0}}, PARAM_TSI, "SLTI"   },
    {{.i={0b001011,       0,       0,       0,       0,        0}}, PARAM_TSI, "SLTIU"  },
    // doubleword add
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b101100}}, PARAM_DST, "DADD"   },
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b101101}}, PARAM_DST, "DADDU"  },
    // {{.i={0b011000,       0,       0,       0,       0,        0}}, PARAM_TSI, "DADDI"  },
    // {{.i={0b011001,       0,       0,       0,       0,        0}}, PARAM_TSI, "DADDIU" },
    // dooubleword sub
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b101110}}, PARAM_DST, "DSUB"   },
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b101111}}, PARAM_DST, "DSUBU"  },

    // Shifter
    {{.i={0b000000, 0b00000,       0,       0,       0, 0b000000}}, PARAM_DTA, "SLL"    },
    {{.i={0b000000, 0b00000,       0,       0,       0, 0b000010}}, PARAM_DTA, "SRL"    },
    {{.i={0b000000, 0b00000,       0,       0,       0, 0b000011}}, PARAM_DTA, "SRA"    },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b000100}}, PARAM_DTS, "SLLV"   },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b000110}}, PARAM_DTS, "SRLV"   },
    {{.i={0b000000,       0,       0,       0, 0b00000, 0b000111}}, PARAM_DTS, "SRAV"   },
    // doubleword
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111000}}, PARAM_DTA, "DSLL"   },
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111010}}, PARAM_DTA, "DSRL"   },
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111011}}, PARAM_DTA, "DSRA"   },
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111100}}, PARAM_DTA, "DSLL32" },
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111110}}, PARAM_DTA, "DSRL32" },
    // {{.i={0b000000, 0b00000,       0,       0,       0, 0b111111}}, PARAM_DTA, "DSRA32" },
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b010100}}, PARAM_DTS, "DSLLV"  },
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b010110}}, PARAM_DTS, "DSRLV"  },
    // {{.i={0b000000,       0,       0,       0, 0b00000, 0b010111}}, PARAM_DTS, "DSRAV"  },

    // Multiply
    // move hi/lo
    {{.i={0b000000, 0b00000, 0b00000,       0, 0b00000, 0b010000}}, PARAM_D,   "MFHI"   },
    {{.i={0b000000,       0, 0b00000, 0b00000, 0b00000, 0b010001}}, PARAM_S,   "MTHI"   },
    {{.i={0b000000, 0b00000, 0b00000,       0, 0b00000, 0b010010}}, PARAM_D,   "MFLO"   },
    {{.i={0b000000,       0, 0b00000, 0b00000, 0b00000, 0b010011}}, PARAM_S,   "MTLO"   },
    // mult
    {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011000}}, PARAM_ST,  "MULT"   },
    {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011001}}, PARAM_ST,  "MULTU"  },
    // div
    {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011010}}, PARAM_ST,  "DIV"    },
    {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011011}}, PARAM_ST,  "DIVU"   },
    // doubleword mult
    // {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011100}}, PARAM_ST,  "DMULT"  },
    // {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011101}}, PARAM_ST,  "DMULTU" },
    // // doubleword div
    // {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011110}}, PARAM_ST,  "DDIV"   },
    // {{.i={0b000000,       0,       0, 0b00000, 0b00000, 0b011111}}, PARAM_ST,  "DDIVU"  },

    // Branch
    {{.i={0b000001,       0, 0b00001,       0,       0,        0}}, PARAM_SO,  "BGEZ"   },
    {{.i={0b000001,       0, 0b00010,       0,       0,        0}}, PARAM_SO,  "BLTZL"  },
    {{.i={0b000001,       0, 0b00011,       0,       0,        0}}, PARAM_SO,  "BGEZL"  },
    // {{.i={0b000001,       0, 0b10000,       0,       0,        0}}, PARAM_SO,  "BLTZAL" },
    // {{.i={0b000001,       0, 0b10001,       0,       0,        0}}, PARAM_SO,  "BGEZAL" },
    // {{.i={0b000001,       0, 0b10010,       0,       0,        0}}, PARAM_SO,  "BLTZALL"},
    // {{.i={0b000001,       0, 0b10011,       0,       0,        0}}, PARAM_SO,  "BGEZALL"},
    {{.i={0b000111,       0, 0b00000,       0,       0,        0}}, PARAM_SO,  "BGTZ"   },
    {{.i={0b010111,       0, 0b00000,       0,       0,        0}}, PARAM_SO,  "BGTZL"  },
    {{.i={0b000110,       0, 0b00000,       0,       0,        0}}, PARAM_SO,  "BLEZ"   },
    {{.i={0b010110,       0, 0b00000,       0,       0,        0}}, PARAM_SO,  "BLEZL"  },
    {{.i={0b000001,       0, 0b00000,       0,       0,        0}}, PARAM_SO,  "BLTZ"   },
    {{.i={0b000100, 0b00000, 0b00000,       0,       0,        0}}, PARAM_B,   "B"      }, // pseudo of beq
    {{.i={0b000100,       0,       0,       0,       0,        0}}, PARAM_STO, "BEQ"    },
    {{.i={0b000101,       0,       0,       0,       0,        0}}, PARAM_STO, "BNE"    },
    {{.i={0b010100,       0,       0,       0,       0,        0}}, PARAM_STO, "BEQL"   },
    {{.i={0b010101,       0,       0,       0,       0,        0}}, PARAM_STO, "BNEL"   },
    // jal (special)
    {{.i={0b000010,       0,       0,       0,       0,        0}}, PARAM_J,   "J"      },
    {{.i={0b000011,       0,       0,       0,       0,        0}}, PARAM_J,   "JAL"    },
    {{.i={0b000000,       0, 0b00000, 0b00000, 0b00000, 0b001000}}, PARAM_S,   "JR"     },
    {{.i={0b000000,       0, 0b00000,       0, 0b00000, 0b001001}}, PARAM_DS,  "JALR"   },
    // {{.i={0b000000,       0,       0,       0,       0, 0b001101}}, PARAM_N,   "BREAK"  },
    // move
    // {{.i={0b010000, 0b00000,       0,       0, 0b00000, 0b000000}}, PARAM_TD,  "MFC0"   },
    // {{.i={0b010000, 0b00100,       0,       0, 0b00000, 0b000000}}, PARAM_TD,  "MTC0"   },
    // system call
    // {{.i={0b000000, 0b00000, 0b00000, 0b00000, 0b00000, 0b001100}}, PARAM_SYS, "SYSCALL"},
    // sync
    // {{.i={0b000000, 0b00000, 0b00000, 0b00000,       0, 0b001111}}, PARAM_SYN, "SYNC"   },
    // trap
    // {{.i={0b000000,       0,       0,       0,       0, 0b110000}}, PARAM_ST2, "TGE"    },
    // {{.i={0b000000,       0,       0,       0,       0, 0b110001}}, PARAM_ST2, "TGEU"   },
    // {{.i={0b000000,       0,       0,       0,       0, 0b110010}}, PARAM_ST2, "TLT"    },
    // {{.i={0b000000,       0,       0,       0,       0, 0b110011}}, PARAM_ST2, "TLTU"   },
    {{.i={0b000000,       0,       0,       0,       0, 0b110100}}, PARAM_ST2, "TEQ"    },
    // {{.i={0b000000,       0,       0,       0,       0, 0b110110}}, PARAM_ST2, "TNE"    },
    // {{.i={0b000001,       0, 0b01000,       0,       0,        0}}, PARAM_SI,  "TGEI"   },
    // {{.i={0b000001,       0, 0b01001,       0,       0,        0}}, PARAM_SI,  "TGEIU"  },
    // {{.i={0b000001,       0, 0b01010,       0,       0,        0}}, PARAM_SI,  "TLTI"   },
    // {{.i={0b000001,       0, 0b01011,       0,       0,        0}}, PARAM_SI,  "TLTIU"  },
    // {{.i={0b000001,       0, 0b01100,       0,       0,        0}}, PARAM_SI,  "TEQI"   },
    // {{.i={0b000001,       0, 0b01110,       0,       0,        0}}, PARAM_SI,  "TNEI"   },

    // Memory Access
    // coprocessor
    // {{.i={0b010000,       0,       0,       0,       0,        0}}, PARAM_N,   "COP0"   },
    // {{.i={0b010001,       0,       0,       0,       0,        0}}, PARAM_N,   "COP1"   },
    // {{.i={0b010010,       0,       0,       0,       0,        0}}, PARAM_N,   "COP2"   },
    // {{.i={0b010011,       0,       0,       0,       0,        0}}, PARAM_N,   "COP3"   },
    // load
    // {{.i={0b011010,       0,       0,       0,       0,        0}}, PARAM_TIS, "LDL"    },
    // {{.i={0b011011,       0,       0,       0,       0,        0}}, PARAM_TIS, "LDR"    },
    {{.i={0b100000,       0,       0,       0,       0,        0}}, PARAM_TIS, "LB"     },
    {{.i={0b100001,       0,       0,       0,       0,        0}}, PARAM_TIS, "LH"     },
    {{.i={0b100010,       0,       0,       0,       0,        0}}, PARAM_TIS, "LWL"    },
    {{.i={0b100011,       0,       0,       0,       0,        0}}, PARAM_TIS, "LW"     },
    {{.i={0b100100,       0,       0,       0,       0,        0}}, PARAM_TIS, "LBU"    },
    {{.i={0b100101,       0,       0,       0,       0,        0}}, PARAM_TIS, "LHU"    },
    {{.i={0b100110,       0,       0,       0,       0,        0}}, PARAM_TIS, "LWR"    },
    // {{.i={0b100111,       0,       0,       0,       0,        0}}, PARAM_TIS, "LWU"    },
    // save
    {{.i={0b101000,       0,       0,       0,       0,        0}}, PARAM_TIS, "SB"     },
    {{.i={0b101001,       0,       0,       0,       0,        0}}, PARAM_TIS, "SH"     },
    // {{.i={0b101010,       0,       0,       0,       0,        0}}, PARAM_TIS, "SWL"    },
    {{.i={0b101011,       0,       0,       0,       0,        0}}, PARAM_TIS, "SW"     },
    // {{.i={0b101100,       0,       0,       0,       0,        0}}, PARAM_TIS, "SDL"    },
    // {{.i={0b101101,       0,       0,       0,       0,        0}}, PARAM_TIS, "SDR"    },
    // {{.i={0b101110,       0,       0,       0,       0,        0}}, PARAM_TIS, "SWR"    },
    // {{.i={0b110000,       0,       0,       0,       0,        0}}, PARAM_TIS, "LL"     },
    {{.i={0b110001,       0,       0,       0,       0,        0}}, PARAM_FIS, "LWC1"   },
    // {{.i={0b110010,       0,       0,       0,       0,        0}}, PARAM_FIS, "LWC2"   },
    // {{.i={0b110011,       0,       0,       0,       0,        0}}, PARAM_FIS, "LWC3"   },
    // {{.i={0b110100,       0,       0,       0,       0,        0}}, PARAM_TIS, "LLD"    },
    {{.i={0b110101,       0,       0,       0,       0,        0}}, PARAM_TIS, "LDC1"   },
    // {{.i={0b110110,       0,       0,       0,       0,        0}}, PARAM_TIS, "LDC2"   },
    // {{.i={0b110111,       0,       0,       0,       0,        0}}, PARAM_TIS, "LD"     },
    // {{.i={0b111000,       0,       0,       0,       0,        0}}, PARAM_TIS, "SC"     },
    {{.i={0b111001,       0,       0,       0,       0,        0}}, PARAM_FIS, "SWC1"   },
    // {{.i={0b111010,       0,       0,       0,       0,        0}}, PARAM_FIS, "SWC2"   },
    // {{.i={0b111011,       0,       0,       0,       0,        0}}, PARAM_FIS, "SWC3"   },
    // {{.i={0b111100,       0,       0,       0,       0,        0}}, PARAM_TIS, "SCD"    },
    {{.i={0b111101,       0,       0,       0,       0,        0}}, PARAM_TIS, "SDC1"   },
    // {{.i={0b111110,       0,       0,       0,       0,        0}}, PARAM_TIS, "SDC2"   },
    // {{.i={0b111111,       0,       0,       0,       0,        0}}, PARAM_TIS, "SD"     },
};

static const InsnData insn_masks[] = {
//                       opcode,      rs,      rt,      rd,      sa, function
    /*PARAM_N  */ {.i={0b111111, 0b11111, 0b11111, 0b11111,       0, 0b111111}},
    /*PARAM_SYS*/ {.i={0b111111,       0,       0,       0,       0, 0b111111}},
    /*PARAM_SYN*/ {.i={0b111111, 0b11111, 0b11111, 0b11111, 0b11111, 0b111111}},
    /*PARAM_S  */ {.i={0b111111,       0, 0b11111, 0b11111, 0b11111, 0b111111}},
    /*PARAM_T  */ {.i={0b111111, 0b11111,       0, 0b11111, 0b11111, 0b111111}},
    /*PARAM_D  */ {.i={0b111111, 0b11111, 0b11111,       0, 0b11111, 0b111111}},
    /*PARAM_ST */ {.i={0b111111,       0,       0, 0b11111, 0b11111, 0b111111}},
    /*PARAM_ST2*/ {.i={0b111111,       0,       0,       0,       0, 0b111111}},
    /*PARAM_DS */ {.i={0b111111,       0, 0b11111,       0, 0b11111, 0b111111}},
    /*PARAM_TD */ {.i={0b111111, 0b11111,       0,       0, 0b11111, 0b111111}},
    /*PARAM_SD */ {.i={0b111111,       0, 0b11111,       0, 0b11111, 0b111111}},
    /*PARAM_STD*/ {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}},
    /*PARAM_DST*/ {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}},
    /*PARAM_DTS*/ {.i={0b111111,       0,       0,       0, 0b11111, 0b111111}},
    /*PARAM_DTA*/ {.i={0b111111, 0b11111,       0,       0,       0, 0b111111}},
    /*PARAM_SI */ {.i={0b111111,       0, 0b11111,       0,       0,        0}},
    /*PARAM_TI */ {.i={0b111111, 0b11111,       0,       0,       0,        0}},
    /*PARAM_STI*/ {.i={0b111111,       0,       0,       0,       0,        0}},
    /*PARAM_TSI*/ {.i={0b111111,       0,       0,       0,       0,        0}},
    /*PARAM_TIS*/ {.i={0b111111,       0,       0,       0,       0,        0}},
    /*PARAM_SO */ {.i={0b111111,       0, 0b11111,       0,       0,        0}},
    /*PARAM_STO*/ {.i={0b111111,       0,       0,       0,       0,        0}},
    /*PARAM_B  */ {.i={0b111111, 0b11111, 0b11111,       0,       0,        0}},
    /*PARAM_J  */ {.i={0b111111,       0,       0,       0,       0,        0}},
//                         COP1,    fmt,       rt,      fs,      fd,      MOV
    /*PARAM_FIS*/ {.i={0b111111,       0,       0,       0,       0,        0}},
    /*PARAM_TFS*/ {.i={0b111111, 0b11111,       0,       0, 0b11111, 0b111111}},
    /*PARAM_FF */ {.i={0b111111,       0, 0b11111,       0,       0, 0b111111}},
    /*PARAM_FFF*/ {.i={0b111111,       0,       0,       0,       0, 0b111111}},
    /*PARAM_CON*/ {.i={0b111111,       0,       0,       0, 0b00011, 0b110000}},
    /*PARAM_BC1*/ {.i={0b111111, 0b11111, 0b00011,       0,       0,        0}},
};

static const char registerMaps[][3] = {
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

    // "F0", "F2",                                     // Subroutine return value
    // "F4", "F6", "F8", "F10"                         // Temporary values
    // "F12", "F14",                                   // Subroutine arguments
    // "F16", "F18",                                   // Temporary values
    // "F20", "F22", "F24", "F26", "F28", "F30",       // Saved Values

static const char conditions[][5] = {
    "F",  "UN",   "EQ",  "UEQ", "OLT", "ULT", "OLE", "ULE",
    "SF", "NGLE", "SEQ", "NGL", "LT",  "NGE", "LE",  "NGT"
};

static const RGBA32 sDisasmColors[] = {
    /*DISASM_COLOR_NOP      */ COLOR_RGBA32_CRASH_DISASM_NOP,
    /*DISASM_COLOR_INSN     */ COLOR_RGBA32_CRASH_DISASM_INST,
    /*DISASM_COLOR_REG      */ COLOR_RGBA32_CRASH_DISASM_REG,
    /*DISASM_COLOR_REG_2    */ COLOR_RGBA32_CRASH_DISASM_REG_2,
    /*DISASM_COLOR_IMMEDIATE*/ COLOR_RGBA32_CRASH_IMMEDIATE,
    /*DISASM_COLOR_ADDRESS  */ COLOR_RGBA32_CRASH_FUNCTION_NAME,
    /*DISASM_COLOR_OFFSET   */ COLOR_RGBA32_CRASH_FUNCTION_NAME_2,
};

static char insn_as_string[CHAR_BUFFER_SIZE];

static char fmt_to_char(InsnData insn) {
    switch (insn.i.rs) {
        case 16: return 'S';
        case 17: return 'D';
        case 20: return 'W';
        case 21: return 'L';
    }

    return 'X';
}

const InsnTemplate *get_insn_type(InsnData insn) {
    if (insn.d) {
        for (s32 i = 0; i < ARRAY_COUNT(insn_db); i++) {
            if ((insn.d & insn_masks[insn_db[i].paramType].d) == insn_db[i].i.d) {
                return &insn_db[i];
            }
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
            case PARAM_B:
            case PARAM_BC1:
                return insn.i.immediate;
        }
    }

    return 0;
}

char *insn_disasm(InsnData insn, u32 isPC) {
    char *strp = &insn_as_string[0];
    uintptr_t target;
    s16 branchOffset;
    char *fname = NULL;
    char insn_name[10];

    bzero(insn_as_string, sizeof(insn_as_string));
    bzero(insn_name, sizeof(insn_name));

    if (insn.d == 0) { // trivial case
        if (isPC) {
            strp += sprintf(strp, "@%08XNOP @%08X<-- CRASH",
                sDisasmColors[DISASM_COLOR_NOP],
                COLOR_RGBA32_CRASH_AT
            );
        } else {
            strp += sprintf(strp, "@%08XNOP",
                sDisasmColors[DISASM_COLOR_NOP]
            );
        }

        return insn_as_string;
    }

    const InsnTemplate *type = get_insn_type(insn);

    if (type) {
        switch (type->paramType) {
            case PARAM_SYS:
            case PARAM_SYN:
            case PARAM_N:
                strp += sprintf(strp, "@%08X%-6s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name
                );
                break;
            case PARAM_S:
                strp += sprintf(strp, "@%08X%-6s @%08X%s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rs]
                );
                break;
            case PARAM_T:
                strp += sprintf(strp, "@%08X%-6s @%08X%s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rt]
                );
                break;
            case PARAM_D:
                strp += sprintf(strp, "@%08X%-6s @%08X%s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rdata.rd]
                );
                break;
            case PARAM_ST:
            case PARAM_ST2:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rs],
                                                      registerMaps[insn.i.rt]
                );
                break;
            case PARAM_DS:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rdata.rd],
                                                      registerMaps[insn.i.rs]
                );
                break;
            case PARAM_TD:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rt],
                                                      registerMaps[insn.i.rdata.rd]
                );
                break;
            case PARAM_SD:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rs],
                                                      registerMaps[insn.i.rdata.rd]
                );
                break;
            case PARAM_STD:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rs],
                                                      registerMaps[insn.i.rt],
                                                      registerMaps[insn.i.rdata.rd]
                );
                break;
            case PARAM_DST:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rdata.rd],
                                                      registerMaps[insn.i.rs],
                                                      registerMaps[insn.i.rt]
                );
                break;
            case PARAM_DTS:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, %s",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rdata.rd],
                                                      registerMaps[insn.i.rt],
                                                      registerMaps[insn.i.rs]
                );
                break;
            case PARAM_DTA:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rdata.rd],
                                                           registerMaps[insn.i.rt],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.rdata.sa
                );
                break;
            case PARAM_SI:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rs],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                );
                break;
            case PARAM_TI:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rt],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                );
                break;
            case PARAM_STI:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rs],
                                                           registerMaps[insn.i.rt],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                );
                break;
            case PARAM_TSI:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rt],
                                                           registerMaps[insn.i.rs],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate
                );
                break;
            case PARAM_TIS:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X0x%04X@%08X(%s)",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], registerMaps[insn.i.rt],
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate,
                    sDisasmColors[DISASM_COLOR_REG_2    ], registerMaps[insn.i.rs]
                );
                break;
            case PARAM_SO:
                branchOffset = (1 + insn.i.immediate);
                strp += sprintf(strp, "@%08X%-6s @%08X%s, @%08X%s0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                    sDisasmColors[DISASM_COLOR_REG   ], registerMaps[insn.i.rs],
                    sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), ABS(branchOffset)
                );
                break;
            case PARAM_STO:
                branchOffset = (1 + insn.i.immediate);
                strp += sprintf(strp, "@%08X%-6s @%08X%s, %s, @%08X%s0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                    sDisasmColors[DISASM_COLOR_REG   ], registerMaps[insn.i.rs],
                                                        registerMaps[insn.i.rt],
                    sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), ABS(branchOffset)
                );
                break;
            case PARAM_B:
            case PARAM_BC1:
                branchOffset = (1 + insn.i.immediate);
                strp += sprintf(strp, "@%08X%-6s @%08X%s0x%04X",
                    sDisasmColors[DISASM_COLOR_INSN  ], type->name,
                    sDisasmColors[DISASM_COLOR_OFFSET], ((branchOffset < 0) ? "-" : "+"), ABS(branchOffset)
                );
                break;
            case PARAM_J:
                target = (0x80000000 | ((insn.d & 0x1FFFFFF) * sizeof(InsnData)));
                strp += sprintf(strp, "@%08X%-6s @%08X0x%08X",
                    sDisasmColors[DISASM_COLOR_INSN   ], type->name,
                    sDisasmColors[DISASM_COLOR_ADDRESS], target
                );
#ifdef INCLUDE_DEBUG_MAP
                fname = parse_map_exact(target);
                if (!((fname == NULL) || ((*(uintptr_t*)target & 0x80000000) == 0))) {
                    strp += sprintf(strp, " (%s)", fname);
                }
#endif
                break;
            case PARAM_FIS:
                strp += sprintf(strp, "@%08X%-6s @%08XF%d, @%08X0x%04X@%08X(%s)",
                    sDisasmColors[DISASM_COLOR_INSN     ], type->name,
                    sDisasmColors[DISASM_COLOR_REG      ], insn.i.rt,
                    sDisasmColors[DISASM_COLOR_IMMEDIATE], insn.i.immediate,
                    sDisasmColors[DISASM_COLOR_REG_2    ], registerMaps[insn.i.rs]
                );
                break;
            case PARAM_TFS:
                strp += sprintf(strp, "@%08X%-6s @%08X%s, F%d",
                    sDisasmColors[DISASM_COLOR_INSN], type->name,
                    sDisasmColors[DISASM_COLOR_REG ], registerMaps[insn.i.rt],
                                                      insn.i.rdata.rd // fs
                );
                break;
            case PARAM_FF:
                sprintf(insn_name, "%s.%c", type->name, fmt_to_char(insn));
                strp += sprintf(strp, "@%08X%-6s @%08XF%d, F%d",
                    sDisasmColors[DISASM_COLOR_INSN], insn_name,
                    sDisasmColors[DISASM_COLOR_REG ], insn.i.rdata.sa, // fd
                                                      insn.i.rdata.rd  // fs
                );
                break;
            case PARAM_FFF:
                sprintf(insn_name, "%s.%c", type->name, fmt_to_char(insn));
                strp += sprintf(strp, "@%08X%-6s @%08XF%d, F%d, F%d",
                    sDisasmColors[DISASM_COLOR_INSN], insn_name,
                    sDisasmColors[DISASM_COLOR_REG ], insn.i.rdata.sa, // fd
                                                      insn.i.rdata.rd, // fs
                                                      insn.i.rt        // ft
                );
                break;
            case PARAM_CON:
                sprintf(insn_name, "%s.%s.%c", type->name, conditions[insn.i.rdata.function & BITMASK(4)], fmt_to_char(insn));
                strp += sprintf(strp, "@%08X%-6s @%08XF%d, F%d",
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

    if (isPC) {
        sprintf(strp, " @%08X<-- CRASH", COLOR_RGBA32_CRASH_AT);
    }

    return insn_as_string;
}
