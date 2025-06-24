#include <PR/ultratypes.h>
#include <stdio.h>

#include "sm64.h"
#include "macros.h"
#include "farcall.h"
#include "disasm.h"
#include "map_parser.h"

static char insn_as_string[100];

InsnTemplate insn_db[] = {
    // We want instructions with opcodes first (prioritized)

    // load/store
    {I_TYPE, PARAM_LUI,  0b001111, 0, "lui"},
    {I_TYPE, PARAM_NONE, 0b100000, 0, "lb"},
    {I_TYPE, PARAM_NONE, 0b100100, 0, "lbu"},
    {I_TYPE, PARAM_NONE, 0b101000, 0, "sb"},
    {I_TYPE, PARAM_NONE, 0b100001, 0, "lh"},
    {I_TYPE, PARAM_NONE, 0b100101, 0, "lhu"},
    {I_TYPE, PARAM_NONE, 0b101001, 0, "sh"},
    {I_TYPE, PARAM_NONE, 0b100011, 0, "lw"},
    {I_TYPE, PARAM_NONE, 0b101011, 0, "sw"},
    {I_TYPE, PARAM_NONE, 0b110111, 0, "ld"},
    {I_TYPE, PARAM_NONE, 0b111111, 0, "sd"},
    {I_TYPE, PARAM_FLOAT_RT, 0b110001, 0, "lwc1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b111001, 0, "swc1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b110101, 0, "ldc1"},
    {I_TYPE, PARAM_FLOAT_RT, 0b111101, 0, "sdc1"},

    // unaligned
    {I_TYPE, PARAM_NONE, 0b100010, 0, "lwl"},
    {I_TYPE, PARAM_NONE, 0b100110, 0, "lwr"},
    {I_TYPE, PARAM_NONE, 0b101010, 0, "swl"},
    {I_TYPE, PARAM_NONE, 0b101110, 0, "swr"},
    // atomics
    {I_TYPE, PARAM_NONE, 0b110000, 0, "ll"},
    {I_TYPE, PARAM_NONE, 0b111000, 0, "sc"},
    {I_TYPE, PARAM_NONE, 0b111100, 0, "scd"},
    // branches
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000100, 0, "beq"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010100, 0, "beql"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000101, 0, "bne"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010101, 0, "bnel"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000111, 0, "bgtz"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010111, 0, "bgtzl"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b000110, 0, "blez"},
    {I_TYPE, PARAM_SWAP_RS_IMM, 0b010110, 0, "blezl"},
    {I_TYPE, PARAM_NONE, 0b001010, 0, "slti"},
    {I_TYPE, PARAM_NONE, 0b001011, 0, "sltiu"},

    // jal (special)
    {J_TYPE, PARAM_JAL, 0b000011, 0, "jal"},
    {J_TYPE, PARAM_JUMP, 0b000010, 0, "j"},

    // bitwise ops (which are opcodes)
    {I_TYPE, PARAM_NONE, 0b001100, 0, "andi"},
    {I_TYPE, PARAM_NONE, 0b001101, 0, "ori"},
    {I_TYPE, PARAM_NONE, 0b001110, 0, "xori"},


    // arithmetic
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b011000, 0, "daddi"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b011001, 0, "daddiu"},
    // and now the ones with 0 for the opcode
    {R_TYPE, PARAM_NONE, 0, 0b100000, "add"},
    {R_TYPE, PARAM_NONE, 0, 0b100001, "addu"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b001000, 0, "addi"},
    {I_TYPE, PARAM_SWAP_RS_IMM,  0b001001, 0, "addiu"},
    {R_TYPE, PARAM_NONE, 0, 0b100010, "sub"},
    {R_TYPE, PARAM_NONE, 0, 0b100011, "subu"},
    {R_TYPE, PARAM_NONE, 0, 0b011000, "mult"},
    {R_TYPE, PARAM_NONE, 0, 0b011001, "multu"},
    {R_TYPE, PARAM_NONE, 0, 0b011010, "div"},
    {R_TYPE, PARAM_NONE, 0, 0b011011, "divu"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010000, "mfhi"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010001, "mthi"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010010, "mflo"},
    {R_TYPE, PARAM_MULT_MOVE, 0, 0b010011, "mtlo"},
    {R_TYPE, PARAM_NONE, 0, 0b101010, "slt"},
    {R_TYPE, PARAM_NONE, 0, 0b101011, "sltu"},

    // bitwise ops (which are functions)
    {R_TYPE, PARAM_NONE, 0, 0b100100, "and"},
    {R_TYPE, PARAM_NONE, 0, 0b100101, "or"},
    {R_TYPE, PARAM_NONE, 0, 0b100110, "xor"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b000000, "sll"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000100, "sllv"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b000010, "srl"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000110, "srlv"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b000011, "sra"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b000111, "srav"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b100111, "nor"},

    {R_TYPE, PARAM_NONE, 0, 0b001001, "jalr"},
    {R_TYPE, PARAM_NONE, 0, 0b001000, "jr"},
    {R_TYPE, PARAM_TRAP, 0, 0b110100, "teq"},
    {R_TYPE, PARAM_EMUX, 0, 0b110110, "tne"},

    {0, PARAM_SYSCALL, 0, 0b001100, "syscall"},

    // instructions involving doubles (deprioritized on the list)
    {R_TYPE, PARAM_NONE, 0, 0b101101, "daddu"},
    {R_TYPE, PARAM_NONE, 0, 0b101110, "dsub"},
    {R_TYPE, PARAM_NONE, 0, 0b101111, "dsubu"},
    {R_TYPE, PARAM_NONE, 0, 0b011101, "dmultu"},
    {R_TYPE, PARAM_NONE, 0, 0b011110, "ddiv"},
    {R_TYPE, PARAM_NONE, 0, 0b011111, "ddivu"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010100, "dsllv"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111100, "dsll32"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111110, "dsrl32"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010110, "dsrlv"},
    {R_TYPE, PARAM_BITSHIFT, 0, 0b111111, "dsra32"},
    {R_TYPE, PARAM_SWAP_RS_RT, 0, 0b010111, "dsrav"},
};


char __mips_gpr[][4] = {
    "$r0",
    "$at",
    "$v0", "$v1",
    "$a0", "$a1", "$a2", "$a3",
    "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
    "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7",
    "$t8", "$t9",
    "$k0", "$k1",
    "$gp", "$sp", "$fp", "$ra",
    "$lo", "$hi"
};

char __mips_fpreg[][5] = {
    "$f0", "$f1", "$f2", "$f3",
    "$f4", "$f5", "$f6", "$f7",
    "$f8", "$f9", "$f10", "$f11",
    "$f12", "$f13", "$f14", "$f15",
    "$f16", "$f17", "$f18", "$f19",
    "$f20", "$f21", "$f22", "$f23",
    "$f24", "$f25", "$f26", "$f27",
    "$f28", "$f29", "$f30", "$f31",
};

u8 insn_is_jal(Insn *i) {
    return (i->opcode == 0b000011);
}

u8 insn_is_jalr(Insn *i) {
    return (i->opcode == 0) && (i->rdata.function == 0b001001);
}

// Last Resort C0/C1 disassembler, from libdragon
static void c1_disasm(u32 *ptr, char *out) {
    static const char *fpu_ops[64]= {
        "radd", "rsub", "rmul", "rdiv", "ssqrt", "sabs", "smov", "sneg",
        "sround.l", "strunc.l", "sceil.l", "sfloor.l", "sround.w", "strunc.w", "sceil.w", "sfloor.w",
        "*", "*", "*", "*", "*", "*", "*", "*",
        "*", "*", "*", "*", "*", "*", "*", "*",
        "scvt.s", "scvt.d", "*", "*", "scvt.w", "scvt.l", "*", "*",
        "*", "*", "*", "*", "*", "*", "*", "*", 
        "hc.f", "hc.un", "hc.eq", "hc.ueq", "hc.olt", "hc.ult", "hc.ole", "hc.ule", 
        "hc.sf", "hc.ngle", "hc.seq", "hc.ngl", "hc.lt", "hc.nge", "hc.le", "hc.ngt", 
    };

    char symbuf[64];

    // Disassemble MIPS instruction
    u32 pc = (u32)ptr;
    u32 op = *ptr;
    s16 imm16 = op & 0xFFFF;
    u32 tgt16 = (pc + 4) + (imm16 << 2);
    u32 imm26 = op & 0x3FFFFFF;
    u32 tgt26 = ((pc + 4) & 0xf0000000) | (imm26 << 2);
    const char *rs = __mips_gpr[(op >> 21) & 0x1F];
    const char *rt = __mips_gpr[(op >> 16) & 0x1F];
    const char *rd = __mips_gpr[(op >> 11) & 0x1F];
    const char *opn = "unimpl";
    if (((op >> 26) & 0x3F) == 17) {
        u32 sub = (op >> 21) & 0x1F;
        switch (sub) {
            case 0: opn = "gmfc1"; break;
            case 1: opn = "gdmfc1"; break;
            case 4: opn = "gmtc1"; break;
            case 5: opn = "gdmtc1"; break;
            case 8: switch ((op >> 16) & 0x1F) {
                case 0: opn = "ybc1f"; break;
                case 2: opn = "ybc1fl"; break;
                case 1: opn = "ybc1t"; break;
                case 3: opn = "ybc1tl"; break;
            } break;
            case 16: case 17: case 20: case 21:
                opn = fpu_ops[(op >> 0) & 0x3F];
                sprintf(symbuf, "%s.%s", opn, (sub == 16) ? "s" : (sub == 17) ? "d" : (sub == 20) ? "w" : "l");
                opn = symbuf;
                rt = __mips_fpreg[(op >> 16) & 0x1F];
                rs = __mips_fpreg[(op >> 11) & 0x1F];
                rd = __mips_fpreg[(op >> 6) & 0x1F];
                break;
        }
    }
    switch (*opn) {
#ifdef DEBUG_EXPORT_SYMBOLS
    /* op tgt26 */        case 'j': sprintf(out, "%-9s %08x <%s>", opn+1, tgt26, parse_map(tgt26, FALSE)); break;
    /* op rs, rt, tgt16 */case 'b': sprintf(out, "%-9s %s, %s, %08x <%s>", opn+1, rs, rt, tgt16, parse_map(tgt16, TRUE)); break;
    /* op tgt16 */        case 'y': sprintf(out, "%-9s %08x <%s>", opn+1, tgt16, parse_map(tgt16, TRUE)); break;
#else
    /* op tgt26 */        case 'j': sprintf(out, "%-9s %08x", opn+1, tgt26); break;
    /* op rs, rt, tgt16 */case 'b': sprintf(out, "%-9s %s, %s, %08x", opn+1, rs, rt, tgt16); break;
    /* op tgt16 */        case 'y': sprintf(out, "%-9s %08x", opn+1, tgt16); break;
#endif // DEBUG_EXPORT_SYMBOLS
    /* op rt, rs, imm */  case 'i': sprintf(out, "%-9s %s, %s, %d", opn+1, rt, rs, (s16)op); break;
    /* op rt, imm */      case 'k': sprintf(out, "%-9s %s, %d", opn+1, rt, (s16)op); break;
    /* op rt, imm(rs) */  case 'm': sprintf(out, "%-9s %s, %d(%s)", opn+1, rt, (s16)op, rs); break;
    /* op fd, imm(rs) */  case 'n': sprintf(out, "%-9s %s, %d(%s)", opn+1, __mips_fpreg[(op >> 16) & 0x1F], (s16)op, rs); break;
    /* op rd, rs, rt  */  case 'r': sprintf(out, "%-9s %s, %s, %s", opn+1, rd, rs, rt); break;
    /* op rd, rs */       case 's': sprintf(out, "%-9s %s, %s", opn+1, rd, rs); break;
    /* op rd, rt, sa  */  case 'e': sprintf(out, "%-9s %s, %s, %ld", opn+1, rd, rt, (op >> 6) & 0x1F); break;
    /* op rs */           case 'w': sprintf(out, "%-9s %s", opn+1, rs); break;
    /* op rd */           case 'c': sprintf(out, "%-9s %s", opn+1, rd); break;
    /* op */              case 'z': sprintf(out, "%-9s", opn+1); break;
    /* op fd, fs, ft */   case 'f': sprintf(out, "%-9s %s, %s, %s", opn+1, rd, rs, rt); break;
    /* op rt, fs */       case 'g': sprintf(out, "%-9s %s, %s", opn+1, rt, __mips_fpreg[(op >> 11) & 0x1F]); break;
    /* op rs, rt */       case 'h': sprintf(out, "%-9s %s, %s", opn+1, rs, rt); break;
    /* op code20 */       case 'a': sprintf(out, "%-9s 0x%lx", opn+1, (op>>6) & 0xFFFFF); break;
    /* op rs, rt, code */ case 't': sprintf(out, "%-9s %s, %s, 0x%lx", opn+1, rs, rt, (op>>6) & 0x3FF); break;
                          default:  sprintf(out, "%-9s", opn+1); break;
    }
}

char *cop1_insn_disasm(InsnData *pc) {
    c1_disasm((u32 *)pc, insn_as_string);

    return insn_as_string;
}

char *branch_insn_disasm(InsnData insn) {
    static char *insn_names[] = {
        [0b00001] = "bgez",
        [0b00011] = "bgezl",
        [0b10001] = "bgezal",
        [0b10011] = "bgezall",
        [0b00000] = "bltz",
        [0b00010] = "bltzl",
        [0b10000] = "bltzal",
        [0b10010] = "bltzall",
    };
    char *strp = &insn_as_string[0];
    char *rs = __mips_gpr[insn.b.rs];
    u16 offset = insn.b.offset;

    for (int i = 0; i < ARRAY_COUNT(insn_as_string); i++) insn_as_string[i] = 0;

    sprintf(strp, "%-9s %s %04X", insn_names[insn.b.sub], rs, offset);

    return insn_as_string;
}

char *insn_disasm(InsnData *addr) {
    InsnData insn = *addr;
    char *strp = &insn_as_string[0];
    int successful_print = 0;
    u32 target;

    if (insn.d == 0) { // trivial case
        return "nop";
    }

    if (insn.i.opcode == OP_BRANCH) {
        return branch_insn_disasm(insn);
    }
    if (insn.i.opcode == OP_COP0) {
        return "cop0 (UNIMPL)";
    }
    if (insn.i.opcode == OP_COP1) {
        return cop1_insn_disasm(addr);
    }

    for (int i = 0; i < ARRAY_COUNT(insn_as_string); i++) insn_as_string[i] = 0;

    for (int i = 0; i < ARRAY_COUNT(insn_db); i++) {
        if (insn.i.opcode != 0 && insn.i.opcode == insn_db[i].opcode) {
            switch (insn_db[i].arbitraryParam) {
                case PARAM_SWAP_RS_IMM:
                    strp += sprintf(strp, "%-9s %s %s  %04X", insn_db[i].name,
                                               __mips_gpr[insn.i.rt],
                                               __mips_gpr[insn.i.rs],
                                               insn.i.immediate
                    ); break;
                case PARAM_LUI:
                    strp += sprintf(strp, "%-9s %s %04X", insn_db[i].name,
                                               __mips_gpr[insn.i.rt],
                                               insn.i.immediate
                    ); break;
                    break;
                case PARAM_JAL:
                    target = 0x80000000 | ((insn.d & 0x1FFFFFF) * 4);
#ifdef DEBUG_EXPORT_SYMBOLS
                        strp += sprintf(strp, "%-9s %s(%08X)", insn_db[i].name,
                                                         parse_map(target, FALSE), target
                        );
#else
                        strp += sprintf(strp, "%-9s %08X", insn_db[i].name,
                                                           target
                        );
#endif // DEBUG_EXPORT_SYMBOLS
                    break;
                case PARAM_JUMP:
                    target = 0x80000000 | (insn.d & 0x03FFFFFF);
                    strp += sprintf(strp, "%-9s %08X", insn_db[i].name,
                                                       target
                    );
                    break;
                case PARAM_FLOAT_RT:
                    strp += sprintf(strp, "%-9s %s, %04X (%s)", insn_db[i].name,
                                               __mips_fpreg[insn.i.rt],
                                               insn.i.immediate,
                                               __mips_gpr[insn.i.rs]
                    ); break;
                case PARAM_NONE:
                    strp += sprintf(strp, "%-9s %s %04X (%s)", insn_db[i].name,
                                                   __mips_gpr[insn.i.rt],
                                                   insn.i.immediate,
                                                   __mips_gpr[insn.i.rs]
                    ); break;

            }
            successful_print = 1;
            break;
        } else if (   (insn.i.rdata.function == 0 && insn.i.opcode == 0) // specifically catch `sll`
                   || (insn.i.rdata.function != 0 && insn.i.rdata.function == insn_db[i].function)
        ) {
            switch (insn_db[i].arbitraryParam) {
                case PARAM_BITSHIFT:
                    strp += sprintf(strp, "%-9s %s %s %04X", insn_db[i].name,
                                                       __mips_gpr[insn.i.rdata.rd],
                                                       __mips_gpr[insn.i.rt],
                                                       insn.i.rdata.shift_amt
                        );
                    break;
                case PARAM_SWAP_RS_RT:
                    strp += sprintf(strp, "%-9s %s %s %s", insn_db[i].name,
                                                       __mips_gpr[insn.i.rdata.rd],
                                                       __mips_gpr[insn.i.rt],
                                                       __mips_gpr[insn.i.rs]
                        );
                    break;
                case PARAM_MULT_MOVE:
                    strp += sprintf(strp, "%-9s %s", insn_db[i].name,
                                                       __mips_gpr[insn.i.rdata.rd]
                        );
                    break;
                case PARAM_EMUX:
                    target = (insn.d >> 6) & 0x3FF;
                    if (insn.i.rs == insn.i.rt) {
                        strp += sprintf(strp, "%-9s %s 0x%02X", "emux",
                                                       __mips_gpr[insn.i.rs],
                                                       target
                        );
                    } else {
                        strp += sprintf(strp, "%-9s %s %s", insn_db[i].name,
                                                       __mips_gpr[insn.i.rs],
                                                       __mips_gpr[insn.i.rt]
                        );
                    }
                    break;
                case PARAM_TRAP:
                    strp += sprintf(strp, "%-9s %s %s", insn_db[i].name,
                                                       __mips_gpr[insn.i.rs],
                                                       __mips_gpr[insn.i.rt]
                    );
                    break;
                case PARAM_SYSCALL:
                    strp += sprintf(strp, "%-9s %d", insn_db[i].name,
                                                    (insn.d & 0x03FFFFC0) >> 6
                    );
                    break;
                case PARAM_NONE:
                    strp += sprintf(strp, "%-9s %s %s %s", insn_db[i].name,
                                                       __mips_gpr[insn.i.rdata.rd],
                                                       __mips_gpr[insn.i.rs],
                                                       __mips_gpr[insn.i.rt]
                        );
                    break;

            }
            successful_print = 1;
            break;
        }
    }
    if (successful_print == 0) {
        strp += sprintf(strp, "unimpl %08X", insn.d);
    }

    return insn_as_string;
}
