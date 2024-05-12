#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

#include "crash_screen/cs_descriptions.h"
#include "crash_screen/cs_draw.h"
#include "crash_screen/cs_main.h"
#include "crash_screen/cs_settings.h"
#include "crash_screen/cs_pages.h"
#include "crash_screen/cs_print.h"
#include "map_parser.h"
#include "registers.h"

#include "insn_disasm.h"

#include "crash_screen/pages/page_disasm.h"

#include "engine/math_util.h"

#include "insn_db.inc.c"


// All chars that can appear in an instruction name, packed by '_PL()'.
static const char insn_alphabet_uppercase[] = (INSN_ALPHABET_STR_0 INSN_ALPHABET_STR_N INSN_ALPHABET_STR_A); // "\0.0123ABCDEFGHIJKLMNOPQRSTUVWXYZ";
static const char insn_alphabet_lowercase[] = (INSN_ALPHABET_STR_0 INSN_ALPHABET_STR_N INSN_ALPHABET_STR_a); // "\0.0123abcdefghijklmnopqrstuvwxyz";


#define PARAM(_id,_fmt) { \
    .id  = _id,  \
    .fmt = _fmt, \
}

#define MIPS_NUM_PARAMS 3
#define PARAM_NOP PARAM(MP_NOP, MP_FMT_I)


const InsnParam insn_param_formats[][MIPS_NUM_PARAMS] = {
    [IFMT_NOP  ] = { PARAM_NOP,                 PARAM_NOP,                 PARAM_NOP,                 }, // nop

    [IFMT_s    ] = { PARAM(MP_RS,    MP_FMT_I), PARAM_NOP,                 PARAM_NOP,                 }, // rs
    [IFMT_d    ] = { PARAM(MP_RD,    MP_FMT_I), PARAM_NOP,                 PARAM_NOP,                 }, // rd
    [IFMT_ds   ] = { PARAM(MP_RD,    MP_FMT_I), PARAM(MP_RS,    MP_FMT_I), PARAM_NOP,                 }, // rd rs
    [IFMT_st   ] = { PARAM(MP_RS,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM_NOP,                 }, // rs rt
    [IFMT_dt   ] = { PARAM(MP_RD,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM_NOP,                 }, // rd rt // for pseudos
    [IFMT_dst  ] = { PARAM(MP_RD,    MP_FMT_I), PARAM(MP_RS,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), }, // rd rs rt
    [IFMT_dts  ] = { PARAM(MP_RD,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM(MP_RS,    MP_FMT_I), }, // rd rt rs

    [IFMT_tdCP0] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_RDCP0, MP_FMT_I), PARAM_NOP,                 }, // rt rd[cp0]
    [IFMT_tSCP1] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_FS,    MP_FMT_F), PARAM_NOP,                 }, // rt fs
    [IFMT_tSFCR] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_FSFCR, MP_FMT_I), PARAM_NOP,                 }, // rt fs

    [IFMT_ST   ] = { PARAM(MP_FS,    MP_FMT_F), PARAM(MP_FT,    MP_FMT_F), PARAM_NOP,                 }, // fs ft
    [IFMT_DST  ] = { PARAM(MP_FD,    MP_FMT_F), PARAM(MP_FS,    MP_FMT_F), PARAM(MP_FT,    MP_FMT_F), }, // fd fs ft

    [IFMT_DS_XX] = { PARAM(MP_FD,    MP_FMT_O), PARAM(MP_FS,    MP_FMT_O), PARAM_NOP,                 }, // op.sd fd fs
    [IFMT_DS_IX] = { PARAM(MP_FD,    MP_FMT_I), PARAM(MP_FS,    MP_FMT_O), PARAM_NOP,                 }, // op.i[.f]
    [IFMT_DS_FX] = { PARAM(MP_FD,    MP_FMT_F), PARAM(MP_FS,    MP_FMT_O), PARAM_NOP,                 }, // op.f[.i]

    [IFMT_tsI  ] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_RS,    MP_FMT_I), PARAM(MP_IMM,   MP_FMT_I), }, // rt rs immediate
    [IFMT_tI   ] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_IMM,   MP_FMT_I), PARAM_NOP,                 }, // rt immediate
    [IFMT_sI   ] = { PARAM(MP_RS,    MP_FMT_I), PARAM(MP_IMM,   MP_FMT_I), PARAM_NOP,                 }, // rs immediate

    [IFMT_to   ] = { PARAM(MP_RT,    MP_FMT_I), PARAM(MP_OFF,   MP_FMT_I), PARAM_NOP,                 }, // rt offset(base)
    [IFMT_To   ] = { PARAM(MP_FT,    MP_FMT_F), PARAM(MP_OFF,   MP_FMT_I), PARAM_NOP,                 }, // ft offset(base)

    [IFMT_dta  ] = { PARAM(MP_RD,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM(MP_SHIFT, MP_FMT_I), }, // rd rt shift
    [IFMT_ste  ] = { PARAM(MP_RS,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM(MP_EXCA,  MP_FMT_I), }, // rs rt exc10
    [IFMT_E    ] = { PARAM(MP_EXCAB, MP_FMT_I), PARAM_NOP,                 PARAM_NOP,                 }, // exc20

    [IFMT_stB  ] = { PARAM(MP_RS,    MP_FMT_I), PARAM(MP_RT,    MP_FMT_I), PARAM(MP_B,     MP_FMT_I), }, // rs rt branch
    [IFMT_sB   ] = { PARAM(MP_RS,    MP_FMT_I), PARAM(MP_B,     MP_FMT_I), PARAM_NOP,                 }, // rs branch
    [IFMT_B    ] = { PARAM(MP_B,     MP_FMT_I), PARAM_NOP,                 PARAM_NOP,                 }, // branch

    [IFMT_J    ] = { PARAM(MP_JUMP,  MP_FMT_I), PARAM_NOP,                 PARAM_NOP,                 }, // jump
};
#define SIZEOF_PARAM_LISTS sizeof(insn_param_formats)


/**
 * @brief Use a specific pseudo-instruction from insn_db_pseudo if 'cond' is TRUE.
 *
 * @param[out] checkInsn A pointer to the InsnTemplate data in insn_db_pseudo matching the given instruction data.
 * @param[in ] id        The index in insn_db_pseudo of the pseudo-instruction template data.
 * @param[in ] cond      Whether to convert the instruction to the pseudo-instruction.
 * @return _Bool Whether the instruction has been converted.
 */
static _Bool check_pseudo_insn(const InsnTemplate** checkInsn, enum PseudoInsns id, _Bool cond) {
    if (cond) {
        *checkInsn = &insn_db_pseudo[id];
        return TRUE;
    }

    return FALSE;
}

/**
 * @brief Checks for pseudo-instructions.
 *
 * @param[out] checkInsn A pointer to the InsnTemplate data in insn_db_pseudo matching the given instruction data.
 * @param[in ] insnSrc   The instruction data that is being read.
 * @return _Bool Whether the instruction has been converted.
 */
static _Bool check_pseudo_instructions(const InsnTemplate** type, InsnData* insnSrc) {
    InsnData insn = *insnSrc;
    // NOP (trivial case).
    if (check_pseudo_insn(type, PSEUDO_NOP, (insn.raw == 0))) return TRUE;

    // There are no known one-line pseudo-instructions in the Coprocessor lists.
    if (insn.cop_opcode == COP_OPCODE) {
        return FALSE;
    }

    switch (insn.opcode) {
        case OPC_SPECIAL:
            switch (insn.func) {
                case OPS_ADD:
                    if (check_pseudo_insn(type, PSEUDO_MOVES, (insn.rt == REG_CPU_R0))) return TRUE;
                    if (check_pseudo_insn(type, PSEUDO_MOVET, (insn.rs == REG_CPU_R0))) return TRUE;
                    break;
                case OPS_OR:
                    if (check_pseudo_insn(type, PSEUDO_MOVES, (insn.rt == REG_CPU_R0))) return TRUE;
            }
            break;
        case OPC_BEQ:
            if (check_pseudo_insn(type, PSEUDO_B,     (insn.rs == insn.rt))) return TRUE;
            if (check_pseudo_insn(type, PSEUDO_BEQZ,  (insn.rt == REG_CPU_R0))) return TRUE;
            break;
        case OPC_BNE:
            if (check_pseudo_insn(type, PSEUDO_BNEZ,  (insn.rt == REG_CPU_R0))) return TRUE;
            break;
        case OPC_ADDI:
            if (check_pseudo_insn(type, PSEUDO_LI,    (insn.rs == REG_CPU_R0))) return TRUE;
            if (check_pseudo_insn(type, PSEUDO_SUBI,  ((s16)insn.immediate < 0))) { insnSrc->immediate = -(s16)insnSrc->immediate; return TRUE; }
            break;
        case OPC_ADDIU:
            if (check_pseudo_insn(type, PSEUDO_LI,    (insn.rs == REG_CPU_R0))) return TRUE;
            break;
        case OPC_BEQL:
            if (check_pseudo_insn(type, PSEUDO_BEQZL, (insn.rt == REG_CPU_R0))) return TRUE;
            break;
        case OPC_BNEL:
            if (check_pseudo_insn(type, PSEUDO_BNEZL, (insn.rt == REG_CPU_R0))) return TRUE;
            break;
        case OPC_DADDI:
            if (check_pseudo_insn(type, PSEUDO_DSUBI, ((s16)insn.immediate < 0))) { insnSrc->immediate = -(s16)insnSrc->immediate; return TRUE; }
            break;
    }

    return FALSE;
}

/**
 * @brief Gets a pointer to the InsnTemplate data matching the given instruction data.
 *
 * @param[in] insnSrc The instruction data that is being read.
 * @return const InsnTemplate* A pointer to the InsnTemplate data matching the given instruction data.
 */
const InsnTemplate* get_insn(InsnData* insnSrc) {
    InsnData insn = *insnSrc;
    const InsnTemplate* checkInsn = NULL;
    u8 opcode = insn.opcode; // First 6 bits.

    // Check for pseudo-instructions.
    if (
        cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_PSEUDOINSNS) &&
        check_pseudo_instructions(&checkInsn, insnSrc)
    ) {
        return checkInsn;
    }

    // Get the instruction list and opcode to compare.
    switch (insn.opcode) {
        case OPC_COP0:
        case OPC_COP1:
        case OPC_COP2:
        case OPC_COP3:
            checkInsn = insn_db_cop_lists[insn.cop_num][insn.cop_subtype]; // Use COPz lists.
            if (checkInsn == NULL) {
                return NULL;
            }
            switch (insn.cop_subtype) {
                case INSN_TYPE_COP_FMT: opcode = insn.fmt;    break; // The 3 bits after the first 8.
                case INSN_TYPE_REGIMM:  opcode = insn.regimm; break; // The 5 bits after the first 11.
                case INSN_TYPE_FUNC:    opcode = insn.func;   break; // Last 6 bits.
                default:                                      break; // 0b11 subtype is unknown and the lists in insn_db_cop_lists are NULL.
            }
            break;
        case OPC_SPECIAL:
            checkInsn = insn_db_spec;
            opcode    = insn.func;
            break;
        case OPC_REGIMM:
            checkInsn = insn_db_regi;
            opcode    = insn.regimm;
            break;
        default:
            checkInsn = insn_db_standard;
            opcode    = insn.opcode;
            break;
    }

    // Loop through the given list.
    while (checkInsn->raw != 0) {
        if (checkInsn->opcode == opcode) {
            return checkInsn;
        }

        checkInsn++;
    }

    return NULL;
}

/**
 * @brief Checks an instruction for its branch offset.
 *
 * @param[in] insn The instruction data that is being read.
 * @return s16 The branch offset. 0 if there is no branch.
 */
s16 insn_check_for_branch_offset(InsnData insn) {
    const InsnTemplate* info = get_insn(&insn);

    if (info != NULL) {
        MIPSParamFmts fmt = info->params;
        const InsnParam* list = &insn_param_formats[fmt][0];

        for (size_t i = 0; i < MIPS_NUM_PARAMS; i++) {
            if (list[i].id == MP_B) {
                return insn.offset;
            }
        }
    }

    return 0x0000;
}

/**
 * @brief Gets the target address of the instruction at 'addr'.
 *
 * @param[in] addr The address of the instruction that is being read.
 * @return Address The target address of the instruction. 'addr' if there is no branch instruction.
 */
Address get_insn_branch_target_from_addr(Address addr) {
    if (!addr_is_in_text_segment(addr)) {
        return addr;
    }

    InsnData insn = {
        .raw = *(Word*)addr, //! TODO: safe read here?
    };

    // Jump instructions are handled differently than branch instructions.
    if (
        (insn.opcode == OPC_J  ) ||
        (insn.opcode == OPC_JAL)
    ) {
        return PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(InsnData));
    }

    s16 branchOffset = insn_check_for_branch_offset(insn);

    if (branchOffset) {
        return (addr + (((s16)branchOffset + 1) * sizeof(InsnData)));
    }

    return addr;
}

/**
 * @brief Gets the corresponding character to print for a Coprocessor-1 (Floating-Point Unit) instruction's format.
 *
 * @param[in] insn The instruction data that is being read.
 * @return char The char value that represents the format in the instruction name.
 */
static char cp1_fmt_to_char(InsnData insn) {
    switch (insn.fmt) {
        case COP1_FMT_SINGLE: return 'S'; // Single
        case COP1_FMT_DOUBLE: return 'D'; // Double
        case COP1_FMT_WORD:   return 'W'; // Word
        case COP1_FMT_LONG:   return 'L'; // Long
    }

    return 'X'; // Unknown
}

/**
 * @brief Prints the new color code if the color has changed.
 *
 * @param[out   ] strp     Pointer to the string.
 * @param[in,out] oldColor The previous color. Gets set to 'newColor' if they are different.
 * @param[in    ] newColor The new color to add to the string.
 */
static void cs_insn_param_check_color_change(char** strp, RGBA32* oldColor, RGBA32 newColor, _Bool formatting) {
    if (formatting && (*oldColor != newColor)) {
        *oldColor = newColor;
        *strp += sprintf(*strp, STR_COLOR_PREFIX, newColor);
    }
}

// A buffer to save registers to. Used by cs_insn_to_string().
RegisterId gSavedRegBuf[REG_BUFFER_SIZE];
int gSavedRegBufSize = 0;

/**
 * @brief Clears gSavedRegBuf.
 */
void clear_saved_reg_buffer(void) {
    bzero(gSavedRegBuf, sizeof(gSavedRegBuf));
    gSavedRegBufSize = 0;
}

/**
 * @brief Appends a register to gSavedRegBuf.
 *
 * @param[in] src      Register source.
 * @param[in] idx      Register index in source.
 * @param[in] type     Register value type.
 * @param[in] isOutput Is output of current instruction (don't print value).
 */
void append_reg_to_buffer(RegisterSources src, int idx, RegisterValueTypes type, _Bool isOutput) {
    if (gSavedRegBufSize < ARRAY_COUNT(gSavedRegBuf)) {
        gSavedRegBuf[gSavedRegBufSize++] = (RegisterId){
            .src = src,
            .idx = idx,
            .valInfo = {
                .type = type,
                .thr  = TRUE,
                .dbl  = FALSE, //! TODO: implement this.
                .out  = isOutput,
            },
        };
    }
}

/**
 * @brief Appends extra registers to gSavedRegBuf for certain instructions.
 * TODO: Can this be optimized for size? Something like this should not be 0x280 bytes.
 *
 * @param[in] ex1 1st extra register slot.
 * @param[in] ex2 2nd extra register slot.
 * @param[in] exo Output mask (don't print value).
 */
void append_extra_regs(u8 ex1, u8 ex2, u8 exo) {
    u8 ex[2] = { ex1, ex2, };
    for (int i = 0; i < ARRAY_COUNT(ex); i++) {
        _Bool isOutput = (exo & (BIT((ARRAY_COUNT(ex) - 1) - i)));
        switch (ex[i]) {
            case EXR_HI:
                append_reg_to_buffer(REGS_SPC, REG_SPC_HI, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_LO:
                append_reg_to_buffer(REGS_SPC, REG_SPC_LO, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_LL:
                append_reg_to_buffer(REGS_SPC, REG_SPC_LLBIT, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_EN:
                append_reg_to_buffer(REGS_CP0, REG_CP0_ENTRYHI,  REG_VAL_TYPE_INT, isOutput);
                append_reg_to_buffer(REGS_CP0, REG_CP0_ENTRYLO0, REG_VAL_TYPE_INT, isOutput);
                append_reg_to_buffer(REGS_CP0, REG_CP0_ENTRYLO1, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_IX:
                append_reg_to_buffer(REGS_CP0, REG_CP0_INX, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_RD:
                append_reg_to_buffer(REGS_CP0, REG_CP0_RAND, REG_VAL_TYPE_INT, isOutput);
                break;
            case EXR_FP:
                append_reg_to_buffer(REGS_FCR, REG_FCR_CONTROL_STATUS, REG_VAL_TYPE_CONDBIT, isOutput);
                break;
            default:
                break;
        }
    }
}

static Address instr_index_to_addr(InsnData insn) {
    return PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(InsnData));
}

// "break" instruction codes generated by gcc:
//! TODO: Are these names accurate, and are there more of these?
static const IdNamePair insn_break_codes[] = {
    { .id = 6, .name = "overflow", },
    { .id = 7, .name = "div0",     },
    ID_LIST_END(),
};


static char insn_as_string[CHAR_BUFFER_SIZE] = "";


#define STR_INSN_NAME_BASE      "%s"
#define STR_FORMAT              "%c"
#define STR_CONDITION           "%s"

#define STR_INSN_NAME           "%-"EXPAND_AND_STRINGIFY(INSN_NAME_DISPLAY_WIDTH)"s"
#define STR_INSN_NAME_FORMAT    STR_INSN_NAME_BASE"."STR_FORMAT

#define STR_IREG                "%s"                            // Register.
#define STR_IMMEDIATE           STR_HEX_PREFIX STR_HEX_HALFWORD // 0xI.
#define STR_OFFSET              "%c"STR_IMMEDIATE               // ±Offset.
#define STR_FUNCTION            STR_HEX_PREFIX STR_HEX_WORD     // Function address.
#define STR_IREG_BASE           "("STR_IREG")"                  // Base register.
#define STR_FREG                "F%02d"                         // Float Register.
#define STR_CODE10              STR_HEX_PREFIX "%03X"           // 10-bit data for exception handler.
#define STR_CODE20              STR_HEX_PREFIX "%05X"           // 20-bit data for exception handler.


#define ADD_COLOR(_c) {                                                 \
    cs_insn_param_check_color_change(&strp, &color, (_c), formatting);  \
}
#define ADD_STR(...) {                  \
    strp += sprintf(strp, __VA_ARGS__); \
}
#define ADD_REG(_src, _idx) {                                                                                       \
    regInfo = get_reg_info((_src), (_idx));                                                                         \
    ADD_STR(STR_IREG, regInfo->name);                                                                               \
    append_reg_to_buffer((_src), (_idx), fmt, (cmdIndex == outputIndex)); \
}
#define ADD_ADDR_REG(_src, _idx) {                                  \
    regInfo = get_reg_info((_src), (_idx));                         \
    ADD_STR(STR_IREG_BASE, regInfo->name);                          \
    append_reg_to_buffer((_src), (_idx), REG_VAL_TYPE_ADDR, FALSE); \
}

/**
 * @brief Converts MIPS instruction data into a formatted string.
 * TODO: This is by far the largest crash screen function. Can it be reduced?
 *
 * @param[in ] addr  The address of the instruction. Used to calculate the target of branches and jumps.
 * @param[in ] insn  The instruction data that is being read.
 * @param[out] fname If the instruction points to a function, this is set to the function's name string in the map data, otherwise NULL.
 * @return char* The formatted string in insn_as_string.
 */
char* cs_insn_to_string(Address addr, InsnData insn, const char** fname, _Bool formatting) {
    char* strp = &insn_as_string[0]; // Pointer to a location inside the string.
    _Bool unimpl = FALSE;

    bzero(insn_as_string, sizeof(insn_as_string));

    // Force include the function with all instructions.
    if (gMipsIIITest == 0) {
        return NULL;
    }

    const InsnTemplate* info = get_insn(&insn);

    if (info != NULL) {
        const InsnParam* curCmd = &insn_param_formats[info->params][0];
        u8 outputIndex = (info->output - 1);
        const RegisterInfo* regInfo = NULL;
        RGBA32 color = COLOR_RGBA32_NONE;
        _Bool separator = FALSE;
        RegisterValueTypes overwriteFmt = REG_VAL_TYPE_FLOAT;

        clear_saved_reg_buffer();

        ADD_COLOR((insn.raw == OPS_NOP) ? COLOR_RGBA32_CRASH_DISASM_NOP : COLOR_RGBA32_CRASH_DISASM_INSN);
        char name[INSN_NAME_DISPLAY_WIDTH] = UNPACK_STR7(insn_alphabet_uppercase, info->name);
        if (info->hasFmt) {
            char fmtChar = cp1_fmt_to_char(insn);
            sprintf(name, STR_INSN_NAME_FORMAT, name, fmtChar);
            overwriteFmt = ((fmtChar == 'S' || fmtChar == 'D') ? REG_VAL_TYPE_FLOAT : REG_VAL_TYPE_INT); // Overwrite secondary format.
        }
        ADD_STR(STR_INSN_NAME, name);

        // Whether to print immediates as decimal values rather than hexadecimal.
        _Bool decImmediates = (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_IMM_FMT) == PRINT_NUM_FMT_DEC);

        // Loop through the chars in the 'insn_param_formats' entry and print the insn data accordingly.
        for (size_t cmdIndex = 0; cmdIndex < MIPS_NUM_PARAMS; cmdIndex++) {
            const MIPSParamID cmd = curCmd->id;
            if (unimpl || (cmd == MP_NOP)) {
                break;
            }

            if (separator) {
                separator = FALSE;
                ADD_STR(", ");
            }

            const RegisterValueTypes fmt = ((curCmd->fmt == MP_FMT_O) ? overwriteFmt : curCmd->fmt);

            switch (cmd) {
                case MP_RS: // CPU 'RS' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rs);
                    separator = TRUE;
                    break;
                case MP_RT: // CPU 'RT' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rt);
                    separator = TRUE;
                    break;
                case MP_RD: // CPU 'RD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rd);
                    separator = TRUE;
                    break;
                case MP_IMM: // Immediate.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_IMMEDIATE);
                    ADD_STR((decImmediates ? "%d" : STR_IMMEDIATE), insn.immediate);
                    break;
                case MP_SHIFT: // Shift amount.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_IMMEDIATE);
                    ADD_STR(STR_IMMEDIATE, insn.sa);
                    break;
                case MP_OFF: // Register base + offset.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_REGOFFSET);
                    ADD_STR(STR_IMMEDIATE, insn.immediate);
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_ADDR_REG(REGS_CPU, insn.base);
                    break;
                case MP_B: // Branch offset.
                    ADD_COLOR(COLOR_RGBA32_CRASH_OFFSET);
                    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_OFFSET_ADDR)) {
                        ADD_STR(STR_FUNCTION, get_insn_branch_target_from_addr(addr));
                    } else {
                        s16 branchOffset = (insn.offset + 1);
                        //! TODO: Is it possible to use the '%+' format specifier with the 0x prefix?
                        ADD_STR(STR_OFFSET, ((branchOffset < 0x0000) ? '-' : '+'), abss(branchOffset));
                    }
                    break;
                case MP_RDCP0: // COP0 'RD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP0, insn.rd);
                    separator = TRUE;
                    break;
                case MP_FSFCR: // FCR 'FS' register;
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_FCR, insn.fs);
                    separator = TRUE;
                    break;
                case MP_FT: // COP1 'FT' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.ft);
                    separator = TRUE;
                    break;
                case MP_FS: // COP1 'FS' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.fs);
                    separator = TRUE;
                    break;
                case MP_FD: // COP1 'FD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.fd);
                    separator = TRUE;
                    break;
                case MP_EXCA: // For TRAP IF instructions.
                    ADD_COLOR(COLOR_RGBA32_LIGHT_GRAY);
                    ADD_STR(STR_CODE10, insn.codeB);
                    break;
                case MP_EXCAB: // For SYSCALL and BREAK instructions.
                    ADD_COLOR(COLOR_RGBA32_LIGHT_GRAY);
                    if (info->opcode == OPS_BREAK) {
                        u16 eA = insn.codeA;
                        u16 eB = insn.codeB;
                        ADD_STR(STR_CODE10", "STR_CODE10, eA, eB);
                        if (eB == 0) {
                            const char* desc = get_name_from_null_terminated_id_list(eA, insn_break_codes);
                            if (desc != NULL) {
                                ADD_COLOR(COLOR_RGBA32_VSC_COMMENT);
                                ADD_STR(" # %s", desc);
                            }
                        }
                    } else { // syscall
                        ADD_STR(STR_CODE20, insn.codeAB);
                    }
                    break;
                case MP_JUMP: // Jump function.
                    ADD_COLOR(COLOR_RGBA32_CRASH_FUNCTION_NAME);
                    Address target = instr_index_to_addr(insn);
                    if (IS_DEBUG_MAP_ENABLED() && cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES) && addr_is_in_text_segment(target)) {
                        const MapSymbol* symbol = get_map_symbol(target, SYMBOL_SEARCH_BACKWARD);

                        if (symbol != NULL) {
                            *fname = get_map_symbol_name(symbol);
                            // Only print as the function name if it's the exact starting address of the function.
                            if (target != symbol->addr) {
                                *fname = NULL;
                            }

                            if (*fname != NULL) {
                                break;
                            }
                        }
                    } else {
                        *fname = NULL;
                    }
                    ADD_STR(STR_FUNCTION, target);
                    break;
                default: // Unknown parameter.
                    unimpl = TRUE;
                    ADD_STR("{%c}", cmd);
                    break;
            }

            curCmd++;
        }

        append_extra_regs(info->ex1, info->ex2, info->exo);
    } else {
        unimpl = TRUE;
    }

    if (unimpl) {
        //! TODO: binary mode for these.
        ADD_STR(" (unimpl)", insn.raw);
    }

    return insn_as_string;
}

//! TODO: Use this for regular disasm too?
typedef enum PACKED PSC_Colors {
    PSC_COL_0,
    PSC_COL_VAR,
    PSC_COL_IMM,
    PSC_COL_OFF,
    PSC_COL_FUNC,
    PSC_COL_DEF,
    PSC_COL_FLOW,
    PSC_COL_COMM,
    NUM_PSC_COLORS,
} PSC_Colors;

const RGBA32 sPSCColors[NUM_PSC_COLORS] = {
    [PSC_COL_VAR ] = COLOR_RGBA32_CRASH_VARIABLE,
    [PSC_COL_IMM ] = COLOR_RGBA32_CRASH_DISASM_IMMEDIATE,
    [PSC_COL_OFF ] = COLOR_RGBA32_CRASH_OFFSET,
    [PSC_COL_FUNC] = COLOR_RGBA32_CRASH_FUNCTION_NAME,
    [PSC_COL_DEF ] = COLOR_RGBA32_VSC_DEFINE,
    [PSC_COL_FLOW] = COLOR_RGBA32_VSC_CONTROL,
    [PSC_COL_COMM] = COLOR_RGBA32_VSC_COMMENT,
};

typedef struct PSC_Entry {
    const char c; // Char
    const PSC_Colors col; // Color
    const u8 pad[2];
    union {
        const char a[4];
        const char* s;
    };
} PSC_Entry;

#define SIZEOF_PSC_ENTRY sizeof(PSC_Entry)

const PSC_Entry psc_entries[] = {
    // Insn name as func:
#define PSC_INSN    '\\'
    { .c = PSC_INSN,   .col = PSC_COL_OFF,  .a = "%s", },

    // Flow control:
#define PSC_GOTO    'g'
    { .c = PSC_GOTO,   .col = PSC_COL_FLOW, .s = "goto ", },
#define PSC_IF      '?'
    { .c = PSC_IF,     .col = PSC_COL_FLOW, .a = "if ", },
#define PSC_RETURN  'r'
    { .c = PSC_RETURN, .col = PSC_COL_FLOW, .s = "return", },

    // Pseudo-defines:
#define PSC_ALIGNL  '['
    { .c = PSC_ALIGNL, .col = PSC_COL_DEF,  .s = "ALIGNL", },
#define PSC_ALIGNR  ']'
    { .c = PSC_ALIGNR, .col = PSC_COL_DEF,  .s = "ALIGNR", },

    // Functions:
#define PSC_ORD     'V'
    { .c = PSC_ORD,    .col = PSC_COL_OFF,  .a = "ord",    }, // ordered
#define PSC_SIG     'G'
    { .c = PSC_SIG,    .col = PSC_COL_OFF,  .a = "sig",    }, // signalling
#define PSC_TRAP    'X'
    { .c = PSC_TRAP,   .col = PSC_COL_OFF,  .s = "trap",   },

    // Non-InsnData registers:
#define PSC_FP      '#'
    { .c = PSC_FP,     .col = PSC_COL_VAR,  .a = "FP", }, // FP condition
#define PSC_RA      '$'
    { .c = PSC_RA,     .col = PSC_COL_VAR,  .a = "RA", },
#define PSC_PC      '.'
    { .c = PSC_PC,     .col = PSC_COL_VAR,  .a = "PC", },
#define PSC_HI      'H'
    { .c = PSC_HI,     .col = PSC_COL_VAR,  .a = "HI", },
#define PSC_LO      'L'
    { .c = PSC_LO,     .col = PSC_COL_VAR,  .a = "LO", },
#define PSC_LL      'l'
    { .c = PSC_LL,     .col = PSC_COL_VAR,  .a = "LL", }, // LLBit

    // InsnData registers:
#define PSC_RS      's'
    { .c = PSC_RS,     .col = PSC_COL_VAR,  .a = "%s", }, // rs
#define PSC_RT      't'
    { .c = PSC_RT,     .col = PSC_COL_VAR,  .a = "%s", }, // rt
#define PSC_RD      'd'
    { .c = PSC_RD,     .col = PSC_COL_VAR,  .a = "%s", }, // rd
#define PSC_RD0     'c'
    { .c = PSC_RD0,    .col = PSC_COL_VAR,  .a = "%s", }, // rd(cp0)
#define PSC_FT      'T'
    { .c = PSC_FT,     .col = PSC_COL_VAR,  .a = "%s", }, // ft
#define PSC_FS      'S'
    { .c = PSC_FS,     .col = PSC_COL_VAR,  .a = "%s", }, // fs
#define PSC_FD      'D'
    { .c = PSC_FD,     .col = PSC_COL_VAR,  .a = "%s", }, // fd
#define PSC_FSC     'F'
    { .c = PSC_FSC,    .col = PSC_COL_VAR,  .a = "%s", }, // fs(fcr)
#define PSC_BASE    'o'
    { .c = PSC_BASE,   .col = PSC_COL_VAR,  .a = "%s", }, // address base (+ offset)
#define PSC_IMM     'i'
    { .c = PSC_IMM,    .col = PSC_COL_IMM,  .s = "0x%X", }, // immediate
#define PSC_SHIFT   'a'
    { .c = PSC_SHIFT,  .col = PSC_COL_IMM,  .a = "%d", }, // shift amount
#define PSC_SH32    'A'
    { .c = PSC_SH32,   .col = PSC_COL_IMM,  .a = "%d", }, // shift amount + 32
#define PSC_BRANCH  'b'
    { .c = PSC_BRANCH, .col = PSC_COL_OFF,  .s = "%c0x%X", }, // branch offset
#define PSC_JUMP    'J'
    { .c = PSC_JUMP,   .col = PSC_COL_FUNC, .s = "0x%08X()", }, // jump address
#define PSC_EXC10   'e'
    { .c = PSC_EXC10,  .col = PSC_COL_IMM,  .s = "0x%X", }, // EXC10
#define PSC_EXC20   'E'
    { .c = PSC_EXC20,  .col = PSC_COL_IMM,  .s = "0x%X", }, // EXC20
};

//! TODO: Clean this up:
#define CAO     "o->i" // "*(o + i)"
#define CAOL    "[(o->i)" // "*ALIGNL(o + i)"
#define CAOR    "](o->i)" // "*ALIGNR(o + i)"
#define CLINK   "$=.+2"
ALIGNED4 const char* pseudo_c_code_formats[] = {
    [PSI_NOP] = /*nop*/ "",
    // jump
    [PSI_J] = /*j*/ "J", [PSI_JAL] = /*jal*/ CLINK"J",
    [PSI_JR] = /*jr*/ "gs", [PSI_JALR] = /*jalr*/ CLINK"gs",
    // branch
    [PSI_B] = /*b*/ "gb", [PSI_BEQ] = /*beq/beql/beqz/beqzl*/ "?(s == t) gb", [PSI_BNE] = /*bne/bnel/bnez/bnezl*/ "?(s != t) gb",
    [PSI_BGTZ] = /*bgtz/bgtzl*/ "?(s > 0) gb",  [PSI_BLTZ] = /*bltz/bltzl*/ "?(s < 0) gb",
    [PSI_BLEZ] = /*blez/blezl*/ "?(s <= 0) gb", [PSI_BGEZ] = /*bgez/bgezl*/ "?(s >= 0) gb",
    [PSI_BLTZAL] = /*bltzal/bltzall*/ CLINK"?(s < 0) gb", [PSI_BGEZAL] = /*bgezal/bgezall*/ CLINK"?(s >= 0) gb",
    // arithmetic
    [PSI_ADDI] = /*addi/addiu/daddi/daddiu*/ "t = s + i", [PSI_SLTI] = /*slti/sltiu*/ "t = s < i", [PSI_ANDI] = /*andi*/ "t = s & i", [PSI_ORI] = /*ori*/ "t = s | i", [PSI_XORI] = /*xori*/ "t = s ^ i",
    [PSI_LI] = /*li*/ "t = i", [PSI_SUBI] = /*subi/dsubi*/ "t = s - i",
    [PSI_LUI] = /*lui*/ "t = i0000",
    // load/store
    [PSI_L] = /*lb/lbu/lh/lhu/lw/lwu/ld/lwc2/lwc3/ldc2*/ "t = "CAO, [PSI_L_L] = /*lwl/ldl*/ "t = "CAOL, [PSI_L_R] = /*lwr/ldr*/ "t = "CAOR,
    [PSI_S] = /*sb/sh/sw/sd/swc2/swc3/sdc2*/             CAO" = t", [PSI_S_L] = /*swl/sdl*/ CAOL" = t", [PSI_S_R] = /*swr/sdr*/ CAOR" = t",
    [PSI_LC1] = /*lwc1/ldc1*/ "T = "CAO, [PSI_SC1] = /*swc1/sdc1*/ CAO" = T",
    [PSI_LL] = /*ll/lld*/ "t = "CAO"; l = 1", [PSI_SC] = /*sc/scd*/ "?(l) { "CAO" = t; } t = l",
    [PSI_CACHE] = /*cache*/ "\\(t, "CAO")",
    // shift
    [PSI_SLI] = /*sll/dsll*/ "d = t << a", [PSI_SRI] = /*srl/sra/dsrl/dsra*/ "d = t >> a",
    [PSI_DSLI32] = /*dsll32*/ "d = t << A", [PSI_DSRI32] = /*dsrl32/dsra32*/ "d = t >> A",
    [PSI_SLV] = /*sllv/dsllv*/ "d = t << s", [PSI_SRV] = /*srlv/srav/dsrlv/dsrav*/ "d = t >> s",
    // system
    [PSI_BREAK] = /*syscall*/ "\\(E)", [PSI_FUNC] = /*sync/tlbp/tlbr/tlbwi/tlbwr/eret*/ "\\()",
    [PSI_MFHI] = /*mfhi*/ "d = H", [PSI_MTHI] = /*mthi*/ "H = s", [PSI_MFLO] = /*mflo*/ "d = L", [PSI_MTLO] = /*mtlo*/ "L = s",
    // arithmetic
    [PSI_MULT] = /*mult/multu/dmult/dmultu*/ "H,L = s * t", [PSI_DIV] = /*div/divu/ddiv/ddivu*/ "H,L = s / t",
    [PSI_ADD] = /*add/addu/dadd/daddu*/ "d = s + t", [PSI_SUB] = /*sub/subu/dsub/dsubu*/ "d = s - t",
    [PSI_AND] = /*and*/ "d = s & t", [PSI_OR] = /*or*/ "d = s | t", [PSI_XOR] = /*xor*/ "d = s ^ t", [PSI_NOR] = /*nor*/ "d = ~s & ~t",
    [PSI_SLT] = /*slt/sltu*/ "d = s < t",
    // trap
    [PSI_TGE ] = /*tge/tgeu*/   "?(s >= t) X(e)", [PSI_TLT] = /*tlt/tltu*/   "?(s < t) X(e)", [PSI_TEQ] = /*teq*/  "?(s == t) X(e)", [PSI_TNE] = /*tne*/  "?(s != t) X(e)",
    [PSI_TGEI] = /*tgei/tgeiu*/ "?(s >= i) X()", [PSI_TLTI] = /*tlti/tltiu*/ "?(s < i) X()", [PSI_TEQI] = /*teqi*/ "?(s == i) X()", [PSI_TNEI] = /*tnei*/ "?(s != i) X()",
    // etc.
    [PSI_MFC0] = /*mfc0/dmfc0*/ "t = c", [PSI_MTC0] = /*mtc0/dmtc0*/ "c = t",
    [PSI_MFC1] = /*mfc1/dmfc1*/ "t = S", [PSI_MTC1] = /*mtc1/dmtc1*/ "S = t",
    [PSI_CFC1] = /*cfc1*/ "t = F", [PSI_CTC1] = /*ctc1*/ "F = t",
    [PSI_BC1F] = /*bc1f/bc1fl*/ "?(!#) gb", [PSI_BC1T] = /*bc1t/bc1tl*/ "?(#) gb",
    // floats
    [PSI_ADDF] = /*add.fmt*/ "D = S + T", [PSI_SUBF] = /*sub.fmt*/ "D = S - T", [PSI_MULF] = /*mul.fmt*/ "D = S * T", [PSI_DIVF] = /*div.fmt*/ "D = S / T",
    [PSI_CVTF] = /*sqrt.fmt/abs.fmt/round.l.fmt/round.w.fmt/trunc.l.fmt/trunc.w.fmt/ceil.l.fmt/ceil.w.fmt/cvt.fmt.fmt/floor.l.fmt/ceil.l.fmt*/ "D = \\(S)",
    [PSI_MOVF] = /*mov.fmt*/ "D = S", [PSI_NEGF] = /*neg.fmt*/ "D = -S",

    // c.cond.fmt //! TODO: Are these correct? Can this be reduced?
#define ORDERED "V(S, T)"
    [PSI_C_F   ] = /*f*/    "# = 0",
    [PSI_C_UN  ] = /*un*/   "# = !"ORDERED,
    [PSI_C_EQ  ] = /*eq*/   "# = (S == T)",
    [PSI_C_UEQ ] = /*ueq*/  "# = !"ORDERED" || (S == T)",
    [PSI_C_OLT ] = /*olt*/  "# = "ORDERED" && (S < T)",
    [PSI_C_ULT ] = /*ult*/  "# = !"ORDERED" || (S < T)",
    [PSI_C_OLE ] = /*ole*/  "# = "ORDERED" && (S <= T)",
    [PSI_C_ULE ] = /*ule*/  "# = !"ORDERED" || (S <= T)",

    [PSI_C_SF  ] = /*sf*/   "# = G(0)",
    [PSI_C_NGLE] = /*ngle*/ /*"# = !"ORDERED" || !((S > T) || (S < T) || (S == T))",*/ "# = !"ORDERED,
    [PSI_C_SEQ ] = /*seq*/  "# = G(S == T)",
    [PSI_C_NGL ] = /*ngl*/  /*"# = !"ORDERED" || !((S > T) || (S < T))",*/ "# = !"ORDERED" || (S == T)",
    [PSI_C_LT  ] = /*lt*/   "# = (S < T)",
    [PSI_C_NGE ] = /*nge*/  "# = !(S >= T)",
    [PSI_C_LE  ] = /*le*/   "# = (S <= T)",
    [PSI_C_NGT ] = /*ngt*/  "# = !(S > T)",
    // move
    [PSI_MOVET] = /*move(rt)*/ "d = t", [PSI_MOVES] = /*move(rs)*/ "d = s",
};

#define SIZEOF_PSC sizeof(pseudo_c_code_formats)


void add_reg_str(char** c, RegisterSources src, int idx, _Bool isBase) {
    if ((src == REGS_CPU) && (idx == REG_CPU_R0)) { // $r0 -> 0
        if (isBase) {
            *c += sprintf(*c, STR_COLOR_PREFIX"NULL"STR_COLOR_PREFIX, COLOR_RGBA32_VSC_DEFINE, gCSDefaultPrintColor);
        } else {
            *c += sprintf(*c, STR_COLOR_PREFIX"0"STR_COLOR_PREFIX, COLOR_RGBA32_CRASH_DISASM_IMMEDIATE, gCSDefaultPrintColor);
        }
    } else {
        const RegisterInfo* regInfo = get_reg_info(src, idx);
        if (regInfo->name != NULL) {
            *c += sprintf(*c, STR_COLOR_PREFIX"%s"STR_COLOR_PREFIX, COLOR_RGBA32_CRASH_VARIABLE, regInfo->name, gCSDefaultPrintColor);
        }
    }
}

//! TODO: Should these be checked via structs/enums instead?
#define INSN_RAW_JR_RA      0x03E00008
#define INSN_RAW_TEQ_R0_R0  0x00000034

char* cs_insn_to_pseudo_c(InsnData insn) {
    _Bool decImmediates = (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_IMM_FMT) == PRINT_NUM_FMT_DEC);
    char* strp = &insn_as_string[0]; // Pointer to a location inside the string.
    bzero(insn_as_string, sizeof(insn_as_string));

    const InsnTemplate* info = get_insn(&insn);

    if (info == NULL) {
        return "";
    }

    u8 pseudoC = info->pseudoC;

    const char* formatStr = pseudo_c_code_formats[pseudoC];
    const char* comment = NULL;

    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_PSEUDOINSNS)) {
        if (insn.raw == INSN_RAW_JR_RA) { // jr $ra -> "return;"
            formatStr = "r";
            comment = "goto RA";
        } else if (insn.raw == INSN_RAW_TEQ_R0_R0) { // teq $r0,$r0 -> "trap();"
            formatStr = "X(e)";
            // comment = "if (0==0)";
        } else if (formatStr[0] == PSC_RA) { // "and link"
            formatStr += STRLEN(CLINK);
            comment = "RA=PC+2";
        }
    }

    const char* c = &formatStr[0];

    const RGBA32 baseColor = gCSDefaultPrintColor;
    RGBA32 newColor = baseColor;

    const char* immFmt = ((decImmediates && (pseudoC != PSI_LUI)) ? "%d" : STR_HEX_PREFIX"%X");

    while (*c != '\0') {
        const PSC_Entry* entry = NULL;

        _Bool found = FALSE;

        for (int i = 0; i < ARRAY_COUNT(psc_entries); i++) {
            entry = &psc_entries[i];
            if (*c == entry->c) {
                found = TRUE;
                break;
            }
        }

        if (!found) {
            if (IS_NUMERIC(*c)) {
                newColor = COLOR_RGBA32_CRASH_DISASM_IMMEDIATE;
                if (newColor != baseColor) {
                    strp += sprintf(strp, STR_COLOR_PREFIX, newColor);
                }
            } else {
                newColor = gCSDefaultPrintColor;
            }
            strp += sprintf(strp, "%c", *c);
        } else {
            const char* foundStr = (entry->a[0] & 0x80) ? entry->s : entry->a; // char* vs. char[4]
            newColor = sPSCColors[entry->col];
            if (newColor != baseColor) {
                strp += sprintf(strp, STR_COLOR_PREFIX, newColor);
            }
            switch (*c) {
                default:        strp += sprintf(strp, foundStr); break; // String without formatting specifiers.
                case PSC_INSN:  strp += sprintf(strp, (char[])UNPACK_STR7(insn_alphabet_lowercase, info->name)); break;

                case PSC_RS:    add_reg_str(&strp, REGS_CPU, insn.rs,  FALSE); break;
                case PSC_RT:    add_reg_str(&strp, REGS_CPU, insn.rt,  FALSE); break;
                case PSC_RD:    add_reg_str(&strp, REGS_CPU, insn.rd,  FALSE); break;
                case PSC_RD0:   add_reg_str(&strp, REGS_CP0, insn.rd,  FALSE); break;
                case PSC_FT:    add_reg_str(&strp, REGS_CP1, insn.ft,  FALSE); break;
                case PSC_FS:    add_reg_str(&strp, REGS_CP1, insn.fs,  FALSE); break;
                case PSC_FD:    add_reg_str(&strp, REGS_CP1, insn.fd,  FALSE); break;
                case PSC_FSC:   add_reg_str(&strp, REGS_FCR, insn.fs,  FALSE); break;
                case PSC_BASE:  add_reg_str(&strp, REGS_CPU, insn.base, TRUE); break;

                case PSC_IMM:   strp += sprintf(strp, immFmt, insn.immediate); break;
                case PSC_SHIFT: strp += sprintf(strp, foundStr, (insn.sa +  0)); break;
                case PSC_SH32:  strp += sprintf(strp, foundStr, (insn.sa + 32)); break;
                case PSC_BRANCH:
                    s16 b = (insn.offset + 1); strp += sprintf(strp, foundStr, ((b < 0x0000) ? '-' : '+'), abss(b));
                    break;
                case PSC_JUMP:
                    Address target = instr_index_to_addr(insn);
                    const char* name = NULL;
                    if (IS_DEBUG_MAP_ENABLED()) {
                        const MapSymbol* symbol = get_map_symbol(target, SYMBOL_SEARCH_BACKWARD);
                        if ((symbol != NULL) && symbol_is_function(symbol)) {
                            name = get_map_symbol_name(symbol);
                            if (name != NULL) {
                                strp += sprintf(strp, "%s()", name);
                            }
                        }
                    }
                    if (name == NULL) {
                        strp += sprintf(strp, foundStr, target);
                    }
                    break;
                case PSC_EXC10:
                    strp += sprintf(strp, immFmt, insn.codeA);
                    break;
                case PSC_EXC20:
                    if (info->opcode == OPS_BREAK) {
                        u16 eA = insn.codeA;
                        u16 eB = insn.codeB;
                        strp += sprintf(strp, (decImmediates ? "%d, %d" : STR_HEX_PREFIX"%X, "STR_HEX_PREFIX"%X"), eA, eB);
                        comment = get_name_from_null_terminated_id_list(eA, insn_break_codes);
                    } else {
                        strp += sprintf(strp, immFmt, insn.codeAB);
                    }
                    break;
            }
        }

        if (newColor != baseColor) {
            newColor = baseColor;
            strp += sprintf(strp, STR_COLOR_PREFIX, baseColor);
        }

        c++;
    }

    *strp++ = ';';

    if (comment != NULL) {
        strp += sprintf(strp, STR_COLOR_PREFIX" // %s", COLOR_RGBA32_VSC_COMMENT, comment);
    }

    return insn_as_string;
}
