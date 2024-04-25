#include <ultra64.h>

#include <string.h>

#include "types.h"
#include "sm64.h"

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
 * @param[in ] insn      The instruction data that is being read.
 * @return _Bool Whether the instruction has been converted.
 */
static _Bool check_pseudo_instructions(const InsnTemplate** type, InsnData insn) {
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
            if (check_pseudo_insn(type, PSEUDO_SUBI,  ((s16)insn.immediate < 0))) return TRUE;
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
            if (check_pseudo_insn(type, PSEUDO_DSUBI, ((s16)insn.immediate < 0))) return TRUE;
            break;
    }

    return FALSE;
}

/**
 * @brief Gets a pointer to the InsnTemplate data matching the given instruction data.
 *
 * @param[in] insn The instruction data that is being read.
 * @return const InsnTemplate* A pointer to the InsnTemplate data matching the given instruction data.
 */
const InsnTemplate* get_insn(InsnData insn) {
    const InsnTemplate* checkInsn = NULL;
    u8 opcode = insn.opcode; // First 6 bits.

    // Check for pseudo-instructions.
    if (
        cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_PSEUDOINSNS) &&
        check_pseudo_instructions(&checkInsn, insn)
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
    while (checkInsn->name != NULL) {
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
    const InsnTemplate* info = get_insn(insn);

    if ((info != NULL) && strchr(info->fmt, CHAR_P_BRANCH)) {
        return insn.offset;
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
        .raw = *(Word*)addr,
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
static char cop1_fmt_to_char(InsnData insn) {
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


static char insn_as_string[CHAR_BUFFER_SIZE] = "";
static char insn_name[INSN_NAME_DISPLAY_WIDTH] = "";


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
#define ADD_REG(_cop, _idx) {                                                                                     \
    regInfo = get_reg_info((_cop), (_idx));                                                                       \
    ADD_STR(STR_IREG, regInfo->name);                                                                             \
    append_reg_to_buffer((_cop), (_idx), ((cmdIndex == info->f2i) ? valFmt2 : valFmt1), (cmdIndex == info->out)); \
}

#define ADD_ADDR_REG(_cop, _idx) {                                  \
    regInfo = get_reg_info((_cop), (_idx));                         \
    ADD_STR(STR_IREG_BASE, regInfo->name);                          \
    append_reg_to_buffer((_cop), (_idx), REG_VAL_TYPE_ADDR, FALSE); \
}


/**
 * @brief Converts MIPS instruction data into a formatted string.
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

    const InsnTemplate* info = get_insn(insn);

    if (info != NULL) {
        const char* curCmd = &info->fmt[0];
        const RegisterInfo* regInfo = NULL;
        RGBA32 color = COLOR_RGBA32_NONE;
        _Bool separator = FALSE;
        enum RegisterValueTypes valFmt1 = (info->f1 ? REG_VAL_TYPE_FLOAT : REG_VAL_TYPE_INT); // Primary format.
        enum RegisterValueTypes valFmt2 = (info->f2 ? REG_VAL_TYPE_FLOAT : REG_VAL_TYPE_INT); // Secondary format.

        clear_saved_reg_buffer();

        // Whether to print immediates as decimal values rather than hexadecimal.
        _Bool decImmediates = (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_IMM_FMT) == PRINT_NUM_FMT_DEC);

        // Loop through the chars in the 'fmt' member of 'info' and print the insn data accordingly.
        for (size_t cmdIndex = 0; cmdIndex < STRLEN(info->fmt); cmdIndex++) {
            if (unimpl || (*curCmd == CHAR_NULL)) {
                break;
            }

            if (separator) {
                separator = FALSE;
                ADD_STR(", ");
            }

            switch (*curCmd) {
                case CHAR_P_NOP: // NOP.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_NOP);
                    ADD_STR(info->name);
                    return insn_as_string;
                case CHAR_P_NAME: // Instruction name.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_INSN);
                    ADD_STR(STR_INSN_NAME, info->name);
                    break;
                case CHAR_P_NAMEF: // Instruction name with format.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_INSN);
                    bzero(insn_name, sizeof(insn_name));
                    char fmtChar = cop1_fmt_to_char(insn);
                    sprintf(insn_name, STR_INSN_NAME_FORMAT, info->name, fmtChar);
                    valFmt2 = ((fmtChar == 'S' || fmtChar == 'D') ? REG_VAL_TYPE_FLOAT : REG_VAL_TYPE_INT); // Overwrite secondary format.
                    ADD_STR(STR_INSN_NAME, insn_name);
                    break;
                case CHAR_P_RS: // CPU 'RS' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rs);
                    separator = TRUE;
                    break;
                case CHAR_P_RT: // CPU 'RT' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rt);
                    separator = TRUE;
                    break;
                case CHAR_P_RD: // CPU 'RD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CPU, insn.rd);
                    separator = TRUE;
                    break;
                case CHAR_P_IMM: // Immediate.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_IMMEDIATE);
                    ADD_STR((decImmediates ? "%d" : STR_IMMEDIATE), insn.immediate);
                    break;
                case CHAR_P_NIMM: // Negative Immediate.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_IMMEDIATE);
                    ADD_STR((decImmediates ? "%d" : STR_IMMEDIATE), -(s16)insn.immediate);
                    break;
                case CHAR_P_SHIFT: // Shift amount.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_IMMEDIATE);
                    ADD_STR(STR_IMMEDIATE, insn.sa);
                    break;
                case CHAR_P_REGOFF: // Register offset offset.
                    ADD_COLOR(COLOR_RGBA32_CRASH_DISASM_REGOFFSET);
                    ADD_STR(STR_IMMEDIATE, insn.immediate);
                    break;
                case CHAR_P_BASE: // Register offset base.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_ADDR_REG(REGS_CPU, insn.base);
                    break;
                case CHAR_P_BRANCH: // Branch offset.
                    ADD_COLOR(COLOR_RGBA32_CRASH_OFFSET);
                    if (cs_get_setting_val(CS_OPT_GROUP_PAGE_DISASM, CS_OPT_DISASM_OFFSET_ADDR)) {
                        ADD_STR(STR_FUNCTION, get_insn_branch_target_from_addr(addr));
                    } else {
                        s16 branchOffset = (insn.offset + 1);
                        //! TODO: Is it possible to use the '%+' format specifier with the 0x prefix?
                        ADD_STR(STR_OFFSET, ((branchOffset < 0x0000) ? '-' : '+'), abss(branchOffset));
                    }
                    break;
                case CHAR_P_COP0D: // COP0 'RD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP0, insn.rd);
                    separator = TRUE;
                    break;
                case CHAR_P_FT: // COP1 'FT' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.ft);
                    separator = TRUE;
                    break;
                case CHAR_P_FS: // COP1 'FS' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.fs);
                    separator = TRUE;
                    break;
                case CHAR_P_FD: // COP1 'FD' register.
                    ADD_COLOR(COLOR_RGBA32_CRASH_VARIABLE);
                    ADD_REG(REGS_CP1, insn.fd);
                    separator = TRUE;
                    break;
                case CHAR_P_EXC10: // For TRAP IF instructions.
                    ADD_COLOR(COLOR_RGBA32_LIGHT_GRAY);
                    ADD_STR(STR_CODE10, insn.code10);
                    break;
                case CHAR_P_EXC20: // For SYSCALL and BREAK instructions.
                    ADD_COLOR(COLOR_RGBA32_LIGHT_GRAY);
                    ADD_STR(STR_CODE20, insn.code20);
                    break;
                case CHAR_P_FUNC: // Jump function.
                    ADD_COLOR(COLOR_RGBA32_CRASH_FUNCTION_NAME);
                    Address target = PHYSICAL_TO_VIRTUAL(insn.instr_index * sizeof(InsnData));
#ifdef INCLUDE_DEBUG_MAP
                    if (cs_get_setting_val(CS_OPT_GROUP_GLOBAL, CS_OPT_GLOBAL_SYMBOL_NAMES) && addr_is_in_text_segment(target)) {
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
                    }
#else // !INCLUDE_DEBUG_MAP
                    *fname = NULL;
#endif // !INCLUDE_DEBUG_MAP
                    ADD_STR(STR_FUNCTION, target);
                    break;
                default: // Unknown parameter.
                    unimpl = TRUE;
                    break;
            }

            curCmd++;
        }
    } else {
        unimpl = TRUE;
    }

    if (unimpl) {
        //! TODO: binary mode for these.
        ADD_STR((STR_HEX_WORD" (unimpl)"), insn.raw);
    }

    return insn_as_string;
}
