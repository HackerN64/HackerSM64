#include <ultra64.h>
#include "types.h"
#include "sm64.h"
#include "crash_main.h"
#include "address_select.h"
#include "game/input.h"
#include "pages/page_disasm.h"


_Bool gAddressSelectMenuOpen = FALSE;
static Address sAddressSelectTarget = 0x00000000;
static s8 sAddressSelectCharIndex = 7;


void draw_address_select(void) {
    crash_screen_draw_dark_rect(
        (JUMP_MENU_X1 -  JUMP_MENU_MARGIN_X     ), (JUMP_MENU_Y1 -  JUMP_MENU_MARGIN_Y     ),
        (JUMP_MENU_W  + (JUMP_MENU_MARGIN_X * 2)), (JUMP_MENU_H  + (JUMP_MENU_MARGIN_Y * 2)),
        CS_DARKEN_SEVEN_EIGHTHS
    );

    // "GO TO:"
    crash_screen_print((SCREEN_CENTER_X - (TEXT_WIDTH(STRLEN("GO TO")) / 2)), JUMP_MENU_Y1, "GO TO:");

    Address addr = sAddressSelectTarget;
    Word data = 0;
    _Bool isValid = try_read_data(&data, addr);

    u32 addressStartX = (SCREEN_CENTER_X - (TEXT_WIDTH(SIZEOF_HEX(Address)) / 2));
    u32 addressStartY = (JUMP_MENU_Y1 + TEXT_HEIGHT(2));
    // "[XXXXXXXX]"
    crash_screen_print(
        addressStartX, addressStartY,
        (STR_COLOR_PREFIX STR_HEX_WORD),
        (isValid ? COLOR_RGBA32_CRASH_YES : COLOR_RGBA32_CRASH_NO), addr
    );

    u32 triangleStartX = ((addressStartX + (sAddressSelectCharIndex * TEXT_WIDTH(1))) - 1);
    u32 triangleStartY = ((addressStartY - TEXT_HEIGHT(1)) + CRASH_SCREEN_CHAR_SPACING_Y);
    // Up arrow:
    crash_screen_draw_vertical_triangle(
        triangleStartX, triangleStartY,
        TEXT_WIDTH(1), TEXT_WIDTH(1),
        COLOR_RGBA32_CRASH_SELECT_ARROW
    );
    triangleStartY += ((TEXT_WIDTH(1) + TEXT_HEIGHT(1)) - 1);
    // Down arrow:
    crash_screen_draw_vertical_triangle(
        triangleStartX, triangleStartY,
        TEXT_WIDTH(1), -TEXT_WIDTH(1),
        COLOR_RGBA32_CRASH_SELECT_ARROW
    );

#ifdef INCLUDE_DEBUG_MAP
    if (isValid) {
        const struct MapSymbol* symbol = get_map_symbol(addr, SYMBOL_SEARCH_BACKWARD);
        if (symbol != NULL) {
            // "[mapped data name]"
            crash_screen_print_symbol_name(JUMP_MENU_X1, (JUMP_MENU_Y1 + TEXT_HEIGHT(4)), JUMP_MENU_CHARS_X, symbol);
        }
    }
#endif

    osWritebackDCacheAll();
}

extern u32 sMapViewerSelectedIndex;

void crash_screen_select_address(void) {
    s8 change = 0;

    if (gCSDirectionFlags.pressed.left) {
        change = -1;
    }
    if (gCSDirectionFlags.pressed.right) {
        change =  1;
    }

    sAddressSelectCharIndex = ((sAddressSelectCharIndex + change) % SIZEOF_HEX(Address));

    const u32 shift = ((SIZEOF_BITS(Address) - BITS_PER_HEX) - (sAddressSelectCharIndex * BITS_PER_HEX));
    const s8 digit = GET_HEX_DIGIT(sAddressSelectTarget, shift);
    s8 new = digit;
    change = 0;

    if (gCSDirectionFlags.pressed.up) {
        change =  1;
    }
    if (gCSDirectionFlags.pressed.down) {
        change = -1;
    }

    if (change != 0) {
        // Wrap to virtual ram address:
        do {
            new = ((new + change) & BITMASK(BITS_PER_HEX));
        } while (SET_HEX_DIGIT(sAddressSelectTarget, new, shift) < VIRTUAL_RAM_START);
    }

    if (new != digit) {
        sAddressSelectTarget = SET_HEX_DIGIT(sAddressSelectTarget, new, shift);
    }

    u16 buttonPressed = gPlayer1Controller->buttonPressed;

    if (buttonPressed & A_BUTTON) {
        // Jump to the address and close the popup.
        gAddressSelectMenuOpen = FALSE;

        switch (gCSPageID) {
            case PAGE_STACK_TRACE:
                crash_screen_set_page(PAGE_DISASM);
                break;
#ifdef INCLUDE_DEBUG_MAP
            case PAGE_MAP_VIEWER:;
                s32 targetIndex = get_symbol_index_from_addr_backward(sAddressSelectTarget);
                if (targetIndex != -1) {
                    if (sMapViewerSelectedIndex == (u32)targetIndex) {
                        if (is_in_code_segment(gMapSymbols[targetIndex].addr)) {
                            crash_screen_set_page(PAGE_DISASM);
                        } else {
                            crash_screen_set_page(PAGE_RAM_VIEWER);
                        }
                    }
                    sMapViewerSelectedIndex = targetIndex;
                }
                break;
            case PAGE_DISASM:
                if (get_symbol_index_from_addr_forward(gSelectedAddress) != get_symbol_index_from_addr_forward(sAddressSelectTarget)) {
                    gFillBranchBuffer = TRUE;
                }
                break;
#endif
            default:
                break;
        }

        gSelectedAddress = sAddressSelectTarget;
    }

    if (buttonPressed & B_BUTTON) {
        // Close the popup without jumping.
        gAddressSelectMenuOpen = FALSE;
    }
}

// Open the jump to address popup.
void open_address_select(Address dest) {
    gAddressSelectMenuOpen = TRUE;
    sAddressSelectTarget = dest;
}
