#include <PR/ultratypes.h>
#include <string.h>

#include "config.h"
#include "controller_select_menu.h"
#include "sm64.h"
#include "engine/math_util.h"
#include "engine/colors.h"
#include "game_init.h"
#include "ingame_menu.h"
#include "input.h"
#include "profiling.h"
#include "fasttext.h"
#include "segment2.h"

#ifdef ENABLE_STATUS_REPOLLING_GUI
 #if (MAX_NUM_PLAYERS > 1)
// Button names, in print order.
ALIGNED8 static const struct ButtonName sButtonNames[16] = {
    { .mask = U_CBUTTONS,   .name = "C UP",    },
    { .mask = D_CBUTTONS,   .name = "C DOWN",  },
    { .mask = L_CBUTTONS,   .name = "C LEFT",  },
    { .mask = R_CBUTTONS,   .name = "C RIGHT", },
    { .mask = U_JPAD,       .name = "D UP",    },
    { .mask = D_JPAD,       .name = "D DOWN",  },
    { .mask = L_JPAD,       .name = "D LEFT",  },
    { .mask = R_JPAD,       .name = "D RIGHT", },
    { .mask = Z_TRIG,       .name = "Z",       },
    { .mask = L_TRIG,       .name = "L",       },
    { .mask = R_TRIG,       .name = "R",       },
    { .mask = A_BUTTON,     .name = "A",       },
    { .mask = B_BUTTON,     .name = "B",       },
    { .mask = X_BUTTON,     .name = "X",       },
    { .mask = Y_BUTTON,     .name = "Y",       },
    { .mask = START_BUTTON, .name = "START",   },
};

/**
 * @brief Creates a string from a combination of buttons and adds it to 'strp'.
 */
static size_t button_combo_to_string(char* strp, u16 buttons) {
    size_t count = 0;

    for (int i = 0; i < ARRAY_COUNT(sButtonNames); i++) {
        const struct ButtonName* buttonName = &sButtonNames[i];

        if (buttons & buttonName->mask) {
            buttons &= ~buttonName->mask;

            if (count) {
                strcat(strp, "+");
                count += strlen("+");
            }

            strcat(strp, buttonName->name);
            count += strlen(buttonName->name);
        }
    }

    return count;
}
 #endif // (MAX_NUM_PLAYERS > 1)

#ifdef CONTROLLERS_INPUT_DISPLAY

// This is 4 32x32 RGBA16 textures, 2048 bytes each, 8192 bytes total.
ALIGNED8 RGBA16 sInputOverlayTextures[MAXCONTROLLERS][CONT_ICON_W * CONT_ICON_H];


// N64 Normal:
ALIGNED4 const ButtonHighlight buttons_display_controller_n64_normal[] = {
    { .mask = CONT_A,           .x = 22, .y = 13, .w = 2, .h = 2, },
    { .mask = CONT_B,           .x = 20, .y = 11, .w = 2, .h = 2, },
    { .mask = CONT_L,           .x =  6, .y =  5, .w = 4, .h = 1, },
    { .mask = CONT_R,           .x = 22, .y =  5, .w = 4, .h = 1, },
    { .mask = CONT_G,           .x = 15, .y =  7, .w = 2, .h = 3, },
    { .mask = CONT_START,       .x = 15, .y = 11, .w = 2, .h = 2, },
    { .mask = CONT_UP,          .x =  7, .y =  8, .w = 2, .h = 2, },
    { .mask = CONT_DOWN,        .x =  7, .y = 12, .w = 2, .h = 2, },
    { .mask = CONT_LEFT,        .x =  5, .y = 10, .w = 2, .h = 2, },
    { .mask = CONT_RIGHT,       .x =  9, .y = 10, .w = 2, .h = 2, },
    { .mask = CONT_E,           .x = 24, .y =  7, .w = 2, .h = 2, },
    { .mask = CONT_D,           .x = 24, .y = 11, .w = 2, .h = 2, },
    { .mask = CONT_C,           .x = 22, .y =  9, .w = 2, .h = 2, },
    { .mask = CONT_F,           .x = 26, .y =  9, .w = 2, .h = 2, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_n64_normal[] = {
    { .mask = ANALOG_STICK,     .x = 15, .y = 17, .w = 2, .h = 2, },
    { .mask = (u16)-1, },
};

// N64 Mouse:
ALIGNED4 const ButtonHighlight buttons_display_controller_n64_mouse[] = {
    { .mask = CONT_A,           .x = 10, .y =  8, .w = 5, .h = 5, },
    { .mask = CONT_B,           .x = 17, .y =  8, .w = 5, .h = 5, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_n64_mouse[] = {
    { .mask = ANALOG_STICK,     .x = 15, .y = 17, .w = 2, .h = 2, },
    { .mask = (u16)-1, },
};

// GBA:
ALIGNED4 const ButtonHighlight buttons_display_controller_gba[] = {
    { .mask = CONT_A,           .x = 27, .y = 13, .w = 2, .h = 2, },
    { .mask = CONT_B,           .x = 24, .y = 14, .w = 2, .h = 2, },
    { .mask = CONT_L,           .x =  3, .y =  9, .w = 3, .h = 1, },
    { .mask = CONT_R,           .x = 26, .y =  9, .w = 3, .h = 1, },
    { .mask = CONT_G,           .x =  7, .y = 20, .w = 1, .h = 1, },
    { .mask = CONT_START,       .x =  7, .y = 10, .w = 1, .h = 1, },
    { .mask = CONT_UP,          .x =  5, .y = 13, .w = 1, .h = 1, },
    { .mask = CONT_DOWN,        .x =  5, .y = 15, .w = 1, .h = 1, },
    { .mask = CONT_LEFT,        .x =  4, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_RIGHT,       .x =  6, .y = 14, .w = 1, .h = 1, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_gba[] = {
    { .mask = ANALOG_STICK,     .x =  4, .y = 13, .w = 3, .h = 3, },
    { .mask = (u16)-1, },
};

// GCN Normal:
ALIGNED4 const ButtonHighlight buttons_display_controller_gcn_normal[] = {
    { .mask = CONT_GCN_START,   .x = 15, .y = 12, .w = 2, .h = 2, },
    { .mask = CONT_GCN_Y,       .x = 21, .y =  9, .w = 2, .h = 2, },
    { .mask = CONT_GCN_X,       .x = 25, .y = 10, .w = 2, .h = 2, },
    { .mask = CONT_GCN_B,       .x = 20, .y = 14, .w = 2, .h = 2, },
    { .mask = CONT_GCN_A,       .x = 22, .y = 11, .w = 3, .h = 3, },
    { .mask = CONT_GCN_L,       .x =  7, .y =  6, .w = 3, .h = 1, },
    { .mask = CONT_GCN_R,       .x = 22, .y =  6, .w = 3, .h = 1, },
    { .mask = CONT_GCN_Z,       .x = 22, .y =  7, .w = 3, .h = 1, },
    { .mask = CONT_GCN_UP,      .x = 11, .y = 17, .w = 1, .h = 1, },
    { .mask = CONT_GCN_DOWN,    .x = 11, .y = 19, .w = 1, .h = 1, },
    { .mask = CONT_GCN_LEFT,    .x = 10, .y = 18, .w = 1, .h = 1, },
    { .mask = CONT_GCN_RIGHT,   .x = 12, .y = 18, .w = 1, .h = 1, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_gcn_normal[] = {
    { .mask = ANALOG_STICK,     .x =  7, .y = 11, .w = 3, .h = 3, },
    { .mask = ANALOG_C_STICK,   .x = 19, .y = 17, .w = 3, .h = 3, },
    { .mask = ANALOG_TRIG_L,    .x =  7, .y =  5, .w = 3, .h = 1, },
    { .mask = ANALOG_TRIG_R,    .x = 22, .y =  5, .w = 3, .h = 1, },
    { .mask = (u16)-1, },
};

// GCN Wheel:
ALIGNED4 const ButtonHighlight buttons_display_controller_gcn_wheel[] = {
    { .mask = CONT_GCN_START,   .x = 15, .y = 22, .w = 2, .h = 2, },
    { .mask = CONT_GCN_Y,       .x = 21, .y = 15, .w = 1, .h = 2, },
    { .mask = CONT_GCN_X,       .x = 22, .y = 14, .w = 2, .h = 1, },
    { .mask = CONT_GCN_B,       .x = 23, .y = 17, .w = 1, .h = 1, },
    { .mask = CONT_GCN_A,       .x = 22, .y = 15, .w = 2, .h = 2, },
    { .mask = CONT_GCN_L,       .x =  8, .y = 18, .w = 3, .h = 2, },
    { .mask = CONT_GCN_R,       .x = 21, .y = 18, .w = 3, .h = 2, },
    { .mask = CONT_GCN_Z,       .x =  9, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_GCN_UP,      .x =  9, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_GCN_DOWN,    .x =  9, .y = 17, .w = 1, .h = 1, },
    { .mask = CONT_GCN_LEFT,    .x =  8, .y = 16, .w = 1, .h = 1, },
    { .mask = CONT_GCN_RIGHT,   .x = 10, .y = 16, .w = 1, .h = 1, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_gcn_wheel[] = {
    { .mask = ANALOG_STICK,     .x = 15, .y = 15, .w = 2, .h = 2, },
    { .mask = ANALOG_TRIG_L,    .x =  8, .y = 13, .w = 2, .h = 2, },
    { .mask = ANALOG_TRIG_R,    .x = 21, .y = 13, .w = 2, .h = 2, },
    { .mask = (u16)-1, },
};

// GCN Keyboard:
ALIGNED4 const ButtonHighlight buttons_display_controller_gcn_keyboard[] = {
    { .mask = CONT_GCN_START,   .x =  7, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_GCN_Y,       .x = 27, .y = 13, .w = 1, .h = 1, },
    { .mask = CONT_GCN_X,       .x = 28, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_GCN_B,       .x = 26, .y = 15, .w = 1, .h = 1, },
    { .mask = CONT_GCN_A,       .x = 27, .y = 14, .w = 1, .h = 1, },
    { .mask = CONT_GCN_L,       .x =  3, .y = 11, .w = 3, .h = 1, },
    { .mask = CONT_GCN_R,       .x = 26, .y = 11, .w = 3, .h = 1, },
    { .mask = CONT_GCN_Z,       .x = 25, .y = 12, .w = 3, .h = 1, },
    { .mask = CONT_GCN_UP,      .x =  6, .y = 16, .w = 1, .h = 1, },
    { .mask = CONT_GCN_DOWN,    .x =  6, .y = 18, .w = 1, .h = 1, },
    { .mask = CONT_GCN_LEFT,    .x =  5, .y = 17, .w = 1, .h = 1, },
    { .mask = CONT_GCN_RIGHT,   .x =  7, .y = 17, .w = 1, .h = 1, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_gcn_keyboard[] = {
    { .mask = ANALOG_STICK,     .x =  4, .y = 14, .w = 1, .h = 1, },
    { .mask = ANALOG_C_STICK,   .x = 25, .y = 16, .w = 1, .h = 1, },
    { .mask = ANALOG_TRIG_L,    .x =  3, .y = 10, .w = 2, .h = 1, },
    { .mask = ANALOG_TRIG_R,    .x = 26, .y = 10, .w = 2, .h = 1, },
    { .mask = (u16)-1, },
};

// GCN Dancepad:
ALIGNED4 const ButtonHighlight buttons_display_controller_gcn_dancepad[] = {
    { .mask = CONT_GCN_START,   .x = 21, .y =  5, .w = 3, .h = 1, },
    { .mask = CONT_GCN_Y,       .x =  8, .y = 22, .w = 3, .h = 3, },
    { .mask = CONT_GCN_X,       .x = 21, .y = 22, .w = 3, .h = 3, },
    { .mask = CONT_GCN_B,       .x =  8, .y =  9, .w = 3, .h = 3, },
    { .mask = CONT_GCN_A,       .x = 21, .y =  9, .w = 3, .h = 3, },
    { .mask = CONT_GCN_Z,       .x =  8, .y =  5, .w = 3, .h = 1, },
    { .mask = CONT_GCN_UP,      .x = 13, .y =  8, .w = 6, .h = 5, },
    { .mask = CONT_GCN_DOWN,    .x = 13, .y = 21, .w = 6, .h = 5, },
    { .mask = CONT_GCN_LEFT,    .x =  7, .y = 14, .w = 5, .h = 6, },
    { .mask = CONT_GCN_RIGHT,   .x = 20, .y = 14, .w = 5, .h = 6, },
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_gcn_dancepad[] = {
    { .mask = ANALOG_STICK,     .x = 14, .y = 15, .w = 4, .h = 4, },
    { .mask = (u16)-1, },
};

// NULL:
ALIGNED4 const ButtonHighlight buttons_display_controller_null[] = {
    { .mask = (u16)-1, },
};
ALIGNED4 const ButtonHighlight analog_display_controller_null[] = {
    { .mask = (u16)-1, },
};

/**
 * @brief Draws a red rectangle on sInputOverlayTextures using the ButtonHighlight data.
 *
 * @param port The port index to use when writing to sInputOverlayTextures.
 * @param buttonHighlight The ButtonHighlight data.
 */
void apply_to_overlay_texture(int port, const ButtonHighlight* buttonHighlight) {
    int startX = buttonHighlight->x;
    int startY = buttonHighlight->y;
    int w = buttonHighlight->w;
    int h = buttonHighlight->h;

    RGBA16* dst = (sInputOverlayTextures[port] + (CONT_ICON_W * startY) + startX);

    for (int y = 0; y < h; y++) {
        for (int x = 0; x < w; x++) {
            *dst++ = GPACK_RGBA5551(255, 0, 0, 255);
        }

        dst += (CONT_ICON_W - w);
    }
}

/**
 * @brief Loops through a ButtonHighlight list and apply all active highlights for a given port based on controller button input.
 *
 * @param port The port to use input from and the port index to use when writing to sInputOverlayTextures.
 * @param buttonHighlightList Pointer to a list of ButtonHighlight data.
 */
static void overlay_texture_buttons(int port, const ButtonHighlight* buttonHighlight) {
    u16 buttons = gControllerPads[port].physButton.raw;

    while (buttonHighlight->mask != (u16)-1) {
        if ((buttons & buttonHighlight->mask) != 0) {
            apply_to_overlay_texture(port, buttonHighlight);
        }

        buttonHighlight++;
    }
}

/**
 * @brief Loops through a ButtonHighlight list and apply all active highlights for a given port based on controller analog input.
 *
 * @param port The port to use input from and the port index to use when writing to sInputOverlayTextures.
 * @param analogHighlight Pointer to a list of ButtonHighlight data.
 */
static void overlay_texture_analog(int port, const ButtonHighlight* analogHighlight) {
    OSContPadEx* pad = &gControllerPads[port];
    const s8 deadzone = ANALOG_DEADZONE_STATUS_POLLING;

    while (analogHighlight->mask != (u16)-1) {
        _Bool active = FALSE;

        switch (analogHighlight->mask) {
            case ANALOG_STICK:   active = ((abss(pad->stick.x  ) > deadzone) || (abss(pad->stick.y  ) > deadzone)); break;
            case ANALOG_C_STICK: active = ((abss(pad->c_stick.x) > deadzone) || (abss(pad->c_stick.y) > deadzone)); break;
            case ANALOG_TRIG_L:  active = (pad->trig.l > deadzone); break;
            case ANALOG_TRIG_R:  active = (pad->trig.r > deadzone); break;
            default: break;
        }

        if (active) {
            apply_to_overlay_texture(port, analogHighlight);
        }

        analogHighlight++;
    }
}

/**
 * @brief Applies highlights for buttons and analog inputs.
 *
 * @param port The port to use input from and the port index to use when writing to sInputOverlayTextures.
 * @param icon The controller icon data.
 */
static void set_overlay_texture(int port, const struct ControllerIcon* icon) {
    overlay_texture_buttons(port, *(icon->buttonHighlightList));
    overlay_texture_analog( port, *(icon->analogHighlightList));
}

 #define CONT_BUTTON_LIST(type) .buttonHighlightList = &buttons_display_controller_##type
 #define CONT_ANALOG_LIST(type) .analogHighlightList = &analog_display_controller_##type
#else // !CONTROLLERS_INPUT_DISPLAY
 #define CONT_BUTTON_LIST(type)
 #define CONT_ANALOG_LIST(type)
#endif // !CONTROLLERS_INPUT_DISPLAY
 #define CONT_TEXTURE(type) .texture = texture_controller_##type

// Controller icons (see segment2.c).
ALIGNED8 static const struct ControllerIcon sControllerIcons[] = {
    { .type = CONT_NONE,              CONT_TEXTURE(port        ), CONT_BUTTON_LIST(null        ), CONT_ANALOG_LIST(null        ), },
    { .type = CONT_TYPE_NORMAL,       CONT_TEXTURE(n64_normal  ), CONT_BUTTON_LIST(n64_normal  ), CONT_ANALOG_LIST(n64_normal  ), },
    { .type = CONT_TYPE_MOUSE,        CONT_TEXTURE(n64_mouse   ), CONT_BUTTON_LIST(n64_mouse   ), CONT_ANALOG_LIST(n64_mouse   ), },
    { .type = CONT_TYPE_VOICE,        CONT_TEXTURE(n64_voice   ), CONT_BUTTON_LIST(null        ), CONT_ANALOG_LIST(null        ), },
    { .type = CONT_TYPE_KEYBOARD,     CONT_TEXTURE(n64_keyboard), CONT_BUTTON_LIST(null        ), CONT_ANALOG_LIST(null        ), },
    { .type = CONT_TYPE_GBA,          CONT_TEXTURE(gba         ), CONT_BUTTON_LIST(gba         ), CONT_ANALOG_LIST(gba         ), },
    { .type = CONT_TYPE_GCN_NORMAL,   CONT_TEXTURE(gcn_normal  ), CONT_BUTTON_LIST(gcn_normal  ), CONT_ANALOG_LIST(gcn_normal  ), },
    { .type = CONT_TYPE_GCN_RECEIVER, CONT_TEXTURE(gcn_receiver), CONT_BUTTON_LIST(null        ), CONT_ANALOG_LIST(null        ), },
    { .type = CONT_TYPE_GCN_WAVEBIRD, CONT_TEXTURE(gcn_wavebird), CONT_BUTTON_LIST(gcn_normal  ), CONT_ANALOG_LIST(gcn_normal  ), },
    { .type = CONT_TYPE_GCN_WHEEL,    CONT_TEXTURE(gcn_wheel   ), CONT_BUTTON_LIST(gcn_wheel   ), CONT_ANALOG_LIST(gcn_wheel   ), },
    { .type = CONT_TYPE_GCN_KEYBOARD, CONT_TEXTURE(gcn_keyboard), CONT_BUTTON_LIST(gcn_keyboard), CONT_ANALOG_LIST(gcn_keyboard), },
    { .type = CONT_TYPE_GCN_DANCEPAD, CONT_TEXTURE(gcn_dancepad), CONT_BUTTON_LIST(gcn_dancepad), CONT_ANALOG_LIST(null        ), },
    { .type = (u16)-1,                CONT_TEXTURE(unknown     ), CONT_BUTTON_LIST(null        ), CONT_ANALOG_LIST(null        ), },
};

/**
 * @brief Loops through sControllerIcons to get the port's controller's corresponding texture.
 *
 * @param[in] port The port with the controller to check.
 * @return const struct ControllerIcon* A pointer to the icon data in sControllerIcons.
 */
const struct ControllerIcon* get_controller_icon(int port) {
    const struct ControllerIcon* icon = &sControllerIcons[0];
    u16 type = gControllerStatuses[port].type;

    while (icon->type != (u16)-1) {
        if (icon->type == type) {
            break;
        }

        icon++;
    }

    return icon;
}

/**
 * @brief Displays controller info (eg. type and player number) while polling for controller statuses.
 * TODO: Fix this not rendering/updating during transitions.
 */
void render_controllers_overlay(void) {
    const s16 texW = CONT_ICON_W;
    const s16 texH = CONT_ICON_H;
    const s16 w = CONT_ICON_W;
    const s16 h = CONT_ICON_H;
    s16 x = SCREEN_CENTER_X;
    const s16 y = (SCREEN_CENTER_Y - (h / 2));
    const s16 spacing = 2;
    const s16 spacedW = (w + spacing);
    const s16 iconStartX = (SCREEN_CENTER_X - (((spacedW * MAXCONTROLLERS) - spacing) / 2));
    char text_buffer[32] = "";
    int port;

    // Only show UI when status polling and not in boot mode.
    if (!gContStatusPolling || gContStatusPollingIsBootMode) {
        return;
    }

    Color col = remap(get_cycle(((f32)CONT_STATUS_POLLING_TIME / 30.0f), 1.0f, gContStatusPollTimer), -1, 1, 127, 255);

    // Darken the screen while polling controller status, similar to pausing the game.
    shade_screen();

 #ifdef CONTROLLERS_INPUT_DISPLAY
    bzero(sInputOverlayTextures, sizeof(sInputOverlayTextures));
 #endif // CONTROLLERS_INPUT_DISPLAY

    Gfx* dlHead = gDisplayListHead;

    // Allow drawing outside the screen borders.
    gDPSetScissor(dlHead++, G_SC_NON_INTERLACE, 0, 0, SCREEN_WIDTH, SCREEN_HEIGHT);

    gSPDisplayList(dlHead++, dl_texrect_rgba32_begin);

    // Draw the port icons:
    for (port = 0; port < MAXCONTROLLERS; port++) {
        const struct ControllerIcon* icon = get_controller_icon(port);
        x = (iconStartX + (spacedW * port));

        texrect_rgba32(&dlHead, icon->texture, texW, texH, x, y, w, h);

 #ifdef CONTROLLERS_INPUT_DISPLAY
        set_overlay_texture(port, icon);

        texrect_rgba32(&dlHead, (Texture*)sInputOverlayTextures[port], texW, texH, x, y, w, h);
 #endif // CONTROLLERS_INPUT_DISPLAY
    }

    gSPDisplayList(dlHead++, dl_texrect_rgba32_end);
    gSPDisplayList(dlHead++, dl_fasttext_begin);

    drawSmallStringCol(&dlHead, (SCREEN_CENTER_X - 79), (SCREEN_CENTER_Y - 40), "WAITING FOR CONTROLLERS...", col, col, col);

    // Instructions (centered text):
    if (gControllerBits) {
        if (gContStatusPollingReadyForInput) {
            sprintf(text_buffer, "PRESS BUTTON TO ASSIGN P%d", (gNumPlayers + 1));
            drawSmallStringCol(&dlHead,
                (SCREEN_CENTER_X - (fasttext_get_str_width(text_buffer) / 2)),
                (SCREEN_CENTER_Y - ((CONT_ICON_H / 2) + 12)),
                text_buffer,
                col, col, col
            );
 #if (MAX_NUM_PLAYERS > 1)
            char comboStr[32] = "";
            button_combo_to_string(comboStr, TOGGLE_CONT_STATUS_POLLING_COMBO);
            sprintf(text_buffer, "OR %s TO EXIT", comboStr);
            drawSmallStringCol(&dlHead,
                (SCREEN_CENTER_X - (fasttext_get_str_width(text_buffer) / 2)),
                (SCREEN_CENTER_Y + ((CONT_ICON_H / 2) + 12)),
                text_buffer,
                col, col, col
            );
 #endif // (MAX_NUM_PLAYERS > 1)
        } else {
            sprintf(text_buffer, "RELEASE ALL INPUTS TO CONTINUE");
            drawSmallStringCol(&dlHead,
                (SCREEN_CENTER_X - (fasttext_get_str_width(text_buffer) / 2)),
                (SCREEN_CENTER_Y - ((CONT_ICON_H / 2) + 12)),
                text_buffer,
                col, col, col
            );
        }
    }

    // Print the assigned player numbers.
    for (port = 0; port < MAXCONTROLLERS; port++) {
        u8 playerNum = gControllerPads[port].playerNum;

        // Print if a controller is plugged in and assigned to a player.
        if ((gControllerStatuses[port].type != CONT_NONE) && (playerNum != 0)) {
            sprintf(text_buffer, "P%d", playerNum);
            s32 playerNumX = ((iconStartX + 9) + (spacedW * port));
            drawSmallString(&dlHead, playerNumX, (SCREEN_CENTER_Y + (CONT_ICON_H / 2)), text_buffer);
        }
    }

    gSPDisplayList(dlHead++, dl_fasttext_end);

    // Disallow drawing outside the screen borders.
    gDPSetScissor(dlHead++, G_SC_NON_INTERLACE, 0, gBorderHeight, SCREEN_WIDTH, (SCREEN_HEIGHT - gBorderHeight));

    gDisplayListHead = dlHead;
}
#endif // ENABLE_STATUS_REPOLLING_GUI
