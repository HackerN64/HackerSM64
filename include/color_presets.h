#pragma once

#include "PR/gbi.h"


// -- Texture/Color format constants --

// NOTE: IA4 and RGBA16 formats are tricky, because while their
// composite values are 4bit aligned, their components are not.


// I4
// A 0000 0000 0000 0000 0000 0000 0000 1111 >> 0x00 & 0x0F = 0-15 * (255/15 = 17)              = 0..255
#define SIZ_I4      0x4
#define MSK_I4      BITMASK(SIZ_I4)

// I8
// A 0000 0000 0000 0000 0000 0000 1111 1111 >> 0x00 & 0xFF                                     = 0..255
#define SIZ_I8      0x8
#define MSK_I8      BITMASK(SIZ_I8)

// IA4
// I 0000 0000 0000 0000 0000 0000 0000 1110 >> 0x01 & 0x07 = 0-7 * (255/7 = 36.4285714286)     = 0..255
// A 0000 0000 0000 0000 0000 0000 0000 0001 >> 0x00 & 0x01 = 0-255
#define SIZ_IA4_I   0x3
#define MSK_IA4_I   BITMASK(SIZ_IA4_I)
#define IDX_IA4_I   ((SIZ_IA4_I * 0) + 1)
#define IA4_I(c)    (((c) >> IDX_IA4_I) & MSK_IA4_I)
#define I_IA4(c)    (((c) & MSK_IA4_I) << IDX_IA4_I)
#define SIZ_IA4_A   0x1
#define MSK_IA4_A   BITMASK(SIZ_IA4_A)
#define IDX_IA4_A   ((SIZ_IA4_A * 0) + 0)
#define IA4_A(c)    (((c) >> IDX_IA4_A) & MSK_IA4_A)
#define A_IA4(c)    (((c) & MSK_IA4_A) << IDX_IA4_A)

// IA8
// I 0000 0000 0000 0000 0000 0000 1111 0000 >> 0x04 & 0x0F = 0-15 * (255/15 = 17)              = 0..255
// A 0000 0000 0000 0000 0000 0000 0000 1111 >> 0x00 & 0x0F = 0-15 * (255/15 = 17)              = 0..255
#define SIZ_IA8_C   0x4
#define MSK_IA8_C   BITMASK(SIZ_IA8_C)
#define IDX_IA8_I   ((SIZ_IA8_C * 1) + 0)
#define IDX_IA8_A   ((SIZ_IA8_C * 0) + 0)
#define IA8_I(c)    (((c) >> IDX_IA8_I) & MSK_IA8_C)
#define IA8_A(c)    (((c) >> IDX_IA8_A) & MSK_IA8_C)
#define I_IA8(c)    (((c) & MSK_IA8_C) << IDX_IA8_I)
#define A_IA8(c)    (((c) & MSK_IA8_C) << IDX_IA8_A)

// IA16
// I 0000 0000 0000 0000 1111 1111 0000 0000 >> 0x8  & 0xFF                                     = 0..255
// A 0000 0000 0000 0000 0000 0000 1111 1111 >> 0x0  & 0xFF                                     = 0..255
#define SIZ_IA16_C  0x8
#define MSK_IA16_C  BITMASK(SIZ_IA16_C)
#define IDX_IA16_I  ((SIZ_IA16_C * 1) + 0)
#define IDX_IA16_A  ((SIZ_IA16_C * 0) + 0)
#define IA16_I(c)   (((c) >> IDX_IA16_I) & MSK_IA16_C)
#define IA16_A(c)   (((c) >> IDX_IA16_A) & MSK_IA16_C)
#define I_IA16(c)   (((c) & MSK_IA16_C) << IDX_IA16_I)
#define A_IA16(c)   (((c) & MSK_IA16_C) << IDX_IA16_A)

// RGBA16
//   0000 0000 0000 0000 0000 0000 0001 1111
// R 0000 0000 0000 0000 1111 1000 0000 0000 >> 0x0B & 0x1F = 0-31 * (255/31 = 8.22580645161)   = 0..255
// G 0000 0000 0000 0000 0000 0111 1100 0000 >> 0x06 & 0x1F = 0-31 * (255/31 = 8.22580645161)   = 0..255
// B 0000 0000 0000 0000 0000 0000 0011 1110 >> 0x01 & 0x1F = 0-31 * (255/31 = 8.22580645161)   = 0..255
// A 0000 0000 0000 0000 0000 0000 0000 0001 >> 0x00 & 0x01 = 0-1
#define SIZ_RGBA16_C 0x5
#define MSK_RGBA16_C BITMASK(SIZ_RGBA16_C)
#define IDX_RGBA16_R ((SIZ_RGBA16_C * 2) + 1)
#define IDX_RGBA16_G ((SIZ_RGBA16_C * 1) + 1)
#define IDX_RGBA16_B ((SIZ_RGBA16_C * 0) + 1)
#define RGBA16_R(c)  (((c) >> IDX_RGBA16_R) & MSK_RGBA16_C)
#define RGBA16_G(c)  (((c) >> IDX_RGBA16_G) & MSK_RGBA16_C)
#define RGBA16_B(c)  (((c) >> IDX_RGBA16_B) & MSK_RGBA16_C)
#define R_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_R)
#define G_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_G)
#define B_RGBA16(c)  (((c) & MSK_RGBA16_C) << IDX_RGBA16_B)
#define SIZ_RGBA16_A 0x1
#define MSK_RGBA16_A BITMASK(SIZ_RGBA16_A)
#define IDX_RGBA16_A ((SIZ_RGBA16_A * 0) + 0)
#define RGBA16_A(c)  (((c) >> IDX_RGBA16_A) & MSK_RGBA16_A)
#define A_RGBA16(c)  (((c) & MSK_RGBA16_A) << IDX_RGBA16_A)

// RGBA32
// R 1111 1111 0000 0000 0000 0000 0000 0000 >> 0x20 & 0xFF                                     = 0..255
// G 0000 0000 1111 1111 0000 0000 0000 0000 >> 0x10 & 0xFF                                     = 0..255
// B 0000 0000 0000 0000 1111 1111 0000 0000 >> 0x08 & 0xFF                                     = 0..255
// A 0000 0000 0000 0000 0000 0000 1111 1111 >> 0x00 & 0xFF                                     = 0..255
#define SIZ_RGBA32_C 0x8
#define MSK_RGBA32_C BITMASK(SIZ_RGBA32_C)
#define SIZ_RGBA32_A SIZ_RGBA32_C
#define MSK_RGBA32_A MSK_RGBA32_C
#define IDX_RGBA32_R ((SIZ_RGBA32_C * 3) + 0)
#define IDX_RGBA32_G ((SIZ_RGBA32_C * 2) + 0)
#define IDX_RGBA32_B ((SIZ_RGBA32_C * 1) + 0)
#define IDX_RGBA32_A ((SIZ_RGBA32_C * 0) + 0)
#define RGBA32_R(c)  (((c) >> IDX_RGBA32_R) & MSK_RGBA32_C)
#define RGBA32_G(c)  (((c) >> IDX_RGBA32_G) & MSK_RGBA32_C)
#define RGBA32_B(c)  (((c) >> IDX_RGBA32_B) & MSK_RGBA32_C)
#define RGBA32_A(c)  (((c) >> IDX_RGBA32_A) & MSK_RGBA32_C)
#define R_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_R)
#define G_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_G)
#define B_RGBA32(c)  (((c) & MSK_RGBA32_C) << IDX_RGBA32_B)
#define A_RGBA32(c)  (((c) & MSK_RGBA32_A) << IDX_RGBA32_A)

// CI4
// P 0000 0000 0000 0000 0000 0000 0000 1111 >> 0x00 & 0x0F = 0-15 * (255/15 = 17)              = 0..255
#define SIZ_CI4 0x4
#define MSK_CI4 BITMASK(SIZ_CI4)

// CI8
// P 0000 0000 0000 0000 0000 0000 1111 1111 >> 0x00 & 0xFF                                     = 0-255
#define SIZ_CI8 0x8
#define MSK_CI8 BITMASK(SIZ_CI8)


// -- Direct format conversion macros --


#define I4_TO_RGBA16_C(c)                               (((c) << (SIZ_RGBA16_C - SIZ_I4)) & MSK_RGBA16_C)
#define I8_TO_RGBA16_C(c)                               (((c) >> (SIZ_I8 - SIZ_RGBA16_C)) & MSK_RGBA16_C)

#define COMPOSITE_TO_COLOR(src, bitmask, index)         (((((src) >> (index)) & (bitmask)) * 255.0f) / (bitmask))
#define COLOR_TO_COMPOSITE(src, bitmask, index)         (((CompositeColor)(((src) * (bitmask)) / 255.0f) & (bitmask)) << (index))

#define COMPOSITE_TO_COLORF(src, bitmask, index)        ((ColorF)(((src) >> (index)) & (bitmask)) / (bitmask))
#define COLORF_TO_COMPOSITE(src, bitmask, index)        (((CompositeColor)((src) * (bitmask)) & (bitmask)) << (index))

#define COLORRGB0_TO_RGBA32(src)                        (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]))
#define COLORRGB1_TO_RGBA32(src)                        (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]) | MSK_RGBA32_A)
#define COLORRGBA_TO_RGBA32(src)                        (R_RGBA32((src)[0]) | G_RGBA32((src)[1]) | B_RGBA32((src)[2]) | A_RGBA32((src)[3]))

#define RGBA32_TO_COLORRGBA(src)                        { RGBA32_R(src), RGBA32_G(src), RGBA32_B(src), RGBA32_A(src) }

// GPACK_RGBA5551 but slightly faster in some cases.
#define RGBA_TO_RGBA16(r, g, b, a)                              \
    ((((r) >> (SIZ_RGBA32_C - SIZ_RGBA16_C)) << IDX_RGBA16_R) | \
     (((g) >> (SIZ_RGBA32_C - SIZ_RGBA16_C)) << IDX_RGBA16_G) | \
     (((b) >> (SIZ_RGBA32_C - SIZ_RGBA16_C)) << IDX_RGBA16_B) | \
     (((a) >> (SIZ_RGBA32_A - SIZ_RGBA16_A)) << IDX_RGBA16_A))

#define COLORRGBA_TO_RGBA16(src)                        RGBA_TO_RGBA16((src)[0], (src)[1], (src)[2], (src)[3])
#define RGBA32_TO_RGBA16(src)                           RGBA_TO_RGBA16(RGBA32_R(src), RGBA32_G(src), RGBA32_B(src), RGBA32_A(src))


// -- Color modification macros color --


#define RGBA32_SHADE(src, amt) (RGBA32)( \
    R_RGBA32(CLAMP((RGBA32_R((RGBA32)(src)) + (amt)), 0, MSK_RGBA32_A)) | \
    G_RGBA32(CLAMP((RGBA32_G((RGBA32)(src)) + (amt)), 0, MSK_RGBA32_A)) | \
    B_RGBA32(CLAMP((RGBA32_B((RGBA32)(src)) + (amt)), 0, MSK_RGBA32_A)) | \
    RGBA32_A((RGBA32)(src)) \
)

#define RGBA32_INVERT(src) (RGBA32)( \
    R_RGBA32(MSK_RGBA32_A - RGBA32_R((RGBA32)(src))) | \
    G_RGBA32(MSK_RGBA32_A - RGBA32_G((RGBA32)(src))) | \
    B_RGBA32(MSK_RGBA32_A - RGBA32_B((RGBA32)(src))) | \
    RGBA32_A((RGBA32)(src)) \
)

#define RGBA32_SET_ALPHA(src, alpha) (((src) & ~MSK_RGBA32_A) | A_RGBA32(alpha))


// -- Presets --


// I4 (I[0..15])                                              IIII
#define COLOR_I4_BLACK                                      0b0000 // 0000 | 0x0 |  0
#define COLOR_I4_GRAY                                       0b0111 // 0111 | 0x7 |  7
#define COLOR_I4_WHITE                                      0b1111 // 1111 | 0xF | 15


// I8 (I[0..255])                                         IIIIIIII
#define COLOR_I8_BLACK                                  0b00000000 // 0x00 // 0000 0000 |   0
#define COLOR_I8_GRAY                                   0b01111111 // 0x7F // 0111 1111 | 127
#define COLOR_I8_WHITE                                  0b11111111 // 0xFF // 1111 1111 | 255


// IA4 (I[0..7], A[0..1])                                     IIIA
#define COLOR_IA4_NONE                                      0b0000 // | 000 0 | 0x00 | 0 0
#define COLOR_IA4_BLACK                                     0b0000 // | 000 1 | 0x0F | 0 1
#define COLOR_IA4_GRAY                                      0b0000 // | 011 1 | 0x7F | 3 1
#define COLOR_IA4_WHITE                                     0b0000 // | 111 1 | 0xFF | 7 1


// IA8 (I[0..15], A[0..15])                               IIIIAAAA
#define COLOR_IA8_NONE                                  0b00000000 // 0000 0000 | 0x00 |  0  0
#define COLOR_IA8_BLACK                                 0b00001111 // 0000 1111 | 0x0F |  0 15
#define COLOR_IA8_GRAY                                  0b01111111 // 0111 1111 | 0x7F |  7 15
#define COLOR_IA8_WHITE                                 0b11111111 // 1111 1111 | 0xFF | 15 15


// IA16 (I[0..255], A[0..255])                    IIIIIIIIAAAAAAAA
#define COLOR_IA16_NONE                         0b0000000000000000 // 0000 0000 0000 0000 | 0x0000 |   0   0
#define COLOR_IA16_BLACK                        0b0000000011111111 // 0000 0000 1111 1111 | 0x00FF |   0 255
#define COLOR_IA16_GRAY                         0b0111111111111111 // 0111 1111 1111 1111 | 0x7FFF | 127 255
#define COLOR_IA16_WHITE                        0b1111111111111111 // 1111 1111 1111 1111 | 0xFFFF | 255 255


// Color RGB(A) Arrays:


// RGBA [0..255]
#define COLOR_RGBA32_NONE                       0x00000000 //   0   0   0   0
#define COLOR_RGBA32_BLACK                      0x000000FF //   0   0   0 255
#define COLOR_RGBA32_DARK_GRAY                  0x3F3F3FFF //  63  63  63 255
#define COLOR_RGBA32_GRAY                       0x7F7F7FFF // 127 127 127 255
#define COLOR_RGBA32_LIGHT_GRAY                 0xBFBFBFFF // 191 191 191 255
#define COLOR_RGBA32_VERY_LIGHT_GRAY            0xDFDFDFFF // 223 223 223 255
#define COLOR_RGBA32_WHITE                      0xFFFFFFFF // 255 255 255 255

// Primary/secondary/tertiary
#define COLOR_RGBA32_RED                        0xFF0000FF // 255   0   0 255
#define COLOR_RGBA32_ORANGE                     0xFF7F00FF // 255 127   0 255
#define COLOR_RGBA32_YELLOW                     0xFFFF00FF // 255 255   0 255
#define COLOR_RGBA32_LIME                       0x7FFF00FF // 127 255   0 255
#define COLOR_RGBA32_GREEN                      0x00FF00FF //   0 255   0 255
#define COLOR_RGBA32_SPRING                     0x00FF7FFF //   0 255 127 255
#define COLOR_RGBA32_CYAN                       0x00FFFFFF //   0 255 255 255
#define COLOR_RGBA32_SKY                        0x007FFFFF //   0 127 255 255
#define COLOR_RGBA32_BLUE                       0x0000FFFF //   0   0 255 255
#define COLOR_RGBA32_PURPLE                     0x7F00FFFF // 127   0 255 255
#define COLOR_RGBA32_MAGENTA                    0xFF00FFFF // 255   0 255 255
#define COLOR_RGBA32_PINK                       0xFF007FFF // 255   0 127 255

// Shades
#define COLOR_RGBA32_LIGHT_RED                  RGBA32_SHADE(COLOR_RGBA32_RED,    127) // 0xFF7F7FFF // 255 127 127 255
#define COLOR_RGBA32_VERY_LIGHT_RED             RGBA32_SHADE(COLOR_RGBA32_RED,    191) // 0xFFBFBFFF // 255 191 191 255
#define COLOR_RGBA32_LIGHT_YELLOW               RGBA32_SHADE(COLOR_RGBA32_YELLOW, 127) // 0xFFFF7FFF // 255 255 127 255
#define COLOR_RGBA32_VERY_LIGHT_YELLOW          RGBA32_SHADE(COLOR_RGBA32_YELLOW, 191) // 0xFFFFBFFF // 255 255 191 255
#define COLOR_RGBA32_LIGHT_GREEN                RGBA32_SHADE(COLOR_RGBA32_GREEN,  127) // 0x7FFF7FFF // 127 255 127 255
#define COLOR_RGBA32_VERY_LIGHT_GREEN           RGBA32_SHADE(COLOR_RGBA32_GREEN,  191) // 0xBFFFBFFF // 191 255 191 255
#define COLOR_RGBA32_LIGHT_CYAN                 RGBA32_SHADE(COLOR_RGBA32_CYAN,   127) // 0x7FFFFFFF // 127 255 255 255
#define COLOR_RGBA32_VERY_LIGHT_CYAN            RGBA32_SHADE(COLOR_RGBA32_CYAN,   191) // 0xBFFFFFFF // 191 255 255 255
#define COLOR_RGBA32_LIGHT_BLUE                 RGBA32_SHADE(COLOR_RGBA32_BLUE,   127) // 0x7F7FFFFF // 127 127 255 255
#define COLOR_RGBA32_VERY_LIGHT_BLUE            RGBA32_SHADE(COLOR_RGBA32_BLUE,   191) // 0xBFBFFFFF // 191 191 255 255

// Elemental
#define COLOR_RGBA32_JRB_SKY                    0x50645AFF //  80 100  90 255
#define COLOR_RGBA32_WATER                      0x055096FF //   5  80 150 255
#define COLOR_RGBA32_ICE                        0x7D9AD0FF // 125 154 208 255
#define COLOR_RGBA32_LAVA                       0x8F0600FF // 143   6   0 255
#define COLOR_RGBA32_SAND                       0xDCA973FF // 220 169 115 255
#define COLOR_RGBA32_ELECTRIC                   0xFFEE00FF // 255 238   0 255

// Lights
#define COLOR_RGBA32_AMP_LIGHT                  COLOR_RGBA32_ELECTRIC
#define COLOR_RGBA32_STAR_LIGHT                 0xFFF000FF // 255 240   0 255
#define COLOR_RGBA32_TRANSPARENT_STAR_LIGHT     0x0F1973FF //  15  25 115 255 // 0x1E32E6FF //  30  50 230 255
#define COLOR_RGBA32_RED_FLAME_LIGHT            0xFF3200C8 // 255  50   0 200
#define COLOR_RGBA32_BLUE_FLAME_LIGHT           0x6464FFFF // 100 100 255 255
#define COLOR_RGBA32_EXPLOSION_LIGHT            COLOR_RGBA32_LIGHT_YELLOW

// Coins
#define COLOR_RGBA32_COIN_YELLOW                COLOR_RGBA32_YELLOW
#define COLOR_RGBA32_COIN_BLUE                  0x8080FFFF // 128 128 255 255
#define COLOR_RGBA32_COIN_RED                   COLOR_RGBA32_RED

// Mario body lights - these should match the light groups in actors/mario/model.inc.c
#define COLOR_RGBA32_MARIO_LIGHTS_BLUE          0x0000FF00 //   0   0 255   0
#define COLOR_RGBA32_MARIO_LIGHTS_RED           0xFF000000 // 255   0   0   0
#define COLOR_RGBA32_MARIO_LIGHTS_WHITE         0xFFFFFF00 // 255 255 255   0
#define COLOR_RGBA32_MARIO_LIGHTS_BROWN1        0x721C0E00 // 114  28  14   0
#define COLOR_RGBA32_MARIO_LIGHTS_BEIGE         0xFEC17900 // 254 193 121   0
#define COLOR_RGBA32_MARIO_LIGHTS_BROWN2        0x73060000 // 115   6   0   0

// Debug box
#define COLOR_RGBA32_DEBUG_DEFAULT              0x00FF0000 //   0 255   0   0
#define COLOR_RGBA32_DEBUG_ALPHA                0x0000007F //   0   0   0 127
#define COLOR_RGBA32_DEBUG_POSITION             0xFFFFFF80 // 255 255 255 128
#define COLOR_RGBA32_DEBUG_HITBOX               0x0000FF80 //   0   0 255 128
#define COLOR_RGBA32_DEBUG_HURTBOX              0xF000008F // 240   0   0 143
#define COLOR_RGBA32_DEBUG_WARP                 0xFFA50080 // 255 165   0 128
#define COLOR_RGBA32_DEBUG_PUPPYVOLUME          0x00FF0000 //   0 255   0   0
#define COLOR_RGBA32_DEBUG_LIGHT                0xFF00FF08 // 255   0 255   8

// VSCode default theme colors
#define COLOR_RGBA32_VSC_COMMENT                0x6A9955FF // 106 153  85 255
#define COLOR_RGBA32_VSC_FUNCTION               0xDCDCAAFF // 220 220 170 255
#define COLOR_RGBA32_VSC_VARIABLE               0x9CDCFEFF // 156 220 254 255
#define COLOR_RGBA32_VSC_DEFINE                 0x569CD6FF //  86 156 214 255
#define COLOR_RGBA32_VSC_ENUM                   0x4FC1FFFF //  79 193 255 255
#define COLOR_RGBA32_VSC_TYPEDEF                0x4EC9B0FF //  78 201 176 255
#define COLOR_RGBA32_VSC_NUMBER                 0xB5CEA8FF // 181 206 168 255
#define COLOR_RGBA32_VSC_STRING                 0xCE9178FF // 206 145 120 255
#define COLOR_RGBA32_VSC_ERROR                  0xF44747FF // 244  71  71 255

// Crash screen
// UI:
#define COLOR_RGBA32_CRASH_BACKGROUND           RGBA32_SET_ALPHA(COLOR_RGBA32_NONE, 0xBF) // 0x000000BF //   0   0   0 191
#define COLOR_RGBA32_CRASH_AT                   COLOR_RGBA32_LIGHT_RED
#define COLOR_RGBA32_CRASH_CONTROLS_DESCRIPTION COLOR_RGBA32_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_HEADER               COLOR_RGBA32_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_PAGE_NAME            COLOR_RGBA32_ORANGE
#define COLOR_RGBA32_CRASH_DIVIDER              COLOR_RGBA32_LIGHT_GRAY
// Generic:
#define COLOR_RGBA32_CRASH_OUT_OF_BOUNDS        COLOR_RGBA32_RED
#define COLOR_RGBA32_CRASH_NULL_CHAR            COLOR_RGBA32_GRAY
// Symbols:
#define COLOR_RGBA32_CRASH_UNKNOWN              COLOR_RGBA32_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_VARIABLE             COLOR_RGBA32_VSC_VARIABLE
#define COLOR_RGBA32_CRASH_FUNCTION_NAME        COLOR_RGBA32_LIGHT_YELLOW
#define COLOR_RGBA32_CRASH_OFFSET               COLOR_RGBA32_VSC_FUNCTION
// Select:
#define COLOR_RGBA32_CRASH_NO                   COLOR_RGBA32_LIGHT_RED
#define COLOR_RGBA32_CRASH_YES                  COLOR_RGBA32_LIGHT_GREEN
#define COLOR_RGBA32_CRASH_PC_HIGHLIGHT         RGBA32_SET_ALPHA(COLOR_RGBA32_RED,  0x7F) // 0xFF00007F // 255   0   0 127
#define COLOR_RGBA32_CRASH_SELECT_HIGHLIGHT     RGBA32_SET_ALPHA(COLOR_RGBA32_GRAY, 0xBF) // 0x7F7F7FBF // 127 127 127 191
#define COLOR_RGBA32_CRASH_SELECT_ARROW         COLOR_RGBA32_ORANGE
// Context:
#define COLOR_RGBA32_CRASH_DESCRIPTION          COLOR_RGBA32_VSC_STRING
#define COLOR_RGBA32_CRASH_THREAD               COLOR_RGBA32_LIGHT_BLUE
// Assert:
#define COLOR_RGBA32_CRASH_FILE_NAME            COLOR_RGBA32_SKY
// Map:
#define COLOR_RGBA32_CRASH_MAP_SYMBOL_TYPE      COLOR_RGBA32_GRAY
// Memory:
#define COLOR_RGBA32_CRASH_MEMORY_COL1          0x00B7FFFF //   0 191 255 255
#define COLOR_RGBA32_CRASH_MEMORY_COL2          0x0087FFFF //   0 135 255 255
#define COLOR_RGBA32_CRASH_MEMORY_ROW1          0x5FDF5FFF //  95 223  95 255
#define COLOR_RGBA32_CRASH_MEMORY_ROW2          0x1F9F1FFF //  31 159  31 255
#define COLOR_RGBA32_CRASH_MEMORY_DATA1         COLOR_RGBA32_WHITE
#define COLOR_RGBA32_CRASH_MEMORY_DATA2         COLOR_RGBA32_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_MEMORY_SELECT        COLOR_RGBA32_WHITE
#define COLOR_RGBA32_CRASH_MEMORY_PC            COLOR_RGBA32_RED
// Disasm:
#define COLOR_RGBA32_CRASH_DISASM_INSN          COLOR_RGBA32_VSC_FUNCTION
#define COLOR_RGBA32_CRASH_DISASM_NOP           COLOR_RGBA32_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_DISASM_IMMEDIATE     COLOR_RGBA32_LIGHT_GREEN
// Settings:
#define COLOR_RGBA32_CRASH_SETTINGS_DESCRIPTION COLOR_RGBA32_VERY_LIGHT_GRAY
#define COLOR_RGBA32_CRASH_SETTINGS_NAMED       COLOR_RGBA32_VSC_ENUM
#define COLOR_RGBA32_CRASH_SETTINGS_NUMERIC     COLOR_RGBA32_VSC_NUMBER
// Unused:
#define COLOR_RGBA32_CRASH_ERROR                0x5F4747FF //  95  71  71 255


// RGB [0..255]

// Grayscale
#define COLOR_RGB_BLACK                         { 0x00, 0x00, 0x00 } //   0   0   0
#define COLOR_RGB_DARK_GRAY                     { 0x3F, 0x3F, 0x3F } //  63  63  63
#define COLOR_RGB_GRAY                          { 0x7F, 0x7F, 0x7F } // 127 127 127
#define COLOR_RGB_LIGHT                         { 0xBF, 0xBF, 0xBF } // 191 191 191
#define COLOR_RGB_WHITE                         { 0xFF, 0xFF, 0xFF } // 255 255 255

// Primary/secondary/tertiary
#define COLOR_RGB_RED                           { 0xFF, 0x00, 0x00 } // 255   0   0
#define COLOR_RGB_ORANGE                        { 0xFF, 0x7F, 0x00 } // 255 127   0
#define COLOR_RGB_YELLOW                        { 0xFF, 0xFF, 0x00 } // 255 255   0
#define COLOR_RGB_LIME                          { 0x7F, 0xFF, 0x00 } // 127 255   0
#define COLOR_RGB_GREEN                         { 0x00, 0xFF, 0x00 } //   0 255   0
#define COLOR_RGB_SPRING                        { 0x00, 0xFF, 0x7F } //   0 255 127
#define COLOR_RGB_CYAN                          { 0x00, 0xFF, 0xFF } //   0 255 255
#define COLOR_RGB_SKY                           { 0x00, 0x7F, 0xFF } //   0 127 255
#define COLOR_RGB_BLUE                          { 0x00, 0x00, 0xFF } //   0   0 255
#define COLOR_RGB_PURPLE                        { 0x7F, 0x00, 0xFF } // 127   0 255
#define COLOR_RGB_MAGENTA                       { 0xFF, 0x00, 0xFF } // 255   0 255
#define COLOR_RGB_PINK                          { 0xFF, 0x00, 0x7F } // 255   0 127

// Elemental
#define COLOR_RGB_JRB_SKY                       { 0x50, 0x64, 0x5A } //  80 100  90
#define COLOR_RGB_WATER                         { 0x05, 0x50, 0x96 } //   5  80 150
#define COLOR_RGB_ICE                           { 0x7D, 0x9A, 0xD0 } // 125 154 208
#define COLOR_RGB_LAVA                          { 0x8F, 0x06, 0x00 } // 143   6   0
#define COLOR_RGB_SAND                          { 0xDC, 0xA9, 0x73 } // 220 169 115
#define COLOR_RGB_ELECTRIC                      { 0xFF, 0xEE, 0x00 } // 255 238   0

#define COLOR_RGB_COIN_YELLOW                   COLOR_RGB_YELLOW
#define COLOR_RGB_COIN_BLUE                     { 0x80, 0x80, 0xFF } // 128 128 255
#define COLOR_RGB_COIN_RED                      COLOR_RGB_RED

// Other:

// Lights defaults
#define DEFAULT_LIGHT_AMB                       0x7F
#define DEFAULT_LIGHT_COL                       0xFE
#define DEFAULT_LIGHT_DIR                       0x28

// RGBF [0..1]
#define COLOR_RGBF_WHITE                        { 1.0f, 1.0f, 1.0f } // 0xFF 0xFF 0xFF | 255 255 255
#define COLOR_RGBF_RED                          { 1.0f, 0.0f, 0.0f } // 0xFF 0x00 0x00 | 255   0   0
#define COLOR_RGBF_GREEN                        { 0.0f, 1.0f, 0.0f } // 0x00 0xFF 0x00 |   0 255   0
#define COLOR_RGBF_BLUE                         { 0.0f, 0.0f, 1.0f } // 0x00 0x00 0xFF |   0   0 255
#define COLOR_RGBF_ERR_DARK_BLUE                { 0.0f, 0.0f, 6.0f } // 0x00 0x00 0x5FF? |   0   0 1535?
#define COLOR_RGBF_PINK                         { 1.0f, 0.0f, 1.0f } // 0xFF 0x00 0xFF | 255   0 255
#define COLOR_RGBF_BLACK                        { 0.0f, 0.0f, 0.0f } // 0x00 0x00 0x00 |   0   0   0
#define COLOR_RGBF_GREY                         { 0.6f, 0.6f, 0.6f } // 0x99 0x99 0x99 | 153 153 153
#define COLOR_RGBF_DARK_GREY                    { 0.4f, 0.4f, 0.4f } // 0x66 0x66 0x66 | 102 102 102
#define COLOR_RGBF_YELLOW                       { 1.0f, 1.0f, 1.0f } // 0xFF 0xFF 0xFF | 255 255 255


// RGBA32 (RGB[0..255], A[0..255])

// Grayscale
#define COLOR_RGBA_NONE                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_NONE)
#define COLOR_RGBA_BLACK                        RGBA32_TO_COLORRGBA(COLOR_RGBA32_BLACK)
#define COLOR_RGBA_DARK_GRAY                    RGBA32_TO_COLORRGBA(COLOR_RGBA32_DARK_GRAY)
#define COLOR_RGBA_GRAY                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_GRAY)
#define COLOR_RGBA_LIGHT_GRAY                   RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_GRAY)
#define COLOR_RGBA_WHITE                        RGBA32_TO_COLORRGBA(COLOR_RGBA32_WHITE)

// Primary/secondary/tertiary
#define COLOR_RGBA_RED                          RGBA32_TO_COLORRGBA(COLOR_RGBA32_RED)
#define COLOR_RGBA_ORANGE                       RGBA32_TO_COLORRGBA(COLOR_RGBA32_ORANGE)
#define COLOR_RGBA_YELLOW                       RGBA32_TO_COLORRGBA(COLOR_RGBA32_YELLOW)
#define COLOR_RGBA_LIME                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIME)
#define COLOR_RGBA_GREEN                        RGBA32_TO_COLORRGBA(COLOR_RGBA32_GREEN)
#define COLOR_RGBA_SPRING                       RGBA32_TO_COLORRGBA(COLOR_RGBA32_SPRING)
#define COLOR_RGBA_CYAN                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_CYAN)
#define COLOR_RGBA_SKY                          RGBA32_TO_COLORRGBA(COLOR_RGBA32_SKY)
#define COLOR_RGBA_BLUE                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_BLUE)
#define COLOR_RGBA_PURPLE                       RGBA32_TO_COLORRGBA(COLOR_RGBA32_PURPLE)
#define COLOR_RGBA_MAGENTA                      RGBA32_TO_COLORRGBA(COLOR_RGBA32_MAGENTA)
#define COLOR_RGBA_PINK                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_PINK)

// Shades
#define COLOR_RGBA_LIGHT_RED                    RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_RED)
#define COLOR_RGBA_VERY_LIGHT_RED               RGBA32_TO_COLORRGBA(COLOR_RGBA32_VERY_LIGHT_RED)
#define COLOR_RGBA_LIGHT_YELLOW                 RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_YELLOW)
#define COLOR_RGBA_VERY_LIGHT_YELLOW            RGBA32_TO_COLORRGBA(COLOR_RGBA32_VERY_LIGHT_YELLOW)
#define COLOR_RGBA_LIGHT_GREEN                  RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_GREEN)
#define COLOR_RGBA_VERY_LIGHT_GREEN             RGBA32_TO_COLORRGBA(COLOR_RGBA32_VERY_LIGHT_GREEN)
#define COLOR_RGBA_LIGHT_CYAN                   RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_CYAN)
#define COLOR_RGBA_VERY_LIGHT_CYAN              RGBA32_TO_COLORRGBA(COLOR_RGBA32_VERY_LIGHT_CYAN)
#define COLOR_RGBA_LIGHT_BLUE                   RGBA32_TO_COLORRGBA(COLOR_RGBA32_LIGHT_BLUE)
#define COLOR_RGBA_VERY_LIGHT_BLUE              RGBA32_TO_COLORRGBA(COLOR_RGBA32_VERY_LIGHT_BLUE)

// Elemental
#define COLOR_RGBA_JRB_SKY                      RGBA32_TO_COLORRGBA(COLOR_RGBA32_JRB_SKY)
#define COLOR_RGBA_WATER                        RGBA32_TO_COLORRGBA(COLOR_RGBA32_WATER)
#define COLOR_RGBA_ICE                          RGBA32_TO_COLORRGBA(COLOR_RGBA32_ICE)
#define COLOR_RGBA_LAVA                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_LAVA)
#define COLOR_RGBA_SAND                         RGBA32_TO_COLORRGBA(COLOR_RGBA32_SAND)
#define COLOR_RGBA_ELECTRIC                     RGBA32_TO_COLORRGBA(COLOR_RGBA32_ELECTRIC)

// Lights
#define COLOR_RGBA_AMP_LIGHT                    RGBA32_TO_COLORRGBA(COLOR_RGBA32_AMP_LIGHT)
#define COLOR_RGBA_STAR_LIGHT                   RGBA32_TO_COLORRGBA(COLOR_RGBA32_STAR_LIGHT)
#define COLOR_RGBA_TRANSPARENT_STAR_LIGHT       RGBA32_TO_COLORRGBA(COLOR_RGBA32_TRANSPARENT_STAR_LIGHT)
#define COLOR_RGBA_RED_FLAME_LIGHT              RGBA32_TO_COLORRGBA(COLOR_RGBA32_RED_FLAME_LIGHT)
#define COLOR_RGBA_BLUE_FLAME_LIGHT             RGBA32_TO_COLORRGBA(COLOR_RGBA32_BLUE_FLAME_LIGHT)
#define COLOR_RGBA_EXPLOSION_LIGHT              RGBA32_TO_COLORRGBA(COLOR_RGBA32_EXPLOSION_LIGHT)

// Coins
#define COLOR_RGBA_COIN_YELLOW_LIGHT            RGBA32_TO_COLORRGBA(COLOR_RGBA32_COIN_YELLOW_LIGHT)
#define COLOR_RGBA_COIN_BLUE_LIGHT              RGBA32_TO_COLORRGBA(COLOR_RGBA32_COIN_BLUE_LIGHT)
#define COLOR_RGBA_COIN_RED_LIGHT               RGBA32_TO_COLORRGBA(COLOR_RGBA32_COIN_RED_LIGHT)

// Mario body lights - these should match the light groups in actors/mario/model.inc.c
#define COLOR_RGBA_MARIO_LIGHTS_BLUE            RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_BLUE)
#define COLOR_RGBA_MARIO_LIGHTS_RED             RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_RED)
#define COLOR_RGBA_MARIO_LIGHTS_WHITE           RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_WHITE)
#define COLOR_RGBA_MARIO_LIGHTS_BROWN1          RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_BROWN1)
#define COLOR_RGBA_MARIO_LIGHTS_BEIGE           RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_BEIGE)
#define COLOR_RGBA_MARIO_LIGHTS_BROWN2          RGBA32_TO_COLORRGBA(COLOR_RGBA32_MARIO_LIGHTS_BROWN2)

// Debug box
#define COLOR_RGBA_DEBUG_DEFAULT                RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_DEFAULT)
#define COLOR_RGBA_DEBUG_ALPHA                  RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_ALPHA)
#define COLOR_RGBA_DEBUG_POSITION               RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_POSITION)
#define COLOR_RGBA_DEBUG_HITBOX                 RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_HITBOX)
#define COLOR_RGBA_DEBUG_HURTBOX                RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_HURTBOX)
#define COLOR_RGBA_DEBUG_WARP                   RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_WARP)
#define COLOR_RGBA_DEBUG_PUPPYVOLUME            RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_PUPPYVOLUME)
#define COLOR_RGBA_DEBUG_LIGHT                  RGBA32_TO_COLORRGBA(COLOR_RGBA32_DEBUG_LIGHT)


// RGBA16 (RGB[0..31], A[0..1])

// Grayscale Colors
//                                                                                             RRRRRGGGGGBBBBBA | RRRR RGGG GGBB BBBA | RRRRR GGGGG BBBBB A | 0x0000 |  R  G  B  A
#define COLOR_RGBA16_NONE                       RGBA32_TO_RGBA16(COLOR_RGBA16_NONE      ) // 0b0000000000000000 | 0000 0000 0000 0000 | 00000 00000 00000 0 | 0x0000 |  0  0  0  0
#define COLOR_RGBA16_BLACK                      RGBA32_TO_RGBA16(COLOR_RGBA16_BLACK     ) // 0b0000000000000001 | 0000 0000 0000 0001 | 00000 00000 00000 1 | 0x0001 |  0  0  0  1
#define COLOR_RGBA16_DARK_GRAY                  RGBA32_TO_RGBA16(COLOR_RGBA16_DARK_GRAY ) // 0b0011100111001111 | 0011 1001 1100 1111 | 00111 00111 00111 1 | 0x39CF |  7  7  7  1
#define COLOR_RGBA16_GRAY                       RGBA32_TO_RGBA16(COLOR_RGBA16_GRAY      ) // 0b0111101111011111 | 0111 1011 1101 1111 | 01111 01111 01111 1 | 0x7BDF | 15 15 15  1
#define COLOR_RGBA16_LIGHT_GRAY                 RGBA32_TO_RGBA16(COLOR_RGBA16_LIGHT_GRAY) // 0b1011110111101111 | 1011 1101 1110 1111 | 10111 10111 10111 1 | 0xBDEF | 23 23 23  1
#define COLOR_RGBA16_WHITE                      RGBA32_TO_RGBA16(COLOR_RGBA16_WHITE     ) // 0b1111111111111111 | 1111 1111 1111 1111 | 11111 11111 11111 1 | 0xFFFF | 31 31 31  1
