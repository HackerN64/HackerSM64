// This file is a modification of a file from https://github.com/danbolt/n64-jam-1, which was licensed under the MPL-2.0 License
// See the original repo for more details.

#include <ultra64.h>
#include "macros.h"

#define TEX_ASCII_START '!'
#define TAB_WIDTH 16

#define G_CC_TEXT PRIMITIVE, 0, TEXEL0, 0, 0, 0, 0, TEXEL0

ALIGNED8 static const u8 fast_font[] = {
#include "textures/fasttext/newfont2.ia4.preswap.inc.c"
};

int computeS(unsigned char letter) {
    int idx = letter;  
    if (letter > 'z') {
        idx -= (3 + 2 + 3 + 1 + 3);
    } else if (letter > '^') {
        idx -= (2 + 3 + 1 + 3);
    } else if (letter > 'Z') {
        idx -= (3 + 1 + 3);
    } else if (letter > '?') {
        idx -= (1 + 3);
    } else if (letter > ';') {
        idx -= (3);
    }

    return (idx - TEX_ASCII_START) * 8;
}

static const u8 fast_text_font_kerning[] = {
    /* */ 2, /*!*/ 4, /*"*/ 5, /*#*/ 0, /*$*/ 0, /*%*/ 7, /*&*/ 7, /*'*/ 2, /*(*/ 5, /*)*/ 5, /***/ 0, /*+*/ 7, /*,*/ 3, /*-*/ 7, /*.*/ 3, /*/*/ 7,
    /*0*/ 7, /*1*/ 7, /*2*/ 7, /*3*/ 7, /*4*/ 7, /*5*/ 7, /*6*/ 7, /*7*/ 7, /*8*/ 7, /*9*/ 7, /*:*/ 3, /*;*/ 3, /*<*/ 0, /*=*/ 0, /*>*/ 0, /*?*/ 7,
    /*@*/ 0, /*A*/ 7, /*B*/ 7, /*C*/ 7, /*D*/ 7, /*E*/ 7, /*F*/ 7, /*G*/ 7, /*H*/ 7, /*I*/ 6, /*J*/ 7, /*K*/ 7, /*L*/ 7, /*M*/ 7, /*N*/ 7, /*O*/ 7,
    /*P*/ 7, /*Q*/ 7, /*R*/ 7, /*S*/ 7, /*T*/ 7, /*U*/ 7, /*V*/ 7, /*W*/ 7, /*X*/ 7, /*Y*/ 7, /*Z*/ 7, /*[*/ 0, /*\*/ 0, /*]*/ 0, /*^*/ 6, /*_*/ 0,
    /*`*/ 0, /*a*/ 6, /*b*/ 6, /*c*/ 6, /*d*/ 6, /*e*/ 6, /*f*/ 6, /*g*/ 6, /*h*/ 6, /*i*/ 2, /*j*/ 6, /*k*/ 5, /*l*/ 3, /*m*/ 6, /*n*/ 6, /*o*/ 6,
    /*p*/ 6, /*q*/ 6, /*r*/ 6, /*s*/ 6, /*t*/ 6, /*u*/ 6, /*v*/ 6, /*w*/ 6, /*x*/ 6, /*y*/ 6, /*z*/ 6, /*{*/ 0, /*|*/ 0, /*}*/ 0, /*~*/ 7,     
};

void drawSmallString_impl(Gfx **dl, int x, int y, const char* string, int r, int g, int b) {
    int i = 0;
    int xPos = x;
    int yPos = y;
    int s = 0;
    Gfx *dlHead = *dl;

    gDPLoadTextureBlock_4bS(dlHead++, fast_font, G_IM_FMT_IA, 672, 12, 0, G_TX_MIRROR | G_TX_WRAP, G_TX_MIRROR | G_TX_WRAP, G_TX_NOMASK, G_TX_NOMASK, G_TX_NOLOD, G_TX_NOLOD);
    gDPSetPrimColor(dlHead++, 0, 0, r, g, b, 255);
    gDPSetCombineMode(dlHead++, G_CC_TEXT, G_CC_TEXT);
    gDPPipeSync(dlHead++);

    while (string[i] != '\0') {
        unsigned int cur_char = string[i];
        s32 goddamnJMeasure;

        if (cur_char == '\n') {
            xPos = x;
            yPos += 12;
            i++;
            continue;
        }

        if (cur_char == '\t') {
            int xDist = xPos - x + 1;
            int tabCount = (xDist + TAB_WIDTH - 1) / TAB_WIDTH;
            xPos = tabCount * TAB_WIDTH + x;
        } else {
            if (cur_char != ' ') {
                s = computeS(cur_char);
                goddamnJMeasure = (s == 512) ? 1 : 0;
                gSPTextureRectangle(dlHead++, (xPos + 0) << 2, (yPos + 0) << 2, (xPos + 8) << 2, (yPos + 12) << 2, 0, (s << 5) + goddamnJMeasure, 0, 1 << 10, 1 << 10);
            }
            xPos += fast_text_font_kerning[cur_char - ' '];
        }

        i++;
    }
    gDPSetPrimColor(dlHead++, 0, 0, 255, 255, 255, 255);
    gDPPipeSync(dlHead++);

    *dl = dlHead;
}
