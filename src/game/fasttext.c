// This file is a modification of a file from https://github.com/danbolt/n64-jam-1, which was licensed under the MPL-2.0 License
// See the original repo for more details.

#include <ultra64.h>

#define TEX_ASCII_START '!'

#define G_CC_TEXT PRIMITIVE, 0, TEXEL0, 0, 0, 0, 0, TEXEL0

__asm__(
 ".section \".rodata\", \"a\", @progbits\n"
 ".global fastfont\n"
 "fastfont:\n"
 ".incbin \"src/game/newfont2_swapped.bin\"\n"
 ".previous\n"
);

extern u8 fastfont[];

// Naive strlen copied from
// https://stackoverflow.com/questions/22520413/c-strlen-implementation-in-one-line-of-code
void my_strlen(const char *str, u32 *len)
{
    for (*len = 0; str[*len]; (*len)++);
}

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

void drawSmallString_impl(Gfx **dl, int x, int y, const char* string, int r, int g, int b) {
  int i = 0;
  int xPos = x;
  int yPos = y;
  int s = 0;
  Gfx *dlHead = *dl;

  r = g = b = 255;
  gDPLoadTextureBlock_4bS(dlHead++, fastfont, G_IM_FMT_IA, 672, 12, 0, G_TX_MIRROR | G_TX_WRAP, G_TX_MIRROR | G_TX_WRAP, G_TX_NOMASK, G_TX_NOMASK, G_TX_NOLOD, G_TX_NOLOD);
  gDPSetPrimColor(dlHead++, 0, 0, r, g, b, 255);
  gDPSetCombineMode(dlHead++, G_CC_TEXT, G_CC_TEXT);
  gDPPipeSync(dlHead++);
  while (string[i] != '\0') {
    if (string[i] == '\n') {
      xPos = x;
      yPos += 12;
      i++;
      continue;
    }

    if (string[i] != ' ') {
      s = computeS(string[i]);
      gSPTextureRectangle(dlHead++, (xPos + 0) << 2, (yPos + 0) << 2, (xPos + 8) << 2, (yPos + 12) << 2, 0, s << 5, 0, 1 << 10, 1 << 10);
    }

    xPos += 8;
    i++;
  }
  gDPSetPrimColor(dlHead++, 0, 0, 255, 255, 255, 255);
  gDPPipeSync(dlHead++);

  *dl = dlHead;
}
