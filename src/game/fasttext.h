#ifndef __FASTTEXT_H__
#define __FASTTEXT_H__

extern const Gfx dl_fasttext_begin[];
extern const Gfx dl_fasttext_end[];

void drawSmallString_impl(Gfx**, int, int, const char*, int, int , int);

static inline void drawSmallString(Gfx **dl, int x, int y, const char* string) {
    drawSmallString_impl(dl, x, y, string, 255, 255, 255);
}

static inline void drawSmallStringCol(Gfx **dl, int x, int y, const char* string, int r, int g, int b) {
    drawSmallString_impl(dl, x, y, string, r, g, b);
}

static inline void drawSmallStringDL(int x, int y, const char* string) {
    Gfx* dlHead = gDisplayListHead;
    gSPDisplayList(dlHead++, dl_fasttext_begin);
    drawSmallString(&dlHead, x, y, string);
    gSPDisplayList(dlHead++, dl_fasttext_end);
    gDisplayListHead = dlHead;
}

static inline void drawSmallStringColDL(int x, int y, const char* string, int r, int g, int b) {
    Gfx* dlHead = gDisplayListHead;
    gSPDisplayList(dlHead++, dl_fasttext_begin);
    drawSmallStringCol(&dlHead, x, y, string, r, g, b);
    gSPDisplayList(dlHead++, dl_fasttext_end);
    gDisplayListHead = dlHead;
}

#endif
