#ifndef __FASTTEXT_H__
#define __FASTTEXT_H__

void drawSmallString_impl(Gfx**, int, int, const char*, int, int , int);

static inline void drawSmallString(Gfx **dl, int x, int y, const char* string) {
    drawSmallString_impl(dl, x, y, string, 255, 255, 255);
}

static inline void drawSmallStringCol(Gfx **dl, int x, int y, const char* string, int r, int g, int b) {
    drawSmallString_impl(dl, x, y, string, r, g, b);
}

static inline void drawSmallStringDL(int x, int y, const char* string) {
    Gfx* dlHead = gDisplayListHead;
    gDPPipeSync(dlHead++);
    gDPSetCycleType(dlHead++, G_CYC_1CYCLE);
    gDPSetRenderMode(dlHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
    gDPSetTexturePersp(dlHead++, G_TP_NONE);
    gDPSetTextureFilter(dlHead++, G_TF_POINT);
    gDPSetTextureLUT(dlHead++, G_TT_NONE);
    drawSmallString(&dlHead, x, y, string);
    gDisplayListHead = dlHead;
}

static inline void drawSmallStringColDL(int x, int y, const char* string, int r, int g, int b) {
    Gfx* dlHead = gDisplayListHead;
    gDPPipeSync(dlHead++);
    gDPSetCycleType(dlHead++, G_CYC_1CYCLE);
    gDPSetRenderMode(dlHead++, G_RM_TEX_EDGE, G_RM_TEX_EDGE2);
    gDPSetTexturePersp(dlHead++, G_TP_NONE);
    gDPSetTextureFilter(dlHead++, G_TF_POINT);
    gDPSetTextureLUT(dlHead++, G_TT_NONE);
    drawSmallStringCol(&dlHead, x, y, string, r, g, b);
    gDisplayListHead = dlHead;
}

#define drawSmallStringPrintf(x, y, string, ...) {  \
    char _text_buffer[sizeof(string)];              \
    sprintf(_text_buffer, string, __VA_ARGS__);     \
    drawSmallStringDL(x, y, _text_buffer);          \
}

#define drawSmallStringColPrintf(x, y, r, g, b, string, ...) {  \
    char _text_buffer[sizeof(string)];                          \
    sprintf(_text_buffer, string, __VA_ARGS__);                 \
    drawSmallStringColDL(x, y, _text_buffer, r, g, b);          \
}

#endif
