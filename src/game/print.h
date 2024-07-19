#ifndef PRINT_H
#define PRINT_H

#include <PR/ultratypes.h>

void print_text_fmt_int(s32 x, s32 y, char *str, s32 n);
void print_text(s32 x, s32 y, char *str);
void print_text_aligned(s32 x, s32 y, char *str, u32 alignment);
void render_text_labels(void);

#endif // PRINT_H
