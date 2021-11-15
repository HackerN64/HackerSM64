#include <ultra64.h>
#include <PR/gs2dex.h>
#include <PR/gu.h>
// #include "stack.h"
typedef float Mat4[4][4];

extern void mat2_dst_mul(uObjMtx *dst, uObjMtx *m1, uObjMtx *m2);

extern void mat2_dst_add(uObjMtx *dst, uObjMtx *m1, uObjMtx *m2);

extern void mat2_ident(uObjMtx *dst, float scale);

extern void mat2_mul(uObjMtx *m1, uObjMtx *m2);

extern void mat2_add(uObjMtx *m1, uObjMtx *m2);

extern void mat2_scale(uObjMtx *dst, int scale);

extern void get_final_mat(uObjMtx *dst);

extern void mat2_translate(uObjMtx *m, int x, int y);

extern void mat2_rotate(uObjMtx *dst, f32 degrees);

extern void mat2_translate_vec(uObjMtx *m, f32 degrees, f32 mag);
