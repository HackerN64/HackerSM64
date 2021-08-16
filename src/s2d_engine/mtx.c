#include <ultra64.h>
#include <PR/gs2dex.h>
#include <PR/gu.h>

#include "config.h"

#define ftoq FTOFIX32
#define qtof FIX32TOF

#define qu510(n)                      ((u16)((n)*0x0400))

void mat2_dst_mul(uObjMtx *dst, uObjMtx *m1, uObjMtx *m2) {
	dst->m.A = ftoq((qtof(m1->m.A) * qtof(m2->m.A)) + (qtof(m1->m.B) * qtof(m2->m.C)));
	dst->m.B = ftoq((qtof(m1->m.A) * qtof(m2->m.B)) + (qtof(m1->m.B) * qtof(m2->m.D)));
	dst->m.C = ftoq((qtof(m1->m.C) * qtof(m2->m.A)) + (qtof(m1->m.D) * qtof(m2->m.C)));
	dst->m.D = ftoq((qtof(m1->m.C) * qtof(m2->m.B)) + (qtof(m1->m.D) * qtof(m2->m.D)));
}

void mat2_dst_add(uObjMtx *dst, uObjMtx *m1, uObjMtx *m2) {
	dst->m.A = (m1->m.A + m2->m.A);
	dst->m.B = (m1->m.B + m2->m.B);
	dst->m.C = (m1->m.C + m2->m.C);
	dst->m.D = (m1->m.D + m2->m.D);
}

void mat2_copy(uObjMtx *dst, uObjMtx *src) {
	dst->m.A = src->m.A;
	dst->m.B = src->m.B;
	dst->m.C = src->m.C;
	dst->m.D = src->m.D;
	dst->m.X = src->m.X;
	dst->m.Y = src->m.Y;
	dst->m.BaseScaleX = src->m.BaseScaleX;
	dst->m.BaseScaleY = src->m.BaseScaleY;
}

void mat2_ident(uObjMtx *dst, float scale) {
	dst->m.A = FTOFIX32(scale);
	dst->m.D = FTOFIX32(scale);
	dst->m.B = 0;
	dst->m.C = 0;

	dst->m.X = 0;
	dst->m.Y = 0;

	dst->m.BaseScaleX = qu510(scale);
	dst->m.BaseScaleY = qu510(scale);
}
// cos -sin sin cos
void mat2_rotate(uObjMtx *dst, f32 degrees) {
	dst->m.A = ftoq(cosf(degrees));
	dst->m.B = ftoq(sinf(degrees));
	dst->m.C = ftoq(-sinf(degrees));
	dst->m.D = ftoq(cosf(degrees));
}

void mat2_mul(uObjMtx *m1, uObjMtx *m2) {
	mat2_dst_mul(m1, m1, m2);
}

void mat2_add(uObjMtx *m1, uObjMtx *m2) {
	mat2_dst_mul(m1, m1, m2);
}

void mat2_scale(uObjMtx *dst, int scale) {
	dst->m.A *= scale;
	dst->m.D *= scale;

	dst->m.BaseScaleX *= scale;
	dst->m.BaseScaleY *= scale;
}

#define	FTOFIX16(x)	(long)((x) * (float)(1 << 2))
#define	FIX16TOF(x)	((float)(x) * (1.0f / (float)(1 << 2)))
void mat2_translate(uObjMtx *m, int x, int y) {
	m->m.X = x<<2;
	m->m.Y = y<<2;
}

void mat2_translate_vec(uObjMtx *m, f32 degrees, f32 mag) {
	m->m.X += FTOFIX16(mag * cosf(degrees));
	m->m.Y += FTOFIX16(mag * sinf(degrees));
}

typedef float Mat4[4][4];
void gu_to_gs2dex(uObjMtx *m1, Mat4 m2) {
	m1->m.A = m1->m.BaseScaleX = FTOFIX32(m2[0][0]);
	m1->m.B = FTOFIX32(m2[0][1]);
	m1->m.C = FTOFIX32(m2[1][0]);
	m1->m.D = m1->m.BaseScaleY = FTOFIX32(m2[1][1]);

	m1->m.X = FTOFIX16(m2[3][0]);
	m1->m.Y = FTOFIX16(m2[3][1]);
}

