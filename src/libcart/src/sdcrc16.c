#include <cart.h>
#include "cartint.h"
#include "sd.h"

/* Thanks to anacierdem for this brilliant implementation. */

/* Spread lower 32 bits into 64 bits */
/* x =     **** **** **** **** abcd efgh ijkl mnop */
/* result: a0b0 c0d0 e0f0 g0h0 i0j0 k0l0 m0n0 o0p0 */
static uint64_t __sd_crc16_spread(uint64_t x)
{
	x = (x << 16 | x) & 0x0000FFFF0000FFFF;
	x = (x <<  8 | x) & 0x00FF00FF00FF00FF;
	x = (x <<  4 | x) & 0x0F0F0F0F0F0F0F0F;
	x = (x <<  2 | x) & 0x3333333333333333;
	x = (x <<  1 | x) & 0x5555555555555555;
	return x;
}

/* Shuffle 32 bits of two values into 64 bits */
/* x =     **** **** **** **** abcd efgh ijkl mnop */
/* y =     **** **** **** **** ABCD EFGH IJKL MNOP */
/* result: aAbB cCdD eEfF gGhH iIjJ kKlL mMnN oOpP */
static uint64_t __sd_crc16_shuffle(uint32_t x, uint32_t y)
{
	return __sd_crc16_spread(x) << 1 | __sd_crc16_spread(y);
}

void __sd_crc16(uint64_t *dst, const uint64_t *src)
{
	int i;
	int n;
	uint64_t x;
	uint64_t y;
	uint32_t a;
	uint32_t b;
	uint16_t crc[4] = {0};
	for (i = 0; i < 512/8; i++)
	{
		x = src[i];
		/* Transpose every 2x2 bit block in the 8x8 matrix */
		/* abcd efgh     aick emgo */
		/* ijkl mnop     bjdl fnhp */
		/* qrst uvwx     qys0 u2w4 */
		/* yz01 2345  \  rzt1 v3x5 */
		/* 6789 ABCD  /  6E8G AICK */
		/* EFGH IJKL     7F9H BJDL */
		/* MNOP QRST     MUOW QYS? */
		/* UVWX YZ?!     NVPX RZT! */
		y = (x ^ (x >> 7)) & 0x00AA00AA00AA00AA;
		x ^= y ^ (y << 7);
		/* Transpose 2x2 blocks inside their 4x4 blocks in the 8x8 matrix */
		/* aick emgo     aiqy emu2 */
		/* bjdl fnhp     bjrz fnv3 */
		/* qys0 u2w4     cks0 gow4 */
		/* rzt1 v3x5  \  dlt1 hpx5 */
		/* 6E8G AICK  /  6EMU AIQY */
		/* 7F9H BJDL     7FNV BJRZ */
		/* MUOW QYS?     8GOW CKS? */
		/* NVPX RZT!     9HPX DLT! */
		y = (x ^ (x >> 14)) & 0x0000CCCC0000CCCC;
		x ^= y ^ (y << 14);
		/* Interleave */
		/* x =     aiqy 6EMU bjrz 7FNV cks0 8GOW dlt1 9HPX */
		/* y =     emu2 AIQY fnv3 BJRZ gow4 CKS? hpx5 DLT! */
		/* result: aeim quy2 6AEI MQUY bfjn rvz3 7BFJ NRVZ */
		/*         cgko sw04 8CGK OSW? dhlp tx15 9DHL PTX! */
		x = __sd_crc16_shuffle(
			(x >> 32 & 0xF0F0F0F0) | (x >> 4 & 0x0F0F0F0F),
			(x >> 28 & 0xF0F0F0F0) | (x >> 0 & 0x0F0F0F0F)
		);
		for (n = 3; n >= 0; n--)
		{
			a = crc[n];
			/* (crc >> 8) ^ dat[0] */
			b = ((x ^ a) >> 8) & 0xFF;
			b ^= b >> 4;
			a = (a << 8) ^ b ^ (b << 5) ^ (b << 12);
			/* (crc >> 8) ^ dat[1] */
			b = (x ^ (a >> 8)) & 0xFF;
			b ^= b >> 4;
			a = (a << 8) ^ b ^ (b << 5) ^ (b << 12);
			crc[n] = a;
			x >>= 16;
		}
	}
	/* Interleave CRC */
	x = __sd_crc16_shuffle(crc[0] << 16 | crc[1], crc[2] << 16 | crc[3]);
	*dst = __sd_crc16_shuffle(x >> 32, x);
}
