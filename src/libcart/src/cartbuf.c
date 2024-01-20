#include <cart.h>
#include "cartint.h"

void __cart_buf_rd(const void *addr)
{
	int i;
	const u_uint64_t *ptr = addr;
	for (i = 0; i < 512/8; i += 2)
	{
		uint64_t a = ptr[i+0];
		uint64_t b = ptr[i+1];
		__cart_buf[i+0] = a;
		__cart_buf[i+1] = b;
	}
}

void __cart_buf_wr(void *addr)
{
	int i;
	u_uint64_t *ptr = addr;
	for (i = 0; i < 512/8; i += 2)
	{
		uint64_t a = __cart_buf[i+0];
		uint64_t b = __cart_buf[i+1];
		ptr[i+0] = a;
		ptr[i+1] = b;
	}
}
