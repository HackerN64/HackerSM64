#include "config.h"
#include "s2d_print.h"
#include "s2d_error.h"

int s2d_error_y = TEX_HEIGHT;

int s2d_check_align(int align) {
	if (align < ALIGN_LEFT || align > ALIGN_RIGHT) {
		s2d_print_alloc(TEX_WIDTH, s2d_error_y, ALIGN_LEFT, "ERROR: invalid alignment");
		s2d_error_y += TEX_HEIGHT;
		return -1;
	}
	return 0;
}

int s2d_check_str(const char *str) {
	if (str == NULL || ((u32)str & 0x80000000) == 0) {
		s2d_print_alloc(TEX_WIDTH, s2d_error_y, ALIGN_LEFT, "ERROR: bad string, or no string specified");
		s2d_error_y += TEX_HEIGHT;
		return -1;
	}
	return 0;
}
