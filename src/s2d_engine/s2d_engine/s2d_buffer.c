#include "config.h"
#include "s2d_error.h"
#include "s2d_print.h"

#define S2D_BUFFERSIZE 100

struct s2d_pos {
	int x;
	int y;
};
static struct s2d_pos s2d_positions[S2D_BUFFERSIZE];
static char *s2d_charBuffer[S2D_BUFFERSIZE];
static int s2d_charBuffer_index = 0;

void s2d_reset_defer_index(void) {
	s2d_charBuffer_index = 0;
}

// TODO: these DO NOT work
// pls debug :)
void s2d_print_deferred(int x, int y, const char *str) {
	s2d_charBuffer[s2d_charBuffer_index] = str;
	s2d_positions[s2d_charBuffer_index].x = x;
	s2d_positions[s2d_charBuffer_index].y = y;

	s2d_charBuffer_index++;
}

void s2d_handle_deferred(void) {
	for (int i = 0; i < s2d_charBuffer_index; i++) {
		s2d_print_alloc(
			s2d_positions[i].x,
			s2d_positions[i].y,
			ALIGN_LEFT,
			s2d_charBuffer[i]
		);
	}
}

