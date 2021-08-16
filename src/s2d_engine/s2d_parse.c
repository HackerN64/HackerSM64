#include <ultra64.h>
#include <PR/gs2dex.h>
#include <PR/gu.h>
#include <stdarg.h>

#include "config.h"

#include "s2d_draw.h"
#include "s2d_print.h"
#include "s2d_ustdlib.h"
#include "s2d_error.h"

static int s2d_width(const char *str, int line, int len);

enum S2DPrintModes {
	MODE_DRAW_DROPSHADOW,
	MODE_DRAW_NORMALTEXT,
};

static int s2d_snprint(int x, int y, int align, const char *str, uObjMtx *buf, int len, int mode) {
	char *p = str;
	int tmp_len = 0;
	int orig_x = x;
	int orig_y = y;
	int line = 0;
	char button = '\0';

	if (*p == '\0') return;

	if (IS_RUNNING_ON_EMULATOR) {
		s2d_rdp_init();
	} else {
		f3d_rdp_init();
	}

	// resets parameters
	s2d_red = s2d_green = s2d_blue = 255;
	s2d_alpha = 255;
	drop_shadow = FALSE;

	switch (align) {
		case ALIGN_CENTER:
			x = orig_x - s2d_width(str, line, len) / 2;
			break;
		case ALIGN_RIGHT:
			x = orig_x - s2d_width(str, line, len);
	}

	do {
		char current_char = *p;

		switch (current_char) {
			case CH_SCALE:
				CH_SKIP(p);
				myScale = (f32)s2d_atoi(p, &p) / 100.0f;
				break;
			case CH_ROT:
				CH_SKIP(p);
				myDegrees = s2d_atoi(p, &p);
				break;
			case CH_TRANSLATE:
				CH_SKIP(p);
				orig_x = s2d_atoi(p, &p);
				line++;
				switch (align) {
					case ALIGN_LEFT:
						x = orig_x;
						break;
					case ALIGN_CENTER:
						x = orig_x - s2d_width(str, line, len) / 2;
						break;
					case ALIGN_RIGHT:
						x = orig_x - s2d_width(str, line, len);
				}
				CH_SKIP(p);
				CH_SKIP(p);
				orig_y = s2d_atoi(p, &p);
				y = orig_y;
				break;
			case CH_COLOR:
				CH_SKIP(p);
				s2d_red = s2d_atoi(p, &p);
				CH_SKIP(p);	CH_SKIP(p);

				s2d_green = s2d_atoi(p, &p);
				CH_SKIP(p);	CH_SKIP(p);

				s2d_blue = s2d_atoi(p, &p);
				CH_SKIP(p);	CH_SKIP(p);
				
				s2d_alpha = s2d_atoi(p, &p);
				break;
			case CH_DROPSHADOW:
				drop_shadow = 1;

				// WIP: drop shadow custom offset
				// TODO: unique offset per string; fix negative offsets
				CH_SKIP(p);
				drop_x = s2d_atoi(p, &p);
				CH_SKIP(p);	CH_SKIP(p);
				drop_y = s2d_atoi(p, &p);
				break;
			case CH_BUTTON:
				if (len - tmp_len == 1) break;
				if (len - tmp_len > 2) {
					CH_SKIP(p);
					break;
				}
				button = p[1];
				CH_SKIP(p);
				switch (button) {
					case 'A':
						if (!(CONTROLLER_HELD_INPUT & A_BUTTON)) return 1;
						break;
					case 'B':
						if (!(CONTROLLER_HELD_INPUT & B_BUTTON)) return 1;
						break;
					case 'Z':
						if (!(CONTROLLER_HELD_INPUT & Z_TRIG)) return 1;
						break;
					case 'L':
						if (!(CONTROLLER_HELD_INPUT & L_TRIG)) return 1;
						break;
					case 'R':
						if (!(CONTROLLER_HELD_INPUT & R_TRIG)) return 1;
						break;
					case 'S':
						if (!(CONTROLLER_HELD_INPUT & START_BUTTON)) return 1;
						break;
				}
				break;
			case '\n':
				line++;
				switch (align) {
					case ALIGN_LEFT:
						x = orig_x;
						break;
					case ALIGN_CENTER:
						x = orig_x - s2d_width(str, line, len) / 2;
						break;
					case ALIGN_RIGHT:
						x = orig_x - s2d_width(str, line, len);
				}
				y += TEX_HEIGHT / TEX_RES;
				break;
			case '\t':
				x += TAB_WIDTH_H / TEX_RES;
				break;
			case '\v':
				x += TAB_WIDTH_V / TEX_RES;
				y += TEX_HEIGHT / TEX_RES;
				break;
			// case CH_SEPARATOR:
			// 	CH_SKIP(p);
			// 	break;
			case CH_RESET:
				s2d_red = s2d_green = s2d_blue = 255;
				s2d_alpha = 255;
				drop_shadow = FALSE;
				drop_x = 0;
				drop_y = 0;
				myScale = 1;
				myDegrees = 0;
				break;
			default:
				if (current_char != '\0' && current_char != CH_SEPARATOR) {
					char *tbl = segmented_to_virtual(s2d_kerning_table);

					if (IS_RUNNING_ON_EMULATOR) {
						if (drop_shadow && mode == MODE_DRAW_DROPSHADOW) {
							draw_s2d_dropshadow(current_char, x + drop_x, y + drop_y, (buf++));
						} else if (mode == MODE_DRAW_NORMALTEXT) {
							draw_s2d_glyph(current_char, x, y, (buf++));
						}
					} else {
						if (drop_shadow && mode == MODE_DRAW_DROPSHADOW) {
							draw_f3d_dropshadow(current_char, x + drop_x, y + drop_y, (buf++));
						} else if (mode == MODE_DRAW_NORMALTEXT) {
							draw_f3d_glyph(current_char, x, y, (buf++));
						}
					}

					(x += (tbl[(int) current_char] * (BASE_SCALE * myScale)));
				}
		}
		if (*p == '\0') break;
		p++;
		tmp_len++;
	} while (tmp_len < len);
	myScale = 1.0f;
	myDegrees = 0;
	drop_shadow = FALSE;
	drop_x = 0;
	drop_y = 0;
	return 0;
}

static int s2d_string_has_dropshadow(const char *str) {
	char *p = str;

	int result = FALSE;

	do {if (*p == CH_DROPSHADOW) result = TRUE;} while (*(++p) != '\0');

	return result;
}

// deprecated
void s2d_print(int x, int y, int align, const char *str, uObjMtx *buf) {
	if (s2d_check_align(align) != 0) return;
	if (s2d_check_str(str)     != 0) return;

	s2d_snprint(x, y, align, str, buf, s2d_strlen(str), MODE_DRAW_DROPSHADOW);
	s2d_snprint(x, y, align, str, buf, s2d_strlen(str), MODE_DRAW_NORMALTEXT);
}

void s2d_print_alloc(int x, int y, int align, const char *str) {
	int len;

	if (s2d_check_align(align) != 0) return;
	if (s2d_check_str(str)     != 0) return;

	len = s2d_strlen(str);

	if (s2d_string_has_dropshadow(str)) {
		uObjMtx *b = alloc(sizeof(uObjMtx) * len);
		s2d_snprint(x, y, align, str, b, len, MODE_DRAW_DROPSHADOW);
	}

	uObjMtx *b2 = alloc(sizeof(uObjMtx) * len);
	s2d_snprint(x, y, align, str, b2, len, MODE_DRAW_NORMALTEXT);
}

// deprecated
void s2d_type_print(int x, int y, int align, const char *str, uObjMtx *buf, int *pos) {
	int len;

	if (s2d_check_align(align) != 0) return;
	if (s2d_check_str(str)     != 0) return;
	
	len = s2d_strlen(str);

	s2d_snprint(x, y, align, str, buf, *pos, MODE_DRAW_DROPSHADOW);

	int result = s2d_snprint(x, y, align, str, buf, *pos, MODE_DRAW_NORMALTEXT);
	if (s2d_timer % 2 == 0) {
		if (*pos < len && result != 1) {
			(*pos)++;
		}
	}
}

void s2d_type_print_alloc(int x, int y, int align, const char *str, int *pos) {
	int len;

	if (s2d_check_align(align) != 0) return;
	if (s2d_check_str(str)     != 0) return;

	len = s2d_strlen(str);

	uObjMtx *b = alloc(sizeof(uObjMtx) * (*pos));

	if (s2d_string_has_dropshadow(str)) {
		uObjMtx *b_d = alloc(sizeof(uObjMtx) * (*pos));
		s2d_snprint(x, y, align, str, b_d, *pos, MODE_DRAW_DROPSHADOW);
	}

	int result = s2d_snprint(x, y, align, str, b, *pos, MODE_DRAW_NORMALTEXT);
	if (s2d_timer % 2 == 0) {
		if (*pos < len && result != 1) {
			(*pos)++;
		}
	}
}

// broken atm
static int s2d_width(const char *str, int line, int len) {
	char *p = str;
	int tmp_len = 0;
	int curLine = 0;
	int width = 0;
	int scale = 1;

	if (*p == '\0') return width;

	do {
		char current_char = *p;
		switch (current_char) {
			case CH_SCALE:
				CH_SKIP(p);
				scale = s2d_atoi(p, &p);
				break;
			case CH_ROT:
				CH_SKIP(p);
				s2d_atoi(p, &p);
				break;
			case CH_TRANSLATE:
				CH_SKIP(p);
				s2d_atoi(p, &p);
				curLine++;
				CH_SKIP(p);
				CH_SKIP(p);
				s2d_atoi(p, &p);
				break;
			case CH_COLOR:
				CH_SKIP(p);
				s2d_atoi(p, &p);
				CH_SKIP(p); CH_SKIP(p);
				s2d_atoi(p, &p);
				CH_SKIP(p); CH_SKIP(p);
				s2d_atoi(p, &p);
				CH_SKIP(p); CH_SKIP(p);
				s2d_atoi(p, &p);
				break;
			case CH_DROPSHADOW:
			case CH_RESET:
				break;
			case '\n':
				curLine++;
				break;
			case '\t':
				if (curLine == line)
					width += TAB_WIDTH_H / TEX_RES;
				break;
			case '\v':
				if (curLine == line)
					width += TAB_WIDTH_V / TEX_RES;
				break;
			default:
				if (current_char != '\0' && curLine == line)
					width += s2d_kerning_table[(int) current_char] * scale;
		}
		if (*p == '\0') break;
		p++;
		tmp_len++;
	} while (tmp_len < len && curLine <= line);
	return width;
}

// void s2d_vsprint(int x, int y, int align, uObjMtx *buf, const char *str, ...) {
// 	int last_chr;
// 	va_list args;
// 	char *dst = alloc(s2d_strlen(str) * 2);
// 	va_start(args, str);
// 	last_chr = vsprintf(dst, str, str, args);
// 	if (last_chr >= 0) {
// 		dst[last_chr] = '\0';
// 	}
// 	s2d_print(x, y, align, dst, buf);
// }


