// int s2d_atoi(char *s, char **s2) {
// 	int ret = 0;
// 	int isNegative = (*s == '-');
// 	if (isNegative) {s++; (*s2)++;}
// 	for (; *s != '\0' && *s != ' ' && *s >= '0' && *s <= '9'; s++) {
// 		ret *= 10;
// 		if (*s >= '0' && *s <= '9')
// 			ret += *s - '0';
// 		else break;
// 		if (!(*(s+1) != '\0' && *(s+1) != ' ' && *(s+1) >= '0' && *(s+1) <= '9')) break;
// 		(*s2)++;
// 	}
// 	if (isNegative) ret *= -1;
// 	return ret;
// }
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define SCALE     "\x80" // SCALE (some scale)
#define ROTATE    "\x81" // ROTATE (degrees) // TODO: maybe add axis?
#define TRANSLATE "\x82" // TRANSLATE (x) (y)

#define CH_SCALE     '\x80'
#define CH_ROT       '\x81'
#define CH_TRANSLATE '\x82'

#define CH_GET_NEXT(x) (*(++x))
#define CH_SKIP(x) ((++x))

int saved_degrees = 0;

int myScale = 1;
int myDegrees = 0;

char *t = "TEST CRINGE";
int s2d_atoi(char *s, char **s2) {
	int ret = 0;
	int isNegative = (*s == '-');
	if (isNegative) {
		s++;
		(*s2)++;
	}
	for (; *s != '\0' && *s != ' ' && *s >= '0' && *s <= '9'; s++) {
		ret *= 10;
		if (*s >= '0' && *s <= '9')
			ret += *s - '0';
		else break;
		if (!(*(s+1) != '\0' && *(s+1) != ' ' && *(s+1) >= '0' && *(s+1) <= '9')) break;
		(*s2)++;
	}
	if (isNegative) ret *= -1;
	return ret;
}

extern unsigned int gGlobalTimer;

void s2d_print(int x, int y, char *str) {
	char *p = str;
	int tx = 0, ty = 0;
	// int r = 0, g = 0, b = 0, a = 0;
	if (*p == '\0') return;
	do {
		char r = *p;
		int s, rd;
		switch (r) {
			case CH_SCALE:
				CH_SKIP(p);
				s = s2d_atoi(p, &p);
				myScale = s;
				break;
			case CH_ROT:
				CH_SKIP(p);
				rd = s2d_atoi(p, &p);
				saved_degrees = rd;
				myDegrees = rd;
				break;
			case CH_TRANSLATE:
				CH_SKIP(p);
				x = s2d_atoi(p, &p);
				CH_SKIP(p);
				CH_SKIP(p);
				y = s2d_atoi(p, &p);
				break;
			// case CH_COLOR:
				// CH_SKIP(p);
				// s2d_red = s2d_atoi(p, &p);
				// CH_SKIP(p);	CH_SKIP(p);

				// s2d_green = s2d_atoi(p, &p);
				// CH_SKIP(p);	CH_SKIP(p);

				// s2d_blue = s2d_atoi(p, &p);
				// CH_SKIP(p);	CH_SKIP(p);

				// s2d_alpha = s2d_atoi(p, &p);
				// break;

				// break;
			default:
				break;
				// if (myDegrees == 0)
				// 	// draw_s2d_glyph(r, (x += (8 * myScale)) + tx, y + ty, (buf++));
				// else
					// draw_s2d_glyph(r, (x += ((8 * myScale))) + tx, y + ty, (buf++));
		}
		p++;
		// myDegrees += saved_degrees;
	} while (*p != '\0');
	myScale = 1;
	myDegrees = 0;
	saved_degrees = 0;
	tx = 0;
	ty = 0;
}

int s2d_ilen(char *s) {
	int ret = 0;
	char *p = s;
	while (*p >= '0' && *p <= '9') {
		ret++;
		p++;
	}
	return ret;
}

int s2d_atoi2(char *s) {
	int ret = 0;
	int isNegative = (*s == '-');
	if (isNegative) {
		s++;
	}
	for (; *s != '\0' && *s != ' ' && *s >= '0' && *s <= '9'; s++) {
		ret *= 10;
		if (*s >= '0' && *s <= '9')
			ret += *s - '0';
		else break;
		if (!(*(s+1) != '\0' && *(s+1) != ' ' && *(s+1) >= '0' && *(s+1) <= '9')) break;
	}
	// (*s2)--;
	if (isNegative) ret *= -1;
	return ret;
}

void s2d_type_print(int x, int y, char *str, int *pos) {
	char *temp_str = str;
	char tmp = temp_str[*pos];
	int len = strlen(str);
	char current_transformation;
	int cur_transform_index;
	switch(tmp) {
		case CH_SCALE:
			current_transformation = CH_SCALE;
			cur_transform_index = *pos;
			// temp_str[*pos] = ' ';
			printf("SCALE %c %d\n", str[*pos + 1], s2d_ilen(str + *pos + 1));
			(*pos) += s2d_ilen(str + *pos+2) + 1;
			break;
		case CH_ROT:
			printf("ROTATE %s %d\n", str + *pos + 1, s2d_ilen(str + *pos + 1));
			(*pos) += s2d_ilen(str + *pos + 2);
			break;
	}
	// temp_str[*pos] = '\0';
	// s2d_print(x, y, temp_str);
	// // temp_str[cur_transform_index] = current_transformation;
	// temp_str[*pos] = tmp;

	// if (gGlobalTimer % 5 == 0) {
		if (*pos < len)
			(*pos)++;
	// }
	printf("%d %c\n", *pos, str[*pos]);
}


char sss[] = SCALE "2";
                // SCALE "4"
                // "big chungus";

char myS[] = "small test"
                ROTATE "2"
                "big test"
                ROTATE "26"
                "italic chungus";

char my2[] = "small test"
                ROTATE "2"
                "big test"
                ROTATE "0000046"
                "big italic chungus"
                ROTATE "124335";

int pos = 0;
int main(void) {
	// printf("%d\n", s2d_atoi(s, &s));
	// printf("%s\n", s);
	// s2d_print(0,0, t);
	char laStrBuf[0x100];
	char ff[0x100];
	sprintf(ff,
            "%s %d:"
            TRANSLATE "134 93" "%s:"
        , "Floor", 213, "Seed");
	sprintf(
        laStrBuf,
            "%s"
            TRANSLATE "138 93" "%s"
            TRANSLATE "138 110" "%s"
            TRANSLATE "138 130" "%s"
        , "The Basement", "LLRRABAB", "Continue", "End Run");

	while (pos != strlen(laStrBuf)) {
		// s2d_type_print(0, 0, myS, &pos);
		printf("%d %s\n",s2d_ilen(laStrBuf + pos), laStrBuf + pos);
		pos++;
	}
	pos = 0;
	while (pos != strlen(ff)) {
		// s2d_type_print(0, 0, myS, &pos);
		printf("%d %s\n",s2d_ilen(ff + pos), ff + pos);
		pos++;
	}
	// laStrBuf[strlen(laStrBuf)] = "\x1";
	// laStrBuf[strlen(laStrBuf)+1] = "\x1";
	// printf("%s\n", laStrBuf);
	// printf("%d\n", s2d_ilen(sss + 1));
}