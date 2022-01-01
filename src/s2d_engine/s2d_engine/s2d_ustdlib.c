// ustdlib means un-standard library
// i'm going to abuse so much of my power here lol
#include "s2d_print.h"
// int s2d_atoi(char *s, char **s2) {
// 	int ret = 0;
// 	int isNegative = (*s == '-');
// 	if (isNegative) {
// 		s++;
// 		(*s2)++;
// 	}
// 	while (*s != '\0' && *s != ' ' && *s >= '0' && *s <= '9') {
// 		ret *= 10;
// 		ret += *s - '0';
// 		s++;
// 		(*s2)++;
// 	}
// 	if (isNegative) ret *= -1;
// 	return ret;
// }

int s2d_atoi(char *s, char **s2) {
	int ret = 0;
	int isNegative = (*s == '-');
	if (isNegative) {
		s++;
		(*s2)++;
	}
	for (; *s != '\0' && *s != ' ' && *s != CH_SEPARATOR && *s >= '0' && *s <= '9'; s++) {
		ret *= 10;
		if (*s >= '0' && *s <= '9')
			ret += *s - '0';
		else break;
		if (!(*(s+1) != '\0' && *(s+1) != CH_SEPARATOR && *(s+1) != ' ' && *(s+1) >= '0' && *(s+1) <= '9')) break;
		(*s2)++;
	}
	if (isNegative) ret *= -1;
	return ret;
}

int s2d_ilen(char *s) {
	int ret = 0;
	char *p = s;
	while (*p >= '0' && *p <= '9' && *p != CH_SEPARATOR) {
		ret++;
		p++;
	}
	return ret;
}

int s2d_strlen(char *s) {
	int result = 0;
	char *p = s;
	do {result++;} while (*(++p) != '\0');
	return result;
}
