#ifndef STRING_H
#define STRING_H

#include "PR/ultratypes.h"

void *memcpy(void *dst, const void *src, size_t size);
size_t strlen(const char *str);
char *strchr(const char *str, s32 ch);
int memcmp(const void *buf1, const void *buf2, size_t count);
void    *memset(void* dst, int c, size_t count);
char  *strcat(char *str1, const char *str2);
int strncmp(const char *str1, const char *str2, int n);

#endif
