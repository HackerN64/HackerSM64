#ifndef LIBDRAGON_TOOLS_UTILS_H
#define LIBDRAGON_TOOLS_UTILS_H

#include "polyfill.h"
#include <string.h>  // memcpy
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sys/stat.h>

/**
 * Misc utilities functions and macros. Internal header.
 */

#define SWAP(a, b) ({ typeof(a) t = a; a = b; b = t; })

#define MAX(a,b)  ({ typeof(a) _a = a; typeof(b) _b = b; _a > _b ? _a : _b; })
#define MIN(a,b)  ({ typeof(a) _a = a; typeof(b) _b = b; _a < _b ? _a : _b; })
#define CLAMP(x, min, max) (MIN(MAX((x), (min)), (max)))

/** @brief Round n up to the next multiple of d */
#define ROUND_UP(n, d) ({ \
    typeof(n) _n = n; typeof(d) _d = d; \
    (((_n) + (_d) - 1) / (_d) * (_d)); \
})

/** @brief Round n down to the previous multiple of d */
#define ROUND_DOWN(n, d) ({ \
    typeof(n) _n = n; typeof(d) _d = d; \
    ((_n >= 0) ? ((_n) / (_d) * (_d)) : -(((-_n + (_d) - 1) / (_d)) * (_d))); \
})

/** @brief Return the ceil of n/d */
#define DIVIDE_CEIL(n, d) ({ \
    typeof(n) _n = n; typeof(d) _d = d; \
    ((_n) + (_d) - 1) / (_d); \
})

/** @brief Absolute number */
#define ABS(x) ({ \
    typeof(x) _x = x; \
    (_x < 0 ? -_x : _x); \
})

/** @brief Type-safe bitcast from float to integer */
#define F2I(f)   ({ uint32_t __i; memcpy(&__i, &(f), 4); __i; })

/** @brief Type-safe bitcast from integer to float */
#define I2F(i)   ({ float __f; memcpy(&__f, &(i), 4); __f; })

/** @brief Hint for the compiler that the condition is likely to happen */
#define LIKELY(cond)    __builtin_expect(!!(cond), 1)
/** @brief Hint for the compiler that the condition is unlikely to happen */
#define UNLIKELY(cond)  __builtin_expect(!!(cond), 0)

/** @brief UTF-8 decoding */
uint32_t __utf8_decode(const char **str);

__attribute__((used))
static char* path_remove_trailing_slash(char *path)
{
    path = strdup(path);
    int n = strlen(path);
    if (path[n-1] == '/' || path[n-1] == '\\')
        path[n-1] = 0;
    return path;
}

__attribute__((used))
static char *change_ext(const char *fn, const char *ext)
{
    char *out = strdup(fn);
    char *dot = strrchr(out, '.');
    if (dot) *dot = 0;
    strcat(out, ext);
    return out;
}

__attribute__((used))
static bool file_exists(const char *filename)
{
    FILE *f = fopen(filename, "r");
    if (f) fclose(f);
    return f != NULL;
}

// Find the directory where the libdragon toolchain is installed.
// This is where you can find GCC, the linker, etc.
__attribute__((used))
static const char *n64_toolchain_dir(void)
{
    static char *n64_inst = NULL;
    if (n64_inst)
        return n64_inst;

    // Find the toolchain installation directory.
    // n64.mk supports having a separate installation for the toolchain and
    // libdragon. So first check if N64_GCCPREFIX is set; if so the toolchain
    // is there. Otherwise, fallback to N64_INST which is where we expect
    // the toolchain to reside.
    n64_inst = getenv("N64_GCCPREFIX");
    if (!n64_inst)
        n64_inst = getenv("N64_INST");
    if (!n64_inst)
        return NULL;

    // Remove the trailing backslash if any. On some system, running
    // popen with a path containing double backslashes will fail, so
    // we normalize it here.
    n64_inst = path_remove_trailing_slash(n64_inst);
    return n64_inst;
}

// Find the directory where the libdragon tools are installed.
// This is where you can find mksprite, mkfont, etc.
__attribute__((used))
static const char *n64_tools_dir(void)
{
    static char *n64_inst = NULL;
    if (n64_inst)
        return n64_inst;

    // Find the tools installation directory.
    n64_inst = getenv("N64_INST");
    if (!n64_inst)
        return NULL;

    // Remove the trailing backslash if any. On some system, running
    // popen with a path containing double backslashes will fail, so
    // we normalize it here.
    n64_inst = path_remove_trailing_slash(n64_inst);
    return n64_inst;
}

__attribute__((used))
static uint8_t* slurp(const char *fn, int *size)
{
    FILE *f = fopen(fn, "rb");
    if (!f) return NULL;
    fseek(f, 0, SEEK_END);
    int sz = ftell(f);
    fseek(f, 0, SEEK_SET);
    uint8_t *buf = (uint8_t*)malloc(sz);
    fread(buf, 1, sz, f);
    fclose(f);
    if (size) *size = sz;
    return buf;
}

#ifdef __cplusplus
#include <vector>
__attribute__((used))
static std::vector<uint8_t> slurp(const char *fn)
{
    std::vector<uint8_t> ret;
    FILE *f = fopen(fn, "rb");
    if (!f) return ret;
    fseek(f, 0, SEEK_END);
    ret.resize(ftell(f));
    fseek(f, 0, SEEK_SET);
    fread(&ret[0], 1, ret.size(), f);
    fclose(f);
    return ret;
}
#endif

__attribute__((used))
static void forward_to_stderr(FILE *log, const char *prefix)
{
    char *line = 0; size_t linesize = 0;
    while (getline(&line, &linesize, log) != -1) {
        fputs(prefix, stderr);
        fputs(line, stderr);
    }
    free(line);
}

#endif
