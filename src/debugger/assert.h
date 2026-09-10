#pragma once

#define ASSERT_MESGBUF_SIZE 512


#ifndef __ASSEMBLER__

extern char *__n64Assert_Filename;
extern u32   __n64Assert_LineNum;
extern char *__n64Assert_Condition;
extern char __n64Assert_MessageBuf[ASSERT_MESGBUF_SIZE + 1];
extern void __n64Assert(char *fileName, u32 lineNum, char *cond);

/**
 * Will always halt with your message of choice
 */
#define errorf(message, ...) { \
    sprintf(__n64Assert_MessageBuf, message ## __VA_ARGS__); \
    __n64Assert(__FILE__, __LINE__, " errorf() "); \
}
#define error(message) { \
    sprintf(__n64Assert_MessageBuf, message); \
    __n64Assert(__FILE__, __LINE__, " error() "); \
}

/**
 * Wrapper for assert/aggress
 */
#define __assert_wrapper(cond) __n64Assert(__FILE__, __LINE__, (cond))

/**
 * `aggress` and `aggressf` will always halt if `cond` is not true (handle with care)
 */
#define aggressf(cond, ...) do {\
    if ((cond) == FALSE) { \
        sprintf(__n64Assert_MessageBuf, __VA_ARGS__); \
        __assert_wrapper(#cond); \
    } \
} while (0);
#define aggress(cond) do {\
    if ((cond) == FALSE) { \
        __n64Assert_MessageBuf[0] = 0; \
        __assert_wrapper(#cond); \
    } \
} while (0);

/**
 * Asserts will halt the game if `cond` is not true, and DEBUG is defined.
 */
#ifdef DEBUG_ASSERTIONS
#define assert(cond) do {\
    if ((cond) == FALSE) { \
        __n64Assert_MessageBuf[0] = 0; \
        __assert_wrapper(#cond); \
    } \
} while (0);
#define assertf(cond, ...) do {\
    if ((cond) == FALSE) { \
        sprintf(__n64Assert_MessageBuf, __VA_ARGS__); \
        __assert_wrapper(#cond); \
    } \
} while (0);
#else // DEBUG_ASSERTIONS
#define assert(cond)
#define assertf(cond, ...)
#endif // DEBUG_ASSERTIONS

#endif // ASSEMBLER
