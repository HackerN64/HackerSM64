#pragma once

#include <ultra64.h>

#include "types.h"


#define ASSERTF_BUFFER_SIZE 255


extern char* __n64Assert_Condition;
extern char* __n64Assert_Filename;
extern u32   __n64Assert_LineNum;
extern char* __n64Assert_Message;


extern void __n64Assert(char* condition, char* fileName, u32 lineNum, char* message);
extern void __n64Assertf(char* condition, char* fileName, u32 lineNum, char* message, ...) __attribute__((format(printf, 4, 5)));


/**
 * Will always cause a crash with your message of choice.
 */
#define ERROR(message)          __n64Assert(NULL, __FILE__, __LINE__, (message))
#define ERRORF(message, ...)    __n64Assertf(NULL, __FILE__, __LINE__, (message), ##__VA_ARGS__)

/**
 * Will always cause a crash if cond is not true (handle with care).
 */
#define ASSERT(condition, message) do {                                         \
    if (!(condition)) {                                                         \
        __n64Assert(#condition, __FILE__, __LINE__, (message));                 \
    }                                                                           \
} while (0)
#define ASSERTF(condition, message, ...) do {                                   \
    if (!(condition)) {                                                         \
        __n64Assertf(#condition, __FILE__, __LINE__, (message), ##__VA_ARGS__); \
    }                                                                           \
} while (0)

/**
 * Will cause a crash if condition is not true, and ENABLE_DEBUG_ASSERTS is defined (allows for quick removal of littered asserts).
 */
#ifdef ENABLE_DEBUG_ASSERTS
    #define DEBUG_ERROR(message)                    ERROR(message)
    #define DEBUG_ERRORF(message, ...)              ERRORF((message), ##__VA_ARGS__)
    #define DEBUG_ASSERT(condition, message)        ASSERT(condition, (message))
    #define DEBUG_ASSERTF(condition, message, ...)  ASSERTF(condition, (message), ##__VA_ARGS__)
#else
    #define DEBUG_ERROR(message)
    #define DEBUG_ERRORF(message, ...)
    #define DEBUG_ASSERT(condition, message)
    #define DEBUG_ASSERTF(condition, message, ...)
#endif
