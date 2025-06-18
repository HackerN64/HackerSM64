#ifndef __LIBDRAGON_BACKTRACE_INTERNAL_H
#define __LIBDRAGON_BACKTRACE_INTERNAL_H

/** @brief The "type" of funciton as categorized by the backtrace heuristic (__bt_analyze_func) */
typedef enum {
    BT_FUNCTION,                ///< Regular function with a stack frame
    BT_FUNCTION_FRAMEPOINTER,   ///< The function uses the register fp as frame pointer (normally, this happens only when the function uses alloca)
    BT_EXCEPTION,               ///< This is an exception handler (inthandler.S)
    BT_LEAF                     ///< Leaf function (no calls), no stack frame allocated, sp/ra not modified
} bt_func_type;

/** @brief Description of a function for the purpose of backtracing (filled by __bt_analyze_func) */
typedef struct {
    bt_func_type type;       ///< Type of the function
    int stack_size;          ///< Size of the stack frame
    int ra_offset;           ///< Offset of the return address from the top of the stack frame
    int fp_offset;           ///< Offset of the saved fp from the top of the stack frame; this is != 0 only if the function modifies fp (maybe as a frame pointer, but not necessarily)
} bt_func_t;

u8 __bt_analyze_func(bt_func_t *func, u32 *ptr, u32 func_start, u8 from_exception);

/** @brief Like #backtrace, but start from an arbitrary context. Useful for backtracing a thread */
int __backtrace_from(void **buffer, int size, u32 *pc, u32 *sp, u32 *fp, u32 *exception_ra);

/**
 * @brief Return the symbol associated to a given address.
 * 
 * This function inspect the symbol table (if any) to search for the
 * specified address. It returns the function name the address belongs
 * to, and the offset within the function as a string in the format
 * "function_name+0x1234".
 * 
 * If the symbol table is not found in the rompack or the address is not found,
 * the return string is "???".
 * 
 * @param vaddr         Address to symbolize 
 * @param buf           Buffer where to store the result
 * @param size          Size of the buffer
 * @return char*        Pointer to the return string. This is within the provided
 *                      buffer, but not necessarily at the beginning because of DMA
 *                      alignment constraints.
 */
char* __symbolize(void *vaddr, char *buf, int size);

#endif

/**
 * @file backtrace.h
 * @brief Backtrace (call stack) support
 * @ingroup backtrace
 */

/**
 * @defgroup backtrace Backtrace (call stack) support
 * @ingroup lowlevel
 * @brief Implementation of functions to walk the stack and dump a backtrace
 * 
 * This module implements two POSIX/GNU standard functions to help walking
 * the stack and providing the current execution context: backtrace() and
 * backtrace_symbols().
 * 
 * The functions have an API fully compatible with the standard ones. The
 * implementation is however optimized for the MIPS/N64 case, and with
 * standard compilation settings. See the documentation in backtrace.c
 * for implementation details.
 * 
 * You can call the functions to inspect the current call stack. For
 * a higher level function that just prints the current call stack
 * on the debug channels, see #debug_backtrace.
 * 
 * @{
 */

#ifndef __LIBDRAGON_BACKTRACE_H
#define __LIBDRAGON_BACKTRACE_H

/** 
 * @brief A stack frame, part of a backtrace
 */
typedef struct {
    u32 addr;              ///< PC address of the frame (MIPS virtual address)

    const char *func;           ///< Name of the function (this should always be present)
    u32 func_offset;       ///< Byte offset of the address within the function

    const char *source_file;    ///< Name of the source file (if known, or "???" otherwise)
    int source_line;            ///< Line number in the source file (if known, or 0 otherwise)

    u8 is_inline;             ///< True if this frame refers to an inlined function
} backtrace_frame_t;

/**
 * @brief Walk the stack and return the current call stack
 * 
 * This function will analyze the current execution context,
 * walking the stack and returning informations on the active
 * call frames.
 * 
 * This function adheres to POSIX specification. It does not
 * allocate memory so it is safe to be called even in the
 * context of low memory conditions or possibly corrupted heap.
 * 
 * If called within an interrupt or exception handler, the function
 * is able to correctly walk backward the interrupt handler and
 * show the context even before the exception was triggered.
 * 
 * @param buffer    Empty array of pointers. This will be populated with pointers
 *                  to the return addresses for each call frame.
 * @param size      Size of the buffer, that is, maximum number of call frames
 *                  that will be walked by the function.
 * @return          Number of call frames walked (at most, size).
 */
int backtrace(void **buffer, int size);

/**
 * @brief Translate the buffer returned by #backtrace into a list of strings
 * 
 * This function symbolizes the buffer returned by #backtrace, translating
 * return addresses into function names and source code locations.
 * 
 * The user-readable strings are allocated on the heap and must be freed by
 * the caller (via a single free() call). There is no need to free each
 * of the returned strings: a single free() call is enough, as they are
 * allocated in a single contiguous block.
 * 
 * This function adheres to POSIX specification.
 * 
 * This function also handles inlined functions. In general, inlined function
 * do not have a real stack frame because they are expanded in place; so for
 * instance a single stack frame (as returned by #backtrace) can correspond
 * to multiple symbolized stack frames, one per each inlined function. Since
 * the POSIX API requires this function to return an array of the same size
 * of the input array, all inlined functions are collapsed into a single
 * string, separated by newlines.
 * 
 * @param buffer    Array of return addresses, populated by #backtrace
 * @param size      Size of the provided buffer, in number of pointers.
 * @return          Array of strings, one for each call frame. The array
 *                  must be freed by the caller with a single free() call.
 * 
 * @see #backtrace_symbols_cb
 */
char** backtrace_symbols(void **buffer, int size);

/**
 * @brief Symbolize the buffer returned by #backtrace, calling a callback for each frame
 * 
 * This function is similar to #backtrace_symbols, but instead of formatting strings
 * into a heap-allocated buffer, it invokes a callback for each symbolized stack
 * frame. This allows to skip the memory allocation if not required, and also allows
 * for custom processing / formatting of the backtrace by the caller.
 * 
 * The callback will receive an opaque argument (cb_arg) and a pointer to a
 * stack frame descriptor (#backtrace_frame_t). The descriptor and all its
 * contents (including strings) is valid only for the duration of the call,
 * so the callback must (deep-)copy any data it needs to keep.
 * 
 * The callback implementation might find useful to call #backtrace_frame_print
 * or #backtrace_frame_print_compact to print the frame information.
 * 
 * @param buffer    Array of return addresses, populated by #backtrace
 * @param size      Size of the provided buffer, in number of pointers.
 * @param flags     Flags to control the symbolization process. Use 0.
 * @param cb        Callback function to invoke for each symbolized frame
 * @param cb_arg    Opaque argument to pass to the callback function
 * @return True if the symbolization was successful, false otherwise.
 *         Notice that the function returns true even if some frames
 *         were not symbolized; false is only used when the function
 *         had to abort before even calling the callback once (eg:
 *         no symbol table was found).
 * 
 * @see #backtrace_symbols
 */
u8 backtrace_symbols_cb(void **buffer, int size, u32 flags,
    void (*cb)(void *, backtrace_frame_t*), void *cb_arg);

/** @} */

#endif

