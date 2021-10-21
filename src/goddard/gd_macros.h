#ifndef GD_MACROS_H
#define GD_MACROS_H

/**
 * @file gd_macros.h
 *
 * Common macros that Goddard used throughout the Mario Head subsytem code.
 */

#ifndef ALIGN
#define ALIGN(VAL_, ALIGNMENT_) (((VAL_) + ((ALIGNMENT_) - 1)) & ~((ALIGNMENT_) - 1))
#endif

#endif // GD_MACROS_H
