/* librtc
 * by Matt "falcobuster" Pharoah
 * https://gitlab.com/mpharoah/librtc
 * 
 * Public Domain (www.unlicense.org)
 * This is free and unencumbered software released into the public domain.
 * 
 * Anyone is free to copy, modify, publish, use, compile, sell, or distribute this software, either in source code form or as a
 * compiled binary, for any purpose, commercial or non-commercial, and by any means.
 */

#ifndef LIBRTC_H
#define LIBRTC_H

// <HackerSM64>
#include "config/config_rom.h"
#ifndef USE_RTC
#error "librtc.h was included without enabling the RTC. Please uncomment the USE_RTC define in config_rom.h"
#endif
// </HackerSM64>

#ifdef __cplusplus
	typedef bool librtc_bool;
#else
	#if __STDC_VERSION__ >= 202311L
		typedef bool librtc_bool;
	#elif __STDC_VERSION__ >= 199901L
		typedef _Bool librtc_bool;
	#elif __GNUC__ >= 15
		typedef unsigned char __attribute__((hardbool(0, 1))) librtc_bool;
	#else
		typedef unsigned char librtc_bool;
	#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* A structure storing the calendar time
 * Uses the same format as the tm struct in the C standard library
 */
typedef struct {
	int tm_sec; // seconds after the minute [0-59]
	int tm_min; // minutes after the hour [0-59]
	int tm_hour; // hours since midnight [0-23]
	int tm_mday; // day of the month [1-31] (remember this starts from 1, not 0)
	int tm_mon; // months since January [0-11]
	int tm_year; // years since 1900 (remember the epoch is 1900 AD, not 1 BC)
	int tm_wday; // days since Sunday [0-6]
	int tm_yday; // days since January 1 [0-365]
	int tm_isdst; // Field included for the sake of matching the C standard library. Has no effect.
} librtc_time;


/* Initializes the RTC clock if it has not already been initialized.
 * Returns true if the flashcart or emulator supports a realtime clock.
 * Note that some emulators and flashcarts may require the RTC bit to be enabled in the Everdrive ROM header.
 * 
 * It is not strictly necessary to call librtc_init as it will be automatically initialized when first calling
 * librtc_get_time or librtc_get_time_raw if not yet initialized. However, to workaround bugs with some flashcarts,
 * there will be a stutter on console the first time librtc_get_time or librtc_get_time_raw is called if librtc_init
 * was not already called 20 milliseconds ago or more. See librtc_ready for more information.
 */
librtc_bool librtc_init();

/* Returns false if the RTC has not been initialized or if the rom is being played on console and 20 milliseconds have not yet
 * passed since the RTC was initialized. Otherwise, returns true, even if RTC is not supported by the flashcart or emulator.
 *
 * On console, there is a 20 millisecond delay between initializing the RTC and when the time can first be fetched.
 * This delay exists to workaround some buggy flashcarts that do not properly set the RTC status. To ensure that the RTC has had
 * enough time to initialize on these buggy flashcarts, a 20 millisecond delay is enforced to avoid returning garbage data.
 *
 * NOTE: The COUNT register on the CPU is used to determine the time passed. Since this counter overflows every ~90 seconds,
 * if the very first call to librtc_get_time or librtc_get_time occurs precisely within a 20 millisecond window after one of the
 * ~90 second cycles since initializing the RTC, it will incorrectly believe that 20 milliseconds have not yet passed.
 * To avoid this, you can call librtc_ready at a regular interval (less then 90 seconds), and the 20 millisecond delay will be
 * permanently marked as complete once it has passed, ensuring that COUNT overflow will no longer be a concern.
 */
librtc_bool librtc_ready();

/* Gets the unmodified current calendar time from the RTC.
 * This function ignores any offset set with librtc_set_offset or librtc_set_time.
 * Use librtc_get_time instead to get the adjusted time that respects the offset value.
 */
__attribute__((access(write_only, 1), nonnull(1)))
librtc_bool librtc_get_time_raw( librtc_time *tm );

/* Sets the number of seconds to be added to the raw RTC time when calling librtc_get_time */
void librtc_set_offset( long long offset );

/* Set the current time by updating the offset value such that calling librtc_get_time now would return the passed in time.
 * Returns the new offset value in seconds.
 */
__attribute__((nonnull(1)))
long long librtc_set_time( const librtc_time *now );

/* Gets the current time. The offset set using librtc_set_offset or librtc_set_time is added to the raw time from the RTC.
 * If the flashcart or emulator does not support RTC, return false and sets tm to the Unix epoch (Jan 1, 1970)
 */
__attribute__((access(write_only, 1), nonnull(1)))
librtc_bool librtc_get_time( librtc_time *tm );

/* Converts the passed in calendar time to Unix time, interpreting the time as UTC. */
__attribute__((pure, nonnull(1), warn_unused_result))
long long librtc_to_unix_time( const librtc_time *tm );

/* Converts the passed in Unix time to calendar time and stores it in tm.
 * If the passed in Unix time is too large or too small to fit in a normalized librtc_time struct,
 * the time is clamped to the minimum/maximum value, and the function returns false.
 */
__attribute__((nonnull(2), access(write_only, 2)))
librtc_bool librtc_from_unix_time( long long unixTime, librtc_time *tm );

/* Returns the current time as the number of seconds since the start of the day [0-86399].
 * This function takes the offset (set with librtc_set_offset or librtc_set_time) into account.
 * This is more efficient than using librtc_get_time as it avoids needing to calculate leap days,
 * nor does it need to perform any 64-bit arithmetic.
 * 
 * Returns -1 if the flashcart or emulator does not support RTC.
 */
__attribute__((warn_unused_result))
int librtc_get_time_of_day();

/* Adds a number of seconds to the calendar time and normalizes it.
 * The passed in struct is modified, and a pointer to the same struct is returned.
 */
__attribute__((always_inline, nonnull(1), access(read_write, 1), returns_nonnull))
inline librtc_time *librtc_add_time( librtc_time *tm, long long seconds ) {
	seconds += librtc_to_unix_time( tm );
	librtc_from_unix_time( seconds, tm );
	return tm;
}

/* Returns the difference in seconds between two calendar times.
 * If the first argument is later than the second argument, the return value is negative.
 */
__attribute__((pure, always_inline, warn_unused_result, nonnull(1, 2)))
inline long long librtc_time_diff( const librtc_time *tm_a, const librtc_time *tm_b ) {
	return librtc_to_unix_time( tm_b ) - librtc_to_unix_time( tm_a );
}

/* Normalizes the calendar time object so that all fields are in range.
 * The day is normalized using tm_year, tm_month, and tm_day
 * (tm_wday and tm_yday are only updated and not read).
 * If the normalized calendar time results in a year that would overflow,
 * the time is clamped to the minimum/maximum value, and the function returns false.
 */
__attribute__((always_inline, nonnull(1), access(read_write, 1)))
inline librtc_bool librtc_normalize( librtc_time *tm ) {
	const long long seconds = librtc_to_unix_time( tm );
	return librtc_from_unix_time( seconds, tm );
}

/* Formats a librtc_time struct into a textual representation using the format specifier.
 * This function behaves identically to the strftime function from the C standard library.
 * strftime documentation: https://en.cppreference.com/w/c/chrono/strftime
 */
__attribute__((format(strftime, 3, 0), nonnull(1, 3, 4), access(write_only, 1)))
unsigned int librtc_strftime( char *str, unsigned int count, const char *format, const librtc_time *tp );

#ifdef __cplusplus
}
#endif

#endif
