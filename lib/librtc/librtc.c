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

// <HackerSM64>
#include "config/config_rom.h"
#ifdef USE_RTC
// </HackerSM64>

#include "librtc.h"

#ifdef __cplusplus
	#define librtc_register
#else
	#define librtc_register register
	#if __STDC_VERSION__ < 202311L
		#ifndef true
			#define true 1
		#endif

		#ifndef false
			#define false 0
		#endif
	#endif
#endif

#define LIBRTC_CLOCKS_PER_SEC 46875000u

#define LIBRTC_SECONDS_IN_MINUTE 60ll
#define LIBRTC_SECONDS_IN_HOUR (60ll * LIBRTC_SECONDS_IN_MINUTE)
#define LIBRTC_SECONDS_IN_DAY (24ll * LIBRTC_SECONDS_IN_HOUR)
#define LIBRTC_SECONDS_IN_NON_LEAP_YEAR (365ll * LIBRTC_SECONDS_IN_DAY)

static const librtc_time LIBRTC_UNIX_EPOCH = {
	0, 0, 0, 1, 0, 70, 4, 0, -1
};

static const char *s_weekdayNames[7] = {
	"Sunday",
	"Monday",
	"Tuesday",
	"Wednesday",
	"Thursday",
	"Friday",
	"Saturday"
};

static const char *s_monthNames[12] = {
	"January",
	"February",
	"March",
	"April",
	"May",
	"June",
	"July",
	"August",
	"September",
	"October",
	"November",
	"December"
};

static const int s_yday_table[12] = {
	0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
};

static unsigned int s_si_buffer[16] __attribute__((aligned(16)));
static unsigned int s_si_backup[16] __attribute__((aligned(16)));
static unsigned int s_prev_dma_addr;

typedef enum {
	LIBRTC_INIT_CALLED = 0x1,
	LIBRTC_NOT_WAITING = 0x2,
	LIBRTC_GOOD = 0x4,

	LIBRTC_READY = LIBRTC_INIT_CALLED | LIBRTC_NOT_WAITING
} librtc_state_t;

static librtc_state_t s_rtc_state = 0;
static long long s_wait_start;
static long long s_wait_end;

static long long s_offset = 0ll;
static int s_tod_offset = 0;

static librtc_bool s_exec_on_write_bug = false;

__attribute__((always_inline))
static inline void librtc_zero_words( unsigned int *addr, unsigned int numWords ) {
	for( unsigned int i = 0; i < numWords; i++ ) addr[i] = 0u;
}

__attribute__((always_inline))
static inline void librtc_copy( librtc_time *dest, const librtc_time *src ) {
	dest->tm_sec = src->tm_sec;
	dest->tm_min = src->tm_min;
	dest->tm_hour = src->tm_hour;
	dest->tm_mday = src->tm_mday;
	dest->tm_mon = src->tm_mon;
	dest->tm_year = src->tm_year;
	dest->tm_wday = src->tm_wday;
	dest->tm_yday = src->tm_yday;
	dest->tm_isdst = src->tm_isdst;
}

__attribute__((always_inline))
static inline long long librtc_clock() {
	long long count;
	asm volatile( "mfc0 %0, $9": "=r"( count ) );
	return count;
}

__attribute__((noinline))
static librtc_bool librtc_enable_interrupts() {
	librtc_register librtc_bool status;
	asm volatile( ".set noat \n\t .align 16 \n\t mfc0 $1, $12 \n\t andi %0, $1, 1 \n\t ori  $1, $1, 1  \n\t mtc0 $1, $12" : "=r"( status ) :: "at" );
	return status;
}

__attribute__((noinline))
static librtc_bool librtc_disable_interrupts() {
	librtc_register librtc_bool status;
	asm volatile( ".set noat \n\t .align 16 \n\t mfc0 $1, $12 \n\t andi %0, $1, 1 \n\t subu $1, $1, %0 \n\t mtc0 $1, $12" : "=r"( status ) :: "at" );
	return status;
}

__attribute__((always_inline))
static inline librtc_bool librtc_set_interrupts( librtc_bool enable ) {
	return enable ? librtc_enable_interrupts() : librtc_disable_interrupts();
}

static inline void __attribute__((always_inline)) si_await_op() {
	// Wait for the SI operation we started to finish, then clear the status
	while( !(*((volatile unsigned int*)0xa4300008u) & *((volatile unsigned int*)0xa430000Cu) & 0x2u) );
	*((volatile unsigned int*)0xa4800018u) = 0u;
}

static inline void __attribute__((always_inline)) si_wait_safe( librtc_bool yield ) {
	// If the SI is currently busy, wait until it isn't
	while( *((volatile unsigned int*)0xa4800018u) & 0x3 ) {
		librtc_set_interrupts( yield );
		while( *((volatile unsigned int*)0xa4800018u) & 0x3 );
		librtc_set_interrupts( false );
	}
}

static void librtc_pif_save() {
	s_prev_dma_addr = *((volatile unsigned int*)0xa4800000u);

	// Save the state of PIF RAM to memory to restore it later.
	// The joybus is only executed when using an SI DMA read, and not when doing
	// a direct read via the memory-mapped address. Thus, this has no side effects.

	volatile unsigned int *si_pif_ram = (volatile unsigned int *)0xbfc007c0u;
	for( int i = 0; i < 16; i++ ) {
		s_si_backup[i] = si_pif_ram[i];
	}
}

static void librtc_pif_restore() {
	if( s_exec_on_write_bug ) {
		// The emulator incorrectly executes the joybus on a DMA write instead of a read
		// Clear the command register so it doesn't execute again
		s_si_backup[15] &= 0xffffff00u;
	} else {
		// Re-parse the stored PIF RAM state
		s_si_backup[15] |= 1u;
	}

	__builtin_mips_cache( 0x19, &s_si_backup[0] );
	__builtin_mips_cache( 0x19, &s_si_backup[4] );
	__builtin_mips_cache( 0x19, &s_si_backup[8] );
	__builtin_mips_cache( 0x19, &s_si_backup[12] );

	*((volatile unsigned int*)0xa4800000u) = (unsigned int)s_si_backup & 0x1FFFFFFFu;
	asm volatile( "":::"memory" );
	*((volatile unsigned int*)0xa4800010u) = 0x1fc007c0u;
	asm volatile( "":::"memory" );
	si_await_op();

	*((volatile unsigned int*)0xa4800000u) = s_prev_dma_addr;
}

static void librtc_dma_write() {
	__builtin_mips_cache( 0x19, &s_si_buffer[0] );
	__builtin_mips_cache( 0x19, &s_si_buffer[4] );
	__builtin_mips_cache( 0x19, &s_si_buffer[8] );
	__builtin_mips_cache( 0x19, &s_si_buffer[12] );

	*((volatile unsigned int*)0xa4800000u) = (unsigned int)s_si_buffer & 0x1FFFFFFFu;
	asm volatile( "":::"memory" );
	*((volatile unsigned int*)0xa4800010u) = 0x1fc007c0u;
	asm volatile( "":::"memory" );

	si_await_op();
}

static void librtc_dma_read() {
	*((volatile unsigned int*)0xa4800000u) = (unsigned int)s_si_buffer & 0x1FFFFFFFu;
	asm volatile( "":::"memory" );
	*((volatile unsigned int*)0xa4800004u) = 0x1fc007c0u;
	asm volatile( "":::"memory" );

	si_await_op();

	__builtin_mips_cache( 0x11, &s_si_buffer[0] );
	__builtin_mips_cache( 0x11, &s_si_buffer[4] );
	__builtin_mips_cache( 0x11, &s_si_buffer[8] );
	__builtin_mips_cache( 0x11, &s_si_buffer[12] );
}

__attribute__((always_inline))
static inline void librtc_exec() {
	librtc_dma_write();
	librtc_dma_read();
}

static inline unsigned char decode_rtc_byte( unsigned char x ) {
	return (((x & 0xF0) >> 4) * 10) + (x & 0x0F);
}

static inline librtc_bool is_leap_year( long long year ) {
	if( year % 4ll != 0ll ) return false;
	if( year % 100ll == 0ll ) return (year % 400ll == 0ll) ? true : false;
	return false;
}

static long long year_to_unix_time( long long year ) {
	librtc_register long long leapYears = -478ll;
	leapYears += (year + 3ll) >> 2;
	leapYears -= ((year > 0ll) ? (year + 99ll) : year) / 100ll;
	leapYears += ((year > 0ll) ? (year + 399ll) : year) / 400ll;

	return (
		((year - 1970ll) * LIBRTC_SECONDS_IN_NON_LEAP_YEAR) +
		(leapYears * LIBRTC_SECONDS_IN_DAY)
	);
}

__attribute__((always_inline))
static inline librtc_bool librtc_is_emulator() {
	librtc_register const volatile unsigned int *const dpc = (const volatile unsigned int*)0xA4100000u;
	return !(dpc[5] | dpc[6] | dpc[7]);
}

__attribute__((always_inline))
static inline void librtc_set_month_and_day( long long year, int yday, int *mon, int *mday ) {
	if( yday < 31 ) {
		*mon = 0;
		*mday = yday + 1;
		return;
	} else if( yday < 59 ) {
		*mon = 1;
		*mday = yday - 30;
		return;
	} else if( yday == 59 ) {
		if( is_leap_year( year ) ) {
			*mon = 1;
			*mday = 29;
		} else {
			*mon = 2;
			*mday = 1;
		}
		return;
	}

	if( is_leap_year( year ) ) yday--;
	if( yday >= 334 ) {
		*mon = 11;
		*mday = yday - 333;
	} else for( int month = (yday + 4) >> 5;; month++ ) {
		if( yday < s_yday_table[month+1] ) {
			*mon = month;
			*mday = yday + 1 - s_yday_table[month];
			return;
		}
	}
}

librtc_bool librtc_init() {
	if( s_rtc_state & LIBRTC_INIT_CALLED ) {
		return s_rtc_state != LIBRTC_READY;
	}

	const librtc_bool intr = librtc_set_interrupts( false );
	if( s_rtc_state & LIBRTC_INIT_CALLED ) {
		librtc_set_interrupts( intr );
		return s_rtc_state != LIBRTC_READY;
	}

	si_wait_safe( intr );
	s_rtc_state |= LIBRTC_INIT_CALLED;
	librtc_pif_save();

	s_si_buffer[0] = 0u;
	s_si_buffer[1] = 0xff010306u;
	s_si_buffer[2] = 0xfffffffeu;
	librtc_zero_words( &s_si_buffer[3], 12 );
	s_si_buffer[15] = 1u;
	librtc_dma_write();

	if(
		*((volatile unsigned char*)0xbfc007c6u) != 0x03 ||
		*((volatile unsigned char*)0xbfc007c8u) != 0xff
	) {
		// On hardware (and accurate emulators such as Ares), writing to the joybus merely causes it to parse the command.
		// The commands are not actually executed until a DMA read is performed. So only the final byte should have changed.
		s_exec_on_write_bug = true;
	}

	librtc_dma_read();

	if( s_si_buffer[2] >> 8 != 0x001000u ) {
		librtc_pif_restore();
		librtc_set_interrupts( intr );
		return false;
	}

	s_rtc_state |= LIBRTC_GOOD;

	s_si_buffer[0] = 0u;
	s_si_buffer[1] = 0x02090700u;
	s_si_buffer[2] = 0u;
	s_si_buffer[3] = 0u;
	s_si_buffer[4] = 0x00fe0000u;
	librtc_zero_words( &s_si_buffer[5], 10 );
	s_si_buffer[15] = 1u;
	librtc_exec();

	s_si_buffer[1] = 0x0a010800u;
	s_si_buffer[2] = 0x03000000u;
	s_si_buffer[4] = 0x00fe0000u;
	s_si_buffer[15] = 1u;
	librtc_exec();

	s_wait_start = librtc_clock();
	s_wait_end = s_wait_start + (LIBRTC_CLOCKS_PER_SEC / 50u);

	if( librtc_is_emulator() ) {
		s_rtc_state |= LIBRTC_NOT_WAITING;
	}

	librtc_pif_restore();
	librtc_set_interrupts( intr );
	return true;
}

librtc_bool librtc_ready() {
	if( !(s_rtc_state & LIBRTC_INIT_CALLED) ) return false;
	if( !(s_rtc_state & LIBRTC_NOT_WAITING) ) {
		const long long now = librtc_clock();
		if( s_wait_end < s_wait_start ) {
			if( now >= s_wait_end && now < s_wait_start ) {
				s_rtc_state |= LIBRTC_NOT_WAITING;
				return true;
			} else {
				return false;
			}
		} else {
			if( now < s_wait_start || now >= s_wait_end ) {
				s_rtc_state |= LIBRTC_NOT_WAITING;
				return true;
			} else {
				return false;
			}
		}
	}

	return true;
}

librtc_bool librtc_get_time_raw( librtc_time *tm ) {
	if( !(s_rtc_state & LIBRTC_READY) ) {
		librtc_init();
		while( !librtc_ready() );
	}

	if( !(s_rtc_state & LIBRTC_GOOD) ) {
		librtc_copy( tm, &LIBRTC_UNIX_EPOCH );
		return false;
	}

	const librtc_bool intr = librtc_set_interrupts( false );
	si_wait_safe( intr );
	librtc_pif_save();

	s_si_buffer[0] = 0u;
	s_si_buffer[1] = 0x02090702u;
	s_si_buffer[2] = 0x00008001u;
	s_si_buffer[3] = 0x04017000u;
	s_si_buffer[4] = 0x80fe0000u;
	librtc_zero_words( &s_si_buffer[5], 10 );
	s_si_buffer[15] = 1u;
	librtc_exec();

	const unsigned char *const data = (const unsigned char*)&s_si_buffer[2];
	tm->tm_sec = (int)decode_rtc_byte( data[0] );
	tm->tm_min = (int)decode_rtc_byte( data[1] );
	tm->tm_hour = (int)decode_rtc_byte( data[2] - 0x80 );
	tm->tm_mday = (int)decode_rtc_byte( data[3] );
	tm->tm_wday = (int)decode_rtc_byte( data[4] );
	tm->tm_mon = (int)decode_rtc_byte( data[5] ) - 1u;
	tm->tm_year = (100 * (int)decode_rtc_byte( data[7] )) + (int)decode_rtc_byte( data[6] );
	if( tm->tm_mon < 12 ) tm->tm_yday = s_yday_table[tm->tm_mon] + tm->tm_mday - 1;
	tm->tm_isdst = -1;

	if( tm->tm_mon >= 2 && is_leap_year( 1900ll + (long long)tm->tm_year ) ) {
		tm->tm_yday++;
	}

	librtc_pif_restore();
	librtc_set_interrupts( intr );
	return true;
}

void librtc_set_offset( long long offset ) {
	s_offset = offset;
	s_tod_offset = (int)(offset % LIBRTC_SECONDS_IN_DAY);
}

long long librtc_set_time( const librtc_time *now ) {
	librtc_time epoch;
	if( librtc_get_time_raw( &epoch ) ) {
		s_offset = librtc_time_diff( &epoch, now );
		s_tod_offset = (int)(s_offset % LIBRTC_SECONDS_IN_DAY);
	}

	return s_offset;
}

librtc_bool librtc_get_time( librtc_time *tm ) {
	if( !librtc_get_time_raw( tm ) ) {
		return false;
	}

	if( s_offset ) {
		librtc_add_time( tm, s_offset );
	}

	return true;
}

long long librtc_to_unix_time( const librtc_time *tm ) {
	int month = tm->tm_mon % 12;
	if( month < 0 ) month += 12;

	long long days = (long long)s_yday_table[month] + (long long)tm->tm_mday - 1ll;
	if( month >= 2 && is_leap_year( (long long)tm->tm_year + 1900ll ) ) days++;
	return (
		year_to_unix_time( (long long)tm->tm_year + 1900ll + (long long)(tm->tm_mon / 12) ) +
		(days * LIBRTC_SECONDS_IN_DAY) +
		((long long)tm->tm_hour * LIBRTC_SECONDS_IN_HOUR) +
		((long long)tm->tm_min * LIBRTC_SECONDS_IN_MINUTE) +
		(long long)tm->tm_sec
	);
}

librtc_bool librtc_from_unix_time( long long unixTime, librtc_time *tm ) {
	tm->tm_isdst = 0;

	if( unixTime >= 67768036191676800ll ) {
		// set to maximum valid time
		tm->tm_sec = 59;
		tm->tm_min = 59;
		tm->tm_hour = 23;
		tm->tm_mday = 31;
		tm->tm_mon = 11;
		tm->tm_year = 0x7FFFFFFF;
		tm->tm_wday = 3;
		tm->tm_yday = 364;
		return false;
	} else if( unixTime < -67768040609721748ll ) {
		// set to minimum valid time
		tm->tm_sec = 0;
		tm->tm_min = 0;
		tm->tm_hour = 0;
		tm->tm_mday = 1;
		tm->tm_mon = 0;
		tm->tm_year = -0x80000000;
		tm->tm_wday = 4;
		tm->tm_yday = 0;
		return false;
	}

	long long yearGuess = 1970ll + (unixTime / 31556952ll);
	long long yearTs = year_to_unix_time( yearGuess );
	if( unixTime < yearTs ) {
		do {
			yearTs = year_to_unix_time( --yearGuess );
		} while( unixTime < yearTs );
	} else if( yearGuess > 0x7fffffffll ) {
		yearGuess = 0x7fffffffll;
		yearTs = year_to_unix_time( yearGuess );
	} else {
		librtc_register const long long nextYearTs = year_to_unix_time( yearGuess + 1ll );
		if( unixTime >= nextYearTs ) {
			yearGuess++;
			yearTs = nextYearTs;
		}
	}

	int ts = (int)(unixTime - yearTs);
	tm->tm_year = (int)(yearGuess - 1900ll);
	tm->tm_yday = ts / (int)LIBRTC_SECONDS_IN_DAY;
	ts %= (int)LIBRTC_SECONDS_IN_DAY;
	librtc_set_month_and_day( yearGuess, tm->tm_yday, &tm->tm_mon, &tm->tm_mday );
	tm->tm_hour = ts / (int)LIBRTC_SECONDS_IN_HOUR;
	ts %= (int)LIBRTC_SECONDS_IN_HOUR;
	tm->tm_min = ts / (int)LIBRTC_SECONDS_IN_MINUTE;
	tm->tm_sec = ts % (int)LIBRTC_SECONDS_IN_MINUTE;
	tm->tm_wday = (int)((4ll + (unixTime / LIBRTC_SECONDS_IN_DAY)) % 7ll);
	if( tm->tm_wday < 0 ) tm->tm_wday += 7;

	return true;
}

int librtc_get_time_of_day() {
	librtc_time tm;
	if( !librtc_get_time_raw( &tm ) ) {
		return -1;
	}

	librtc_register int seconds = tm.tm_sec;
	seconds += tm.tm_min * (int)LIBRTC_SECONDS_IN_MINUTE;
	seconds += tm.tm_hour * (int)LIBRTC_SECONDS_IN_HOUR;
	seconds += s_tod_offset;

	seconds %= (int)LIBRTC_SECONDS_IN_DAY;
	if( seconds < 0 ) {
		seconds = (int)LIBRTC_SECONDS_IN_DAY - seconds;
	}

	return seconds;
}

static inline librtc_bool strftime_push_text( char *str, unsigned int *i, unsigned int count, const char *text ) {
	for( ; *text; (*i)++ ) {
		if( *i >= count - 1 ) return false;
		str[*i] = *(text++);
	}
	return true;
}

static inline librtc_bool strftime_push_number( char *str, unsigned int *i, unsigned int count, long long n ) {
	if( n < 0ll ) {
		str[(*i)++] = '-';
		n = -n;
	}

	char buff[17];
	unsigned int j = 0u;
	do {
		buff[j++] = '0' + (char)(n % 10ll);
		n /= 10ll;
	} while( n );

	if( *i + j < *i || *i + j >= count ) return false;
	for( int k = (int)j - 1; k >= 0; k-- ) {
		str[(*i)++] = buff[k];
	}

	return true;
}

static inline int get_wby_days( int day, int wday ) {
	return day - ((day - wday + 382) % 7) + 3;
}

static long long get_wby_years( long long year, int day, int wday ) {
	if( get_wby_days( day, wday ) < 0 ) {
		return year - 1ll;
	} else if( get_wby_days( day - (is_leap_year( year ) ? 366ll : 365ll), wday ) > 0 ) {
		return year + 1ll;
	} else {
		return year;
	}
}

static int get_wby_weeks( long long year, int day, int wday ) {
	int days = get_wby_days( day, wday );
	if( days < 0 ) {
		days = get_wby_days( day + (is_leap_year( year ) ? 366ll : 365ll), wday );
	} else {
		const int days2 = get_wby_days( day - (is_leap_year( year ) ? 366ll : 365ll), wday );
		if( days2 > 0 ) days = days2;
	}

	return 1 + (days / 7);
}

unsigned int librtc_strftime_internal( char *str, unsigned int count, const char *format, const librtc_time *tp ) {
	const long long year = (long long)tp->tm_year + 1900ll;
	for( unsigned int i = 0; i < count; format++ ) {
		if( !*format ) {
			str[i] = '\0';
			return i;
		} else if( *format != '%' ) {
			str[i++] = *format;
			continue;
		}

		format++;
		const librtc_bool alt = (*format == '0');
		if( *format == 'E' || alt ) {
			format++;
		}

		switch( *format ) {
			case '%':
				str[i++] = '%';
				break;
			case 'n':
				str[i++] = '\n';
				break;
			case 't':
				str[i++] = '\t';
				break;
			case 'Y':
				if( !strftime_push_number( str, &i, count, year ) ) return 0;
				break;
			case 'y':
				if( i < count - 2 ) {
					librtc_register int yy = (int)(year % 100ll);
					if( yy < 0 ) yy += 100;
					str[i++] = '0' + (char)(yy / 10);
					str[i++] = '0' + (char)(yy % 10);
					break;
				} else return 0;
			case 'C':
				if( !strftime_push_number( str, &i, count, year / 100ll ) ) return 0;
				break;
			case 'G':
				if( !strftime_push_number( str, &i, count, get_wby_years(year, tp->tm_yday, tp->tm_wday ) ) ) return 0;
				break;
			case 'g':
				if( i < count - 2 ) {
					librtc_register int wyear = (int)(get_wby_years(year, tp->tm_yday, tp->tm_wday ) % 100ll);
					if( wyear < 0 ) wyear += 100;
					str[i++] = '0' + (char)(wyear / 10);
					str[i++] = '0' + (char)(wyear % 10);
					break;
				} else return 0;
				break;
			case 'b':
			case 'h':
				if( i >= count - 3 ) return 0;
				str[i++] = s_monthNames[tp->tm_mon][0];
				str[i++] = s_monthNames[tp->tm_mon][1];
				str[i++] = s_monthNames[tp->tm_mon][2];
				break;
			case 'B':
				if( !strftime_push_text( str, &i, count, s_monthNames[tp->tm_mon] ) ) return 0;
				break;
			case 'm':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)((tp->tm_mon + 1) / 10);
				str[i++] = '0' + (char)((tp->tm_mon + 1) % 10);
				break;
			case 'U':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)((tp->tm_yday - tp->tm_wday + 7) / 70);
				str[i++] = '0' + (char)(((tp->tm_yday - tp->tm_wday + 7) / 7) % 10);
				break;
			case 'W':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)((tp->tm_yday - ((tp->tm_wday + 6) % 7) + 7) / 70);
				str[i++] = '0' + (char)(((tp->tm_yday - ((tp->tm_wday + 6) % 7) + 7) / 7) % 10);
				break;
			case 'V':
				if( i < count - 2 ) {
					librtc_register const int week = get_wby_weeks(year, tp->tm_yday, tp->tm_wday );
					str[i++] = '0' + (char)(week / 10);
					str[i++] = '0' + (char)(week % 10);
					break;
				} else return 0;
			case 'j':
				if( i >= count - 3 ) return 0;
				str[i++] = '0' + (char)(tp->tm_yday / 100);
				str[i++] = '0' + (char)((tp->tm_yday / 10) % 10);
				str[i++] = '0' + (char)(tp->tm_yday % 10);
				break;
			case 'd':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)(tp->tm_mday / 10);
				str[i++] = '0' + (char)(tp->tm_mday % 10);
				break;
			case 'e':
				if( i >= count - 2 ) return 0;
				if( alt || tp->tm_mday > 9) {
					str[i++] = '0' + (char)(tp->tm_mday / 10);
					str[i++] = '0' + (char)(tp->tm_mday % 10);
				} else {
					str[i++] = ' ';
					str[i++] = '0' + (char)tp->tm_mday;
				}
				break;
			case 'a':
				if( i >= count - 3 ) return 0;
				str[i++] = s_weekdayNames[tp->tm_wday][0];
				str[i++] = s_weekdayNames[tp->tm_wday][1];
				str[i++] = s_weekdayNames[tp->tm_wday][2];
				break;
			case 'A':
				if( !strftime_push_text( str, &i, count, s_weekdayNames[tp->tm_wday] ) ) return 0;
				break;
			case 'w':
				str[i++] = '0' + (char)tp->tm_wday;
				break;
			case 'u':
				str[i++] = tp->tm_wday ? ('0' + (char)tp->tm_wday) : '7';
				break;
			case 'H':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)(tp->tm_hour / 10);
				str[i++] = '0' + (char)(tp->tm_hour % 10);
				break;
			case 'I':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)((1 + ((tp->tm_hour + 11) % 12)) / 10);
				str[i++] = '0' + (char)((1 + ((tp->tm_hour + 11) % 12)) % 10);
				break;
			case 'M':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)(tp->tm_min / 10);
				str[i++] = '0' + (char)(tp->tm_min % 10);
				break;
			case 'S':
				if( i >= count - 2 ) return 0;
				str[i++] = '0' + (char)(tp->tm_sec / 10);
				str[i++] = '0' + (char)(tp->tm_sec % 10);
				break;
			case 'p':
				if( i >= count - 2 ) return 0;
				str[i++] = (tp->tm_hour < 12) ? 'A' : 'P';
				str[i++] = 'M';
				break;
			case 'c':
				if( i < count - 21 ) {
					const unsigned int j = librtc_strftime_internal( &str[i], count - (unsigned int)i, "%a %b %e %H:%M:%S %Y", tp );
					if( !j ) return 0;
					i += j;
					break;
				} else return 0;
			case 'x':
			case 'D':
				if( i >= count - 8 ) return 0;
				i += librtc_strftime_internal( &str[i], count - (unsigned int)i, "%m/%d/%y", tp );
				break;
			case 'X':
			case 'T':
				if( i >= count - 8 ) return 0;
				i += librtc_strftime_internal( &str[i], count - (unsigned int)i, "%H:%M:%S", tp );
				break;
			case 'F':
				if( i < count - 7 ) {
					const unsigned int j = librtc_strftime_internal( &str[i], count - (unsigned int)i, "%Y-%m-%d", tp );
					if( !j ) return 0;
					i += j;
					break;
				} else return 0;
			case 'r':
				if( i >= count - 11 ) return 0;
				i += librtc_strftime_internal( &str[i], count - (unsigned int)i, "%I:%M:%S %p", tp );
				break;
			case 'R':
				if( i >= count - 5 ) return 0;
				i += librtc_strftime_internal( &str[i], count - (unsigned int)i, "%H:%M", tp );
				break;
			default:
				return 0;
		}
	}

	return 0;
}

unsigned int librtc_strftime( char *str, unsigned int count, const char *format, const librtc_time *tp ) {
	librtc_time tm;
	librtc_copy( &tm, tp );
	librtc_normalize( &tm );

	str[count - 1] = '\0';
	return librtc_strftime_internal( str, count, format, &tm );
}

// <HackerSM64>
#endif
// </HackerSM64>
