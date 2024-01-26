#ifndef LIBPL_TYPES_
#define LIBPL_TYPES_

typedef signed char  bool8_t;
typedef signed short bool16_t;
typedef signed int   bool32_t;

#ifndef TRUE
	#define TRUE 1
#endif

#ifndef FALSE
	#define FALSE 0
#endif

#ifndef NULL
	#ifdef __cplusplus
		#if __cplusplus > 199711L
			#define NULL nullptr
		#else
			#define NULL 0
		#endif
	#else
		#define NULL ((void*)0)
	#endif
#endif

#endif
