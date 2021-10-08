#ifndef __GZIP_H__
#define	__GZIP_H__


#ifndef NULL
#ifdef __cplusplus
#define NULL    0
#else
#define NULL    ((void *)0)
#endif
#endif

/*
#ifndef u8
#define	u8		unsigned char;
#endif
#ifndef u16
#define	u16		unsigned short;
#endif
#ifndef u32
#define	u32		unsigned long;
#endif
*/

#define INBUFSIZ	8192	// input buffer size
#define OUTBUFSIZ	8192	// output buffer size
#define WSIZE		0x8000

typedef struct {
	u8 *	next_addr;
	u32	rest_size;
} FILE_HND;

#define get_byte()  (inptr < insize ? inbuf[inptr++] : fill_inbuf(0))
#define try_byte()  (inptr < insize ? inbuf[inptr++] : fill_inbuf(1))

/* Macros for getting two-byte and four-byte header values */
#define SH(p) ((u16)(u8)((p)[0]) | ((u16)(u8)((p)[1]) << 8))
#define LG(p) ((u32)(SH(p)) | ((u32)(SH((p)+2)) << 16))

extern unsigned int insize;	// valid bytes in inbuf
extern unsigned int inptr;	// index of next byte to be processed in inbuf
extern unsigned int outcnt;	// bytes in output buffer
extern	u8	inbuf[];		// input buffer
extern	u8	outbuf[];		// output buffer
extern	u8	window[];		// Sliding window and suffix table

extern int	inflate(void);
extern int	fill_inbuf(int eof_ok);
extern int	flush_window(void);


//===========================================================================
//
//
u32   expand_gzip(u8 *src_addr, u8 *dst_addr, u32 size, u32 outbytes_limit);


#endif
