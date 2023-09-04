#ifndef _IE_PACK_H
#define _IE_PACK_H

typedef struct s_Propack
{
	u8	Id[3];						// MUST be RNC
	u8	Method;						// MUST be 0,1,2 (will have 0x80 added if indexed type)
	u32	UnpackedSize;
	u32	PackedSize;					// Including size of ALL data
	u16	BlockCount;					// Indexed method block count
	u8	BlockSize;					// When method is indexed, this gives us the block size in KB.
	u8	Reserved[3];
	u8	Data[0];
} t_Propack;


/* Prototypes for unpack routines in assembly - Note: these just do the raw unpacking
   everything else is done at a level above
*/

extern void Propack_UnpackM1(void *FilePtr, void *OutputBuffer);
extern void Propack_UnpackM2(void *FilePtr, void *OutputBuffer);

#endif // _IE_PACK_H
