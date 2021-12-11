#ifndef SRAM_H
#define SRAM_H


/*----------------------------------------------------------------------*/
/*	nuPiSramInit  - Initialization of handle for SRAM		*/
/*----------------------------------------------------------------------*/
extern int nuPiInitSram();
/*----------------------------------------------------------------------*/
/*	nuPiReadWriteSram  - DMA transfers data to and from SRAM.	*/
/*	IN:	addr	      SRAM address. 				*/
/*		buf_ptr	RDRAM address. 					*/
/*		size	      Transfer size. 				*/
/*	RET:	None							*/
/*----------------------------------------------------------------------*/
extern int nuPiReadWriteSram(u32 addr, void* buf_ptr, u32 size, s32 flag);

/*----------------------------------------------------------------------*/
/*	nuPiReadSram - Read from SRAM				*/
/*----------------------------------------------------------------------*/
#define nuPiReadSram(addr, buf_ptr, size)				\
	nuPiReadWriteSram(addr, buf_ptr, size, OS_READ)
    
/*----------------------------------------------------------------------*/
/*	nuPiWriteSram - Write to SRAM				*/
/*----------------------------------------------------------------------*/
#define nuPiWriteSram(addr, buf_ptr, size)				\
    nuPiReadWriteSram(addr, buf_ptr, size, OS_WRITE)    

#endif // SRAM_H