/*------------------------------------------------------------------------------
* PROPACK Unpack Source Code - Sony PSX, Method 1
*
* Copyright (c) 1991-96 Rob Northen Computing, U.K. All Rights Reserved.
*
* Conversion from 68000 by Brian Watson, Iguana Entertainment Limited.
* Converted to compile with SGI 'as' 10/31/96 by Biscuit
*
* File: rnc1.s
*
* Date: 10.21.96
*------------------------------------------------------------------------------
*/
/*------------------------------------------------------------------------------
* Equates
*------------------------------------------------------------------------------
*/
#define HEADER_LEN     	18
#define crc_poly        0xa001

#define	RAW_TABLE       0
#define POS_TABLE       (RAW_TABLE+16*8)
#define LEN_TABLE       (POS_TABLE+16*8)
#define BUFSIZE         (16*8*3)

#define ra		$31	/* ra */
#define d0             	$8	/* t0 */
#define d1             	$9	/* t1 */
#define d2		$10	/* t2 */
#define d3		$11	/* t3 */
#define d4		$12	/* t4 */
#define d5		$13	/* t5 */
#define d6		$14	/* t6 */
#define d7		$15	/* t7 */

#define xa0		$16	/* s0 */
#define xa1		$17	/* s1 */
#define xa2		$18	/* s2 */
#define xa3		$19	/* s3 */
#define xa4		$20	/* s4 */
#define xa5		$21	/* s5 */
#define xa6		$22	/* s6 */
#define xa7		$23	/* s7 */

#define TEMP            $2	/* v1 */
#define input           xa3	/* s3 */
#define output          xa4	/* s4 */
#define output_hi	xa5	/* s5 */

#define counts          d4	/* t4 */
#define bit_buffer      d6	/* t6 */
#define bit_count       d7	/* t7 */


#define zero		$0	/* zero */
#define tmp		$1	/* at */
#define tmp2		$2	/* v0 */
#define const_ff	$3	/* v1 */
#define const_ff000000 	$4	/* a0 */
#define const_80000000	$5	/* a1 */
#define const_ffffff00	$6	/* a2 */
#define const_8000	$7	/* a3 */
#define a0 		$4
#define a1		$5
/*------------------------------------------------------------------------------
* Macros
*------------------------------------------------------------------------------
*/
#define pop(reg)	lw reg,0($sp) ; add $sp,$sp,8

#define dbra reg,dest	bne	reg,zero,dest ; sub	reg,reg,1
/*------------------------------------------------------------------------------
* PROPACK Unpack Routine (Compact Version) - MC68000, Method 1
*
* on entry,
*           a0.l = start address of packed file
*           a1.l = start address to unpack file
*           (note: a1 cannot be equal to a0)
*           stack space required: $1DC bytes
*
*           all other registers are preserved
* Note:
*	lines with a comment beginning with '*' are delay slot instructions
*	beware the delay slot, it can really screw you up!
*------------------------------------------------------------------------------
*/
	.text
	.set noreorder
	.option pic0
	.set noat

	.globl	Propack_UnpackM1
	.ent	Propack_UnpackM1 2
Propack_UnpackM1:
 	.frame	$sp, 0, $31
	/*
	 Save any registers required here
	*/
		sub	$sp, $sp, 88
		sw  	$4, 0($sp)
		sw  	$5, 4($sp)
		sw  	$6, 8($sp)
		sw  	$7, 12($sp)
		sw  	$8, 16($sp)
		sw  	$9, 20($sp)
		sw  	$10,24($sp)
		sw  	$11,28($sp)
		sw  	$12,32($sp)
		sw  	$13,36($sp)
		sw  	$14,40($sp)
		sw  	$15,44($sp)
		sw  	$16,48($sp)
		sw  	$17,52($sp)
		sw  	$18,56($sp)
		sw  	$19,60($sp)
		sw  	$20,64($sp)
		sw  	$21,68($sp)
		sw  	$22,72($sp)
		sw  	$23,76($sp)
		sw  	$31,80($sp)
		/* Set up entry parameters, the RNC code always uses a0, a1 for */
		/* the I/O datastream and we must use R3K registers A0,A1 */
		move	xa0,a0		       		/* move input,a0 */
		jal	pp_Unpack			/* bsr	pp_Unpack */
		 move	xa1,a1				/* move output,a1 */

	/*
		Restore previously saved registers here
	*/
		lw  	$4, 0($sp)
		lw  	$5, 4($sp)
		lw  	$6, 8($sp)
		lw  	$7, 12($sp)
		lw  	$8, 16($sp)
		lw  	$9, 20($sp)
		lw  	$10,24($sp)
		lw  	$11,28($sp)
		lw  	$12,32($sp)
		lw  	$13,36($sp)
		lw  	$14,40($sp)
		lw  	$15,44($sp)
		lw  	$16,48($sp)
		lw  	$17,52($sp)
		lw  	$18,56($sp)
		lw  	$19,60($sp)
		lw  	$20,64($sp)
		lw  	$21,68($sp)
		lw  	$22,72($sp)
		lw  	$23,76($sp)
		lw  	$31,80($sp)

		j	ra				/* rts */
		 add	$sp, $sp, 88
pp_Unpack:
		sub	const_ff,zero,1			/* Set up constant 0xff */
		and	const_ff,const_ff,0x00ff
		sll	const_ff000000,const_ff,24	/* Set up constant 0xff000000 */
		sll	const_80000000,const_ff,31	/* Set up constant 0x80000000 */
		srl	const_8000,const_80000000,16	/* Set up constant 0x8000 */
		nor	const_ffffff00,zero,const_ff	/* Set up constant 0xffffff00 */

		sub	$sp,$sp,8+BUFSIZE		/* sub.w	  #bufsize,sp, store link address */
		add	xa2,$sp,8			/* move.w  sp,a2 */
		sw	ra,($sp)
            	lw	d0, 4(xa0)			/* get uncomp'd length */
		add	xa0,xa0,8 			/* skip past comp'd and uncomp'd lengths */
		add	input,xa0,HEADER_LEN-8		/* lea     HEADER_LEN-8(a0),input (skip past rest of header) */
		move	output,xa1			/* move.l  a1,output */
		lbu	bit_buffer,1(input)		/* move.b  1(input),bit_buffer */
		lbu	tmp,0(input)			/* move.b  0(input),bit_buffer (after bit_buffer is rotated) */
		move	bit_count,zero			/* moveq.l #0,bit_count */
		sll	bit_buffer,bit_buffer,8		/* " " rotate high byte into position */
		or	bit_buffer,bit_buffer,tmp	/* " " and or it in to the bit_buffer */
		add	output_hi,output,d0		/* lea     (output,d0.l),output_hi */
		add	d0,zero,2			/* moveq.l #2,d0 */
		jal	input_bits			/* bsr     input_bits */
		 add	d1,zero,2			/* moveq.l #2,d1 */
unpack2:
	/* */
	/* Fill the hufftables with a set value so we can see if they work */
	/* */
            	jal	make_huftable			/* bsr     make_huftable */
            	 move	xa0,xa2				/* move.l  a2,a0 */
            	jal	make_huftable			/* bsr     make_huftable */
            	 add	xa0,xa2,POS_TABLE		/* lea     POS_TABLE(a2),a0 */
            	jal	make_huftable			/* bsr     make_huftable */
            	 add	xa0,xa2,LEN_TABLE 		/* lea     LEN_TABLE(a2),a0 */
            	add	d0,zero,-1			/* moveq.l #-1,d0 */
            	jal	input_bits			/* bsr     input_bits */
            	 add	d1,zero,16			/* moveq.l #16,d1 */
            	move	counts,d0			/* move.w  d0,counts */
		sub	tmp,zero,1
		srl	tmp,tmp,16
            	sub	counts,counts,1			/* subq.w  #1,counts */
            	j	unpack5				/* bra.s   unpack5 */
		 and	counts,tmp,counts
unpack3:
            	add	xa0,xa2,POS_TABLE   		/* lea     POS_TABLE(a2),a0 */
            	jal	input_value			/* bsr.s   input_value */
            	 move	d0,zero				/* moveq.l #0,d0 */
            	subu	d0,zero,d0			/* neg.l   d0 */
            	add	xa1,output,d0			/* lea     -1(output,d0.l),a1 */
		add	xa1,xa1,-1			/* " " */
            	jal	input_value			/* bsr.s   input_value */
            	 add	xa0,xa2,LEN_TABLE		/* lea     LEN_TABLE(a2),a0 */
		add	output,output,1			/* move.b  (a1)+,(output)+ */
            	lbu	tmp,(xa1)			/* " " */
		add	xa1,xa1,1			/* " " */
		sb	tmp,-1(output)			/* " " */
                addu    tmp2,xa1,d0
.global unpack4
unpack4:
		add	output,output,1			/* move.b  (a1)+,(output)+ */
            	lbu	tmp,(xa1)			/* " " */
		sb	tmp,-1(output)			/* " " */
  		bne	xa1,tmp2,unpack4		/* dbra    d0,unpack4 */
		 add	xa1,xa1,1
unpack5:
            	jal	input_value			/* bsr.s   input_value */
            	 move	xa0,xa2				/* move.l  a2,a0 */
		sub	d0,d0,1				/* subq.w  #1,d0 */
		bltz	d0,unpack6			/* bmi.s   unpack6 */
		 nop
		jal	getrawREP			/* getrawREP */
		 nop
            	lbu	tmp,1(input)			/* move.b  1(input),d0 */
            	lbu	d0,0(input)			/* move.b  (input),d0 */
		sll	tmp,tmp,8			/* rol.w   #8,d0 */
		or	d0,tmp,d0			/* " " */

		sll	d0,d0,bit_count			/* lsl.l   bit_count,d0 */
            	add	d1,zero,1			/* moveq.l #1,d1 */
            	sll	d1,d1,bit_count			/* lsl.w   bit_count,d1 */
            	subu	d1,d1,1				/* subq.w  #1,d1 */
            	and	bit_buffer,bit_buffer,d1	/* and.l   d1,bit_buffer */
            	or	bit_buffer,d0,bit_buffer	/* or.l    d0,bit_buffer */
unpack6:
		bne	counts,zero,unpack3		/* dbra    counts,unpack3 */
		 sub	counts,counts,1
            	sub	tmp,output,output_hi		/* cmp.l   output_hi,output */
		bltz	tmp,unpack2			/* bcs.s	  unpack2 */
		 nop					/* " " */
		lw	ra,($sp)			/* Restore from linked jumps */
		jr	ra				/* rts */
		 add	$sp, $sp, 8 + BUFSIZE		/* lea     BUFSIZE(sp),sp */
input_value:
                lhu     d0,0(xa0)			/* move.w  (a0)+,d0 */
            	and	d0,d0,bit_buffer		/* and.w   bit_buffer,d0 */
                lhu     tmp,2(xa0)			/* move.w  (a0)+,d0 */
		sub	d0,d0,tmp			/* " " */
		bne	d0,zero,input_value		/* bne.s   input_value */
		 add	xa0,xa0,4			/* " " */
            	lbu	d1,16*4-4(xa0)			/* move.b  16*4-4(a0),d1 */
            	sub	bit_count,bit_count,d1		/* sub.b   d1,bit_count */
            	bgezl	bit_count,input_value2		/* bge.s   input_value2 */
            	 srl	bit_buffer,bit_buffer,d1	/* lsr.l   d1,bit_buffer */
		sw	ra,-8($sp)
            	jal	input_bits3			/* bsr.s   input_bits3 */
		 sub	$sp,$sp,8			/* Preserve link address for return*/
		lw	ra,($sp)			/* Restore from linked jumps */
		add	$sp,$sp,8
input_value2:
            	lbu	d0,16*4-3(xa0)			/* move.b  16*4-3(a0),d0 */
            	sub	tmp,d0,2			/* cmp.b   #2,d0 */
            	bltz	tmp,input_value4		/* blt.s   input_value4 */
            	 nop
            	sub	d0,d0,1				/* subq.b  #1,d0 */
            	move	d1,d0				/* move.b  d0,d1 */
            	move	d2,d0				/* move.b  d0,d2 */
            	lhu	d0,16*4-2(xa0)			/* move.w  16*4-2(a0),d0 */
            	sub	bit_count,bit_count,d1		/* sub.b   d1,bit_count */
            	and	d0,d0,bit_buffer		/* and.w   bit_buffer,d0 */
		add	tmp,zero,1
            	sll	tmp,tmp,d2			/* bset    d2,d0 */
            	bltz	bit_count,input_bits3		/* bge.s   input_value3 */
		 or	d0,tmp,d0			/* " " */
            	srl	bit_buffer,bit_buffer,d1	/* lsr.l   d1,bit_buffer */
input_value4:
		jr	ra
		nop					/*(Delay Slot) */

/*------------------------------------------------------------------------------
/ input_bits - Entry:
//			D0 - Mask to and output data with
//			D1 - Number of bits to fetch
//	       Exit:
//			D0 - Value read from buffer
//------------------------------------------------------------------------------
*/
input_bits:
            	sub	bit_count,bit_count,d1		/* sub.b   d1,bit_count */
            	bgez	bit_count,input_bits_ret	/* bge.s   input_bits2 */
            	 and	d0,bit_buffer,d0		/* and.w   bit_buffer,d0 */
input_bits3:
            	add	bit_count,bit_count,d1		/* Make sure we have correct # of bits left in buffer */
            	srl	bit_buffer,bit_buffer,bit_count	/* lsr.l   bit_count,bit_buffer */

		add	input,input,2			/* jump passed our data for some odd reason */
		lbu	tmp,0(input)			/* Get our new word to add into bit buffer */
		lbu	tmp2,1(input)			/* byte at a time..... */
		sll	tmp,tmp,16			/* shift this into the right place */
		sll	tmp2,tmp2,24
		or	bit_buffer,bit_buffer,tmp2	/* OR in the 2 new bytes read to the bit buffer */
		or	bit_buffer,bit_buffer,tmp

		sub	d1,d1,bit_count			/* sub.b   bit_count,d1 */
            	add	bit_count,zero,16		/* moveq.l #16,bit_count */
            	sub	bit_count,bit_count,d1		/* sub.b   d1,bit_count */
input_bits_ret:
		jr	ra				/* rts */
            	 srl	bit_buffer,bit_buffer,d1	/*(Delay Slot)lsr.l   d1,bit_buffer */
/*------------------------------------------------------------------------------
// Make_HufTable - Builds huffman decode table
//------------------------------------------------------------------------------
*/
make_huftable:
		sub	$sp,$sp,8			/* Preserve link address for return*/
		sw	ra,0($sp)
		add	d0,zero,0x1f			/* moveq.l #$1f,d0 */
		jal	input_bits			/* bsr.s   input_bits */
            	 add	d1,zero,0x5			/* moveq.l #5,d1 */
		lw	ra,($sp)			/* Restore from linked jumps */

		sub	d0,d0,1				/* subq.w  #1,d0 */
		bltz	d0,make_huftable8		/* bmi.s   make_huftable8 */
		 add	$sp,$sp,8
            	move	d2,d0				/* move.w  d0,d2 */
            	move	d3,d0				/* move.w  d0,d3 */
            	sub	$sp,$sp,24			/* lea     -16(sp),sp */
            	add	xa1,$sp,8			/* move.l  sp,a1 */
		sw	ra,0($sp)			/* Preserve link address for return*/
make_huftable3:
		add	d0,zero,15			/* moveq.l #$f,d0 */
		jal	input_bits			/* bsr.s   input_bits */
            	 add	d1,zero,4			/* moveq.l #4,d1 */
            	sb	d0,(xa1)			/* move.b  d0,(a1)+ */
		add	xa1,xa1,1			/* " " */
		bne	d2,zero,make_huftable3		/* dbra    d2,make_huftable3 */
		 sub	d2,d2,1
		lw	ra,($sp)			/* Restore from linked jumps */
		move	d0,const_80000000		/* moveq.l #1,d0 (with shift == 0x8000000) */
            	add	d1,zero,1			/* moveq.l #1,d1 */
		move	d2,zero				/* moveq.l #0,d2 */
            	sub	$sp,$sp,8			/* movem.l d5-d7,-(sp) */
		sw	d5,0($sp)			/* " " */
		sw	d6,4($sp)			/* " " */
		sw	d7,8($sp)			/* " " */
make_huftable4:
            	move	d4,d3				/* move.w  d3,d4 */
            	add	xa1,$sp,16			/* lea     12(sp),a1 */
make_huftable5:
            	lbu	tmp,0(xa1)			/* cmp.b   (a1)+,d1 */
		bne	d1,tmp,make_huftable7		/* bne.s   make_huftable7 */
		 add	xa1,xa1,1			/* " " */
            	add	d5,zero,1			/* moveq.l #1,d5 */
            	sll	d5,d5,d1			/* lsl.w   d1,d5 */
            	sub	d5,d5,1				/* subq.w  #1,d5 */
		sh	d5,0(xa0)			/* move.w  d5,(a0)+ */
		add	xa0,xa0,2			/* " " */
            	move	d5,d2				/* move.l  d2,d5 */
		sll	tmp,d5,16			/* swap 	  d5 */
		srl	d5,d5,16			/* " " */
		or	d5,d5,tmp			/* " " */
            	move	d7,d1				/* move.w  d1,d7 */
            	sub	d7,d7,1				/* subq.w  #1,d7 */
make_huftable6:
		sub	tmp,zero,1
		srl	tmp,tmp,16
		and	d6,tmp,d6
		and	tmp,d5,const_8000		/* roxl.w  #1,d5 */
		sll	d5,d5,1				/* " " */
		beq	tmp,zero,make_ht6_1		/* roxr.w  #1,d6 */
            	 srl	d6,d6,1				/* " " */
		or	d6,d6,const_8000		/* " " */
make_ht6_1:	bne    	d7,zero,make_huftable6		/* dbra    d7,make_huftable6 */
		sub	d7,d7,1
		add	d5,zero,16			/* moveq.l #16,d5 */
		sub	d5,d5,d1			/* sub.b   d1,d5 */
		srl	d6,d6,d5			/* lsr.w   d5,d6 */
		sh	d6,0(xa0)			/* move.w  d6,(a0)+ */
		add	xa0,xa0,2			/* " " */
            	sb	d1,16*4-4(xa0)			/* move.b  d1,16*4-4(a0) */
            	move	d5,d3				/* move.b  d3,d5 */
            	subu	d5,d5,d4			/* sub.b   d4,d5 */
            	sb	d5,16*4-3(xa0)			/* move.b  d5,16*4-3(a0) */
            	add	d6,zero,1			/* moveq.l #1,d6 */
            	subu	d5,d5,1				/* subq.b  #1,d5 */
            	sll	d6,d6,d5			/* lsl.w   d5,d6 */
            	subu	d6,d6,1				/* subq.w  #1,d6 */
		sh	d6,16*4-2(xa0)                  /* move.w  d6,16*4-2(a0) */
            	addu	d2,d2,d0			/* add.l   d0,d2 */
make_huftable7:
            	bne    	d4,zero,make_huftable5		/* dbra    d4,make_huftable5 */
		sub	d4,d4,1
            	srl	d0,d0,1				/* lsr.l   #1,d0 */
            	add	d1,d1,1				/* addq.b  #1,d1 */
		add	tmp,zero,17			/* cmp.b   #17,d1 */
		bne	d1,tmp,make_huftable4		/* bne.s   make_huftable4 */
            	lw	d5,0($sp) 			/* movem.l (sp)+,d5-d7 */
		lw	d6,4($sp)			/* " " */
		lw	d7,8($sp)			/* " " */
		jr	ra
            	 add	$sp,$sp,32			/* lea 16(sp),sp */
getrawREP:
		lbu	tmp,0(input)
		sb	tmp,0(output)
		add	output,output,1
		add	input,input,1
		bne    	d0,zero,getrawREP		/* dbra   d0,getrawREP */
		 sub	d0,d0,1
make_huftable8:
		jr	ra
		nop					/*(Delay Slot) */

.end Propack_UnpackM1
