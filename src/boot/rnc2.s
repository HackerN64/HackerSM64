/*------------------------------------------------------------------------------
* PROPACK Unpack Source Code - Sony PSX, Method 2
*
* Copyright (c) 1991-95 Rob Northen Computing, U.K. All Rights Reserved.
*
* Conversion from 68000 by Chris Kirby, Iguana Entertainment Limited.
* Converted to compile with SGI as, 9/13/96 Biscuit
*		Beware the commentless void.........
*
* File: rnc2.s
*
* Date: 16.03.95
*------------------------------------------------------------------------------*/

#define header_len     	18
#define crc_poly        0xa001

#define r0		$0
#define r4		$4
#define r5		$5

#define input 		$3
#define input_hi 	$4
#define output		$5
#define output_hi	$6
#define len		$8
#define pos		$9
#define bitbuf		$10
#define xsp		$11
#define temp		$12
#define xd0		$13
#define	xa0		$14
#define ext		$15
#define xd1		$16
#define xa1		$17
#define temp2		$18
#define postop		$19
#define ra			$31
#define retval		$2

	.text
	.set noreorder
	.option pic0

	.globl	Propack_UnpackM2
	.ent	Propack_UnpackM2 2
Propack_UnpackM2:

 	.frame	$sp, 0, $31

	sub	$sp, $sp, 72
	sw		$4,	0($sp)
	sw		$5,	4($sp)
	sw		$6,	8($sp)
	sw		$7,	12($sp)
	sw		$8,	16($sp)
	sw		$9,	20($sp)
	sw		$10,	24($sp)
	sw		$11,	28($sp)
	sw		$12,	32($sp)
	sw		$13,	36($sp)
	sw		$14,	40($sp)
	sw		$15,	44($sp)
	sw		$16,	48($sp)
	sw		$17,	52($sp)
	sw		$18,	56($sp)
	sw		$19,	60($sp)
	sw		$31,	64($sp)
	la		temp, __PPUnpack
	nop
	jalr	temp
	nop

	lw		$4,	0($sp)
	lw		$5,	4($sp)
	lw		$6,	8($sp)
	lw		$7,	12($sp)
	lw		$8,	16($sp)
	lw		$9,	20($sp)
	lw		$10,	24($sp)
	lw		$11,	28($sp)
	lw		$12,	32($sp)
	lw		$13,	36($sp)
	lw		$14,	40($sp)
	lw		$15,	44($sp)
	lw		$16,	48($sp)
	lw		$17,	52($sp)
	lw		$18,	56($sp)
	lw		$19,	60($sp)
	lw		$31,	64($sp)

	add	$sp, $sp, 72
	jr		ra

__PPUnpack:
/*        addi  r4,r4,9 - what the hell was this for?*/
        move  xa0,r4          /* source */
        move  xa1,r5          /* dest */
        li    xd0,0

        la   xsp,mystack
#if 0
			lbu	temp,0(xa0)
			nop
			seq	xa0,temp, "R"
			beq	xa0,0, RNC_Error

			lbu	temp,1(xa0)
			nop
			seq	xa0,temp, "N"
			beq	xa0,0, RNC_Error

			lbu	temp,2(xa0)
			nop
			seq	xa0,temp, "C"
			beq	xa0,0, RNC_Error

			lbu	temp,3(xa0)
			nop
			seq	xa0, temp, 2
			beq	xa0, 0, RNC_BadFormat
#endif


        addiu xa0,xa0,4
        lbu   xd0,0(xa0)
        lbu   temp,1(xa0)
        lbu   temp2,2(xa0)
        lbu   ext,3(xa0)
        sll   xd0,xd0,24
        sll   temp,temp,16
        sll   temp2,temp2,8
        or    xd0,xd0,temp
        or    xd0,xd0,temp2
        or    xd0,xd0,ext

        addiu xa0,xa0,4

        sw    xd0,0(xsp)
        move  input,xa0
        addi  input,input,header_len-8

        move  output,xa1
        move  output_hi,output
        addu   output_hi,output_hi,xd0

        lbu   xd0,0(xa0)
        lbu   temp,1(xa0)
        lbu   temp2,2(xa0)
        lbu   ext,3(xa0)
        sll   xd0,xd0,24
        sll   temp,temp,16
        sll   temp2,temp2,8
        or    xd0,xd0,temp
        or    xd0,xd0,temp2
        or    xd0,xd0,ext


        addu  xa0,xa0,4

        move  input_hi,input
        add   input_hi,input_hi,xd0

        sub  xsp,xsp,2
        sh    r0,0(xsp)


unpack7:
        lui   bitbuf,0xffff
        ori   bitbuf,0xff00

        li    ext,1

        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8

        j     xloop
        andi  ext,ext,1

RNC_Error:
			li		retval, -1			/* Error Bad Header */
			j		ra
			nop

RNC_BadFormat:
			li		retval, -2			/* Error Unknown format */
			j		ra
			nop

fetch0:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back0
        andi  ext,ext,1
fetch1:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back1
        andi  ext,ext,1
fetch2:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back2
        andi  ext,ext,1
fetch3:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back3
        andi  ext,ext,1
fetch4:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back4
        andi  ext,ext,1
fetch5:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back5
        andi  ext,ext,1
fetch6:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back6
        andi  ext,ext,1
fetch7:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back7
        andi  ext,ext,1


raw:
        li    len,3

x4bits:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch0
        nop


back0:
        addu  pos,pos,pos
        addu  pos,pos,ext
        srl   ext,pos,16
        andi  ext,ext,1

        sub  len,len,1
        bgez  len,x4bits
        nop
        addi  pos,pos,2

getrawrep2:
        lbu   temp,0(input)
        lbu   temp2,1(input)
        sb    temp,0(output)
        sb    temp2,1(output)

        lbu   temp,2(input)
        lbu   temp2,3(input)
        sb    temp,2(output)
        sb    temp2,3(output)



        addiu input,input,4
        sub  pos,pos,1

        bgez  pos,getrawrep2
        addi  output,output,4

        j     xloop
        nop

getlen:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch1
        nop
back1:
        addu  len,len,len
        addu  len,len,ext

        addu  bitbuf,bitbuf,bitbuf

        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch2
        nop


back2:
        beq   r0,ext,copy
        nop
        sub  len,len,1
        add   bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch3
        nop

back3:
        addu  len,len,len
        addu  len,len,ext
        srl   ext,len,16
        andi  ext,ext,1

        sub  temp,len,9
        beq   r0,temp,raw
        nop


copy:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch4
        nop


back4:
        beq   r0,ext,bytedisp2
        nop
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch5
        nop

back5:
        addu  pos,pos,pos
        addu  pos,pos,ext

        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch6
        nop

back6:
        bne   r0,ext,bigdisp
        nop
        bne   r0,pos,bytedisp
        nop
        addi  pos,pos,1

another:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch7
        nop

back7:
        addu  pos,pos,pos
        addu  pos,pos,ext
        srl   ext,pos,16
        andi  ext,ext,1

bytedisp:
        sll   temp,pos,8
        andi  temp,temp,0xff00
        srl   pos,pos,8
        or    pos,pos,temp


bytedisp2:
        andi  postop,pos,0xff00

        lbu   pos,0(input)

        addiu input,input,1
        or    pos,pos,postop
        move  xa0,output
        sub   xa0,xa0,pos
        sub  xa0,xa0,1
        andi  ext,len,1
        srl   len,len,1
        beq   r0,ext,bytedisp3
        nop

        lbu   temp,0(xa0)
        addi  xa0,xa0,1
        sb    temp,0(output)
        addi  output,output,1


bytedisp3:
        sub  len,len,1
        bne   r0,pos,bytedisp5
        nop

        andi  postop,pos,0xff00
        lbu   pos,0(xa0)
        nop
        or    pos,pos,postop
bytedisp4:
        sb    pos,0(output)
        sub  len,len,1
        sb    pos,1(output)

        bgez  len,bytedisp4
        addi  output,output,2

        j     xloop
        nop
bytedisp5:
        lbu   temp2,1(xa0)
        lbu   temp,0(xa0)
        sb    temp2,1(output)
        sb    temp,0(output)

        addi  xa0,xa0,2

        sub  len,len,1
        bgez  len,bytedisp5
        addi  output,output,2

        j     xloop
        nop


getbits:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        andi  ext,ext,1
        bne   r0,ext,string
        nop

xbyte:
        lbu   temp,0(input)
        addi  input,input,1
        sb    temp,0(output)
        addi  output,output,1


xloop:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        bne   r0,ext,chkz
        nop

        lbu   temp,0(input)
        addi  input,input,1
        sb    temp,0(output)
        addi  output,output,1

        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        beq   r0,ext,xbyte
        nop
chkz:
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,getbits
        nop


string:
        li    len,2
        li    pos,0

        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch8
        nop


back8:
        beq   r0,ext,getlen
        nop

smalls:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1

        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch9
        nop

back9:

        beq   r0,ext,bytedisp2
        nop
        addi  len,len,1
        add   bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1

        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch10
        nop

back10:
        beq   r0,ext,copy
        nop

        lbu   len,0(input)
        addi  input,input,1
        nop
        beq   r0,len,overnout
        addi  len,len,8
        j     copy
        nop

bigdisp:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1

        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch11
        nop

back11:
        addu  pos,pos,pos
        addu  pos,pos,ext
        srl   ext,pos,16
        andi  ext,ext,1
        ori   pos,pos,4

        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1

        andi  bitbuf,bitbuf,0xff
        beq   r0,bitbuf,fetch12
        nop

back12:
        bne   r0,ext,bytedisp
        nop
        j     another
        nop

fetch8:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back8
        andi  ext,ext,1
fetch9:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back9
        andi  ext,ext,1
fetch10:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back10
        andi  ext,ext,1
fetch11:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back11
        andi  ext,ext,1
fetch12:
        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        j     back12
        andi  ext,ext,1

overnout:
        addu  bitbuf,bitbuf,bitbuf
        srl   ext,bitbuf,8
        andi  ext,ext,1
        andi  bitbuf,bitbuf,0xff
        bne   r0,bitbuf,check4end
        nop

        lbu   bitbuf,0(input)
        addi  input,input,1
        addu  bitbuf,bitbuf,bitbuf
        addu  bitbuf,bitbuf,ext
        srl   ext,bitbuf,8
        andi  ext,ext,1

check4end:
        bne   r0,ext,xloop
        nop

        lhu   xd0,0(xsp)
        addi  xsp,xsp,2
        beq   r0,xd0,unpack9
        nop

unpack8:
        lh    xd1,0(xsp)
        addi  xsp,xsp,2
        sb    xd1,0(output)
        addi  output,output,1
        sub  xd0,xd0,1
        bne   r0,xd0,unpack8
        nop

unpack9:
        j     unpack11
        nop
unpack10:
        sw    xd1,0(xsp)

unpack11:
			li		retval,0
			jr		ra
        nop

	.data
        .align 4

        .word    0,0,0,0,0,0,0,0,0,0,0,0
mystack:
	.word    0

	.end
