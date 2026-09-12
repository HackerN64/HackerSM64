##############################################
#  LZ4T - Fast decompressor in assembly
#  Written by aglab2 inspired by Rasky LZ4 and devwizard YAZ0
##############################################

# NOTE: to optimize for speed, this decompressor can write up to 8 bytes
# after the end of the output buffer. The outut buffer must have been sized
# accordingly to accomodate for this.

#define MINMATCH    4

#define inbuf       $s0
#define nibbles     $s1
#define outbuf      $s2
#define match_combo_mask $s3
#define len         $s4
#define match_lim   $s5
#define match_min   $s6
#define v0_st       $s7

#define dma_ctx     $s8
#define dma_ptr     $v0

#define shift       $t9
#define len_add     $t8
#define match_len   $t7
#define match_off   $t6
#define match_combo $t5
#define off_nibble  $t4

    .section .text.lz4t_unpack_fast
	.p2align 5
    .globl lz4t_unpack_fast 
    .func lz4t_unpack_fast
    .set at
    .set noreorder

lz4t_unpack_fast:
    addiu $sp, $sp, -0x40
    sw $ra, 0x14($sp)
    sw $s0, 0x18($sp)
    sw $s1, 0x1c($sp)
    sw $s2, 0x20($sp)
    sw $s3, 0x24($sp)
    sw $s4, 0x28($sp)
    sw $s5, 0x2C($sp)
    sw $s6, 0x30($sp)
    sw $s7, 0x34($sp)
    sw $s8, 0x38($sp)

    move $s0, $a0
    lw $s1, 12($a0)
    move $s2, $a1
    move dma_ctx, $a2
    lbu match_combo_mask, 8($a0)
    sll match_combo_mask, 28
    lbu match_min, 9($a0)

    move dma_ptr, $a0
    addiu $s0, 16

.Lloop:
    sub $t0, inbuf, dma_ptr                     # check if we need to wait for dma
    bgezal $t0, dma_async_ctx_read                    # if inbuf >= dma_ptr, wait for dma
     move $a0, dma_ctx

    bnez nibbles, .Lprocess_nibbles
     li match_lim, 7
     
.Lload_nibbles:
    lwl nibbles, 0(inbuf)
    lwr nibbles, 3(inbuf)
    beqz nibbles, .Lend
    add inbuf, 4

.Lprocess_nibbles:
    bgez nibbles, .Lmatches
     srl len, nibbles, 28

.Lliterals:
    andi len, 7
    beqz len, .Llarge_literals
     nop

.Lsmall_literal:
    ldl $t0, 0(inbuf)
    ldr $t0, 7(inbuf)
    add inbuf, len
    sdl $t0, 0(outbuf)
    sdr $t0, 7(outbuf)
    sll nibbles, 4
    beq len, match_lim, .Lloop
    add outbuf, len

.Lmatches_ex:
    sub $t0, inbuf, dma_ptr                     # check if we need to wait for dma
    bgezal $t0, dma_async_ctx_read                    # if inbuf >= dma_ptr, wait for dma
     move $a0, dma_ctx

    bnez nibbles, .Lprocess_ex_match_nibble
     li match_lim, 15
     
.Lload_nibbles2:
    lwl nibbles, 0(inbuf)
    lwr nibbles, 3(inbuf)
    beqz nibbles, .Lend
    add inbuf, 4

.Lprocess_ex_match_nibble:
    srl len, nibbles, 28

.Lmatches:
    lwl match_combo, 0(inbuf)
    lwr match_combo, 3(inbuf)
    addiu inbuf, 2
    srl match_off, match_combo, 16

    beqz match_combo_mask, .Lfull_offset
    sll nibbles, 4
    srl nibbles, 4
    and off_nibble, match_combo, match_combo_mask
    or nibbles, off_nibble
    andi match_off, 0xfff
.Lfull_offset:

    bne len, match_lim, .Lmatch
     addu match_len, len, match_min

    # len is sign extended match_combo[8:15]
    sll match_combo, 16
    sra len, match_combo, 24
    add inbuf, 1
    bltzal len, .Lread_large_amount
     andi len, 0x7f

    add match_len, len

.Lmatch:
    ble match_off, match_len, .Lmatch1_loop     # check if we can do 8-byte copy
     sub v0_st, outbuf, match_off                 # calculate start of match
.Lmatch8_loop:                                  # 8-byte copy loop
    ldl $t0, -1(v0_st)                             # load 8 bytes
    ldr $t0, 6(v0_st)
    addiu v0_st, 8
    sdl $t0, 0(outbuf)                          # store 8 bytes
    sdr $t0, 7(outbuf)
    addiu match_len, -8
    bgtz match_len, .Lmatch8_loop               # check we went past match_len
     addiu outbuf, 8
    b .Lloop                                    # jump to main loop
     addu outbuf, match_len                     # adjust pointer remove extra bytes

.Lmatch1_loop:                                  # 1-byte copy loop
    lbu $t0, -1(v0_st)                             # load 1 byte
    addiu v0_st, 1
    sb $t0, 0(outbuf)                           # store 1 byte
    addiu match_len, -1
    bgtz match_len, .Lmatch1_loop               # check we went past match_len
     addiu outbuf, 1
    b .Lloop                                    # jump to main loop
     nop

.Llarge_literals:
    lb len, 0(inbuf)
    add inbuf, 1
    bltzal len, .Lread_large_amount
     andi len, 0x7f

    move v0_st, inbuf                            # store start of literals into v0_st
    addiu len, 22
    add inbuf, len                        # advance inbuf to end of literals
.Lcopy_lit:
    sub $t0, v0_st, dma_ptr                     # check if all the literals have been DMA'd
    bgezal $t0, dma_async_ctx_read                       # if not, wait for DMA
     move $a0, dma_ctx
    ldl $t0, 0(v0_st)                             # load 8 bytes of literals
    ldr $t0, 7(v0_st)
    addiu v0_st, 8
    sdl $t0, 0(outbuf)                          # store 8 bytes of literals
    sdr $t0, 7(outbuf)
    addiu len, -8
    bgez len, .Lcopy_lit                  # check if we went past the end of literals
     addiu outbuf, 8
    addu outbuf, len                      # adjust outbuf to roll back extra copied bytes

    b .Lmatches_ex
    sll nibbles, 4

.Lend:
    lw $ra, 0x14($sp)
    lw $s0, 0x18($sp)
    lw $s1, 0x1c($sp)
    lw $s2, 0x20($sp)
    lw $s3, 0x24($sp)
    lw $s4, 0x28($sp)
    lw $s5, 0x2C($sp)
    lw $s6, 0x30($sp)
    lw $s7, 0x34($sp)
    lw $s8, 0x38($sp)
    jr $ra
    addiu $sp, $sp, 0x40

.Lread_large_amount:
    li shift, 7
.Lread_large_amount_loop:
    lb $t0, 0(inbuf)
    add inbuf, 1
    andi $t1, $t0, 0x7f
    sllv $t1, $t1, shift
    or len, $t1
    bltz $t0, .Lread_large_amount_loop
    add shift, 7
    jr $ra
    nop

.endfunc
