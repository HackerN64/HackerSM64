##############################################
#  LZ4 - Fast decompressor in assembly
#  Written by Rasky, adjusted for libultra by aglab2
##############################################

# NOTE: to optimize for speed, this decompressor can write up to 8 bytes
# after the end of the output buffer. The outut buffer must have been sized
# accordingly to accomodate for this.

#define MINMATCH    4

#define inbuf       $s0
#define inbuf_end   $s1
#define outbuf      $s2

#define match_off   $s3
#define match_len   $s4
#define token       $s5
#define outbuf_orig $s6
#define v0_st       $s7

#define dma_ctx     $s8
#define dma_ptr     $v0

    .section .text.decompress_lz4_full_fast
	.p2align 5
    .globl decompress_lz4_full_fast 
    .func decompress_lz4_full_fast
    .set at
    .set noreorder

decompress_lz4_full_fast:
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
    move $s1, $a1
    move $s2, $a2
    move dma_ctx, $a3

    add $s1, $s0                                # calculate end of input buffer
    move outbuf_orig, outbuf
    move dma_ptr, $a0
    addiu dma_ptr, dma_ptr, -16

.Lloop:
    sub $t0, inbuf, dma_ptr                     # check if we need to wait for dma
    bgezal $t0, dma_async_ctx_read                    # if inbuf >= dma_ptr, wait for dma
     move $a0, dma_ctx
    lbu token, 0(inbuf)                         # read token byte
    addiu inbuf, 1
    srl match_len, token, 4                     # extract literal length
    beqz match_len, .Lmatches                   # if literal length is 0, jump to matches
     addiu $t0, match_len, -0xF                 # check if literal length is 15
    bgezal $t0, .Lread_match_len                # if literal length is 15, read more
     nop
    move v0_st, inbuf                            # store start of literals into v0_st
    add inbuf, match_len                        # advance inbuf to end of literals
.Lcopy_lit:
    sub $t0, v0_st, dma_ptr                     # check if all the literals have been DMA'd
    bgezal $t0, dma_async_ctx_read                       # if not, wait for DMA
     move $a0, dma_ctx
    ldl $t0, 0(v0_st)                             # load 8 bytes of literals
    ldr $t0, 7(v0_st)
    addiu v0_st, 8
    sdl $t0, 0(outbuf)                          # store 8 bytes of literals
    sdr $t0, 7(outbuf)
    addiu match_len, -8
    bgez match_len, .Lcopy_lit                  # check if we went past the end of literals
     addiu outbuf, 8
    addu outbuf, match_len                      # adjust outbuf to roll back extra copied bytes

.Lmatches:
    bge inbuf, inbuf_end, .Lend                 # check if we went past the end of the input
     andi match_len, token, 0xF                 # extract match length    
    lbu match_off, 1(inbuf)                     # read 16-bit match offset (little endian)
    lbu $t0, 0(inbuf)
    addiu inbuf, 2
    sll match_off, 8
    or match_off, $t0
    
    addiu $t0, match_len, -0xF                  # check if match length is 15
    bgezal $t0, .Lread_match_len                # if match length is 15, read more
     addiu match_len, MINMATCH                  # add implicit MINMATCH to match length

.Lmatch:
    blt match_off, match_len, .Lmatch1_loop     # check if we can do 8-byte copy
     sub v0_st, outbuf, match_off                 # calculate start of match
.Lmatch8_loop:                                  # 8-byte copy loop
    ldl $t0, 0(v0_st)                             # load 8 bytes
    ldr $t0, 7(v0_st)
    addiu v0_st, 8
    sdl $t0, 0(outbuf)                          # store 8 bytes
    sdr $t0, 7(outbuf)
    addiu match_len, -8
    bgtz match_len, .Lmatch8_loop               # check we went past match_len
     addiu outbuf, 8
    b .Lloop                                    # jump to main loop
     addu outbuf, match_len                     # adjust pointer remove extra bytes

.Lmatch1_loop:                                  # 1-byte copy loop
    lbu $t0, 0(v0_st)                             # load 1 byte
    addiu v0_st, 1
    sb $t0, 0(outbuf)                           # store 1 byte
    addiu match_len, -1
    bgtz match_len, .Lmatch1_loop               # check we went past match_len
     addiu outbuf, 1
    b .Lloop                                    # jump to main loop
     nop

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

.Lread_match_len:                               # read extended match length
    addiu $sp, $sp, -0x18
    sw $ra, 0x14($sp)
.Lread_match_len_loop:
    sub $t0, inbuf, dma_ptr                     # check if we need to wait for dma
    bgezal $t0, dma_async_ctx_read                       # if inbuf >= dma_ptr, wait for dma
     move $a0, dma_ctx
    lbu $t0, 0(inbuf)                           # read 1 byte
    addiu inbuf, 1
    beq $t0, 0xFF, .Lread_match_len_loop        # if byte is 0xFF, continue reading
     add match_len, $t0                         # add to match_len
    lw $ra, 0x14($sp)
    jr $ra
    addiu $sp, $sp, 0x18

.endfunc
