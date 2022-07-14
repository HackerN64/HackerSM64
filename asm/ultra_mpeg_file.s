/******************************************************************************
 *        ultra_mpeg - An MPEG-1/2 decoder library for the Nintendo 64        *
 *                       Copyright (C) 2020  devwizard                        *
 *     This project is licensed under the terms of the MIT license.  See      *
 *     LICENSE for more information.                                          *
 ******************************************************************************/

.globl fopen
fopen:
    move    $v0, $0
    jr      $ra

.globl fseek
fseek:
    jr      $ra

.globl ftell
ftell:
    move    $v0, $0
    jr      $ra

.globl fclose
fclose:
    jr      $ra

.globl fread
fread:
    move    $v0, $0
    jr      $ra
