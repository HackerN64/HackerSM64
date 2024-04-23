#pragma once

#include <ultra64.h>
#include <PR/R4300.h>
#include <PR/rcp.h>
#include <PR/rdb.h>

#include "types.h"


// -- COP0 --

// $Index
typedef union Reg_CP0_Index {
    struct PACKED {
        u32 P     :  1; // Probe success or failure. Set to 1 when the previous TLBProbe (TLBP) instruction was unsuccessful; set to 0 when successful.
        u32       : 25; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Index :  6; // Index to the TLB entry affected by the TLBRead and TLBWrite instructions.
    };
    u32 raw;
} Reg_CP0_Index;
// $Random
typedef union Reg_CP0_Random {
    struct PACKED {
        u32        : 26; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Random :  6; // TLB Random Index.
    };
    u32 raw;
} Reg_CP0_Random;
// $EntryLo0
// $EntryLo1
typedef union Reg_CP0_EntryLo_32 {
    struct PACKED {
        u32     :  6; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 PFN : 20; // Page frame number; the high-order bits of the physical address.
        u32 C   :  3; // Specifies the TLB page attribute. [0-7, 2 = cache is not used, otherwise cache is used.]
        u32 D   :  1; // Dirty. If this bit is set, the page is marked as dirty and, therefore, writable. This bit is actually a write-protect bit that software can use to prevent alteration of data.
        u32 V   :  1; // Valid. If this bit is set, it indicates that the TLB entry is valid; otherwise, a TLBL or TLBS miss occurs.
        u32 G   :  1; // Global. If this bit is set in both Entry Lo0 and Entry Lo1, then the processor ignores the ASID during TLB lookup
    };
    u32 raw;
} Reg_CP0_EntryLo_32;
typedef union Reg_CP0_EntryLo_64 {
    struct PACKED {
        u64     : 38; // RFU. Must be written as zeroes, and returns zeroes when read.
        u64 PFN : 20; // Page frame number; the high-order bits of the physical address.
        u64 C   :  3; // Specifies the TLB page attribute. [0-7, 2 = cache is not used, otherwise cache is used.]
        u64 D   :  1; // Dirty. If this bit is set, the page is marked as dirty and, therefore, writable. This bit is actually a write-protect bit that software can use to prevent alteration of data.
        u64 V   :  1; // Valid. If this bit is set, it indicates that the TLB entry is valid; otherwise, a TLBL or TLBS miss occurs.
        u64 G   :  1; // Global. If this bit is set in both Entry Lo0 and Entry Lo1, then the processor ignores the ASID during TLB lookup
    };
    u64 raw;
} Reg_CP0_EntryLo_64;
// $Context
typedef union Reg_CP0_Context_32 {
    struct PACKED {
        u32 PTEBase :  9; // Base address of page table entry.
        u32 BadVPN2 : 19; // Page number of virtual address whose translation is invalid divided by 2.
        u32         :  4; // RFU. Must be written zeroes; returns zeroes when read.
    };
    u32 raw;
} Reg_CP0_Context_32;
typedef union Reg_CP0_Context_64 {
    struct PACKED {
        u64 PTEBase : 41; // Base address of page table entry.
        u64 BadVPN2 : 19; // Page number of virtual address whose translation is invalid divided by 2.
        u64         :  4; // RFU. Must be written zeroes; returns zeroes when read.
    };
    u64 raw;
} Reg_CP0_Context_64;
// $PageMask
typedef union Reg_CP0_PageMask {
    struct PACKED {
        u32      :  9;
        u32 mask : 12; // [0b000000000000:4KB, 0b000000000011:16KB, 0b000000001111:64KB, 0b000000111111:256KB, 0b000011111111:1MB, 0b001111111111:4MB, 0b111111111111:16MB]
        u32      : 21;
    };
    u32 raw;
} Reg_CP0_PageMask;
// $Wired
typedef union Reg_CP0_Wired {
    struct PACKED {
        u32       : 26; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Wired :  6; // TLB Wired boundary.
    };
    u32 raw;
} Reg_CP0_Wired;
// $BadVAddr
// $Count
// $EntryHi
typedef union Reg_CP0_EntryHi_32 {
    struct PACKED {
        u32 VPN2 : 19; // Virtual page number divided by two (maps to two pages)
        u32      :  5; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 ASID :  8; // Address space ID field. An 8-bit field that lets multiple processes share the TLB; virtual addresses for each process can be shared.
    };
    u32 raw;
} Reg_CP0_EntryHi_32;
typedef union Reg_CP0_EntryHi_64 {
    struct PACKED {
        u64 R    :  2; // Region. [00:user, 01:supervisor, 11:Kernel] used to match vAddr. [63...62]
        u64 Fill : 22; // RFU. Writing this data to this area is ignored. 0 is returned when this bit area read.
        u64 VPN2 : 27; // Virtual page number divided by two (maps to two pages).
        u64      :  5; // RFU. Must be written as zeroes, and returns zeroes when read.
        u64 ASID :  8; // Address space ID field. An 8-bit field that lets multiple processes share the TLB; virtual addresses for each process can be shared.
    };
    u64 raw;
} Reg_CP0_EntryHi_64;
// $Compare
// $Status
typedef union Reg_CP0_Status {
    struct PACKED {
        u32 CU   : 4; // Controls the usability of each of the four coprocessor unit numbers. [1:usable, 0:unusable]
#undef RP // Previously defined in PR/region.h
        u32 RP   : 1; // Enables low-power operation by reducing the internal clock frequency and the system interface clock frequency to one-quarter speed. [0:normal, 1:low power mode (1/4th CPU clock speed)]
        u32 FR   : 1; // Enables additional floating-point registers. [0:16 registers, 1:32 registers]
        u32 RE   : 1; // Reverse-Endian bit, enables reverse of system endianness in User mode. [0:disabled (big endian), 1:reversed (little endian)]
        u32 DS   : 9; // Diagnostic status field.
        u32 IM   : 8; // Interrupt Mask field, enables external, internal, coprocessors or software interrupts (&’d against interrupt pending in $Cause). [0:disabled, 1:enabled]
        u32 KX   : 1; // Enables 64-bit addressing in Kernel mode. When this bit is set, XTLB miss exception is generated on TLB misses in Kernel mode addresses space (64-bit operation is always valid in Kernel mode). [0:32-bit, 1:64-bit]
        u32 SX   : 1; // Enables 64-bit addressing and operations in Supervisor mode. When this bit is set, XTLB miss exception is generated on TLB misses in Supervisor mode addresses space. [0:32-bit, 1:64-bit]
        u32 UX   : 1; // Enables 64-bit addressing and operations in User mode. When this bit is set, XTLB miss exception is generated on TLB misses in User mode addresses space. [0:32-bit, 1:64-bit]
        u32 KSU  : 2; // Specifies and indicates execution mode bits [10:User, 01:Supervisor, 00:Kernel]
        u32 ERL  : 1; // Specifies and indicates error level (are we currently handling an error?). [0:normal, 1:error]
        u32 EXL  : 1; // Specifies and indicates exception level (are we currently handling an exception?). [0:normal, 1:exception]
        u32 IE   : 1; // Specifies and indicates global interrupt enable (should interrupts be handled?). [0:disable interrupts, 1:enable interrupts]
    };
    struct PACKED {
        u32     :  7;
        u32 ITS :  1; // Enables Instruction Trace Support.
        u32     :  1; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 BEV :  1; // Controls the location of TLB miss and general purpose exception vectors. [0:normal, 1:bootstrap]
        u32 TS  :  1; // Indicates TLB shutdown has occurred (read-only); used to avoid damage to the TLB if more than one TLB entry matches a single virtual address. [0:does not occur, 1:occur] After TLB shutdown, the processor must be reset to restart. TLB shutdown can occur even when a TLB entry with which the virtual address has matched is set to be invalid (V bit of the entry is cleared).
        u32 SR  :  1; // Indicates whether a Soft Reset or NMI has occured. [0:has not occured, 1:has occured]
        u32     :  1; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 CH  :  1; // CP0 condition bit. Read/write access by software only; not accessible by hardware. [0:false, 1:true]
        u32 CE  :  1; // These bits are defined to maintain compatibility with the VR4200, and is not used by the hardware of the VR4300.
        u32 DE  :  1; // These bits are defined to maintain compatibility with the VR4200, and is not used by the hardware of the VR4300.
        u32     : 16;
    } DS_;
    struct PACKED {
        u32          : 16;
        u32 Timer    :  1; // Mask bit for timer interrupt.
        u32 External :  5; // Mask bits for external interrupts Int[4:0], or external write requests.
        u32 Software :  2; // Mask bits for software interrupts and IP(1:0) of the $Cause register.
        u32          :  8;
    } IM_;
    struct PACKED {
        u32 CP3 :  1; // Coprocessor 3 enabled This bit is ignored by the N64, there is no COP3!
        u32 CP2 :  1; // Coprocessor 2 enabled This bit is ignored by the N64, there is no COP2!
        u32 CP1 :  1; // Coprocessor 1 enabled If this bit is 0, all COP1 instructions throw exceptions.
        u32 CP0 :  1; // Coprocessor 0 enabled This bit is ignored by the N64, COP0 is always enabled!
        u32     : 28;
    } CU_;
    u32 raw;
} Reg_CP0_Status;
// $Cause
typedef union Reg_CP0_Cause {
    struct PACKED {
        u32 BD          :  1; // Indicates whether the last exception occurred has been executed in a branch delay slot. [1:delay, 0:normal]
        u32             :  1; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 CE          :  2; // Coprocessor unit number referenced when a Coprocessor Unusable exception has occurred. If this exception does not occur, undefined.
        u32             : 12; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 IP          :  8; // Indicates an interrupt is pending. [1:interrupt pending, 0:no interrupt]
        u32             :  1; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Exc_Code    :  5; // Exception code field.
        u32             :  2; // RFU. Must be written as zeroes, and returns zeroes when read.
    };
    struct PACKED {
        u32          : 16;
        u32 Timer    :  1; // Timer interrupt. This is connected to the $Count/$Compare interrupt mechanism inside COP0.
        u32 External :  5; // External normal interrupts. Controlled by Int[4:0], or external write requests.
        u32 Software :  2; // Software interrupts. Only these bits can cause interrupt exception when they are set to 1 by software. This is writable by MTC0, and is used as a “software interrupt”.
        u32          :  8;
    } IP_;
    struct PACKED {
        u32           : 17;
        u32 Indy_W    :  1; // Connected to the Indy dev kit’s RDB port. Set to 1 when a value is written.
        u32 Indy_R    :  1; // Connected to the Indy dev kit’s RDB port. Set to 1 when a value is read.
        u32 Reset     :  1; // This is connected to the Reset button on the top of the console. When pressed, this becomes 1.
        u32 Cartridge :  1; // This is connected to the cartridge slot. Cartridges with special hardware can trigger this interrupt. Unsure how common this is in practice.
        u32 MI        :  1; // This is connected to the MI interrupt process described above. It is set to 1 when ((MI_INTR_REG & MI_INTR_MASK_REG) != 0).
        u32           :  8;
    } IP_External_;
    u32 raw;
} Reg_CP0_Cause;
// $EPC
// $PRId
typedef union Reg_CP0_PRId {
    struct PACKED {
        // On N64, Company_Options and Company_Name are RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Company_Options :  8; // Relatively new addition, legacy processors have it defined as 0. https://en.wikichip.org/wiki/mips/prid_register.
        u32 Company_Name    :  8; // Relatively new addition, legacy processors have it defined as 0. https://en.wikichip.org/wiki/mips/prid_register.
        u32 Imp             :  8; // Processor ID number (0x0B for the VR4300 seriesTM).
        u32 Rev             :  8; // Processor revision number.
    };
    struct PACKED {
        u32       : 24;
        u32 major :  5;
        u32 minor :  3;
    } Rev_;
    u32 raw;
} Reg_CP0_PRId;
// $Config
typedef union Reg_CP0_Config {
    struct PACKED { //! TODO: return to this to add more documentation.
        u32    :  1; // 0b0
        u32 EC :  3; // Operating frequency ratio (read-only). The value displayed corresponds to the frequency ratio set by the DivMode pins on power application.
        u32 EP :  3; // Sets transfer data pattern (single/block write request). [0:D (default on cold reset), 6:DxxDxx:2 doublewords/6 cycles, others:RFU]
        u32    :  8; // 0b00000110
        u32 BE :  1; // Sets BigEndianMem (endianness). [0:Little endian, 1:Big endian (default on cold reset)]
        u32    : 11; // 0b11001000110
        u32 CU :  1; // RFU. However, can be read or written by software.
        u32 K0 :  3; // Sets coherency algorithm of kseg0. [0b10:Cache is not used, others:Cache is used]
    };
    u32 raw;
} Reg_CP0_Config;
// $LLAddr
// $WatchLo
typedef union Reg_CP0_WatchLo {
    struct PACKED {
        u32 PAddr0 : 29; // Bits 31:3 of the physical address.
        u32        :  1; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 R      :  1; // Exception occurs when load instruction is executed if set to 1.
        u32 W      :  1; // Exception occurs when store instruction is executed if set to 1.
    };
    u32 raw;
} Reg_CP0_WatchLo;
// $WatchHi
typedef union Reg_CP0_WatchHi {
    struct PACKED {
        u32        : 28; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 PAddr1 :  4; // Bits 35:32 of a physical address.
    };
    u32 raw;
} Reg_CP0_WatchHi;
// $XContext
typedef union Reg_CP0_XContext {
    struct PACKED {
        u64 PTEBase : 31; // Base address of page table entry
        u64 R       :  2; // Space identifier (bits 63 and 62 of virtual address). [0b00:User, 0b01:Supervisor, 0b11:Kernel]
        u64 BadVPN2 : 27; // Virtual address whose translation is invalid (bits 39:13).
        u64         :  4; // Must be written as zeroes, and returns zeroes when read.
    };
    u64 raw;
} Reg_CP0_XContext;
// $ParityErr
typedef union Reg_CP0_PErr {
    struct PACKED {
        u32            : 24; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Diagnostic :  8; // 8-bit self-diagnosis area.
    };
    u32 Raw;
} Reg_CP0_PErr;
// $CacheErr
// $TagLo
typedef union Reg_CP0_TagLo {
    struct PACKED {
        u32        :  4; // RFU. Must be written as zeroes; returns zeroes when read.
        u32 PTagLo : 20; // Physical address bits 31:12.
        u32 PState :  2; //! Specifies the primary cache state. [Data cache:[0b11:Valid, 0b00:invalid], Instruction cache:[0b10:Valid, 0b00:Invalid]]
        u32        :  6; // RFU. Must be written as zeroes; returns zeroes when read.
    };
    u32 raw;
} Reg_CP0_TagLo;
// $TagHi
// $ErrorEPC


// -- COP1 --

// FCR0: Coprocessor implementation/revision register
// FCR1 to FCR30: Reserved
// FCR31: Rounding mode, cause, exception enables, and flags

// $FCR0
typedef union Reg_FCR_0 {
    struct PACKED {
        u32     : 16; // RFU. Must be written as zeroes, and returns zeroes when read.
        u32 Imp :  8; // Processor ID number (0x0B for the VR4300 seriesTM).
        u32 Rev :  8; // Processor revision number.
    };
    struct PACKED {
        u32       : 24;
        u32 major :  5;
        u32 minor :  3;
    } Rev_;
    u32 raw;
} Reg_FCR_0;
// $FCR31 (FPCSR)
typedef union Reg_FPR_31 {
    struct PACKED {
        u32         :  7;
        u32 FS      :  1; // Flush denorm to zero.
        u32 C       :  1; // Condition bit.
        u32         :  5;
        u32 Cause   :  6; // See Reg_FPCSR.cause.
        u32 Enables :  5; // See Reg_FPCSR.enable.
        u32 Flags   :  5; // See Reg_FPCSR.flag.
        u32 RM      :  2; // Round to: [1:zero, 2:+inf, 3:-inf].
    };
    struct PACKED {
        u32   : 14;
        u32 E :  1; // cause: unimplemented operation
        u32 V :  1; // cause: invalid operation
        u32 Z :  1; // cause: division by zero
        u32 O :  1; // cause: overflow
        u32 U :  1; // cause: underflow
        u32 I :  1; // cause: inexact operation
        u32   : 12;
    } cause_;
    struct PACKED {
        u32   : 20;
        u32 V :  1; // enable: invalid operation
        u32 Z :  1; // enable: division by zero
        u32 O :  1; // enable: overflow
        u32 U :  1; // enable: underflow
        u32 I :  1; // enable: inexact operation
        u32   :  7;
    } enable_;
    struct PACKED {
        u32   : 25;
        u32 V :  1; // flag: invalid operation
        u32 Z :  1; // flag: division by zero
        u32 O :  1; // flag: overflow
        u32 U :  1; // flag: underflow
        u32 I :  1; // flag: inexact operation
        u32   :  2;
    } flag_;
    u32 raw;
} Reg_FPR_31;


// -- RDRAM--
// https://n64brew.dev/wiki/RDRAM#RDRAM_registers

// RDRAM_CONFIG_REG/RDRAM_DEVICE_TYPE_REG
#define RDRAM_CONFIG_COLUMN_BITS    0xF0000000
#define RDRAM_CONFIG_BN             0x04000000
#define RDRAM_CONFIG_EN             0x01000000
#define RDRAM_CONFIG_BANK_BITS      0x00F00000
#define RDRAM_CONFIG_ROW_BITS       0x000F0000
#define RDRAM_CONFIG_VERSION        0x000000F0
#define RDRAM_CONFIG_TYPE           0x0000000F
typedef union Reg_RDRAM_Status {
    struct PACKED {
        u32 ColumnBits : 4; // Number of column address bits, or said differently, declares that this RDRAM device has 2^ColumnBits bytes per row.
        u32            : 1;
        u32 Bn         : 1; // Bonus, number of bits per byte. [0:8bit byte, 1:9bit byte]
        u32            : 1;
        u32 En         : 1; // Enhanced speed grade. [0:Normal, 1:Low Latency]
        u32 BankBits   : 4; // Number of bank address bits, or said differently, declares that this RDRAM device has 2^BankBits banks.
        u32 RowBits    : 4; // Number of row address bits, or said differently, declares that this RDRAM devices has 2^RowBits rows per bank.
        u32            : 8;
        u32 Version    : 4; // RDRAM version. [0b0001:Extended architecture (Base RDRAM protocol), 0b0010:Concurrent RDRAM device (not used on N64, as far as we know)]
        u32 type       : 4; // Device type. [0b0000:RDRAM device]
    };
    u32 raw;
} Reg_RDRAM_Config;
// RDRAM_DEVICE_ID_REG
typedef union Reg_RDRAM_DeviceId {
    struct PACKED {
        u32 IdField_25_20 : 6; //
        u32               : 2; //
        u32 IdField_26    : 1; //
        u32               : 7; //
        u32 IdField_24_27 : 8; //
        u32 IdField_35    : 1; //
        u32               : 7; //
    };
    u32 raw;
} Reg_RDRAM_DeviceId;
// RDRAM_DELAY_REG
typedef union Reg_RDRAM_Delay {
    struct PACKED {
        u32             : 2; //
        u32 AckWinDelay : 3; // Adjusts the size of the acknowledge window. Normally set to 5 = 101b. [bits=tcycles : 101=5,110=6,111=7,000=8,001=9,010=10,011=11,100=12]
        u32 AckWinBits  : 3; // Read-only. Number of bits of AckWinDelay (3).
        u32             : 2; //
        u32 ReadDelay   : 3; // Delay between end of request and start of Read data packet. Normally set to 7 = 111b. Defaults to 8 = 001b after reset. [bits=tcycles : 111=7,000=8,001=9,010=10,011=11,100=12,101=13,110=14]
        u32 ReadBits    : 3; // Read-only. Number of bits of ReadDelay (3).
        u32             : 3; //
        u32 AckDelay    : 2; // Delay between end of request and start of Ack data packet. Normally set to 3 = 11b. [bits=tcycles = 11=3,00=4,01=5,10=6]
        u32 AckBits     : 3; // Read-only. Number of bits of AckDelay (2).
        u32             : 2; //
        u32 WriteDelay  : 3; // Delay between end of request and start of Write data packet. Normally set to 1 = 001b. Defaults to 4 = 100b after reset. [bits=tcycles : 001=1,010=2,011=3,100=4,101=5,110=6,111=7,000=8]
        u32 WriteBits   : 3; // Read-only. Number of bits of WriteDelay (3).
    };
    u32 raw;
} Reg_RDRAM_Delay;
// RDRAM_MODE_REG
#define RDRAM_MODE_CE_MASK 0x80000000
#define RDRAM_MODE_X2_MASK 0x40000000
#define RDRAM_MODE_PL_MASK 0x20000000
#define RDRAM_MODE_SV_MASK 0x10000000 // always 0
#define RDRAM_MODE_SK_MASK 0x08000000 // always 0
#define RDRAM_MODE_AS_MASK 0x04000000 // always 1
#define RDRAM_MODE_DE_MASK 0x02000000
#define RDRAM_MODE_LE_MASK 0x01000000
#define RDRAM_MODE_AD_MASK 0x00080000
#define RDRAM_MODE_C5_MASK 0x00800000
#define RDRAM_MODE_C4_MASK 0x00008000
#define RDRAM_MODE_C3_MASK 0x00000080
#define RDRAM_MODE_C2_MASK 0x00400000
#define RDRAM_MODE_C1_MASK 0x00004000
#define RDRAM_MODE_C0_MASK 0x00000040
#define RDRAM_MODE_CC_MASK (RDRAM_MODE_C5_MASK | RDRAM_MODE_C4_MASK | RDRAM_MODE_C3_MASK | RDRAM_MODE_C2_MASK | RDRAM_MODE_C1_MASK | RDRAM_MODE_C0_MASK) // 0x00C0C0C0
typedef union Reg_RDRAM_Mode {
    struct PACKED {
        u32 CE : 1; // CCEnable: Current Control Enable. [0:manual, 1:auto]
        u32 X2 : 1; // CCMult: Should be 1. Inverted when read. (Toshiba datasheet states that it select wether X1 or X2 register is used for the current control register).
        u32 PL : 1; // Select PowerDown Latency
        u32 SV : 1; // SkipValue: For tests. 0
        u32 SK : 1; // Skip: For tests. 0
        u32 AS : 1; // AutoSkip: For tests. 1
        u32 DE : 1; // DeviceEnable: Enable RDRAM device. When disabled, only broadcast register requests can be executed. [0=disabled,1=enabled]
        u32 LE : 1; // Enable PowerDown mode for RDRAM that supports it to reduce power consumption.
        u32 C5 : 1; //! TODO:
        u32 C2 : 1; //! TODO:
        u32    : 2;
        u32 AD : 1; // AckDis: For low latency RDRAM only. Allows to supress acknowledge response when set to 1.
        u32    : 3;
        u32 C4 : 1; //! TODO:
        u32 C1 : 1; //! TODO:
        u32    : 6;
        u32 C3 : 1; //! TODO:
        u32 C0 : 1; //! TODO:
    };
    u32 raw;
} Reg_RDRAM_Mode;
// RDRAM_REF_INTERVAL_REG
typedef union Reg_RDRAM_RefInterval {
    struct PACKED {
        u32 : 32;
    };
    u32 raw;
} Reg_RDRAM_RefInterval;
// RDRAM_REF_ROW_REG
typedef union Reg_RDRAM_RefRow {
    struct PACKED {
        u32 RowField_A : 7; // Current row being refreshed.
        u32            : 4;
        u32 BankField  : 1; // Current bank being refreshed.
        u32            : 3;
        u32            : 6;
        u32 RowField_B : 2; // Current row being refreshed.
        u32            : 8;
    };
    u32 raw;
} Reg_RDRAM_RefRow;
// RDRAM_RAS_INTERVAL_REG
typedef union Reg_RDRAM_RasInterval {
    struct PACKED {
        u32               : 3;
        u32 RowPrecharge  : 5; // Specify RowPrecharge timing.
        u32               : 3;
        u32 RowSense      : 5; // Specify RowSense timing.
        u32               : 3;
        u32 RowImpRestore : 5; // Specify RowImpRestore timing.
        u32               : 3;
        u32 RowExpRestore : 5; // Specify RowImpRestore timing.
    };
    u32 raw;
} Reg_RDRAM_RasInterval;
// RDRAM_MIN_INTERVAL_REG
typedef union Reg_RDRAM_MinInterval {
    struct PACKED {
        u32 MAD3     : 1; // MAD[3]: MinAckDelay: Minimum of AckDelay of RDRAM.
        u32 MRD3     : 1; // MRD[3]: MinReadDelay: Minimum of ReadDelay of RDRAM.
        u32 MWD3     : 1; // MWD[3]: MinWriteDelay: Minimum of WriteDelay of RDRAM.
        u32          : 5;
        u32 MAD2     : 1; // MAD[2]: MinAckDelay: Minimum of AckDelay of RDRAM.
        u32 MRD2     : 1; // MRD[2]: MinReadDelay: Minimum of ReadDelay of RDRAM.
        u32 MWD2     : 1; // MWD[2]: MinWriteDelay: Minimum of WriteDelay of RDRAM.
        u32          : 5;
        u32 MAD1     : 1; // MAD[1]: MinAckDelay: Minimum of AckDelay of RDRAM.
        u32 MRD1     : 1; // MRD[1]: MinReadDelay: Minimum of ReadDelay of RDRAM.
        u32 MWD1     : 1; // MWD[1]: MinWriteDelay: Minimum of WriteDelay of RDRAM.
        u32          : 5;
        u32 MAD0     : 1; // MAD[0]: MinAckDelay: Minimum of AckDelay of RDRAM.
        u32 MRD0     : 1; // MRD[0]: MinReadDelay: Minimum of ReadDelay of RDRAM.
        u32 MWD0     : 1; // MWD[0]: MinWriteDelay: Minimum of WriteDelay of RDRAM.
        u32 SpecFunc : 5; // SpecFunc: Performs various commands when written, see table below.
                          // 00000 = Nop      = Do nothing
                          // xxxx1 = SetRR    = Manual refresh. The device immediately preforms a single a burst refresh of two rows per bank
                          // x0x10 = ClrRE    = Disable automatic refresh
                          // x01xx = SetPD    = Enter the powerdown state
                          // ×1x0x = SetRE    = Enable automatic refresh
                          // 1xxxx = Reserved
    };
    u32 raw;
} Reg_RDRAM_MinInterval;
// RDRAM_ADDR_SELECT_REG
typedef union Reg_RDRAM_AddressSelect {
    struct PACKED {
        u32 SwapField_A :  7; // Each bit swaps two bits of the address. When all bits are 0, there is no swaping.
        u32             :  7;
        u32 SwapField_B :  2; // Each bit swaps two bits of the address. When all bits are 0, there is no swaping.
        u32             : 16;
    };
    u32 raw;
} Reg_RDRAM_AddressSelect;
// RDRAM_DEVICE_MANUF_REG
typedef union Reg_RDRAM_DeviceManufacturer {
    struct PACKED {
        u32 ManufactureCode : 16; // Manufacture is allowed to put whatever they want here?
        u32 Manufacture     : 16; // Code specifying the manufacturing company, see table below.
                                  // 0x0002 = Toshiba
                                  // 0x0003 = Fujitsu
                                  // 0x0005 = NEC
                                  // 0x0007 = Hitachi
                                  // 0x0009 = Oki
                                  // 0x000a = LG Semicon
                                  // 0x0010 = Samsung
                                  // 0x0013 = Hyundai

    };
    u32 raw;
} Reg_RDRAM_DeviceManufacturer;


// -- SP --
// https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#RSP_Internal_Registers

// DP_MEM_ADDR_REG
typedef union Reg_SP_MemAddr {
    struct PACKED {
        u32          : 19; // Undefined: Initialized to 0
        u32 MEM_BANK :  1; // Bank accessed by the transfer [0=DMEM,1=IMEM]
        u32 MEM_ADDR : 10; // DMEM or IMEM address used in SP DMAs. Notice that the lowest 3 bits are always 0.
    };
    u32 raw;
} Reg_SP_MemAddr;
// SP_DRAM_ADDR_REG
typedef union Reg_SP_DRAMAddr {
    struct PACKED {
        u32           :  8; // Undefined: Initialized to 0
        u32 DRAM_ADDR : 24; // RDRAM address used in SP DMAs. Notice that the lowest 3 bits are always 0.
    };
    u32 raw;
} Reg_SP_DRAMAddr;
// SP_RD_LEN_REG
typedef union Reg_SP_ReadLength {
    struct PACKED {
        u32 SKIP  : 12; // Number of bytes to skip in RDRAM after each row. Notice that the lowest 3 bits are always 0.
        u32 COUNT :  8; // Number of rows to transfer minus 1.
        u32 RDLEN : 12; // Number of bytes to transfer for each row minus 1. Notice that the lowest 3 bits are always 0.
    };
    u32 raw;
} Reg_SP_ReadLength;
// SP_WR_LEN_REG
typedef union Reg_SP_WriteLength {
    struct PACKED {
        u32 SKIP  : 12; // Number of bytes to skip in RDRAM after each row. Notice that the lowest 3 bits are always 0.
        u32 COUNT :  8; // Number of rows to transfer minus 1.
        u32 WRLEN : 12; // Number of bytes to transfer for each row minus 1. Notice that the lowest 3 bits are always 0.
    };
    u32 raw;
} Reg_SP_WriteLength;
// SP_STATUS_REG
typedef union Reg_SP_Status_read {
    struct PACKED {
        u32                    : 17;
        u32 signal_7_set       :  1;
        u32 signal_6_set       :  1;
        u32 signal_5_set       :  1;
        u32 signal_4_set       :  1; // cpusignal
        u32 signal_3_set       :  1; // rspsignal
        u32 signal_2_set       :  1; // taskdone
        u32 signal_1_set       :  1; // yielded
        u32 signal_0_set       :  1; // yield
        u32 interrupt_on_break :  1;
        u32 single_step        :  1;
        u32 io_full            :  1;
        u32 dma_full           :  1;
        u32 dma_busy           :  1;
        u32 broke              :  1;
        u32 halt               :  1;
    };
    u32 raw;
} Reg_SP_Status_read;
typedef union Reg_SP_Status_write {
    struct PACKED {
        u32                : 7;
        u32 set_signal_7   : 1;
        u32 clr_signal_7   : 1;
        u32 set_signal_6   : 1;
        u32 clr_signal_6   : 1;
        u32 set_signal_5   : 1;
        u32 clr_signal_5   : 1;
        u32 set_signal_4   : 1; // set cpusignal
        u32 clr_signal_4   : 1; // clr cpusignal
        u32 set_signal_3   : 1; // set rspsignal
        u32 clr_signal_3   : 1; // clr rspsignal
        u32 set_signal_2   : 1; // set taskdone
        u32 clr_signal_2   : 1; // clr taskdone
        u32 set_signal_1   : 1; // set yielded
        u32 clr_signal_1   : 1; // clr yielded
        u32 set_signal_0   : 1; // set yield
        u32 clr_signal_0   : 1; // clr yield
        u32 set_intr_break : 1;
        u32 clr_intr_break : 1;
        u32 set_sstep      : 1;
        u32 clr_sstep      : 1;
        u32 set_intr       : 1;
        u32 clr_intr       : 1;
        u32 clr_broke      : 1;
        u32 set_halt       : 1;
        u32 clr_halt       : 1;
    };
    u32 raw;
} Reg_SP_Status_write;
// SP_DMA_FULL_REG
// SP_DMA_BUSY_REG
// SP_SEMAPHORE_REG
// SP_PC_REG
// SP_IBIST_REG


// -- DPC --

// DPC_STATUS_REG
//! TODO: https://n64brew.dev/wiki/Reality_Signal_Processor/Interface#SP_STATUS
typedef union Reg_DPC_Status_write {
    struct PACKED {
        u32 clr_clock_ctr     : 1;
        u32 clr_cmd_ctr       : 1;
        u32 clr_pipe_ctr      : 1;
        u32 clr_tmem_ctr      : 1;
        u32 set_flush         : 1;
        u32 clr_flush         : 1;
        u32 set_freeze        : 1;
        u32 clr_freeze        : 1;
        u32 set_xbus_dmem_dma : 1;
        u32 clr_xbus_dmem_dma : 1;
    };
    u32 raw;
} Reg_DPC_Status_write;
typedef union Reg_DPC_Status_read {
    struct PACKED {
        u32               : 21;
        u32 start_valid   :  1;
        u32 end_valid     :  1;
        u32 dma_busy      :  1;
        u32 cbuf_busy     :  1;
        u32 cmd_busy      :  1;
        u32 pipe_busy     :  1;
        u32 tmem_busy     :  1;
        u32 start_gclk    :  1;
        // u32 frozen         : 1;
        u32 flush         :  1;
        u32 freeze        :  1;
        u32 xbus_dmem_dma :  1;
    };
    u32 raw;
} Reg_DPC_Status_read;

// -- DPS --

// -- MI --
// https://n64brew.dev/wiki/MIPS_Interface#Registers

// MI_INIT_MODE_REG/MI_MODE_REG
typedef union Reg_MI_Mode_write {
    struct PACKED {
        u32             : 18;
        u32 SetUpper    :  1; // Set Upper mode.
        u32 ClearUpper  :  1; // Clear Upper mode.
        u32 ClearDP     :  1; // Clear the DP Interrupt.
        u32 SetEBus     :  1; // Set Ebus mode.
        u32 ClearEBus   :  1; // Clear Ebus mode.
        u32 SetRepeat   :  1; // Set repeat mode. Automatically clears after a single Rambus write.
        u32 ClearRepeat :  1; // Clear repeat mode.
        u32 RepeatCount :  7; // Number of bytes (minus 1) to write in repeat mode.
    };
    u32 raw;
} Reg_MI_Mode_write;
typedef union Reg_MI_Mode_read {
    struct PACKED {
        u32             : 22;
        u32 Upper       :  1; // Upper mode enabled.
        u32 EBus        :  1; // EBus mode enabled.
        u32 Repeat      :  1; // Repeat mode enabled, Automatically clears after a single Rambus write.
        u32 RepeatCount :  7; // Number of bytes (minus 1) to write in repeat mode.
    };
    u32 raw;
} Reg_MI_Mode_read;
// MI_VERSION_REG/MI_NOOP_REG
typedef union Reg_MI_Version {
    struct PACKED {
        u32 RSP_VERSION : 8; // RSP hardware version.
        u32 RDP_VERSION : 8; // RDP hardware version.
        u32 RAC_VERSION : 8; // RAC hardware version.
        u32 IO_VERSION  : 8; // IO hardware version.
    };
    u32 raw;
} Reg_MI_Version;
// MI_INTR_REG
typedef union Reg_MI_Interrupt {
    struct PACKED {
        u32    : 26; // Undefined: Initialized to 0
        u32 DP :  1; // DP: Interrupt flag - Set when the RDP finishes a full sync (requested explicitly via a SYNC_FULL command).
        u32 PI :  1; // PI: Interrupt flag - Set when a PI DMA transfer finishes.
        u32 VI :  1; // VI: Interrupt flag - Set when the VI starts processing a specific half-line of the screen (VI_V_CURRENT == VI_V_INTR). Usually, this is configured with VI_V_CURRENT = 2 so that it behaves as a VBlank interrupt.
        u32 AI :  1; // AI: Interrupt flag - Set when the AI begins playing back a new audio buffer (to notify that the next one should be enqueued as soon as possible, to avoid crackings).
        u32 SI :  1; // SI: Interrupt flag - Set when a SI DMA to/from PIF RAM finishes
        u32 SP :  1; // SP: Interrupt flag - Set when the RSP executes a BREAK opcode while SP_STATUS has been configured with the INTERRUPT_ON_BREAK bit; alternatively, it can also be set by explicitly writing the INTERRUPT flag in the SP_STATUS register (by either the CPU or the RSP itself).
    };
    u32 raw;
} Reg_MI_Interrupt;
// MI_INTR_MASK_REG
typedef union Reg_MI_Mask_write {
    struct PACKED {
        u32         : 20;
        u32 SetDP   :  1; // Set DP Interrupt Mask.
        u32 ClearDP :  1; // Clear DP Interrupt Mask.
        u32 SetPI   :  1; // Set PI Interrupt Mask.
        u32 ClearPI :  1; // Clear PI Interrupt Mask.
        u32 SetVI   :  1; // Set VI Interrupt Mask.
        u32 ClearVI :  1; // Clear VI Interrupt Mask.
        u32 SetAI   :  1; // Set AI Interrupt Mask.
        u32 ClearAI :  1; // Clear AI Interrupt Mask.
        u32 SetSI   :  1; // Set SI Interrupt Mask.
        u32 ClearSI :  1; // Clear SI Interrupt Mask.
        u32 SetSP   :  1; // Set SP Interrupt Mask.
        u32 ClearSP :  1; // Clear SP Interrupt Mask.
    };
    u32 raw;
} Reg_MI_Mask_write;
typedef union Reg_MI_Mask_read {
    struct PACKED {
        u32    : 26;
        u32 DP :  1; // DP Interrupt Mask.
        u32 PI :  1; // PI Interrupt Mask.
        u32 VI :  1; // VI Interrupt Mask.
        u32 AI :  1; // AI Interrupt Mask.
        u32 SI :  1; // SI Interrupt Mask.
        u32 SP :  1; // SP Interrupt Mask.
    };
    u32 raw;
} Reg_MI_Mask_read;

// -- VI --

// VI_CONTROL_REG
#define VI_CTRL_PIXEL_ADVANCE_MASK  0x0F000
#define VI_CTRL_KILL_WE             0x00800
#define VI_CTRL_TEST_MODE           0x00080
#define VI_CTRL_VBUS_CLOCK_ENABLE   0x00020 //! TODO: Warning to never set this bit.
#define VI_CTRL_TYPE_MASK           0x00003

// -- AI--

// AI_STATUS_REG
#define AI_STATUS_ENABLED   0x03000000
#define AI_STATUS_WC        0x00080000
#define AI_STATUS_BC        0x00010000
#define AI_STATUS_COUNT     0x00007FFE
#define AI_STATUS_FULL2     0x00000001

// -- PI --

// PI_STATUS_REG
#define PI_STATUS_INTR 0x08

// -- RI --

// RI_MODE_REG
#define RI_MODE_STOP_R  0x08
#define RI_MODE_STOP_T  0x04
#define RI_MODE_OP_MODE 0x03
// RI_CONFIG_REG
#define RI_CONFIG_AUTO  0x40
#define RI_CONFIG_CC    0x3F
// RI_REFRESH_REG/RI_COUNT_REG
#define RI_REFRESH_MULTIBANK    0x00780000
#define RI_REFRESH_OPT          0x00040000
#define RI_REFRESH_EN           0x00020000
#define RI_REFRESH_BANK         0x00010000
#define RI_REFRESH_DIRTY        0x0000FF00
#define RI_REFRESH_CLEAN        0x000000FF

// -- SI --

// SI_STATUS_REG
#define SI_STATUS_DMA_STATE_MASK    0x0F00
#define SI_STATUS_PCH_STATE_MASK    0x00F0
#define SI_STATUS_READ_PENDING      0x0004
