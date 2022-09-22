# Makefile to rebuild SM64 split image

include util.mk

# Default target
default: all

TARGET_STRING := sm64

# Preprocessor definitions
DEFINES :=

SRC_DIRS :=
USE_DEBUG := 0

#==============================================================================#
# Build Options                                                                #
#==============================================================================#

# These options can either be set by building with 'make SETTING=value'.
# 'make clean' may be required first.

# Build for the N64 (turn this off for ports)
TARGET_N64 ?= 1

# CONSOLE - selects the console to target
#   bb - Targets the iQue Player (codenamed BB)
#   n64 - Targets the N64
CONSOLE ?= n64
$(eval $(call validate-option,CONSOLE,n64 bb))

ifeq      ($(CONSOLE),n64)
  INCLUDE_DIRS   += include/n64
  LIBS_DIR       := lib/n64
else ifeq ($(CONSOLE),bb)
  INCLUDE_DIRS   += include/ique
  LIBS_DIR       := lib/ique
  DEFINES        += BBPLAYER=1
endif

# COMPILER - selects the C compiler to use
#   gcc - uses the GNU C Compiler
#   clang - uses clang C/C++ frontend for LLVM
COMPILER ?= gcc
$(eval $(call validate-option,COMPILER,gcc clang))


# LIBGCCDIR - selects the libgcc configuration for checking for dividing by zero
#   trap - GCC default behavior, uses teq instructions which some emulators don't like
#   divbreak - this is similar to IDO behavior, and is default.
#   nocheck - never checks for dividing by 0. Technically fastest, but also UB so not recommended
LIBGCCDIR ?= divbreak
$(eval $(call validate-option,LIBGCCDIR,trap divbreak nocheck))


# SAVETYPE - selects the save type
#   eep4k - uses EEPROM 4kbit
#   eep16k - uses EEPROM 16kbit (There aren't any differences in syntax, but this is provided just in case)
#   sram - uses SRAM 256Kbit
SAVETYPE ?= eep4k
$(eval $(call validate-option,SAVETYPE,eep4k eep16k sram))
ifeq ($(SAVETYPE),eep4k)
  DEFINES += EEP=1 EEP4K=1
else ifeq ($(SAVETYPE),eep16k)
  DEFINES += EEP=1 EEP16K=1
else ifeq ($(SAVETYPE),sram)
  DEFINES += SRAM=1
endif

DEFINES += NO_ERRNO_H=1 NO_GZIP=1

# VERSION - selects the version of the game to build
#   jp - builds the 1996 Japanese version
#   us - builds the 1996 North American version
#   eu - builds the 1997 PAL version
#   sh - builds the 1997 Japanese Shindou version, with rumble pak support
VERSION ?= us
$(eval $(call validate-option,VERSION,jp us eu sh))

ifeq      ($(VERSION),jp)
  DEFINES += VERSION_JP=1
else ifeq ($(VERSION),us)
  DEFINES += VERSION_US=1
else ifeq ($(VERSION),eu)
  DEFINES += VERSION_EU=1
else ifeq ($(VERSION),sh)
  DEFINES += VERSION_SH=1
endif

# FIXLIGHTS - converts light objects to light color commands for assets, needed for vanilla-style lighting
FIXLIGHTS ?= 1

DEBUG_MAP_STACKTRACE_FLAG := -D DEBUG_MAP_STACKTRACE

TARGET := sm64


# GRUCODE - selects which RSP microcode to use.
#   f3dex   -
#   f3dex2  -
#   l3dex2  - F3DEX2 version that only renders in wireframe
#   f3dzex  - newer, experimental microcode used in Animal Crossing
#   super3d - extremely experimental version of Fast3D lacking many features for speed
GRUCODE ?= f3dzex
$(eval $(call validate-option,GRUCODE,f3dex f3dex2 f3dex2pl f3dzex super3d l3dex2))

ifeq ($(GRUCODE),f3dex) # Fast3DEX
  DEFINES += F3DEX_GBI=1 F3DEX_GBI_SHARED=1
else ifeq ($(GRUCODE),f3dex2) # Fast3DEX2
  DEFINES += F3DEX_GBI_2=1 F3DEX_GBI_SHARED=1
else ifeq ($(GRUCODE),l3dex2) # Line3DEX2
  DEFINES += L3DEX2_GBI=1 L3DEX2_ALONE=1 F3DEX_GBI_2=1 F3DEX_GBI_SHARED=1
else ifeq ($(GRUCODE),f3dex2pl) # Fast3DEX2_PosLight
  DEFINES += F3DEX2PL_GBI=1 F3DEX_GBI_2=1 F3DEX_GBI_SHARED=1
else ifeq ($(GRUCODE),f3dzex) # Fast3DZEX (2.08J / Animal Forest - Dōbutsu no Mori)
  DEFINES += F3DZEX_NON_GBI_2=1 F3DEX_GBI_2=1 F3DEX_GBI_SHARED=1
else ifeq ($(GRUCODE),super3d) # Super3D
  $(warning Super3D is experimental. Try at your own risk.)
  DEFINES += SUPER3D_GBI=1 F3D_NEW=1
endif

LIBRARIES := nustd hvqm2 z goddard

# TEXT ENGINES
#   s2dex_text_engine - Text Engine by someone2639
TEXT_ENGINE := none
ifeq ($(TEXT_ENGINE), s2dex_text_engine)
  DEFINES += S2DEX_GBI_2=1 S2DEX_TEXT_ENGINE=1
  LIBRARIES += s2d_engine
  DUMMY != make -C src/s2d_engine COPY_DIR=$(shell pwd)/lib/
endif
# add more text engines here

LINK_LIBRARIES = $(foreach i,$(LIBRARIES),-l$(i))


#==============================================================================#
# Optimization flags                                                           #
#==============================================================================#

# Default non-gcc opt flags
DEFAULT_OPT_FLAGS = -Ofast

# Main opt flags
GCC_MAIN_OPT_FLAGS = \
  -Ofast \
  --param case-values-threshold=20 \
  --param max-completely-peeled-insns=10 \
  --param max-unrolled-insns=10 \
  -finline-limit=1 \
  -freorder-blocks-algorithm=simple  \
  -ffunction-sections \
  -fdata-sections

# Surface Collision
GCC_COLLISION_OPT_FLAGS = \
  -Ofast \
  --param case-values-threshold=20 \
  --param max-completely-peeled-insns=100 \
  --param max-unrolled-insns=100 \
  -finline-limit=0 \
  -fno-inline \
  -freorder-blocks-algorithm=simple  \
  -ffunction-sections \
  -fdata-sections \
  -falign-functions=32

# Math Util
GCC_MATH_UTIL_OPT_FLAGS = \
  -Ofast \
  -fno-unroll-loops \
  -fno-peel-loops \
  --param case-values-threshold=20  \
  -ffunction-sections \
  -fdata-sections \
  -falign-functions=32
#   - setting any sort of -finline-limit has shown to worsen performance with math_util.c,
#     lower values were the worst, the higher you go - the closer performance gets to not setting it at all

# Rendering graph node
GCC_GRAPH_NODE_OPT_FLAGS = \
  -Ofast \
  --param case-values-threshold=20 \
  --param max-completely-peeled-insns=100 \
  --param max-unrolled-insns=100 \
  -finline-limit=0 \
  -freorder-blocks-algorithm=simple  \
  -ffunction-sections \
  -fdata-sections \
  -falign-functions=32
#==============================================================================#

ifeq ($(COMPILER),gcc)
  MIPSISET     := -mips3
  OPT_FLAGS           := $(GCC_MAIN_OPT_FLAGS)
  COLLISION_OPT_FLAGS  = $(GCC_COLLISION_OPT_FLAGS)
  MATH_UTIL_OPT_FLAGS  = $(GCC_MATH_UTIL_OPT_FLAGS)
  GRAPH_NODE_OPT_FLAGS = $(GCC_GRAPH_NODE_OPT_FLAGS)
else ifeq ($(COMPILER),clang)
  # clang doesn't support ABI 'o32' for 'mips3'
  MIPSISET     := -mips2
  OPT_FLAGS    := $(DEFAULT_OPT_FLAGS)
  COLLISION_OPT_FLAGS  = $(DEFAULT_OPT_FLAGS)
  MATH_UTIL_OPT_FLAGS  = $(DEFAULT_OPT_FLAGS)
  GRAPH_NODE_OPT_FLAGS = $(DEFAULT_OPT_FLAGS)
endif

# UNF - whether to use UNFLoader flashcart library
#   1 - includes code in ROM
#   0 - does not
UNF ?= 0
$(eval $(call validate-option,UNF,0 1))
ifeq ($(UNF),1)
  DEFINES += UNF=1
  SRC_DIRS += src/usb
  USE_DEBUG := 1
endif

# ISVPRINT - whether to fake IS-Viewer presence,
# allowing for usage of CEN64 (and possibly Project64) to print messages to terminal.
#   1 - includes code in ROM
#   0 - does not
ISVPRINT ?= 0
$(eval $(call validate-option,ISVPRINT,0 1))
ifeq ($(ISVPRINT),1)
  DEFINES += ISVPRINT=1
  USE_DEBUG := 1
endif

ifeq ($(USE_DEBUG),1)
  ULTRALIB := ultra_d
  DEFINES += DEBUG=1
else
  ULTRALIB := ultra_rom
  DEFINES += _FINALROM=1 NDEBUG=1
endif

# HVQM - whether to use HVQM fmv library
#   1 - includes code in ROM
#   0 - does not
HVQM ?= 0
$(eval $(call validate-option,HVQM,0 1))
ifeq ($(HVQM),1)
  DEFINES += HVQM=1
  SRC_DIRS += src/hvqm
endif

BUILD_DIR_BASE := build
# BUILD_DIR is the location where all build artifacts are placed
BUILD_DIR      := $(BUILD_DIR_BASE)/$(VERSION)_$(CONSOLE)

COMPRESS ?= rnc1
$(eval $(call validate-option,COMPRESS,mio0 yay0 gzip rnc1 rnc2 uncomp))
ifeq ($(COMPRESS),gzip)
  DEFINES += GZIP=1
  LIBZRULE := $(BUILD_DIR)/libz.a
  LIBZLINK := -lz
else ifeq ($(COMPRESS),rnc1)
  DEFINES += RNC1=1
else ifeq ($(COMPRESS),rnc2)
  DEFINES += RNC2=1
else ifeq ($(COMPRESS),yay0)
  DEFINES += YAY0=1
else ifeq ($(COMPRESS),mio0)
  DEFINES += MIO0=1
else ifeq ($(COMPRESS),uncomp)
  DEFINES += UNCOMPRESSED=1
endif

GZIPVER ?= std
$(eval $(call validate-option,GZIPVER,std libdef))

# Whether to hide commands or not
VERBOSE ?= 0
ifeq ($(VERBOSE),0)
  V := @
endif

# Whether to colorize build messages
COLOR ?= 1

#==============================================================================#
# Universal Dependencies                                                       #
#==============================================================================#

TOOLS_DIR := tools


# (This is a bit hacky, but a lot of rules implicitly depend
# on tools and assets, and we use directory globs further down
# in the makefile that we want should cover assets.)

PYTHON := python3

ifeq ($(filter clean distclean print-%,$(MAKECMDGOALS)),)

  # Make sure assets exist
  NOEXTRACT ?= 0
  ifeq ($(NOEXTRACT),0)
    DUMMY != $(PYTHON) extract_assets.py $(VERSION) >&2 || echo FAIL
    ifeq ($(DUMMY),FAIL)
      $(error Failed to extract assets from US ROM)
    endif
    ifneq (,$(wildcard baserom.jp.z64))
      DUMMY != $(PYTHON) extract_assets.py jp >&2 || echo FAIL
      ifeq ($(DUMMY),FAIL)
        $(error Failed to extract assets from JP ROM)
      endif
    endif
    ifneq (,$(wildcard baserom.eu.z64))
      DUMMY != $(PYTHON) extract_assets.py eu >&2 || echo FAIL
      ifeq ($(DUMMY),FAIL)
        $(error Failed to extract assets from EU ROM)
      endif
    endif
    ifneq (,$(wildcard baserom.sh.z64))
      DUMMY != $(PYTHON) extract_assets.py sh >&2 || echo FAIL
      ifeq ($(DUMMY),FAIL)
        $(error Failed to extract assets from SH ROM)
      endif
    endif
  endif

  # Make tools if out of date
  $(info Building tools...)
  DUMMY != $(MAKE) -s -C $(TOOLS_DIR) >&2 || echo FAIL
    ifeq ($(DUMMY),FAIL)
      $(error Failed to build tools)
    endif
  $(info Building ROM...)

endif


#==============================================================================#
# Target Executable and Sources                                                #
#==============================================================================#

BUILD_DIR_BASE := build
# BUILD_DIR is the location where all build artifacts are placed
BUILD_DIR      := $(BUILD_DIR_BASE)/$(VERSION)_$(CONSOLE)
ROM            := $(BUILD_DIR)/$(TARGET_STRING).z64
ELF            := $(BUILD_DIR)/$(TARGET_STRING).elf
LIBZ           := $(BUILD_DIR)/libz.a
LD_SCRIPT      := sm64.ld
YAY0_DIR       := $(BUILD_DIR)/bin
SOUND_BIN_DIR  := $(BUILD_DIR)/sound
TEXTURE_DIR    := textures
ACTOR_DIR      := actors
LEVEL_DIRS     := $(patsubst levels/%,%,$(dir $(wildcard levels/*/header.h)))

# Directories containing source files
SRC_DIRS += src src/boot src/game src/engine src/audio src/menu src/buffers actors levels bin data assets asm lib sound
LIBZ_SRC_DIRS := src/libz
GODDARD_SRC_DIRS := src/goddard src/goddard/dynlists
BIN_DIRS := bin bin/$(VERSION)

# File dependencies and variables for specific files
include Makefile.split

# Source code files
LEVEL_C_FILES     := $(wildcard levels/*/leveldata.c) $(wildcard levels/*/script.c) $(wildcard levels/*/geo.c)
C_FILES           := $(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.c)) $(LEVEL_C_FILES)
LIBZ_C_FILES     := $(foreach dir,$(LIBZ_SRC_DIRS),$(wildcard $(dir)/*.c))
GODDARD_C_FILES   := $(foreach dir,$(GODDARD_SRC_DIRS),$(wildcard $(dir)/*.c))
S_FILES           := $(foreach dir,$(SRC_DIRS),$(wildcard $(dir)/*.s))
GENERATED_C_FILES := $(BUILD_DIR)/assets/mario_anim_data.c $(BUILD_DIR)/assets/demo_data.c

# Sound files
SOUND_BANK_FILES    := $(wildcard sound/sound_banks/*.json)
SOUND_SAMPLE_DIRS   := $(wildcard sound/samples/*)
SOUND_SAMPLE_AIFFS  := $(foreach dir,$(SOUND_SAMPLE_DIRS),$(wildcard $(dir)/*.aiff))
SOUND_SAMPLE_TABLES := $(foreach file,$(SOUND_SAMPLE_AIFFS),$(BUILD_DIR)/$(file:.aiff=.table))
SOUND_SAMPLE_AIFCS  := $(foreach file,$(SOUND_SAMPLE_AIFFS),$(BUILD_DIR)/$(file:.aiff=.aifc))
SOUND_SEQUENCE_DIRS := sound/sequences sound/sequences/$(VERSION)
# all .m64 files in SOUND_SEQUENCE_DIRS, plus all .m64 files that are generated from .s files in SOUND_SEQUENCE_DIRS
SOUND_SEQUENCE_FILES := \
  $(foreach dir,$(SOUND_SEQUENCE_DIRS),\
    $(wildcard $(dir)/*.m64) \
    $(foreach file,$(wildcard $(dir)/*.s),$(BUILD_DIR)/$(file:.s=.m64)) \
  )

# Object files
O_FILES := $(foreach file,$(C_FILES),$(BUILD_DIR)/$(file:.c=.o)) \
           $(foreach file,$(S_FILES),$(BUILD_DIR)/$(file:.s=.o)) \
           $(foreach file,$(GENERATED_C_FILES),$(file:.c=.o)) \
           lib/PR/hvqm/hvqm2sp1.o lib/PR/hvqm/hvqm2sp2.o

LIBZ_O_FILES := $(foreach file,$(LIBZ_C_FILES),$(BUILD_DIR)/$(file:.c=.o))
GODDARD_O_FILES := $(foreach file,$(GODDARD_C_FILES),$(BUILD_DIR)/$(file:.c=.o))

# Automatic dependency files
DEP_FILES := $(O_FILES:.o=.d) $(LIBZ_O_FILES:.o=.d) $(GODDARD_O_FILES:.o=.d) $(BUILD_DIR)/$(LD_SCRIPT).d

#==============================================================================#
# Compiler Options                                                             #
#==============================================================================#

# detect prefix for MIPS toolchain
ifneq ($(call find-command,mips64-elf-ld),)
  CROSS := mips64-elf-
else ifneq ($(call find-command,mips64-ld),)
  CROSS := mips64-
else ifneq ($(call find-command,mips-linux-gnu-ld),)
  CROSS := mips-linux-gnu-
else ifneq ($(call find-command,mips64-linux-gnu-ld),)
  CROSS := mips64-linux-gnu-
else ifneq ($(call find-command,mips-ld),)
  CROSS := mips-
else
  $(error Unable to detect a suitable MIPS toolchain installed)
endif

export LD_LIBRARY_PATH=./tools

AS        := $(CROSS)as
ifeq ($(COMPILER),gcc)
  CC      := $(CROSS)gcc
  $(BUILD_DIR)/actors/%.o:           OPT_FLAGS := -Ofast -mlong-calls
  $(BUILD_DIR)/levels/%.o:           OPT_FLAGS := -Ofast -mlong-calls
else ifeq ($(COMPILER),clang)
  CC      := clang
endif
# Prefer gcc's cpp if installed on the system
ifneq (,$(call find-command,cpp-10))
  CPP     := cpp-10
else
  CPP     := cpp
endif
ifneq ($(call find-command,mips-n64-ld),)
LD        := mips-n64-ld
else
LD        := tools/mips64-elf-ld
endif
AR        := $(CROSS)ar
OBJDUMP   := $(CROSS)objdump
OBJCOPY   := $(CROSS)objcopy

ifeq ($(TARGET_N64),1)
  TARGET_CFLAGS := -nostdinc -DTARGET_N64 -D_LANGUAGE_C
  CC_CFLAGS := -fno-builtin
endif

INCLUDE_DIRS += include $(BUILD_DIR) $(BUILD_DIR)/include src . include/hvqm
ifeq ($(TARGET_N64),1)
  INCLUDE_DIRS += include/libc
endif

C_DEFINES := $(foreach d,$(DEFINES),-D$(d))
DEF_INC_CFLAGS := $(foreach i,$(INCLUDE_DIRS),-I$(i)) $(C_DEFINES)

# C compiler options
CFLAGS = -G 0 $(OPT_FLAGS) $(TARGET_CFLAGS) $(MIPSISET) $(DEF_INC_CFLAGS)
ifeq ($(COMPILER),gcc)
  CFLAGS += -mno-shared -march=vr4300 -mfix4300 -mabi=32 -mhard-float -mdivide-breaks -fno-stack-protector -fno-common -fno-zero-initialized-in-bss -fno-PIC -mno-abicalls -fno-strict-aliasing -fno-inline-functions -ffreestanding -fwrapv -Wall -Wextra
  CFLAGS += -Wno-missing-braces
else ifeq ($(COMPILER),clang)
  CFLAGS += -mfpxx -target mips -mabi=32 -G 0 -mhard-float -fomit-frame-pointer -fno-stack-protector -fno-common -I include -I src/ -I $(BUILD_DIR)/include -fno-PIC -mno-abicalls -fno-strict-aliasing -fno-inline-functions -ffreestanding -fwrapv -Wall -Wextra
  CFLAGS += -Wno-missing-braces
else
  CFLAGS += -non_shared -Wab,-r4300_mul -Xcpluscomm -Xfullwarn -signed -32
endif
ASMFLAGS = -G 0 $(OPT_FLAGS) $(TARGET_CFLAGS) -mips3 $(DEF_INC_CFLAGS) -mno-shared -march=vr4300 -mfix4300 -mabi=32 -mhard-float -mdivide-breaks -fno-stack-protector -fno-common -fno-zero-initialized-in-bss -fno-PIC -mno-abicalls -fno-strict-aliasing -fno-inline-functions -ffreestanding -fwrapv -Wall -Wextra

ASFLAGS     := -march=vr4300 -mabi=32 $(foreach i,$(INCLUDE_DIRS),-I$(i)) $(foreach d,$(DEFINES),--defsym $(d))
RSPASMFLAGS := $(foreach d,$(DEFINES),-definelabel $(subst =, ,$(d)))

# C preprocessor flags
CPPFLAGS := -P -Wno-trigraphs $(DEF_INC_CFLAGS)

#==============================================================================#
# Miscellaneous Tools                                                          #
#==============================================================================#

# N64 tools
YAY0TOOL              := $(TOOLS_DIR)/slienc
MIO0TOOL              := $(TOOLS_DIR)/mio0
RNCPACK               := $(TOOLS_DIR)/rncpack
ROMALIGN              := $(TOOLS_DIR)/romalign
FILESIZER             := $(TOOLS_DIR)/filesizer
N64CKSUM              := $(TOOLS_DIR)/n64cksum
N64GRAPHICS           := $(TOOLS_DIR)/n64graphics
N64GRAPHICS_CI        := $(TOOLS_DIR)/n64graphics_ci
BINPNG                := $(TOOLS_DIR)/BinPNG.py
TEXTCONV              := $(TOOLS_DIR)/textconv
AIFF_EXTRACT_CODEBOOK := $(TOOLS_DIR)/aiff_extract_codebook
VADPCM_ENC            := $(TOOLS_DIR)/vadpcm_enc
EXTRACT_DATA_FOR_MIO  := $(TOOLS_DIR)/extract_data_for_mio
SKYCONV               := $(TOOLS_DIR)/skyconv
FIXLIGHTS_PY          := $(TOOLS_DIR)/fixlights.py
ifeq ($(GZIPVER),std)
GZIP                  := gzip
else
GZIP                  := libdeflate-gzip
endif
# Use the system installed armips if available. Otherwise use the one provided with this repository.
ifneq (,$(call find-command,armips))
  RSPASM              := armips
else
  RSPASM              := $(TOOLS_DIR)/armips
endif
ENDIAN_BITWIDTH       := $(BUILD_DIR)/endian-and-bitwidth
EMULATOR = mupen64plus
EMU_FLAGS =
LOADER = loader64
LOADER_FLAGS = -vwf
SHA1SUM = sha1sum
PRINT = printf

ifeq ($(COLOR),1)
NO_COL  := \033[0m
RED     := \033[0;31m
GREEN   := \033[0;32m
BLUE    := \033[0;34m
YELLOW  := \033[0;33m
BLINK   := \033[32;5m
endif

# For non-IDO, use objcopy instead of extract_data_for_mio
ifneq ($(COMPILER),ido)
  EXTRACT_DATA_FOR_MIO := $(OBJCOPY) -O binary --only-section=.data
endif

# Common build print status function
define print
  @$(PRINT) "$(GREEN)$(1) $(YELLOW)$(2)$(GREEN) -> $(BLUE)$(3)$(NO_COL)\n"
endef

#==============================================================================#
# Main Targets                                                                 #
#==============================================================================#

all: $(ROM)
	@$(SHA1SUM) $(ROM)
	@$(PRINT) "${BLINK}Build succeeded.\n$(NO_COL)"
	@$(PRINT) "==== Build Options ====$(NO_COL)\n"
	@$(PRINT) "${GREEN}Version:        $(BLUE)$(VERSION)$(NO_COL)\n"
	@$(PRINT) "${GREEN}Microcode:      $(BLUE)$(GRUCODE)$(NO_COL)\n"
	@$(PRINT) "${GREEN}Console:        $(BLUE)$(CONSOLE)$(NO_COL)\n"

clean:
	$(RM) -r $(BUILD_DIR_BASE)
	make -C src/s2d_engine clean

distclean: clean
	$(PYTHON) extract_assets.py --clean
	$(MAKE) -C $(TOOLS_DIR) clean

test: $(ROM)
	$(EMULATOR) $(EMU_FLAGS) $<

test-pj64: $(ROM)
	wine ~/Desktop/new64/Project64.exe $<
# someone2639

load: $(ROM)
	$(LOADER) $(LOADER_FLAGS) $<

libultra: $(BUILD_DIR)/libultra.a

# Extra object file dependencies
$(BUILD_DIR)/asm/boot.o:              $(IPL3_RAW_FILES)
$(BUILD_DIR)/src/game/crash_screen.o: $(CRASH_TEXTURE_C_FILES)
$(BUILD_DIR)/src/game/version.o:      $(BUILD_DIR)/src/game/version_data.h
$(BUILD_DIR)/lib/aspMain.o:           $(BUILD_DIR)/rsp/audio.bin
$(SOUND_BIN_DIR)/sound_data.o:        $(SOUND_BIN_DIR)/sound_data.ctl $(SOUND_BIN_DIR)/sound_data.tbl $(SOUND_BIN_DIR)/sequences.bin $(SOUND_BIN_DIR)/bank_sets
$(BUILD_DIR)/levels/scripts.o:        $(BUILD_DIR)/include/level_headers.h

ifeq ($(VERSION),sh)
  $(BUILD_DIR)/src/audio/load_sh.o: $(SOUND_BIN_DIR)/bank_sets.inc.c $(SOUND_BIN_DIR)/sequences_header.inc.c $(SOUND_BIN_DIR)/ctl_header.inc.c $(SOUND_BIN_DIR)/tbl_header.inc.c
endif

$(CRASH_TEXTURE_C_FILES): TEXTURE_ENCODING := u32

ifeq ($(COMPILER),gcc)
$(BUILD_DIR)/src/libz/%.o: OPT_FLAGS := -Os
$(BUILD_DIR)/src/libz/%.o: CFLAGS += -Wno-implicit-fallthrough -Wno-unused-parameter -Wno-pointer-sign
endif

ifeq ($(VERSION),eu)
  TEXT_DIRS := text/de text/us text/fr

  # EU encoded text inserted into individual segment 0x19 files,
  # and course data also duplicated in leveldata.c
  $(BUILD_DIR)/bin/eu/translation_en.o: $(BUILD_DIR)/text/us/define_text.inc.c
  $(BUILD_DIR)/bin/eu/translation_de.o: $(BUILD_DIR)/text/de/define_text.inc.c
  $(BUILD_DIR)/bin/eu/translation_fr.o: $(BUILD_DIR)/text/fr/define_text.inc.c
  $(BUILD_DIR)/levels/menu/leveldata.o: $(BUILD_DIR)/include/text_strings.h
  $(BUILD_DIR)/levels/menu/leveldata.o: $(BUILD_DIR)/text/us/define_courses.inc.c
  $(BUILD_DIR)/levels/menu/leveldata.o: $(BUILD_DIR)/text/de/define_courses.inc.c
  $(BUILD_DIR)/levels/menu/leveldata.o: $(BUILD_DIR)/text/fr/define_courses.inc.c
else
  ifeq ($(VERSION),sh)
    TEXT_DIRS := text/jp
    $(BUILD_DIR)/bin/segment2.o: $(BUILD_DIR)/text/jp/define_text.inc.c
  else
    TEXT_DIRS := text/$(VERSION)
    # non-EU encoded text inserted into segment 0x02
    $(BUILD_DIR)/bin/segment2.o: $(BUILD_DIR)/text/$(VERSION)/define_text.inc.c
  endif
endif

$(BUILD_DIR)/src/usb/usb.o: OPT_FLAGS := -O0
$(BUILD_DIR)/src/usb/usb.o: CFLAGS += -Wno-unused-variable -Wno-sign-compare -Wno-unused-function
$(BUILD_DIR)/src/usb/debug.o: OPT_FLAGS := -O0
$(BUILD_DIR)/src/usb/debug.o: CFLAGS += -Wno-unused-parameter -Wno-maybe-uninitialized
# File specific opt flags
$(BUILD_DIR)/src/audio/heap.o:          OPT_FLAGS := -Os -fno-jump-tables
$(BUILD_DIR)/src/audio/synthesis.o:     OPT_FLAGS := -Os -fno-jump-tables

$(BUILD_DIR)/src/engine/surface_collision.o:  OPT_FLAGS := $(COLLISION_OPT_FLAGS)
$(BUILD_DIR)/src/engine/math_util.o:          OPT_FLAGS := $(MATH_UTIL_OPT_FLAGS)
$(BUILD_DIR)/src/game/rendering_graph_node.o: OPT_FLAGS := $(GRAPH_NODE_OPT_FLAGS)

# $(info OPT_FLAGS:            $(OPT_FLAGS))
# $(info COLLISION_OPT_FLAGS:  $(COLLISION_OPT_FLAGS))
# $(info MATH_UTIL_OPT_FLAGS:  $(MATH_UTIL_OPT_FLAGS))
# $(info GRAPH_NODE_OPT_FLAGS: $(GRAPH_NODE_OPT_FLAGS))

ALL_DIRS := $(BUILD_DIR) $(addprefix $(BUILD_DIR)/,$(SRC_DIRS) asm/debug $(GODDARD_SRC_DIRS) $(LIBZ_SRC_DIRS) $(ULTRA_BIN_DIRS) $(BIN_DIRS) $(TEXTURE_DIRS) $(TEXT_DIRS) $(SOUND_SAMPLE_DIRS) $(addprefix levels/,$(LEVEL_DIRS)) rsp include) $(YAY0_DIR) $(addprefix $(YAY0_DIR)/,$(VERSION)) $(SOUND_BIN_DIR) $(SOUND_BIN_DIR)/sequences/$(VERSION)

# Make sure build directory exists before compiling anything
DUMMY != mkdir -p $(ALL_DIRS)

$(BUILD_DIR)/include/text_strings.h: $(BUILD_DIR)/include/text_menu_strings.h
$(BUILD_DIR)/src/menu/file_select.o: $(BUILD_DIR)/include/text_strings.h
$(BUILD_DIR)/src/menu/star_select.o: $(BUILD_DIR)/include/text_strings.h
$(BUILD_DIR)/src/game/ingame_menu.o: $(BUILD_DIR)/include/text_strings.h
$(BUILD_DIR)/src/game/puppycam2.o:   $(BUILD_DIR)/include/text_strings.h



#==============================================================================#
# Texture Generation                                                           #
#==============================================================================#
TEXTURE_ENCODING := u8

# Convert PNGs to RGBA32, RGBA16, IA16, IA8, IA4, IA1, I8, I4 binary files
$(BUILD_DIR)/%: %.png
	$(call print,Converting:,$<,$@)
	$(V)$(N64GRAPHICS) -s raw -i $@ -g $< -f $(lastword $(subst ., ,$@))

$(BUILD_DIR)/%.inc.c: %.png
	$(call print,Converting:,$<,$@)
	$(V)$(N64GRAPHICS) -s $(TEXTURE_ENCODING) -i $@ -g $< -f $(lastword ,$(subst ., ,$(basename $<)))

# Color Index CI8
$(BUILD_DIR)/%.ci8.inc.c: %.ci8.png
	$(call print,Converting CI:,$<,$@)
	$(V)$(BINPNG) $< $@ 8

# Color Index CI4
$(BUILD_DIR)/%.ci4.inc.c: %.ci4.png
	$(call print,Converting CI:,$<,$@)
	$(V)$(BINPNG) $< $@ 4


#==============================================================================#
# Compressed Segment Generation                                                #
#==============================================================================#

# Link segment file to resolve external labels
# TODO: ideally this would be `-Trodata-segment=0x07000000` but that doesn't set the address
$(BUILD_DIR)/%.elf: $(BUILD_DIR)/%.o
	$(call print,Linking ELF file:,$<,$@)
	$(V)$(LD) -e 0 -Ttext=$(SEGMENT_ADDRESS) -Map $@.map -o $@ $<
# Override for leveldata.elf, which otherwise matches the above pattern
.SECONDEXPANSION:
$(BUILD_DIR)/levels/%/leveldata.elf: $(BUILD_DIR)/levels/%/leveldata.o $(BUILD_DIR)/bin/$$(TEXTURE_BIN).elf
	$(call print,Linking ELF file:,$<,$@)
	$(V)$(LD) -e 0 -Ttext=$(SEGMENT_ADDRESS) -Map $@.map --just-symbols=$(BUILD_DIR)/bin/$(TEXTURE_BIN).elf -o $@ $<

$(BUILD_DIR)/%.bin: $(BUILD_DIR)/%.elf
	$(call print,Extracting compressible data from:,$<,$@)
	$(V)$(EXTRACT_DATA_FOR_MIO) $< $@

$(BUILD_DIR)/levels/%/leveldata.bin: $(BUILD_DIR)/levels/%/leveldata.elf
	$(call print,Extracting compressible data from:,$<,$@)
	$(V)$(EXTRACT_DATA_FOR_MIO) $< $@

ifeq ($(COMPRESS),gzip)
include compression/gziprules.mk
else ifeq ($(COMPRESS),rnc1)
include compression/rnc1rules.mk
else ifeq ($(COMPRESS),rnc2)
include compression/rnc2rules.mk
else ifeq ($(COMPRESS),yay0)
include compression/yay0rules.mk
else ifeq ($(COMPRESS),mio0)
include compression/mio0rules.mk
else ifeq ($(COMPRESS),uncomp)
include compression/uncomprules.mk
endif

#==============================================================================#
# Sound File Generation                                                        #
#==============================================================================#

$(BUILD_DIR)/%.table: %.aiff
	$(call print,Extracting codebook:,$<,$@)
	$(V)$(AIFF_EXTRACT_CODEBOOK) $< >$@

$(BUILD_DIR)/%.aifc: $(BUILD_DIR)/%.table %.aiff
	$(call print,Encoding ADPCM:,$(word 2,$^),$@)
	$(V)$(VADPCM_ENC) -c $^ $@

$(ENDIAN_BITWIDTH): $(TOOLS_DIR)/determine-endian-bitwidth.c
	@$(PRINT) "$(GREEN)Generating endian-bitwidth $(NO_COL)\n"
	$(V)$(CC) -c $(CFLAGS) -o $@.dummy2 $< 2>$@.dummy1; true
	$(V)grep -o 'msgbegin --endian .* --bitwidth .* msgend' $@.dummy1 > $@.dummy2
	$(V)head -n1 <$@.dummy2 | cut -d' ' -f2-5 > $@
	$(V)$(RM) $@.dummy1
	$(V)$(RM) $@.dummy2

$(SOUND_BIN_DIR)/sound_data.ctl: sound/sound_banks/ $(SOUND_BANK_FILES) $(SOUND_SAMPLE_AIFCS) $(ENDIAN_BITWIDTH)
	@$(PRINT) "$(GREEN)Generating:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(PYTHON) $(TOOLS_DIR)/assemble_sound.py $(BUILD_DIR)/sound/samples/ sound/sound_banks/ $(SOUND_BIN_DIR)/sound_data.ctl $(SOUND_BIN_DIR)/ctl_header $(SOUND_BIN_DIR)/sound_data.tbl $(SOUND_BIN_DIR)/tbl_header $(C_DEFINES) $$(cat $(ENDIAN_BITWIDTH))

$(SOUND_BIN_DIR)/sound_data.tbl: $(SOUND_BIN_DIR)/sound_data.ctl
	@true

$(SOUND_BIN_DIR)/ctl_header: $(SOUND_BIN_DIR)/sound_data.ctl
	@true

$(SOUND_BIN_DIR)/tbl_header: $(SOUND_BIN_DIR)/sound_data.ctl
	@true

$(SOUND_BIN_DIR)/sequences.bin: $(SOUND_BANK_FILES) sound/sequences.json $(SOUND_SEQUENCE_DIRS) $(SOUND_SEQUENCE_FILES) $(ENDIAN_BITWIDTH)
	@$(PRINT) "$(GREEN)Generating:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(PYTHON) $(TOOLS_DIR)/assemble_sound.py --sequences $@ $(SOUND_BIN_DIR)/sequences_header $(SOUND_BIN_DIR)/bank_sets sound/sound_banks/ sound/sequences.json $(SOUND_SEQUENCE_FILES) $(C_DEFINES) $$(cat $(ENDIAN_BITWIDTH))

$(SOUND_BIN_DIR)/bank_sets: $(SOUND_BIN_DIR)/sequences.bin
	@true

$(SOUND_BIN_DIR)/sequences_header: $(SOUND_BIN_DIR)/sequences.bin
	@true

$(SOUND_BIN_DIR)/%.m64: $(SOUND_BIN_DIR)/%.o
	$(call print,Converting to M64:,$<,$@)
	$(V)$(OBJCOPY) -j .rodata $< -O binary $@


#==============================================================================#
# Generated Source Code Files                                                  #
#==============================================================================#

# Convert binary file to a comma-separated list of byte values for inclusion in C code
$(BUILD_DIR)/%.inc.c: $(BUILD_DIR)/%
	$(call print,Converting to C:,$<,$@)
	$(V)hexdump -v -e '1/1 "0x%X,"' $< > $@
	$(V)echo >> $@

# Generate animation data
$(BUILD_DIR)/assets/mario_anim_data.c: $(wildcard assets/anims/*.inc.c)
	@$(PRINT) "$(GREEN)Generating animation data $(NO_COL)\n"
	$(V)$(PYTHON) $(TOOLS_DIR)/mario_anims_converter.py > $@

# Generate demo input data
$(BUILD_DIR)/assets/demo_data.c: assets/demo_data.json $(wildcard assets/demos/*.bin)
	@$(PRINT) "$(GREEN)Generating demo data $(NO_COL)\n"
	$(V)$(PYTHON) $(TOOLS_DIR)/demo_data_converter.py assets/demo_data.json $(DEF_INC_CFLAGS) > $@

# Encode in-game text strings
$(BUILD_DIR)/include/text_strings.h: include/text_strings.h.in
	$(call print,Encoding:,$<,$@)
	$(V)$(TEXTCONV) charmap.txt $< $@
$(BUILD_DIR)/include/text_menu_strings.h: include/text_menu_strings.h.in
	$(call print,Encoding:,$<,$@)
	$(V)$(TEXTCONV) charmap_menu.txt $< $@
$(BUILD_DIR)/text/%/define_courses.inc.c: text/define_courses.inc.c text/%/courses.h
	@$(PRINT) "$(GREEN)Preprocessing: $(BLUE)$@ $(NO_COL)\n"
	$(V)$(CPP) $(CPPFLAGS) $< -o - -I text/$*/ | $(TEXTCONV) charmap.txt - $@
$(BUILD_DIR)/text/%/define_text.inc.c: text/define_text.inc.c text/%/courses.h text/%/dialogs.h
	@$(PRINT) "$(GREEN)Preprocessing: $(BLUE)$@ $(NO_COL)\n"
	$(V)$(CPP) $(CPPFLAGS) $< -o - -I text/$*/ | $(TEXTCONV) charmap.txt - $@

# Level headers
$(BUILD_DIR)/include/level_headers.h: levels/level_headers.h.in
	$(call print,Preprocessing level headers:,$<,$@)
	$(V)$(CPP) $(CPPFLAGS) -I . $< | sed -E 's|(.+)|#include "\1"|' > $@

# Generate version_data.h
$(BUILD_DIR)/src/game/version_data.h: tools/make_version.sh
	@$(PRINT) "$(GREEN)Generating:  $(BLUE)$@ $(NO_COL)\n"
	$(V)tools/make_version.sh $(CROSS) > $@

#==============================================================================#
# Compilation Recipes                                                          #
#==============================================================================#

# Compile C code
ifeq ($(FIXLIGHTS),1)
# This must not be run multiple times at once, so we run it ahead of time rather than in a rule
DUMMY != $(FIXLIGHTS_PY) actors
DUMMY != $(FIXLIGHTS_PY) levels
endif
$(BUILD_DIR)/%.o: %.c
	$(call print,Compiling:,$<,$@)
	$(V)$(CC) -c $(CFLAGS) -MMD -MF $(BUILD_DIR)/$*.d  -o $@ $<
$(BUILD_DIR)/%.o: $(BUILD_DIR)/%.c
	$(call print,Compiling:,$<,$@)
	$(V)$(CC) -c $(CFLAGS) -MMD -MF $(BUILD_DIR)/$*.d  -o $@ $<

# Assemble assembly code
$(BUILD_DIR)/%.o: %.s
	$(call print,Assembling:,$<,$@)
	$(V)$(CROSS)gcc -c $(ASMFLAGS) $(foreach i,$(INCLUDE_DIRS),-Wa,-I$(i)) -x assembler-with-cpp -MMD -MF $(BUILD_DIR)/$*.d  -o $@ $<

# Assemble RSP assembly code
$(BUILD_DIR)/rsp/%.bin $(BUILD_DIR)/rsp/%_data.bin: rsp/%.s
	$(call print,Assembling:,$<,$@)
	$(V)$(RSPASM) -sym $@.sym $(RSPASMFLAGS) -strequ CODE_FILE $(BUILD_DIR)/rsp/$*.bin -strequ DATA_FILE $(BUILD_DIR)/rsp/$*_data.bin $<

# Run linker script through the C preprocessor
$(BUILD_DIR)/$(LD_SCRIPT): $(LD_SCRIPT) $(BUILD_DIR)/goddard.txt
	$(call print,Preprocessing linker script:,$<,$@)
	$(V)$(CPP) $(CPPFLAGS) -DBUILD_DIR=$(BUILD_DIR) $(DEBUG_MAP_STACKTRACE_FLAG) -MMD -MP -MT $@ -MF $@.d -o $@ $<

# Link libgoddard
$(BUILD_DIR)/libgoddard.a: $(GODDARD_O_FILES)
	@$(PRINT) "$(GREEN)Linking libgoddard:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(AR) rcs -o $@ $(GODDARD_O_FILES)

# Link libz
$(BUILD_DIR)/libz.a: $(LIBZ_O_FILES)
	@$(PRINT) "$(GREEN)Linking libz:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(AR) rcs -o $@ $(LIBZ_O_FILES)

# SS2: Goddard rules to get size
$(BUILD_DIR)/sm64_prelim.ld: sm64.ld $(O_FILES) $(YAY0_OBJ_FILES) $(SEG_FILES) $(BUILD_DIR)/libgoddard.a $(BUILD_DIR)/libz.a
	$(call print,Preprocessing preliminary linker script:,$<,$@)
	$(V)$(CPP) $(CPPFLAGS) -DPRELIMINARY=1 -DBUILD_DIR=$(BUILD_DIR) -MMD -MP -MT $@ -MF $@.d -o $@ $<

$(BUILD_DIR)/sm64_prelim.elf: $(BUILD_DIR)/sm64_prelim.ld
	@$(PRINT) "$(GREEN)Linking Preliminary ELF file:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(LD) --gc-sections -L $(BUILD_DIR) -T undefined_syms.txt -T $< -Map $(BUILD_DIR)/sm64_prelim.map --no-check-sections $(addprefix -R ,$(SEG_FILES)) -o $@ $(O_FILES) -L$(LIBS_DIR) -l$(ULTRALIB) -Llib $(LINK_LIBRARIES) -u sprintf -u osMapTLB -Llib/gcclib/$(LIBGCCDIR) -lgcc

$(BUILD_DIR)/goddard.txt: $(BUILD_DIR)/sm64_prelim.elf
	$(call print,Getting Goddard size...)
	$(V)python3 tools/getGoddardSize.py $(BUILD_DIR)/sm64_prelim.map $(VERSION)

$(BUILD_DIR)/asm/debug/map.o: asm/debug/map.s $(BUILD_DIR)/sm64_prelim.elf
	$(call print,Assembling:,$<,$@)
	$(V)python3 tools/mapPacker.py $(BUILD_DIR)/sm64_prelim.map $(BUILD_DIR)/bin/addr.bin $(BUILD_DIR)/bin/name.bin
	$(V)$(CROSS)gcc -c $(ASMFLAGS) $(foreach i,$(INCLUDE_DIRS),-Wa,-I$(i)) -x assembler-with-cpp -MMD -MF $(BUILD_DIR)/$*.d  -o $@ $<

# Link SM64 ELF file
$(ELF): $(BUILD_DIR)/sm64_prelim.elf $(BUILD_DIR)/asm/debug/map.o $(O_FILES) $(YAY0_OBJ_FILES) $(SEG_FILES) $(BUILD_DIR)/$(LD_SCRIPT) undefined_syms.txt $(BUILD_DIR)/libz.a $(BUILD_DIR)/libgoddard.a
	@$(PRINT) "$(GREEN)Linking ELF file:  $(BLUE)$@ $(NO_COL)\n"
	$(V)$(LD) --gc-sections -L $(BUILD_DIR) -T undefined_syms.txt -T $(BUILD_DIR)/$(LD_SCRIPT) -T goddard.txt -Map $(BUILD_DIR)/sm64.$(VERSION).map --no-check-sections $(addprefix -R ,$(SEG_FILES)) -o $@ $(O_FILES) -L$(LIBS_DIR) -l$(ULTRALIB) -Llib $(LINK_LIBRARIES) -u sprintf -u osMapTLB -Llib/gcclib/$(LIBGCCDIR) -lgcc -lrtc

# Build ROM
$(ROM): $(ELF)
	$(call print,Building ROM:,$<,$@)
ifeq      ($(CONSOLE),n64)
	$(V)$(OBJCOPY) --pad-to=0x101000 --gap-fill=0xFF $< $@ -O binary
else ifeq ($(CONSOLE),bb)
	$(V)$(OBJCOPY) --gap-fill=0x00 $< $@ -O binary
	$(V)dd if=$@ of=tmp bs=16K conv=sync status=none
	$(V)mv tmp $@
endif
	$(V)$(N64CKSUM) $@

$(BUILD_DIR)/$(TARGET).objdump: $(ELF)
	$(OBJDUMP) -D $< > $@

.PHONY: all clean distclean default test load
# with no prerequisites, .SECONDARY causes no intermediate target to be removed
.SECONDARY:

# Remove built-in rules, to improve performance
MAKEFLAGS += --no-builtin-rules

-include $(DEP_FILES)

print-% : ; $(info $* is a $(flavor $*) variable set to [$($*)]) @true
