# Makefile for s2d_engine.a

TARGET = libs2d_engine.a

default: all

BUILD_DIR ?= build
DUMMY != mkdir -p $(BUILD_DIR)

C_FILES = $(wildcard ./*.c)
O_FILES = $(foreach file,$(C_FILES),$(BUILD_DIR)/$(file:.c=.o))

CROSS := mips-linux-gnu-
CC = $(CROSS)gcc
AR = $(CROSS)ar

INCLUDE_BASE := ../../
I_DIRS = src include/n64/PR include/n64 include/libc include .
FULL_I_DIRS = $(addprefix $(INCLUDE_BASE),$(I_DIRS))
I_FLAGS = $(foreach i,$(FULL_I_DIRS),-I$(i))

TARGET_CFLAGS = -nostdinc -I include/libc -DTARGET_N64 -DF3DEX_GBI_2 -DNON_MATCHING -DAVOID_UB
CFLAGS := -Wall $(TARGET_CFLAGS) $(I_FLAGS) -fno-stack-protector -march=vr4300 -mtune=vr4300 -mfix4300 -mabi=32 -mno-shared -G 0 -fno-PIC -mno-abicalls -fno-zero-initialized-in-bss -fno-toplevel-reorder -Wno-missing-braces


all: $(BUILD_DIR)/$(TARGET)

clean:
	rm -r build/

COPY_DIR := none
ifneq ($(MAKECMDGOALS),clean)
	ifeq ($(COPY_DIR),none)
		$(error S2DEX Text Engine: No COPY_DIR defined!)
	endif
endif

$(BUILD_DIR)/$(TARGET): $(O_FILES)
	$(AR) rcs -o $@ $(O_FILES)
	cp $@ $(COPY_DIR)

$(BUILD_DIR)/%.o: %.c
	$(CC) -c $(CFLAGS) -o $@ $<

print-% : ; $(info $* is a $(flavor $*) variable set to [$($*)]) @true
