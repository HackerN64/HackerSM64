#!/bin/bash
# Given a list of header files, compute the bss index that results from
# including them. (See prevent_bss_reordering.h for more information.)

TEMPC=$(mktemp -t bss.XXXXXXX.c)
TEMPO=$(mktemp -t bss.XXXXXXX.o)
trap "rm -f $TEMPC $TEMPO" EXIT

set -e

if [[ $# = 0 ]]; then
    echo "Usage: ./tools/calc_bss.sh file1.h file2.h ..." >&2
    exit 1
fi

if [ -z "$QEMU_IRIX" ]; then
    echo "env variable QEMU_IRIX should point to the qemu-mips binary" >&2
    exit 1
fi

# detect prefix for MIPS toolchain unless CROSS is already defined
if [ -z "$CROSS" ]; then
  if command -v mips64-elf-ld &> /dev/null ; then
    CROSS=mips64-elf-
  elif command -v mips-n64-ld &> /dev/null ; then
    CROSS=mips-n64-
  elif command -v mips64-ld &> /dev/null ; then
    CROSS=mips64-
  elif command -v mips-linux-gnu-ld &> /dev/null ; then
    CROSS=mips-linux-gnu-
  elif command -v mips64-linux-gnu-ld &> /dev/null ; then
    CROSS=mips64-linux-gnu-
  elif command -v mips-ld &> /dev/null ; then
    CROSS=mips-
  else
    echo "Unable to detect a suitable MIPS toolchain installed"
    exit 1
  fi
fi

# bss indexing starts at 3
for I in {3..255}; do
    echo "char bss$I;" >> $TEMPC
done
for I in {0..2}; do
    echo "char bss$I;" >> $TEMPC
done

while [[ $# -gt 0 ]]; do
    echo "#include \"$1\"" >> $TEMPC
    shift
done

echo "char measurement;" >> $TEMPC

$QEMU_IRIX -silent -L $IRIX_ROOT $IRIX_ROOT/usr/bin/cc -c -non_shared -G 0 \
    -g -Xcpluscomm -mips2 -I $(pwd)/include/ $TEMPC -o $TEMPO

LINE=$(${CROSS}objdump -t $TEMPO | grep measurement | cut -d' ' -f1)
NUM=$((0x$LINE - 1))
echo "bss index: $NUM"
