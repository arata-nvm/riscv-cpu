#!/bin/bash

f=$1
SAVE_DIR=/src/src/hex

FILE_NAME="${f##*/}"
# riscv64-unknown-elf-objcopy -O binary $f $SAVE_DIR/$FILE_NAME.bin
od -An -tx1 -w1 -v $FILE_NAME > $SAVE_DIR/$FILE_NAME.hex
rm -f $SAVE_DIR/$FILE_NAME.bin
