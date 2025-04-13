#!/bin/bash
# Usage: elftohex.sh <input> <output>

input=$1
output=$2

riscv32-unknown-elf-objcopy -O binary $input $output.bin
od -An -tx1 -w1 -v $output.bin > $output
rm -f $output.bin
