#!/bin/bash

mkdir -p src/hex/riscv-tests

for file in src/c/riscv-tests/isa/rv32mi-p-* src/c/riscv-tests/isa/rv32ua-p-* src/c/riscv-tests/isa/rv32ui-p-* src/c/riscv-tests/isa/rv32um-p-*; do
  echo "Processing $file"
  filename=$(basename "$file")
  if [[ "$file" != *.dump ]]; then
    src/script/elftohex.sh "$file" "src/hex/riscv-tests/$filename.hex"
  elif [[ "$file" == *.dump ]]; then
    cp "$file" "src/hex/riscv-tests/$filename"
  fi
done
