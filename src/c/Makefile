CC=riscv64-unknown-elf-gcc
CCFLAGS=-O2 -march=rv32i -mabi=ilp32

LD=riscv64-unknown-elf-ld
LDFLAGS=-b elf32-littleriscv -T link.ld

OBJCOPY=riscv64-unknown-elf-objcopy

OD=od
ODFLAGS=-An -tx1 -w1 -v

OBJDUMP=riscv64-unknown-elf-objdump
OBJDUMPFLAGS=-b elf32-littleriscv -D

RM=rm

%: %.c
	$(CC) $(CCFLAGS) -c -o $@.o $<
	$(LD) $(LDFLAGS) -o $@ $@.o
	$(OBJCOPY) -O binary $@ $@.bin
	$(OD) $(ODFLAGS) $@.bin > ../hex/$@.hex
	$(OBJDUMP) $(OBJDUMPFLAGS) $@ > ./$@.dump
	rm -f $@.o
	rm -f $@
	rm -f $@.bin
