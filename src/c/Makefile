GCC_PREFIX = riscv64-unknown-elf-

CC=$(GCC_PREFIX)gcc
LD=$(GCC_PREFIX)ld
OBJCOPY=$(GCC_PREFIX)objcopy
OBJDUMP=$(GCC_PREFIX)objdump
OD=od
RM=rm

ARCH=rv32i

CCFLAGS=-O2 \
				-fno-stack-protector \
				-fno-zero-initialized-in-bss \
				-ffreestanding \
				-fno-builtin \
				-nostdlib \
				-nodefaultlibs \
				-nostartfiles \
				-mstrict-align \
				-march=$(ARCH) \
				-mabi=ilp32

LDFLAGS=-T link.ld

ODFLAGS=-An -tx1 -w1 -v

OBJDUMPFLAGS=-b elf32-littleriscv -D

%: %.c
	$(CC) $(CCFLAGS) -c -o $@.o $<
	$(LD) $(LDFLAGS) -o $@ $@.o
	$(OBJCOPY) -O binary $@ $@.bin
	$(OD) $(ODFLAGS) $@.bin > ../hex/$@.hex
	$(OBJDUMP) $(OBJDUMPFLAGS) $@ > ./$@.dump
	$(RM) -f $@.o
	$(RM) -f $@
	$(RM) -f $@.bin
