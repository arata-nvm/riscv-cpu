OUTPUT_FORMAT("elf32-littleriscv");
OUTPUT_ARCH("riscv");

ENTRY(_start);

SECTIONS
{
	. = 0x80000000;
	.text : { *(.text) }
	.rodata : { *(.rodata) }
	.data : { *(.data) }
	. = ALIGN(4);
	__bss_start = .;
	.bss : { *(.bss) }
	__bss_end = .;
}
