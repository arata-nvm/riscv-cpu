.text
.align 2
.global _start
.global _end
.extern main

_start:
  li sp, 0xf000
	call main
	j _end
_end:
  j _end
