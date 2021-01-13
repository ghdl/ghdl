/*  GRT stack implementation for x86.
    Copyright (C) 2002 - 2014 Tristan Gingold.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <gnu.org/licenses>.

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
*/
	.file		"chkstk.S"

	.text

#ifdef __APPLE__
#define __chkstk ___chkstk
#endif

	/* Function called to loop on the process.  */
	.align 4
#ifdef __ELF__
	.type	__chkstk,@function
#endif
	.globl __chkstk
__chkstk:
	testl	%eax,%eax
	je	0f
	subl	$4,%eax		/* 4 bytes already used by call.  */
	subl	%eax,%esp
	jmp	*(%esp,%eax)
0:
	ret
#ifdef __ELF__
	.size	__chkstk, . - __chkstk
#endif

	.ident	"Written by T.Gingold"

#if defined(__linux__) && defined(__ELF__)
	.section .note.GNU-stack,"",%progbits
#endif
