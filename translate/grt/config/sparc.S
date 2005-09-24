/*  GRT stack implementation for x86.
    Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold.

    GHDL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING.  If not, write to the Free
    Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
*/
	.file		"sparc.S"
	
	.section	".text"

	/* Stack structure is:
           +4 :	cur_length
	   +0 :	cur_sp
	   -4 :	return address
	   -8 :	process function to be executed
           -12:	function argument
	   ...
           -72:	%sp
	*/
	
	/* Function called to loop on the process.  */	
	.align 4
	.type	grt_stack_loop,#function
grt_stack_loop:
	ld	[%sp + 64], %o1
	jmpl	%o1 + 0, %o7
	ld	[%sp + 68], %o0
	ba	grt_stack_loop
	nop
	.size	grt_stack_loop, . - grt_stack_loop

	/* function Stack_Create (Func : Address; Arg : Address)
	                         return Stack_Type;  */
	.align	4
	.global grt_stack_create
	.type	 grt_stack_create,#function
grt_stack_create:
	/* Standard prologue.  */
	save	%sp,-80,%sp
	
	/* Allocate the stack, and exit in case of failure  */
	call	grt_stack_allocate
	nop
	cmp	%o0, 0
	be	.Ldone
	nop

	/* Note: %o0 contains the address of the stack_context.  This is
	   also the top of the stack.  */
				
	/* Prepare stack.  */

	/* The return function.  */
	sethi	%hi(grt_stack_loop - 8), %l2
	or	%lo(grt_stack_loop - 8), %l2, %l2

	/* Create a frame for grt_stack_loop. */
	sub	%o0, (64 + 8), %l1

	/* The function to be executed.  */
	st	%i0, [%l1 + 64]
	/* The argument.  */
	st	%i1, [%l1 + 68]

	/* Create a frame for grt_stack_switch.  */
	sub	%l1, 64, %l0

	/* Save frame pointer.  */
	st	%l1, [%l0 + 56]
	/* Save return address.  */
	st	%l2, [%l0 + 60]

	/* Save stack pointer.  */
	st	%l0, [%o0]

.Ldone:
	ret
	restore %o0, %g0, %o0
	.size	 grt_stack_create,. - grt_stack_create


	.align 4
	.global grt_stack_switch
	/* Arguments:	TO, FROM.
	   Both are pointers to a stack_context.  */
	.type	 grt_stack_switch,#function
grt_stack_switch:
	/* Standard prologue.  */
	save	%sp,-80,%sp

	/* Flush and invalidate windows.
	   It is not clear wether the current window is saved or not,
	    therefore, I assume it is not.
	*/
	ta	3

	/* Only IN registers %fp and %i7 (return address) must be saved.
	   Of course, I could use std/ldd, but it is not as clear
	*/
	/* Save current frame pointer.  */
	st	%fp, [%sp + 56]
	/* Save return address.  */
	st	%i7, [%sp + 60]

	/* Save stack pointer.  */
	st	%sp, [%i1]

	/* Load stack pointer.  */
	ld	[%i0], %sp
	
	/* Load return address.  */
	ld	[%sp + 60], %i7
	/* Load frame pointer.  */
	ld	[%sp + 56], %fp

	/* Return.  */
	ret
	restore	
	.size	 grt_stack_switch, . - grt_stack_switch

	
	.ident	"Written by T.Gingold"
