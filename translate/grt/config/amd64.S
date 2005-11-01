/*  GRT stack implementation for amd64 (x86_64)
    Copyright (C) 2005 Tristan Gingold.

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
	.file		"amd64.S"
	.version	"01.01"
	
	.text

	/* Function called to loop on the process.  */	
	.align 4
	.type	grt_stack_loop,@function
grt_stack_loop:
	mov	0(%rsp),%rdi
	call	*8(%rsp)
	jmp	grt_stack_loop
	.size	grt_stack_loop, . - grt_stack_loop

	/* function Stack_Create (Func : Address; Arg : Address)
 	                          return Stack_Type;
           Args: FUNC (RDI), ARG (RSI)
	*/
	.align	4
	.globl grt_stack_create
	.type	 grt_stack_create,@function
grt_stack_create:
	/* Standard prologue.  */
	pushq	%rbp
	movq	%rsp,%rbp
	/* Save args.  */
	sub	$0x10,%rsp
	mov	%rdi,-8(%rbp)
	mov	%rsi,-16(%rbp)
	
	/* Allocate the stack, and exit in case of failure  */
	callq	grt_stack_allocate
	test	%rax,%rax
	je	.Ldone

	/* Note: %RAX contains the address of the stack_context.  This is
	   also the top of the stack.  */
				
	/* Prepare stack.  */
	/* The function to be executed.  */
	mov	-8(%rbp), %rdi
	mov	%rdi, -8(%rax)
	/* The argument.  */
	mov	-16(%rbp), %rsi
	mov	%rsi, -16(%rax)
	/* The return function.  Must be 8 mod 16.  */
	movq	$grt_stack_loop, -24(%rax)
	/* The context.  */
	mov	%rbp, -32(%rax)
	mov	%rbx, -40(%rax)
	mov	%r12, -48(%rax)
	mov	%r13, -56(%rax)
	mov	%r14, -64(%rax)
	mov	%r15, -72(%rax)

	/* Save the new stack pointer to the stack context.  */
	lea	-72(%rax), %rsi
	mov	%rsi, (%rax)

.Ldone:
	leave
	ret
	.size	 grt_stack_create,. - grt_stack_create



	.align 4
	.globl grt_stack_switch
	/* Arguments:	TO (RDI), FROM (RSI) [VAL (RDX)]
	   Both are pointers to a stack_context.  */
	.type	 grt_stack_switch,@function
grt_stack_switch:
	/* Save call-used registers.  */
	pushq	%rbp
	pushq	%rbx
	pushq	%r12
	pushq	%r13
	pushq	%r14
	pushq	%r15
	/* Save the current stack.  */
	movq	%rsp, (%rsi)
	/* Stack switch.  */
	movq	(%rdi), %rsp
	/* Restore call-used registers.  */
	popq	%r15
	popq	%r14
	popq	%r13
	popq	%r12
	popq	%rbx
	popq	%rbp
	/* Return val.  */
	movq	%rdx, %rax
	/* Run.  */
	ret
	.size	 grt_stack_switch, . - grt_stack_switch

	
	.ident	"Written by T.Gingold"
