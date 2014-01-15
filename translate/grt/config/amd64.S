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

#ifdef __ELF__
#define ENTRY(func) .align 4; .globl func; .type func,@function; func:
#define END(func) .size func, . - func
#define NAME(name) name
#elif __APPLE__
#define ENTRY(func) .align 4; .globl _##func; _##func:
#define END(func)
#define NAME(name) _##name
#else
#define ENTRY(func) .align 4; func:
#define END(func)
#define NAME(name) name
#endif
	.text

	/* Function called to loop on the process.  */
ENTRY(grt_stack_loop)
	mov	0(%rsp),%rdi
	call	*8(%rsp)
	jmp	NAME(grt_stack_loop)
END(grt_stack_loop)

	/* function Stack_Create (Func : Address; Arg : Address)
 	                          return Stack_Type;
           Args: FUNC (RDI), ARG (RSI)
	*/
ENTRY(grt_stack_create)
	/* Standard prologue.  */
	pushq	%rbp
	movq	%rsp,%rbp
	/* Save args.  */
	sub	$0x10,%rsp
	mov	%rdi,-8(%rbp)
	mov	%rsi,-16(%rbp)

	/* Allocate the stack, and exit in case of failure  */
	callq	NAME(grt_stack_allocate)
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
#if __APPLE__
	movq	_grt_stack_loop@GOTPCREL(%rip), %rsi
	movq	%rsi, -24(%rax)
#else
	movq	$grt_stack_loop, -24(%rax)
#endif
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
END(grt_stack_create)



	/* Arguments:	TO (RDI), FROM (RSI) [VAL (RDX)]
	   Both are pointers to a stack_context.  */
ENTRY(grt_stack_switch)
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
END(grt_stack_switch)

	.ident	"Written by T.Gingold"
