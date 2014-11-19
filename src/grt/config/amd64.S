/*  GRT stack implementation for amd64 (x86_64) -*- asm -*-
    Copyright (C) 2005 - 2014 Tristan Gingold.

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

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
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

	/* Function called to loop on the process.
	   At entry  |  Frame          |  In body
	   ----------+-----------------+---------
	   %rsp      |  0 (ret addr)   |  %rbp+8
	             |  saved %rbp     |  %rbp
	*/
ENTRY(grt_stack_loop)
	/* Standard prologue.  */
	pushq	%rbp
	movq	%rsp,%rbp
	/* Body.  */
0:	mov	%r13,%rdi
	call	%r12
	jmp	0b
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

	/* Prepare fake frame for stack_loop.  */
	xorq	%rsi, %rsi
	mov	%rsi,-8(%rax)	/* Null return address (must be 8 mod 16).  */
	/* The return function.  */
#if __APPLE__
	movq	_grt_stack_loop@GOTPCREL(%rip), %rdi
	movq	%rdi, -16(%rax)
#else
	movq	$grt_stack_loop, -16(%rax)
#endif
	/* The context.  */
	mov	%rsi, -24(%rax)	/* %rbp <- 0 (null frame) */
	mov	%rbx, -32(%rax)
	mov	-8(%rbp), %rdi
	mov	%rdi, -40(%rax) /* %r12 <- function address.  */
	mov	-16(%rbp), %rdi
	mov	%rdi, -48(%rax) /* %r13 <- function argument.  */
	mov	%r14, -56(%rax)
	mov	%r15, -64(%rax)

	/* Save the new stack pointer to the stack context.  */
	lea	-64(%rax), %rsi
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
