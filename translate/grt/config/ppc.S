/*  GRT stack implementation for ppc.
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
	.file		"ppc.S"
	
	.section	".text"

#define OFF 240

#define GREG(x) x
#define FREG(x) x
	
#define r0 GREG(0)
#define r1 GREG(1)
#define r2 GREG(2)
#define r3 GREG(3)
#define r4 GREG(4)
#define r5 GREG(5)
#define r6 GREG(6)
#define r7 GREG(7)
#define r8 GREG(8)
#define r9 GREG(9)
#define r10 GREG(10)
#define r11 GREG(11)
#define r12 GREG(12)
#define r13 GREG(13)
#define r14 GREG(14)
#define r15 GREG(15)
#define r16 GREG(16)
#define r17 GREG(17)
#define r18 GREG(18)
#define r19 GREG(19)
#define r20 GREG(20)
#define r21 GREG(21)
#define r22 GREG(22)
#define r23 GREG(23)
#define r24 GREG(24)
#define r25 GREG(25)
#define r26 GREG(26)
#define r27 GREG(27)
#define r28 GREG(28)
#define r29 GREG(29)
#define r30 GREG(30)
#define r31 GREG(31)

#define f0 FREG(0)
#define f1 FREG(1)
#define f2 FREG(2)
#define f3 FREG(3)
#define f4 FREG(4)
#define f5 FREG(5)
#define f6 FREG(6)
#define f7 FREG(7)
#define f8 FREG(8)
#define f9 FREG(9)
#define f10 FREG(10)
#define f11 FREG(11)
#define f12 FREG(12)
#define f13 FREG(13)
#define f14 FREG(14)
#define f15 FREG(15)
#define f16 FREG(16)
#define f17 FREG(17)
#define f18 FREG(18)
#define f19 FREG(19)
#define f20 FREG(20)
#define f21 FREG(21)
#define f22 FREG(22)
#define f23 FREG(23)
#define f24 FREG(24)
#define f25 FREG(25)
#define f26 FREG(26)
#define f27 FREG(27)
#define f28 FREG(28)
#define f29 FREG(29)
#define f30 FREG(30)
#define f31 FREG(31)

	/* Stack structure is:
           +4 :	cur_length   \  Stack
	   +0 :	cur_sp       /  Context
	   -4 :	arg
	   -8 :	func
	
	   -12:	pad
	   -16: pad
           -20: LR save word
	   -24:	Back chain
	
	   -28: fp/gp saved registers. 
	   -4 :	return address
	   -8 :	process function to be executed
           -12:	function argument
	   ...
           -72:	%sp
	*/
	
	/* Function called to loop on the process.  */	
	.align 4
	.type	grt_stack_loop,@function
grt_stack_loop:
	/* Get function.  */
	lwz	r0,16(r1)
	/* Get argument.  */
	lwz	r3,20(r1)
	mtlr	r0
	blrl
	b	grt_stack_loop
	.size	grt_stack_loop, . - grt_stack_loop

	/* function Stack_Create (Func : Address; Arg : Address)
	                         return Stack_Type;  */
	.align	4
	.global grt_stack_create
	.type	 grt_stack_create,@function
grt_stack_create:
	/* Standard prologue.  */
	stwu	r1,-32(r1)
	mflr	r0
	stw	r0,36(r1)

	/* Save arguments.  */
	stw	r3,24(r1)
	stw	r4,28(r1)
	
	/* Allocate the stack, and exit in case of failure  */
	bl	grt_stack_allocate
	cmpwi	0,r3,0
	beq-	.Ldone

	/* Note: r3 contains the address of the stack_context.  This is
	   also the top of the stack.  */
				
	/* Prepare stack.  */
	/* Align the stack.  */
	addi	r5,r3,-24

	/* Save the parameters.  */
	lwz	r6,24(r1)
	stw	r6,16(r5)
	lwz	r7,28(r1)
	stw	r7,20(r5)
	
	/* The return function.  */
	lis	r4,grt_stack_loop@ha
	la	r4,grt_stack_loop@l(r4)
	stw	r4,4(r5)
	/* Back-Chain.  */
	addi	r4,r1,32
	stw	r4,0(r5)

	/* Save register.
	   They should be considered as garbage.  */
	addi	r4,r5,-OFF
	
	stfd	f31,(OFF - 8)(r4)
	stfd	f30,(OFF - 16)(r4)
	stfd	f29,(OFF - 24)(r4)
	stfd	f28,(OFF - 32)(r4)
	stfd	f27,(OFF - 40)(r4)
	stfd	f26,(OFF - 48)(r4)
	stfd	f25,(OFF - 56)(r4)
	stfd	f24,(OFF - 64)(r4)
	stfd	f23,(OFF - 72)(r4)
	stfd	f22,(OFF - 80)(r4)
	stfd	f21,(OFF - 88)(r4)
	stfd	f20,(OFF - 96)(r4)
	stfd	f19,(OFF - 104)(r4)
	stfd	f18,(OFF - 112)(r4)
	stfd	f17,(OFF - 120)(r4)
	stfd	f16,(OFF - 128)(r4)
	stfd	f15,(OFF - 136)(r4)
	stfd	f14,(OFF - 144)(r4)
	stw	r31,(OFF - 148)(r4)
	stw	r30,(OFF - 152)(r4)
	stw	r29,(OFF - 156)(r4)
	stw	r28,(OFF - 160)(r4)
	stw	r27,(OFF - 164)(r4)
	stw	r26,(OFF - 168)(r4)
	stw	r25,(OFF - 172)(r4)
	stw	r24,(OFF - 176)(r4)
	stw	r23,(OFF - 180)(r4)
	stw	r22,(OFF - 184)(r4)
	stw	r21,(OFF - 188)(r4)
	stw	r20,(OFF - 192)(r4)
	stw	r19,(OFF - 196)(r4)
	stw	r18,(OFF - 200)(r4)
	stw	r17,(OFF - 204)(r4)
	stw	r16,(OFF - 208)(r4)
	stw	r15,(OFF - 212)(r4)
	stw	r14,(OFF - 216)(r4)
	mfcr	r0
	stw	r0, (OFF - 220)(r4)

	/* Save stack pointer.  */
	stw	r4, 0(r3)

.Ldone:
	lwz	r0,36(r1)
	mtlr	r0
	addi	r1,r1,32
	blr
	.size	 grt_stack_create,. - grt_stack_create


	.align 4
	.global grt_stack_switch
	/* Arguments:	TO, FROM.
	   Both are pointers to a stack_context.  */
	.type	 grt_stack_switch,@function
grt_stack_switch:
	/* Standard prologue, save return address.  */
	stwu	r1,(-OFF)(r1)
	mflr	r0
	stw	r0,(OFF + 4)(r1)

	/* Save r14-r31, f14-f31, CR
	   This is 18 words + 18 double words, ie 216 bytes.  */
	/* Maybe use the savefpr function ? */
	stfd	f31,(OFF - 8)(r1)
	stfd	f30,(OFF - 16)(r1)
	stfd	f29,(OFF - 24)(r1)
	stfd	f28,(OFF - 32)(r1)
	stfd	f27,(OFF - 40)(r1)
	stfd	f26,(OFF - 48)(r1)
	stfd	f25,(OFF - 56)(r1)
	stfd	f24,(OFF - 64)(r1)
	stfd	f23,(OFF - 72)(r1)
	stfd	f22,(OFF - 80)(r1)
	stfd	f21,(OFF - 88)(r1)
	stfd	f20,(OFF - 96)(r1)
	stfd	f19,(OFF - 104)(r1)
	stfd	f18,(OFF - 112)(r1)
	stfd	f17,(OFF - 120)(r1)
	stfd	f16,(OFF - 128)(r1)
	stfd	f15,(OFF - 136)(r1)
	stfd	f14,(OFF - 144)(r1)
	stw	r31,(OFF - 148)(r1)
	stw	r30,(OFF - 152)(r1)
	stw	r29,(OFF - 156)(r1)
	stw	r28,(OFF - 160)(r1)
	stw	r27,(OFF - 164)(r1)
	stw	r26,(OFF - 168)(r1)
	stw	r25,(OFF - 172)(r1)
	stw	r24,(OFF - 176)(r1)
	stw	r23,(OFF - 180)(r1)
	stw	r22,(OFF - 184)(r1)
	stw	r21,(OFF - 188)(r1)
	stw	r20,(OFF - 192)(r1)
	stw	r19,(OFF - 196)(r1)
	stw	r18,(OFF - 200)(r1)
	stw	r17,(OFF - 204)(r1)
	stw	r16,(OFF - 208)(r1)
	stw	r15,(OFF - 212)(r1)
	stw	r14,(OFF - 216)(r1)
	mfcr	r0
	stw	r0, (OFF - 220)(r1)
	
	/* Save stack pointer.  */
	stw	r1, 0(r4)

	/* Load stack pointer.  */
	lwz	r1, 0(r3)
			

	lfd	f31,(OFF - 8)(r1)
	lfd	f30,(OFF - 16)(r1)
	lfd	f29,(OFF - 24)(r1)
	lfd	f28,(OFF - 32)(r1)
	lfd	f27,(OFF - 40)(r1)
	lfd	f26,(OFF - 48)(r1)
	lfd	f25,(OFF - 56)(r1)
	lfd	f24,(OFF - 64)(r1)
	lfd	f23,(OFF - 72)(r1)
	lfd	f22,(OFF - 80)(r1)
	lfd	f21,(OFF - 88)(r1)
	lfd	f20,(OFF - 96)(r1)
	lfd	f19,(OFF - 104)(r1)
	lfd	f18,(OFF - 112)(r1)
	lfd	f17,(OFF - 120)(r1)
	lfd	f16,(OFF - 128)(r1)
	lfd	f15,(OFF - 136)(r1)
	lfd	f14,(OFF - 144)(r1)
	lwz	r31,(OFF - 148)(r1)
	lwz	r30,(OFF - 152)(r1)
	lwz	r29,(OFF - 156)(r1)
	lwz	r28,(OFF - 160)(r1)
	lwz	r27,(OFF - 164)(r1)
	lwz	r26,(OFF - 168)(r1)
	lwz	r25,(OFF - 172)(r1)
	lwz	r24,(OFF - 176)(r1)
	lwz	r23,(OFF - 180)(r1)
	lwz	r22,(OFF - 184)(r1)
	lwz	r21,(OFF - 188)(r1)
	lwz	r20,(OFF - 192)(r1)
	lwz	r19,(OFF - 196)(r1)
	lwz	r18,(OFF - 200)(r1)
	lwz	r17,(OFF - 204)(r1)
	lwz	r16,(OFF - 208)(r1)
	lwz	r15,(OFF - 212)(r1)
	lwz	r14,(OFF - 216)(r1)
	lwz	r0, (OFF - 220)(r1)
	mtcr	r0

	lwz	r0,(OFF + 4)(r1)
	mtlr	r0
	addi	r1,r1,OFF
	blr
	.size	 grt_stack_switch, . - grt_stack_switch

	
	.ident	"Written by T.Gingold"
