	.file	"ia64.S"
	.pred.safe_across_calls p1-p5,p16-p63
	
	.text
	.align 16
	.proc grt_stack_loop
grt_stack_loop:
	alloc r32 = ar.pfs, 0, 1, 1, 0
	.body
	;;
1:	mov r33 = r4	
	br.call.sptk.many b0 = b1
	;;
	br 1b
	.endp

	frame_size = 480
	
	.global grt_stack_switch#
	.proc grt_stack_switch#
	/* r32:	struct stack_context *TO, r33:  struct stack_context *FROM.  */
	//  Registers to be saved:
	//  ar.rsc, ar.bsp, ar.pfs, ar.lc, ar.rnat [5]
	//  gp, r4-r7 (+ Nat)                      [6]
	//  f2-f5, f16-f31                         [20]
	//  p1-p5, p16-p63                         [1] ???
	//  b1-b5                                  [5]
	//  f2-f5, f16-f31			   [20*16]
grt_stack_switch:
	.prologue 2, 2
	.vframe r2
	{
	alloc r31=ar.pfs, 2, 0, 0, 0
	mov r14 = ar.rsc
	adds r12 = -frame_size, r12
	.body
	;;
	}
	// Save ar.rsc, ar.bsp, ar.pfs
	{
	st8 [r12] = r14      		// sp + 0  <- ar.rsc
	mov r15 = ar.bsp
	adds r22 = (5*8), r12
	;;
	}
	{
	st8.spill [r22] = r1, 8		// sp + 40 <- r1
	;; 
	st8.spill [r22] = r4, 8		// sp + 48 <- r4
	adds r20 = 8, r12
	;;
	}
	st8 [r20] = r15, 8		// sp + 8  <- ar.bsp
	st8.spill [r22] = r5, 8		// sp + 56 <- r5
	mov r15 = ar.lc
	;;
	{
	st8 [r20] = r31, 8		// sp + 16 <- ar.pfs
	//  Flush dirty registers to the backing store
	flushrs
	mov r14 = b0
	;;
	}
	{
	st8 [r20] = r15, 8		// sp + 24 <- ar.lc
	//  Set the RSE in enforced lazy mode.
	mov ar.rsc = 0
	;;
	}
	{
	//  Save sp.
	st8 [r33] = r12
	mov r15 = ar.rnat
	mov r16 = b1
	;;
	}
	{
	st8.spill [r22] = r6, 8		// sp + 64 <- r6
	st8 [r20] = r15, 64		// sp + 32 <- ar.rnat
	;;
	}
	{
	st8.spill [r22] = r7, 16	// sp + 72 <- r7
	st8 [r20] = r14, 8		// sp + 96 <- b0
	mov r15 = b2
	;;
	}
	{
	mov r17 = ar.unat
	;;
	st8 [r22] = r17, 24		// sp + 88 <- ar.unat
	mov r14 = b3
	;; 
	}
	{
	st8 [r20] = r16, 16		// sp + 104 <- b1
	st8 [r22] = r15, 16		// sp + 112 <- b2
	mov r17 = b4
	;;
	}
	{
	st8 [r20] = r14, 16		// sp + 120 <- b3
	st8 [r22] = r17, 16		// sp + 128 <- b4
	mov r15 = b5
	;; 
	}
	{
	//  Read new sp.
	ld8 r21 = [r32]
	;; 
	st8 [r20] = r15, 24		// sp + 136 <- b5
	mov r14 = pr
	;; 
	}
	;;
	st8 [r22] = r14, 32		// sp + 144 <- pr
	stf.spill [r20] = f2, 32	// sp + 160 <- f2
	;;
	stf.spill [r22] = f3, 32	// sp + 176 <- f3
	stf.spill [r20] = f4, 32	// sp + 192 <- f4
	;;
	stf.spill [r22] = f5, 32	// sp + 208 <- f5
	stf.spill [r20] = f16, 32	// sp + 224 <- f16
	;;
	stf.spill [r22] = f17, 32	// sp + 240 <- f17
	stf.spill [r20] = f18, 32	// sp + 256 <- f18
	;;
	stf.spill [r22] = f19, 32	// sp + 272 <- f19
	stf.spill [r20] = f20, 32	// sp + 288 <- f20
	;;
	stf.spill [r22] = f21, 32	// sp + 304 <- f21
	stf.spill [r20] = f22, 32	// sp + 320 <- f22
	;;
	stf.spill [r22] = f23, 32	// sp + 336 <- f23
	stf.spill [r20] = f24, 32	// sp + 352 <- f24
	;;
	stf.spill [r22] = f25, 32	// sp + 368 <- f25
	stf.spill [r20] = f26, 32	// sp + 384 <- f26
	;;
	stf.spill [r22] = f27, 32	// sp + 400 <- f27
	stf.spill [r20] = f28, 32	// sp + 416 <- f28
	;;
	stf.spill [r22] = f29, 32	// sp + 432 <- f29
	stf.spill [r20] = f30, 32	// sp + 448 <- f30
	;;
	{
	stf.spill [r22] = f31, 32	// sp + 464 <- f31
	invala	
	adds r20 = 8, r21
	;;
	}
	ld8 r14 = [r21], 88		// sp + 0 (ar.rsc)
	ld8 r16 = [r20], 8		// sp + 8 (ar.bsp)
	;; 
	ld8 r15 = [r21], -56		// sp + 88 (ar.unat)
	;; 
	ld8 r18 = [r20], 8		// sp + 16 (ar.pfs)
	mov ar.unat = r15
	ld8 r17 = [r21], 8		// sp + 32 (ar.rnat)
	;;
	ld8 r15 = [r20], 72		// sp + 24 (ar.lc)
	ld8.fill r1 = [r21], 8		// sp + 40 (r1)
	mov ar.bspstore = r16
	;; 
	ld8.fill r4 = [r21], 8		// sp + 48 (r4)
	mov ar.pfs = r18
	mov ar.rnat = r17
	;;
	mov ar.rsc = r14
	mov ar.lc = r15
	ld8 r17 = [r20], 8		// sp + 96 (b0)
	;;
	{
	ld8.fill r5 = [r21], 8		// sp + 56 (r5)
	ld8 r14 = [r20], 8		// sp + 104 (b1)
	mov b0 = r17
	;;
	}
	{
	ld8.fill r6 = [r21], 8		// sp + 64 (r6)
	ld8 r15 = [r20], 8		// sp + 112 (b2)
	mov b1 = r14
	;;
	}
	ld8.fill r7 = [r21], 64		// sp + 72 (r7)
	ld8 r14 = [r20], 8		// sp + 120 (b3)
	mov b2 = r15
	;;
	ld8 r15 = [r20], 16		// sp + 128 (b4)
	ld8 r16 = [r21], 40		// sp + 136 (b5)
	mov b3 = r14
	;;
	{
	ld8 r14 = [r20], 16		// sp + 144 (pr)
	;;
	ldf.fill f2 = [r20], 32		// sp + 160 (f2)
	mov b4 = r15
	;;
	}
	ldf.fill f3 = [r21], 32		// sp + 176 (f3)
	ldf.fill f4 = [r20], 32		// sp + 192 (f4)
	mov b5 = r16
	;;
	ldf.fill f5 = [r21], 32		// sp + 208 (f5)
	ldf.fill f16 = [r20], 32	// sp + 224 (f16)
	mov pr = r14, -1
	;;
	ldf.fill f17 = [r21], 32	// sp + 240 (f17)
	ldf.fill f18 = [r20], 32	// sp + 256 (f18)
	;;
	ldf.fill f19 = [r21], 32	// sp + 272 (f19)
	ldf.fill f20 = [r20], 32	// sp + 288 (f20)
	;;
	ldf.fill f21 = [r21], 32	// sp + 304 (f21)
	ldf.fill f22 = [r20], 32	// sp + 320 (f22)
	;;
	ldf.fill f23 = [r21], 32	// sp + 336 (f23)
	ldf.fill f24 = [r20], 32	// sp + 352 (f24)
	;;
	ldf.fill f25 = [r21], 32	// sp + 368 (f25)
	ldf.fill f26 = [r20], 32	// sp + 384 (f26)
	;;
	ldf.fill f27 = [r21], 32	// sp + 400 (f27)
	ldf.fill f28 = [r20], 32	// sp + 416 (f28)
	;;
	ldf.fill f29 = [r21], 32	// sp + 432 (f29)
	ldf.fill f30 = [r20], 32	// sp + 448 (f30)
	;;
	ldf.fill f31 = [r21], 32	// sp + 464 (f31)
	mov r12 = r20
	br.ret.sptk.many b0
	;;
	.endp grt_stack_switch#
	
	.align 16
	// r32:	 func, r33: arg
	.global grt_stack_create#
	.proc grt_stack_create#
grt_stack_create:
	.prologue 14, 34
	.save ar.pfs, r35
	alloc r35 = ar.pfs, 2, 3, 0, 0
	.save rp, r34
	//  Compute backing store.
	movl r14 = stack_max_size
	;; 
	.body
	{
	ld4 r36 = [r14]		// r14: bsp
	mov r34 = b0
	br.call.sptk.many b0 = grt_stack_allocate#
	;;
	}
	{
	ld8 r22 = [r32], 8	// read ip (-> b1)
	;;
	ld8 r23 = [r32]		// read r1 from func
	adds r21 = -(frame_size + 16) + 32, r8
	;;
	}
	{
	st8 [r21] = r0, -32	// sp + 32 (ar.rnat = 0)
	;; 
	st8 [r8] = r21		// Save cur_sp
	mov r18 = 0x0f		// ar.rsc: LE, PL=3, Eager
	;;
	}
	{
	st8 [r21] = r18, 40	// sp + 0 (ar.rsc)
	;;
	st8 [r21] = r23, 64	// sp + 40 (r1 = func.r1)
	mov b0 = r34
	;;
	}
	{
	st8 [r21] = r22, -96	// sp + 104 (b1 = func.ip)
	movl r15 = grt_stack_loop
	;; 
	}	
	sub r14 = r8, r36	// Backing store base
	;;
	adds r14 = 16, r14	// Add sizeof (stack_context)
	adds r20 = 40, r21
	;;
	{
	st8 [r21] = r14, 88	// sp + 8 (ar.bsp)
	;; 
	st8 [r21] = r15, -80	// sp + 96 (b0 = grt_stack_loop)
	mov r16 = (0 << 7) | 1	// CFM:	sol=0, sof=1
	;;
	}
	{
	st8 [r21] = r16, 8	// sp + 16 (ar.pfs)
	;; 
	st8 [r21] = r0, 24	// sp + 24 (ar.lc)
	mov ar.pfs = r35
	;;
	}
	{
	st8 [r20] = r0, 8	// sp + 32 (ar.rnat)
	st8 [r21] = r33		// sp + 48 (r4 = arg)
	br.ret.sptk.many b0
	;;
	}
	.endp grt_stack_create#
	.ident	"GCC: (GNU) 4.0.2"
