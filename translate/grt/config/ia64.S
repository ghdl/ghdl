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
	
	.global grt_stack_switch#
	.proc grt_stack_switch#
	/* r32:	struct stack_context *TO, r33:  struct stack_context *FROM.  */
	//  Registers to be saved:
	//  ar.rsc, ar.bsp, ar.pfs, ar.lc, ar.rnat [5]
	//  gp, r4-r7 (+ Nat)                      [6]
	//  f2-f5, f16-f31                         [20]
	//  p1-p5, p16-p63                         [1] ???
	//  b1-b5                                  [5]
grt_stack_switch:
	.prologue 2, 2
	.vframe r2
	alloc r31=ar.pfs, 2, 0, 0, 0
	adds r12 = -160, r12
	.body
	;;
	// Save ar.rsc, ar.bsp, ar.pfs
	{
	mov r14 = ar.rsc
	mov r15 = ar.bsp
	adds r22 = (5*8), r12
	}
	;;
	{
	st8 [r12] = r14      		// sp + 0  <- ar.rsc
	st8.spill [r22] = r1, 8		// sp + 40 <- r1
	adds r20 = 8, r12
	}
	;;
	st8 [r20] = r15, 8		// sp + 8  <- ar.bsp
	st8.spill [r22] = r4, 8		// sp + 48 <- r4
	;;
	mov r15 = ar.lc
	st8 [r20] = r31, 8		// sp + 16 <- ar.pfs
	st8.spill [r22] = r5, 8		// sp + 56 <- r5
	;;
	mov r14 = b0
	st8 [r20] = r15, 8		// sp + 24 <- ar.lc
	//  Flush dirty registers to the backing store
	flushrs
	;;
	//  Set the RSE in enforced lazy mode.
	mov ar.rsc = 0
	;; 
	mov r15 = ar.rnat	
	st8.spill [r22] = r6, 8		// sp + 64 <- r6
	;;
	mov r16 = b1
	st8.spill [r22] = r7, 16	// sp + 72 <- r7
	st8 [r20] = r15, 64		// sp + 32 <- ar.rnat
	;;
	mov r15 = b2
	mov r17 = ar.unat
	st8 [r20] = r14, 8		// sp + 96 <- b0
	;;
	mov r14 = b3
	st8 [r22] = r17, 24		// sp + 88 <- ar.unat
	st8 [r20] = r16, 16		// sp + 104 <- b1
	;;
	st8 [r22] = r15, 16		// sp + 112 <- b2
	st8 [r20] = r14, 16		// sp + 120 <- b3
	mov r16 = b4
	;;
	st8 [r22] = r16, 16		// sp + 128 <- b4
	//  Read new sp.
	ld8 r21 = [r32]
	mov r15 = b5
	;; 
	mov r14 = pr
	st8 [r20] = r15			// sp + 136 <- b5
	;;
	st8 [r22] = r14			// sp + 144 <- pr
	adds r20 = 8, r21
	;;
	invala	
	//  Save sp.
	st8 [r33] = r12
	ld8 r14 = [r21], 88		// sp + 0 (ar.rsc)
	;; 
	ld8 r15 = [r21], -56		// sp + 88 (ar.unat)
	ld8 r16 = [r20], 8		// sp + 8 (ar.bsp)
	;;
	ld8 r17 = [r21], 8		// sp + 32 (ar.rnat)
	ld8 r18 = [r20], 8		// sp + 16 (ar.pfs)
	mov ar.unat = r15
	;;
	ld8 r15 = [r20], 72		// sp + 24 (ar.lc)
	ld8.fill r1 = [r21], 8		// sp + 40 (r1)
	mov ar.bspstore = r16
	;; 
	mov ar.rnat = r17
	mov ar.pfs = r18
	ld8.fill r4 = [r21], 8		// sp + 48 (r4)
	;;
	mov ar.rsc = r14
	mov ar.lc = r15
	ld8 r17 = [r20], 8		// sp + 96 (b0)
	;;
	mov b0 = r17
	ld8.fill r5 = [r21], 8		// sp + 56 (r5)
	ld8 r14 = [r20], 8		// sp + 104 (b1)
	;;
	mov b1 = r14
	ld8.fill r6 = [r21], 8		// sp + 64 (r6)
	ld8 r15 = [r20], 8		// sp + 112 (b2)
	;;
	mov b2 = r15
	ld8.fill r7 = [r21], 8		// sp + 72 (r7)
	ld8 r14 = [r20], 8		// sp + 120 (b3)
	;;
	mov b3 = r14
	ld8 r15 = [r20], 8		// sp + 128 (b4)
	;;
	mov b4 = r15
	ld8 r14 = [r20], 8		// sp + 136 (b5)
	;;
	mov b5 = r14
	ld8 r15 = [r20], 8		// sp + 144 (pr)
	mov r12 = r21
	;;
	mov pr = r15, -1
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
	alloc r35 = ar.pfs, 2, 4, 0, 0
	.save rp, r34
	mov r34 = b0
	;; 
	.body
	br.call.sptk.many b0 = grt_stack_allocate#
	;;
	//  Compute backing store.
	movl r14=stack_max_size
	ld8 r22 = [r32], 8	// read ip (-> b1)
	adds r20 = -(160 + 16), r8
	adds r21 = -(160 + 16) + 32, r8
	;;
	mov r18 = 0x0f		// ar.rsc: LE, PL=3, Eager
	ld4 r14 = [r14]		// r16: bsp
	st8 [r21] = r0, 8	// sp + 32 (ar.rnat = 0)
	ld8 r23 = [r32]		// read r1 from func
	st8 [r8] = r20		// Save cur_sp
	;;
	st8 [r20] = r18, 8	// sp + 0 (ar.rsc)
	st8 [r21] = r23, 64	// sp + 40 (r1 = func.r1)
	sub r14 = r8, r14	// Backing store base
	;;
	adds r14 = 16, r14	// Add sizeof (stack_context)
	st8 [r21] = r22, -8	// sp + 104 (b1 = func.ip)
	;; 
	movl r15 = grt_stack_loop
	mov r16 = (0 << 7) | 1	// CFM:	sol=0, sof=1
	st8 [r20] = r14, 8	// sp + 8 (ar.bsp)
	;; 
	st8 [r21] = r15, -48	// sp + 96 (b0 = grt_stack_loop)
	st8 [r20] = r16, 8	// sp + 16 (ar.pfs)
	;;
	st8 [r20] = r0, 8	// sp + 24 (ar.lc)
	st8 [r21] = r33		// sp + 48 (r4 = arg)
	;;
	st8 [r20] = r0, 8	// sp + 32 (ar.rnat)
	;; 
	
	mov ar.pfs = r35
	mov b0 = r34
	br.ret.sptk.many b0
	;;
	.endp grt_stack_create#
	.ident	"GCC: (GNU) 4.0.2"
