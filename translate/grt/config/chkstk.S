	.file		"chkstk.S"
	.version	"01.01"
	
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
