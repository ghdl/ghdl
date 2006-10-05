	.file		"chkstk.S"
	.version	"01.01"
	
	.text

	/* Function called to loop on the process.  */	
	.align 4
	.type	__chkstk,@function
	.global __chkstk
__chkstk:
	testl	%eax,%eax
	je	0f
	subl	$4,%eax		/* 4 bytes already used by call.  */
	subl	%eax,%esp
	jmp	*(%esp,%eax)
0:
	ret
	.size	__chkstk, . - __chkstk

	.ident	"Written by T.Gingold"
