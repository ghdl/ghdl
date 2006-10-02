	.file		"chkstk.S"
	.version	"01.01"
	
	.text

	/* Function called to loop on the process.  */	
	.align 4
	.type	_chkstk,@function
	.global _chkstk
_chkstk:
	testl	%eax,%eax
	je	_chkstk_zero
	subl	$4,%eax		/* 4 bytes already used by call.  */
	subl	%eax,%esp
	jmp	*(%esp,%eax)
_chkstk_zero:
	ret
	.size	_chkstk, . - _chkstk

	.ident	"Written by T.Gingold"
