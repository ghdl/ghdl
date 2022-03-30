/*  GRT stack implementation for x86.
    Copyright (C) 2002 - 2014 Tristan Gingold.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <gnu.org/licenses>.

    As a special exception, if other files instantiate generics from this
    unit, or you link this unit with other files to produce an executable,
    this unit does not by itself cause the resulting executable to be
    covered by the GNU General Public License. This exception does not
    however invalidate any other reasons why the executable file might be
    covered by the GNU Public License.
*/
	.file		"chkstk-x64.S"

	.text

/* Check stack for alloca: probe every page.
   Argument is %rax (amount of bytes to be allocated)
   *No* registers are clobbered.  */
	
	.align 4
	.globl __chkstk_ms
__chkstk_ms:
	push   %rcx
	push   %rax
	cmp    $0x1000,%rax
	lea    0x18(%rsp),%rcx
	jb     1f
0:	sub    $0x1000,%rcx
	orq    $0x0,(%rcx)
	sub    $0x1000,%rax
	cmp    $0x1000,%rax
	ja     0b
1:	sub    %rax,%rcx
	orq    $0x0,(%rcx)
	pop    %rax
	pop    %rcx
	ret
