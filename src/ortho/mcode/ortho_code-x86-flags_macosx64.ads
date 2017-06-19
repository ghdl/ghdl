--  X86-64 ABI flags for MacOS X.
--  Copyright (C) 2006 - 2015 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Interfaces; use Interfaces;

package Ortho_Code.X86.Flags_Macosx64 is
   --  If true, OE_Alloca calls __chkstk (Windows), otherwise OE_Alloc
   --  modifies ESP directly.
   Flag_Alloca_Call : constant Boolean := False;

   --  Prefered stack alignment.
   --  Must be a power of 2.
   Stack_Boundary : constant Unsigned_32 := 2 ** 4;

   --  Alignment for double (64 bit float).
   Mode_F64_Align : constant Natural := 3;

   --  64 bits.
   M64 : constant Boolean := True;

   --  Not Windows x64 calling convention.
   Win64 : constant Boolean := False;
end Ortho_Code.X86.Flags_Macosx64;
