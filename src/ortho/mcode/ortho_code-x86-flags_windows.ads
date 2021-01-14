--  X86 ABI flags.
--  Copyright (C) 2006 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
with Interfaces; use Interfaces;

package Ortho_Code.X86.Flags_Windows is
   --  If true, OE_Alloca calls __chkstk (Windows), otherwise OE_Alloc
   --  modifies ESP directly.
   Flag_Alloca_Call : constant Boolean := True;

   --  Prefered stack alignment.
   --  Must be a power of 2.
   Stack_Boundary : constant Unsigned_32 := 2 ** 3;

   --  Alignment for double (64 bit float).
   Mode_F64_Align : constant Natural := 3;

   --  32 bits.
   M64 : constant Boolean := False;

   --  Not Windows x64 calling convention.
   Win64 : constant Boolean := False;
end Ortho_Code.X86.Flags_Windows;
