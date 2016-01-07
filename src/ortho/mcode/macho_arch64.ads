--  Macho definitions.
--  Copyright (C) 2015 Tristan Gingold
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
with Macho;

package Macho_Arch64 is
   subtype Addr_T is Unsigned_64;
   subtype Header is Macho.Header_64;
   Header_Size : constant Natural := Macho.Header_64_Size;
   Magic : constant Unsigned_32 := Macho.Magic_64;

   Lc_Segment : constant Unsigned_32 := Macho.Lc_Segment_64;
   subtype Segment_Command is Macho.Segment_Command_64;
   Segment_Command_Size : constant Natural := Macho.Segment_Command_64_Size;

   subtype Section is Macho.Section_64;
   Section_Size : constant Natural := Macho.Section_64_Size;

   subtype Nlist is Macho.Nlist_64;
   Nlist_Size : constant Natural := Macho.Nlist_64_Size;
end Macho_Arch64;
