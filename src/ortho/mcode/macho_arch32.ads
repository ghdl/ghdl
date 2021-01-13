--  Macho definitions.
--  Copyright (C) 2015 Tristan Gingold
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
with Macho;

package Macho_Arch32 is
   subtype Addr_T is Unsigned_32;
   subtype Header is Macho.Header_32;
   Header_Size : constant Natural := Macho.Header_32_Size;
   Magic : constant Unsigned_32 := Macho.Magic_32;

   Lc_Segment : constant Unsigned_32 := Macho.Lc_Segment_32;
   subtype Segment_Command is Macho.Segment_Command_32;
   Segment_Command_Size : constant Natural := Macho.Segment_Command_32_Size;

   subtype Section is Macho.Section_32;
   Section_Size : constant Natural := Macho.Section_32_Size;

   subtype Nlist is Macho.Nlist_32;
   Nlist_Size : constant Natural := Macho.Nlist_32_Size;
end Macho_Arch32;
