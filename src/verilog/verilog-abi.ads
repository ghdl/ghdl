--  Types for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Types; use Types;

package Verilog.Abi is
   --  For pointers.
   Ptr_Size : constant := Standard'Address_Size / 8;
   Ptr_Align : constant := Ptr_Size;

   --  For SV strings.
   Sv_String_Size : constant := Ptr_Size;
   Sv_String_Align : constant := Ptr_Align;

   --  For real/shortreal.
   Real_Size : constant := Fp64'Size / 8;
   Real_Align : constant := Fp64'Alignment;

   Shortreal_Size : constant := Fp32'Size / 8;
   Shortreal_Align : constant := Fp32'Alignment;
end Verilog.Abi;
