--  Verilog basic types for internal objects
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;

package Verilog.Types is
   --  Width (number of bits/logic) of a packed type.  Note that a type cannot
   --  be empty (so the minimal width is 1).
   type Width_Type is new Nat32;
   for Width_Type'Size use 32;

   --  Size of verilog types.
   type Tsize_Type is new Nat32;
   for Tsize_Type'Size use 32;

   --  Bit offset in packed type indexing.
   --  According to IEE1800-2017 6.9.1 Specifying vectors, the maximum length
   --  limit is at least 2**16 bits.
   type Bit_Offset is new Uns32;
   for Bit_Offset'Size use 32;

   type Bn_Index is new Uns32;
   for Bn_Index'Size use 32;
   No_Bn_Index : constant Bn_Index := 0;

   type Obj_Id is new Nat32;
   No_Obj_Id : constant Obj_Id := 0;

   type Lit_Id is new Nat32;
   No_Lit_Id : constant Lit_Id := 0;

   type Proc_Id is new Nat32;
   No_Proc_Id : constant Proc_Id := 0;

   type Scope_Id is new Nat32;
   No_Scope_Id : constant Scope_Id := 0;
end Verilog.Types;
