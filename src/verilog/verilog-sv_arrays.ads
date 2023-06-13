--  Verilog dynamic arrays
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

with System;
with Ada.Unchecked_Conversion;
with Types; use Types;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Sv_Arrays is
   type Sv_Dyn_Array_Type (Ssize : Storage_Index) is record
      --  Number of elements.
      Size : Int32;
      --  The data.
      Base : Storage_Type (1 .. Ssize);
   end record;
   type Sv_Dyn_Array_Ptr is access all Sv_Dyn_Array_Type;

   type Sv_Dyn_Array_Ptr_Ptr is access all Sv_Dyn_Array_Ptr;

   function To_Sv_Dyn_Array_Ptr_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Sv_Dyn_Array_Ptr_Ptr);

   procedure Delete (Arr : in out Sv_Dyn_Array_Ptr);
end Verilog.Sv_Arrays;
