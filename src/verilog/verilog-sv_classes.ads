--  Verilog classes
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
with Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;

package Verilog.Sv_Classes is
   type Sv_Class_Type is record
      --  Important: like a subroutine frame, the first word is orig.
      Orig : Nodes.Node;
      Refcnt : Uns32;
      Vtable : System.Address;
   end record;

   --  Like frames.
   Class_Link_Size   : constant Storage_Index := Sv_Class_Type'Size / 8;
   Class_Link_Align  : constant Storage_Index := Sv_Class_Type'Alignment;
   Class_Link_Offset : constant Storage_Index := 0;

   type Sv_Class_Handle is access all Sv_Class_Type;
   type Sv_Class_Ptr is access all Sv_Class_Handle;

   function To_Sv_Class_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Sv_Class_Ptr);

   function To_Frame_Ptr is new Ada.Unchecked_Conversion
     (Sv_Class_Handle, Verilog.Storages.Frame_Ptr);
   function To_Sv_Class_Handle is new Ada.Unchecked_Conversion
     (Verilog.Storages.Frame_Ptr, Sv_Class_Handle);
end Verilog.Sv_Classes;
