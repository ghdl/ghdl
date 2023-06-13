--  Verilog data storage
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
with Types; use Types;
with Interfaces;
with Ada.Unchecked_Conversion;

package Verilog.Storages is
   subtype Data_Ptr is System.Address;
   No_Data_Ptr : constant Data_Ptr := System.Null_Address;
   function Is_Null (Data : Data_Ptr) return Boolean;
   pragma Inline (Is_Null);

   type Storage_Index is new Uns32;
   type Storage_Type is
     array (Storage_Index range <>) of Interfaces.Unsigned_8;
   for Storage_Type'Alignment use 8;

   type Storage_Acc is access Storage_Type;
   type Storage_Ptr is access all Storage_Type (Storage_Index);

   function "+" (Data : Data_Ptr; Off : Storage_Index) return Data_Ptr;
   pragma Inline ("+");

   function To_Storage is
      new Ada.Unchecked_Conversion (Data_Ptr, Storage_Ptr);

   type Big_Frame_Type is new Storage_Type (Storage_Index);
   type Frame_Ptr is access all Big_Frame_Type;
   Null_Frame : constant Frame_Ptr := null;

   function To_Frame_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Frame_Ptr);

   type Fp32_Ptr is access all Fp32;
   function To_Fp32_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Fp32_Ptr);

   type Fp64_Ptr is access all Fp64;
   function To_Fp64_Ptr is
      new Ada.Unchecked_Conversion (Data_Ptr, Fp64_Ptr);

   type Any_Handle_Type is new Data_Ptr;
   type Any_Handle_Arr is array (Natural range <>) of Any_Handle_Type;

   --  Low-level memory management.
   function Malloc (S : Storage_Index) return Data_Ptr;
   pragma Import (C, Malloc);

   function Realloc (Ptr : Data_Ptr; S : Storage_Index) return Data_Ptr;
   pragma Import (C, Realloc);

   procedure Memcpy (Dst : Data_Ptr; Src : Data_Ptr; Sz : Storage_Index);
   pragma Import (C, Memcpy);

   procedure Free (F : Data_Ptr);
   pragma Import (C, Free);
end Verilog.Storages;
