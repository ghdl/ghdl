--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with System;
with Ada.Unchecked_Conversion;

with Types; use Types;

with Grt.Types; use Grt.Types;

package Elab.Memtype is
   type Memory_Element is mod 2**8;
   type Memory_Array is array (Size_Type range <>) of Memory_Element;

   --  Thin pointer for a generic pointer.
   type Memory_Ptr is access all Memory_Array (Size_Type);
   pragma No_Strict_Aliasing (Memory_Ptr);

   --  For conversions use Address to avoid compiler warnings about alignment.
   function To_Address is new Ada.Unchecked_Conversion
     (Memory_Ptr, System.Address);
   function To_Memory_Ptr is new Ada.Unchecked_Conversion
     (System.Address, Memory_Ptr);

   --  Low-level functions.

   function "+" (Base : Memory_Ptr; Off : Size_Type) return Memory_Ptr;

   procedure Write_U8 (Mem : Memory_Ptr; Val : Ghdl_U8);
   function Read_U8 (Mem : Memory_Ptr) return Ghdl_U8;

   procedure Write_U32 (Mem : Memory_Ptr; Val : Ghdl_U32);
   function Read_U32 (Mem : Memory_Ptr) return Ghdl_U32;

   procedure Write_I32 (Mem : Memory_Ptr; Val : Ghdl_I32);
   function Read_I32 (Mem : Memory_Ptr) return Ghdl_I32;

   procedure Write_I64 (Mem : Memory_Ptr; Val : Ghdl_I64);
   function Read_I64 (Mem : Memory_Ptr) return Ghdl_I64;

   procedure Write_Fp64 (Mem : Memory_Ptr; Val : Fp64);
   function Read_Fp64 (Mem : Memory_Ptr) return Fp64;
end Elab.Memtype;
