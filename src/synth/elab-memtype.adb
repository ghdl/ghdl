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

with System; use System;
with System.Storage_Elements;

package body Elab.Memtype is

   function "+" (Base : Memory_Ptr; Off : Size_Type) return Memory_Ptr
   is
      use System.Storage_Elements;
   begin
      return To_Memory_Ptr (To_Address (Base) + Storage_Offset (Off));
   end "+";

   type Ghdl_U8_Ptr is access all Ghdl_U8;
   function To_U8_Ptr is
      new Ada.Unchecked_Conversion (Address, Ghdl_U8_Ptr);

   procedure Write_U8 (Mem : Memory_Ptr; Val : Ghdl_U8) is
   begin
      To_U8_Ptr (To_Address (Mem)).all := Val;
   end Write_U8;

   function Read_U8 (Mem : Memory_Ptr) return Ghdl_U8 is
   begin
      return To_U8_Ptr (To_Address (Mem)).all;
   end Read_U8;

   procedure Write_I32 (Mem : Memory_Ptr; Val : Ghdl_I32)
   is
      V : Ghdl_I32;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      V := Val;
   end Write_I32;

   function Read_I32 (Mem : Memory_Ptr) return Ghdl_I32
   is
      V : Ghdl_I32;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      return V;
   end Read_I32;

   procedure Write_U32 (Mem : Memory_Ptr; Val : Ghdl_U32)
   is
      V : Ghdl_U32;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      V := Val;
   end Write_U32;

   function Read_U32 (Mem : Memory_Ptr) return Ghdl_U32
   is
      V : Ghdl_U32;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      return V;
   end Read_U32;

   procedure Write_I64 (Mem : Memory_Ptr; Val : Ghdl_I64)
   is
      V : Ghdl_I64;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      V := Val;
   end Write_I64;

   function Read_I64 (Mem : Memory_Ptr) return Ghdl_I64
   is
      V : Ghdl_I64;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      return V;
   end Read_I64;

   procedure Write_Fp64 (Mem : Memory_Ptr; Val : Fp64)
   is
      V : Fp64;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      V := Val;
   end Write_Fp64;

   function Read_Fp64 (Mem : Memory_Ptr) return Fp64
   is
      V : Fp64;
      for V'Address use To_Address (Mem);
      pragma Import (Ada, V);
   begin
      return V;
   end Read_Fp64;

end Elab.Memtype;
