--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Ada.Unchecked_Conversion;
with System;
with Areapools;

package body Synth.Values is
   function To_Value_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Acc);
   function To_Value_Range_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Range_Acc);
   function To_Value_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Values.Value_Array_Acc);

   function Create_Value_Wire (W : Wire_Id; Rng : Value_Range_Acc)
                              return Value_Acc
   is
      subtype Value_Type_Wire is Value_Type (Values.Value_Wire);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Wire);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Wire,
                 W => W,
                 W_Range => Rng)));
   end Create_Value_Wire;

   function Create_Value_Net (N : Net; Rng : Value_Range_Acc) return Value_Acc
   is
      subtype Value_Type_Net is Value_Type (Value_Net);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Net);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                Value_Type_Net'(Kind => Value_Net, N => N, N_Range => Rng)));
   end Create_Value_Net;

   function Create_Value_Lit (Val : Iir_Value_Literal_Acc; Typ : Iir)
                             return Value_Acc
   is
      subtype Value_Type_Lit is Value_Type (Value_Lit);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Lit);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Lit, Lit => Val, Lit_Type => Typ)));
   end Create_Value_Lit;

   function Bounds_To_Nbr_Elements (Bounds : Value_Bounds_Array_Acc)
                                   return Iir_Index32
   is
      Len : Iir_Index32;
   begin
      Len := 1;
      for I in Bounds.D'Range loop
         Len := Len * Bounds.D (I).Length;
      end loop;
      return Len;
   end Bounds_To_Nbr_Elements;

   procedure Create_Array_Data (Arr : Value_Acc)
   is
      use System;
      use Areapools;
      Len : constant Iir_Index32 := Bounds_To_Nbr_Elements (Arr.Bounds);

      subtype Data_Type is Values.Value_Array_Type (Len);
      Res : Address;
   begin
      --  Manually allocate the array to handle large arrays without
      --  creating a large temporary value.
      Areapools.Allocate
        (Current_Pool.all, Res,
         Data_Type'Size / Storage_Unit, Data_Type'Alignment);

      declare
         --  Discard the warnings for no pragma Import as we really want
         --  to use the default initialization.
         pragma Warnings (Off);
         Addr1 : constant Address := Res;
         Init : Data_Type;
         for Init'Address use Addr1;
         pragma Warnings (On);
      begin
         null;
      end;

      Arr.Arr := To_Value_Array_Acc (Res);
   end Create_Array_Data;

   function Create_Array_Value (Bounds : Value_Bounds_Array_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Array is Value_Type (Values.Value_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Array);

      Res : Value_Acc;
   begin
      Res := To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Values.Value_Array,
                 Arr => null, Bounds => Bounds)));
      Create_Array_Data (Res);
      return Res;
   end Create_Array_Value;

   function Create_Range_Value (Rng : Value_Range) return Value_Range_Acc
   is
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Range);
   begin
      return To_Value_Range_Acc (Alloc (Current_Pool, Rng));
   end Create_Range_Value;

   function Bounds_To_Range (Val : Iir_Value_Literal_Acc)
                            return Value_Range_Acc
   is
      pragma Assert (Val.Kind = Iir_Value_Range);
      pragma Assert (Val.Left.Kind = Iir_Value_I64);
      pragma Assert (Val.Right.Kind = Iir_Value_I64);
   begin
      return Create_Range_Value ((Dir => Val.Dir,
                                  Len => Width (Val.Length),
                                  Left => Int32 (Val.Left.I64),
                                  Right => Int32 (Val.Right.I64)));
   end Bounds_To_Range;
end Synth.Values;
