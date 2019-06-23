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

package body Synth.Values is
   function To_Value_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Acc);
   function To_Value_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Values.Value_Array_Acc);
   function To_Value_Bound_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Bound_Acc);
   function To_Value_Bound_Array_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Bound_Array_Acc);

   function Create_Value_Wire (W : Wire_Id; Bnd : Value_Bound_Acc)
                              return Value_Acc
   is
      subtype Value_Type_Wire is Value_Type (Values.Value_Wire);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Wire);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Wire,
                                   W => W,
                                   W_Bound => Bnd)));
   end Create_Value_Wire;

   function Create_Value_Net (N : Net; Bnd : Value_Bound_Acc) return Value_Acc
   is
      subtype Value_Type_Net is Value_Type (Value_Net);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Net);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                Value_Type_Net'(Kind => Value_Net, N => N, N_Bound => Bnd)));
   end Create_Value_Net;

   function Create_Value_Mux2 (Cond : Value_Acc; T : Value_Acc; F : Value_Acc)
                              return Value_Acc
   is
      subtype Value_Type_Mux2 is Value_Type (Value_Mux2);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Mux2);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Mux2, M_Cond => Cond, M_T => T, M_F => F)));
   end Create_Value_Mux2;

   function Create_Value_Discrete (Val : Int64) return Value_Acc
   is
      subtype Value_Type_Discrete is Value_Type (Value_Discrete);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Discrete);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Discrete, Scal => Val)));
   end Create_Value_Discrete;

   function Create_Value_Float (Val : Fp64) return Value_Acc
   is
      subtype Value_Type_Float is Value_Type (Value_Float);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Float);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Float, Fp => Val)));
   end Create_Value_Float;

   function Create_Value_Array (Len : Iir_Index32) return Value_Array_Acc
   is
      use System;
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

      return To_Value_Array_Acc (Res);
   end Create_Value_Array;

   function Create_Value_Array (Bounds : Value_Bound_Array_Acc;
                                Arr : Value_Array_Acc)
                               return Value_Acc
   is
      subtype Value_Type_Array is Value_Type (Value_Array);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Array);

      Res : Value_Acc;
   begin
      Res := To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Array,
                                   Arr => Arr, Bounds => Bounds)));
      return Res;
   end Create_Value_Array;

   procedure Create_Array_Data (Arr : Value_Acc)
   is
      Len : Width;
   begin
      Len := 1;
      for I in Arr.Bounds.D'Range loop
         Len := Len * Arr.Bounds.D (I).Len;
      end loop;

      Arr.Arr := Create_Value_Array (Iir_Index32 (Len));
   end Create_Array_Data;


   function Create_Value_Array (Bounds : Value_Bound_Array_Acc)
                               return Value_Acc
   is
      Res : Value_Acc;
   begin
      Res := Create_Value_Array (Bounds, null);
      Create_Array_Data (Res);
      return Res;
   end Create_Value_Array;

   function Create_Value_Bound_Array (Ndim : Iir_Index32)
                                     return Value_Bound_Array_Acc
   is
      use System;
      subtype Data_Type is Value_Bound_Array (Ndim);
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

      return To_Value_Bound_Array_Acc (Res);
   end Create_Value_Bound_Array;

   function Create_Value_Bounds (Bounds : Value_Bound_Array_Acc)
                                return Value_Acc
   is
      subtype Value_Type_Bounds is Value_Type (Value_Bounds);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Bounds);

      Res : Value_Acc;
   begin
      Res := To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Bounds,
                                   Bnds => Bounds)));
      return Res;
   end Create_Value_Bounds;

   function Create_Value_Instance (Inst : Instance_Id) return Value_Acc
   is
      subtype Value_Type_Instance is Value_Type (Value_Instance);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Instance);
   begin
      return To_Value_Acc
        (Alloc (Current_Pool,
                (Kind => Value_Instance, Instance => Inst)));
   end Create_Value_Instance;

   function Create_Value_Range (Rng : Value_Range_Type) return Value_Acc
   is
      subtype Value_Type_Range is Value_Type (Value_Range);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Range);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Range, Rng => Rng)));
   end Create_Value_Range;

   function Create_Value_Fp_Range (Rng : Value_Fp_Range_Type) return Value_Acc
   is
      subtype Value_Type_Fp_Range is Value_Type (Value_Fp_Range);
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Type_Fp_Range);
   begin
      return To_Value_Acc (Alloc (Current_Pool,
                                  (Kind => Value_Fp_Range, Fp_Rng => Rng)));
   end Create_Value_Fp_Range;

   function Create_Value_Bound (Dir : Iir_Direction; Left, Right : Value_Acc)
                               return Value_Bound_Acc is
   begin
      pragma Assert (Left.Kind = Right.Kind);
      case Left.Kind is
         when Value_Discrete =>
            declare
               Len : Int64;
            begin
               case Dir is
                  when Iir_To =>
                     Len := Right.Scal - Left.Scal + 1;
                  when Iir_Downto =>
                     Len := Left.Scal - Right.Scal + 1;
               end case;
               if Len < 0 then
                  Len := 0;
               end if;
               return Create_Value_Bound
                 ((Dir, Int32 (Left.Scal), Int32 (Right.Scal),
                   Len => Uns32 (Len)));
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Value_Bound;

   function Create_Value_Bound (Bnd : Value_Bound_Type) return Value_Bound_Acc
   is
      function Alloc is new Areapools.Alloc_On_Pool_Addr (Value_Bound_Type);
   begin
      return To_Value_Bound_Acc (Alloc (Current_Pool, Bnd));
   end Create_Value_Bound;

   function Copy (Src: in Value_Acc) return Value_Acc
   is
      Res: Value_Acc;
   begin
      case Src.Kind is
         when Value_Range =>
            Res := Create_Value_Range (Src.Rng);
         when Value_Fp_Range =>
            Res := Create_Value_Fp_Range (Src.Fp_Rng);
         when Value_Wire =>
            Res := Create_Value_Wire (Src.W, Src.W_Bound);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Copy;

   function Unshare (Src : Value_Acc; Pool : Areapool_Acc)
                    return Value_Acc
   is
      Prev_Pool : constant Areapool_Acc := Current_Pool;
      Res : Value_Acc;
   begin
      Current_Pool := Pool;
      Res := Copy (Src);
      Current_Pool := Prev_Pool;
      return Res;
   end Unshare;
end Synth.Values;
