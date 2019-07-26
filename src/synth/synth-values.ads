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

with Types; use Types;
with Netlists; use Netlists;
with Vhdl.Nodes; use Vhdl.Nodes;
with Synth.Environment; use Synth.Environment;
with Areapools; use Areapools;


package Synth.Values is
   --  Values is how signals and variables are decomposed.  This is similar to
   --  values in simulation, but simplified (no need to handle files,
   --  accesses...)

   type Value_Kind is
     (
      --  Value is for a vector or a bit, and is the output of a gate.
      Value_Net,

      --  Also a vector or a bit, but from an object.  Has to be transformed
      --  into a net.
      Value_Wire,

      Value_Mux2,

      --  A discrete value (integer or enumeration).
      Value_Discrete,

      Value_Float,

      Value_Range,
      Value_Fp_Range,

      --  A range with a length.
      Value_Bound,

      --  A vector of bounds, for arrays.
      Value_Bounds,

      --  A non-vector array.
      Value_Array,

      --  A record.
      Value_Record,

      --  A package.
      Value_Instance
     );

   type Value_Type (Kind : Value_Kind);

   type Value_Acc is access Value_Type;

   type Value_Type_Array is array (Iir_Index32 range <>) of Value_Acc;

   type Value_Array_Type (Len : Iir_Index32) is record
      --  Values are from left to right.  So V(1) is at index 'Left.
      V : Value_Type_Array (1 .. Len);
   end record;

   type Value_Array_Acc is access Value_Array_Type;

   type Value_Range_Type is record
      --  An integer range.
      Dir : Iir_Direction;

      --  Netlist representation: signed or unsigned, width of bus.
      Is_Signed : Boolean;
      W : Width;

      Left : Int64;
      Right : Int64;
   end record;

   type Value_Fp_Range_Type is record
      Dir : Iir_Direction;
      Left : Fp64;
      Right : Fp64;
   end record;

   type Value_Bound_Type is record
      Dir : Iir_Direction;
      Left : Int32;
      Right : Int32;
      Len : Width;
   end record;

   type Value_Bound_Acc is access Value_Bound_Type;

   No_Bound : constant Value_Bound_Acc := null;

   type Value_Bound_Array_Type is array (Iir_Index32 range <>) of
     Value_Bound_Acc;

   type Value_Bound_Array (Len : Iir_Index32) is record
      D : Value_Bound_Array_Type (1 .. Len);
   end record;

   type Value_Bound_Array_Acc is access Value_Bound_Array;

   type Instance_Id is new Nat32;

   type Value_Type (Kind : Value_Kind) is record
      case Kind is
         when Value_Net =>
            N : Net;
            N_Bound : Value_Bound_Acc;
         when Value_Wire =>
            W : Wire_Id;
            W_Bound : Value_Bound_Acc;
         when Value_Mux2 =>
            M_Cond : Value_Acc;
            M_T : Value_Acc;
            M_F : Value_Acc;
         when Value_Discrete =>
            Scal : Int64;
         when Value_Float =>
            Fp : Fp64;
         when Value_Range =>
            Rng : Value_Range_Type;
         when Value_Fp_Range =>
            Fp_Rng : Value_Fp_Range_Type;
         when Value_Bound =>
            Bnd : Value_Bound_Acc;
         when Value_Bounds =>
            Bnds : Value_Bound_Array_Acc;
         when Value_Array =>
            Arr : Value_Array_Acc;
            Bounds : Value_Bound_Array_Acc;
         when Value_Record =>
            Rec : Value_Array_Acc;
         when Value_Instance =>
            Instance : Instance_Id;
      end case;
   end record;

   Global_Pool : aliased Areapool;
   Expr_Pool : aliased Areapool;

   --  Areapool used by Create_*_Value
   Current_Pool : Areapool_Acc := Expr_Pool'Access;

   --  Pool for objects allocated in the current instance.
   Instance_Pool : Areapool_Acc;

   function Is_Equal (L, R : Value_Acc) return Boolean;

   --  Create a Value_Net.
   function Create_Value_Net (N : Net; Bnd : Value_Bound_Acc) return Value_Acc;

   --  Create a Value_Wire.  For a bit wire, RNG must be null.
   function Create_Value_Wire (W : Wire_Id; Bnd : Value_Bound_Acc)
                              return Value_Acc;

   --  Create a mux2.
   function Create_Value_Mux2 (Cond : Value_Acc; T : Value_Acc; F : Value_Acc)
                              return Value_Acc;

   function Create_Value_Discrete (Val : Int64) return Value_Acc;

   function Create_Value_Float (Val : Fp64) return Value_Acc;

   function Create_Value_Array (Len : Iir_Index32) return Value_Array_Acc;
   function Create_Value_Bound_Array (Ndim : Iir_Index32)
                                     return Value_Bound_Array_Acc;

   --  Create a Value_Array.
   function Create_Value_Array (Bounds : Value_Bound_Array_Acc;
                                Arr : Value_Array_Acc)
                               return Value_Acc;

   --  Like the previous one but automatically build the array.
   function Create_Value_Array (Bounds : Value_Bound_Array_Acc)
                               return Value_Acc;

   function Create_Value_Bounds (Bounds : Value_Bound_Array_Acc)
                                return Value_Acc;

   --  Allocate the ARR component of the Value_Type ARR, using BOUNDS.
   procedure Create_Array_Data (Arr : Value_Acc);

   function Create_Value_Instance (Inst : Instance_Id) return Value_Acc;

   function Create_Value_Bound (Bnd : Value_Bound_Type) return Value_Bound_Acc;

   --  Allocate a Value_Range.
   function Create_Value_Range (Rng : Value_Range_Type) return Value_Acc;
   function Create_Value_Fp_Range (Rng : Value_Fp_Range_Type) return Value_Acc;
   function Create_Value_Bound (Dir : Iir_Direction; Left, Right : Value_Acc)
                               return Value_Bound_Acc;

   function Unshare (Src : Value_Acc; Pool : Areapool_Acc)
                    return Value_Acc;

   function Extract_Bound (Val : Value_Acc) return Value_Bound_Acc;

   function Get_Bound_Width (Bnd : Value_Bound_Acc) return Width;
end Synth.Values;
