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
with Areapools; use Areapools;

with Grt.Files_Operations;

with Netlists; use Netlists;

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Environment; use Synth.Environment;
with Synth.Source; use Synth.Source;

package Synth.Values is
   type Discrete_Range_Type is record
      --  An integer range.
      Dir : Iir_Direction;

      --  Netlist representation: signed or unsigned, width of vector.
      Is_Signed : Boolean;

      Left : Int64;
      Right : Int64;
   end record;

   --  Return the width of RNG.
   function Discrete_Range_Width (Rng : Discrete_Range_Type) return Width;

   type Float_Range_Type is record
      Dir : Iir_Direction;
      Left : Fp64;
      Right : Fp64;
   end record;

   type Bound_Type is record
      Dir : Iir_Direction;
      Left : Int32;
      Right : Int32;
      Len : Width;
   end record;

   type Bound_Array_Type is array (Iir_Index32 range <>) of Bound_Type;

   type Bound_Array (Len : Iir_Index32) is record
      D : Bound_Array_Type (1 .. Len);
   end record;

   type Bound_Array_Acc is access Bound_Array;

   type Type_Kind is
     (
      Type_Bit,
      Type_Logic,
      Type_Discrete,
      Type_Float,
      Type_Vector,
      Type_Unbounded_Vector,

      --  A slice is for a slice of vector with dynamic bounds.  So the bounds
      --  of the result aren't known, but its width is.
      Type_Slice,
      Type_Array,
      Type_Unbounded_Array,
      Type_Record,

      Type_Access,
      Type_File
     );

   subtype Type_Nets is Type_Kind range Type_Bit .. Type_Logic;

   type Type_Type (Kind : Type_Kind);
   type Type_Acc is access Type_Type;

   type Rec_El_Type is record
      Off : Uns32;
      Typ : Type_Acc;
   end record;

   type Rec_El_Array_Type is array (Iir_Index32 range <>) of Rec_El_Type;
   type Rec_El_Array (Len : Iir_Index32) is record
      E : Rec_El_Array_Type (1 .. Len);
   end record;

   type Rec_El_Array_Acc is access Rec_El_Array;

   type Type_Type (Kind : Type_Kind) is record
      --  False if the type is not synthesisable: is or contains access/file.
      Is_Synth : Boolean;

      --  Number of bits for this type.
      W : Width;

      case Kind is
         when Type_Bit
           | Type_Logic =>
            null;
         when Type_Discrete =>
            Drange : Discrete_Range_Type;
         when Type_Float =>
            Frange : Float_Range_Type;
         when Type_Vector =>
            Vbound : Bound_Type;
            Vec_El : Type_Acc;
         when Type_Unbounded_Vector =>
            Uvec_El : Type_Acc;
         when Type_Slice =>
            Slice_El : Type_Acc;
         when Type_Array =>
            Abounds : Bound_Array_Acc;
            Arr_El : Type_Acc;
         when Type_Unbounded_Array =>
            Uarr_Ndim : Iir_Index32;
            Uarr_El : Type_Acc;
         when Type_Record =>
            Rec : Rec_El_Array_Acc;
         when Type_Access =>
            Acc_Acc : Type_Acc;
         when Type_File =>
            File_Typ : Type_Acc;
      end case;
   end record;

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

      --  A discrete value (integer or enumeration).
      Value_Discrete,

      Value_Float,

      --  An array (const if all elements are constants).
      Value_Array,
      Value_Const_Array,

      --  A record (const if all elements are constants).
      Value_Record,
      Value_Const_Record,

      Value_Access,
      Value_File,

      --  A package.
      Value_Instance,

      --  A constant.  This is a named value.  One purpose is to avoid to
      --  create many times the same net for the same value.
      Value_Const,

      --  An alias.  This is a reference to another value with a different
      --  (but compatible) type.
      Value_Alias,

      --  A subtype.  Contains only a type.
      Value_Subtype
     );

   type Value_Type (Kind : Value_Kind);

   type Value_Acc is access Value_Type;

   type Value_Type_Array is array (Iir_Index32 range <>) of Value_Acc;

   type Value_Array_Type (Len : Iir_Index32) is record
      --  Values are from left to right.  So V(1) is at index 'Left.
      V : Value_Type_Array (1 .. Len);
   end record;

   type Value_Array_Acc is access Value_Array_Type;

   type Instance_Id is new Nat32;

   type Heap_Index is new Uns32;
   Null_Heap_Index : constant Heap_Index := 0;

   subtype File_Index is Grt.Files_Operations.Ghdl_File_Index;

   type Value_Type (Kind : Value_Kind) is record
      Typ : Type_Acc;
      case Kind is
         when Value_Net =>
            N : Net;
         when Value_Wire =>
            W : Wire_Id;
         when Value_Discrete =>
            Scal : Int64;
         when Value_Float =>
            Fp : Fp64;
         when Value_Array
           | Value_Const_Array =>
            Arr : Value_Array_Acc;
         when Value_Record
           | Value_Const_Record =>
            Rec : Value_Array_Acc;
         when Value_Access =>
            Acc : Heap_Index;
         when Value_File =>
            File : File_Index;
         when Value_Instance =>
            Instance : Instance_Id;
         when Value_Subtype =>
            null;
         when Value_Const =>
            C_Val : Value_Acc;
            C_Loc : Syn_Src;
            C_Net : Net;
         when Value_Alias =>
            A_Obj : Value_Acc;
            A_Off : Uns32;
      end case;
   end record;

   Global_Pool : aliased Areapool;
   Expr_Pool : aliased Areapool;

   --  Areapool used by Create_*_Value
   Current_Pool : Areapool_Acc := Expr_Pool'Access;

   --  Pool for objects allocated in the current instance.
   Instance_Pool : Areapool_Acc;

   --  Types.
   function Create_Discrete_Type (Rng : Discrete_Range_Type; W : Width)
                                 return Type_Acc;
   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc;
   function Create_Vec_Type_By_Length (Len : Width; El : Type_Acc)
                                      return Type_Acc;
   function Create_Vector_Type (Bnd : Bound_Type; El_Type : Type_Acc)
                               return Type_Acc;
   function Create_Unbounded_Vector (El_Type : Type_Acc) return Type_Acc;
   function Create_Slice_Type (W : Width; El_Type : Type_Acc) return Type_Acc;
   function Create_Bound_Array (Ndims : Iir_Index32) return Bound_Array_Acc;
   function Create_Array_Type (Bnd : Bound_Array_Acc; El_Type : Type_Acc)
                              return Type_Acc;
   function Create_Unbounded_Array (Ndim : Iir_Index32; El_Type : Type_Acc)
                                   return Type_Acc;
   function Create_Rec_El_Array (Nels : Iir_Index32) return Rec_El_Array_Acc;

   function Create_Record_Type (Els : Rec_El_Array_Acc; W : Width)
                               return Type_Acc;

   function Create_Access_Type (Acc_Type : Type_Acc) return Type_Acc;

   function Create_File_Type (File_Type : Type_Acc) return Type_Acc;

   --  Return the element of a vector/array/unbounded_array.
   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc;

   function Is_Bounded_Type (Typ : Type_Acc) return Boolean;

   --  True if VAL is static, ie contains neither nets nor wires.
   function Is_Static (Val : Value_Acc) return Boolean;

   --  Can also return true for nets and wires.
   function Is_Static_Val (Val : Value_Acc) return Boolean;

   function Is_Equal (L, R : Value_Acc) return Boolean;
   function Are_Types_Equal (L, R : Type_Acc) return Boolean;

   --  Create a Value_Net.
   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Value_Acc;

   --  Create a Value_Wire.  For a bit wire, RNG must be null.
   function Create_Value_Wire (W : Wire_Id; Wtype : Type_Acc) return Value_Acc;

   function Create_Value_Discrete (Val : Int64; Vtype : Type_Acc)
                                  return Value_Acc;

   function Create_Value_Float (Val : Fp64; Vtype : Type_Acc) return Value_Acc;

   function Create_Value_Access (Vtype : Type_Acc; Acc : Heap_Index)
                                return Value_Acc;

   function Create_Value_File (Vtype : Type_Acc; File : File_Index)
                              return Value_Acc;

   function Create_Value_Subtype (Typ : Type_Acc) return Value_Acc;

   function Create_Value_Array (Len : Iir_Index32) return Value_Array_Acc;

   --  Create a Value_Array.
   function Create_Value_Array (Bounds : Type_Acc; Arr : Value_Array_Acc)
                               return Value_Acc;
   function Create_Value_Const_Array (Bounds : Type_Acc; Arr : Value_Array_Acc)
                                     return Value_Acc;

   --  Like the previous one but automatically build the array.
   function Create_Value_Array (Bounds : Type_Acc) return Value_Acc;

   --  Allocate the ARR component of the Value_Type ARR, using BOUNDS.
   procedure Create_Array_Data (Arr : Value_Acc);

   function Create_Value_Record (Typ : Type_Acc; Els : Value_Array_Acc)
                                return Value_Acc;
   function Create_Value_Const_Record (Typ : Type_Acc; Els : Value_Array_Acc)
                                      return Value_Acc;

   function Create_Value_Instance (Inst : Instance_Id) return Value_Acc;

   function Create_Value_Alias (Obj : Value_Acc; Off : Uns32; Typ : Type_Acc)
                               return Value_Acc;
   function Create_Value_Const (Val : Value_Acc; Loc : Syn_Src)
                               return Value_Acc;

   --  If VAL is a const, replace it by its value.
   procedure Strip_Const (Val : in out Value_Acc);
   function Strip_Const (Val : Value_Acc) return Value_Acc;

   function Unshare (Src : Value_Acc; Pool : Areapool_Acc)
                    return Value_Acc;

   --  Get the number of indexes in array type TYP without counting
   --  sub-elements.
   function Get_Array_Flat_Length (Typ : Type_Acc) return Width;

   --  Return length of dimension DIM of type T.
   function Get_Bound_Length (T : Type_Acc; Dim : Iir_Index32) return Width;

   function Is_Matching_Bounds (L, R : Type_Acc) return Boolean;

   function Get_Type_Width (Atype : Type_Acc) return Width;

   --  Create a default initial value for TYP.
   function Create_Value_Default (Typ : Type_Acc) return Value_Acc;

   procedure Init;

   --  Set by Init.
   Boolean_Type : Type_Acc := null;
   Logic_Type : Type_Acc := null;
   Bit_Type : Type_Acc := null;
end Synth.Values;
