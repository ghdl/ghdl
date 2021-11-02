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

with Types; use Types;
with Areapools; use Areapools;

with Grt.Types; use Grt.Types;

with Elab.Memtype; use Elab.Memtype;

with Vhdl.Nodes; use Vhdl.Nodes;

package Elab.Vhdl_Objtypes is
   type Discrete_Range_Type is record
      --  An integer range.
      Dir : Direction_Type;

      --  Netlist representation: signed or unsigned, width of vector.
      Is_Signed : Boolean;

      Left : Int64;
      Right : Int64;
   end record;

   --  Return the width of RNG.
   function Discrete_Range_Width (Rng : Discrete_Range_Type) return Uns32;

   function Build_Discrete_Range_Type
     (L : Int64; R : Int64; Dir : Direction_Type) return Discrete_Range_Type;

   type Float_Range_Type is record
      Dir : Direction_Type;
      Left : Fp64;
      Right : Fp64;
   end record;

   type Bound_Type is record
      Dir : Direction_Type;
      Left : Int32;
      Right : Int32;
      Len : Uns32;
   end record;

   type Bound_Array_Type is array (Dim_Type range <>) of Bound_Type;

   type Bound_Array (Ndim : Dim_Type) is record
      D : Bound_Array_Type (1 .. Ndim);
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
      Type_Unbounded_Record,
      Type_Record,

      Type_Access,
      Type_File,
      Type_Protected
     );

   subtype Type_Nets is Type_Kind range Type_Bit .. Type_Logic;
   subtype Type_All_Discrete is Type_Kind range Type_Bit .. Type_Discrete;
   subtype Type_Records is
     Type_Kind range Type_Unbounded_Record .. Type_Record;

   type Type_Type (Kind : Type_Kind);
   type Type_Acc is access Type_Type;

   type Rec_El_Type is record
      --  Bit offset: offset of the element in a net.
      Boff : Uns32;

      --  Memory offset: offset of the element in memory.
      Moff : Size_Type;

      --  Type of the element.
      Typ : Type_Acc;
   end record;

   type Rec_El_Array_Type is array (Iir_Index32 range <>) of Rec_El_Type;
   type Rec_El_Array (Len : Iir_Index32) is record
      E : Rec_El_Array_Type (1 .. Len);
   end record;

   type Rec_El_Array_Acc is access Rec_El_Array;

   --  Power of 2 alignment.
   type Palign_Type is range 0 .. 3;

   type Type_Type (Kind : Type_Kind) is record
      --  False if the type is not synthesisable: is or contains access/file.
      Is_Synth : Boolean;

      --  Alignment (in bytes) for this type.
      Al : Palign_Type;

      --  Number of bytes (when in memory) for this type.
      Sz : Size_Type;

      --  Number of bits (when in a net) for this type.
      --  Can be zero only if the type has only 0 or 1 value (like a discrete
      --  type with 1 element, a null vector, or a null array).
      --  For non synthesizable types (like files or protected type), just
      --  use 32.
      W : Uns32;

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
            Uarr_Ndim : Dim_Type;
            Uarr_El : Type_Acc;
         when Type_Record
           | Type_Unbounded_Record =>
            Rec : Rec_El_Array_Acc;
         when Type_Access =>
            Acc_Acc : Type_Acc;
         when Type_File =>
            File_Typ  : Type_Acc;
            File_Signature : String_Acc;
         when Type_Protected =>
            null;
      end case;
   end record;

   type Memtyp is record
      Typ : Type_Acc;
      Mem : Memory_Ptr;
   end record;

   Null_Memtyp : constant Memtyp := (null, null);

   --  Offsets for a value.
   type Value_Offsets is record
      Net_Off : Uns32;
      Mem_Off : Size_Type;
   end record;

   No_Value_Offsets : constant Value_Offsets := (0, 0);

   function "+" (L, R : Value_Offsets) return Value_Offsets;

   Global_Pool : aliased Areapool;
   Expr_Pool : aliased Areapool;

   --  Areapool used by Create_*_Value
   Current_Pool : Areapool_Acc := Expr_Pool'Access;

   --  Pool for objects allocated in the current instance.
   Instance_Pool : Areapool_Acc;

   --  Types.
   function Create_Discrete_Type (Rng : Discrete_Range_Type;
                                  Sz : Size_Type;
                                  W : Uns32)
                                 return Type_Acc;

   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc;
   function Create_Vec_Type_By_Length (Len : Uns32; El : Type_Acc)
                                      return Type_Acc;
   function Create_Vector_Type (Bnd : Bound_Type; El_Type : Type_Acc)
                               return Type_Acc;
   function Create_Unbounded_Vector (El_Type : Type_Acc) return Type_Acc;
   function Create_Slice_Type (Len : Uns32; El_Type : Type_Acc)
                              return Type_Acc;
   function Create_Bound_Array (Ndims : Dim_Type) return Bound_Array_Acc;
   function Create_Array_Type (Bnd : Bound_Array_Acc; El_Type : Type_Acc)
                              return Type_Acc;
   function Create_Unbounded_Array (Ndim : Dim_Type; El_Type : Type_Acc)
                                   return Type_Acc;
   function Create_Rec_El_Array (Nels : Iir_Index32) return Rec_El_Array_Acc;

   function Create_Record_Type (Els : Rec_El_Array_Acc) return Type_Acc;
   function Create_Unbounded_Record (Els : Rec_El_Array_Acc) return Type_Acc;

   function Create_Access_Type (Acc_Type : Type_Acc) return Type_Acc;

   function Create_File_Type (File_Type : Type_Acc) return Type_Acc;

   function Create_Protected_Type return Type_Acc;

   function In_Bounds (Bnd : Bound_Type; V : Int32) return Boolean;
   function In_Range (Rng : Discrete_Range_Type; V : Int64) return Boolean;

   --  Return the bounds of dimension DIM of a vector/array.  For a vector,
   --  DIM must be 1.
   function Get_Array_Bound (Typ : Type_Acc; Dim : Dim_Type)
                            return Bound_Type;

   --  Return the length of RNG.
   function Get_Range_Length (Rng : Discrete_Range_Type) return Uns32;

   --  Return the element of a vector/array/unbounded_array.
   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc;

   function Is_Bounded_Type (Typ : Type_Acc) return Boolean;

   function Are_Types_Equal (L, R : Type_Acc) return Boolean;

   --  Return the length of a vector type.
   function Vec_Length (Typ : Type_Acc) return Iir_Index32;

   --  Get the number of indexes in array type TYP without counting
   --  sub-elements.
   function Get_Array_Flat_Length (Typ : Type_Acc) return Iir_Index32;

   --  Return length of dimension DIM of type T.
   function Get_Bound_Length (T : Type_Acc; Dim : Dim_Type) return Uns32;

   function Is_Matching_Bounds (L, R : Type_Acc) return Boolean;

   function Get_Type_Width (Atype : Type_Acc) return Uns32;

   --  Low-level functions

   function Read_U8 (Mt : Memtyp) return Ghdl_U8;
   function Read_Fp64 (Mt : Memtyp) return Fp64;

   procedure Write_Discrete (Mem : Memory_Ptr; Typ : Type_Acc; Val : Int64);
   function Read_Discrete (Mt : Memtyp) return Int64;

   --  Memory allocation.

   function Create_Memory_U8 (Val : Ghdl_U8; Vtype : Type_Acc)
                             return Memtyp;
   function Create_Memory_Fp64 (Val : Fp64; Vtype : Type_Acc)
                               return Memtyp;
   function Create_Memory_Discrete (Val : Int64; Vtype : Type_Acc)
                                   return Memtyp;

   function Alloc_Memory (Vtype : Type_Acc) return Memory_Ptr;
   function Create_Memory (Vtype : Type_Acc) return Memtyp;

   --  Like Create_Memory but initialize to 0.  To be used only for types
   --  of width 0.
   function Create_Memory_Zero (Vtype : Type_Acc) return Memtyp;

   function Is_Equal (L, R : Memtyp) return Boolean;

   procedure Copy_Memory (Dest : Memory_Ptr; Src : Memory_Ptr; Sz : Size_Type);

   function Unshare (Src : Memtyp) return Memtyp;

   procedure Init;

   --  Set by Init.
   Boolean_Type : Type_Acc := null;
   Logic_Type : Type_Acc := null;
   Bit_Type : Type_Acc := null;

   --  Also set by init.
   Bit0 : Memtyp;
   Bit1 : Memtyp;
end Elab.Vhdl_Objtypes;
