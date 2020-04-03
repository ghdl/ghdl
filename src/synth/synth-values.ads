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

with Ada.Unchecked_Deallocation;

with Types; use Types;
with Areapools; use Areapools;

with Grt.Types; use Grt.Types;
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
      Type_Record,

      Type_Access,
      Type_File
     );

   subtype Type_Nets is Type_Kind range Type_Bit .. Type_Logic;

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
            Uarr_Ndim : Dim_Type;
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

      --  Any kind of constant value, raw stored in memory.
      Value_Memory,

      Value_File,

      --  A constant.  This is a named value.  One purpose is to avoid to
      --  create many times the same net for the same value.
      Value_Const,

      --  An alias.  This is a reference to another value with a different
      --  (but compatible) type.
      Value_Alias
     );

   type Value_Type (Kind : Value_Kind);

   type Value_Acc is access Value_Type;

   type Value_Type_Array is array (Iir_Index32 range <>) of Value_Acc;

   type Value_Array_Type (Ln : Iir_Index32) is record
      --  Values are from left to right.  So V(1) is at index 'Left.
      V : Value_Type_Array (1 .. Ln);
   end record;

   type Value_Array_Acc is access Value_Array_Type;

   type Heap_Index is new Uns32;
   Null_Heap_Index : constant Heap_Index := 0;

   subtype File_Index is Grt.Files_Operations.Ghdl_File_Index;

   type Memory_Element is mod 2**8;
   type Memory_Array is array (Size_Type range <>) of Memory_Element;

   --  Flat pointer for a generic pointer.
   type Memory_Ptr is access all Memory_Array (Size_Type);

   type Memtyp is record
      Typ : Type_Acc;
      Mem : Memory_Ptr;
   end record;

   --  Offsets for a value.
   type Value_Offsets is record
      Net_Off : Uns32;
      Mem_Off : Size_Type;
   end record;

   function "+" (L, R : Value_Offsets) return Value_Offsets;

   type Value_Type (Kind : Value_Kind) is record
      case Kind is
         when Value_Net =>
            N : Net;
         when Value_Wire =>
            W : Wire_Id;
         when Value_Memory =>
            Mem : Memory_Ptr;
         when Value_File =>
            File : File_Index;
         when Value_Const =>
            C_Val : Value_Acc;
            C_Loc : Syn_Src;
            C_Net : Net;
         when Value_Alias =>
            A_Obj : Value_Acc;
            A_Off : Value_Offsets;
      end case;
   end record;

   --  A tuple of type and value.
   type Valtyp is record
      Typ : Type_Acc;
      Val : Value_Acc;
   end record;

   No_Valtyp : constant Valtyp := (null, null);

   type Valtyp_Array is array (Nat32 range <>) of Valtyp;
   type Valtyp_Array_Acc is access Valtyp_Array;

   procedure Free_Valtyp_Array is new Ada.Unchecked_Deallocation
     (Valtyp_Array, Valtyp_Array_Acc);

   Global_Pool : aliased Areapool;
   Expr_Pool : aliased Areapool;

   --  Areapool used by Create_*_Value
   Current_Pool : Areapool_Acc := Expr_Pool'Access;

   --  Pool for objects allocated in the current instance.
   Instance_Pool : Areapool_Acc;

   --  Types.
   function Create_Discrete_Type (Rng : Discrete_Range_Type;
                                  Sz : Size_Type;
                                  W : Width)
                                 return Type_Acc;

   function Create_Float_Type (Rng : Float_Range_Type) return Type_Acc;
   function Create_Vec_Type_By_Length (Len : Width; El : Type_Acc)
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

   function Create_Access_Type (Acc_Type : Type_Acc) return Type_Acc;

   function Create_File_Type (File_Type : Type_Acc) return Type_Acc;

   --  Return the bounds of dimension DIM of a vector/array.  For a vector,
   --  DIM must be 1.
   function Get_Array_Bound (Typ : Type_Acc; Dim : Dim_Type)
                            return Bound_Type;

   --  Return the length of RNG.
   function Get_Range_Length (Rng : Discrete_Range_Type) return Uns32;

   --  Return the element of a vector/array/unbounded_array.
   function Get_Array_Element (Arr_Type : Type_Acc) return Type_Acc;

   function Is_Bounded_Type (Typ : Type_Acc) return Boolean;

   --  True if VAL is static, ie contains neither nets nor wires.
   function Is_Static (Val : Value_Acc) return Boolean;

   --  Can also return true for nets and wires.
   function Is_Static_Val (Val : Value_Acc) return Boolean;

   function Is_Equal (L, R : Valtyp) return Boolean;
   function Are_Types_Equal (L, R : Type_Acc) return Boolean;

   --  Create a Value_Net.
   function Create_Value_Net (N : Net; Ntype : Type_Acc) return Valtyp;

   --  Create a Value_Wire.  For a bit wire, RNG must be null.
   function Create_Value_Wire (W : Wire_Id; Wtype : Type_Acc) return Valtyp;

   function Create_Value_Memory (Vtype : Type_Acc) return Valtyp;

   function Create_Value_Uns (Val : Uns64; Vtype : Type_Acc) return Valtyp;
   function Create_Value_Int (Val : Int64; Vtype : Type_Acc) return Valtyp;
   function Create_Value_Discrete (Val : Int64; Vtype : Type_Acc)
                                  return Valtyp;

   function Create_Value_Access (Val : Heap_Index; Acc_Typ : Type_Acc)
                                return Valtyp;

   function Create_Value_Float (Val : Fp64; Vtype : Type_Acc) return Valtyp;

   function Create_Value_File (Vtype : Type_Acc; File : File_Index)
                              return Valtyp;

   function Create_Value_Alias
     (Obj : Value_Acc; Off : Value_Offsets; Typ : Type_Acc) return Valtyp;

   function Create_Value_Const (Val : Valtyp; Loc : Syn_Src)
                               return Valtyp;

   --  If VAL is a const, replace it by its value.
   procedure Strip_Const (Vt : in out Valtyp);

   --  If VAL is a const or an alias, replace it by its value.
   --  Used to extract the real data of a static value.  Note that the type
   --  is not correct anymore.
   function Strip_Alias_Const (V : Valtyp) return Valtyp;

   --  Return the memtyp of V; also strip const and aliases.
   function Get_Memtyp (V : Valtyp) return Memtyp;

   function Unshare (Src : Valtyp; Pool : Areapool_Acc) return Valtyp;

   --  Return the length of a vector type.
   function Vec_Length (Typ : Type_Acc) return Iir_Index32;

   --  Get the number of indexes in array type TYP without counting
   --  sub-elements.
   function Get_Array_Flat_Length (Typ : Type_Acc) return Iir_Index32;

   --  Return length of dimension DIM of type T.
   function Get_Bound_Length (T : Type_Acc; Dim : Dim_Type) return Width;

   function Is_Matching_Bounds (L, R : Type_Acc) return Boolean;

   function Get_Type_Width (Atype : Type_Acc) return Width;

   --  Create a default initial value for TYP.
   function Create_Value_Default (Typ : Type_Acc) return Valtyp;
   procedure Write_Value_Default (M : Memory_Ptr; Typ : Type_Acc);

   --  Convert a value to a string.  The value must be a const_array of scalar,
   --  which represent characters.
   function Value_To_String (Val : Valtyp) return String;

   --  Memory access.
   procedure Write_Discrete (Vt : Valtyp; Val : Int64);
   function Read_Discrete (Mem : Memory_Ptr; Typ : Type_Acc) return Int64;
   function Read_Discrete (Vt : Valtyp) return Int64;

   procedure Write_Access (Mem : Memory_Ptr; Val : Heap_Index);
   function Read_Access (Vt : Valtyp) return Heap_Index;

   function Read_Fp64 (Mem : Memory_Ptr) return Fp64;
   function Read_Fp64 (Vt : Valtyp) return Fp64;

   --  Low level subprograms.
   function Read_U8 (Mem : Memory_Ptr) return Ghdl_U8;
   procedure Write_U8 (Mem : Memory_Ptr; Val : Ghdl_U8);

   function "+" (Base : Memory_Ptr; Off : Size_Type) return Memory_Ptr;

   procedure Copy_Memory (Dest : Memory_Ptr; Src : Memory_Ptr; Sz : Size_Type);
   procedure Write_Value (Dest : Memory_Ptr; Vt : Valtyp);

   procedure Init;

   --  Set by Init.
   Boolean_Type : Type_Acc := null;
   Logic_Type : Type_Acc := null;
   Bit_Type : Type_Acc := null;
end Synth.Values;
