--  Naive values for interpreted simulation
--  Copyright (C) 2014 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Ada.Text_IO;
with Types; use Types;
with Iirs; use Iirs;
with Grt.Types; use Grt.Types;
with Grt.Signals; use Grt.Signals;
with Grt.Files;
with Areapools; use Areapools;
-- with System.Debug_Pools;

package Iir_Values is
   -- During simulation, all values are contained into objects of type
   -- iir_value_literal.  The annotation pass creates such objects for every
   -- literal of units.  The elaboration pass creates such objects for
   -- signals, variables, contants...
   -- The simulator uses iir_value_literal for intermediate results, for
   -- computed values...

   -- There is several kinds of iir_value_literal, mainly depending on the
   -- type of the value:
   --
   -- iir_value_e32:
   --  the value is an enumeration literal.  The enum field contains the
   --  position of the literal (same as 'pos).
   --
   -- iir_value_i64:
   --  the value is an integer.
   --
   -- iir_value_f64:
   --  the value is a floating point.
   --
   -- iir_value_range:
   --  Boundaries and direction.
   --
   -- iir_value_array:
   --  All the values are contained in the array Val_Array.
   --  Boundaries of the array are contained in the array BOUNDS, one element
   --  per dimension, from 1 to number of dimensions.
   --
   -- iir_value_signal:
   --  Special case: the iir_value_literal designates a signal.
   --
   -- iir_value_record
   --  For records.
   --
   -- iir_value_access
   --  for accesses.
   --
   -- iir_value_file
   --  for files.

   --  Memory management:
   --  The values are always allocated on areapool, which uses a mark/release
   --  management. A release operation frees all the memory of the areapool
   --  allocated since the mark. This memory management is very efficient.
   --
   --  There is one areapool per processes; there is one mark per instances.
   --  Objects (variables, signals, constants, iterators, ...) are allocated
   --  on the per-process pool.  When an activation frame is created (due
   --  to a call to a subprogram), a mark is saved. When the activation frame
   --  is removed (due to a return from subprogram), the memory is released to
   --  the mark. That's simple.
   --
   --  Objects for the process is allocated in that areapool, but never
   --  released (could be if the process is waiting forever if the user don't
   --  need to inspect values).
   --
   --  Signals and constants for blocks/entity/architecture are allocated on
   --  a global pool.
   --
   --  In fact this is not so simple because of functions: they return a
   --  value.  The current solution is to compute every expressions on a
   --  expression pool (only one is needed as the computation cannot be
   --  suspended), use the result (copy in case of assignment or return), and
   --  release that pool.
   --
   --  It is highly recommended to share values as much as possible for
   --  expressions (for example, alias the values of 'others =>'). Do not
   --  share values for names, but be sure to keep the original nodes.
   --  ??? In fact sharing is required to pass actual by references.
   --  When an object is created, be sure to unshare the values.  This is
   --  usually achieved by Copy.
   --
   --  Finally, a pool is also needed during elaboration (as elaboration is
   --  not done within the context of a process).

   type Iir_Value_Kind is
     (Iir_Value_B1, Iir_Value_E32,
      Iir_Value_I64, Iir_Value_F64,
      Iir_Value_Access,
      Iir_Value_File,
      Iir_Value_Range,
      Iir_Value_Array, Iir_Value_Record,
      Iir_Value_Protected,
      Iir_Value_Signal,
      Iir_Value_Terminal,
      Iir_Value_Quantity);

   type Protected_Index_Type is new Natural;

   type Quantity_Index_Type is new Natural;
   type Terminal_Index_Type is new Natural;

   --  Scalar values.  Only these ones can be signals.
   subtype Iir_Value_Scalars is
     Iir_Value_Kind range Iir_Value_B1 .. Iir_Value_F64;

   type Iir_Value_Literal (Kind: Iir_Value_Kind);

   type Iir_Value_Literal_Acc is access Iir_Value_Literal;

   -- Must start at 0.
   -- Thus, length of the array is val_array'last - 1.
   type Iir_Value_Literal_Array is array (Iir_Index32 range <>) of
     Iir_Value_Literal_Acc;

   type Iir_Value_Literal_Array_Acc is access Iir_Value_Literal_Array;

   type Value_Bounds_Array (Nbr_Dims : Iir_Index32) is record
      D : Iir_Value_Literal_Array (1 .. Nbr_Dims);
   end record;

   type Value_Bounds_Array_Acc is access Value_Bounds_Array;

   type Value_Array (Len : Iir_Index32) is record
      V : Iir_Value_Literal_Array (1 .. Len);
   end record;

   type Value_Array_Acc is access Value_Array;

   type Iir_Value_Literal (Kind: Iir_Value_Kind) is record
      case Kind is
         when Iir_Value_B1 =>
            B1 : Ghdl_B1;
         when Iir_Value_E32 =>
            E32 : Ghdl_E32;
         when Iir_Value_I64 =>
            I64 : Ghdl_I64;
         when Iir_Value_F64 =>
            F64 : Ghdl_F64;
         when Iir_Value_Access =>
            Val_Access: Iir_Value_Literal_Acc;
         when Iir_Value_File =>
            File: Grt.Files.Ghdl_File_Index;
         when Iir_Value_Array =>
            Val_Array: Value_Array_Acc; --  range 1 .. N
            Bounds : Value_Bounds_Array_Acc;   --  range 1 .. Dim
         when Iir_Value_Record =>
            Val_Record: Value_Array_Acc; -- range 1 .. N
         when Iir_Value_Signal =>
            Sig : Ghdl_Signal_Ptr;
         when Iir_Value_Protected =>
            Prot : Protected_Index_Type;
         when Iir_Value_Quantity =>
            Quantity : Quantity_Index_Type;
         when Iir_Value_Terminal =>
            Terminal : Terminal_Index_Type;
         when Iir_Value_Range =>
            Dir: Iir_Direction;
            Length : Iir_Index32;
            Left: Iir_Value_Literal_Acc;
            Right: Iir_Value_Literal_Acc;
      end case;
   end record;

   -- What is chosen for time.
   -- Currently only int32 is available, but time should use an int64.
   subtype Iir_Value_Time is Ghdl_I64;

   Global_Pool : aliased Areapool;
   Expr_Pool : aliased Areapool;

   --  Areapool used by Create_*_Value
   Current_Pool : Areapool_Acc := Expr_Pool'Access;

   --  Pool for objects allocated in the current instance.
   Instance_Pool : Areapool_Acc;

   function Create_Signal_Value (Sig : Ghdl_Signal_Ptr)
     return Iir_Value_Literal_Acc;

   function Create_Terminal_Value (Terminal : Terminal_Index_Type)
                                  return Iir_Value_Literal_Acc;

   function Create_Quantity_Value (Quantity : Quantity_Index_Type)
                                  return Iir_Value_Literal_Acc;

   function Create_B1_Value (Val : Ghdl_B1) return Iir_Value_Literal_Acc;

   function Create_E32_Value (Val : Ghdl_E32) return Iir_Value_Literal_Acc;

   -- Return an iir_value_literal_acc (iir_value_int64).
   function Create_I64_Value (Val : Ghdl_I64) return Iir_Value_Literal_Acc;

   --  Return an iir_value_literal_acc (iir_value_fp64)
   function Create_F64_Value (Val : Ghdl_F64) return Iir_Value_Literal_Acc;

   function Create_Access_Value (Val : Iir_Value_Literal_Acc)
                                return Iir_Value_Literal_Acc;

   function Create_File_Value (Val : Grt.Files.Ghdl_File_Index)
                              return Iir_Value_Literal_Acc;

   function Create_Protected_Value (Prot : Protected_Index_Type)
                                   return Iir_Value_Literal_Acc;

   -- Return an iir_value_literal (iir_value_record) of NBR elements.
   function Create_Record_Value
     (Nbr : Iir_Index32; Pool : Areapool_Acc := Current_Pool)
     return Iir_Value_Literal_Acc;

   --  Allocate array and the dimension vector (but bounds and values aren't
   --  allocated).
   function Create_Array_Value (Dim : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool)
                               return Iir_Value_Literal_Acc;

   --  Allocate the Val_Array vector.
   procedure Create_Array_Data (Arr : Iir_Value_Literal_Acc;
                                Len : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool);

   -- Return an array of length LENGTH and DIM bounds.
   -- If DIM is 0, then the bounds array is not allocated.
   function Create_Array_Value (Length: Iir_Index32;
                                Dim : Iir_Index32;
                                Pool : Areapool_Acc := Current_Pool)
                               return Iir_Value_Literal_Acc;

   --  Create a range_value of life LIFE.
   function Create_Range_Value
     (Left, Right : Iir_Value_Literal_Acc;
      Dir : Iir_Direction;
      Length : Iir_Index32)
     return Iir_Value_Literal_Acc;

   --  Create a range_value (compute the length)
   function Create_Range_Value
     (Left, Right : Iir_Value_Literal_Acc;
      Dir : Iir_Direction)
      return Iir_Value_Literal_Acc;

   -- Return true if the value of LEFT and RIGHT are equal.
   -- Return false if they are not equal.
   -- Raise constraint_error if the types differes.
   -- Value or sub-value must not be indirect.
   function Is_Equal (Left, Right: Iir_Value_Literal_Acc) return Boolean;

   --  Return TRUE iif ARANGE is a nul range.
   function Is_Nul_Range (Arange : Iir_Value_Literal_Acc) return Boolean;

   -- Get order of LEFT with RIGHT.
   -- Must be discrete kind (enum, int, fp, physical) or array (uni dim).
   type Order is (Less, Equal, Greater);
   function Compare_Value (Left, Right : Iir_Value_Literal_Acc)
                           return Order;

   --  Check that SRC has the same structure as DEST.  Report an error at
   --  LOC if not.
   procedure Check_Bounds (Dest : Iir_Value_Literal_Acc;
                           Src : Iir_Value_Literal_Acc;
                           Loc : Iir);

   -- Store (by copy) SRC into DEST.
   -- The type must be equal (otherwise  constraint_error is raised).
   -- Life of DEST must be Target, otherwise program_error is raised.
   -- Value or sub-value must not be indirect.
   procedure Store (Dest : Iir_Value_Literal_Acc; Src : Iir_Value_Literal_Acc);

   --  Create a copy of SRC allocated in POOL.
   function Unshare (Src : Iir_Value_Literal_Acc; Pool : Areapool_Acc)
                    return Iir_Value_Literal_Acc;

   --  If SRC is an array, just copy the bounds in POOL and return it.
   --  Otherwise return SRC.  Values are always kept, so that this could
   --  be used by alias declarations.
   function Unshare_Bounds (Src : Iir_Value_Literal_Acc; Pool : Areapool_Acc)
                           return Iir_Value_Literal_Acc;

   --  Create a copy of SRC on the heap.
   function Unshare_Heap (Src : Iir_Value_Literal_Acc)
                         return Iir_Value_Literal_Acc;

   --  Deallocate value accessed by ACC.
   procedure Free_Heap_Value (Acc : Iir_Value_Literal_Acc);

   --  Increment.
   --  VAL must be of kind integer or enumeration.
   --  VAL must be of life temporary.
   procedure Increment (Val : Iir_Value_Literal_Acc);

   --  Copy BOUNDS of SRC with a specified life.
   --  Note: val_array is allocated but not filled.
   function Copy_Array_Bound (Src : Iir_Value_Literal_Acc)
                             return Iir_Value_Literal_Acc;

   --  Copy the bounds (well the array containing the values) of SRC.
   --  Val_record is allocated but not filled.
   function Copy_Record (Src : Iir_Value_Literal_Acc)
                        return Iir_Value_Literal_Acc;

   --  Return the number of scalars elements in VALS.
   function Get_Nbr_Of_Scalars (Val : Iir_Value_Literal_Acc) return Natural;

   --  Return the position of an enumerated type value.
   function Get_Enum_Pos (Val : Iir_Value_Literal_Acc) return Natural;

   -- Well known values.
   -- Boolean_to_lit can be used to convert a boolean value from Ada to a
   -- boolean value for vhdl.
   type Lit_Enum_Type is array (Boolean) of Iir_Value_Literal_Acc;
   Lit_Enum_0 : constant Iir_Value_Literal_Acc :=
     new Iir_Value_Literal'(Kind => Iir_Value_B1,
                            B1 => False);
   Lit_Enum_1 : constant Iir_Value_Literal_Acc :=
     new Iir_Value_Literal'(Kind => Iir_Value_B1,
                            B1 => True);
   Boolean_To_Lit: constant Lit_Enum_Type :=
     (False => Lit_Enum_0, True => Lit_Enum_1);
   Lit_Boolean_False: Iir_Value_Literal_Acc
     renames Boolean_To_Lit (False);
   Lit_Boolean_True: Iir_Value_Literal_Acc
     renames Boolean_To_Lit (True);

   -- Literal NULL.
   Null_Lit: constant Iir_Value_Literal_Acc :=
     new Iir_Value_Literal'(Kind => Iir_Value_Access,
                            Val_Access => null);

   -- Disp a value_literal in raw form.
   procedure Disp_Value (Value: Iir_Value_Literal_Acc);
   procedure Disp_Value_Tab (Value: Iir_Value_Literal_Acc;
                             Tab: Ada.Text_IO.Count);

   -- Disp a value_literal in readable form.
   procedure Disp_Iir_Value (Value: Iir_Value_Literal_Acc; A_Type: Iir);
end Iir_Values;

