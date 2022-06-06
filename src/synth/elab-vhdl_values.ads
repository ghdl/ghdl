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

with Ada.Unchecked_Deallocation;

with Types; use Types;
with Areapools; use Areapools;

with Grt.Files_Operations;

with Vhdl.Nodes; use Vhdl.Nodes;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Memtype; use Elab.Memtype;

package Elab.Vhdl_Values is
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

      Value_Signal,

      --  Any kind of constant value, raw stored in memory.
      Value_Memory,

      Value_File,

      --  A constant.  This is a named value.  One purpose is to avoid to
      --  create many times the same net for the same value.
      Value_Const,

      --  An alias.  This is a reference to another value with a different
      --  (but compatible) type.
      Value_Alias,

      --  Used only for associations.
      Value_Dyn_Alias
     );

   type Value_Type (Kind : Value_Kind);

   type Value_Acc is access Value_Type;

   type Heap_Index is new Uns32;
   Null_Heap_Index : constant Heap_Index := 0;

   subtype File_Index is Grt.Files_Operations.Ghdl_File_Index;

   type Signal_Index_Type is new Uns32;
   No_Signal_Index : constant Signal_Index_Type := 0;

   type Value_Type (Kind : Value_Kind) is record
      case Kind is
         when Value_Net
           | Value_Wire =>
            N : Uns32;
         when Value_Signal =>
            S : Signal_Index_Type;
            Init : Value_Acc;
         when Value_Memory =>
            Mem : Memory_Ptr;
         when Value_File =>
            File : File_Index;
         when Value_Const =>
            C_Val : Value_Acc;
            C_Loc : Node;
            C_Net : Uns32;
         when Value_Alias =>
            A_Obj : Value_Acc;
            A_Typ : Type_Acc;  --  The type of A_Obj.
            A_Off : Value_Offsets;
         when Value_Dyn_Alias =>
            D_Obj : Value_Acc;
            D_Poff : Uns32;     --  Offset from D_Obj
            D_Ptyp : Type_Acc;  --  Type of the prefix (after offset).
            D_Voff : Uns32;     --  Variable offset
            D_Eoff : Uns32;     --  Fixed offset.
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

   --  True if VAL is static, ie contains neither nets nor wires.
   function Is_Static (Val : Value_Acc) return Boolean;

   function Is_Equal (L, R : Valtyp) return Boolean;

   function Create_Value_Memtyp (Mt : Memtyp) return Valtyp;

   --  Create a Value_Net.
   function Create_Value_Net (S : Uns32) return Value_Acc;

   --  Create a Value_Wire.
   function Create_Value_Wire (S : Uns32) return Value_Acc;

   function Create_Value_Signal (S : Signal_Index_Type; Init : Value_Acc)
                                return Value_Acc;

   function Create_Value_Memory (Vtype : Type_Acc) return Valtyp;
   function Create_Value_Memory (Mt : Memtyp) return Valtyp;

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
     (Obj : Valtyp; Off : Value_Offsets; Typ : Type_Acc) return Valtyp;

   function Create_Value_Dyn_Alias (Obj : Value_Acc;
                                    Poff : Uns32;
                                    Ptyp : Type_Acc;
                                    Voff : Uns32;
                                    Eoff : Uns32) return Value_Acc;

   function Create_Value_Const (Val : Valtyp; Loc : Node) return Valtyp;

   --  If VAL is a const, replace it by its value.
   procedure Strip_Const (Vt : in out Valtyp);

   --  If VAL is a const or an alias, replace it by its value.
   --  Used to extract the real data of a static value.  Note that the type
   --  is not correct anymore.
   function Strip_Alias_Const (V : Valtyp) return Valtyp;

   --  Return the memory of a Value_Memory value, but also handle const and
   --  aliases.
   function Get_Memory (V : Valtyp) return Memory_Ptr;

   --  Return the memtyp of V; also strip const and aliases.
   function Get_Memtyp (V : Valtyp) return Memtyp;

   function Unshare (Src : Valtyp; Pool : Areapool_Acc) return Valtyp;

   --  Create a default initial value for TYP.
   function Create_Value_Default (Typ : Type_Acc) return Valtyp;
   procedure Write_Value_Default (M : Memory_Ptr; Typ : Type_Acc);

   --  Convert a value to a string.  The value must be a const_array of scalar,
   --  which represent characters.
   function Value_To_String (Val : Valtyp) return String;

   --  Memory access.
   procedure Write_Discrete (Vt : Valtyp; Val : Int64);
   function Read_Discrete (Vt : Valtyp) return Int64;

   procedure Write_Access (Mem : Memory_Ptr; Val : Heap_Index);
   function Read_Access (Mt : Memtyp) return Heap_Index;
   function Read_Access (Vt : Valtyp) return Heap_Index;

   function Read_Fp64 (Vt : Valtyp) return Fp64;

   procedure Write_Value (Dest : Memory_Ptr; Vt : Valtyp);

   procedure Update_Index (Rng : Discrete_Range_Type; V : in out Valtyp);
end Elab.Vhdl_Values;
