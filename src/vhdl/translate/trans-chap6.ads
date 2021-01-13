--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap6 is
   --  Translate NAME.
   function Translate_Name (Name : Iir; Mode : Object_Kind_Type) return Mnode;

   --  Translate signal NAME.  Return both the signal name SIG and the value
   --  name VAL.
   procedure Translate_Signal_Name
     (Name : Iir; Sig : out Mnode; Val : out Mnode);

   --  Translate signal NAME into its node (SIG) and its direct driver
   --  node (DRV).
   procedure Translate_Direct_Driver
     (Name : Iir; Sig : out Mnode; Drv : out Mnode);

   --  Translate port NAME to its node (SIG) and its default value (INIT).
   procedure Translate_Port_Init
     (Name : Iir; Sig : out Mnode; Init : out Mnode);

   --  Direct driver of SIG (must be present).
   function Get_Signal_Direct_Driver (Sig : Iir) return Mnode;

   --  Initial value of PORT (must be present).
   function Get_Port_Init_Value (Port : Iir) return Mnode;

   --  Get record element EL of PREFIX.
   function Translate_Selected_Element
     (Prefix : Mnode; El : Iir_Element_Declaration) return Mnode;

   --  Get array element at OFFSET of PREFIX.  If unbounded, PREFIX must be
   --  stabilized.
   function Translate_Indexed_Name_By_Offset
     (Prefix : Mnode; Prefix_Type : Iir; Offset : O_Dnode) return Mnode;

   function Stabilize_If_Unbounded (Val : Mnode) return Mnode;

   function Get_Array_Bound_Length (Arr : Mnode; Arr_Type : Iir; Dim : Natural)
                                   return O_Enode;

   procedure Gen_Bound_Error (Loc : Iir);

   --  Generate code to emit a program error.
   Prg_Err_Missing_Return   : constant Natural := 1;
   Prg_Err_Block_Configured : constant Natural := 2;
   pragma Unreferenced (Prg_Err_Block_Configured);
   Prg_Err_Dummy_Config     : constant Natural := 3;
   Prg_Err_No_Choice        : constant Natural := 4;
   Prg_Err_Bad_Choice       : constant Natural := 5;
   Prg_Err_Unreach_State    : constant Natural := 6;
   procedure Gen_Program_Error (Loc : Iir; Code : Natural);

   --  Generate code to emit a failure if COND is TRUE, indicating an
   --  index violation for dimension DIM of an array.  LOC is usually
   --  the expression which has computed the index and is used only for
   --  its location.
   procedure Check_Bound_Error (Cond : O_Enode; Loc : Iir);

   --  Get the deepest range_expression of ATYPE.
   --   This follows 'range and 'reverse_range.
   --  Set IS_REVERSE to true if the range must be reversed.
   procedure Get_Deep_Range_Expression
     (Atype : Iir; Rng : out Iir; Is_Reverse : out Boolean);

   --  Get the offset of INDEX in the range RNG.
   --  This checks INDEX belongs to the range.
   --  RANGE_TYPE is the subtype of the array index (or the subtype of RNG).
   --  For unconstrained ranges, INDEX_EXPR must be NULL_IIR and RANGE_TYPE
   --   must be set.
   function Translate_Index_To_Offset (Rng        : Mnode;
                                       Index      : Mnode;
                                       Index_Expr : Iir;
                                       Range_Type : Iir;
                                       Loc        : Iir)
                                       return O_Enode;
end Trans.Chap6;
