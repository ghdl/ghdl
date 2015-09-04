--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

package Trans.Chap6 is
   --  Translate NAME.
   --  RES contains a lnode for the result. This is the object.
   --    RES can be a tree, so it may be referenced only once.
   --  SIG is true if RES is a signal object.
   function Translate_Name (Name : Iir) return Mnode;

   --  Translate signal NAME into its node (SIG) and its direct driver
   --  node (DRV).
   procedure Translate_Direct_Driver
     (Name : Iir; Sig : out Mnode; Drv : out Mnode);

   --  Same as Translate_Name, but only for formal names.
   --  If SCOPE_TYPE and SCOPE_PARAM are not null, use them for the scope
   --  of the base name.
   --  Indeed, for recursive instantiation, NAME can designates the actual
   --  and the formal.
   --       function Translate_Formal_Name (Scope_Type : O_Tnode;
   --                                       Scope_Param : O_Lnode;
   --                                       Name : Iir)
   --                                      return Mnode;

   --  Get record element EL of PREFIX.
   function Translate_Selected_Element (Prefix : Mnode;
                                        El     : Iir_Element_Declaration)
                                           return Mnode;

   function Get_Array_Bound_Length (Arr      : Mnode;
                                    Arr_Type : Iir;
                                    Dim      : Natural)
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
   procedure Check_Bound_Error (Cond : O_Enode; Loc : Iir; Dim : Natural);

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
                                       Index      : O_Enode;
                                       Index_Expr : Iir;
                                       Range_Type : Iir;
                                       Loc        : Iir)
                                          return O_Enode;
end Trans.Chap6;
