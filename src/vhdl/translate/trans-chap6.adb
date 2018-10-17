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

with Files_Map;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Evaluation; use Evaluation;
with Trans.Chap3;
with Trans.Chap7;
with Trans.Chap14;
with Trans.Helpers2; use Trans.Helpers2;
with Trans_Decls; use Trans_Decls;

package body Trans.Chap6 is
   use Trans.Helpers;

   function Get_Array_Bound_Length
     (Arr : Mnode; Arr_Type : Iir; Dim : Natural) return O_Enode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Arr_Type);
      Index_Type, Constraint : Iir;
   begin
      if Tinfo.Type_Locally_Constrained then
         Index_Type := Get_Index_Type (Arr_Type, Dim - 1);
         Constraint := Get_Range_Constraint (Index_Type);
         return New_Lit (Chap7.Translate_Static_Range_Length (Constraint));
      else
         return M2E
           (Chap3.Range_To_Length
              (Chap3.Get_Array_Range (Arr, Arr_Type, Dim)));
      end if;
   end Get_Array_Bound_Length;

   procedure Gen_Bound_Error (Loc : Iir)
   is
      Constr    : O_Assoc_List;
      Name      : Name_Id;
      Line, Col : Natural;
   begin
      Files_Map.Location_To_Position (Get_Location (Loc), Name, Line, Col);

      Start_Association (Constr, Ghdl_Bound_Check_Failed);
      Assoc_Filename_Line (Constr, Line);
      New_Procedure_Call (Constr);
   end Gen_Bound_Error;

   procedure Gen_Direction_Error (Loc : Iir)
   is
      Constr    : O_Assoc_List;
      Name      : Name_Id;
      Line, Col : Natural;
   begin
      Files_Map.Location_To_Position (Get_Location (Loc), Name, Line, Col);

      Start_Association (Constr, Ghdl_Direction_Check_Failed);
      Assoc_Filename_Line (Constr, Line);
      New_Procedure_Call (Constr);
   end Gen_Direction_Error;

   procedure Gen_Program_Error (Loc : Iir; Code : Natural)
   is
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Ghdl_Program_Error);

      if Current_Filename_Node = O_Dnode_Null then
         New_Association (Assoc, New_Lit (New_Null_Access (Char_Ptr_Type)));
         New_Association (Assoc,
                          New_Lit (New_Signed_Literal (Ghdl_I32_Type, 0)));
      else
         Assoc_Filename_Line (Assoc, Get_Line_Number (Loc));
      end if;
      New_Association
        (Assoc, New_Lit (New_Unsigned_Literal (Ghdl_Index_Type,
         Unsigned_64 (Code))));
      New_Procedure_Call (Assoc);
   end Gen_Program_Error;

   --  Generate code to emit a failure if COND is TRUE, indicating an
   --  index violation for dimension DIM of an array.  LOC is usually
   --  the expression which has computed the index and is used only for
   --  its location.
   procedure Check_Bound_Error (Cond : O_Enode; Loc : Iir; Dim : Natural)
   is
      pragma Unreferenced (Dim);
      If_Blk : O_If_Block;
   begin
      Start_If_Stmt (If_Blk, Cond);
      Gen_Bound_Error (Loc);
      Finish_If_Stmt (If_Blk);
   end Check_Bound_Error;

   procedure Check_Direction_Error (Cond : O_Enode; Loc : Iir)
   is
      If_Blk : O_If_Block;
   begin
      Start_If_Stmt (If_Blk, Cond);
      Gen_Direction_Error (Loc);
      Finish_If_Stmt (If_Blk);
   end Check_Direction_Error;

   --  Return TRUE if an array whose index type is RNG_TYPE indexed by
   --  an expression of type EXPR_TYPE needs a bound check.
   function Need_Index_Check (Expr_Type : Iir; Rng_Type : Iir)
                                 return Boolean
   is
      Rng : Iir;
   begin
      --  Do checks if type of the expression is not a subtype.
      --  FIXME: EXPR_TYPE shound not be NULL_IIR (generate stmt)
      if Expr_Type = Null_Iir then
         return True;
      end if;
      case Get_Kind (Expr_Type) is
         when Iir_Kind_Integer_Subtype_Definition
            | Iir_Kind_Enumeration_Subtype_Definition
            | Iir_Kind_Enumeration_Type_Definition =>
            null;
         when others =>
            return True;
      end case;

      --  No check if the expression has the type of the index.
      if Expr_Type = Rng_Type then
         return False;
      end if;

      --  No check for 'Range or 'Reverse_Range.
      Rng := Get_Range_Constraint (Expr_Type);
      if (Get_Kind (Rng) = Iir_Kind_Range_Array_Attribute
          or Get_Kind (Rng) = Iir_Kind_Reverse_Range_Array_Attribute)
        and then Get_Type (Rng) = Rng_Type
      then
         return False;
      end if;

      return True;
   end Need_Index_Check;

   procedure Get_Deep_Range_Expression
     (Atype : Iir; Rng : out Iir; Is_Reverse : out Boolean)
   is
      T : Iir;
      R : Iir;
   begin
      Is_Reverse := False;

      --  T is an integer/enumeration subtype.
      T := Atype;
      loop
         case Get_Kind (T) is
            when Iir_Kind_Integer_Subtype_Definition
               | Iir_Kind_Enumeration_Subtype_Definition
               | Iir_Kind_Enumeration_Type_Definition =>
               --  These types have a range.
               null;
            when others =>
               Error_Kind ("get_deep_range_expression(1)", T);
         end case;

         R := Get_Range_Constraint (T);
         case Get_Kind (R) is
            when Iir_Kind_Range_Expression =>
               Rng := R;
               return;
            when Iir_Kind_Range_Array_Attribute =>
               null;
            when Iir_Kind_Reverse_Range_Array_Attribute =>
               Is_Reverse := not Is_Reverse;
            when others =>
               Error_Kind ("get_deep_range_expression(2)", R);
         end case;
         T := Get_Index_Subtype (R);
         if T = Null_Iir then
            Rng := Null_Iir;
            return;
         end if;
      end loop;
   end Get_Deep_Range_Expression;

   function Translate_Index_To_Offset (Rng        : Mnode;
                                       Index      : O_Enode;
                                       Index_Expr : Iir;
                                       Range_Type : Iir;
                                       Loc        : Iir)
                                          return O_Enode
   is
      Need_Check   : Boolean;
      Dir          : O_Enode;
      If_Blk       : O_If_Block;
      Res          : O_Dnode;
      Off          : O_Dnode;
      Bound        : O_Enode;
      Cond1, Cond2 : O_Enode;
      Index_Node   : O_Dnode;
      Bound_Node   : O_Dnode;
      Index_Info   : Type_Info_Acc;
      Deep_Rng     : Iir;
      Deep_Reverse : Boolean;
   begin
      Index_Info := Get_Info (Get_Base_Type (Range_Type));
      if Index_Expr = Null_Iir then
         Need_Check := True;
         Deep_Rng := Null_Iir;
         Deep_Reverse := False;
      else
         Need_Check := Need_Index_Check (Get_Type (Index_Expr), Range_Type);
         Get_Deep_Range_Expression (Range_Type, Deep_Rng, Deep_Reverse);
      end if;

      Res := Create_Temp (Ghdl_Index_Type);

      Open_Temp;

      Off := Create_Temp (Index_Info.Ortho_Type (Mode_Value));

      Bound := M2E (Chap3.Range_To_Left (Rng));

      if Deep_Rng /= Null_Iir then
         if Get_Direction (Deep_Rng) = Iir_To xor Deep_Reverse then
            --  Direction TO:  INDEX - LEFT.
            New_Assign_Stmt (New_Obj (Off),
                             New_Dyadic_Op (ON_Sub_Ov,
                               Index, Bound));
         else
            --  Direction DOWNTO: LEFT - INDEX.
            New_Assign_Stmt (New_Obj (Off),
                             New_Dyadic_Op (ON_Sub_Ov,
                               Bound, Index));
         end if;
      else
         Index_Node := Create_Temp_Init
           (Index_Info.Ortho_Type (Mode_Value), Index);
         Bound_Node := Create_Temp_Init
           (Index_Info.Ortho_Type (Mode_Value), Bound);
         Dir := M2E (Chap3.Range_To_Dir (Rng));

         --  Non-static direction.
         Start_If_Stmt (If_Blk,
                        New_Compare_Op (ON_Eq, Dir,
                          New_Lit (Ghdl_Dir_To_Node),
                          Ghdl_Bool_Type));
         --  Direction TO:  INDEX - LEFT.
         New_Assign_Stmt (New_Obj (Off),
                          New_Dyadic_Op (ON_Sub_Ov,
                            New_Obj_Value (Index_Node),
                            New_Obj_Value (Bound_Node)));
         New_Else_Stmt (If_Blk);
         --  Direction DOWNTO: LEFT - INDEX.
         New_Assign_Stmt (New_Obj (Off),
                          New_Dyadic_Op (ON_Sub_Ov,
                            New_Obj_Value (Bound_Node),
                            New_Obj_Value (Index_Node)));
         Finish_If_Stmt (If_Blk);
      end if;

      --  Get the offset.
      New_Assign_Stmt
        (New_Obj (Res), New_Convert_Ov (New_Obj_Value (Off),
         Ghdl_Index_Type));

      --  Check bounds.
      if Need_Check then
         Cond1 := New_Compare_Op
           (ON_Lt,
            New_Obj_Value (Off),
            New_Lit (New_Signed_Literal (Index_Info.Ortho_Type (Mode_Value),
              0)),
            Ghdl_Bool_Type);

         Cond2 := New_Compare_Op
           (ON_Ge,
            New_Obj_Value (Res),
            M2E (Chap3.Range_To_Length (Rng)),
            Ghdl_Bool_Type);
         Check_Bound_Error (New_Dyadic_Op (ON_Or, Cond1, Cond2), Loc, 0);
      end if;

      Close_Temp;

      return New_Obj_Value (Res);
   end Translate_Index_To_Offset;

   --  Translate index EXPR in dimension DIM of thin array into an
   --  offset.
   --  This checks bounds.
   function Translate_Thin_Index_Offset (Index_Type : Iir;
                                         Dim        : Natural;
                                         Expr       : Iir)
                                            return O_Enode
   is
      Index_Range     : constant Iir := Get_Range_Constraint (Index_Type);
      Obound          : O_Cnode;
      Res             : O_Dnode;
      Cond2           : O_Enode;
      Index           : O_Enode;
      Index_Base_Type : Iir;
      V               : Iir_Int64;
      B               : Iir_Int64;
   begin
      B := Eval_Pos (Get_Left_Limit (Index_Range));
      if Get_Expr_Staticness (Expr) = Locally then
         V := Eval_Pos (Eval_Static_Expr (Expr));
         if Get_Direction (Index_Range) = Iir_To then
            B := V - B;
         else
            B := B - V;
         end if;
         return New_Lit
           (New_Unsigned_Literal (Ghdl_Index_Type, Unsigned_64 (B)));
      else
         Index_Base_Type := Get_Base_Type (Index_Type);
         Index := Chap7.Translate_Expression (Expr, Index_Base_Type);

         if Get_Direction (Index_Range) = Iir_To then
            --  Direction TO:  INDEX - LEFT.
            if B /= 0 then
               Obound := Chap7.Translate_Static_Range_Left
                 (Index_Range, Index_Base_Type);
               Index := New_Dyadic_Op (ON_Sub_Ov, Index, New_Lit (Obound));
            end if;
         else
            --  Direction DOWNTO:  LEFT - INDEX.
            Obound := Chap7.Translate_Static_Range_Left
              (Index_Range, Index_Base_Type);
            Index := New_Dyadic_Op (ON_Sub_Ov, New_Lit (Obound), Index);
         end if;

         --  Get the offset.
         Index := New_Convert_Ov (Index, Ghdl_Index_Type);

         --  Since the value is unsigned, both left and right bounds are
         --  checked in the same time.
         if Get_Type (Expr) /= Index_Type then
            Res := Create_Temp_Init (Ghdl_Index_Type, Index);

            Cond2 := New_Compare_Op
              (ON_Ge, New_Obj_Value (Res),
               New_Lit (Chap7.Translate_Static_Range_Length (Index_Range)),
               Ghdl_Bool_Type);
            Check_Bound_Error (Cond2, Expr, Dim);
            Index := New_Obj_Value (Res);
         end if;

         return Index;
      end if;
   end Translate_Thin_Index_Offset;

   --  Translate an indexed name.
   type Indexed_Name_Data is record
      Offset : O_Dnode;
      Res    : Mnode;
   end record;

   function Translate_Indexed_Name_Init (Prefix_Orig : Mnode; Expr : Iir)
                                            return Indexed_Name_Data
   is
      Prefix_Type : constant Iir := Get_Type (Get_Prefix (Expr));
      Prefix_Info : constant Type_Info_Acc := Get_Info (Prefix_Type);
      Index_List  : constant Iir_Flist := Get_Index_List (Expr);
      Type_List   : constant Iir_Flist := Get_Index_Subtype_List (Prefix_Type);
      Nbr_Dim     : constant Natural := Get_Nbr_Elements (Index_List);
      Prefix      : Mnode;
      Index       : Iir;
      Offset      : O_Dnode;
      R           : O_Enode;
      Length      : O_Enode;
      Itype       : Iir;
      Ibasetype   : Iir;
      Range_Ptr   : Mnode;
   begin
      case Type_Mode_Arrays (Prefix_Info.Type_Mode) is
         when Type_Mode_Unbounded_Array =>
            Prefix := Stabilize (Prefix_Orig);
         when Type_Mode_Bounded_Arrays =>
            Prefix := Prefix_Orig;
      end case;
      Offset := Create_Temp (Ghdl_Index_Type);
      for Dim in 1 .. Nbr_Dim loop
         Index := Get_Nth_Element (Index_List, Dim - 1);
         Itype := Get_Index_Type (Type_List, Dim - 1);
         Ibasetype := Get_Base_Type (Itype);
         Open_Temp;
         --  Compute index for the current dimension.
         case Prefix_Info.Type_Mode is
            when Type_Mode_Unbounded_Array =>
               Range_Ptr := Stabilize
                 (Chap3.Get_Array_Range (Prefix, Prefix_Type, Dim));
               R := Translate_Index_To_Offset
                 (Range_Ptr,
                  Chap7.Translate_Expression (Index, Ibasetype),
                  Null_Iir, Itype, Index);
            when Type_Mode_Bounded_Arrays =>
               if Prefix_Info.Type_Locally_Constrained then
                  R := Translate_Thin_Index_Offset (Itype, Dim, Index);
               else
                  --  Manually extract range since there is no infos for
                  --   index subtype.
                  Range_Ptr := Chap3.Bounds_To_Range
                    (Chap3.Get_Composite_Type_Bounds (Prefix_Type),
                     Prefix_Type, Dim);
                  Stabilize (Range_Ptr);
                  R := Translate_Index_To_Offset
                    (Range_Ptr,
                     Chap7.Translate_Expression (Index, Ibasetype),
                     Index, Itype, Index);
               end if;
            when others =>
               raise Internal_Error;
         end case;
         if Dim = 1 then
            --  First dimension.
            New_Assign_Stmt (New_Obj (Offset), R);
         else
            --  If there are more dimension(s) to follow, then multiply
            --  the current offset by the length of the current dimension.
            if Prefix_Info.Type_Locally_Constrained then
               Length := New_Lit (Chap7.Translate_Static_Range_Length
                                  (Get_Range_Constraint (Itype)));
            else
               Length := M2E (Chap3.Range_To_Length (Range_Ptr));
            end if;
            New_Assign_Stmt
              (New_Obj (Offset),
               New_Dyadic_Op (ON_Add_Ov,
                 New_Dyadic_Op (ON_Mul_Ov,
                   New_Obj_Value (Offset),
                   Length),
                 R));
         end if;
         Close_Temp;
      end loop;

      return (Offset => Offset,
              Res => Chap3.Index_Base
                (Chap3.Get_Composite_Base (Prefix), Prefix_Type,
                 New_Obj_Value (Offset)));
   end Translate_Indexed_Name_Init;

   function Translate_Indexed_Name_Finish
     (Prefix : Mnode; Expr : Iir; Data : Indexed_Name_Data)
         return Mnode
   is
   begin
      return Chap3.Index_Base (Chap3.Get_Composite_Base (Prefix),
                               Get_Type (Get_Prefix (Expr)),
                               New_Obj_Value (Data.Offset));
   end Translate_Indexed_Name_Finish;

   function Translate_Indexed_Name (Prefix : Mnode; Expr : Iir)
                                       return Mnode
   is
   begin
      return Translate_Indexed_Name_Init (Prefix, Expr).Res;
   end Translate_Indexed_Name;

   type Slice_Name_Data is record
      Off    : Unsigned_64;
      Is_Off : Boolean;

      Unsigned_Diff : O_Dnode;

      --  Variable pointing to the prefix.
      Prefix_Var : Mnode;

      --  Variable pointing to slice.
      Slice_Range : Mnode;
   end record;

   procedure Translate_Slice_Name_Init
     (Prefix : Mnode; Expr : Iir_Slice_Name; Data : out Slice_Name_Data)
   is
      --  Type of the prefix.
      Prefix_Type : constant Iir := Get_Type (Get_Prefix (Expr));

      --  Type info of the prefix.
      Prefix_Info : Type_Info_Acc;

      --  Type of the first (and only) index of the prefix array type.
      Index_Type : constant Iir := Get_Index_Type (Prefix_Type, 0);

      --  Type of the slice.
      Slice_Type : constant Iir := Get_Type (Expr);
      Slice_Info : Type_Info_Acc;

      --  True iff the direction of the slice is known at compile time.
      Static_Range : Boolean;

      --  Suffix of the slice (discrete range).
      Expr_Range : constant Iir := Get_Suffix (Expr);

      --  Variable pointing to the prefix.
      Prefix_Var : Mnode;

      --  Type info of the range base type.
      Index_Info : Type_Info_Acc;

      --  Variables pointing to slice and prefix ranges.
      Slice_Range  : Mnode;
      Prefix_Range : Mnode;

      Diff            : O_Dnode;
      Unsigned_Diff   : O_Dnode;
      If_Blk, If_Blk1 : O_If_Block;
   begin
      --  Evaluate slice bounds.
      Chap3.Create_Array_Subtype (Slice_Type);

      --  The info may have just been created.
      Prefix_Info := Get_Info (Prefix_Type);
      Slice_Info := Get_Info (Slice_Type);

      if Slice_Info.Type_Mode = Type_Mode_Static_Array
        and then Slice_Info.Type_Locally_Constrained
        and then Prefix_Info.Type_Mode = Type_Mode_Static_Array
        and then Prefix_Info.Type_Locally_Constrained
      then
         Data.Is_Off := True;
         Data.Prefix_Var := Prefix;

         --  Both prefix and result are constrained array.
         declare
            Prefix_Left, Slice_Left : Iir_Int64;
            Off                     : Iir_Int64;
            Slice_Index_Type        : Iir;
            Slice_Range             : Iir;
            Slice_Length            : Iir_Int64;
            Index_Range             : Iir;
         begin
            Index_Range := Get_Range_Constraint (Index_Type);
            Prefix_Left := Eval_Pos (Get_Left_Limit (Index_Range));
            Slice_Index_Type := Get_Index_Type (Slice_Type, 0);
            Slice_Range := Get_Range_Constraint (Slice_Index_Type);
            Slice_Left := Eval_Pos (Get_Left_Limit (Slice_Range));
            Slice_Length := Eval_Discrete_Range_Length (Slice_Range);
            if Slice_Length = 0 then
               --  Null slice.
               Data.Off := 0;
               return;
            end if;
            if Get_Direction (Index_Range) /= Get_Direction (Slice_Range)
            then
               --  This is allowed with vhdl87
               Off := 0;
               Slice_Length := 0;
            else
               --  Both prefix and slice are thin array.
               case Get_Direction (Index_Range) is
                  when Iir_To =>
                     Off := Slice_Left - Prefix_Left;
                  when Iir_Downto =>
                     Off := Prefix_Left - Slice_Left;
               end case;
               if Off < 0 then
                  --  Must have been caught by sem.
                  raise Internal_Error;
               end if;
               if Off + Slice_Length
                 > Eval_Discrete_Range_Length (Index_Range)
               then
                  --  Must have been caught by sem.
                  raise Internal_Error;
               end if;
            end if;
            Data.Off := Unsigned_64 (Off);

            return;
         end;
      end if;

      Data.Is_Off := False;

      --  Save prefix.
      Prefix_Var := Stabilize (Prefix);

      Index_Info := Get_Info (Get_Base_Type (Index_Type));

      --  Save prefix bounds.
      Prefix_Range := Stabilize
        (Chap3.Get_Array_Range (Prefix_Var, Prefix_Type, 1));

      --  Save slice bounds.
      Slice_Range := Stabilize
        (Chap3.Bounds_To_Range (Chap3.Get_Composite_Type_Bounds (Slice_Type),
         Slice_Type, 1));

      --  TRUE if the direction of the slice is known.
      Static_Range := Get_Kind (Expr_Range) = Iir_Kind_Range_Expression;

      --  Check direction against same direction, error if different.
      --  FIXME: what about v87 -> if different then null slice
      if not Static_Range
        or else Get_Kind (Prefix_Type) /= Iir_Kind_Array_Subtype_Definition
      then
         --  Check same direction.
         Check_Direction_Error
           (New_Compare_Op (ON_Neq,
                            M2E (Chap3.Range_To_Dir (Prefix_Range)),
                            M2E (Chap3.Range_To_Dir (Slice_Range)),
                            Ghdl_Bool_Type),
            Expr);
      end if;

      Unsigned_Diff := Create_Temp (Ghdl_Index_Type);

      --  Check if not a null slice.
      --  The bounds of a null slice may be out of range.  So DIFF cannot
      --  be computed by substraction.
      Start_If_Stmt
        (If_Blk, New_Compare_Op (ON_Eq,
                                 M2E (Chap3.Range_To_Length (Slice_Range)),
                                 New_Lit (Ghdl_Index_0),
                                 Ghdl_Bool_Type));
      New_Assign_Stmt (New_Obj (Unsigned_Diff), New_Lit (Ghdl_Index_0));
      New_Else_Stmt (If_Blk);
      Diff := Create_Temp (Index_Info.Ortho_Type (Mode_Value));

      --  Compute the offset in the prefix.
      if not Static_Range then
         Start_If_Stmt
           (If_Blk1, New_Compare_Op (ON_Eq,
                                     M2E (Chap3.Range_To_Dir (Slice_Range)),
                                     New_Lit (Ghdl_Dir_To_Node),
                                     Ghdl_Bool_Type));
      end if;
      if not Static_Range or else Get_Direction (Expr_Range) = Iir_To then
         --  Diff = slice - bounds.
         New_Assign_Stmt
           (New_Obj (Diff),
            New_Dyadic_Op (ON_Sub_Ov,
                           M2E (Chap3.Range_To_Left (Slice_Range)),
                           M2E (Chap3.Range_To_Left (Prefix_Range))));
      end if;
      if not Static_Range then
         New_Else_Stmt (If_Blk1);
      end if;
      if not Static_Range or else Get_Direction (Expr_Range) = Iir_Downto
      then
         --  Diff = bounds - slice.
         New_Assign_Stmt
           (New_Obj (Diff),
            New_Dyadic_Op (ON_Sub_Ov,
                           M2E (Chap3.Range_To_Left (Prefix_Range)),
                           M2E (Chap3.Range_To_Left (Slice_Range))));
      end if;
      if not Static_Range then
         Finish_If_Stmt (If_Blk1);
      end if;

      --  Note: this also check for overflow.
      New_Assign_Stmt
        (New_Obj (Unsigned_Diff),
         New_Convert_Ov (New_Obj_Value (Diff), Ghdl_Index_Type));

      --  Check bounds.
      declare
         Err_1 : O_Enode;
         Err_2 : O_Enode;
      begin
         --  Bounds error if left of slice is before left of prefix.
         Err_1 := New_Compare_Op
           (ON_Lt,
            New_Obj_Value (Diff),
            New_Lit (New_Signed_Literal (Index_Info.Ortho_Type (Mode_Value),
                                         0)),
            Ghdl_Bool_Type);
         --  Bounds error if right of slice is after right of prefix.
         Err_2 := New_Compare_Op
           (ON_Gt,
            New_Dyadic_Op (ON_Add_Ov,
                           New_Obj_Value (Unsigned_Diff),
                           M2E (Chap3.Range_To_Length (Slice_Range))),
            M2E (Chap3.Range_To_Length (Prefix_Range)),
            Ghdl_Bool_Type);
         Check_Bound_Error (New_Dyadic_Op (ON_Or, Err_1, Err_2), Expr, 1);
      end;
      Finish_If_Stmt (If_Blk);

      Data.Slice_Range := Slice_Range;
      Data.Prefix_Var := Prefix_Var;
      Data.Unsigned_Diff := Unsigned_Diff;
      Data.Is_Off := False;
   end Translate_Slice_Name_Init;

   function Translate_Slice_Name_Finish
     (Prefix : Mnode; Expr : Iir_Slice_Name; Data : Slice_Name_Data)
         return Mnode
   is
      --  Type of the slice.
      Slice_Type : constant Iir := Get_Type (Expr);
      Slice_Info : constant Type_Info_Acc := Get_Info (Slice_Type);

      --  Object kind of the prefix.
      Kind : constant Object_Kind_Type := Get_Object_Kind (Prefix);

      Res_D : O_Dnode;
   begin
      if Data.Is_Off then
         return Chap3.Slice_Base
           (Prefix, Slice_Type, New_Lit (New_Index_Lit (Data.Off)));
      else
         --  Create the result (fat array) and assign the bounds field.
         case Slice_Info.Type_Mode is
            when Type_Mode_Unbounded_Array =>
               Res_D := Create_Temp (Slice_Info.Ortho_Type (Kind));
               New_Assign_Stmt
                 (New_Selected_Element (New_Obj (Res_D),
                  Slice_Info.B.Bounds_Field (Kind)),
                  New_Value (M2Lp (Data.Slice_Range)));
               New_Assign_Stmt
                 (New_Selected_Element (New_Obj (Res_D),
                                        Slice_Info.B.Base_Field (Kind)),
                  M2E (Chap3.Slice_Base
                         (Chap3.Get_Composite_Base (Prefix),
                          Slice_Type,
                          New_Obj_Value (Data.Unsigned_Diff))));
               return Dv2M (Res_D, Slice_Info, Kind);
            when Type_Mode_Bounded_Arrays =>
               return Chap3.Slice_Base
                 (Chap3.Get_Composite_Base (Prefix),
                  Slice_Type,
                  New_Obj_Value (Data.Unsigned_Diff));
            when others =>
               raise Internal_Error;
         end case;
      end if;
   end Translate_Slice_Name_Finish;

   function Translate_Slice_Name (Prefix : Mnode; Expr : Iir_Slice_Name)
                                     return Mnode
   is
      Data : Slice_Name_Data;
   begin
      Translate_Slice_Name_Init (Prefix, Expr, Data);
      return Translate_Slice_Name_Finish (Data.Prefix_Var, Expr, Data);
   end Translate_Slice_Name;

   function Translate_Interface_Name
     (Inter : Iir; Info : Ortho_Info_Acc; Mode : Object_Kind_Type)
     return Mnode
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Get_Type (Inter));
   begin
      case Info.Kind is
         when Kind_Object =>
            --  For a generic.
            pragma Assert (Mode = Mode_Value);
            return Get_Var (Info.Object_Var, Type_Info, Mode);
         when Kind_Signal =>
            --  For a port.
            if Mode = Mode_Signal then
               return Get_Var (Info.Signal_Sig, Type_Info, Mode_Signal);
            else
               pragma Assert (Info.Signal_Valp /= Null_Var);
               if Type_Info.Type_Mode in Type_Mode_Unbounded then
                  return Get_Var (Info.Signal_Valp, Type_Info, Mode_Value);
               else
                  return Get_Varp (Info.Signal_Valp, Type_Info, Mode_Value);
               end if;
            end if;
         when Kind_Interface =>
            --  For a parameter.
            if Info.Interface_Field (Mode) = O_Fnode_Null then
               --  Normal case: the parameter was translated as an ortho
               --  interface.
               case Info.Interface_Mechanism (Mode) is
                  when Pass_By_Copy =>
                     return Dv2M (Info.Interface_Decl (Mode), Type_Info, Mode);
                  when Pass_By_Address =>
                     --  Parameter is passed by reference.
                     return Dp2M (Info.Interface_Decl (Mode), Type_Info, Mode);
               end case;
            else
               --  The parameter was put somewhere else.
               declare
                  Subprg      : constant Iir := Get_Parent (Inter);
                  Subprg_Info : constant Subprg_Info_Acc :=
                    Get_Info (Subprg);
                  Linter      : O_Lnode;
               begin
                  if Info.Interface_Decl (Mode) = O_Dnode_Null then
                     --  The parameter is passed via a field of the PARAMS
                     --  record parameter.
                     if Subprg_Info.Subprg_Params_Var = Null_Var then
                        --  Direct access to the parameter.
                        Linter := New_Obj (Subprg_Info.Res_Interface);
                     else
                        --  Unnesting case: upscope access.
                        Linter := Get_Var (Subprg_Info.Subprg_Params_Var);
                     end if;
                     Linter := New_Selected_Element
                       (New_Acc_Value (Linter), Info.Interface_Field (Mode));
                  else
                     --  Unnesting case: the parameter was copied in the
                     --  subprogram frame so that nested subprograms can
                     --  reference it.  Use field in FRAME.
                     Linter := New_Selected_Element
                       (Get_Instance_Ref (Subprg_Info.Subprg_Frame_Scope),
                        Info.Interface_Field (Mode));
                  end if;
                  case Info.Interface_Mechanism (Mode) is
                     when Pass_By_Copy =>
                        return Lv2M (Linter, Type_Info, Mode);
                     when Pass_By_Address =>
                        return Lp2M (Linter, Type_Info, Mode);
                  end case;
               end;
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Translate_Interface_Name;

   function Translate_Selected_Element
     (Prefix : Mnode; El : Iir_Element_Declaration) return Mnode
   is
      El_Type       : constant Iir := Get_Type (El);
      El_Tinfo      : constant Type_Info_Acc := Get_Info (El_Type);
      Kind          : constant Object_Kind_Type := Get_Object_Kind (Prefix);
      El_Info       : Field_Info_Acc;
      Base_Tinfo    : Type_Info_Acc;
      Stable_Prefix : Mnode;
      Base, Res, Fat_Res : Mnode;
      Rec_Layout : Mnode;
      El_Descr : Mnode;
      Box_Field : O_Fnode;
      B : O_Lnode;
   begin
      --  There are 3 cases:
      --  a) the record is bounded (and so is the element).
      --  b) the record is unbounded and the element is bounded
      --  c) the record is unbounded and the element is unbounded.
      --  If the record is unbounded, PREFIX is a fat pointer.
      --  On top of that, the element may be complex.

      --  For record subtypes, there is no info for elements that have not
      --  changed.
      El_Info := Get_Info (El);
      if El_Info = null then
         El_Info := Get_Info (Get_Base_Element_Declaration (El));
      end if;

      if Is_Unbounded_Type (El_Tinfo) then
         Stable_Prefix := Stabilize (Prefix);

         --  Result is a fat pointer, create it and set bounds.
         --  FIXME: layout for record, bounds for array!
         Fat_Res := Create_Temp (El_Tinfo, Kind);
         El_Descr := Chap3.Record_Layout_To_Element_Layout
           (Chap3.Get_Composite_Bounds (Stable_Prefix), El);
         case El_Tinfo.Type_Mode is
            when Type_Mode_Unbounded_Record =>
               null;
            when Type_Mode_Unbounded_Array =>
               El_Descr := Chap3.Layout_To_Bounds (El_Descr);
            when others =>
               raise Internal_Error;
         end case;
         New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (Fat_Res)),
                          M2Addr (El_Descr));
      else
         Stable_Prefix := Prefix;
      end if;

      --  Get the base.
      Base := Chap3.Get_Composite_Base (Stable_Prefix);
      Base_Tinfo := Get_Type_Info (Base);
      Box_Field := Base_Tinfo.S.Box_Field (Kind);

      if (Box_Field = O_Fnode_Null
            or else Get_Type_Staticness (El_Type) /= Locally)
        and then (Is_Complex_Type (El_Tinfo) or Is_Unbounded_Type (El_Tinfo))
      then
         Stabilize (Base);

         if Box_Field /= O_Fnode_Null
           and then Get_Type_Staticness (El_Type) /= Locally
         then
            --  Unbox.
            B := New_Selected_Element (M2Lv (Base), Box_Field);
         else
            B := M2Lv (Base);
         end if;

         --  The element is complex: it's an offset.
         Rec_Layout := Chap3.Get_Composite_Bounds (Stable_Prefix);
         Res := E2M
           (New_Unchecked_Address
              (New_Slice
                   (New_Access_Element
                        (New_Unchecked_Address (M2Lv (Base), Char_Ptr_Type)),
                    Chararray_Type,
                    New_Value
                      (Chap3.Record_Layout_To_Element_Offset
                         (Rec_Layout, El, Kind))),
               El_Tinfo.B.Base_Ptr_Type (Kind)),
            El_Tinfo, Kind);
      else
         --  Normal element.
         B := M2Lv (Base);

         if Box_Field /= O_Fnode_Null
           and then Get_Kind (El) = Iir_Kind_Element_Declaration
         then
            --  Unbox.
            B := New_Selected_Element (B, Box_Field);
         end if;

         Res := Lv2M (New_Selected_Element (B, El_Info.Field_Node (Kind)),
                      El_Tinfo, Kind);
      end if;

      if Is_Unbounded_Type (El_Tinfo) then
         New_Assign_Stmt
           (New_Selected_Element (M2Lv (Fat_Res),
                                  El_Tinfo.B.Base_Field (Kind)),
            M2Addr (Res));
         return Fat_Res;
      else
         return Res;
      end if;
   end Translate_Selected_Element;

   --       function Translate_Formal_Interface_Name (Scope_Type : O_Tnode;
   --                                                 Scope_Param : O_Lnode;
   --                                                 Name : Iir;
   --                                                 Kind : Object_Kind_Type)
   --                                                return Mnode
   --       is
   --          Type_Info : Type_Info_Acc;
   --          Info : Ortho_Info_Acc;
   --          Res : Mnode;
   --       begin
   --          Type_Info := Get_Info (Get_Type (Name));
   --          Info := Get_Info (Name);
   --          Push_Scope_Soft (Scope_Type, Scope_Param);
   --          Res := Get_Var (Info.Object_Var, Type_Info, Kind);
   --          Clear_Scope_Soft (Scope_Type);
   --          return Res;
   --       end Translate_Formal_Interface_Name;

   --       function Translate_Formal_Name (Scope_Type : O_Tnode;
   --                                       Scope_Param : O_Lnode;
   --                                       Name : Iir)
   --                                      return Mnode
   --       is
   --          Prefix : Iir;
   --          Prefix_Name : Mnode;
   --       begin
   --          case Get_Kind (Name) is
   --             when Iir_Kind_Interface_Constant_Declaration =>
   --                return Translate_Formal_Interface_Name
   --                  (Scope_Type, Scope_Param, Name, Mode_Value);

   --             when Iir_Kind_Interface_Signal_Declaration =>
   --                return Translate_Formal_Interface_Name
   --                  (Scope_Type, Scope_Param, Name, Mode_Signal);

   --             when Iir_Kind_Indexed_Name =>
   --                Prefix := Get_Prefix (Name);
   --                Prefix_Name := Translate_Formal_Name
   --                  (Scope_Type, Scope_Param, Prefix);
   --                return Translate_Indexed_Name (Prefix_Name, Name);

   --             when Iir_Kind_Slice_Name =>
   --                Prefix := Get_Prefix (Name);
   --                Prefix_Name := Translate_Formal_Name
   --                  (Scope_Type, Scope_Param, Prefix);
   --                return Translate_Slice_Name (Prefix_Name, Name);

   --             when Iir_Kind_Selected_Element =>
   --                Prefix := Get_Prefix (Name);
   --                Prefix_Name := Translate_Formal_Name
   --                  (Scope_Type, Scope_Param, Prefix);
   --                return Translate_Selected_Element
   --                  (Prefix_Name, Get_Selected_Element (Name));

   --             when others =>
   --                Error_Kind ("translate_generic_name", Name);
   --          end case;
   --       end Translate_Formal_Name;

   function Translate_Object_Alias_Name (Name : Iir; Mode : Object_Kind_Type)
                                        return Mnode
   is
      Name_Type : constant Iir := Get_Type (Name);
      Name_Info : constant Ortho_Info_Acc := Get_Info (Name);
      Type_Info : constant Type_Info_Acc := Get_Info (Name_Type);
      R : O_Lnode;
      pragma Assert (Mode <= Name_Info.Alias_Kind);
   begin
      --  Alias_Var is not like an object variable, since it is
      --  always a pointer to the aliased object.
      case Type_Info.Type_Mode is
         when Type_Mode_Unbounded_Array =>
            --  Get_Var for Mnode is ok here as an unbounded object is always
            --  a pointer (and so is an alias).
            return Get_Var (Name_Info.Alias_Var (Mode), Type_Info, Mode);
         when Type_Mode_Bounded_Arrays
           | Type_Mode_Bounded_Records
           | Type_Mode_Acc
           | Type_Mode_Bounds_Acc =>
            R := Get_Var (Name_Info.Alias_Var (Mode));
            return Lp2M (R, Type_Info, Mode);
         when Type_Mode_Scalar =>
            R := Get_Var (Name_Info.Alias_Var (Mode));
            if Mode = Mode_Signal then
               return Lv2M (R, Type_Info, Mode_Signal);
            else
               return Lp2M (R, Type_Info, Mode_Value);
            end if;
         when others =>
            raise Internal_Error;
      end case;
   end Translate_Object_Alias_Name;

   function Translate_Name (Name : Iir; Mode : Object_Kind_Type) return Mnode
   is
      Name_Type : constant Iir := Get_Type (Name);
      Name_Info : constant Ortho_Info_Acc := Get_Info (Name);
      Type_Info : constant Type_Info_Acc := Get_Info (Name_Type);
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Constant_Declaration
           | Iir_Kind_Variable_Declaration
           | Iir_Kind_File_Declaration =>
            pragma Assert (Mode = Mode_Value);
            return Get_Var (Name_Info.Object_Var, Type_Info, Mode_Value);

         when Iir_Kind_Attribute_Name =>
            return Translate_Name (Get_Named_Entity (Name), Mode);
         when Iir_Kind_Attribute_Value =>
            pragma Assert (Mode = Mode_Value);
            declare
               Attr : constant Iir := Get_Attribute_Specification (Name);
               Val : Iir;
            begin
               if Get_Expr_Staticness (Get_Expression (Attr)) = None then
                  Val := Name;
               else
                  --  If the expression is static, an object is created only
                  --  for the first value.
                  Val := Get_Attribute_Value_Spec_Chain (Attr);
               end if;
               return Get_Var (Get_Info (Val).Object_Var,
                               Type_Info, Mode_Value);
            end;

         when Iir_Kind_Object_Alias_Declaration =>
            --  Alias_Var is not like an object variable, since it is
            --  always a pointer to the aliased object.
            declare
               R : O_Lnode;
            begin
               pragma Assert (Mode <= Name_Info.Alias_Kind);
               case Type_Info.Type_Mode is
                  when Type_Mode_Unbounded_Array =>
                     return Get_Var (Name_Info.Alias_Var (Mode), Type_Info,
                                     Mode);
                  when Type_Mode_Bounded_Arrays
                     | Type_Mode_Bounded_Records
                     | Type_Mode_Acc
                     | Type_Mode_Bounds_Acc =>
                     R := Get_Var (Name_Info.Alias_Var (Mode));
                     return Lp2M (R, Type_Info, Mode);
                  when Type_Mode_Scalar =>
                     R := Get_Var (Name_Info.Alias_Var (Mode));
                     if Mode = Mode_Signal then
                        return Lv2M (R, Type_Info, Mode_Signal);
                     else
                        return Lp2M (R, Type_Info, Mode_Value);
                     end if;
                  when others =>
                     raise Internal_Error;
               end case;
            end;

         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Guard_Signal_Declaration =>
            if Mode = Mode_Signal then
               return Get_Var (Name_Info.Signal_Sig, Type_Info, Mode_Signal);
            else
               return Get_Var (Name_Info.Signal_Val, Type_Info, Mode_Value);
            end if;

         when Iir_Kind_Interface_Constant_Declaration
           | Iir_Kind_Interface_File_Declaration
           | Iir_Kind_Interface_Variable_Declaration =>
            pragma Assert (Mode = Mode_Value);
            return Translate_Interface_Name (Name, Name_Info, Mode_Value);

         when Iir_Kind_Interface_Signal_Declaration =>
            return Translate_Interface_Name (Name, Name_Info, Mode);

         when Iir_Kind_Indexed_Name =>
            return Translate_Indexed_Name
              (Translate_Name (Get_Prefix (Name), Mode), Name);

         when Iir_Kind_Slice_Name =>
            return Translate_Slice_Name
              (Translate_Name (Get_Prefix (Name), Mode), Name);

         when Iir_Kind_Dereference
           | Iir_Kind_Implicit_Dereference =>
            pragma Assert (Mode = Mode_Value);
            declare
               Prefix : constant Iir := Get_Prefix (Name);
               Prefix_Type : constant Iir := Get_Type (Prefix);
               Pt_Info : constant Type_Info_Acc := Get_Info (Prefix_Type);
               Pfx : O_Enode;
               Pfx_Var : O_Dnode;
            begin
               Pfx := Chap7.Translate_Expression (Prefix);
               if Pt_Info.Type_Mode = Type_Mode_Bounds_Acc then
                  Pfx_Var := Create_Temp_Init
                    (Pt_Info.Ortho_Type (Mode_Value), Pfx);
                  return Chap7.Bounds_Acc_To_Fat_Pointer
                    (Pfx_Var, Prefix_Type);
               else
                  return Lv2M
                    (New_Access_Element
                       (New_Convert_Ov
                          (Pfx, Type_Info.Ortho_Ptr_Type (Mode_Value))),
                     Type_Info, Mode_Value);
               end if;
            end;

         when Iir_Kind_Selected_Element =>
            return Translate_Selected_Element
              (Translate_Name (Get_Prefix (Name), Mode),
               Get_Selected_Element (Name));

         when Iir_Kind_Function_Call =>
            pragma Assert (Mode = Mode_Value);
            --  This can appear as a prefix of a name, therefore, the
            --  result is always a composite type or an access type.
            return E2M (Chap7.Translate_Expression (Name),
                        Type_Info, Mode_Value);

         when Iir_Kind_Image_Attribute =>
            pragma Assert (Mode = Mode_Value);
            --  Can appear as a prefix.
            return E2M (Chap14.Translate_Image_Attribute (Name),
                        Type_Info, Mode_Value);

         when Iir_Kind_Simple_Name
            | Iir_Kind_Selected_Name =>
            return Translate_Name (Get_Named_Entity (Name), Mode);

         when others =>
            Error_Kind ("translate_name", Name);
      end case;
   end Translate_Name;

   function Get_Signal_Direct_Driver (Sig : Iir) return Mnode
   is
      Info : constant Ortho_Info_Acc := Get_Info (Sig);
      Type_Info : constant Type_Info_Acc := Get_Info (Get_Type (Sig));
   begin
      return Get_Var (Info.Signal_Driver, Type_Info, Mode_Value);
   end Get_Signal_Direct_Driver;

   function Get_Port_Init_Value (Port : Iir) return Mnode
   is
      Info : constant Ortho_Info_Acc := Get_Info (Port);
      Type_Info : constant Type_Info_Acc := Get_Info (Get_Type (Port));
   begin
      return Get_Var (Info.Signal_Val, Type_Info, Mode_Value);
   end Get_Port_Init_Value;

   generic
      with procedure Translate_Signal_Base
        (Name : Iir; Sig : out Mnode; Drv : out Mnode);
   procedure Translate_Signal (Name : Iir; Sig : out Mnode; Drv : out Mnode);

   procedure Translate_Signal (Name : Iir; Sig : out Mnode; Drv : out Mnode) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Simple_Name
            | Iir_Kind_Selected_Name =>
            Translate_Signal (Get_Named_Entity (Name), Sig, Drv);
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Guard_Signal_Declaration
           | Iir_Kind_Object_Alias_Declaration =>
            Translate_Signal_Base (Name, Sig, Drv);
         when Iir_Kind_Slice_Name =>
            declare
               Data    : Slice_Name_Data;
               Pfx_Sig : Mnode;
               Pfx_Drv : Mnode;
            begin
               Translate_Signal (Get_Prefix (Name), Pfx_Sig, Pfx_Drv);
               Translate_Slice_Name_Init (Pfx_Sig, Name, Data);
               Sig := Translate_Slice_Name_Finish
                 (Data.Prefix_Var, Name, Data);
               Drv := Translate_Slice_Name_Finish
                 (Pfx_Drv, Name, Data);
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Data    : Indexed_Name_Data;
               Pfx_Sig : Mnode;
               Pfx_Drv : Mnode;
            begin
               Translate_Signal (Get_Prefix (Name), Pfx_Sig, Pfx_Drv);
               Data := Translate_Indexed_Name_Init (Pfx_Sig, Name);
               Sig := Data.Res;
               Drv := Translate_Indexed_Name_Finish (Pfx_Drv, Name, Data);
            end;
         when Iir_Kind_Selected_Element =>
            declare
               El      : constant Iir := Get_Selected_Element (Name);
               Pfx_Sig : Mnode;
               Pfx_Drv : Mnode;
            begin
               Translate_Signal (Get_Prefix (Name), Pfx_Sig, Pfx_Drv);
               Sig := Translate_Selected_Element (Pfx_Sig, El);
               Drv := Translate_Selected_Element (Pfx_Drv, El);
            end;
         when others =>
            Error_Kind ("translate_signal", Name);
      end case;
   end Translate_Signal;

   procedure Translate_Direct_Driver_Base
     (Name : Iir; Sig : out Mnode; Drv : out Mnode) is
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            declare
               Name_Type : constant Iir := Get_Type (Name);
               Name_Info : constant Ortho_Info_Acc := Get_Info (Name);
               Type_Info : constant Type_Info_Acc := Get_Info (Name_Type);
            begin
               Sig := Get_Var (Name_Info.Signal_Sig, Type_Info, Mode_Signal);
               Drv := Get_Var (Name_Info.Signal_Driver, Type_Info, Mode_Value);
            end;
         when Iir_Kind_Object_Alias_Declaration =>
            Translate_Direct_Driver (Get_Name (Name), Sig, Drv);
         when others =>
            Error_Kind ("translate_direct_driver_base", Name);
      end case;
   end Translate_Direct_Driver_Base;

   procedure Translate_Direct_Driver_1 is new
     Translate_Signal (Translate_Signal_Base => Translate_Direct_Driver_Base);

   procedure Translate_Direct_Driver
     (Name : Iir; Sig : out Mnode; Drv : out Mnode)
     renames Translate_Direct_Driver_1;

   procedure Translate_Port_Init_Base
     (Name : Iir; Sig : out Mnode; Drv : out Mnode)
   is
      Name_Type : constant Iir := Get_Type (Name);
      Name_Info : constant Ortho_Info_Acc := Get_Info (Name);
      Type_Info : constant Type_Info_Acc := Get_Info (Name_Type);
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Interface_Signal_Declaration =>
            Sig := Get_Var (Name_Info.Signal_Sig, Type_Info, Mode_Signal);
            Drv := Get_Var (Name_Info.Signal_Val, Type_Info, Mode_Value);
         when others =>
            Error_Kind ("translate_direct_driver_base", Name);
      end case;
   end Translate_Port_Init_Base;

   procedure Translate_Port_Init_1 is new
     Translate_Signal (Translate_Signal_Base => Translate_Port_Init_Base);

   procedure Translate_Port_Init
     (Name : Iir; Sig : out Mnode; Init : out Mnode)
     renames Translate_Port_Init_1;

   procedure Translate_Signal_Base
     (Name : Iir; Sig : out Mnode; Val : out Mnode)
   is
      Name_Type : constant Iir := Get_Type (Name);
      Name_Info : constant Ortho_Info_Acc := Get_Info (Name);
      Type_Info : constant Type_Info_Acc := Get_Info (Name_Type);
   begin
      case Get_Kind (Name) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Stable_Attribute
           | Iir_Kind_Quiet_Attribute
           | Iir_Kind_Delayed_Attribute
           | Iir_Kind_Transaction_Attribute
           | Iir_Kind_Guard_Signal_Declaration =>
            Sig := Get_Var (Name_Info.Signal_Sig, Type_Info, Mode_Signal);
            Val := Get_Var (Name_Info.Signal_Val, Type_Info, Mode_Value);
         when Iir_Kind_Interface_Signal_Declaration =>
            Sig := Translate_Interface_Name (Name, Name_Info, Mode_Signal);
            Val := Translate_Interface_Name (Name, Name_Info, Mode_Value);
         when Iir_Kind_Object_Alias_Declaration =>
            Sig := Translate_Object_Alias_Name (Name, Mode_Signal);
            Val := Translate_Object_Alias_Name (Name, Mode_Value);
         when others =>
            Error_Kind ("translate_signal_base", Name);
      end case;
   end Translate_Signal_Base;

   procedure Translate_Signal_Name_1 is new
     Translate_Signal (Translate_Signal_Base);

   procedure Translate_Signal_Name
     (Name : Iir; Sig : out Mnode; Val : out Mnode)
     renames Translate_Signal_Name_1;
end Trans.Chap6;
