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

package Trans.Chap7 is
   --  Allocate a value (with the same bounds) on the stack for SIG.
   --  Note: SIG must be stable.
   function Allocate_Value_From_Signal (Sig : Mnode; Sig_Type : Iir)
                                       return Mnode;

   --  Generic function to extract a value from a signal.
   generic
      with function Read_Value (Sig : O_Enode; Sig_Type : Iir)
                                   return O_Enode;
   function Translate_Signal_Value (Sig : Mnode; Sig_Type : Iir) return Mnode;

   function Translate_Signal_Driving_Value (Sig : Mnode; Sig_Type : Iir)
                                           return Mnode;

   --  For conversions.
   procedure Set_Driving_Value (Sig : Mnode; Sig_Type : Iir; Val : Mnode);

   --  Translate expression EXPR into ortho tree.
   function Translate_Expression (Expr : Iir; Rtype : Iir := Null_Iir)
                                 return O_Enode;
   function Translate_Expression (Expr : Iir; Rtype : Iir := Null_Iir)
                                 return Mnode;

   --  Translate range and return an lvalue containing the range.
   --  The node returned can be used only one time.
   function Translate_Range (Arange : Iir; Range_Type : Iir) return O_Lnode;

   --  Translate range expression ARANGE and store the result into RES, of
   --  type RANGE_TYPE.
   procedure Translate_Range (Res : Mnode; Arange : Iir; Range_Type : Iir);
   function Translate_Static_Range (Arange : Iir; Range_Type : Iir)
                                   return O_Cnode;

   --  Same as Translate_Range, but for a discrete range (ie: ARANGE
   --  can be a discrete subtype indication).
   procedure Translate_Discrete_Range (Res : Mnode; Arange : Iir);

   --  Return TRUE iff constant declaration DECL can be staticly defined.
   --  This is of course true if its expression is a locally static literal,
   --  but can be true in a few cases for aggregates.
   --  This function belongs to Translation, since it is defined along
   --  with the translate_static_aggregate procedure.
   function Is_Static_Constant (Decl : Iir_Constant_Declaration)
                                   return Boolean;

   --  Translate the static expression EXPR into an ortho expression whose
   --  type must be RES_TYPE.  Therefore, an implicite conversion might
   --  occurs.
   function Translate_Static_Expression (Expr : Iir; Res_Type : Iir)
                                        return O_Cnode;
   function Translate_Numeric_Literal (Expr : Iir; Res_Type : O_Tnode)
                                      return O_Cnode;
   function Translate_Enumeration_Literal (Atype : Iir; Pos : Natural)
                                          return O_Cnode;

   --  Convert (if necessary) EXPR of type EXPR_TYPE to type ATYPE.
   function Translate_Implicit_Conv (Expr      : O_Enode;
                                     Expr_Type : Iir;
                                     Atype     : Iir;
                                     Is_Sig    : Object_Kind_Type;
                                     Loc       : Iir)
                                    return O_Enode;

   function Translate_Type_Conversion
     (Expr : O_Enode; Expr_Type : Iir; Res_Type : Iir; Loc : Iir)
     return O_Enode;

   --  Subtype conversions.
   procedure Convert_Constrained_To_Unconstrained
     (Res : in out Mnode; Expr : Mnode);

   --  Convert bounds SRC (of type SRC_TYPE) to RES (of type RES_TYPE).
   procedure Translate_Type_Conversion_Bounds
     (Res : Mnode; Src : Mnode; Res_Type : Iir; Src_Type : Iir; Loc : Iir);

   --  Convert range EXPR into ortho tree.
   --  If RANGE_TYPE /= NULL_IIR, convert bounds to RANGE_TYPE.
   --function Translate_Range (Expr : Iir; Range_Type : Iir) return O_Enode;
   function Translate_Static_Range_Left
     (Expr : Iir; Range_Type : Iir := Null_Iir)
         return O_Cnode;
   function Translate_Static_Range_Right
     (Expr : Iir; Range_Type : Iir := Null_Iir)
         return O_Cnode;
   function Translate_Static_Range_Dir (Expr : Iir) return O_Cnode;
   function Translate_Static_Range_Length (Expr : Iir) return O_Cnode;

   --  These functions evaluates left bound/right bound/length of the
   --  range expression EXPR.
   function Translate_Range_Expression_Left (Expr       : Iir;
                                             Range_Type : Iir := Null_Iir)
                                                return O_Enode;
   function Translate_Range_Expression_Right (Expr       : Iir;
                                              Range_Type : Iir := Null_Iir)
                                                 return O_Enode;
   function Translate_Range_Expression_Length (Expr : Iir) return O_Enode;

   --  Get the length of any range expression (ie maybe an attribute).
   function Translate_Range_Length (Expr : Iir) return O_Enode;

   --  Assign AGGR to TARGET of type TARGET_TYPE.
   procedure Translate_Aggregate
     (Target : Mnode; Target_Type : Iir; Aggr : Iir);

   --  Fill BOUNDS from aggregate AGGR.
   procedure Translate_Aggregate_Bounds
     (Bounds : Mnode; Aggr : Iir; Mode : Object_Kind_Type);

   --  Convert bounds access PTR to a fat pointer.
   function Bounds_Acc_To_Fat_Pointer (Ptr : O_Dnode; Acc_Type : Iir)
                                      return Mnode;

   --  Translate implicit functions defined by a type.
   type Implicit_Subprogram_Infos is private;
   procedure Init_Implicit_Subprogram_Infos
     (Infos : out Implicit_Subprogram_Infos);
   procedure Translate_Implicit_Subprogram_Spec
     (Subprg : Iir; Infos : in out Implicit_Subprogram_Infos);
   procedure Translate_Implicit_Subprogram_Body (Subprg : Iir);

   --  Assign EXPR to TARGET.  LOC is the location used to report errors.
   --  FIXME: do the checks.
   procedure Translate_Assign
     (Target : Mnode; Expr : Iir; Target_Type : Iir);
   procedure Translate_Assign
     (Target : Mnode;
      Val    : O_Enode; Expr : Iir; Target_Type : Iir; Loc : Iir);

   --  Find the declaration of the predefined function IMP in type
   --  definition BASE_TYPE.
   function Find_Predefined_Function
     (Base_Type : Iir; Imp : Iir_Predefined_Functions)
         return Iir;

   function Translate_Lib_Operator (Left, Right : O_Enode; Func : O_Dnode)
                                   return O_Enode;

   function Get_ON_Op (Imp : Iir) return ON_Op_Kind;
private
   type Implicit_Subprogram_Infos is record
      Arr_Eq_Info     : Operator_Info_Acc;
      Rec_Eq_Info     : Operator_Info_Acc;
      Arr_Cmp_Info    : Operator_Info_Acc;
      Arr_Shl_Info    : Operator_Info_Acc;
      Arr_Sha_Info    : Operator_Info_Acc;
      Arr_Rot_Info    : Operator_Info_Acc;
   end record;
end Trans.Chap7;
