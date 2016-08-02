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

with Evaluation; use Evaluation;
with Std_Package; use Std_Package;
with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
with Trans_Decls; use Trans_Decls;
with Trans.Chap3;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Rtis;
with Trans.Helpers2; use Trans.Helpers2;
with Trans.Foreach_Non_Composite;

package body Trans.Chap14 is
   use Trans.Helpers;

   function Translate_Array_Attribute_To_Range (Expr : Iir) return Mnode
   is
      Prefix    : constant Iir := Get_Prefix (Expr);
      Type_Name : constant Iir := Is_Type_Name (Prefix);
      Arr       : Mnode;
      Dim       : Natural;
   begin
      if Type_Name /= Null_Iir then
         --  Prefix denotes a type name
         Arr := T2M (Type_Name, Mode_Value);
      else
         --  Prefix is an object.
         Arr := Chap6.Translate_Name (Prefix, Mode_Value);
      end if;
      Dim := Natural (Get_Value (Get_Parameter (Expr)));
      return Chap3.Get_Array_Range (Arr, Get_Type (Prefix), Dim);
   end Translate_Array_Attribute_To_Range;

   function Translate_Range_Array_Attribute (Expr : Iir)
                                                return O_Lnode is
   begin
      return M2Lv (Translate_Array_Attribute_To_Range (Expr));
   end Translate_Range_Array_Attribute;

   function Translate_Length_Array_Attribute (Expr : Iir; Rtype : Iir)
                                                 return O_Enode
   is
      Rng : Mnode;
      Val : O_Enode;
   begin
      Rng := Translate_Array_Attribute_To_Range (Expr);
      Val := M2E (Chap3.Range_To_Length (Rng));
      if Rtype /= Null_Iir then
         Val := New_Convert_Ov (Val, Get_Ortho_Type (Rtype, Mode_Value));
      end if;
      return Val;
   end Translate_Length_Array_Attribute;

   --  Extract high or low bound of RANGE_VAR.
   function Range_To_High_Low
     (Range_Var : Mnode; Range_Type : Iir; Is_High : Boolean)
         return Mnode
   is
      Op         : ON_Op_Kind;
      If_Blk     : O_If_Block;
      Range_Svar : constant Mnode := Stabilize (Range_Var);
      Res        : O_Dnode;
      Tinfo      : constant Ortho_Info_Acc :=
        Get_Info (Get_Base_Type (Range_Type));
   begin
      Res := Create_Temp (Tinfo.Ortho_Type (Mode_Value));
      Open_Temp;
      if Is_High then
         Op := ON_Neq;
      else
         Op := ON_Eq;
      end if;
      Start_If_Stmt (If_Blk,
                     New_Compare_Op (Op,
                       M2E (Chap3.Range_To_Dir (Range_Svar)),
                       New_Lit (Ghdl_Dir_To_Node),
                       Ghdl_Bool_Type));
      New_Assign_Stmt (New_Obj (Res),
                       M2E (Chap3.Range_To_Left (Range_Svar)));
      New_Else_Stmt (If_Blk);
      New_Assign_Stmt (New_Obj (Res),
                       M2E (Chap3.Range_To_Right (Range_Svar)));
      Finish_If_Stmt (If_Blk);
      Close_Temp;
      return Dv2M (Res, Tinfo, Mode_Value);
   end Range_To_High_Low;

   function Translate_High_Low_Type_Attribute
     (Atype : Iir; Is_High : Boolean) return O_Enode
   is
      Cons : constant Iir := Get_Range_Constraint (Atype);
   begin
      --  FIXME: improve code if constraint is a range expression.
      if Get_Type_Staticness (Atype) = Locally then
         if Get_Direction (Cons) = Iir_To xor Is_High then
            return New_Lit
              (Chap7.Translate_Static_Range_Left (Cons, Atype));
         else
            return New_Lit
              (Chap7.Translate_Static_Range_Right (Cons, Atype));
         end if;
      else
         return M2E (Range_To_High_Low
                     (Chap3.Type_To_Range (Atype), Atype, Is_High));
      end if;
   end Translate_High_Low_Type_Attribute;

   function Translate_High_Low_Array_Attribute (Expr    : Iir;
                                                Is_High : Boolean)
                                                   return O_Enode
   is
   begin
      --  FIXME: improve code if index is a range expression.
      return M2E (Range_To_High_Low
                  (Translate_Array_Attribute_To_Range (Expr),
                     Get_Type (Expr), Is_High));
   end Translate_High_Low_Array_Attribute;

   function Translate_Low_Array_Attribute (Expr : Iir)
                                              return O_Enode
   is
   begin
      return Translate_High_Low_Array_Attribute (Expr, False);
   end Translate_Low_Array_Attribute;

   function Translate_High_Array_Attribute (Expr : Iir)
                                               return O_Enode
   is
   begin
      return Translate_High_Low_Array_Attribute (Expr, True);
   end Translate_High_Array_Attribute;

   function Translate_Left_Array_Attribute (Expr : Iir)
                                               return O_Enode
   is
      Rng : Mnode;
   begin
      Rng := Translate_Array_Attribute_To_Range (Expr);
      return M2E (Chap3.Range_To_Left (Rng));
   end Translate_Left_Array_Attribute;

   function Translate_Right_Array_Attribute (Expr : Iir)
                                                return O_Enode
   is
      Rng : Mnode;
   begin
      Rng := Translate_Array_Attribute_To_Range (Expr);
      return M2E (Chap3.Range_To_Right (Rng));
   end Translate_Right_Array_Attribute;

   function Translate_Ascending_Array_Attribute (Expr : Iir)
                                                    return O_Enode
   is
      Rng : Mnode;
   begin
      Rng := Translate_Array_Attribute_To_Range (Expr);
      return New_Compare_Op (ON_Eq,
                             M2E (Chap3.Range_To_Dir (Rng)),
                             New_Lit (Ghdl_Dir_To_Node),
                             Std_Boolean_Type_Node);
   end Translate_Ascending_Array_Attribute;

   function Translate_Left_Type_Attribute (Atype : Iir) return O_Enode is
   begin
      if Get_Type_Staticness (Atype) = Locally then
         return New_Lit (Chap7.Translate_Static_Range_Left
                         (Get_Range_Constraint (Atype), Atype));
      else
         return M2E (Chap3.Range_To_Left (Chap3.Type_To_Range (Atype)));
      end if;
   end Translate_Left_Type_Attribute;

   function Translate_Right_Type_Attribute (Atype : Iir) return O_Enode is
   begin
      if Get_Type_Staticness (Atype) = Locally then
         return New_Lit (Chap7.Translate_Static_Range_Right
                         (Get_Range_Constraint (Atype), Atype));
      else
         return M2E (Chap3.Range_To_Right (Chap3.Type_To_Range (Atype)));
      end if;
   end Translate_Right_Type_Attribute;

   function Translate_Dir_Type_Attribute (Atype : Iir) return O_Enode
   is
      Info : Type_Info_Acc;
   begin
      if Get_Type_Staticness (Atype) = Locally then
         return New_Lit (Chap7.Translate_Static_Range_Dir
                         (Get_Range_Constraint (Atype)));
      else
         Info := Get_Info (Atype);
         return New_Value
           (New_Selected_Element (Get_Var (Info.T.Range_Var),
            Info.T.Range_Dir));
      end if;
   end Translate_Dir_Type_Attribute;

   function Translate_Val_Attribute (Attr : Iir) return O_Enode
   is
      Val       : O_Enode;
      Attr_Type : Iir;
      Res_Var   : O_Dnode;
      Res_Type  : O_Tnode;
   begin
      Attr_Type := Get_Type (Attr);
      Res_Type := Get_Ortho_Type (Attr_Type, Mode_Value);
      Res_Var := Create_Temp (Res_Type);
      Val := Chap7.Translate_Expression (Get_Parameter (Attr));

      case Get_Kind (Attr_Type) is
         when Iir_Kind_Enumeration_Type_Definition
            | Iir_Kind_Enumeration_Subtype_Definition =>
            --  For enumeration, always check the value is in the enum
            --  range.
            declare
               Val_Type : O_Tnode;
               Val_Var  : O_Dnode;
               If_Blk   : O_If_Block;
            begin
               Val_Type := Get_Ortho_Type (Get_Type (Get_Parameter (Attr)),
                                           Mode_Value);
               Val_Var := Create_Temp_Init (Val_Type, Val);
               Start_If_Stmt
                 (If_Blk,
                  New_Dyadic_Op
                    (ON_Or,
                     New_Compare_Op (ON_Lt,
                       New_Obj_Value (Val_Var),
                       New_Lit (New_Signed_Literal
                         (Val_Type, 0)),
                       Ghdl_Bool_Type),
                     New_Compare_Op (ON_Ge,
                       New_Obj_Value (Val_Var),
                       New_Lit (New_Signed_Literal
                         (Val_Type,
                            Integer_64
                              (Get_Nbr_Elements
                                 (Get_Enumeration_Literal_List
                                      (Attr_Type))))),
                       Ghdl_Bool_Type)));
               Chap6.Gen_Bound_Error (Attr);
               Finish_If_Stmt (If_Blk);
               Val := New_Obj_Value (Val_Var);
            end;
         when others =>
            null;
      end case;

      New_Assign_Stmt (New_Obj (Res_Var), New_Convert_Ov (Val, Res_Type));
      Chap3.Check_Range
        (Res_Var, Attr, Get_Type (Get_Prefix (Attr)), Attr);
      return New_Obj_Value (Res_Var);
   end Translate_Val_Attribute;

   function Translate_Pos_Attribute (Attr : Iir; Res_Type : Iir)
                                        return O_Enode
   is
      T     : O_Dnode;
      Ttype : O_Tnode;
   begin
      Ttype := Get_Ortho_Type (Res_Type, Mode_Value);
      T := Create_Temp (Ttype);
      New_Assign_Stmt
        (New_Obj (T),
         New_Convert_Ov (Chap7.Translate_Expression (Get_Parameter (Attr)),
           Ttype));
      Chap3.Check_Range (T, Attr, Res_Type, Attr);
      return New_Obj_Value (T);
   end Translate_Pos_Attribute;

   function Translate_Succ_Pred_Attribute (Attr : Iir) return O_Enode
   is
      Expr_Type : constant Iir := Get_Type (Attr);
      Tinfo     : constant Type_Info_Acc := Get_Info (Expr_Type);
      Prefix_Type : constant Iir :=
        Get_Type (Get_Named_Entity (Get_Prefix (Attr)));
      Ttype     : O_Tnode;
      Expr      : O_Enode;
      Is_Inc    : Boolean;
      Op        : ON_Op_Kind;
   begin
      --  FIXME: should check bounds.
      Expr := Chap7.Translate_Expression (Get_Parameter (Attr), Expr_Type);
      Ttype := Tinfo.Ortho_Type (Mode_Value);
      case Get_Kind (Attr) is
         when Iir_Kind_Succ_Attribute =>
            Is_Inc := True;
         when Iir_Kind_Pred_Attribute =>
            Is_Inc := False;
         when Iir_Kind_Leftof_Attribute =>
            Is_Inc :=
              Get_Direction (Get_Range_Constraint (Prefix_Type)) = Iir_Downto;
         when Iir_Kind_Rightof_Attribute =>
            Is_Inc :=
              Get_Direction (Get_Range_Constraint (Prefix_Type)) = Iir_To;
         when others =>
            Error_Kind ("translate_succ_pred_attribute", Attr);
      end case;

      if Is_Inc then
         Op := ON_Add_Ov;
      else
         Op := ON_Sub_Ov;
      end if;
      case Tinfo.Type_Mode is
         when Type_Mode_B1
            | Type_Mode_E8
            | Type_Mode_E32 =>
            --  Should check it is not the last.
            declare
               List : constant Iir_List := Get_Enumeration_Literal_List
                 (Get_Base_Type (Expr_Type));
               Limit : Iir;
               L : O_Dnode;
            begin
               L := Create_Temp_Init (Ttype, Expr);
               if Is_Inc then
                  Limit := Get_Last_Element (List);
               else
                  Limit := Get_First_Element (List);
               end if;
               Chap6.Check_Bound_Error
                 (New_Compare_Op (ON_Eq,
                  New_Obj_Value (L),
                  New_Lit (Get_Ortho_Expr (Limit)),
                  Ghdl_Bool_Type),
                  Attr, 0);
               return New_Convert_Ov
                 (New_Dyadic_Op
                    (Op,
                     New_Convert_Ov (New_Obj_Value (L), Ghdl_I32_Type),
                     New_Lit (New_Signed_Literal (Ghdl_I32_Type, 1))),
                  Ttype);
            end;
         when Type_Mode_I32
            | Type_Mode_P64 =>
            return New_Dyadic_Op
              (Op, Expr, New_Lit (New_Signed_Literal (Ttype, 1)));
         when others =>
            raise Internal_Error;
      end case;
   end Translate_Succ_Pred_Attribute;

   type Bool_Sigattr_Data_Type is record
      Label : O_Snode;
      Field : O_Fnode;
   end record;

   procedure Bool_Sigattr_Non_Composite_Signal
     (Targ : Mnode; Targ_Type : Iir; Data : Bool_Sigattr_Data_Type)
   is
      pragma Unreferenced (Targ_Type);
   begin
      Gen_Exit_When (Data.Label,
                     New_Value (Get_Signal_Field (Targ, Data.Field)));
   end Bool_Sigattr_Non_Composite_Signal;

   function Bool_Sigattr_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Bool_Sigattr_Data_Type)
         return Bool_Sigattr_Data_Type
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Data;
   end Bool_Sigattr_Prepare_Data_Composite;

   function Bool_Sigattr_Update_Data_Array (Data      : Bool_Sigattr_Data_Type;
                                            Targ_Type : Iir;
                                            Index     : O_Dnode)
                                               return Bool_Sigattr_Data_Type
   is
      pragma Unreferenced (Targ_Type, Index);
   begin
      return Data;
   end Bool_Sigattr_Update_Data_Array;

   function Bool_Sigattr_Update_Data_Record
     (Data      : Bool_Sigattr_Data_Type;
      Targ_Type : Iir;
      El        : Iir_Element_Declaration)
      return Bool_Sigattr_Data_Type
   is
      pragma Unreferenced (Targ_Type, El);
   begin
      return Data;
   end Bool_Sigattr_Update_Data_Record;

   procedure Bool_Sigattr_Finish_Data_Composite
     (Data : in out Bool_Sigattr_Data_Type)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Bool_Sigattr_Finish_Data_Composite;

   procedure Bool_Sigattr_Foreach is new Foreach_Non_Composite
     (Data_Type => Bool_Sigattr_Data_Type,
      Composite_Data_Type => Bool_Sigattr_Data_Type,
      Do_Non_Composite => Bool_Sigattr_Non_Composite_Signal,
      Prepare_Data_Array => Bool_Sigattr_Prepare_Data_Composite,
      Update_Data_Array => Bool_Sigattr_Update_Data_Array,
      Finish_Data_Array => Bool_Sigattr_Finish_Data_Composite,
      Prepare_Data_Record => Bool_Sigattr_Prepare_Data_Composite,
      Update_Data_Record => Bool_Sigattr_Update_Data_Record,
      Finish_Data_Record => Bool_Sigattr_Finish_Data_Composite);

   function Translate_Bool_Signal_Attribute (Attr : Iir; Field : O_Fnode)
                                                return O_Enode
   is
      Data        : Bool_Sigattr_Data_Type;
      Res         : O_Dnode;
      Name        : Mnode;
      Prefix      : constant Iir := Get_Prefix (Attr);
      Prefix_Type : constant Iir := Get_Type (Prefix);
   begin
      if Get_Kind (Prefix_Type) in Iir_Kinds_Scalar_Type_Definition then
         --  Effecient handling for a scalar signal.
         Name := Chap6.Translate_Name (Prefix, Mode_Signal);
         return New_Value (Get_Signal_Field (Name, Field));
      else
         --  Element per element handling for composite signals.
         Res := Create_Temp (Std_Boolean_Type_Node);
         Open_Temp;
         New_Assign_Stmt (New_Obj (Res), New_Lit (Std_Boolean_True_Node));
         Name := Chap6.Translate_Name (Prefix, Mode_Signal);
         Start_Loop_Stmt (Data.Label);
         Data.Field := Field;
         Bool_Sigattr_Foreach (Name, Prefix_Type, Data);
         New_Assign_Stmt (New_Obj (Res), New_Lit (Std_Boolean_False_Node));
         New_Exit_Stmt (Data.Label);
         Finish_Loop_Stmt (Data.Label);
         Close_Temp;
         return New_Obj_Value (Res);
      end if;
   end Translate_Bool_Signal_Attribute;

   function Translate_Event_Attribute (Attr : Iir) return O_Enode is
   begin
      return Translate_Bool_Signal_Attribute
        (Attr, Ghdl_Signal_Event_Field);
   end Translate_Event_Attribute;

   function Translate_Active_Attribute (Attr : Iir) return O_Enode is
   begin
      return Translate_Bool_Signal_Attribute
        (Attr, Ghdl_Signal_Active_Field);
   end Translate_Active_Attribute;

   --  Read signal value FIELD of signal SIG.
   function Get_Signal_Value_Field
     (Sig : O_Enode; Sig_Type : Iir; Field : O_Fnode) return O_Lnode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Sig_Type);
      S_Type : constant O_Tnode := Tinfo.Ortho_Ptr_Type (Mode_Value);
      T      : O_Lnode;
   begin
      T := New_Access_Element (Sig);
      return New_Access_Element
        (New_Unchecked_Address (New_Selected_Element (T, Field), S_Type));
   end Get_Signal_Value_Field;

   function Get_Signal_Field (Sig : Mnode; Field : O_Fnode)
                                 return O_Lnode
   is
      S : O_Enode;
   begin
      S := New_Convert_Ov (New_Value (M2Lv (Sig)), Ghdl_Signal_Ptr);
      return New_Selected_Element (New_Access_Element (S), Field);
   end Get_Signal_Field;

   function Read_Last_Value (Sig : O_Enode; Sig_Type : Iir) return O_Enode
   is
   begin
      return New_Value (Get_Signal_Value_Field
                        (Sig, Sig_Type, Ghdl_Signal_Last_Value_Field));
   end Read_Last_Value;

   function Translate_Last_Value is new Chap7.Translate_Signal_Value
     (Read_Value => Read_Last_Value);

   function Translate_Last_Value_Attribute (Attr : Iir) return O_Enode
   is
      Prefix      : constant Iir := Get_Prefix (Attr);
      Prefix_Type : constant Iir := Get_Type (Prefix);
      Name        : Mnode;
   begin
      Name := Chap6.Translate_Name (Prefix, Mode_Signal);
      return Translate_Last_Value (M2E (Name), Prefix_Type);
   end Translate_Last_Value_Attribute;

   function Read_Last_Time (Sig : O_Enode; Field : O_Fnode) return O_Enode
   is
      T : O_Lnode;
   begin
      T := New_Access_Element (New_Convert_Ov (Sig, Ghdl_Signal_Ptr));
      return New_Value (New_Selected_Element (T, Field));
   end Read_Last_Time;

   type Last_Time_Data is record
      Var   : O_Dnode;
      Field : O_Fnode;
   end record;

   procedure Translate_Last_Time_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Last_Time_Data)
   is
      pragma Unreferenced (Targ_Type);
      Val    : O_Dnode;
      If_Blk : O_If_Block;
   begin
      Open_Temp;
      Val := Create_Temp_Init
        (Std_Time_Otype,
         Read_Last_Time (New_Value (M2Lv (Targ)), Data.Field));
      Start_If_Stmt (If_Blk,
                     New_Compare_Op (ON_Gt,
                       New_Obj_Value (Val),
                       New_Obj_Value (Data.Var),
                       Ghdl_Bool_Type));
      New_Assign_Stmt (New_Obj (Data.Var), New_Obj_Value (Val));
      Finish_If_Stmt (If_Blk);
      Close_Temp;
   end Translate_Last_Time_Non_Composite;

   function Last_Time_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Data : Last_Time_Data)
         return Last_Time_Data
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Data;
   end Last_Time_Prepare_Data_Composite;

   function Last_Time_Update_Data_Array (Data      : Last_Time_Data;
                                         Targ_Type : Iir;
                                         Index     : O_Dnode)
                                            return Last_Time_Data
   is
      pragma Unreferenced (Targ_Type, Index);
   begin
      return Data;
   end Last_Time_Update_Data_Array;

   function Last_Time_Update_Data_Record (Data      : Last_Time_Data;
                                          Targ_Type : Iir;
                                          El        : Iir_Element_Declaration)
                                             return Last_Time_Data
   is
      pragma Unreferenced (Targ_Type, El);
   begin
      return Data;
   end Last_Time_Update_Data_Record;

   procedure Last_Time_Finish_Data_Composite
     (Data : in out Last_Time_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Last_Time_Finish_Data_Composite;

   procedure Translate_Last_Time is new Foreach_Non_Composite
     (Data_Type => Last_Time_Data,
      Composite_Data_Type => Last_Time_Data,
      Do_Non_Composite => Translate_Last_Time_Non_Composite,
      Prepare_Data_Array => Last_Time_Prepare_Data_Composite,
      Update_Data_Array => Last_Time_Update_Data_Array,
      Finish_Data_Array => Last_Time_Finish_Data_Composite,
      Prepare_Data_Record => Last_Time_Prepare_Data_Composite,
      Update_Data_Record => Last_Time_Update_Data_Record,
      Finish_Data_Record => Last_Time_Finish_Data_Composite);

   function Translate_Last_Time_Attribute (Prefix : Iir; Field : O_Fnode)
                                              return O_Enode
   is
      Prefix_Type : constant Iir := Get_Type (Prefix);
      Name        : Mnode;
      Info        : Type_Info_Acc;
      Var         : O_Dnode;
      Data        : Last_Time_Data;
      Right_Bound : Iir_Int64;
      If_Blk      : O_If_Block;
   begin
      Name := Chap6.Translate_Name (Prefix, Mode_Signal);
      Info := Get_Info (Prefix_Type);
      Var := Create_Temp (Std_Time_Otype);

      if Info.Type_Mode in Type_Mode_Scalar then
         New_Assign_Stmt (New_Obj (Var),
                          Read_Last_Time (M2E (Name), Field));
      else
         --  Init with a negative value.
         New_Assign_Stmt
           (New_Obj (Var),
            New_Lit (New_Signed_Literal (Std_Time_Otype, -1)));
         Data := Last_Time_Data'(Var => Var, Field => Field);
         Translate_Last_Time (Name, Prefix_Type, Data);
      end if;

      Right_Bound := Get_Value
        (Get_Right_Limit (Get_Range_Constraint (Time_Subtype_Definition)));

      --  VAR < 0 ?
      Start_If_Stmt
        (If_Blk,
         New_Compare_Op (ON_Lt,
           New_Obj_Value (Var),
           New_Lit (New_Signed_Literal (Std_Time_Otype, 0)),
           Ghdl_Bool_Type));
      --  LRM 14.1 Predefined attributes
      --   [...]; otherwise, it returns TIME'HIGH.
      New_Assign_Stmt
        (New_Obj (Var),
         New_Lit (New_Signed_Literal
           (Std_Time_Otype, Integer_64 (Right_Bound))));
      New_Else_Stmt (If_Blk);
      --  Returns NOW - Var.
      New_Assign_Stmt (New_Obj (Var),
                       New_Dyadic_Op (ON_Sub_Ov,
                         New_Obj_Value (Ghdl_Now),
                         New_Obj_Value (Var)));
      Finish_If_Stmt (If_Blk);
      return New_Obj_Value (Var);
   end Translate_Last_Time_Attribute;

   --  Return TRUE if the scalar signal SIG is being driven.
   function Read_Driving_Attribute (Sig : O_Enode) return O_Enode
   is
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Ghdl_Signal_Driving);
      New_Association (Assoc, New_Convert_Ov (Sig, Ghdl_Signal_Ptr));
      return New_Function_Call (Assoc);
   end Read_Driving_Attribute;

   procedure Driving_Non_Composite_Signal
     (Targ : Mnode; Targ_Type : Iir; Label : O_Snode)
   is
      pragma Unreferenced (Targ_Type);
   begin
      Gen_Exit_When
        (Label,
         New_Monadic_Op
           (ON_Not, Read_Driving_Attribute (New_Value (M2Lv (Targ)))));
   end Driving_Non_Composite_Signal;

   function Driving_Prepare_Data_Composite
     (Targ : Mnode; Targ_Type : Iir; Label : O_Snode)
         return O_Snode
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Label;
   end Driving_Prepare_Data_Composite;

   function Driving_Update_Data_Array (Label     : O_Snode;
                                       Targ_Type : Iir;
                                       Index     : O_Dnode)
                                          return O_Snode
   is
      pragma Unreferenced (Targ_Type, Index);
   begin
      return Label;
   end Driving_Update_Data_Array;

   function Driving_Update_Data_Record (Label     : O_Snode;
                                        Targ_Type : Iir;
                                        El        : Iir_Element_Declaration)
                                           return O_Snode
   is
      pragma Unreferenced (Targ_Type, El);
   begin
      return Label;
   end Driving_Update_Data_Record;

   procedure Driving_Finish_Data_Composite (Label : in out O_Snode)
   is
      pragma Unreferenced (Label);
   begin
      null;
   end Driving_Finish_Data_Composite;

   procedure Driving_Foreach is new Foreach_Non_Composite
     (Data_Type => O_Snode,
      Composite_Data_Type => O_Snode,
      Do_Non_Composite => Driving_Non_Composite_Signal,
      Prepare_Data_Array => Driving_Prepare_Data_Composite,
      Update_Data_Array => Driving_Update_Data_Array,
      Finish_Data_Array => Driving_Finish_Data_Composite,
      Prepare_Data_Record => Driving_Prepare_Data_Composite,
      Update_Data_Record => Driving_Update_Data_Record,
      Finish_Data_Record => Driving_Finish_Data_Composite);

   function Translate_Driving_Attribute (Attr : Iir) return O_Enode
   is
      Label       : O_Snode;
      Res         : O_Dnode;
      Name        : Mnode;
      Prefix      : Iir;
      Prefix_Type : Iir;
   begin
      Prefix := Get_Prefix (Attr);
      Prefix_Type := Get_Type (Prefix);

      if Get_Kind (Prefix_Type) in Iir_Kinds_Scalar_Type_Definition then
         --  Effecient handling for a scalar signal.
         Name := Chap6.Translate_Name (Prefix, Mode_Signal);
         return Read_Driving_Attribute (New_Value (M2Lv (Name)));
      else
         --  Element per element handling for composite signals.
         Res := Create_Temp (Std_Boolean_Type_Node);
         Open_Temp;
         New_Assign_Stmt (New_Obj (Res), New_Lit (Std_Boolean_False_Node));
         Name := Chap6.Translate_Name (Prefix, Mode_Signal);
         Start_Loop_Stmt (Label);
         Driving_Foreach (Name, Prefix_Type, Label);
         New_Assign_Stmt (New_Obj (Res), New_Lit (Std_Boolean_True_Node));
         New_Exit_Stmt (Label);
         Finish_Loop_Stmt (Label);
         Close_Temp;
         return New_Obj_Value (Res);
      end if;
   end Translate_Driving_Attribute;

   function Read_Driving_Value (Sig : O_Enode; Sig_Type : Iir)
                                   return O_Enode
   is
      Tinfo  : Type_Info_Acc;
      Subprg : O_Dnode;
      Assoc  : O_Assoc_List;
   begin
      Tinfo := Get_Info (Sig_Type);
      case Tinfo.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Signal_Driving_Value_B1;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Signal_Driving_Value_E8;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Signal_Driving_Value_E32;
         when Type_Mode_I32
            | Type_Mode_P32 =>
            Subprg := Ghdl_Signal_Driving_Value_I32;
         when Type_Mode_P64
            | Type_Mode_I64 =>
            Subprg := Ghdl_Signal_Driving_Value_I64;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Signal_Driving_Value_F64;
         when others =>
            raise Internal_Error;
      end case;
      Start_Association (Assoc, Subprg);
      New_Association (Assoc, New_Convert_Ov (Sig, Ghdl_Signal_Ptr));
      return New_Convert_Ov (New_Function_Call (Assoc),
                             Tinfo.Ortho_Type (Mode_Value));
   end Read_Driving_Value;

   function Translate_Driving_Value is new Chap7.Translate_Signal_Value
     (Read_Value => Read_Driving_Value);

   function Translate_Driving_Value_Attribute (Attr : Iir) return O_Enode
   is
      Prefix      : constant Iir := Get_Prefix (Attr);
      Name        : Mnode;
   begin
      Name := Chap6.Translate_Name (Prefix, Mode_Signal);
      return Translate_Driving_Value (M2E (Name), Get_Type (Prefix));
   end Translate_Driving_Value_Attribute;

   function Translate_Image_Attribute (Attr : Iir) return O_Enode
   is
      Prefix_Type : constant Iir :=
        Get_Base_Type (Get_Type (Get_Prefix (Attr)));
      Pinfo       : constant Type_Info_Acc := Get_Info (Prefix_Type);
      Res         : O_Dnode;
      Subprg      : O_Dnode;
      Assoc       : O_Assoc_List;
      Conv        : O_Tnode;
   begin
      Res := Create_Temp (Std_String_Node);
      Create_Temp_Stack2_Mark;
      case Pinfo.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Image_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Image_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Image_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32 =>
            Subprg := Ghdl_Image_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P32 =>
            Subprg := Ghdl_Image_P32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64 =>
            Subprg := Ghdl_Image_P64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Image_F64;
            Conv := Ghdl_Real_Type;
         when others =>
            raise Internal_Error;
      end case;
      Start_Association (Assoc, Subprg);
      New_Association (Assoc,
                       New_Address (New_Obj (Res), Std_String_Ptr_Node));
      New_Association
        (Assoc,
         New_Convert_Ov
           (Chap7.Translate_Expression (Get_Parameter (Attr), Prefix_Type),
            Conv));
      case Pinfo.Type_Mode is
         when Type_Mode_B1
            | Type_Mode_E8
            | Type_Mode_E32
            | Type_Mode_P32
            | Type_Mode_P64 =>
            New_Association
              (Assoc, New_Lit (Rtis.New_Rti_Address (Pinfo.Type_Rti)));
         when Type_Mode_I32
            | Type_Mode_F64 =>
            null;
         when others =>
            raise Internal_Error;
      end case;
      New_Procedure_Call (Assoc);
      return New_Address (New_Obj (Res), Std_String_Ptr_Node);
   end Translate_Image_Attribute;

   function Translate_Value_Attribute (Attr : Iir) return O_Enode
   is
      Prefix_Type : constant Iir :=
        Get_Base_Type (Get_Type (Get_Prefix (Attr)));
      Pinfo       : constant Type_Info_Acc := Get_Info (Prefix_Type);
      Subprg      : O_Dnode;
      Assoc       : O_Assoc_List;
   begin
      case Pinfo.Type_Mode is
         when Type_Mode_B1 =>
            Subprg := Ghdl_Value_B1;
         when Type_Mode_E8 =>
            Subprg := Ghdl_Value_E8;
         when Type_Mode_E32 =>
            Subprg := Ghdl_Value_E32;
         when Type_Mode_I32 =>
            Subprg := Ghdl_Value_I32;
         when Type_Mode_P32 =>
            Subprg := Ghdl_Value_P32;
         when Type_Mode_P64 =>
            Subprg := Ghdl_Value_P64;
         when Type_Mode_F64 =>
            Subprg := Ghdl_Value_F64;
         when others =>
            raise Internal_Error;
      end case;
      Start_Association (Assoc, Subprg);
      New_Association
        (Assoc,
         Chap7.Translate_Expression (Get_Parameter (Attr),
           String_Type_Definition));
      case Pinfo.Type_Mode is
         when Type_Mode_B1
            | Type_Mode_E8
            | Type_Mode_E32
            | Type_Mode_P32
            | Type_Mode_P64 =>
            New_Association
              (Assoc, New_Lit (Rtis.New_Rti_Address (Pinfo.Type_Rti)));
         when Type_Mode_I32
            | Type_Mode_F64 =>
            null;
         when others =>
            raise Internal_Error;
      end case;
      return New_Convert_Ov (New_Function_Call (Assoc),
                             Pinfo.Ortho_Type (Mode_Value));
   end Translate_Value_Attribute;

   function Translate_Path_Instance_Name_Attribute (Attr : Iir)
                                                       return O_Enode
   is
      Name        : constant Path_Instance_Name_Type :=
        Get_Path_Instance_Name_Suffix (Attr);
      Res         : O_Dnode;
      Name_Cst    : O_Dnode;
      Str_Cst     : O_Cnode;
      Constr      : O_Assoc_List;
      Is_Instance : constant Boolean :=
        Get_Kind (Attr) = Iir_Kind_Instance_Name_Attribute;
   begin
      Create_Temp_Stack2_Mark;

      Res := Create_Temp (Std_String_Node);
      Str_Cst := Create_String_Len (Name.Suffix, Create_Uniq_Identifier);
      New_Const_Decl (Name_Cst, Create_Uniq_Identifier, O_Storage_Private,
                      Ghdl_Str_Len_Type_Node);
      Start_Init_Value (Name_Cst);
      Finish_Init_Value (Name_Cst, Str_Cst);
      if Is_Instance then
         Start_Association (Constr, Ghdl_Get_Instance_Name);
      else
         Start_Association (Constr, Ghdl_Get_Path_Name);
      end if;
      New_Association
        (Constr, New_Address (New_Obj (Res), Std_String_Ptr_Node));
      if Name.Path_Instance = Null_Iir then
         Rtis.Associate_Null_Rti_Context (Constr);
      else
         Rtis.Associate_Rti_Context (Constr, Name.Path_Instance);
      end if;
      New_Association (Constr,
                       New_Address (New_Obj (Name_Cst),
                         Ghdl_Str_Len_Ptr_Node));
      New_Procedure_Call (Constr);
      return New_Address (New_Obj (Res), Std_String_Ptr_Node);
   end Translate_Path_Instance_Name_Attribute;
end Trans.Chap14;
