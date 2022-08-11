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

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap9;
with Trans_Decls; use Trans_Decls;
with Trans.Helpers2; use Trans.Helpers2;
with Trans.Foreach_Non_Composite;

package body Trans.Chap5 is
   use Trans.Helpers;

   procedure Save_Map_Env (Env : out Map_Env; Scope_Ptr : Var_Scope_Acc) is
   begin
      Env := (Scope_Ptr => Scope_Ptr,
              Scope => Scope_Ptr.all);
   end Save_Map_Env;

   procedure Restore_Map_Env (Env : Map_Env)
   is
      --  Avoid potential compiler bug with discriminant check.
      pragma Suppress (Discriminant_Check);
   begin
      Env.Scope_Ptr.all := Env.Scope;
   end Restore_Map_Env;

   procedure Set_Map_Env (Env : Map_Env) is
   begin
      --  In some cases, the previous environment is still needed (for example
      --  an implicit array conversion accesses to both the formal and the
      --  actual type).  Set ENV only if it is not null, in order not to erase
      --  the previous env.
      if not Is_Null (Env.Scope) then
         Restore_Map_Env (Env);
      end if;
   end Set_Map_Env;

   procedure Translate_Attribute_Specification
     (Spec : Iir_Attribute_Specification)
   is
      Spec_Expr : constant Iir := Get_Expression (Spec);
      Spec_Type : constant Iir := Get_Type (Spec_Expr);
      Attr      : constant Iir_Attribute_Declaration :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
      Mark      : Id_Mark_Type;
      Mark2     : Id_Mark_Type;
      Info      : Object_Info_Acc;
      Val       : Iir;
      Num       : Natural;
      Vtype     : O_Tnode;
   begin
      Push_Identifier_Prefix_Uniq (Mark);
      if Is_Anonymous_Type_Definition (Spec_Type) then
         Push_Identifier_Prefix (Mark2, "OT");
         Chap3.Translate_Subtype_Definition (Spec_Type, True);
         Pop_Identifier_Prefix (Mark2);
      end if;

      Num := 1;
      Val := Get_Attribute_Value_Spec_Chain (Spec);
      while Is_Valid (Val) loop
         Info := Add_Info (Val, Kind_Object);
         Vtype := Chap4.Get_Object_Type (Get_Info (Spec_Type), Mode_Value);
         if Get_Static_Attribute_Flag (Spec)
           and then Get_Expr_Staticness (Spec_Expr) = Locally
         then
            --  Create a global var so that the attribute can be referenced
            --  from outside.  This is possible only if the attribute is
            --  locally static.
            Info.Object_Var := Create_Global_Var
              (Create_Identifier (Attr, "V"), Vtype, Global_Storage);
         else
            Info.Object_Var := Create_Var
              (Create_Var_Identifier (Attr, "V", Num), Vtype, Global_Storage);
         end if;

         --  Create only one object if the expression is static.
         exit when Get_Expr_Staticness (Spec_Expr) /= None;

         Val := Get_Spec_Chain (Val);
         Num := Num + 1;
      end loop;
      Pop_Identifier_Prefix (Mark);
   end Translate_Attribute_Specification;

   procedure Elab_Attribute_Specification
     (Spec : Iir_Attribute_Specification)
   is
      Expr : constant Iir := Get_Expression (Spec);
      Val    : Iir;
   begin
      Val := Get_Attribute_Value_Spec_Chain (Spec);
      while Is_Valid (Val) loop
         Chap4.Elab_Object_Value (Val, Expr);
         exit when Get_Expr_Staticness (Expr) /= None;
         Val := Get_Spec_Chain (Val);
      end loop;
   end Elab_Attribute_Specification;

   procedure Gen_Elab_Disconnect_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Time : O_Dnode)
   is
      pragma Unreferenced (Targ_Type);
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Ghdl_Signal_Set_Disconnect);
      New_Association
        (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      New_Association (Assoc, New_Obj_Value (Time));
      New_Procedure_Call (Assoc);
   end Gen_Elab_Disconnect_Non_Composite;

   function Gen_Elab_Disconnect_Prepare
     (Targ : Mnode; Targ_Type : Iir; Time : O_Dnode) return O_Dnode
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Time;
   end Gen_Elab_Disconnect_Prepare;

   function Gen_Elab_Disconnect_Update_Data_Array
     (Time : O_Dnode; Targ_Type : Iir; Index : O_Dnode) return O_Dnode
   is
      pragma Unreferenced (Targ_Type, Index);
   begin
      return Time;
   end Gen_Elab_Disconnect_Update_Data_Array;

   function Gen_Elab_Disconnect_Update_Data_Record
     (Time : O_Dnode; Targ_Type : Iir; El : Iir_Element_Declaration)
     return O_Dnode
   is
      pragma Unreferenced (Targ_Type, El);
   begin
      return Time;
   end Gen_Elab_Disconnect_Update_Data_Record;

   procedure Gen_Elab_Disconnect is new Foreach_Non_Composite
     (Data_Type => O_Dnode,
      Composite_Data_Type => O_Dnode,
      Do_Non_Composite => Gen_Elab_Disconnect_Non_Composite,
      Prepare_Data_Array => Gen_Elab_Disconnect_Prepare,
      Update_Data_Array => Gen_Elab_Disconnect_Update_Data_Array,
      Prepare_Data_Record => Gen_Elab_Disconnect_Prepare,
      Update_Data_Record => Gen_Elab_Disconnect_Update_Data_Record);

   procedure Elab_Disconnection_Specification
     (Spec : Iir_Disconnection_Specification)
   is
      Val  : O_Dnode;
      List : constant Iir_Flist := Get_Signal_List (Spec);
      El   : Iir;
   begin
      Val := Create_Temp_Init
        (Std_Time_Otype,
         Chap7.Translate_Expression (Get_Expression (Spec)));
      for I in Flist_First .. Flist_Last (List) loop
         El := Get_Nth_Element (List, I);
         Gen_Elab_Disconnect (Chap6.Translate_Name (El, Mode_Signal),
                              Get_Type (El), Val);
      end loop;
   end Elab_Disconnection_Specification;

   type Connect_Mode is
     (
      --  Actual is a source for the formal.
      Connect_Source,

      --  Both.
      Connect_Both,

      --  Effective value of actual is the effective value of the formal.
      Connect_Effective,

      --  Actual is a value.
      Connect_Value
     );

   type Connect_Data is record
      Actual_Sig  : Mnode;
      Actual_Type : Iir;

      --  Mode of the connection.
      Mode : Connect_Mode;

      --  If true, formal signal is a copy of the actual.
      By_Copy : Boolean;
   end record;

   --  Connect_effective: FORMAL is set from ACTUAL.
   --  Connect_Source: ACTUAL is set from FORMAL (source of ACTUAL).
   procedure Connect_Scalar
     (Formal_Sig : Mnode; Formal_Type : Iir; Data : Connect_Data)
   is
      Act_Node, Form_Node : Mnode;
   begin
      if Data.By_Copy then
         New_Assign_Stmt (M2Lv (Formal_Sig), M2E (Data.Actual_Sig));
         return;
      end if;

      case Data.Mode is
         when Connect_Both =>
            Open_Temp;
            Act_Node := Stabilize (Data.Actual_Sig, True);
            Form_Node := Stabilize (Formal_Sig, True);
         when Connect_Source
            | Connect_Effective =>
            Act_Node := Data.Actual_Sig;
            Form_Node := Formal_Sig;
         when Connect_Value =>
            null;
      end case;

      if Data.Mode in Connect_Source .. Connect_Both then
         --  Formal is a source to actual.
         declare
            Constr : O_Assoc_List;
         begin
            Start_Association (Constr, Ghdl_Signal_Add_Source);
            New_Association (Constr, New_Convert_Ov (M2E (Act_Node),
                             Ghdl_Signal_Ptr));
            New_Association (Constr, New_Convert_Ov (M2E (Form_Node),
                             Ghdl_Signal_Ptr));
            New_Procedure_Call (Constr);
         end;
      end if;

      if Data.Mode in Connect_Both .. Connect_Effective then
         --  The effective value of formal is the effective value of actual.
         declare
            Constr : O_Assoc_List;
         begin
            Start_Association (Constr, Ghdl_Signal_Effective_Value);
            New_Association (Constr, New_Convert_Ov (M2E (Form_Node),
                             Ghdl_Signal_Ptr));
            New_Association (Constr, New_Convert_Ov (M2E (Act_Node),
                             Ghdl_Signal_Ptr));
            New_Procedure_Call (Constr);
         end;
      end if;

      if Data.Mode = Connect_Value then
         declare
            Type_Info : constant Type_Info_Acc := Get_Info (Formal_Type);
            Subprg    : O_Dnode;
            Constr    : O_Assoc_List;
            Conv      : O_Tnode;
         begin
            case Type_Info.Type_Mode is
               when Type_Mode_B1 =>
                  Subprg := Ghdl_Signal_Associate_B1;
                  Conv := Ghdl_Bool_Type;
               when Type_Mode_E8 =>
                  Subprg := Ghdl_Signal_Associate_E8;
                  Conv := Ghdl_I32_Type;
               when Type_Mode_E32 =>
                  Subprg := Ghdl_Signal_Associate_E32;
                  Conv := Ghdl_I32_Type;
               when Type_Mode_I32
                 | Type_Mode_P32 =>
                  Subprg := Ghdl_Signal_Associate_I32;
                  Conv := Ghdl_I32_Type;
               when Type_Mode_P64 =>
                  Subprg := Ghdl_Signal_Associate_I64;
                  Conv := Ghdl_I64_Type;
               when Type_Mode_F64 =>
                  Subprg := Ghdl_Signal_Associate_F64;
                  Conv := Ghdl_Real_Type;
               when others =>
                  Error_Kind ("connect_scalar", Formal_Type);
            end case;
            Start_Association (Constr, Subprg);
            New_Association (Constr,
                             New_Convert_Ov (New_Value (M2Lv (Formal_Sig)),
                                             Ghdl_Signal_Ptr));
            New_Association (Constr,
                             New_Convert_Ov (M2E (Data.Actual_Sig), Conv));
            New_Procedure_Call (Constr);
         end;
      end if;

      if Data.Mode = Connect_Both then
         Close_Temp;
      end if;
   end Connect_Scalar;

   function Connect_Prepare_Data_Composite
     (Targ : Mnode; Formal_Type : Iir; Data : Connect_Data)
     return Connect_Data
   is
      pragma Unreferenced (Targ, Formal_Type);
      Res   : Connect_Data;
      Atype : constant Iir := Get_Base_Type (Data.Actual_Type);
   begin
      if Get_Kind (Atype) = Iir_Kind_Record_Type_Definition then
         Res := Data;
         Stabilize (Res.Actual_Sig);
         return Res;
      else
         return Data;
      end if;
   end Connect_Prepare_Data_Composite;

   function Connect_Update_Data_Array
     (Data : Connect_Data; Formal_Type : Iir; Index : O_Dnode)
     return Connect_Data
   is
      pragma Unreferenced (Formal_Type);
      Res : Connect_Data;
   begin
      --  FIXME: should check matching elements!
      Res := (Actual_Sig => Chap6.Translate_Indexed_Name_By_Offset
                (Data.Actual_Sig, Data.Actual_Type, Index),
              Actual_Type => Get_Element_Subtype (Data.Actual_Type),
              Mode => Data.Mode,
              By_Copy => Data.By_Copy);
      return Res;
   end Connect_Update_Data_Array;

   function Connect_Update_Data_Record
     (Data : Connect_Data; Formal_Type : Iir; El : Iir_Element_Declaration)
     return Connect_Data
   is
      pragma Unreferenced (Formal_Type);
      Res : Connect_Data;
      Fel : Iir;
   begin
      Fel := Get_Nth_Element
        (Get_Elements_Declaration_List (Data.Actual_Type),
         Natural (Get_Element_Position (El)));
      Res := (Actual_Sig =>
                Chap6.Translate_Selected_Element (Data.Actual_Sig, Fel),
              Actual_Type => Get_Type (Fel),
              Mode => Data.Mode,
              By_Copy => Data.By_Copy);
      return Res;
   end Connect_Update_Data_Record;

   procedure Connect is new Foreach_Non_Composite
     (Data_Type => Connect_Data,
      Composite_Data_Type => Connect_Data,
      Do_Non_Composite => Connect_Scalar,
      Prepare_Data_Array => Connect_Prepare_Data_Composite,
      Update_Data_Array => Connect_Update_Data_Array,
      Prepare_Data_Record => Connect_Prepare_Data_Composite,
      Update_Data_Record => Connect_Update_Data_Record);

   procedure Elab_Port_Map_Aspect_Assoc (Assoc : Iir;
                                         Formal : Iir;
                                         By_Copy : Boolean;
                                         Formal_Env : Map_Env;
                                         Actual_Env : Map_Env)
   is
      Actual      : constant Iir := Strip_Reference_Name (Get_Actual (Assoc));
      Formal_Type : constant Iir := Get_Type (Formal);
      Actual_Type : constant Iir := Get_Type (Actual);
      Port        : constant Iir := Get_Interface_Of_Formal (Formal);
      Formal_Tinfo : Type_Info_Acc;
      Formal_Sig  : Mnode;
      Formal_Val  : Mnode;
      Actual_Sig  : Mnode;
      Actual_Val  : Mnode;
      Init_Node   : Mnode;
      Actual_En   : O_Enode;
      Data        : Connect_Data;
      Mode        : Connect_Mode;
   begin
      pragma Assert
        (Get_Kind (Assoc) in Iir_Kinds_Association_Element_By_Actual);

      Open_Temp;
      if Get_Actual_Conversion (Assoc) = Null_Iir
        and then Get_Formal_Conversion (Assoc) = Null_Iir
      then
         --  Usual case: without conversions.
         if Is_Signal_Name (Actual) then
            --  LRM93 4.3.1.2
            --  For a signal of a scalar type, each source is either
            --  a driver or an OUT, INOUT, BUFFER or LINKAGE port of
            --  a component instance or of a block statement with
            --  which the signals associated.

            --  LRM93 12.6.2
            --  For a scalar signal S, the effective value of S is
            --  determined in the following manner:
            --  *  If S is [...] a port of mode BUFFER or [...],
            --     then the effective value of S is the same as
            --     the driving value of S.
            --  *  If S is a connected port of mode IN or INOUT,
            --     then the effective value of S is the same as
            --     the effective value of the actual part of the
            --     association element that associates an actual
            --     with S.
            --  *  [...]
            case Get_Mode (Port) is
               when Iir_In_Mode =>
                  Mode := Connect_Effective;
               when Iir_Inout_Mode =>
                  Mode := Connect_Both;
               when Iir_Out_Mode
                  | Iir_Buffer_Mode
                  | Iir_Linkage_Mode =>
                  Mode := Connect_Source;
               when Iir_Unknown_Mode =>
                  raise Internal_Error;
            end case;

            if By_Copy then
               Set_Map_Env (Actual_Env);
               Chap6.Translate_Signal_Name (Actual, Actual_Sig, Actual_Val);
               Set_Map_Env (Formal_Env);
               Chap6.Translate_Signal_Name (Formal, Formal_Sig, Formal_Val);

               --  Copy pointer to the values.
               Formal_Tinfo := Get_Info (Formal_Type);
               --  Need to convert base, as you can assign a bounded type
               --  to an unbounded type (or the opposite).  Maybe convert
               --  only when needed ?  Subtype matching is checked below.
               case Formal_Tinfo.Type_Mode is
                  when Type_Mode_Unbounded_Array
                     | Type_Mode_Unbounded_Record =>
                     New_Assign_Stmt
                       (M2Lp (Chap3.Get_Composite_Base (Formal_Val)),
                        New_Convert_Ov
                          (M2Addr (Chap3.Get_Composite_Base (Actual_Val)),
                           Formal_Tinfo.B.Base_Ptr_Type (Mode_Value)));
                  when Type_Mode_Bounded_Arrays
                     | Type_Mode_Bounded_Records =>
                     New_Assign_Stmt
                       (M2Lp (Formal_Val),
                        New_Convert_Ov
                          (M2Addr (Chap3.Get_Composite_Base (Actual_Val)),
                           Formal_Tinfo.Ortho_Ptr_Type (Mode_Value)));
                  when others =>
                     New_Assign_Stmt
                       (M2Lp (Formal_Val), M2Addr (Actual_Val));
               end case;
            else
               Set_Map_Env (Actual_Env);
               Actual_Sig := Chap6.Translate_Name (Actual, Mode_Signal);
               Set_Map_Env (Formal_Env);
               Formal_Sig := Chap6.Translate_Name (Formal, Mode_Signal);
            end if;

         else
            --  Association by value.  The formal cannot be referenced in the
            --  actual, but the type of the formal may be used by the actual.
            Set_Map_Env (Formal_Env);
            Chap6.Translate_Signal_Name (Formal, Formal_Sig, Formal_Val);
            Actual_En := Chap7.Translate_Expression (Actual, Formal_Type);
            Actual_Sig := E2M (Actual_En, Get_Info (Formal_Type), Mode_Value);
            Mode := Connect_Value;
--            raise Internal_Error;
         end if;

         if Get_Kind (Formal_Type) in Iir_Kinds_Array_Type_Definition then
            --  Check length matches.
            --  FIXME: records ?
            Stabilize (Formal_Sig);
            Stabilize (Actual_Sig);
            Chap3.Check_Composite_Match (Formal_Type, Formal_Sig,
                                         Actual_Type, Actual_Sig,
                                         Assoc);
         end if;

         Data := (Actual_Sig => Actual_Sig,
                  Actual_Type => Actual_Type,
                  Mode => Mode,
                  By_Copy => By_Copy);
         Connect (Formal_Sig, Formal_Type, Data);

         --  Set driving value
         if By_Copy
           and then (Mode = Connect_Both or Mode = Connect_Source)
         then
            Formal_Sig := Chap6.Translate_Name (Formal, Mode_Signal);

            if Is_Valid (Get_Default_Value (Port)) then
               Init_Node := Chap6.Get_Port_Init_Value (Formal);
            else
               Init_Node := Mnode_Null;
            end if;
            Chap9.Gen_Port_Init_Driving
              (Formal_Sig, Formal_Type, Init_Node);
         end if;
      else
         if Get_Actual_Conversion (Assoc) /= Null_Iir then
            Chap4.Elab_In_Conversion (Assoc, Formal, Actual_Sig);
            Set_Map_Env (Formal_Env);
            Formal_Sig := Chap6.Translate_Name (Formal, Mode_Signal);
            Data := (Actual_Sig => Actual_Sig,
                     Actual_Type => Formal_Type,
                     Mode => Connect_Effective,
                     By_Copy => False);
            Connect (Formal_Sig, Formal_Type, Data);
            Set_Map_Env (Actual_Env);
         end if;
         if Get_Formal_Conversion (Assoc) /= Null_Iir then
            --  flow: FORMAL to ACTUAL
            Chap4.Elab_Out_Conversion (Assoc, Formal, Formal_Sig);
            Set_Map_Env (Actual_Env);
            Actual_Sig := Chap6.Translate_Name (Actual, Mode_Signal);
            Data := (Actual_Sig => Actual_Sig,
                     Actual_Type => Actual_Type,
                     Mode => Connect_Source,
                     By_Copy => False);
            Set_Map_Env (Formal_Env);
            Connect (Formal_Sig, Actual_Type, Data);
         end if;
      end if;

      Close_Temp;
   end Elab_Port_Map_Aspect_Assoc;

   procedure Elab_Port_Map_Aspect_Assoc_Or_Inertial (Assoc : Iir;
                                                     Formal : Iir;
                                                     Formal_Env : Map_Env;
                                                     Actual_Env : Map_Env) is
   begin
      if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression
        and then Get_Inertial_Flag (Assoc)
      then
         Chap9.Elab_Inertial_Association (Assoc, Formal);
      else
         Elab_Port_Map_Aspect_Assoc
           (Assoc, Formal, False, Formal_Env, Actual_Env);
      end if;
   end Elab_Port_Map_Aspect_Assoc_Or_Inertial;

   function Alloc_Bounds (Atype : Iir; Alloc : Allocation_Kind) return Mnode
   is
      Tinfo : constant Type_Info_Acc := Get_Info (Atype);
      Var : O_Dnode;
   begin
      Var := Create_Temp (Tinfo.B.Bounds_Ptr_Type);
      New_Assign_Stmt
        (New_Obj (Var),
         Gen_Alloc (Alloc,
                    New_Lit (New_Sizeof (Tinfo.B.Bounds_Type,
                                         Ghdl_Index_Type)),
                    Tinfo.B.Bounds_Ptr_Type));
      return Dp2M (Var, Tinfo, Mode_Value,
                   Tinfo.B.Bounds_Type,
                   Tinfo.B.Bounds_Ptr_Type);
   end Alloc_Bounds;

   function Get_Unconstrained_Port_Bounds (Assoc : Iir; Inter : Iir)
                                          return Mnode
   is
      Actual : constant Iir := Get_Actual (Assoc);
      Actual_Type : constant Iir := Get_Type (Actual);
      In_Conv : constant Iir := Get_Actual_Conversion (Assoc);
      Out_Conv : constant Iir := Get_Formal_Conversion (Assoc);

      function Get_Actual_Bounds (Save : Boolean) return Mnode
      is
         Tinfo       : Type_Info_Acc;
         Bounds : Mnode;
         Bounds_Copy : Mnode;
      begin
         if Is_Fully_Constrained_Type (Actual_Type) then
            Chap3.Create_Composite_Subtype (Actual_Type);
            Bounds := Chap3.Get_Composite_Type_Bounds (Actual_Type);
            Tinfo := Get_Info (Actual_Type);
            if Save
              and then
                Chap3.Get_Composite_Type_Layout_Alloc (Tinfo) = Alloc_Stack
            then
               --  We need a copy.
               Bounds_Copy := Alloc_Bounds (Actual_Type, Alloc_System);
               Chap3.Copy_Bounds (Bounds_Copy, Bounds, Actual_Type);
               return Bounds_Copy;
            else
               return Bounds;
            end if;
         else
            --  Actual type is unconstrained, but as this is an object reads
            --  bounds from the object.
            return Chap3.Get_Composite_Bounds
              (Chap6.Translate_Name (Actual, Mode_Value));
         end if;
      end Get_Actual_Bounds;

      In_Conv_Type : Iir;
      Param_Type : Iir;
      Res_Type : Iir;
      Bounds : Mnode;
      Can_Convert : Boolean;
      Res : Mnode;
   begin
      if In_Conv = Null_Iir and then Out_Conv = Null_Iir then
         --  The easy and usual case.  Get bounds from the actual.
         return Get_Actual_Bounds (True);
      end if;

      Can_Convert := False;
      if In_Conv /= Null_Iir then
         In_Conv_Type := Get_Type (In_Conv);
         if Is_Fully_Constrained_Type (In_Conv_Type) then
            --  The 'in' conversion gives the type.
            return Chap3.Get_Composite_Type_Bounds (In_Conv_Type);
         elsif Get_Kind (In_Conv) = Iir_Kind_Type_Conversion then
            --  Convert bounds of the actual.
            Can_Convert := True;
         else
            pragma Assert (Get_Kind (In_Conv) = Iir_Kind_Function_Call);
            --  Cannot use anything from the in conversion.
            null;
         end if;
      end if;
      if Out_Conv /= Null_Iir then
         if Get_Kind (Out_Conv) = Iir_Kind_Function_Call then
            Param_Type := Get_Type (Get_Interface_Declaration_Chain
                                      (Get_Implementation (Out_Conv)));
            if Is_Fully_Constrained_Type (Param_Type) then
               return Chap3.Get_Composite_Type_Bounds (Param_Type);
            else
               pragma Assert (Can_Convert);
               null;
            end if;
         else
            pragma Assert (Get_Kind (Out_Conv) = Iir_Kind_Type_Conversion);
            --  Automatically convert actual type to the formal type.
            Can_Convert := True;
         end if;
      end if;

      pragma Assert (Can_Convert);
      Res_Type := Get_Type (Get_Association_Interface (Assoc, Inter));
      Bounds := Get_Actual_Bounds (False);
      Res := Alloc_Bounds (Res_Type, Alloc_System);
      Chap7.Translate_Type_Conversion_Bounds
        (Res, Bounds, Res_Type, Actual_Type, Assoc);
      return Res;
   end Get_Unconstrained_Port_Bounds;

   --  Set bounds for PORT.
   procedure Elab_Unconstrained_Port_Bounds (Port : Iir; Assoc : Iir)
   is
      Info : Signal_Info_Acc;
      Bounds : Mnode;
      Act_Node : Mnode;
   begin
      Open_Temp;
      case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
         when Iir_Kind_Association_Element_By_Expression
            | Iir_Kind_Association_Element_By_Name =>
            pragma Assert (Get_Whole_Association_Flag (Assoc));
            Bounds := Get_Unconstrained_Port_Bounds (Assoc, Port);
         when Iir_Kind_Association_Element_Open =>
            declare
               Actual_Type : constant Iir :=
                 Get_Type (Get_Default_Value (Port));
            begin
               Chap3.Create_Composite_Subtype (Actual_Type);
               Bounds := Chap3.Get_Composite_Type_Bounds (Actual_Type);
            end;
         when Iir_Kind_Association_Element_By_Individual =>
            declare
               Actual_Type : constant Iir := Get_Actual_Type (Assoc);
            begin
               Chap3.Translate_Anonymous_Subtype_Definition
                 (Actual_Type, False);
               Chap3.Create_Composite_Subtype (Actual_Type);
               Bounds := Chap3.Get_Composite_Type_Bounds (Actual_Type);
            end;
      end case;

      Stabilize (Bounds);
      for K in Object_Kind_Type loop
         Act_Node := Chap6.Translate_Name (Port, K);
         --  Note: this works only because it is not stabilized, and
         --  therefore the bounds field is returned and not a pointer to
         --  the bounds.
         New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds (Act_Node)),
                          M2Addr (Bounds));
      end loop;

      --  Set bounds of init value (if present)
      Info := Get_Info (Port);
      if Info.Signal_Val /= Null_Var then
         New_Assign_Stmt (M2Lp (Chap3.Get_Composite_Bounds
                                  (Chap6.Get_Port_Init_Value (Port))),
                          M2Addr (Bounds));
      end if;
      Close_Temp;
   end Elab_Unconstrained_Port_Bounds;

   procedure Elab_Port_Map_Aspect
     (Header : Iir; Map : Iir; Block_Parent : Iir; Formal_Env : Map_Env)
   is
      Actual_Env : Map_Env;
      Assoc : Iir;
      Inter : Iir;
      Value : Iir;
   begin
      Save_Map_Env (Actual_Env, Formal_Env.Scope_Ptr);

      --  Ports.
      Assoc := Get_Port_Map_Aspect_Chain (Map);
      Inter := Get_Port_Chain (Header);
      while Assoc /= Null_Iir loop
         declare
            Formal : constant Iir := Get_Association_Formal (Assoc, Inter);
            Formal_Base : constant Iir := Get_Interface_Of_Formal (Formal);
            Fb_Type : constant Iir := Get_Type (Formal_Base);
            Fbt_Info : constant Type_Info_Acc := Get_Info (Fb_Type);
         begin
            Set_Map_Env (Formal_Env);

            --  Set bounds of unbounded ports.
            if Get_Whole_Association_Flag (Assoc)
              and then Fbt_Info.Type_Mode in Type_Mode_Unbounded
            then
               Elab_Unconstrained_Port_Bounds (Formal, Assoc);
            end if;

            --  Allocate storage of ports.
            --  (Only once for each port, individual association are ignored).
            Open_Temp;
            case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
               when Iir_Kind_Association_Element_By_Individual
                 | Iir_Kind_Association_Element_Open =>
                  pragma Assert (Get_Whole_Association_Flag (Assoc));
                  Chap4.Elab_Signal_Declaration_Storage (Formal, False);
               when Iir_Kind_Association_Element_By_Expression
                  | Iir_Kind_Association_Element_By_Name =>
                  if Get_Whole_Association_Flag (Assoc) then
                     Chap4.Elab_Signal_Declaration_Storage
                       (Formal, Get_Collapse_Signal_Flag (Assoc));
                  end if;
            end case;
            Close_Temp;

            --  Create or copy signals.
            Open_Temp;
            case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
               when Iir_Kind_Association_Element_By_Expression
                 | Iir_Kind_Association_Element_By_Name =>
                  if Get_Whole_Association_Flag (Assoc) then
                     if Get_Collapse_Signal_Flag (Assoc) then
                        Value := Get_Default_Value (Formal_Base);
                        if Is_Valid (Value) then
                           --  Set default value.
                           Chap9.Destroy_Types (Value);
                           Chap4.Elab_Object_Init
                             (Get_Var (Get_Info (Formal_Base).Signal_Val,
                                       Fbt_Info, Mode_Value),
                              Inter, Value, Alloc_System);
                        end if;
                        --  For collapsed association, copy signals.
                        Elab_Port_Map_Aspect_Assoc
                          (Assoc, Formal, True, Formal_Env, Actual_Env);
                     else
                        --  Create non-collapsed signals.
                        Chap4.Elab_Signal_Declaration_Object
                          (Formal, Block_Parent, False);
                        --  And associate
                        Elab_Port_Map_Aspect_Assoc_Or_Inertial
                             (Assoc, Formal, Formal_Env, Actual_Env);
                     end if;
                  else
                     --  By sub-element.
                     --  Never collapsed, signal was already created (by the
                     --  By_Individual association).
                     --  And associate.
                     Elab_Port_Map_Aspect_Assoc_Or_Inertial
                       (Assoc, Formal, Formal_Env, Actual_Env);
                  end if;
               when Iir_Kind_Association_Element_Open
                 | Iir_Kind_Association_Element_By_Individual =>
                  --  Create non-collapsed signals.
                  pragma Assert (Get_Whole_Association_Flag (Assoc));
                  Chap4.Elab_Signal_Declaration_Object
                    (Formal, Block_Parent, False);
            end case;
            Close_Temp;
         end;
         Next_Association_Interface (Assoc, Inter);
      end loop;
      Restore_Map_Env (Actual_Env);
   end Elab_Port_Map_Aspect;

   procedure Elab_Generic_Map_Aspect
     (Header : Iir; Map : Iir; Formal_Env : Map_Env)
   is
      Actual_Env : Map_Env;
      Assoc  : Iir;
      Formal : Iir;
      Inter : Iir;
   begin
      Save_Map_Env (Actual_Env, Formal_Env.Scope_Ptr);

      --  Elab generics, and associate.
      Assoc := Get_Generic_Map_Aspect_Chain (Map);
      Inter := Get_Generic_Chain (Header);
      while Assoc /= Null_Iir loop
         Formal := Get_Association_Formal (Assoc, Inter);
         Open_Temp;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               declare
                  Targ : Mnode;
               begin
                  if Get_Whole_Association_Flag (Assoc) then
                     Set_Map_Env (Formal_Env);
                     Chap4.Elab_Object_Storage (Formal);
                     Targ := Chap6.Translate_Name (Formal, Mode_Value);
                     Set_Map_Env (Actual_Env);
                     Chap4.Elab_Object_Init
                       (Targ, Formal, Get_Actual (Assoc), Alloc_System);
                  else
                     Set_Map_Env (Formal_Env);
                     Targ := Chap6.Translate_Name (Formal, Mode_Value);
                     Set_Map_Env (Actual_Env);
                     Chap7.Translate_Assign
                       (Targ, Get_Actual (Assoc), Get_Type (Formal));
                  end if;
               end;
            when Iir_Kind_Association_Element_Open =>
               case Get_Kind (Formal) is
                  when Iir_Kind_Interface_Constant_Declaration =>
                     declare
                        Value : constant Iir := Get_Default_Value (Formal);
                     begin
                        pragma Assert (Is_Valid (Value));
                        Set_Map_Env (Formal_Env);
                        Chap4.Elab_Object_Value (Formal, Value);
                        Chap9.Destroy_Types (Value);
                        Set_Map_Env (Actual_Env);
                     end;
                  when Iir_Kind_Interface_Package_Declaration =>
                     --  The package interface have generics and implicitly
                     --  defines an instantiated package.
                     pragma Assert
                       (Get_Generic_Map_Aspect_Chain (Formal) /= Null_Iir);
                     Set_Map_Env (Formal_Env);
                     Chap2.Elab_Package_Instantiation_Declaration (Formal);
                     Set_Map_Env (Actual_Env);
                  when Iir_Kinds_Interface_Subprogram_Declaration =>
                     --  Expanded.
                     null;
                  when others =>
                     Error_Kind ("elab_generic_map_aspect(open)", Formal);
               end case;
            when Iir_Kind_Association_Element_By_Individual =>
               --  Create the object.
               declare
                  Formal_Type : constant Iir := Get_Type (Formal);
                  Obj_Info    : constant Object_Info_Acc := Get_Info (Formal);
                  Obj_Type    : constant Iir := Get_Actual_Type (Assoc);
                  Formal_Node : Mnode;
                  Type_Info   : Type_Info_Acc;
                  Bounds      : Mnode;
               begin
                  Set_Map_Env (Formal_Env);
                  Chap3.Elab_Object_Subtype_Indication (Formal); -- FIXME?
                  Type_Info := Get_Info (Formal_Type);
                  Formal_Node := Get_Var
                    (Obj_Info.Object_Var, Type_Info, Mode_Value);
                  Stabilize (Formal_Node);
                  if Obj_Type = Null_Iir then
                     Chap4.Allocate_Complex_Object
                       (Formal_Type, Alloc_System, Formal_Node);
                  else
                     Chap3.Create_Composite_Subtype (Obj_Type);
                     Bounds := Chap3.Get_Composite_Type_Bounds (Obj_Type);
                     Chap3.Translate_Object_Allocation
                       (Formal_Node, Alloc_System, Formal_Type, Bounds);
                  end if;
                  Set_Map_Env (Actual_Env);
               end;
            when Iir_Kind_Association_Element_Package =>
               pragma Assert (Get_Kind (Formal) =
                                Iir_Kind_Interface_Package_Declaration);
               declare
                  Uninst_Pkg  : constant Iir :=
                    Get_Uninstantiated_Package_Decl (Formal);
                  Uninst_Info : constant Ortho_Info_Acc :=
                    Get_Info (Uninst_Pkg);
                  Formal_Info : constant Ortho_Info_Acc := Get_Info (Formal);
                  Actual      : constant Iir := Get_Named_Entity
                    (Get_Actual (Assoc));
                  Actual_Info : constant Ortho_Info_Acc := Get_Info (Actual);
               begin
                  New_Assign_Stmt
                    (Get_Var (Formal_Info.Package_Instance_Spec_Var),
                     New_Address
                       (Get_Instance_Ref
                            (Actual_Info.Package_Instance_Spec_Scope),
                        Uninst_Info.Package_Spec_Ptr_Type));
                  New_Assign_Stmt
                    (Get_Var (Formal_Info.Package_Instance_Body_Var),
                     New_Address
                       (Get_Instance_Ref
                            (Actual_Info.Package_Instance_Body_Scope),
                        Uninst_Info.Package_Body_Ptr_Type));
               end;
            when Iir_Kind_Association_Element_Type
              | Iir_Kind_Association_Element_Subprogram =>
               null;
            when others =>
               Error_Kind ("elab_generic_map_aspect(1)", Assoc);
         end case;
         Close_Temp;
         Next_Association_Interface (Assoc, Inter);
      end loop;
      Restore_Map_Env (Actual_Env);
   end Elab_Generic_Map_Aspect;

   procedure Elab_Map_Aspect
     (Header : Iir; Maps : Iir; Block_Parent : Iir; Formal_Env : Map_Env) is
   begin
      --  The use of FORMAL_ENV (and then later ACTUAL_ENV) is rather fragile
      --  as in some cases both the formal and the actual are referenced in the
      --  same time (like Check_Array_Match).  But the env are different only
      --  in case of direct recursive instantation (rare).  To stay on the safe
      --  side, FORMAL_ENV must be active/set.

      --  The generic map must be done before the elaboration of
      --  the ports, since a port subtype may depend on a generic.
      Elab_Generic_Map_Aspect (Header, Maps, Formal_Env);

      Elab_Port_Map_Aspect (Header, Maps, Block_Parent, Formal_Env);
   end Elab_Map_Aspect;
end Trans.Chap5;
