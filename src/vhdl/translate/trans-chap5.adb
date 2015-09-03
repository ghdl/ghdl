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

with Errorout; use Errorout;
with Iirs_Utils; use Iirs_Utils;
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

   procedure Translate_Attribute_Specification
     (Spec : Iir_Attribute_Specification)
   is
      Attr   : constant Iir_Attribute_Declaration :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
      Atinfo : constant Type_Info_Acc := Get_Info (Get_Type (Attr));
      Mark   : Id_Mark_Type;
      Info   : Object_Info_Acc;
   begin
      Push_Identifier_Prefix_Uniq (Mark);
      Info := Add_Info (Spec, Kind_Object);
      Info.Object_Var := Create_Var
        (Create_Var_Identifier (Attr),
         Chap4.Get_Object_Type (Atinfo, Mode_Value),
         Global_Storage);
      Pop_Identifier_Prefix (Mark);
   end Translate_Attribute_Specification;

   procedure Elab_Attribute_Specification
     (Spec : Iir_Attribute_Specification)
   is
      Attr : constant Iir_Attribute_Declaration :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
   begin
      --  Kludge
      Set_Info (Attr, Get_Info (Spec));
      Chap4.Elab_Object_Value (Attr, Get_Expression (Spec));
      Clear_Info (Attr);
   end Elab_Attribute_Specification;

   procedure Gen_Elab_Disconnect_Non_Composite (Targ      : Mnode;
                                                Targ_Type : Iir;
                                                Time      : O_Dnode)
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
     (Targ : Mnode; Targ_Type : Iir; Time : O_Dnode)
         return O_Dnode
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Time;
   end Gen_Elab_Disconnect_Prepare;

   function Gen_Elab_Disconnect_Update_Data_Array (Time      : O_Dnode;
                                                   Targ_Type : Iir;
                                                   Index     : O_Dnode)
                                                      return O_Dnode
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

   procedure Gen_Elab_Disconnect_Finish_Data_Composite
     (Data : in out O_Dnode)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Gen_Elab_Disconnect_Finish_Data_Composite;

   procedure Gen_Elab_Disconnect is new Foreach_Non_Composite
     (Data_Type => O_Dnode,
      Composite_Data_Type => O_Dnode,
      Do_Non_Composite => Gen_Elab_Disconnect_Non_Composite,
      Prepare_Data_Array => Gen_Elab_Disconnect_Prepare,
      Update_Data_Array => Gen_Elab_Disconnect_Update_Data_Array,
      Finish_Data_Array => Gen_Elab_Disconnect_Finish_Data_Composite,
      Prepare_Data_Record => Gen_Elab_Disconnect_Prepare,
      Update_Data_Record => Gen_Elab_Disconnect_Update_Data_Record,
      Finish_Data_Record => Gen_Elab_Disconnect_Finish_Data_Composite);

   procedure Elab_Disconnection_Specification
     (Spec : Iir_Disconnection_Specification)
   is
      Val  : O_Dnode;
      List : constant Iir_List := Get_Signal_List (Spec);
      El   : Iir;
   begin
      Val := Create_Temp_Init
        (Std_Time_Otype,
         Chap7.Translate_Expression (Get_Expression (Spec)));
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         Gen_Elab_Disconnect (Chap6.Translate_Name (El),
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
      Actual_Node : Mnode;
      Actual_Type : Iir;

      --  Mode of the connection.
      Mode : Connect_Mode;

      --  If true, formal signal is a copy of the actual.
      By_Copy : Boolean;
   end record;

   --  Connect_effective: FORMAL is set from ACTUAL.
   --  Connect_Source: ACTUAL is set from FORMAL (source of ACTUAL).
   procedure Connect_Scalar (Formal_Node : Mnode;
                             Formal_Type : Iir;
                             Data        : Connect_Data)
   is
      Act_Node, Form_Node : Mnode;
   begin
      if Data.By_Copy then
         New_Assign_Stmt (M2Lv (Formal_Node), M2E (Data.Actual_Node));
         return;
      end if;

      case Data.Mode is
         when Connect_Both =>
            Open_Temp;
            Act_Node := Stabilize (Data.Actual_Node, True);
            Form_Node := Stabilize (Formal_Node, True);
         when Connect_Source
            | Connect_Effective =>
            Act_Node := Data.Actual_Node;
            Form_Node := Formal_Node;
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
            Type_Info : Type_Info_Acc;
            Subprg    : O_Dnode;
            Constr    : O_Assoc_List;
            Conv      : O_Tnode;
         begin
            Type_Info := Get_Info (Formal_Type);
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
               when Type_Mode_I32 =>
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
                             New_Convert_Ov (New_Value (M2Lv (Formal_Node)),
                               Ghdl_Signal_Ptr));
            New_Association (Constr,
                             New_Convert_Ov (M2E (Data.Actual_Node), Conv));
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
      Atype : Iir;
   begin
      Atype := Get_Base_Type (Data.Actual_Type);
      if Get_Kind (Atype) = Iir_Kind_Record_Type_Definition then
         Res := Data;
         Stabilize (Res.Actual_Node);
         return Res;
      else
         return Data;
      end if;
   end Connect_Prepare_Data_Composite;

   function Connect_Update_Data_Array (Data        : Connect_Data;
                                       Formal_Type : Iir;
                                       Index       : O_Dnode)
                                          return Connect_Data
   is
      pragma Unreferenced (Formal_Type);
      Res : Connect_Data;
   begin
      --  FIXME: should check matching elements!
      Res := (Actual_Node =>
                Chap3.Index_Base (Chap3.Get_Array_Base (Data.Actual_Node),
                  Data.Actual_Type, New_Obj_Value (Index)),
              Actual_Type => Get_Element_Subtype (Data.Actual_Type),
              Mode => Data.Mode,
              By_Copy => Data.By_Copy);
      return Res;
   end Connect_Update_Data_Array;

   function Connect_Update_Data_Record (Data        : Connect_Data;
                                        Formal_Type : Iir;
                                        El          : Iir_Element_Declaration)
                                           return Connect_Data
   is
      pragma Unreferenced (Formal_Type);
      Res : Connect_Data;
   begin
      Res := (Actual_Node =>
                Chap6.Translate_Selected_Element (Data.Actual_Node, El),
              Actual_Type => Get_Type (El),
              Mode => Data.Mode,
              By_Copy => Data.By_Copy);
      return Res;
   end Connect_Update_Data_Record;

   procedure Connect_Finish_Data_Composite (Data : in out Connect_Data)
   is
      pragma Unreferenced (Data);
   begin
      null;
   end Connect_Finish_Data_Composite;

   procedure Connect is new Foreach_Non_Composite
     (Data_Type => Connect_Data,
      Composite_Data_Type => Connect_Data,
      Do_Non_Composite => Connect_Scalar,
      Prepare_Data_Array => Connect_Prepare_Data_Composite,
      Update_Data_Array => Connect_Update_Data_Array,
      Finish_Data_Array => Connect_Finish_Data_Composite,
      Prepare_Data_Record => Connect_Prepare_Data_Composite,
      Update_Data_Record => Connect_Update_Data_Record,
      Finish_Data_Record => Connect_Finish_Data_Composite);

   procedure Elab_Unconstrained_Port (Port : Iir; Actual : Iir)
   is
      Actual_Type : constant Iir := Get_Type (Actual);
      Act_Node    : Mnode;
      Bounds      : Mnode;
      Tinfo       : Type_Info_Acc;
      Bound_Var   : O_Dnode;
   begin
      Open_Temp;
      if Is_Fully_Constrained_Type (Actual_Type) then
         Chap3.Create_Array_Subtype (Actual_Type);
         Tinfo := Get_Info (Actual_Type);
         Bounds := Chap3.Get_Array_Type_Bounds (Actual_Type);
         if Get_Alloc_Kind_For_Var (Tinfo.T.Array_Bounds) = Alloc_Stack then
            --  We need a copy.
            Bound_Var := Create_Temp (Tinfo.T.Bounds_Ptr_Type);
            New_Assign_Stmt
              (New_Obj (Bound_Var),
               Gen_Alloc (Alloc_System,
                          New_Lit (New_Sizeof (Tinfo.T.Bounds_Type,
                                               Ghdl_Index_Type)),
                          Tinfo.T.Bounds_Ptr_Type));
            Gen_Memcpy (New_Obj_Value (Bound_Var),
                        M2Addr (Bounds),
                        New_Lit (New_Sizeof (Tinfo.T.Bounds_Type,
                                             Ghdl_Index_Type)));
            Bounds := Dp2M (Bound_Var, Tinfo, Mode_Value,
                            Tinfo.T.Bounds_Type,
                            Tinfo.T.Bounds_Ptr_Type);
         end if;
      else
         Bounds := Chap3.Get_Array_Bounds (Chap6.Translate_Name (Actual));
      end if;
      Act_Node := Chap6.Translate_Name (Port);
      New_Assign_Stmt
        (-- FIXME: this works only because it is not stabilized,
         -- and therefore the bounds field is returned and not
         -- a pointer to the bounds.
         M2Lp (Chap3.Get_Array_Bounds (Act_Node)),
         M2Addr (Bounds));
      Close_Temp;
   end Elab_Unconstrained_Port;

   procedure Elab_Port_Map_Aspect_Assoc (Assoc : Iir; By_Copy : Boolean)
   is
      Formal      : constant Iir := Get_Formal (Assoc);
      Actual      : constant Iir := Get_Actual (Assoc);
      Formal_Type : constant Iir := Get_Type (Formal);
      Actual_Type : constant Iir := Get_Type (Actual);
      Inter       : constant Iir := Get_Association_Interface (Assoc);
      Formal_Node : Mnode;
      Actual_Node : Mnode;
      Data        : Connect_Data;
      Mode        : Connect_Mode;
   begin
      pragma Assert
        (Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression);

      Open_Temp;
      if Get_In_Conversion (Assoc) = Null_Iir
        and then Get_Out_Conversion (Assoc) = Null_Iir
      then
         Formal_Node := Chap6.Translate_Name (Formal);
         pragma Assert (Get_Object_Kind (Formal_Node) = Mode_Signal);
         if Is_Signal_Name (Actual) then
            --  LRM93 4.3.1.2
            --  For a signal of a scalar type, each source is either
            --  a driver or an OUT, INOUT, BUFFER or LINKAGE port of
            --  a component instance or of a block statement with
            --  which the signalis associated.

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
            case Get_Mode (Inter) is
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

            --  translate actual (abort if not a signal).
            Actual_Node := Chap6.Translate_Name (Actual);
            if Get_Object_Kind (Actual_Node) /= Mode_Signal then
               raise Internal_Error;
            end if;
         else
            declare
               Actual_Val : O_Enode;
            begin
               Actual_Val := Chap7.Translate_Expression
                 (Actual, Formal_Type);
               Actual_Node := E2M
                 (Actual_Val, Get_Info (Formal_Type), Mode_Value);
               Mode := Connect_Value;
            end;
         end if;

         if Get_Kind (Formal_Type) in Iir_Kinds_Array_Type_Definition
         then
            --  Check length matches.
            Stabilize (Formal_Node);
            Stabilize (Actual_Node);
            Chap3.Check_Array_Match (Formal_Type, Formal_Node,
                                     Actual_Type, Actual_Node,
                                     Assoc);
         end if;

         Data := (Actual_Node => Actual_Node,
                  Actual_Type => Actual_Type,
                  Mode => Mode,
                  By_Copy => By_Copy);
         Connect (Formal_Node, Formal_Type, Data);
      else
         if Get_In_Conversion (Assoc) /= Null_Iir then
            Chap4.Elab_In_Conversion (Assoc, Actual_Node);
            Formal_Node := Chap6.Translate_Name (Formal);
            Data := (Actual_Node => Actual_Node,
                     Actual_Type => Formal_Type,
                     Mode => Connect_Effective,
                     By_Copy => False);
            Connect (Formal_Node, Formal_Type, Data);
         end if;
         if Get_Out_Conversion (Assoc) /= Null_Iir then
            --  flow: FORMAL to ACTUAL
            Chap4.Elab_Out_Conversion (Assoc, Formal_Node);
            Actual_Node := Chap6.Translate_Name (Actual);
            Data := (Actual_Node => Actual_Node,
                     Actual_Type => Actual_Type,
                     Mode => Connect_Source,
                     By_Copy => False);
            Connect (Formal_Node, Actual_Type, Data);
         end if;
      end if;

      Close_Temp;
   end Elab_Port_Map_Aspect_Assoc;

   --  Return TRUE if the collapse_signal_flag is set for each individual
   --  association.
   function Inherit_Collapse_Flag (Assoc : Iir) return Boolean
   is
      El : Iir;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_By_Individual =>
            El := Get_Individual_Association_Chain (Assoc);
            while El /= Null_Iir loop
               if Inherit_Collapse_Flag (El) = False then
                  return False;
               end if;
               El := Get_Chain (El);
            end loop;
            return True;
         when Iir_Kind_Choice_By_Expression
            | Iir_Kind_Choice_By_Range
            | Iir_Kind_Choice_By_Name =>
            El := Assoc;
            while El /= Null_Iir loop
               if not Inherit_Collapse_Flag (Get_Associated_Expr (Assoc))
               then
                  return False;
               end if;
               El := Get_Chain (El);
            end loop;
            return True;
         when Iir_Kind_Association_Element_By_Expression =>
            return Get_Collapse_Signal_Flag (Assoc);
         when others =>
            Error_Kind ("inherit_collapse_flag", Assoc);
      end case;
   end Inherit_Collapse_Flag;

   procedure Elab_Generic_Map_Aspect (Mapping : Iir)
   is
      Assoc  : Iir;
      Formal : Iir;
   begin
      --  Elab generics, and associate.
      Assoc := Get_Generic_Map_Aspect_Chain (Mapping);
      while Assoc /= Null_Iir loop
         Open_Temp;
         Formal := Strip_Denoting_Name (Get_Formal (Assoc));
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               declare
                  Targ : Mnode;
               begin
                  if Get_Whole_Association_Flag (Assoc) then
                     Chap4.Elab_Object_Storage (Formal);
                     Targ := Chap6.Translate_Name (Formal);
                     Chap4.Elab_Object_Init
                       (Targ, Formal, Get_Actual (Assoc));
                  else
                     Targ := Chap6.Translate_Name (Formal);
                     Chap7.Translate_Assign
                       (Targ, Get_Actual (Assoc), Get_Type (Formal));
                  end if;
               end;
            when Iir_Kind_Association_Element_Open =>
               declare
                  Value : constant Iir := Get_Default_Value (Formal);
               begin
                  Chap4.Elab_Object_Value (Formal, Value);
                  Chap9.Destroy_Types (Value);
               end;
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
                  Chap3.Elab_Object_Subtype (Formal_Type);
                  Type_Info := Get_Info (Formal_Type);
                  Formal_Node := Get_Var
                    (Obj_Info.Object_Var, Type_Info, Mode_Value);
                  Stabilize (Formal_Node);
                  if Obj_Type = Null_Iir then
                     Chap4.Allocate_Complex_Object
                       (Formal_Type, Alloc_System, Formal_Node);
                  else
                     Chap3.Create_Array_Subtype (Obj_Type);
                     Bounds := Chap3.Get_Array_Type_Bounds (Obj_Type);
                     Chap3.Translate_Object_Allocation
                       (Formal_Node, Alloc_System, Formal_Type, Bounds);
                  end if;
               end;
            when Iir_Kind_Association_Element_Package =>
               pragma Assert (Get_Kind (Formal) =
                                Iir_Kind_Interface_Package_Declaration);
               declare
                  Uninst_Pkg  : constant Iir := Get_Named_Entity
                    (Get_Uninstantiated_Package_Name (Formal));
                  Uninst_Info : constant Ortho_Info_Acc :=
                    Get_Info (Uninst_Pkg);
                  Formal_Info : constant Ortho_Info_Acc :=
                    Get_Info (Formal);
                  Actual      : constant Iir := Get_Named_Entity
                    (Get_Actual (Assoc));
                  Actual_Info : constant Ortho_Info_Acc :=
                    Get_Info (Actual);
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
            when others =>
               Error_Kind ("elab_generic_map_aspect(1)", Assoc);
         end case;
         Close_Temp;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Elab_Generic_Map_Aspect;

   procedure Elab_Port_Map_Aspect (Mapping : Iir; Block_Parent : Iir)
   is
      Assoc               : Iir;
      Formal              : Iir;
      Formal_Base         : Iir;
      Fb_Type             : Iir;
      Fbt_Info            : Type_Info_Acc;
      Collapse_Individual : Boolean := False;
   begin
      --  Ports.
      Assoc := Get_Port_Map_Aspect_Chain (Mapping);
      while Assoc /= Null_Iir loop
         Formal := Get_Formal (Assoc);
         Formal_Base := Get_Association_Interface (Assoc);
         Fb_Type := Get_Type (Formal_Base);

         Open_Temp;
         --  Set bounds of unconstrained ports.
         Fbt_Info := Get_Info (Fb_Type);
         if Fbt_Info.Type_Mode = Type_Mode_Fat_Array then
            case Get_Kind (Assoc) is
               when Iir_Kind_Association_Element_By_Expression =>
                  if Get_Whole_Association_Flag (Assoc) then
                     Elab_Unconstrained_Port (Formal, Get_Actual (Assoc));
                  end if;
               when Iir_Kind_Association_Element_Open =>
                  declare
                     Value : constant Iir := Get_Default_Value (Formal_Base);
                     Actual_Type : constant Iir := Get_Type (Value);
                     Bounds      : Mnode;
                     Formal_Node : Mnode;
                  begin
                     Chap3.Create_Array_Subtype (Actual_Type);
                     Bounds := Chap3.Get_Array_Type_Bounds (Actual_Type);
                     Formal_Node := Chap6.Translate_Name (Formal);
                     New_Assign_Stmt
                       (M2Lp (Chap3.Get_Array_Bounds (Formal_Node)),
                        M2Addr (Bounds));
                     Chap9.Destroy_Types (Value);
                  end;
               when Iir_Kind_Association_Element_By_Individual =>
                  declare
                     Actual_Type : Iir;
                     Bounds      : Mnode;
                     Formal_Node : Mnode;
                  begin
                     Actual_Type := Get_Actual_Type (Assoc);
                     Chap3.Create_Array_Subtype (Actual_Type);
                     Bounds := Chap3.Get_Array_Type_Bounds (Actual_Type);
                     Formal_Node := Chap6.Translate_Name (Formal);
                     New_Assign_Stmt
                       (M2Lp (Chap3.Get_Array_Bounds (Formal_Node)),
                        M2Addr (Bounds));
                  end;
               when others =>
                  Error_Kind ("elab_map_aspect(2)", Assoc);
            end case;
         end if;
         Close_Temp;

         --  Allocate storage of ports.
         Open_Temp;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Individual
               | Iir_Kind_Association_Element_Open =>
               Chap4.Elab_Signal_Declaration_Storage (Formal);
            when Iir_Kind_Association_Element_By_Expression =>
               if Get_Whole_Association_Flag (Assoc) then
                  Chap4.Elab_Signal_Declaration_Storage (Formal);
               end if;
            when others =>
               Error_Kind ("elab_map_aspect(3)", Assoc);
         end case;
         Close_Temp;

         --  Create or copy signals.
         Open_Temp;
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_By_Expression =>
               if Get_Whole_Association_Flag (Assoc) then
                  if Get_Collapse_Signal_Flag (Assoc) then
                     --  For collapsed association, copy signals.
                     Elab_Port_Map_Aspect_Assoc (Assoc, True);
                  else
                     --  Create non-collapsed signals.
                     Chap4.Elab_Signal_Declaration_Object
                       (Formal, Block_Parent, False);
                     --  And associate.
                     Elab_Port_Map_Aspect_Assoc (Assoc, False);
                  end if;
               else
                  --  By sub-element.
                  --  Either the whole signal is collapsed or it was already
                  --  created.
                  --  And associate.
                  Elab_Port_Map_Aspect_Assoc (Assoc, Collapse_Individual);
               end if;
            when Iir_Kind_Association_Element_Open =>
               --  Create non-collapsed signals.
               Chap4.Elab_Signal_Declaration_Object
                 (Formal, Block_Parent, False);
            when Iir_Kind_Association_Element_By_Individual =>
               --  Inherit the collapse flag.
               --  If it is set for all sub-associations, continue.
               --  Otherwise, create signals and do not collapse.
               --  FIXME: this may be slightly optimized.
               if not Inherit_Collapse_Flag (Assoc) then
                  --  Create the formal.
                  Chap4.Elab_Signal_Declaration_Object
                    (Formal, Block_Parent, False);
                  Collapse_Individual := False;
               else
                  Collapse_Individual := True;
               end if;
            when others =>
               Error_Kind ("elab_map_aspect(4)", Assoc);
         end case;
         Close_Temp;

         Assoc := Get_Chain (Assoc);
      end loop;
   end Elab_Port_Map_Aspect;

   procedure Elab_Map_Aspect (Mapping : Iir; Block_Parent : Iir) is
   begin
      --  The generic map must be done before the elaboration of
      --  the ports, since a port subtype may depend on a generic.
      Elab_Generic_Map_Aspect (Mapping);

      Elab_Port_Map_Aspect (Mapping, Block_Parent);
   end Elab_Map_Aspect;
end Trans.Chap5;
