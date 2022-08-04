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

with Flags;
with Libraries;

with Vhdl.Canon;
with Vhdl.Canon_PSL;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Nodes_Meta;
with Vhdl.Ieee.Std_Logic_1164;

with PSL.Types; use PSL.Types;
with PSL.Nodes;
with PSL.NFAs;
with PSL.NFAs.Utils;
with PSL.Subsets;
with PSL.Errors; use PSL.Errors;

with Trans_Analyzes;
with Trans.Chap1;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap5;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap8;
with Trans.Rtis;
with Translation; use Translation;
with Trans_Decls; use Trans_Decls;
with Trans.Helpers2; use Trans.Helpers2;
with Trans.Foreach_Non_Composite;

package body Trans.Chap9 is
   use Trans.Helpers;

   procedure Destroy_Types (N : PSL_Node);

   procedure Set_Direct_Drivers (Proc : Iir)
   is
      Proc_Info : constant Proc_Info_Acc := Get_Info (Proc);
      Drivers   : constant Direct_Drivers_Acc := Proc_Info.Process_Drivers;
      Info      : Ortho_Info_Acc;
      Var       : Var_Type;
      Sig       : Iir;
   begin
      for I in Drivers.all'Range loop
         Var := Drivers (I).Var;
         if Var /= Null_Var then
            Sig := Get_Object_Prefix (Drivers (I).Sig);
            Info := Get_Info (Sig);
            pragma Assert (Info.Kind = Kind_Signal);
            case Info.Kind is
               when Kind_Signal =>
                  Info.Signal_Driver := Var;
               when Kind_Alias =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
      end loop;
   end Set_Direct_Drivers;

   procedure Reset_Direct_Drivers (Proc : Iir)
   is
      Proc_Info : constant Proc_Info_Acc := Get_Info (Proc);
      Drivers   : constant Direct_Drivers_Acc := Proc_Info.Process_Drivers;
      Info      : Ortho_Info_Acc;
      Var       : Var_Type;
      Sig       : Iir;
   begin
      for I in Drivers.all'Range loop
         Var := Drivers (I).Var;
         if Var /= Null_Var then
            Sig := Get_Object_Prefix (Drivers (I).Sig);
            Info := Get_Info (Sig);
            case Info.Kind is
               when Kind_Signal =>
                  Info.Signal_Driver := Null_Var;
               when Kind_Alias =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
         end if;
      end loop;
   end Reset_Direct_Drivers;

   procedure Translate_Process_Statement (Proc : Iir; Base : Block_Info_Acc)
   is
      use Trans.Chap8;
      Info       : constant Proc_Info_Acc := Get_Info (Proc);
      Is_Non_Sensitized : constant Boolean :=
        Get_Kind (Proc) = Iir_Kind_Process_Statement;
      Inter_List : O_Inter_List;
      Instance   : O_Dnode;
   begin
      Start_Procedure_Decl (Inter_List, Create_Identifier ("PROC"),
                            O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Process_Subprg);

      Start_Subprogram_Body (Info.Process_Subprg);
      Push_Local_Factory;
      --  Push scope for architecture declarations.
      Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

      if Is_Non_Sensitized then
         Chap8.State_Entry (Info);
      end if;

      Chap8.Translate_Statements_Chain
        (Get_Sequential_Statement_Chain (Proc));

      if Is_Non_Sensitized then
         Chap8.State_Jump (State_Init);
         Chap8.State_Leave (Proc);
      end if;

      Clear_Scope (Base.Block_Scope);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Translate_Process_Statement;

   procedure Translate_Implicit_Guard_Signal
     (Guard : Iir; Base : Block_Info_Acc)
   is
      Guard_Expr : constant Iir := Get_Guard_Expression (Guard);
      Info       : constant Signal_Info_Acc := Get_Info (Guard);
      Inter_List : O_Inter_List;
      Instance   : O_Dnode;
   begin
      --  Create the subprogram to compute the value of GUARD.
      Start_Function_Decl (Inter_List, Create_Identifier ("_GUARD_PROC"),
                           O_Storage_Private, Std_Boolean_Type_Node);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Signal_Function);

      Start_Subprogram_Body (Info.Signal_Function);
      Push_Local_Factory;
      Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);
      Open_Temp;
      New_Return_Stmt (Chap7.Translate_Expression (Guard_Expr));
      Close_Temp;
      Clear_Scope (Base.Block_Scope);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Translate_Implicit_Guard_Signal;

   procedure Translate_Component_Instantiation_Statement (Inst : Iir)
   is
      Info : Block_Info_Acc;
      Ports : Iir;

      Mark, Mark2 : Id_Mark_Type;
      Assoc, Inter : Iir;
      Num : Iir_Int32;
      Has_Conv_Record : Boolean := False;
   begin
      Info := Add_Info (Inst, Kind_Block);
      Push_Identifier_Prefix (Mark, Get_Label (Inst));
      Num := 0;

      --  Add a pointer to the instance.
      if Is_Component_Instantiation (Inst) then
         --  Via a component declaration.
         declare
            Comp : constant Iir :=
              Get_Named_Entity (Get_Instantiated_Unit (Inst));
            Comp_Info : constant Comp_Info_Acc := Get_Info (Comp);
         begin
            Info.Block_Link_Field := Add_Instance_Factory_Field
              (Create_Identifier_Without_Prefix (Inst),
               Get_Scope_Type (Comp_Info.Comp_Scope));
            Ports := Comp;
         end;
      else
         --  Direct instantiation.
         Info.Block_Link_Field := Add_Instance_Factory_Field
           (Create_Identifier_Without_Prefix (Inst),
            Rtis.Ghdl_Component_Link_Type);
         Ports := Get_Entity_From_Entity_Aspect (Get_Instantiated_Unit (Inst));
      end if;

      --  When conversions are used, the subtype of the actual (or of the
      --  formal for formal conversions) may not be yet translated.  This
      --  can happen if the name is a slice.
      --  We need to translate it and create variables in the instance
      --  because it will be referenced by the conversion subprogram.
      Assoc := Get_Port_Map_Aspect_Chain (Inst);
      Inter := Get_Port_Chain (Ports);
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Name then
            declare
               Act_Conv : constant Iir := Get_Actual_Conversion (Assoc);
               Act_Type : constant Iir := Get_Type (Get_Actual (Assoc));
               Form_Conv : constant Iir := Get_Formal_Conversion (Assoc);
               Formal : constant Iir := Get_Formal (Assoc);
               Need_Actual : constant Boolean := Act_Conv /= Null_Iir
                 and then Is_Anonymous_Type_Definition (Act_Type);
               Need_Formal : constant Boolean := Form_Conv /= Null_Iir
                 and then Is_Anonymous_Type_Definition (Get_Type (Formal));
            begin
               if Need_Actual or Need_Formal then
                  --  Lazy creation of the record.
                  if not Has_Conv_Record then
                     Has_Conv_Record := True;
                     Push_Instance_Factory (Info.Block_Scope'Access);
                  end if;

                  --  FIXME: handle with overload multiple case on the same
                  --  formal.
                  Push_Identifier_Prefix
                    (Mark2,
                     Get_Identifier
                       (Get_Association_Interface (Assoc, Inter)), Num);
                  Num := Num + 1;
                  if Need_Actual then
                     Chap3.Translate_Anonymous_Subtype_Definition
                       (Act_Type, True);
                  end if;
                  if Need_Formal then
                     Chap3.Translate_Anonymous_Subtype_Definition
                       (Get_Type (Formal), True);
                  end if;
                  Pop_Identifier_Prefix (Mark2);
               end if;
            end;
         end if;
         Next_Association_Interface (Assoc, Inter);
      end loop;
      if Has_Conv_Record then
         Pop_Instance_Factory (Info.Block_Scope'Access);
         New_Type_Decl
           (Create_Identifier (Get_Identifier (Inst), "__CONVS"),
            Get_Scope_Type (Info.Block_Scope));
         Info.Block_Parent_Field := Add_Instance_Factory_Field
           (Create_Identifier_Without_Prefix (Get_Identifier (Inst),
            "__CONVS"),
            Get_Scope_Type (Info.Block_Scope));
      end if;
      Pop_Identifier_Prefix (Mark);
   end Translate_Component_Instantiation_Statement;

   procedure Translate_Process_Declarations (Proc : Iir)
   is
      Mark : Id_Mark_Type;
      Info : Ortho_Info_Acc;

      Drivers     : Iir_List;
      It          : List_Iterator;
      Nbr_Drivers : Natural;
      Sig         : Iir;
   begin
      Info := Add_Info (Proc, Kind_Process);

      --  Create process record.
      Push_Identifier_Prefix (Mark, Get_Identifier (Proc));
      Push_Instance_Factory (Info.Process_Scope'Access);
      Chap4.Translate_Declaration_Chain (Proc);

      if Get_Kind (Proc) = Iir_Kind_Process_Statement then
         --  The state variable.
         Info.Process_State := Create_Var (Create_Var_Identifier ("STATE"),
                                           Ghdl_Index_Type, O_Storage_Local);

         --  Add declarations for statements (iterator, call) and state.
         Chap4.Translate_Statements_Chain_State_Declaration
           (Get_Sequential_Statement_Chain (Proc),
            Info.Process_Locvar_Scope'Access);

         Add_Scope_Field (Wki_Locvars, Info.Process_Locvar_Scope);
      end if;

      if Flag_Direct_Drivers then
         --  Create direct drivers.
         Drivers := Trans_Analyzes.Extract_Drivers (Proc);
         if Flag_Dump_Drivers then
            Trans_Analyzes.Dump_Drivers (Proc, Drivers);
         end if;

         Nbr_Drivers := Get_Nbr_Elements (Drivers);
         Info.Process_Drivers := new Direct_Driver_Arr (1 .. Nbr_Drivers);
         It := List_Iterate (Drivers);
         for I in 1 .. Nbr_Drivers loop
            pragma Assert (Is_Valid (It));
            Sig := Get_Element (It);
            Info.Process_Drivers (I) := (Sig => Sig, Var => Null_Var);
            Sig := Get_Object_Prefix (Sig);
            pragma Assert
              (Get_Kind (Sig) /= Iir_Kind_Object_Alias_Declaration);
            if not Get_After_Drivers_Flag (Sig) then
               Info.Process_Drivers (I).Var :=
                 Create_Var (Create_Var_Identifier (Sig, "_DDRV", I),
                             Chap4.Get_Object_Type
                               (Get_Info (Get_Type (Sig)), Mode_Value));

               --  Do not create driver severals times.
               Set_After_Drivers_Flag (Sig, True);
            end if;
            Next (It);
         end loop;
         pragma Assert (not Is_Valid (It));
         Trans_Analyzes.Free_Drivers_List (Drivers);
      end if;
      Pop_Instance_Factory (Info.Process_Scope'Access);
      New_Type_Decl (Create_Identifier ("INSTTYPE"),
                     Get_Scope_Type (Info.Process_Scope));
      Pop_Identifier_Prefix (Mark);

      --  Create a field in the parent record.
      Add_Scope_Field (Create_Identifier_Without_Prefix (Proc),
                       Info.Process_Scope);
   end Translate_Process_Declarations;

   procedure Translate_Psl_Directive_Declarations (Stmt : Iir)
   is
      Mark : Id_Mark_Type;
      Info : Ortho_Info_Acc;
   begin
      Info := Add_Info (Stmt, Kind_Psl_Directive);

      --  Create process record.
      Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
      Push_Instance_Factory (Info.Psl_Scope'Access);

      --  Create the state vector type.
      Info.Psl_Vect_Type := New_Array_Subtype
        (Std_Boolean_Array_Type,
         Std_Boolean_Type_Node,
         New_Index_Lit (Unsigned_64 (Get_PSL_Nbr_States (Stmt))));
      New_Type_Decl (Create_Identifier ("VECTTYPE"), Info.Psl_Vect_Type);

      --  Create count variables
      if Get_Kind (Stmt) = Iir_Kind_Psl_Endpoint_Declaration then
         --  FIXME: endpoint is a variable (and not a signal). This is required
         --  to have the right value for the current cycle, but as a
         --  consequence, this process must be evaluated before using the
         --  endpoint.
         Info.Psl_Finish_Count_Var := Create_Var
           (Create_Var_Identifier ("ENDPOINT"), Std_Boolean_Type_Node);
      else
         Info.Psl_Finish_Count_Var := Create_Var
           (Create_Var_Identifier ("FINISH_COUNT"), Ghdl_Index_Type);
         Info.Psl_Start_Count_Var := Create_Var
           (Create_Var_Identifier ("START_COUNT"), Ghdl_Index_Type);
      end if;

      Info.Psl_State_Var := Create_Var
         (Create_Var_Identifier ("STATE"), Trans.Rtis.Ghdl_Rti_Psl_State);

      Info.Psl_Vect_Var := Create_Var
        (Create_Var_Identifier ("VECT"), Info.Psl_Vect_Type);

      Pop_Instance_Factory (Info.Psl_Scope'Access);
      New_Type_Decl (Create_Identifier ("INSTTYPE"),
                     Get_Scope_Type (Info.Psl_Scope));
      Pop_Identifier_Prefix (Mark);

      --  Create a field in the parent record.
      Add_Scope_Field
        (Create_Identifier_Without_Prefix (Stmt), Info.Psl_Scope);
   end Translate_Psl_Directive_Declarations;

   --  Initialize/reset the PSL state vector for STMT.
   procedure Elab_PSL_State_Vector (Stmt : Iir)
   is
      Info   : constant Psl_Info_Acc := Get_Info (Stmt);
      Var_I  : O_Dnode;
      Label  : O_Snode;
   begin
      Start_Declare_Stmt;
      New_Var_Decl (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
      New_Assign_Stmt (New_Indexed_Element (Get_Var (Info.Psl_Vect_Var),
                                            New_Lit (Ghdl_Index_0)),
                       New_Lit (Std_Boolean_True_Node));
      New_Assign_Stmt (New_Obj (Var_I), New_Lit (Ghdl_Index_1));
      Start_Loop_Stmt (Label);
      Gen_Exit_When
        (Label,
         New_Compare_Op (ON_Ge,
                         New_Obj_Value (Var_I),
                         New_Lit (New_Unsigned_Literal
                                    (Ghdl_Index_Type,
                                     Unsigned_64 (Get_PSL_Nbr_States (Stmt)))),
                         Ghdl_Bool_Type));
      New_Assign_Stmt (New_Indexed_Element (Get_Var (Info.Psl_Vect_Var),
                       New_Obj_Value (Var_I)),
                       New_Lit (Std_Boolean_False_Node));
      Inc_Var (Var_I);
      Finish_Loop_Stmt (Label);
      Finish_Declare_Stmt;
   end Elab_PSL_State_Vector;

   function Translate_Psl_Expr (Expr : PSL_Node; Eos : Boolean)
                               return O_Enode
   is
      use PSL.Nodes;
   begin
      case Get_Kind (Expr) is
         when N_HDL_Bool
           | N_HDL_Expr =>
            declare
               E     : constant Iir := Get_HDL_Node (Expr);
               Rtype : constant Iir := Get_Base_Type (Get_Type (E));
               Res   : O_Enode;
            begin
               Res := Chap7.Translate_Expression (E);
               if Rtype = Boolean_Type_Definition then
                  return Res;
               elsif Rtype = Bit_Type_Definition then
                  return New_Compare_Op
                    (ON_Eq,
                     Res,
                     New_Lit (Get_Ortho_Literal (Bit_1)),
                     Get_Ortho_Type (Boolean_Type_Definition, Mode_Value));
               elsif Rtype = Vhdl.Ieee.Std_Logic_1164.Std_Ulogic_Type then
                  return New_Value
                    (New_Indexed_Element
                       (New_Obj (Ghdl_Std_Ulogic_To_Boolean_Array),
                        New_Convert_Ov (Res, Ghdl_Index_Type)));
               else
                  Error_Kind ("translate_psl_expr/hdl_expr", Expr);
               end if;
            end;
         when N_True =>
            return New_Lit (Std_Boolean_True_Node);
         when N_EOS =>
            if Eos then
               return New_Lit (Std_Boolean_True_Node);
            else
               return New_Lit (Std_Boolean_False_Node);
            end if;
         when N_Not_Bool =>
            return New_Monadic_Op
              (ON_Not,
               Translate_Psl_Expr (Get_Boolean (Expr), Eos));
         when N_And_Bool =>
            return New_Dyadic_Op
              (ON_And,
               Translate_Psl_Expr (Get_Left (Expr), Eos),
               Translate_Psl_Expr (Get_Right (Expr), Eos));
         when N_Or_Bool =>
            return New_Dyadic_Op
              (ON_Or,
               Translate_Psl_Expr (Get_Left (Expr), Eos),
               Translate_Psl_Expr (Get_Right (Expr), Eos));
         when N_Imp_Bool =>
            --  Equivalent to (not (a) or (b)) so this is a short-cut
            --  operation.
            declare
               Bool : constant O_Tnode :=
                 Get_Ortho_Type (Boolean_Type_Definition, Mode_Value);
               L : O_Dnode;
               If_Blk   : O_If_Block;
            begin
               L := Create_Temp (Bool);
               New_Assign_Stmt
                 (New_Obj (L), Translate_Psl_Expr (Get_Left (Expr), Eos));
               Start_If_Stmt (If_Blk, New_Obj_Value (L));
               New_Assign_Stmt
                 (New_Obj (L), Translate_Psl_Expr (Get_Right (Expr), Eos));
               New_Else_Stmt (If_Blk);
               New_Assign_Stmt
                 (New_Obj (L), New_Lit (Std_Boolean_True_Node));
               Finish_If_Stmt (If_Blk);
               return New_Obj_Value (L);
            end;
         when others =>
            Error_Kind ("translate_psl_expr", Expr);
      end case;
   end Translate_Psl_Expr;

   procedure Create_Psl_Final_Proc
     (Stmt : Iir; Base : Block_Info_Acc; Instance : out O_Dnode)
   is
      Info       : constant Psl_Info_Acc := Get_Info (Stmt);
      Inter_List : O_Inter_List;
   begin
      Start_Procedure_Decl (Inter_List, Create_Identifier ("FINALPROC"),
                            O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Psl_Proc_Final_Subprg);
   end Create_Psl_Final_Proc;

   --  Create an independant procedure to report coverage, as it is needed
   --  twice and the expression must not be translated twice.
   procedure Translate_Psl_Report
     (Stmt : Iir; Base : Block_Info_Acc; Proc : out O_Dnode)
   is
      Inter_List : O_Inter_List;
      Instance   : O_Dnode;
      Pass       : O_Dnode;
      Loc        : O_Dnode;
      Msg_Var    : O_Dnode;
      Blk        : O_If_Block;
      Expr       : Iir;
      Assocs     : O_Assoc_List;
   begin
      Start_Procedure_Decl (Inter_List, Create_Identifier ("REPORTPROC"),
                            O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      New_Interface_Decl (Inter_List, Pass, Get_Identifier ("pass_fail"),
                          Ghdl_Bool_Type);
      Finish_Subprogram_Decl (Inter_List, Proc);

      Start_Subprogram_Body (Proc);
      Push_Local_Factory;
      --  Push scope for architecture declarations.
      Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

      Loc := Chap4.Get_Location (Stmt);
      New_Var_Decl (Msg_Var, Get_Identifier ("msg"), O_Storage_Local,
                    Std_String_Ptr_Node);
      Expr := Get_Report_Expression (Stmt);
      if Expr = Null_Iir then
         New_Assign_Stmt (New_Obj (Msg_Var),
                          New_Lit (New_Null_Access (Std_String_Ptr_Node)));
      else
         New_Assign_Stmt
           (New_Obj (Msg_Var),
            Chap7.Translate_Expression (Expr, String_Type_Definition));
      end if;

      Start_If_Stmt (Blk, New_Obj_Value (Pass));

      Start_Association (Assocs, Ghdl_Psl_Cover);
      New_Association (Assocs, New_Obj_Value (Msg_Var));
      New_Association (Assocs, New_Lit (Get_Ortho_Literal
                                          (Severity_Level_Note)));
      New_Association (Assocs, New_Address (New_Obj (Loc),
                                            Ghdl_Location_Ptr_Node));
      New_Procedure_Call (Assocs);

      New_Else_Stmt (Blk);

      Start_Association (Assocs, Ghdl_Psl_Cover_Failed);
      New_Association (Assocs, New_Obj_Value (Msg_Var));
      New_Association (Assocs, New_Lit (Get_Ortho_Literal
                                          (Severity_Level_Warning)));
      New_Association (Assocs, New_Address (New_Obj (Loc),
                                            Ghdl_Location_Ptr_Node));
      New_Procedure_Call (Assocs);

      Finish_If_Stmt (Blk);

      Clear_Scope (Base.Block_Scope);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Translate_Psl_Report;

   procedure Call_Psl_Fail (Stmt : Iir; Subprg : O_Dnode)
   is
      Assocs  : O_Assoc_List;
      Loc     : O_Dnode;
   begin
      Loc := Chap4.Get_Location (Stmt);
      Start_Association (Assocs, Subprg);
      New_Association (Assocs, New_Address (New_Obj (Loc),
                                            Ghdl_Location_Ptr_Node));
      New_Procedure_Call (Assocs);
   end Call_Psl_Fail;

   procedure Translate_Psl_Directive_Statement
     (Stmt : Iir; Base : Block_Info_Acc)
   is
      use PSL.NFAs;
      use PSL.Nodes;
      use PSL.Subsets;
      Info       : constant Psl_Info_Acc := Get_Info (Stmt);
      Inter_List : O_Inter_List;
      Instance   : O_Dnode;
      Var_I      : O_Dnode;
      Var_Nvec   : O_Dnode;
      Var_SFlag  : O_Dnode;
      Report_Proc : O_Dnode;
      Label      : O_Snode;
      Clk_Blk    : O_If_Block;
      Abort_Blk  : O_If_Block;
      S_Blk      : O_If_Block;
      E_Blk      : O_If_Block;
      S          : NFA_State;
      S_Num      : Int32;
      E          : NFA_Edge;
      Sd         : NFA_State;
      Cond       : O_Enode;
      NFA        : PSL_NFA;
      D_Lit      : O_Cnode;
      Assocs     : O_Assoc_List;
      Has_Sync_Abort  : Boolean;
      Has_Async_Abort : Boolean;
      Abort_Prop : PSL_Node;
   begin
      Has_Async_Abort := False;
      Has_Sync_Abort := False;
      Abort_Prop := Null_PSL_Node;
      case Get_Kind (Stmt) is
         when Iir_Kind_Psl_Cover_Directive =>
            Translate_Psl_Report (Stmt, Base, Report_Proc);
         when Iir_Kind_Psl_Restrict_Directive =>
            null;
         when Iir_Kind_Psl_Endpoint_Declaration =>
            null;
         when Iir_Kinds_Psl_Property_Directive =>
            if Get_PSL_Abort_Flag (Stmt) then
               Abort_Prop := Get_Psl_Property (Stmt);
               Has_Async_Abort := Is_Async_Abort (Abort_Prop);
               Has_Sync_Abort := not Has_Async_Abort;
            end if;
         when others =>
            raise Internal_Error;
      end case;

      Start_Procedure_Decl (Inter_List, Create_Identifier ("PROC"),
                            O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Psl_Proc_Subprg);

      Start_Subprogram_Body (Info.Psl_Proc_Subprg);
      Push_Local_Factory;
      --  Push scope for architecture declarations.
      Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

      --  New state vector.
      New_Var_Decl (Var_Nvec, Wki_Res, O_Storage_Local, Info.Psl_Vect_Type);

      --  Flag for active edge from start state (assertion "started" flag).
      New_Var_Decl (Var_SFlag, Wki_Flag, O_Storage_Local, Ghdl_Bool_Type);

      --  'if' statement for async abort
      Open_Temp;
      if Has_Async_Abort then
         Start_If_Stmt (Abort_Blk,
                        Translate_Psl_Expr (Get_Boolean (Abort_Prop), False));
         Elab_PSL_State_Vector (Stmt);
         New_Else_Stmt (Abort_Blk);
      end if;

      --  Global 'if' statement for the clock.
      Start_If_Stmt (Clk_Blk,
                     Translate_Psl_Expr (Get_PSL_Clock (Stmt), False));

      --  'if' statement for sync abort.
      if Has_Sync_Abort then
         Start_If_Stmt (Abort_Blk,
                        Translate_Psl_Expr (Get_Boolean (Abort_Prop), False));
         Elab_PSL_State_Vector (Stmt);
         New_Else_Stmt (Abort_Blk);
      end if;

      --  Initialize the new state vector.
      Start_Declare_Stmt;
      New_Var_Decl (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
      Init_Var (Var_I);
      case Get_Kind (Stmt) is
         when Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Endpoint_Declaration =>
            --  Sequences for cover or endpoints are detected on every cycle,
            --  so the start state is always active.
            New_Assign_Stmt (New_Indexed_Element (Get_Var (Info.Psl_Vect_Var),
                                                  New_Obj_Value (Var_I)),
                             New_Lit (Std_Boolean_True_Node));
            Inc_Var (Var_I);
         when others =>
            null;
      end case;
      Start_Loop_Stmt (Label);
      Gen_Exit_When
        (Label,
         New_Compare_Op (ON_Ge,
                         New_Obj_Value (Var_I),
                         New_Lit (New_Unsigned_Literal
                                    (Ghdl_Index_Type,
                                     Unsigned_64 (Get_PSL_Nbr_States (Stmt)))),
                         Ghdl_Bool_Type));
      New_Assign_Stmt (New_Indexed_Element (New_Obj (Var_Nvec),
                                            New_Obj_Value (Var_I)),
                       New_Lit (Std_Boolean_False_Node));
      Inc_Var (Var_I);
      Finish_Loop_Stmt (Label);
      Finish_Declare_Stmt;

      -- Default "started" flag is not set
      New_Assign_Stmt (New_Obj (Var_SFlag), New_Lit (Ghdl_Bool_False_Node));

      -- Default simplified state -> Inactive
      New_Assign_Stmt (Get_Var (Info.Psl_State_Var),
                       New_Lit (Trans.Rtis.Ghdl_Rti_Psl_State_Inactive));

      --  For each state: if set, evaluate all outgoing edges.
      NFA := Get_PSL_NFA (Stmt);
      S := Get_First_State (NFA);
      while S /= No_State loop
         S_Num := Get_State_Label (S);
         Open_Temp;

         Start_If_Stmt
           (S_Blk,
            New_Value
              (New_Indexed_Element (Get_Var (Info.Psl_Vect_Var),
               New_Lit (New_Index_Lit
                 (Unsigned_64 (S_Num))))));

         -- Get simplified state:
         --  - If in transient state -> In progress.
         -- Set also if in final state, will be overrided later in
         -- failure check.
         if S /= Get_First_State(NFA) then
            New_Assign_Stmt (Get_Var (Info.Psl_State_Var),
                             New_Lit (Trans.Rtis.Ghdl_Rti_Psl_State_Running));
         end if;

         E := Get_First_Src_Edge (S);
         while E /= No_Edge loop
            Sd := Get_Edge_Dest (E);
            Open_Temp;

            D_Lit := New_Index_Lit (Unsigned_64 (Get_State_Label (Sd)));
            Cond := New_Monadic_Op
              (ON_Not,
               New_Value (New_Indexed_Element (New_Obj (Var_Nvec),
                 New_Lit (D_Lit))));
            Cond := New_Dyadic_Op
              (ON_And, Cond, Translate_Psl_Expr (Get_Edge_Expr (E), False));

            -- If NFA edge expression is valid -> Fire-up destination state.
            Start_If_Stmt (E_Blk, Cond);
            New_Assign_Stmt
              (New_Indexed_Element (New_Obj (Var_Nvec), New_Lit (D_Lit)),
               New_Lit (Std_Boolean_True_Node));
            -- If we fire from start state -> set "started" flag.
            if S = Get_First_State (NFA) then
               New_Assign_Stmt (New_Obj (Var_SFlag),
                                New_Lit (Ghdl_Bool_True_Node));
            end if;
            Finish_If_Stmt (E_Blk);

            Destroy_Types (Get_Edge_Expr (E));

            Close_Temp;
            E := Get_Next_Src_Edge (E);
         end loop;

         Finish_If_Stmt (S_Blk);
         Close_Temp;
         S := Get_Next_State (S);
      end loop;


      --  Check fail state.
      S := Get_Final_State (NFA);
      S_Num := Get_State_Label (S);
      pragma Assert (S_Num = Get_PSL_Nbr_States (Stmt) - 1);
      Cond := New_Value
        (New_Indexed_Element (New_Obj (Var_Nvec),
                              New_Lit (New_Index_Lit
                                         (Unsigned_64 (S_Num)))));

      if Get_Kind (Stmt) = Iir_Kind_Psl_Endpoint_Declaration then
         New_Assign_Stmt (Get_Var (Info.Psl_Finish_Count_Var), Cond);
      else
         Start_If_Stmt (S_Blk, Cond);
         Open_Temp;
         case Get_Kind (Stmt) is
            when Iir_Kind_Psl_Assert_Directive =>
               Chap8.Translate_Report
                 (Stmt, Ghdl_Psl_Assert_Failed, Severity_Level_Error);
               New_Assign_Stmt (
                  Get_Var (Info.Psl_State_Var),
                  New_Lit (Trans.Rtis.Ghdl_Rti_Psl_State_Failed));
            when Iir_Kind_Psl_Assume_Directive =>
               Call_Psl_Fail (Stmt, Ghdl_Psl_Assume_Failed);
               New_Assign_Stmt (
                  Get_Var (Info.Psl_State_Var),
                  New_Lit (Trans.Rtis.Ghdl_Rti_Psl_State_Failed));
            when Iir_Kind_Psl_Cover_Directive =>
               if Get_Report_Expression (Stmt) /= Null_Iir then
                  Start_Association (Assocs, Report_Proc);
                  New_Association (Assocs, New_Obj_Value (Instance));
                  New_Association (Assocs, New_Lit (Ghdl_Bool_True_Node));
                  New_Procedure_Call (Assocs);
               end if;
               New_Assign_Stmt (
                  Get_Var (Info.Psl_State_Var),
                  New_Lit (Trans.Rtis.Ghdl_Rti_Psl_State_Covered));
            when others =>
               Error_Kind ("Translate_Psl_Directive_Statement", Stmt);
         end case;
         New_Assign_Stmt
           (Get_Var (Info.Psl_Finish_Count_Var),
            New_Dyadic_Op (ON_Add_Ov,
                           New_Value (Get_Var (Info.Psl_Finish_Count_Var)),
                           New_Lit (Ghdl_Index_1)));
         Close_Temp;
         Finish_If_Stmt (S_Blk);

         -- Check "started" flag, increment started count if set
         Start_If_Stmt (S_Blk, New_Value (New_Obj (Var_SFlag)));
         New_Assign_Stmt
               (Get_Var (Info.Psl_Start_Count_Var),
               New_Dyadic_Op (ON_Add_Ov,
                              New_Value (Get_Var (Info.Psl_Start_Count_Var)),
                              New_Lit (Ghdl_Index_1)));
         Finish_If_Stmt (S_Blk);
      end if;

      --  Assign state vector.
      Start_Declare_Stmt;
      New_Var_Decl (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
      Init_Var (Var_I);
      Start_Loop_Stmt (Label);
      Gen_Exit_When
        (Label,
         New_Compare_Op (ON_Ge,
                         New_Obj_Value (Var_I),
                         New_Lit (New_Unsigned_Literal
                                    (Ghdl_Index_Type,
                                     Unsigned_64 (Get_PSL_Nbr_States (Stmt)))),
                         Ghdl_Bool_Type));
      New_Assign_Stmt
        (New_Indexed_Element (Get_Var (Info.Psl_Vect_Var),
         New_Obj_Value (Var_I)),
         New_Value (New_Indexed_Element (New_Obj (Var_Nvec),
                                         New_Obj_Value (Var_I))));
      Inc_Var (Var_I);
      Finish_Loop_Stmt (Label);
      Finish_Declare_Stmt;

      if Has_Sync_Abort then
         Finish_If_Stmt (Abort_Blk);
      end if;

      Finish_If_Stmt (Clk_Blk);

      if Has_Async_Abort then
         Finish_If_Stmt (Abort_Blk);
      end if;
      Close_Temp;

      Clear_Scope (Base.Block_Scope);
      Pop_Local_Factory;
      Finish_Subprogram_Body;

      --  The finalizer.
      case Get_Kind (Stmt) is
         when Iir_Kind_Psl_Assert_Directive
            | Iir_Kind_Psl_Assume_Directive =>
            if Get_PSL_EOS_Flag (Stmt) then
               Create_Psl_Final_Proc (Stmt, Base, Instance);

               Start_Subprogram_Body (Info.Psl_Proc_Final_Subprg);
               Push_Local_Factory;
               --  Push scope for architecture declarations.
               Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

               S := Get_Final_State (NFA);
               E := Get_First_Dest_Edge (S);
               while E /= No_Edge loop
                  Sd := Get_Edge_Src (E);

                  if PSL.NFAs.Utils.Has_EOS (Get_Edge_Expr (E)) then

                     S_Num := Get_State_Label (Sd);
                     Open_Temp;

                     Cond := New_Value
                       (New_Indexed_Element
                          (Get_Var (Info.Psl_Vect_Var),
                           New_Lit (New_Index_Lit (Unsigned_64 (S_Num)))));
                     Cond := New_Dyadic_Op
                       (ON_And, Cond,
                        Translate_Psl_Expr (Get_Edge_Expr (E), True));
                     Start_If_Stmt (E_Blk, Cond);
                     if Get_Kind (Stmt) = Iir_Kind_Psl_Assert_Directive then
                        Chap8.Translate_Report
                          (Stmt, Ghdl_Psl_Assert_Failed, Severity_Level_Error);
                     else
                        Call_Psl_Fail (Stmt, Ghdl_Psl_Assume_Failed);
                     end if;
                     New_Return_Stmt;
                     Finish_If_Stmt (E_Blk);

                     Close_Temp;
                  end if;

                  E := Get_Next_Dest_Edge (E);
               end loop;

               Clear_Scope (Base.Block_Scope);
               Pop_Local_Factory;
               Finish_Subprogram_Body;
            else
               Info.Psl_Proc_Final_Subprg := O_Dnode_Null;
            end if;

         when Iir_Kind_Psl_Cover_Directive =>
            Create_Psl_Final_Proc (Stmt, Base, Instance);

            Start_Subprogram_Body (Info.Psl_Proc_Final_Subprg);
            Push_Local_Factory;
            --  Push scope for architecture declarations.
            Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

            Start_If_Stmt
              (S_Blk,
               New_Compare_Op (ON_Eq,
                               New_Value (Get_Var (Info.Psl_Finish_Count_Var)),
                               New_Lit (Ghdl_Index_0),
                               Ghdl_Bool_Type));
            Start_Association (Assocs, Report_Proc);
            New_Association (Assocs, New_Obj_Value (Instance));
            New_Association (Assocs, New_Lit (Ghdl_Bool_False_Node));
            New_Procedure_Call (Assocs);
            Finish_If_Stmt (S_Blk);

            Clear_Scope (Base.Block_Scope);
            Pop_Local_Factory;
            Finish_Subprogram_Body;

         when Iir_Kind_Psl_Endpoint_Declaration =>
            Info.Psl_Proc_Final_Subprg := O_Dnode_Null;

         when others =>
            Error_Kind ("Translate_Psl_Directive_Statement(3)", Stmt);
      end case;
   end Translate_Psl_Directive_Statement;

   procedure Translate_If_Case_Generate_Statement_Body
     (Bod : Iir; Num : Int32; Origin : Iir)
   is
      Info : Block_Info_Acc;
      Mark2 : Id_Mark_Type;
   begin
      Info := Add_Info (Bod, Kind_Block);

      Push_Identifier_Prefix (Mark2, Get_Alternative_Label (Bod));

      Chap1.Start_Block_Decl (Bod);
      Push_Instance_Factory (Info.Block_Scope'Access);

      --  Add a parent field in the current instance.
      Info.Block_Origin_Field := Add_Instance_Factory_Field
        (Get_Identifier ("ORIGIN"),
         Get_Info (Origin).Block_Decls_Ptr_Type);

      Info.Block_Id := Num;

      Chap9.Translate_Block_Declarations (Bod, Bod);

      Pop_Instance_Factory (Info.Block_Scope'Access);

      Pop_Identifier_Prefix (Mark2);
   end Translate_If_Case_Generate_Statement_Body;

   procedure Translate_If_Case_Generate_Statement (Stmt : Iir)
   is
      Stmt_Info : Ortho_Info_Acc;
   begin
      Stmt_Info := Add_Info (Stmt, Kind_Generate);
      Stmt_Info.Generate_Parent_Field := Add_Instance_Factory_Field
        (Create_Identifier_Without_Prefix (Stmt), Ghdl_Ptr_Type);
      Stmt_Info.Generate_Body_Id := Add_Instance_Factory_Field
        (Create_Identifier_Without_Prefix (Get_Identifier (Stmt), "_ID"),
         Ghdl_Index_Type);
   end Translate_If_Case_Generate_Statement;

   procedure Translate_If_Generate_Statement (Stmt : Iir; Origin : Iir)
   is
      Clause : Iir;
      Bod : Iir;
      Mark : Id_Mark_Type;
      Num : Int32;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));

      Translate_If_Case_Generate_Statement (Stmt);

      --  Translate generate statement body.
      Num := 0;
      Clause := Stmt;
      while Clause /= Null_Iir loop
         Bod := Get_Generate_Statement_Body (Clause);
         Translate_If_Case_Generate_Statement_Body (Bod, Num, Origin);
         Clause := Get_Generate_Else_Clause (Clause);
         Num := Num + 1;
      end loop;

      Pop_Identifier_Prefix (Mark);
   end Translate_If_Generate_Statement;

   procedure Translate_Case_Generate_Statement (Stmt : Iir; Origin : Iir)
   is
      Alt : Iir;
      Bod : Iir;
      Mark : Id_Mark_Type;
      Num : Int32;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));

      Translate_If_Case_Generate_Statement (Stmt);

      --  Translate generate statement body.
      Num := 0;
      Alt := Get_Case_Statement_Alternative_Chain (Stmt);
      while Alt /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Alt) then
            Bod := Get_Associated_Block (Alt);
            Translate_If_Case_Generate_Statement_Body (Bod, Num, Origin);
            Num := Num + 1;
         end if;
         Alt := Get_Chain (Alt);
      end loop;

      Pop_Identifier_Prefix (Mark);
   end Translate_Case_Generate_Statement;

   procedure Translate_For_Generate_Statement (Stmt : Iir; Origin : Iir)
   is
      Bod : constant Iir := Get_Generate_Statement_Body (Stmt);
      Param : constant Iir := Get_Parameter_Specification (Stmt);
      Iter_Type : constant Iir := Get_Type (Param);
      Info      : Block_Info_Acc;
      Mark      : Id_Mark_Type;
      It_Info   : Ortho_Info_Acc;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));

      Chap3.Translate_Object_Subtype_Indication (Param, True);

      Info := Add_Info (Bod, Kind_Block);
      Chap1.Start_Block_Decl (Bod);
      Push_Instance_Factory (Info.Block_Scope'Access);

      --  Add a parent field in the current instance.  This is
      --  the first field (known by GRT).
      Info.Block_Origin_Field := Add_Instance_Factory_Field
        (Get_Identifier ("ORIGIN"),
         Get_Info (Origin).Block_Decls_Ptr_Type);

      --  Flag (if block was configured).
      Info.Block_Configured_Field := Add_Instance_Factory_Field
        (Get_Identifier ("CONFIGURED"), Ghdl_Bool_Type);

      --  Iterator.
      It_Info := Add_Info (Param, Kind_Iterator);
      It_Info.Iterator_Var := Create_Var
        (Create_Var_Identifier (Param),
         Get_Info (Get_Base_Type (Iter_Type)).Ortho_Type (Mode_Value));

      Chap9.Translate_Block_Declarations (Bod, Bod);

      Pop_Instance_Factory (Info.Block_Scope'Access);

      --  Create array type of block_decls_type
      Info.Block_Decls_Array_Type := New_Array_Type
        (Get_Scope_Type (Info.Block_Scope), Ghdl_Index_Type);
      New_Type_Decl (Create_Identifier ("INSTARRTYPE"),
                     Info.Block_Decls_Array_Type);
      --  Create access to the array type.
      Info.Block_Decls_Array_Ptr_Type := New_Access_Type
        (Info.Block_Decls_Array_Type);
      New_Type_Decl (Create_Identifier ("INSTARRPTR"),
                     Info.Block_Decls_Array_Ptr_Type);

      --  Add a field in the parent instance (Pop_Instance_Factory
      --  has already been called).  This is a pointer INSTARRPTR
      --  to an array INSTARRTYPE of instace.  The size of each
      --  element is stored in the RTI.
      Info.Block_Parent_Field := Add_Instance_Factory_Field
        (Create_Identifier_Without_Prefix (Stmt),
         Info.Block_Decls_Array_Ptr_Type);

      Pop_Identifier_Prefix (Mark);
   end Translate_For_Generate_Statement;

   procedure Translate_Block_Statement (Stmt : Iir; Origin : Iir)
   is
      Hdr   : constant Iir_Block_Header := Get_Block_Header (Stmt);
      Guard : constant Iir := Get_Guard_Decl (Stmt);
      Info  : Block_Info_Acc;
      Mark  : Id_Mark_Type;
   begin
      Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));

      Info := Add_Info (Stmt, Kind_Block);
      Chap1.Start_Block_Decl (Stmt);
      Push_Instance_Factory (Info.Block_Scope'Access);

      --  Implicit guard signal.
      if Guard /= Null_Iir then
         Chap4.Translate_Declaration (Guard);
      end if;

      --  generics, ports.
      if Hdr /= Null_Iir then
         Chap4.Translate_Generic_Chain (Hdr);
         Chap4.Translate_Port_Chain (Hdr);
      end if;

      Chap9.Translate_Block_Declarations (Stmt, Origin);

      Pop_Instance_Factory (Info.Block_Scope'Access);
      Pop_Identifier_Prefix (Mark);

      --  Create a field in the parent record.
      Add_Scope_Field (Create_Identifier_Without_Prefix (Stmt),
                       Info.Block_Scope);
   end Translate_Block_Statement;

   --  Create the instance for block BLOCK.
   --  ORIGIN can be either an entity, an architecture or a block statement.
   procedure Translate_Block_Declarations (Block : Iir; Origin : Iir)
   is
      El : Iir;
   begin
      Chap4.Translate_Declaration_Chain (Block);

      El := Get_Concurrent_Statement_Chain (Block);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Process_Statement
               | Iir_Kind_Sensitized_Process_Statement =>
               Translate_Process_Declarations (El);
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Restrict_Directive
               | Iir_Kind_Psl_Declaration =>
               null;
            when Iir_Kind_Psl_Assert_Directive
              | Iir_Kind_Psl_Assume_Directive
              | Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Translate_Psl_Directive_Declarations (El);
            when Iir_Kind_Component_Instantiation_Statement =>
               Translate_Component_Instantiation_Statement (El);
            when Iir_Kind_Block_Statement =>
               Translate_Block_Statement (El, Origin);
            when Iir_Kind_For_Generate_Statement =>
               Translate_For_Generate_Statement (El, Origin);
            when Iir_Kind_If_Generate_Statement =>
               Translate_If_Generate_Statement (El, Origin);
            when Iir_Kind_Case_Generate_Statement =>
               Translate_Case_Generate_Statement (El, Origin);
            when others =>
               Error_Kind ("translate_block_declarations", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Block_Declarations;

   procedure Translate_Component_Instantiation_Subprogram
     (Stmt : Iir; Base : Block_Info_Acc)
   is
      procedure Set_Component_Link (Ref_Scope : Var_Scope_Type;
                                    Comp_Field : O_Fnode)
      is
      begin
         New_Assign_Stmt
           (New_Selected_Element
              (New_Selected_Element (Get_Instance_Ref (Ref_Scope),
               Comp_Field),
               Rtis.Ghdl_Component_Link_Stmt),
            Rtis.Get_Context_Rti (Stmt));
      end Set_Component_Link;

      Info : constant Block_Info_Acc := Get_Info (Stmt);

      Parent      : constant Iir := Get_Parent (Stmt);
      Parent_Info : constant Block_Info_Acc := Get_Info (Parent);

      Line : constant Natural := Get_Line_Number (Stmt);

      Comp       : Iir;
      Comp_Info  : Comp_Info_Acc;
      Inter_List : O_Inter_List;
      Instance   : O_Dnode;
   begin
      --  Create the elaborator for the instantiation.
      New_Debug_Line_Decl (Line);

      Start_Procedure_Decl (Inter_List, Create_Identifier ("COMP_ELAB"),
                            O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Base.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Block_Elab_Subprg (Elab_Decls));

      Start_Subprogram_Body (Info.Block_Elab_Subprg (Elab_Decls));
      Push_Local_Factory;
      Set_Scope_Via_Param_Ptr (Base.Block_Scope, Instance);

      --  Add access to the instantiation-specific data.
      --  This is used only for anonymous subtype variables.
      if Has_Scope_Type (Info.Block_Scope) then
         Set_Scope_Via_Field (Info.Block_Scope,
                              Info.Block_Parent_Field,
                              Parent_Info.Block_Scope'Access);
      end if;

      Comp := Get_Instantiated_Unit (Stmt);
      if Is_Entity_Instantiation (Stmt) then
         --  This is a direct instantiation.
         Set_Component_Link (Parent_Info.Block_Scope,
                             Info.Block_Link_Field);
         Translate_Entity_Instantiation (Comp, Stmt, Stmt, Null_Iir);
      else
         Comp := Get_Named_Entity (Comp);
         Comp_Info := Get_Info (Comp);
         Set_Scope_Via_Field (Comp_Info.Comp_Scope,
                              Info.Block_Link_Field,
                              Parent_Info.Block_Scope'Access);

         --  Set the link from component declaration to component
         --  instantiation statement.
         Set_Component_Link (Comp_Info.Comp_Scope, Comp_Info.Comp_Link);

         Chap5.Elab_Map_Aspect (Comp, Stmt, Comp, (Comp_Info.Comp_Scope'Access,
                                                   Comp_Info.Comp_Scope));

         Clear_Scope (Comp_Info.Comp_Scope);
      end if;

      if Has_Scope_Type (Info.Block_Scope) then
         Clear_Scope (Info.Block_Scope);
      end if;

      Clear_Scope (Base.Block_Scope);
      Pop_Local_Factory;
      New_Debug_Line_Stmt (Line);
      Finish_Subprogram_Body;
   end Translate_Component_Instantiation_Subprogram;

   procedure Translate_Generate_Statement_Body_Subprograms
     (Bod : Iir; Base_Info : Block_Info_Acc)
   is
      Info : constant Block_Info_Acc := Get_Info (Bod);
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Subprgs.Push_Subprg_Instance (Info.Block_Scope'Access,
                                    Info.Block_Decls_Ptr_Type,
                                    Wki_Instance,
                                    Prev_Subprg_Instance);
      Set_Scope_Via_Field_Ptr (Base_Info.Block_Scope,
                               Info.Block_Origin_Field,
                               Info.Block_Scope'Access);
      Translate_Block_Subprograms (Bod, Bod);
      Clear_Scope (Base_Info.Block_Scope);
      Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
   end Translate_Generate_Statement_Body_Subprograms;

   --  Translate concurrent statements into subprograms.
   procedure Translate_Block_Subprograms (Block : Iir; Base_Block : Iir)
   is
      Base_Info : constant Block_Info_Acc := Get_Info (Base_Block);
      Stmt      : Iir;
      Mark      : Id_Mark_Type;
      Kind      : Iir_Kind;
   begin
      Chap4.Translate_Declaration_Chain_Subprograms
        (Block, Subprg_Translate_Spec_And_Body);

      Stmt := Get_Concurrent_Statement_Chain (Block);
      while Stmt /= Null_Iir loop
         Kind := Get_Kind (Stmt);
         case Kind is
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when others =>
               Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
         end case;

         case Kind is
            when Iir_Kind_Process_Statement
               | Iir_Kind_Sensitized_Process_Statement =>
               if Flag_Direct_Drivers then
                  Chap9.Set_Direct_Drivers (Stmt);
               end if;

               Chap4.Translate_Declaration_Chain_Subprograms
                 (Stmt, Subprg_Translate_Spec_And_Body);
               Translate_Process_Statement (Stmt, Base_Info);

               if Flag_Direct_Drivers then
                  Chap9.Reset_Direct_Drivers (Stmt);
               end if;
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Restrict_Directive
               | Iir_Kind_Psl_Declaration =>
               null;
            when Iir_Kind_Psl_Assert_Directive
              | Iir_Kind_Psl_Assume_Directive
              | Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Translate_Psl_Directive_Statement (Stmt, Base_Info);
            when Iir_Kind_Component_Instantiation_Statement =>
               Chap4.Translate_Association_Subprograms
                 (Stmt, Block, Base_Block,
                  Get_Entity_From_Entity_Aspect
                    (Get_Instantiated_Unit (Stmt)));
               Translate_Component_Instantiation_Subprogram
                 (Stmt, Base_Info);
            when Iir_Kind_Block_Statement =>
               declare
                  Guard : constant Iir := Get_Guard_Decl (Stmt);
                  Hdr   : constant Iir := Get_Block_Header (Stmt);
               begin
                  if Guard /= Null_Iir then
                     Translate_Implicit_Guard_Signal (Guard, Base_Info);
                  end if;
                  if Hdr /= Null_Iir then
                     Chap4.Translate_Association_Subprograms
                       (Hdr, Block, Base_Block, Null_Iir);
                  end if;
                  Translate_Block_Subprograms (Stmt, Base_Block);
               end;
            when Iir_Kind_For_Generate_Statement =>
               Translate_Generate_Statement_Body_Subprograms
                 (Get_Generate_Statement_Body (Stmt), Base_Info);
            when Iir_Kind_If_Generate_Statement =>
               declare
                  Clause : Iir;
                  Bod : Iir;
                  Mark2 : Id_Mark_Type;
               begin
                  Clause := Stmt;
                  while Clause /= Null_Iir loop
                     Bod := Get_Generate_Statement_Body (Clause);
                     Push_Identifier_Prefix
                       (Mark2, Get_Alternative_Label (Bod));
                     Translate_Generate_Statement_Body_Subprograms
                       (Bod, Base_Info);
                     Pop_Identifier_Prefix (Mark2);
                     Clause := Get_Generate_Else_Clause (Clause);
                  end loop;
               end;
            when Iir_Kind_Case_Generate_Statement =>
               declare
                  Alt : Iir;
                  Bod : Iir;
                  Mark2 : Id_Mark_Type;
               begin
                  Alt := Get_Case_Statement_Alternative_Chain (Stmt);
                  while Alt /= Null_Iir loop
                     if not Get_Same_Alternative_Flag (Alt) then
                        Bod := Get_Associated_Block (Alt);
                        Push_Identifier_Prefix
                          (Mark2, Get_Alternative_Label (Bod));
                        Translate_Generate_Statement_Body_Subprograms
                          (Bod, Base_Info);
                        Pop_Identifier_Prefix (Mark2);
                     end if;
                     Alt := Get_Chain (Alt);
                  end loop;
               end;
            when others =>
               Error_Kind ("translate_block_subprograms", Stmt);
         end case;
         case Kind is
            when Iir_Kind_Psl_Default_Clock =>
               null;
            when others =>
               Pop_Identifier_Prefix (Mark);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Translate_Block_Subprograms;

   --  Remove anonymous and implicit type definitions in a list of names.
   --  Such type definitions are created during slice translations, however
   --  variables created are defined in the translation scope.
   --  If the type is referenced again, the variables must be reachable.
   --  This is not the case for elaborator subprogram (which may references
   --  slices in the sensitivity or driver list) and the process subprg.
   procedure Destroy_Types_In_Chain (Chain : Iir)
   is
      N : Iir;
   begin
      N := Chain;
      while N /= Null_Iir loop
         Destroy_Types (N);
         N := Get_Chain (N);
      end loop;
   end Destroy_Types_In_Chain;

   procedure Destroy_Types_In_List (L : Iir_List)
   is
      El : Iir;
      It : List_Iterator;
   begin
      case L is
         when Null_Iir_List
            | Iir_List_All =>
            return;
         when others =>
            It := List_Iterate (L);
            while Is_Valid (It) loop
               El := Get_Element (It);
               Destroy_Types (El);
               Next (It);
            end loop;
      end case;
   end Destroy_Types_In_List;

   procedure Destroy_Types_In_Flist (L : Iir_Flist)
   is
      El : Iir;
   begin
      case L is
         when Null_Iir_Flist
            | Iir_Flist_All
            | Iir_Flist_Others =>
            return;
         when others =>
            for I in Flist_First .. Flist_Last (L) loop
               El := Get_Nth_Element (L, I);
               Destroy_Types (El);
            end loop;
      end case;
   end Destroy_Types_In_Flist;

   procedure Destroy_Types (N : Iir) is
   begin
      --  Nothing to do for null node.
      if N = Null_Iir then
         return;
      end if;

      declare
         use Vhdl.Nodes_Meta;
         Kind      : constant Iir_Kind := Get_Kind (N);
         Fields    : constant Fields_Array := Get_Fields (Kind);
         F         : Fields_Enum;
      begin
         case Kind is
            when Iir_Kind_Object_Alias_Declaration =>
               --  No types to free, don't try to recurse as the name can be
               --  a slice (which will then be freed).
               return;
            when others =>
               null;
         end case;

         for I in Fields'Range loop
            F := Fields (I);
            case F is
               when Field_Literal_Subtype
                 | Field_Slice_Subtype
                 | Field_Allocator_Subtype =>
                  declare
                     T : constant Iir := Get_Iir (N, F);
                     Info : Type_Info_Acc;
                  begin
                     Info := Get_Info (T);
                     if Info /= null then
                        Free_Type_Info (Info);
                        Clear_Info (T);
                     end if;
                  end;
               when others =>
                  null;
            end case;

            case Get_Field_Type (F) is
               when Type_Iir =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Destroy_Types (Get_Iir (N, F));
                     when Attr_Ref
                       | Attr_Forward_Ref
                       | Attr_Maybe_Forward_Ref =>
                        null;
                     when Attr_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Destroy_Types (Get_Iir (N, F));
                        end if;
                     when Attr_Chain =>
                        Destroy_Types_In_Chain (Get_Iir (N, F));
                     when Attr_Chain_Next =>
                        null;
                     when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_List =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Destroy_Types_In_List (Get_Iir_List (N, F));
                     when Attr_Of_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Destroy_Types_In_List (Get_Iir_List (N, F));
                        end if;
                     when Attr_Ref
                        | Attr_Of_Ref =>
                        null;
                     when others =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_Flist =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Destroy_Types_In_Flist (Get_Iir_Flist (N, F));
                     when Attr_Of_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Destroy_Types_In_Flist (Get_Iir_Flist (N, F));
                        end if;
                     when Attr_Ref
                        | Attr_Of_Ref =>
                        null;
                     when others =>
                        raise Internal_Error;
                  end case;
               when Type_PSL_NFA
                 | Type_PSL_Node =>
                  --  For endpoint in PSL HDL_Expr.
                  null;
               when Type_Date_Type
                 | Type_Date_State_Type
                 | Type_Time_Stamp_Id
                 | Type_File_Checksum_Id
                 | Type_String8_Id
                 | Type_Source_Ptr
                 | Type_Source_File_Entry
                 | Type_Number_Base_Type
                 | Type_Iir_Constraint
                 | Type_Iir_Mode
                 | Type_Iir_Index32
                 | Type_Int64
                 | Type_Boolean
                 | Type_Iir_Staticness
                 | Type_Iir_All_Sensitized
                 | Type_Iir_Signal_Kind
                 | Type_Tri_State_Type
                 | Type_Iir_Pure_State
                 | Type_Iir_Delay_Mechanism
                 | Type_Iir_Force_Mode
                 | Type_Iir_Predefined_Functions
                 | Type_Direction_Type
                 | Type_Iir_Int32
                 | Type_Int32
                 | Type_Fp64
                 | Type_Token_Type
                 | Type_Scalar_Size
                 | Type_Name_Id =>
                  null;
            end case;
         end loop;
      end;
   end Destroy_Types;

   procedure Destroy_Types (N : PSL_Node)
   is
      use PSL.Nodes;
   begin
      case Get_Kind (N) is
         when N_HDL_Bool
           | N_HDL_Expr =>
            Destroy_Types (Get_HDL_Node (N));
         when N_True =>
            null;
         when N_EOS =>
            null;
         when N_Not_Bool =>
            Destroy_Types (Get_Boolean (N));
         when N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool =>
            Destroy_Types (Get_Left (N));
            Destroy_Types (Get_Right (N));
         when others =>
            Error_Kind ("destroy_types(psl)", N);
      end case;
   end Destroy_Types;

   function Foreach_Non_Composite_Prepare_Data_Array_Mnode
     (Targ : Mnode; Targ_Type : Iir; Val : Mnode) return Mnode
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      return Val;
   end Foreach_Non_Composite_Prepare_Data_Array_Mnode;

   function Foreach_Non_Composite_Prepare_Data_Record_Mnode
     (Targ : Mnode; Targ_Type : Iir; Val : Mnode) return Mnode
   is
      pragma Unreferenced (Targ, Targ_Type);
   begin
      if Val = Mnode_Null then
         return Mnode_Null;
      else
         return Stabilize (Val);
      end if;
   end Foreach_Non_Composite_Prepare_Data_Record_Mnode;

   function Foreach_Non_Composite_Update_Data_Array_Mnode
     (Val : Mnode; Targ_Type : Iir; Index : O_Dnode) return Mnode is
   begin
      if Val = Mnode_Null then
         return Mnode_Null;
      else
         return Chap6.Translate_Indexed_Name_By_Offset
           (Chap6.Stabilize_If_Unbounded (Val), Targ_Type, Index);
      end if;
   end Foreach_Non_Composite_Update_Data_Array_Mnode;

   function Foreach_Non_Composite_Update_Data_Record_Mnode
     (Val : Mnode; Targ_Type : Iir; El : Iir_Element_Declaration)
     return Mnode
   is
      pragma Unreferenced (Targ_Type);
   begin
      if Val = Mnode_Null then
         return Mnode_Null;
      else
         return Chap6.Translate_Selected_Element (Val, El);
      end if;
   end Foreach_Non_Composite_Update_Data_Record_Mnode;

   procedure Gen_Register_Direct_Driver_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Drv : Mnode)
   is
      pragma Unreferenced (Targ_Type);
      Constr : O_Assoc_List;
   begin
      Start_Association (Constr, Ghdl_Signal_Add_Direct_Driver);
      New_Association
        (Constr, New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      New_Association
        (Constr, New_Unchecked_Address (M2Lv (Drv), Ghdl_Ptr_Type));
      New_Procedure_Call (Constr);
   end Gen_Register_Direct_Driver_Non_Composite;

   procedure Gen_Register_Direct_Driver is new Foreach_Non_Composite
     (Data_Type => Mnode,
      Composite_Data_Type => Mnode,
      Do_Non_Composite => Gen_Register_Direct_Driver_Non_Composite,
      Prepare_Data_Array => Foreach_Non_Composite_Prepare_Data_Array_Mnode,
      Update_Data_Array => Foreach_Non_Composite_Update_Data_Array_Mnode,
      Prepare_Data_Record => Foreach_Non_Composite_Prepare_Data_Record_Mnode,
      Update_Data_Record => Foreach_Non_Composite_Update_Data_Record_Mnode);

   procedure Gen_Add_Port_Driver_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Init : O_Enode)
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Targ_Type);
      Constr : O_Assoc_List;
      Init_Subprg : O_Dnode;
      Conv : O_Tnode;
   begin
      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
           | Type_Mode_P32 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
           | Type_Mode_I64 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Init_Subprg := Ghdl_Signal_Add_Port_Driver_F64;
            Conv := Ghdl_Real_Type;
         when others =>
            Error_Kind ("gen_add_port_driver_non_composite", Targ_Type);
      end case;

      Start_Association (Constr, Init_Subprg);
      New_Association
        (Constr, New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      New_Association (Constr, New_Convert_Ov (Init, Conv));
      New_Procedure_Call (Constr);
   end Gen_Add_Port_Driver_Non_Composite;

   procedure Gen_Add_Port_Driver_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Init : Mnode) is
   begin
      Gen_Add_Port_Driver_Non_Composite (Targ, Targ_Type, M2E (Init));
   end Gen_Add_Port_Driver_Non_Composite;

   procedure Gen_Add_Port_Driver is new Foreach_Non_Composite
     (Data_Type => Mnode,
      Composite_Data_Type => Mnode,
      Do_Non_Composite => Gen_Add_Port_Driver_Non_Composite,
      Prepare_Data_Array => Foreach_Non_Composite_Prepare_Data_Array_Mnode,
      Update_Data_Array => Foreach_Non_Composite_Update_Data_Array_Mnode,
      Prepare_Data_Record => Foreach_Non_Composite_Prepare_Data_Record_Mnode,
      Update_Data_Record => Foreach_Non_Composite_Update_Data_Record_Mnode);

   type Add_Port_Driver_Default_Data is null record;

   procedure Gen_Add_Port_Driver_Non_Composite_Default
     (Targ : Mnode; Targ_Type : Iir; Init : Add_Port_Driver_Default_Data)
   is
      pragma Unreferenced (Init);
   begin
      Gen_Add_Port_Driver_Non_Composite
        (Targ, Targ_Type, Chap4.Get_Scalar_Initial_Value (Targ_Type));
   end Gen_Add_Port_Driver_Non_Composite_Default;

   function Gen_Add_Port_Driver_Prepare_Data_Composite_Default
     (Targ : Mnode; Targ_Type : Iir; Data : Add_Port_Driver_Default_Data)
     return Add_Port_Driver_Default_Data
   is
      pragma Unreferenced (Targ);
      pragma Unreferenced (Targ_Type);
   begin
      return Data;
   end Gen_Add_Port_Driver_Prepare_Data_Composite_Default;

   function Gen_Add_Port_Driver_Update_Data_Array_Default
     (Data : Add_Port_Driver_Default_Data; Targ_Type : Iir; Index : O_Dnode)
     return Add_Port_Driver_Default_Data
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (Index);
   begin
      return Data;
   end Gen_Add_Port_Driver_Update_Data_Array_Default;

   function Gen_Add_Port_Driver_Update_Data_Record_Default
     (Data : Add_Port_Driver_Default_Data;
      Targ_Type : Iir;
      El : Iir_Element_Declaration)
     return Add_Port_Driver_Default_Data
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (El);
   begin
      return Data;
   end Gen_Add_Port_Driver_Update_Data_Record_Default;

   procedure Gen_Add_Port_Driver_Default is new Foreach_Non_Composite
     (Data_Type => Add_Port_Driver_Default_Data,
      Composite_Data_Type => Add_Port_Driver_Default_Data,
      Do_Non_Composite => Gen_Add_Port_Driver_Non_Composite_Default,
      Prepare_Data_Array =>
        Gen_Add_Port_Driver_Prepare_Data_Composite_Default,
      Update_Data_Array =>
        Gen_Add_Port_Driver_Update_Data_Array_Default,
      Prepare_Data_Record =>
        Gen_Add_Port_Driver_Prepare_Data_Composite_Default,
      Update_Data_Record =>
        Gen_Add_Port_Driver_Update_Data_Record_Default);

   procedure Gen_Port_Init_Driving_Scalar
     (Targ : Mnode; Targ_Type : Iir; Init : Mnode)
   is
      Type_Info : constant Type_Info_Acc := Get_Info (Targ_Type);
      Assoc : O_Assoc_List;
      Init_Subprg : O_Dnode;
      Init_Val : O_Enode;
      Conv : O_Tnode;
   begin
      case Type_Info.Type_Mode is
         when Type_Mode_B1 =>
            Init_Subprg := Ghdl_Signal_Init_B1;
            Conv := Ghdl_Bool_Type;
         when Type_Mode_E8 =>
            Init_Subprg := Ghdl_Signal_Init_E8;
            Conv := Ghdl_I32_Type;
         when Type_Mode_E32 =>
            Init_Subprg := Ghdl_Signal_Init_E32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_I32
           | Type_Mode_P32 =>
            Init_Subprg := Ghdl_Signal_Init_I32;
            Conv := Ghdl_I32_Type;
         when Type_Mode_P64
           | Type_Mode_I64 =>
            Init_Subprg := Ghdl_Signal_Init_I64;
            Conv := Ghdl_I64_Type;
         when Type_Mode_F64 =>
            Init_Subprg := Ghdl_Signal_Init_F64;
            Conv := Ghdl_Real_Type;
         when others =>
            Error_Kind ("merge_signals_rti_non_composite", Targ_Type);
      end case;

      --  Init the signal.
      Start_Association (Assoc, Init_Subprg);
      New_Association
        (Assoc,
         New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      if Init /= Mnode_Null then
         Init_Val := M2E (Init);
      else
         Init_Val := Chap4.Get_Scalar_Initial_Value (Targ_Type);
      end if;
      New_Association (Assoc, New_Convert_Ov (Init_Val, Conv));
      New_Procedure_Call (Assoc);
   end Gen_Port_Init_Driving_Scalar;

   procedure Gen_Port_Init_Driving_1 is new Foreach_Non_Composite
     (Data_Type => Mnode,
      Composite_Data_Type => Mnode,
      Do_Non_Composite => Gen_Port_Init_Driving_Scalar,
      Prepare_Data_Array => Foreach_Non_Composite_Prepare_Data_Array_Mnode,
      Update_Data_Array => Foreach_Non_Composite_Update_Data_Array_Mnode,
      Prepare_Data_Record => Foreach_Non_Composite_Prepare_Data_Record_Mnode,
      Update_Data_Record => Foreach_Non_Composite_Update_Data_Record_Mnode);

   procedure Gen_Port_Init_Driving
     (Port : Mnode; Port_Type : Iir; Init : Mnode)
     renames Gen_Port_Init_Driving_1;

   --    procedure Register_Scalar_Direct_Driver (Sig : Mnode;
   --                                             Sig_Type : Iir;
   --                                             Drv : Mnode)
   --    is
   --       pragma Unreferenced (Sig_Type);
   --       Constr : O_Assoc_List;
   --    begin
   --       Start_Association (Constr, Ghdl_Signal_Add_Direct_Driver);
   --       New_Association
   --  (Constr, New_Convert_Ov (New_Value (M2Lv (Sig)), Ghdl_Signal_Ptr));
   --       New_Association
   --         (Constr, New_Unchecked_Address (M2Lv (Drv), Ghdl_Ptr_Type));
   --       New_Procedure_Call (Constr);
   --    end Register_Scalar_Direct_Driver;

   --  PROC: the process to be elaborated
   --  BASE_INFO: info for the global block
   procedure Elab_Process (Proc : Iir; Base_Info : Block_Info_Acc)
   is
      Info          : constant Proc_Info_Acc := Get_Info (Proc);
      Is_Sensitized : constant Boolean :=
        Get_Kind (Proc) = Iir_Kind_Sensitized_Process_Statement;
      Subprg        : O_Dnode;
      Constr        : O_Assoc_List;
      List          : Iir_List;
      List_Orig     : Iir_List;
      Final         : Boolean;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Proc));

      --  Register process.
      if Is_Sensitized then
         if Get_Postponed_Flag (Proc) then
            Subprg := Ghdl_Postponed_Sensitized_Process_Register;
         else
            Subprg := Ghdl_Sensitized_Process_Register;
         end if;
      else
         if Get_Postponed_Flag (Proc) then
            Subprg := Ghdl_Postponed_Process_Register;
         else
            Subprg := Ghdl_Process_Register;
         end if;
      end if;

      Start_Association (Constr, Subprg);
      New_Association
        (Constr, New_Unchecked_Address
           (Get_Instance_Ref (Base_Info.Block_Scope), Ghdl_Ptr_Type));
      New_Association
        (Constr,
         New_Lit (New_Subprogram_Address (Info.Process_Subprg,
                                          Ghdl_Ptr_Type)));
      Rtis.Associate_Rti_Context (Constr, Proc);
      New_Procedure_Call (Constr);

      --  First elaborate declarations since a driver may depend on
      --  an alias declaration.
      --  Also, with vhdl 08 a sensitivity element may depend on an alias.
      Open_Temp;
      Chap4.Elab_Declaration_Chain (Proc, Final);
      Close_Temp;

      --  Register drivers.
      if Flag_Direct_Drivers then
         Chap9.Set_Direct_Drivers (Proc);

         for I in Info.Process_Drivers.all'Range loop
            declare
               Sig : constant Iir := Info.Process_Drivers (I).Sig;
               Base : constant Iir := Get_Object_Prefix (Sig);
               Sig_Node, Drv_Node, Init_Node : Mnode;
               Base_Type : Iir;
            begin
               Open_Temp;
               Chap9.Destroy_Types (Sig);
               if Info.Process_Drivers (I).Var /= Null_Var then
                  --  Elaborate direct driver.  Done only once.
                  Chap4.Elab_Direct_Driver_Declaration_Storage (Base);

                  --  Initial value.
                  Drv_Node := Chap6.Get_Signal_Direct_Driver (Base);
                  Base_Type := Get_Type (Base);
                  if Get_Kind (Base) = Iir_Kind_Interface_Signal_Declaration
                  then
                     --  From the port default value.
                     if Is_Valid (Get_Default_Value (Base)) then
                        Chap3.Translate_Object_Copy
                          (Drv_Node, Chap6.Get_Port_Init_Value (Base),
                           Base_Type);
                     else
                        Chap4.Init_Object (Drv_Node, Base_Type);
                     end if;
                  else
                     --  From the signal value.
                     Chap3.Translate_Object_Copy
                       (Drv_Node, Chap7.Translate_Expression (Base),
                        Base_Type);
                  end if;
               end if;
               if Chap4.Has_Direct_Driver (Base) then
                  --  Signal has a direct driver.
                  Chap6.Translate_Direct_Driver (Sig, Sig_Node, Drv_Node);
                  Gen_Register_Direct_Driver
                    (Sig_Node, Get_Type (Sig), Drv_Node);
               else
                  --  TODO (issue328): add default value
                  if Get_Kind (Base) = Iir_Kind_Interface_Signal_Declaration
                  then
                     if Is_Valid (Get_Default_Value (Base)) then
                        Chap6.Translate_Port_Init
                          (Sig, Sig_Node, Init_Node);
                        Gen_Add_Port_Driver
                          (Sig_Node, Get_Type (Sig), Init_Node);
                     else
                        Sig_Node := Chap6.Translate_Name (Sig, Mode_Signal);

                        --  At least GNAT GPL 2017 reports this warning:
                        --   'others choices is redundant'
                        pragma Warnings (Off);
                        Gen_Add_Port_Driver_Default
                          (Sig_Node, Get_Type (Sig), (others => <>));
                        pragma Warnings (On);
                     end if;
                  else
                     Register_Signal (Chap6.Translate_Name (Sig, Mode_Signal),
                                      Get_Type (Sig),
                                      Ghdl_Process_Add_Driver);
                  end if;
               end if;
               Close_Temp;
            end;
         end loop;

         Chap9.Reset_Direct_Drivers (Proc);
      else
         List := Trans_Analyzes.Extract_Drivers (Proc);
         Destroy_Types_In_List (List);
         Register_Signal_List (List, Ghdl_Process_Add_Driver);
         if Flag_Dump_Drivers then
            Trans_Analyzes.Dump_Drivers (Proc, List);
         end if;
         Trans_Analyzes.Free_Drivers_List (List);
      end if;

      if Is_Sensitized then
         List_Orig := Get_Sensitivity_List (Proc);
         if List_Orig = Iir_List_All then
            List := Vhdl.Canon.Canon_Extract_Sensitivity_Process (Proc);
         else
            List := List_Orig;
         end if;
         --  For extracted sensitivity, any signal can appear in the list.
         --  Remove transient types now.
         Destroy_Types_In_List (List);
         Register_Signal_List (List, Ghdl_Process_Add_Sensitivity);
         if List_Orig = Iir_List_All then
            Destroy_Iir_List (List);
         end if;
      else
         --  Initialize state.
         New_Assign_Stmt
           (Get_Var (Info.Process_State), New_Lit (Ghdl_Index_0));
      end if;
   end Elab_Process;

   procedure Elab_Inertial_Association (Assoc : Iir; Formal : Iir)
   is
      Info      : constant Inertial_Info_Acc := Get_Info (Assoc);
      Constr        : O_Assoc_List;
      List          : Iir_List;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Assoc));

      --  Register proc.
      Start_Association (Constr, Ghdl_Sensitized_Process_Register);
      New_Association
        (Constr, New_Convert_Ov (Get_Instance_Access (Info.Inertial_Block),
                                 Ghdl_Ptr_Type));
      New_Association
        (Constr,
         New_Lit (New_Subprogram_Address (Info.Inertial_Proc,
                                          Ghdl_Ptr_Type)));
      Rtis.Associate_Null_Rti_Context (Constr);
      New_Procedure_Call (Constr);

      --  Driver
      Register_Signal (Chap6.Translate_Name (Formal, Mode_Signal),
                       Get_Type (Formal),
                       Ghdl_Process_Add_Driver);

      --  Sensitivity
      List := Create_Iir_List;
      Vhdl.Canon.Canon_Extract_Sensitivity_Expression
        (Get_Actual (Assoc), List, False);
      --  For extracted sensitivity, any signal can appear in the list.
      --  Remove transient types now.
      Destroy_Types_In_List (List);
      Register_Signal_List (List, Ghdl_Process_Add_Sensitivity);
      Destroy_Iir_List (List);
   end Elab_Inertial_Association;

   --  PROC: the process to be elaborated
   --  BLOCK: the block containing the process (its parent)
   --  BASE_INFO: info for the global block
   procedure Elab_Psl_Directive (Stmt      : Iir;
                                 Base_Info : Block_Info_Acc)
   is
      Info   : constant Psl_Info_Acc := Get_Info (Stmt);
      Constr : O_Assoc_List;
      List   : Iir_List;
      Init   : O_Cnode;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Stmt));

      --  Register process.
      Start_Association (Constr, Ghdl_Sensitized_Process_Register);
      New_Association
        (Constr, New_Unchecked_Address
           (Get_Instance_Ref (Base_Info.Block_Scope), Ghdl_Ptr_Type));
      New_Association
        (Constr,
         New_Lit (New_Subprogram_Address (Info.Psl_Proc_Subprg,
           Ghdl_Ptr_Type)));
      Rtis.Associate_Rti_Context (Constr, Stmt);
      New_Procedure_Call (Constr);

      --  Register clock sensitivity.
      List := Get_PSL_Clock_Sensitivity (Stmt);
      Destroy_Types_In_List (List);
      Register_Signal_List (List, Ghdl_Process_Add_Sensitivity);

      --  Register async sensitivity.
      if Get_Kind (Stmt) in Iir_Kinds_Psl_Property_Directive
        and then Get_PSL_Abort_Flag (Stmt)
      then
         declare
            use PSL.Nodes;
            Prop : constant PSL_Node := Get_Psl_Property (Stmt);
            List : Iir_List;
         begin
            if PSL.Subsets.Is_Async_Abort (Prop) then
               List := Create_Iir_List;
               Vhdl.Canon_PSL.Canon_Extract_Sensitivity
                 (Get_Boolean (Prop), List);
               Destroy_Types_In_List (List);
               Register_Signal_List (List, Ghdl_Process_Add_Sensitivity);
               Destroy_Iir_List (List);
            end if;
         end;
      end if;

      --  Register finalizer (if any).
      if Info.Psl_Proc_Final_Subprg /= O_Dnode_Null then
         Start_Association (Constr, Ghdl_Finalize_Register);
         New_Association
           (Constr, New_Unchecked_Address
              (Get_Instance_Ref (Base_Info.Block_Scope),
               Ghdl_Ptr_Type));
         New_Association
           (Constr,
            New_Lit (New_Subprogram_Address (Info.Psl_Proc_Final_Subprg,
              Ghdl_Ptr_Type)));
         New_Procedure_Call (Constr);
      end if;

      --  Initialize state vector.
      Elab_PSL_State_Vector (Stmt);

      if Get_Kind (Stmt) = Iir_Kind_Psl_Endpoint_Declaration then
         Init := Std_Boolean_False_Node;
      else
         Init := Ghdl_Index_0;
      end if;
      New_Assign_Stmt (Get_Var (Info.Psl_Finish_Count_Var), New_Lit (Init));
   end Elab_Psl_Directive;

   procedure Elab_Implicit_Guard_Signal
     (Block : Iir_Block_Statement; Block_Info : Block_Info_Acc)
   is
      Guard     : constant Iir := Get_Guard_Decl (Block);
      Info      : constant Signal_Info_Acc := Get_Info (Guard);
      Type_Info : constant Type_Info_Acc := Get_Info (Get_Type (Guard));
      Constr    : O_Assoc_List;
   begin
      --  Create the guard signal.
      Start_Association (Constr, Ghdl_Signal_Create_Guard);
      New_Association
        (Constr, New_Unchecked_Address (Get_Var (Info.Signal_Val),
                                        Ghdl_Ptr_Type));
      New_Association
        (Constr, New_Unchecked_Address
           (Get_Instance_Ref (Block_Info.Block_Scope), Ghdl_Ptr_Type));
      New_Association
        (Constr,
         New_Lit (New_Subprogram_Address (Info.Signal_Function,
                                          Ghdl_Ptr_Type)));
      --         New_Association (Constr, Chap6.Get_Instance_Name_Ref (Block));
      New_Assign_Stmt (Get_Var (Info.Signal_Sig),
                       New_Convert_Ov (New_Function_Call (Constr),
                                       Type_Info.Ortho_Type (Mode_Signal)));

      --  Register sensitivity list of the guard signal.
      Register_Signal_List (Get_Guard_Sensitivity_List (Guard),
                            Ghdl_Signal_Guard_Dependence);
   end Elab_Implicit_Guard_Signal;

   procedure Translate_Entity_Instantiation
     (Aspect : Iir; Mapping : Iir; Parent : Iir; Config_Override : Iir)
   is
      Entity_Unit : Iir_Design_Unit;
      Config      : Iir;
      Arch        : Iir;
      Entity      : Iir_Entity_Declaration;
      Entity_Info : Block_Info_Acc;
      Arch_Info   : Block_Info_Acc;

      Instance_Size    : O_Dnode;
      Arch_Elab        : O_Dnode_Elab;
      Arch_Config      : O_Dnode;
      Arch_Config_Type : O_Tnode;

      Var_Sub : O_Dnode;
   begin
      --  Extract entity, architecture and configuration from
      --  binding aspect.
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Entity := Get_Entity (Aspect);
            Arch := Get_Architecture (Aspect);
            if Arch = Null_Iir then
               if Flags.Flag_Elaborate then
                  --  This is valid only during elaboration.
                  Arch := Libraries.Get_Latest_Architecture (Entity);
               end if;
            else
               if Is_Valid (Get_Named_Entity (Arch)) then
                  Arch := Get_Named_Entity (Arch);
               end if;
            end if;
            Config := Null_Iir;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Config := Get_Configuration (Aspect);
            Entity := Get_Entity (Config);
            Arch := Strip_Denoting_Name
              (Get_Block_Specification (Get_Block_Configuration (Config)));
         when Iir_Kind_Entity_Aspect_Open =>
            return;
         when others =>
            Error_Kind ("translate_entity_instantiation", Aspect);
      end case;
      Entity_Unit := Get_Design_Unit (Entity);
      Entity_Info := Get_Info (Entity);
      if Config_Override /= Null_Iir then
         Config := Config_Override;
         if Get_Kind (Arch) = Iir_Kind_Simple_Name then
            Arch := Get_Block_Specification
              (Get_Block_Configuration (Config));
         end if;
      end if;

      --  1) Create instance for the arch
      if Arch /= Null_Iir then
         Arch_Info := Get_Info (Arch);
         if Config = Null_Iir
           and then Get_Kind (Arch) = Iir_Kind_Architecture_Body
         then
            Config := Get_Default_Configuration_Declaration (Arch);
            if Config /= Null_Iir then
               Config := Get_Library_Unit (Config);
            end if;
         end if;
      else
         Arch_Info := null;
      end if;
      if Arch_Info = null or Config = Null_Iir then
         declare
            function Get_Arch_Name return String is
            begin
               if Arch /= Null_Iir then
                  return "ARCH__" & Identifier_To_String (Arch);
               else
                  return "LASTARCH";
               end if;
            end Get_Arch_Name;

            Entity_Library : constant Iir :=
              Get_Library (Get_Design_File (Entity_Unit));
            Str       : constant String :=
              Identifier_To_String (Entity_Library) & "__"
              & Identifier_To_String (Entity) & "__"
              & Get_Arch_Name & "__";
            Sub_Inter : O_Inter_List;
            Arg       : O_Dnode;
            Id : O_Ident;
         begin
            if Arch_Info = null then
               New_Const_Decl
                 (Instance_Size, Get_Identifier (Str & "INSTSIZE"),
                  O_Storage_External, Ghdl_Index_Type);

               for K in Elab_Kind loop
                  case K is
                     when Elab_Decls =>
                        Id := Get_Identifier (Str & "DECL_ELAB");
                     when Elab_Stmts =>
                        Id := Get_Identifier (Str & "STMT_ELAB");
                  end case;
                  Start_Procedure_Decl (Sub_Inter, Id, O_Storage_External);
                  New_Interface_Decl (Sub_Inter, Arg, Wki_Instance,
                                      Entity_Info.Block_Decls_Ptr_Type);
                  Finish_Subprogram_Decl (Sub_Inter, Arch_Elab (K));
               end loop;
            end if;

            if Config = Null_Iir then
               Start_Procedure_Decl
                 (Sub_Inter, Get_Identifier (Str & "DEFAULT_CONFIG"),
                  O_Storage_External);
               New_Interface_Decl (Sub_Inter, Arg, Wki_Instance,
                                   Entity_Info.Block_Decls_Ptr_Type);
               Finish_Subprogram_Decl (Sub_Inter, Arch_Config);

               Arch_Config_Type := Entity_Info.Block_Decls_Ptr_Type;
            end if;
         end;
      end if;

      if Arch_Info = null then
         if Config /= Null_Iir then
            --  Architecture is unknown, but we know how to configure
            --  the block inside it.
            raise Internal_Error;
         end if;
      else
         Instance_Size := Arch_Info.Block_Instance_Size;
         Arch_Elab := Arch_Info.Block_Elab_Subprg;
         if Config /= Null_Iir then
            Arch_Config := Get_Info (Config).Config_Subprg;
            Arch_Config_Type := Arch_Info.Block_Decls_Ptr_Type;
         end if;
      end if;

      --  Create the instance variable and allocate storage.
      New_Var_Decl (Var_Sub, Get_Identifier ("SUB_INSTANCE"),
                    O_Storage_Local, Entity_Info.Block_Decls_Ptr_Type);

      New_Assign_Stmt
        (New_Obj (Var_Sub),
         Gen_Alloc (Alloc_System, New_Obj_Value (Instance_Size),
           Entity_Info.Block_Decls_Ptr_Type));

      --  1.5) link instance.
      declare
         procedure Set_Links (Ref_Scope  : Var_Scope_Type;
                              Link_Field : O_Fnode)
         is
         begin
            --  Set the ghdl_component_link_instance field.
            New_Assign_Stmt
              (New_Selected_Element
                 (New_Selected_Element
                    (Get_Instance_Ref (Ref_Scope), Link_Field),
                  Rtis.Ghdl_Component_Link_Instance),
               New_Address
                 (New_Selected_Acc_Value
                    (New_Obj (Var_Sub), Entity_Info.Block_Link_Field),
                  Rtis.Ghdl_Entity_Link_Acc));
            --  Set the ghdl_entity_link_parent field.
            New_Assign_Stmt
              (New_Selected_Element
                 (New_Selected_Acc_Value
                    (New_Obj (Var_Sub), Entity_Info.Block_Link_Field),
                  Rtis.Ghdl_Entity_Link_Parent),
               New_Address
                 (New_Selected_Element
                    (Get_Instance_Ref (Ref_Scope), Link_Field),
                  Rtis.Ghdl_Component_Link_Acc));
         end Set_Links;
      begin
         case Get_Kind (Parent) is
            when Iir_Kind_Component_Declaration =>
               --  Instantiation via a component declaration.
               declare
                  Comp_Info : constant Comp_Info_Acc := Get_Info (Parent);
               begin
                  Set_Links (Comp_Info.Comp_Scope, Comp_Info.Comp_Link);
               end;
            when Iir_Kind_Component_Instantiation_Statement =>
               --  Direct instantiation.
               declare
                  Parent_Info : constant Block_Info_Acc :=
                    Get_Info (Get_Parent (Parent));
               begin
                  Set_Links (Parent_Info.Block_Scope,
                             Get_Info (Parent).Block_Link_Field);
               end;
            when others =>
               Error_Kind ("translate_entity_instantiation(1)", Parent);
         end case;
      end;

      --  Elab entity packages.
      declare
         Assoc : O_Assoc_List;
      begin
         Start_Association (Assoc, Entity_Info.Block_Elab_Pkg_Subprg);
         New_Procedure_Call (Assoc);
      end;

      --  Elab map aspects.
      declare
         use Chap5;
         Entity_Map : Map_Env;
      begin
         Entity_Map.Scope_Ptr := Entity_Info.Block_Scope'Access;
         Set_Scope_Via_Param_Ptr (Entity_Map.Scope, Var_Sub);
         Chap5.Elab_Map_Aspect (Entity, Mapping, Entity, Entity_Map);
         Clear_Scope (Entity_Map.Scope);
      end;

      --  3) Elab instance.
      declare
         Assoc : O_Assoc_List;
      begin
         for K in Elab_Kind loop
            Start_Association (Assoc, Arch_Elab (K));
            New_Association (Assoc, New_Obj_Value (Var_Sub));
            New_Procedure_Call (Assoc);
         end loop;
      end;

      --  5) Configure
      declare
         Assoc : O_Assoc_List;
      begin
         Start_Association (Assoc, Arch_Config);
         New_Association (Assoc, New_Convert_Ov (New_Obj_Value (Var_Sub),
                          Arch_Config_Type));
         New_Procedure_Call (Assoc);
      end;
   end Translate_Entity_Instantiation;

   procedure Elab_Decl_If_Case_Generate_Statement
     (Stmt : Iir; Parent : Iir; Base_Block : Iir)
   is
      Kind : constant Iir_Kinds_If_Case_Generate_Statement := Get_Kind (Stmt);
      Parent_Info : constant Block_Info_Acc := Get_Info (Parent);

      --  Used to get Block_Parent_Field, set in the first generate statement
      --  body.
      Stmt_Info : constant Generate_Info_Acc := Get_Info (Stmt);

      Label : O_Snode;

      --  Set the instance field in the parent.
      procedure Set_Parent_Field (Val : O_Enode; Num : Nat32)
      is
         V : O_Lnode;
      begin
         V := Get_Instance_Ref (Parent_Info.Block_Scope);
         V := New_Selected_Element (V, Stmt_Info.Generate_Parent_Field);
         New_Assign_Stmt (V, Val);

         V := Get_Instance_Ref (Parent_Info.Block_Scope);
         V := New_Selected_Element (V, Stmt_Info.Generate_Body_Id);
         New_Assign_Stmt (V, New_Lit (New_Index_Lit (Unsigned_64 (Num))));
      end Set_Parent_Field;

      procedure Elab_Decl_If_Case_Generate_Body (Bod : Iir)
      is
         Info        : constant Block_Info_Acc := Get_Info (Bod);
         Var         : O_Dnode;
      begin
         Var := Create_Temp (Info.Block_Decls_Ptr_Type);

         New_Assign_Stmt
           (New_Obj (Var),
            Gen_Alloc (Alloc_System,
                       New_Lit (Get_Scope_Size (Info.Block_Scope)),
                       Info.Block_Decls_Ptr_Type));

         --  Add a link to child in parent.  This must be done before
         --  elaboration, in case of use.
         Set_Parent_Field
           (New_Convert_Ov (New_Obj_Value (Var), Ghdl_Ptr_Type),
            Info.Block_Id);

         --  Add a link to parent in child.
         New_Assign_Stmt
           (New_Selected_Acc_Value (New_Obj (Var), Info.Block_Origin_Field),
            Get_Instance_Access (Base_Block));
            --  Elaborate block
         Set_Scope_Via_Param_Ptr (Info.Block_Scope, Var);
         Elab_Block_Declarations (Bod, Bod);
         Clear_Scope (Info.Block_Scope);
      end Elab_Decl_If_Case_Generate_Body;
   begin
      Start_Loop_Stmt (Label);

      case Kind is
         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause    : Iir;
               Condition : Iir;
               Blk       : O_If_Block;
               Num       : Nat32;
            begin
               Clause := Stmt;
               Num := 0;
               loop
                  Condition := Get_Condition (Clause);
                  Open_Temp;

                  if Condition /= Null_Iir then
                     Start_If_Stmt
                       (Blk, Chap7.Translate_Expression (Condition));
                  end if;

                  Open_Temp;
                  Elab_Decl_If_Case_Generate_Body
                    (Get_Generate_Statement_Body (Clause));
                  Close_Temp;

                  Num := Num + 1;

                  New_Exit_Stmt (Label);

                  if Condition /= Null_Iir then
                     Finish_If_Stmt (Blk);
                  end if;

                  Close_Temp;

                  exit when Condition = Null_Iir;

                  Clause := Get_Generate_Else_Clause (Clause);
                  if Clause = Null_Iir then
                     --  No block.
                     Set_Parent_Field
                       (New_Lit (New_Null_Access (Ghdl_Ptr_Type)), Num);
                     New_Exit_Stmt (Label);
                     exit;
                  end if;
               end loop;
            end;
         when Iir_Kind_Case_Generate_Statement =>
            --  FIXME: handle one-dimensional expressions.
            declare
               Expr : constant Iir := Get_Expression (Stmt);
               Expr_Type : constant Iir := Get_Type (Expr);
               Base_Type : constant Iir := Get_Base_Type (Expr_Type);
               Tinfo : constant Type_Info_Acc := Get_Info (Base_Type);
               E : O_Dnode;
               Alt : Iir;
               Cur_Alt : Iir;
               Cond : O_Enode;
               Sub_Cond : O_Enode;
               Var_Rng : O_Dnode;
               Rng : Mnode;
               C1, C2 : O_Enode;
               Blk       : O_If_Block;
            begin
               Open_Temp;
               Alt := Get_Case_Statement_Alternative_Chain (Stmt);
               E := Create_Temp_Init
                 (Tinfo.Ortho_Type (Mode_Value),
                  Chap7.Translate_Expression (Expr, Base_Type));

               loop
                  Open_Temp;

                  Cur_Alt := Alt;
                  Cond := O_Enode_Null;
                  loop
                     case Get_Kind (Alt) is
                        when Iir_Kind_Choice_By_Others =>
                           pragma Assert (Cond = O_Enode_Null);
                           pragma Assert (Get_Chain (Alt) = Null_Iir);
                           Sub_Cond := O_Enode_Null;
                        when Iir_Kind_Choice_By_Expression =>
                           Sub_Cond := New_Compare_Op
                             (ON_Eq,
                              New_Obj_Value (E),
                              Chap7.Translate_Expression
                                (Get_Choice_Expression (Alt), Base_Type),
                              Ghdl_Bool_Type);
                        when Iir_Kind_Choice_By_Range =>
                           Var_Rng := Create_Temp (Tinfo.B.Range_Type);
                           Rng := Dv2M (Var_Rng, Tinfo, Mode_Value,
                                        Tinfo.B.Range_Type,
                                        Tinfo.B.Range_Ptr_Type);
                           Chap7.Translate_Discrete_Range
                             (Rng, Get_Choice_Range (Alt));
                           C1 := New_Dyadic_Op
                             (ON_And,
                              New_Compare_Op
                                (ON_Eq,
                                 M2E (Chap3.Range_To_Dir (Rng)),
                                 New_Lit (Ghdl_Dir_To_Node),
                                 Ghdl_Bool_Type),
                              New_Dyadic_Op
                                (ON_And,
                                 New_Compare_Op
                                   (ON_Ge,
                                    New_Obj_Value (E),
                                    M2E (Chap3.Range_To_Left (Rng)),
                                    Ghdl_Bool_Type),
                                 New_Compare_Op
                                   (ON_Le,
                                    New_Obj_Value (E),
                                    M2E (Chap3.Range_To_Right (Rng)),
                                    Ghdl_Bool_Type)));
                           C2 := New_Dyadic_Op
                             (ON_And,
                              New_Compare_Op
                                (ON_Eq,
                                 M2E (Chap3.Range_To_Dir (Rng)),
                                 New_Lit (Ghdl_Dir_Downto_Node),
                                 Ghdl_Bool_Type),
                              New_Dyadic_Op
                                (ON_And,
                                 New_Compare_Op
                                   (ON_Le,
                                    New_Obj_Value (E),
                                    M2E (Chap3.Range_To_Left (Rng)),
                                    Ghdl_Bool_Type),
                                 New_Compare_Op
                                   (ON_Ge,
                                    New_Obj_Value (E),
                                    M2E (Chap3.Range_To_Right (Rng)),
                                    Ghdl_Bool_Type)));
                           Sub_Cond := New_Dyadic_Op (ON_Or, C1, C2);
                        when others =>
                           Error_Kind
                             ("Elab_Decl_If_Case_Generate_Statement", Alt);
                     end case;
                     if Cond = O_Enode_Null then
                        Cond := Sub_Cond;
                     else
                        Cond := New_Dyadic_Op (ON_Or, Cond, Sub_Cond);
                     end if;
                     Alt := Get_Chain (Alt);
                     exit when Alt = Null_Iir;
                     exit when not Get_Same_Alternative_Flag (Alt);
                  end loop;

                  if Cond /= O_Enode_Null then
                     Start_If_Stmt (Blk, Cond);
                  end if;

                  Open_Temp;
                  Elab_Decl_If_Case_Generate_Body
                    (Get_Associated_Block (Cur_Alt));
                  Close_Temp;

                  New_Exit_Stmt (Label);

                  if Cond /= O_Enode_Null then
                     Finish_If_Stmt (Blk);
                  end if;

                  Close_Temp;
                  exit when Alt = Null_Iir;
               end loop;
               Close_Temp;
            end;
      end case;
      Finish_Loop_Stmt (Label);
   end Elab_Decl_If_Case_Generate_Statement;

   procedure Elab_Stmt_If_Case_Generate_Statement (Stmt : Iir; Parent : Iir)
   is
      Parent_Info : constant Block_Info_Acc := Get_Info (Parent);

      --  Used to get Block_Parent_Field, set in the first generate statement
      --  body.
      Stmt_Info : constant Generate_Info_Acc := Get_Info (Stmt);

      Case_Blk : O_Case_Block;

      procedure Elab_Stmt_If_Case_Generate_Statement_Body (Bod : Iir)
      is
         Info : constant Block_Info_Acc := Get_Info (Bod);
         Var : O_Dnode;
      begin
         Start_Choice (Case_Blk);
         New_Expr_Choice
           (Case_Blk, New_Index_Lit (Unsigned_64 (Info.Block_Id)));
         Finish_Choice (Case_Blk);

         Open_Temp;
         Var := Create_Temp_Init
           (Info.Block_Decls_Ptr_Type,
            New_Convert_Ov
              (New_Value (New_Selected_Element
                            (Get_Instance_Ref (Parent_Info.Block_Scope),
                             Stmt_Info.Generate_Parent_Field)),
               Info.Block_Decls_Ptr_Type));

         Set_Scope_Via_Param_Ptr (Info.Block_Scope, Var);
         Elab_Block_Statements (Bod, Bod);
         Clear_Scope (Info.Block_Scope);
         Close_Temp;
      end Elab_Stmt_If_Case_Generate_Statement_Body;
   begin
      Start_Case_Stmt
        (Case_Blk, New_Value (New_Selected_Element
                                (Get_Instance_Ref (Parent_Info.Block_Scope),
                                 Stmt_Info.Generate_Body_Id)));

      case Iir_Kinds_If_Case_Generate_Statement (Get_Kind (Stmt)) is
         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause : Iir;
            begin
               Clause := Stmt;
               while Clause /= Null_Iir loop
                  Elab_Stmt_If_Case_Generate_Statement_Body
                    (Get_Generate_Statement_Body (Clause));
                  Clause := Get_Generate_Else_Clause (Clause);
               end loop;
            end;
         when Iir_Kind_Case_Generate_Statement =>
            declare
               Alt : Iir;
            begin
               Alt := Get_Case_Statement_Alternative_Chain (Stmt);
               while Alt /= Null_Iir loop
                  if not Get_Same_Alternative_Flag (Alt) then
                     Elab_Stmt_If_Case_Generate_Statement_Body
                       (Get_Associated_Block (Alt));
                  end if;
                  Alt := Get_Chain (Alt);
               end loop;
            end;
      end case;
      Start_Choice (Case_Blk);
      New_Default_Choice (Case_Blk);
      Finish_Choice (Case_Blk);
      Finish_Case_Stmt (Case_Blk);
   end Elab_Stmt_If_Case_Generate_Statement;

   procedure Elab_Decl_For_Generate_Statement
     (Stmt : Iir_Generate_Statement; Parent : Iir; Base_Block : Iir)
   is
      Iter           : constant Iir := Get_Parameter_Specification (Stmt);
      Iter_Type      : constant Iir := Get_Type (Iter);
      Iter_Base_Type : constant Iir := Get_Base_Type (Iter_Type);
      Iter_Type_Info : constant Type_Info_Acc := Get_Info (Iter_Base_Type);
      Bod            : constant Iir := Get_Generate_Statement_Body (Stmt);
      Info           : constant Block_Info_Acc := Get_Info (Bod);
      Parent_Info    : constant Block_Info_Acc := Get_Info (Parent);
      --         Base_Info : constant Block_Info_Acc := Get_Info (Base_Block);
      Var_Inst       : O_Dnode;
      Var_I          : O_Dnode;
      Label          : O_Snode;
      V              : O_Lnode;
      Var            : O_Dnode;
      Range_Ptr      : O_Dnode;
   begin
      Open_Temp;

      --  Evaluate iterator range.
      Chap3.Elab_Object_Subtype_Indication (Iter);

      Range_Ptr := Create_Temp_Ptr
        (Iter_Type_Info.B.Range_Ptr_Type,
         Get_Var (Get_Info (Iter_Type).S.Range_Var));

      --  Allocate instances.
      Var_Inst := Create_Temp (Info.Block_Decls_Array_Ptr_Type);
      New_Assign_Stmt
        (New_Obj (Var_Inst),
         Gen_Alloc
           (Alloc_System,
            New_Dyadic_Op (ON_Mul_Ov,
                           New_Value_Selected_Acc_Value
                             (New_Obj (Range_Ptr),
                              Iter_Type_Info.B.Range_Length),
                           New_Lit (Get_Scope_Size (Info.Block_Scope))),
            Info.Block_Decls_Array_Ptr_Type));

      --  Add a link to child in parent.
      V := Get_Instance_Ref (Parent_Info.Block_Scope);
      V := New_Selected_Element (V, Info.Block_Parent_Field);
      New_Assign_Stmt (V, New_Obj_Value (Var_Inst));

      --  Start loop.
      Var_I := Create_Temp (Ghdl_Index_Type);
      Init_Var (Var_I);
      Start_Loop_Stmt (Label);
      Gen_Exit_When
        (Label,
         New_Compare_Op (ON_Eq,
                         New_Obj_Value (Var_I),
                         New_Value_Selected_Acc_Value
                           (New_Obj (Range_Ptr),
                            Iter_Type_Info.B.Range_Length),
                         Ghdl_Bool_Type));

      Var := Create_Temp_Ptr
        (Info.Block_Decls_Ptr_Type,
         New_Indexed_Element (New_Acc_Value (New_Obj (Var_Inst)),
                              New_Obj_Value (Var_I)));
      --  Add a link to parent in child.
      New_Assign_Stmt
        (New_Selected_Acc_Value (New_Obj (Var), Info.Block_Origin_Field),
         Get_Instance_Access (Base_Block));
      --  Mark the block as not (yet) configured.
      New_Assign_Stmt
        (New_Selected_Acc_Value (New_Obj (Var), Info.Block_Configured_Field),
         New_Lit (Ghdl_Bool_False_Node));

      --  Elaborate block
      Set_Scope_Via_Param_Ptr (Info.Block_Scope, Var);

      --  Set iterator value.
      --  FIXME: this could be slighly optimized...
      declare
         Val    : O_Dnode;
         If_Blk : O_If_Block;
      begin
         Val := Create_Temp (Iter_Type_Info.Ortho_Type (Mode_Value));
         Start_If_Stmt
           (If_Blk,
            New_Compare_Op (ON_Eq,
                            New_Value_Selected_Acc_Value
                              (New_Obj (Range_Ptr),
                               Iter_Type_Info.B.Range_Dir),
                            New_Lit (Ghdl_Dir_To_Node),
                            Ghdl_Bool_Type));
         New_Assign_Stmt (New_Obj (Val), New_Value_Selected_Acc_Value
                            (New_Obj (Range_Ptr),
                             Iter_Type_Info.B.Range_Left));
         New_Else_Stmt (If_Blk);
         New_Assign_Stmt (New_Obj (Val), New_Value_Selected_Acc_Value
                            (New_Obj (Range_Ptr),
                             Iter_Type_Info.B.Range_Right));
         Finish_If_Stmt (If_Blk);

         New_Assign_Stmt
           (Get_Var (Get_Info (Iter).Iterator_Var),
            New_Dyadic_Op
              (ON_Add_Ov,
               New_Obj_Value (Val),
               New_Convert_Ov (New_Obj_Value (Var_I),
                               Iter_Type_Info.Ortho_Type (Mode_Value))));
      end;

      --  Elaboration.
      Elab_Block_Declarations (Bod, Bod);

      Clear_Scope (Info.Block_Scope);

      Inc_Var (Var_I);
      Finish_Loop_Stmt (Label);
      Close_Temp;
   end Elab_Decl_For_Generate_Statement;

   procedure Elab_Stmt_For_Generate_Statement
     (Stmt : Iir_Generate_Statement; Parent : Iir)
   is
      Iter           : constant Iir := Get_Parameter_Specification (Stmt);
      Iter_Type      : constant Iir := Get_Type (Iter);
      Iter_Base_Type : constant Iir := Get_Base_Type (Iter_Type);
      Iter_Type_Info : constant Type_Info_Acc := Get_Info (Iter_Base_Type);
      Bod            : constant Iir := Get_Generate_Statement_Body (Stmt);
      Info           : constant Block_Info_Acc := Get_Info (Bod);
      Parent_Info    : constant Block_Info_Acc := Get_Info (Parent);
      Var_Inst       : O_Dnode;
      Var_I          : O_Dnode;
      Label          : O_Snode;
      Var            : O_Dnode;
      Var_Len        : O_Dnode;
   begin
      Open_Temp;

      --  Evaluate iterator range.
      Chap3.Elab_Object_Subtype_Indication (Iter);

      --  Allocate instances.
      Var_Inst := Create_Temp_Init
        (Info.Block_Decls_Array_Ptr_Type,
         New_Value (New_Selected_Element
                      (Get_Instance_Ref (Parent_Info.Block_Scope),
                       Info.Block_Parent_Field)));
      Var_Len := Create_Temp_Init
        (Ghdl_Index_Type,
         New_Value (New_Selected_Element
                      (Get_Var (Get_Info (Iter_Type).S.Range_Var),
                       Iter_Type_Info.B.Range_Length)));

      --  Start loop.
      Var_I := Create_Temp (Ghdl_Index_Type);
      Init_Var (Var_I);
      Start_Loop_Stmt (Label);
      Gen_Exit_When
        (Label, New_Compare_Op (ON_Eq,
                                New_Obj_Value (Var_I),
                                New_Obj_Value (Var_Len),
                                Ghdl_Bool_Type));

      Var := Create_Temp_Ptr
        (Info.Block_Decls_Ptr_Type,
         New_Indexed_Element (New_Acc_Value (New_Obj (Var_Inst)),
                              New_Obj_Value (Var_I)));

      --  Elaborate block
      Set_Scope_Via_Param_Ptr (Info.Block_Scope, Var);

      --  Elaboration.
      Elab_Block_Statements (Bod, Bod);

      Clear_Scope (Info.Block_Scope);

      Inc_Var (Var_I);
      Finish_Loop_Stmt (Label);
      Close_Temp;
   end Elab_Stmt_For_Generate_Statement;

   procedure Merge_Signals_Rti_Non_Composite
     (Targ : Mnode; Targ_Type : Iir; Sig : Iir)
   is
      pragma Unreferenced (Targ_Type);
      Assoc : O_Assoc_List;
   begin
      Start_Association (Assoc, Ghdl_Signal_Merge_Rti);

      New_Association
        (Assoc, New_Convert_Ov (New_Value (M2Lv (Targ)), Ghdl_Signal_Ptr));
      New_Association
        (Assoc, New_Unchecked_Address (New_Obj (Get_Info (Sig).Signal_Rti),
                                       Rtis.Ghdl_Rti_Access));
      New_Procedure_Call (Assoc);
   end Merge_Signals_Rti_Non_Composite;

   function Merge_Signals_Rti_Prepare
     (Targ : Mnode; Targ_Type : Iir; Sig : Iir) return Iir
   is
      pragma Unreferenced (Targ);
      pragma Unreferenced (Targ_Type);
   begin
      return Sig;
   end Merge_Signals_Rti_Prepare;

   function Merge_Signals_Rti_Update_Data_Array
     (Sig : Iir; Targ_Type : Iir; Index : O_Dnode) return Iir
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (Index);
   begin
      return Sig;
   end Merge_Signals_Rti_Update_Data_Array;

   function Merge_Signals_Rti_Update_Data_Record
     (Sig : Iir; Targ_Type : Iir; El : Iir_Element_Declaration) return Iir
   is
      pragma Unreferenced (Targ_Type);
      pragma Unreferenced (El);
   begin
      return Sig;
   end Merge_Signals_Rti_Update_Data_Record;

   procedure Merge_Signals_Rti is new Foreach_Non_Composite
     (Data_Type => Iir,
      Composite_Data_Type => Iir,
      Do_Non_Composite => Merge_Signals_Rti_Non_Composite,
      Prepare_Data_Array => Merge_Signals_Rti_Prepare,
      Update_Data_Array => Merge_Signals_Rti_Update_Data_Array,
      Prepare_Data_Record => Merge_Signals_Rti_Prepare,
      Update_Data_Record => Merge_Signals_Rti_Update_Data_Record);

   procedure Merge_Signals_Rti_Of_Port_Chain (Chain : Iir)
   is
      Port      : Iir;
   begin
      Port := Chain;
      while Port /= Null_Iir loop
         Open_Temp;
         Merge_Signals_Rti
           (Chap6.Translate_Name (Port, Mode_Signal), Get_Type (Port), Port);
         Close_Temp;

         Port := Get_Chain (Port);
      end loop;
   end Merge_Signals_Rti_Of_Port_Chain;

   procedure Elab_Block_Declarations (Block : Iir; Base_Block : Iir)
   is
      Base_Info : constant Block_Info_Acc := Get_Info (Base_Block);
      Stmt      : Iir;
      Final     : Boolean;
   begin
      New_Debug_Line_Stmt (Get_Line_Number (Block));

      case Get_Kind (Block) is
         when Iir_Kind_Entity_Declaration =>
            Merge_Signals_Rti_Of_Port_Chain (Get_Port_Chain (Block));
         when Iir_Kind_Architecture_Body =>
            null;
         when Iir_Kind_Block_Statement =>
            declare
               Header : constant Iir_Block_Header :=
                 Get_Block_Header (Block);
               Guard  : constant Iir := Get_Guard_Decl (Block);
               Block_Env : Chap5.Map_Env;
               Block_Info : Block_Info_Acc;
            begin
               if Guard /= Null_Iir then
                  New_Debug_Line_Stmt (Get_Line_Number (Guard));
                  Elab_Implicit_Guard_Signal (Block, Base_Info);
               end if;
               if Header /= Null_Iir then
                  New_Debug_Line_Stmt (Get_Line_Number (Header));
                  Block_Info := Get_Info (Block);
                  Block_Env := (Block_Info.Block_Scope'Access,
                                Block_Info.Block_Scope);
                  Chap5.Elab_Map_Aspect (Header, Header, Block, Block_Env);
                  Merge_Signals_Rti_Of_Port_Chain (Get_Port_Chain (Header));
               end if;
            end;
         when Iir_Kind_Generate_Statement_Body =>
            null;
         when others =>
            Error_Kind ("elab_block_declarations", Block);
      end case;

      Open_Temp;
      Chap4.Elab_Declaration_Chain (Block, Final);
      Close_Temp;

      Stmt := Get_Concurrent_Statement_Chain (Block);
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Process_Statement
               | Iir_Kind_Sensitized_Process_Statement =>
               null;
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_Psl_Endpoint_Declaration
               | Iir_Kind_Psl_Restrict_Directive
               | Iir_Kind_Psl_Assert_Directive
               | Iir_Kind_Psl_Assume_Directive
               | Iir_Kind_Psl_Cover_Directive =>
               null;
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Info   : constant Block_Info_Acc := Get_Info (Stmt);
                  Constr : O_Assoc_List;
               begin
                  Start_Association
                    (Constr, Info.Block_Elab_Subprg (Elab_Decls));
                  New_Association
                    (Constr, Get_Instance_Access (Base_Block));
                  New_Procedure_Call (Constr);
               end;
            when Iir_Kind_Block_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Block_Declarations (Stmt, Base_Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when Iir_Kind_If_Generate_Statement
              | Iir_Kind_Case_Generate_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Decl_If_Case_Generate_Statement
                    (Stmt, Block, Base_Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when Iir_Kind_For_Generate_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Decl_For_Generate_Statement (Stmt, Block, Base_Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when others =>
               Error_Kind ("elab_block_declarations", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Elab_Block_Declarations;

   procedure Elab_Block_Statements (Block : Iir; Base_Block : Iir)
   is
      Base_Info : constant Block_Info_Acc := Get_Info (Base_Block);
      Stmt      : Iir;
   begin
      Stmt := Get_Concurrent_Statement_Chain (Block);
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Process_Statement
               | Iir_Kind_Sensitized_Process_Statement =>
               Elab_Process (Stmt, Base_Info);
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Restrict_Directive
               | Iir_Kind_Psl_Declaration =>
               null;
            when Iir_Kind_Psl_Assert_Directive
              | Iir_Kind_Psl_Assume_Directive
              | Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Elab_Psl_Directive (Stmt, Base_Info);
            when Iir_Kind_Component_Instantiation_Statement =>
               null;
            when Iir_Kind_Block_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Block_Statements (Stmt, Base_Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when Iir_Kind_If_Generate_Statement
              | Iir_Kind_Case_Generate_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Stmt_If_Case_Generate_Statement (Stmt, Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when Iir_Kind_For_Generate_Statement =>
               declare
                  Mark : Id_Mark_Type;
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Stmt));
                  Elab_Stmt_For_Generate_Statement (Stmt, Block);
                  Pop_Identifier_Prefix (Mark);
               end;
            when others =>
               Error_Kind ("elab_block_statements", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Elab_Block_Statements;
end Trans.Chap9;
