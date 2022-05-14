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

with Std_Names;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Sem_Inst;
with Vhdl.Nodes_Meta;
with Vhdl.Utils; use Vhdl.Utils;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap5;
with Trans.Chap6;
with Trans.Chap8;
with Trans.Rtis;
with Trans.Helpers2; use Trans.Helpers2;
with Trans_Decls; use Trans_Decls;
with Translation; use Translation;

package body Trans.Chap2 is
   use Trans.Subprgs;
   use Trans.Helpers;

   type Name_String_Xlat_Array is array (Name_Id range <>) of String (1 .. 4);

   --  Ortho function names are only composed of [A-Za-z0-9_].  For VHDL
   --  functions whose name is an operator symbol, we need to create a name
   --  with letters.
   Operator_String_Xlat : constant
     Name_String_Xlat_Array (Std_Names.Name_Id_Operators) :=
     (Std_Names.Name_Op_Equality => "OPEq",
      Std_Names.Name_Op_Inequality => "OPNe",
      Std_Names.Name_Op_Less => "OPLt",
      Std_Names.Name_Op_Less_Equal => "OPLe",
      Std_Names.Name_Op_Greater => "OPGt",
      Std_Names.Name_Op_Greater_Equal => "OPGe",
      Std_Names.Name_Op_Plus => "OPPl",
      Std_Names.Name_Op_Minus => "OPMi",
      Std_Names.Name_Op_Mul => "OPMu",
      Std_Names.Name_Op_Div => "OPDi",
      Std_Names.Name_Op_Exp => "OPEx",
      Std_Names.Name_Op_Concatenation => "OPCc",
      Std_Names.Name_Op_Condition => "OPCd",
      Std_Names.Name_Op_Match_Equality => "OPQe",
      Std_Names.Name_Op_Match_Inequality => "OPQi",
      Std_Names.Name_Op_Match_Less => "OPQL",
      Std_Names.Name_Op_Match_Less_Equal => "OPQl",
      Std_Names.Name_Op_Match_Greater => "OPQG",
      Std_Names.Name_Op_Match_Greater_Equal => "OPQg");

   --  Set the identifier prefix with the subprogram identifier and
   --  overload number if any.
   procedure Push_Subprg_Identifier (Spec : Iir; Mark : out Id_Mark_Type)
   is
      Id : constant Name_Id := Get_Identifier (Spec);
   begin
      --  FIXME: name_shift_operators, name_logical_operators,
      --   name_word_operators, name_mod, name_rem
      if Id in Std_Names.Name_Id_Operators then
         Push_Identifier_Prefix
           (Mark, Operator_String_Xlat (Id), Get_Overload_Number (Spec));
      else
         Push_Identifier_Prefix (Mark, Id, Get_Overload_Number (Spec));
      end if;
   end Push_Subprg_Identifier;

   --  Return the type of a subprogram interface.
   procedure Translate_Interface_Mechanism (Inter : Iir)
   is
      Spec : constant Iir := Get_Parent (Inter);
      pragma Assert (Get_Kind (Spec) in Iir_Kinds_Subprogram_Declaration);
      Info : constant Interface_Info_Acc := Get_Info (Inter);
      Tinfo : constant Type_Info_Acc := Get_Info (Get_Type (Inter));
      Mech : Call_Mechanism;
   begin
      --  Mechanism.
      case Type_Mode_Valid (Tinfo.Type_Mode) is
         when Type_Mode_Pass_By_Copy =>
            Mech := Pass_By_Copy;
         when Type_Mode_Pass_By_Address =>
            Mech := Pass_By_Address;
      end case;

      case Iir_Kinds_Interface_Object_Declaration (Get_Kind (Inter)) is
         when Iir_Kind_Interface_Constant_Declaration
            | Iir_Kind_Interface_File_Declaration =>
            Info.Interface_Mechanism (Mode_Value) := Mech;
         when Iir_Kind_Interface_Variable_Declaration =>
            if Get_Foreign_Flag (Spec)
              and then Get_Mode (Inter) in Iir_Out_Modes
            then
               Mech := Pass_By_Address;
            end if;
            Info.Interface_Mechanism (Mode_Value) := Mech;
         when Iir_Kind_Interface_Signal_Declaration =>
            Info.Interface_Mechanism (Mode_Signal) := Mech;
            --  Values are always passed by address.
            if Get_Kind (Spec) = Iir_Kind_Procedure_Declaration then
               Mech := Pass_By_Address;
            end if;
            Info.Interface_Mechanism (Mode_Value) := Mech;
         when Iir_Kind_Interface_Quantity_Declaration =>
            raise Internal_Error;
      end case;
   end Translate_Interface_Mechanism;

   function Translate_Interface_Type (Inter : Iir; Mode : Object_Kind_Type)
                                     return O_Tnode
   is
      Info : constant Interface_Info_Acc := Get_Info (Inter);
      Tinfo : constant Type_Info_Acc := Get_Info (Get_Type (Inter));
   begin
      case Info.Interface_Mechanism (Mode) is
         when Pass_By_Address =>
            return Tinfo.Ortho_Ptr_Type (Mode);
         when Pass_By_Copy =>
            return Tinfo.Ortho_Type (Mode);
      end case;
   end Translate_Interface_Type;

   procedure Translate_Subprogram_Interfaces (Spec : Iir)
   is
      Inter : Iir;
      Mark  : Id_Mark_Type;
      Info  : Subprg_Info_Acc;
      El_List : O_Element_List;
      Param_Info : Ortho_Info_Acc;
   begin
      --  Set the identifier prefix with the subprogram identifier and
      --  overload number if any.
      Push_Subprg_Identifier (Spec, Mark);

      --  Translate interface types.
      Inter := Get_Interface_Declaration_Chain (Spec);
      while Inter /= Null_Iir loop
         Chap3.Translate_Object_Subtype_Indication (Inter);
         Inter := Get_Chain (Inter);
      end loop;

      if Get_Kind (Spec) = Iir_Kind_Procedure_Declaration then
         --  Create the param record (except for foreign subprogram).
         Info := Get_Info (Spec);
         Inter := Get_Interface_Declaration_Chain (Spec);
         if (Inter /= Null_Iir or else Get_Suspend_Flag (Spec))
           and then not Get_Foreign_Flag (Spec)
         then
            Start_Record_Type (El_List);

            --  Create fields for interfaces.
            while Inter /= Null_Iir loop
               Param_Info := Add_Info (Inter, Kind_Interface);
               Translate_Interface_Mechanism (Inter);

               New_Record_Field
                 (El_List, Param_Info.Interface_Field (Mode_Value),
                  Create_Identifier_Without_Prefix (Inter),
                  Translate_Interface_Type (Inter, Mode_Value));

               if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration
               then
                  New_Record_Field
                    (El_List, Param_Info.Interface_Field (Mode_Signal),
                     Create_Identifier_Without_Prefix (Inter, "SIG"),
                     Translate_Interface_Type (Inter, Mode_Signal));
               end if;
               Inter := Get_Chain (Inter);
            end loop;

            if Get_Suspend_Flag (Spec) then
               New_Record_Field (El_List, Info.Subprg_State_Field,
                                 Get_Identifier ("STATE"), Ghdl_Index_Type);
               New_Record_Field (El_List, Info.Subprg_Locvars_Field,
                                 Get_Identifier ("FRAME"), Ghdl_Ptr_Type);
            end if;

            --  Declare the record type and an access to the record.
            Finish_Record_Type (El_List, Info.Subprg_Params_Type);
            New_Type_Decl (Create_Identifier ("PARAMSTYPE"),
                           Info.Subprg_Params_Type);
            Info.Subprg_Params_Ptr :=
              New_Access_Type (Info.Subprg_Params_Type);
            New_Type_Decl (Create_Identifier ("PARAMSPTR"),
                           Info.Subprg_Params_Ptr);
         else
            Info.Subprg_Params_Type := O_Tnode_Null;
            Info.Subprg_Params_Ptr := O_Tnode_Null;
         end if;
      end if;
      Pop_Identifier_Prefix (Mark);
   end Translate_Subprogram_Interfaces;

   procedure Elab_Subprogram_Interfaces (Spec : Iir)
   is
      Inter : Iir;
   begin
      --  Translate interface types.
      Inter := Get_Interface_Declaration_Chain (Spec);
      while Inter /= Null_Iir loop
         Chap3.Elab_Object_Subtype_Indication (Inter);
         Inter := Get_Chain (Inter);
      end loop;
   end Elab_Subprogram_Interfaces;

   procedure Translate_Subprogram_Declaration (Spec : Iir)
   is
      Info : constant Subprg_Info_Acc := Get_Info (Spec);
      Is_Func : constant Boolean :=
        Get_Kind (Spec) = Iir_Kind_Function_Declaration;
      Is_Foreign : constant Boolean := Get_Foreign_Flag (Spec);
      Inter : Iir;
      Param_Info : Ortho_Info_Acc;
      Arg_Type : O_Tnode;
      Tinfo : Type_Info_Acc;
      Interface_List : O_Inter_List;
      Mark : Id_Mark_Type;
      Rtype : Iir;
      Id : O_Ident;
      Storage : O_Storage;
      Foreign : Foreign_Info_Type;
   begin
      --  Set the identifier prefix with the subprogram identifier and
      --  overload number if any.
      Push_Subprg_Identifier (Spec, Mark);

      --  Create the subprogram identifier.
      if Is_Foreign then
         --  Special handling for foreign subprograms.
         Foreign := Translate_Foreign_Id (Spec);
         case Foreign.Kind is
            when Foreign_Unknown =>
               Id := Create_Identifier;
            when Foreign_Intrinsic =>
               Id := Create_Identifier;
            when Foreign_Vhpidirect =>
               Id := Get_Identifier
                 (Foreign.Subprg_Name (1 .. Foreign.Subprg_Len));
         end case;
         Storage := O_Storage_External;
      else
         Foreign := Foreign_Bad;
         Id := Create_Identifier;
         Storage := Global_Storage;
      end if;

      if Is_Func then
         --  If the result of a function is a composite type for ortho,
         --  the result is allocated by the caller and an access to it is
         --  given to the function.
         Rtype := Get_Return_Type (Spec);
         Info.Use_Stack2 := False;
         Tinfo := Get_Info (Rtype);

         if Is_Composite (Tinfo) then
            Start_Procedure_Decl (Interface_List, Id, Storage);
            New_Interface_Decl
              (Interface_List, Info.Res_Interface,
               Get_Identifier ("RESULT"),
               Tinfo.Ortho_Ptr_Type (Mode_Value));
            --  Furthermore, if the result type is unconstrained, the
            --  function will allocate it on a secondary stack.
            if not Is_Fully_Constrained_Type (Rtype) then
               Info.Use_Stack2 := True;
            end if;
         else
            --  Normal function.
            Start_Function_Decl
              (Interface_List, Id, Storage, Tinfo.Ortho_Type (Mode_Value));
            Info.Res_Interface := O_Dnode_Null;
         end if;
      else
         Start_Procedure_Decl (Interface_List, Id, Storage);

         if Info.Subprg_Params_Type /= O_Tnode_Null then
            New_Interface_Decl (Interface_List, Info.Res_Interface,
                                Get_Identifier ("PARAMS"),
                                Info.Subprg_Params_Ptr);
         else
            Info.Res_Interface := O_Dnode_Null;
         end if;
      end if;

      --  Instance parameter if any.
      if not Is_Foreign then
         Subprgs.Create_Subprg_Instance (Interface_List, Spec);
      end if;

      --  Translate interfaces.
      if Is_Func or else Is_Foreign then
         Inter := Get_Interface_Declaration_Chain (Spec);
         while Inter /= Null_Iir loop
            --  Create the info.
            Param_Info := Add_Info (Inter, Kind_Interface);
            Translate_Interface_Mechanism (Inter);

            Arg_Type := Translate_Interface_Type (Inter, Mode_Value);
            New_Interface_Decl
              (Interface_List, Param_Info.Interface_Decl (Mode_Value),
               Create_Identifier_Without_Prefix (Inter), Arg_Type);

            if Get_Kind (Inter) = Iir_Kind_Interface_Signal_Declaration then
               Arg_Type := Translate_Interface_Type (Inter, Mode_Signal);
               New_Interface_Decl
                 (Interface_List, Param_Info.Interface_Decl (Mode_Signal),
                  Create_Identifier_Without_Prefix (Inter, "SIG"),
                  Arg_Type);
            end if;
            Inter := Get_Chain (Inter);
         end loop;
      end if;
      Finish_Subprogram_Decl (Interface_List, Info.Subprg_Node);

      --  Call the hook for foreign subprograms.
      if Is_Foreign and then Foreign_Hook /= null then
         Foreign_Hook.all (Spec, Foreign, Info.Subprg_Node);
      end if;

      Save_Local_Identifier (Info.Subprg_Local_Id);
      Pop_Identifier_Prefix (Mark);
   end Translate_Subprogram_Declaration;

   --  Return TRUE iff subprogram specification SPEC is translated in an
   --  ortho function.
   function Is_Subprogram_Ortho_Function (Spec : Iir) return Boolean
   is
   begin
      if Get_Kind (Spec) = Iir_Kind_Procedure_Declaration then
         return False;
      end if;
      if Get_Info (Spec).Res_Interface /= O_Dnode_Null then
         return False;
      end if;
      return True;
   end Is_Subprogram_Ortho_Function;

   --  Return TRUE iif SUBPRG_BODY declares explicitly or implicitely
   --  (or even implicitely by translation) a subprogram.
   function Has_Nested_Subprograms (Subprg_Body : Iir) return Boolean
   is
      Decl  : Iir;
      Atype : Iir;
   begin
      Decl := Get_Declaration_Chain (Subprg_Body);
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Function_Declaration
               | Iir_Kind_Procedure_Declaration =>
               return True;
            when Iir_Kind_Function_Body
               | Iir_Kind_Procedure_Body =>
               --  The declaration preceed the body.
               raise Internal_Error;
            when Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Atype := Get_Type_Definition (Decl);
               case Iir_Kinds_Type_And_Subtype_Definition (Get_Kind (Atype)) is
                  when Iir_Kinds_Scalar_Type_And_Subtype_Definition =>
                     null;
                  when Iir_Kind_Access_Type_Definition
                     | Iir_Kind_Access_Subtype_Definition =>
                     null;
                  when Iir_Kind_File_Type_Definition =>
                     return True;
                  when Iir_Kind_Protected_Type_Declaration =>
                     --  We suppose there is at least one method.
                     return True;
                  when Iir_Kinds_Composite_Type_Definition =>
                     --  At least for "=".
                     return True;
                  when Iir_Kind_Incomplete_Type_Definition
                    | Iir_Kind_Interface_Type_Definition =>
                     null;
               end case;
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Body =>
               if Has_Nested_Subprograms (Decl) then
                  return True;
               end if;
            when others =>
               null;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
      return False;
   end Has_Nested_Subprograms;

   procedure Translate_Subprogram_Body (Subprg : Iir)
   is
      Spec : constant Iir := Get_Subprogram_Specification (Subprg);
      Info : constant Ortho_Info_Acc := Get_Info (Spec);

      --  True if the subprogram is suspendable (can be true only for
      --  procedures).
      Has_Suspend : constant Boolean :=
        Get_Kind (Spec) = Iir_Kind_Procedure_Declaration
        and then Get_Suspend_Flag (Spec);

      --  True if the subprogram is translated to a function in ortho.
      Is_Ortho_Func  : constant Boolean := Is_Subprogram_Ortho_Function (Spec);

      Old_Subprogram : Iir;
      Mark           : Id_Mark_Type;
      Final          : Boolean;

      --  Set for a public method.  In this case, the lock must be acquired
      --  and retained.
      Is_Prot : Boolean := False;

      --  True if the body has local (nested) subprograms.
      Has_Nested : Boolean;

      Frame_Ptr_Type : O_Tnode;
      Upframe_Field  : O_Fnode;
      Upframe_Scope  : Var_Scope_Acc;

      Frame     : O_Dnode;
      Frame_Ptr : O_Dnode;

      Has_Return : Boolean;

      Prev_Subprg_Instances : Subprgs.Subprg_Instance_Stack;
   begin
      --  Do not translate body for foreign subprograms.
      if Get_Foreign_Flag (Spec) then
         return;
      end if;

      --  Check if there are nested subprograms to unnest.  In that case,
      --  a frame record is created, which is less efficient than the
      --  use of local variables.
      if Flag_Unnest_Subprograms then
         Has_Nested := Has_Nested_Subprograms (Subprg);
      else
         Has_Nested := False;
      end if;

      --  Set the identifier prefix with the subprogram identifier and
      --  overload number if any.
      Push_Subprg_Identifier (Spec, Mark);
      Restore_Local_Identifier (Info.Subprg_Local_Id);

      if Has_Nested or else Has_Suspend then
         --  Unnest subprograms.
         --  Create an instance for the local declarations.
         Push_Frame_Factory (Info.Subprg_Frame_Scope'Access, Has_Suspend);
         Add_Subprg_Instance_Field (Upframe_Field, Upframe_Scope);

         if Info.Subprg_Params_Ptr /= O_Tnode_Null then
            --  Field for the parameters structure
            Info.Subprg_Params_Var :=
              Create_Var (Create_Var_Identifier ("PARAMS"),
                          Info.Subprg_Params_Ptr);
         else
            --  Create fields for parameters.
            --  FIXME: do it only if they are referenced in nested
            --  subprograms.
            declare
               Inter      : Iir;
               Inter_Type : O_Tnode;
               Inter_Info : Inter_Info_Acc;
            begin
               Inter := Get_Interface_Declaration_Chain (Spec);
               while Inter /= Null_Iir loop
                  Inter_Info := Get_Info (Inter);
                  if Inter_Info.Interface_Decl (Mode_Value) /= O_Dnode_Null
                  then
                     Inter_Type :=
                       Translate_Interface_Type (Inter, Mode_Value);
                     Inter_Info.Interface_Field (Mode_Value) :=
                       Add_Instance_Factory_Field
                       (Create_Identifier_Without_Prefix (Inter), Inter_Type);

                     if Get_Kind (Inter)
                       = Iir_Kind_Interface_Signal_Declaration
                     then
                        Inter_Type :=
                          Translate_Interface_Type (Inter, Mode_Signal);
                        Inter_Info.Interface_Field (Mode_Signal) :=
                          Add_Instance_Factory_Field
                          (Create_Identifier_Without_Prefix (Inter, "SIG"),
                           Inter_Type);
                     end if;
                  end if;
                  Inter := Get_Chain (Inter);
               end loop;
            end;
         end if;

         Chap4.Translate_Declaration_Chain (Subprg);

         if Has_Suspend then
            --  Add declarations for statements (iterator, call) and state.
            Chap4.Translate_Statements_Chain_State_Declaration
              (Get_Sequential_Statement_Chain (Subprg),
               Info.Subprg_Locvars_Scope'Access);
            Add_Scope_Field (Wki_Locvars, Info.Subprg_Locvars_Scope);
         end if;

         Pop_Frame_Factory (Info.Subprg_Frame_Scope'Access);

         New_Type_Decl (Create_Identifier ("_FRAMETYPE"),
                        Get_Scope_Type (Info.Subprg_Frame_Scope));
         Declare_Scope_Acc
           (Info.Subprg_Frame_Scope,
            Create_Identifier ("_FRAMEPTR"), Frame_Ptr_Type);

         Rtis.Generate_Subprogram_Body (Subprg);

         --  Local frame
         Subprgs.Push_Subprg_Instance
           (Info.Subprg_Frame_Scope'Access, Frame_Ptr_Type,
            Wki_Upframe, Prev_Subprg_Instances);
         --  Link to previous frame
         Subprgs.Start_Prev_Subprg_Instance_Use_Via_Field
           (Upframe_Scope, Upframe_Field);

         Chap4.Translate_Declaration_Chain_Subprograms
           (Subprg, Subprg_Translate_Spec_And_Body);

         --  Link to previous frame
         Subprgs.Finish_Prev_Subprg_Instance_Use_Via_Field
           (Upframe_Scope, Upframe_Field);
         --  Local frame
         Subprgs.Pop_Subprg_Instance (Wki_Upframe, Prev_Subprg_Instances);
      end if;

      --  Create the body.  Add a line very early, before any statement.

      Start_Subprogram_Body (Info.Subprg_Node);
      New_Debug_Line_Stmt (Get_Line_Number (Subprg));

      Start_Subprg_Instance_Use (Spec);

      --  Variables will be created on the stack.
      Push_Local_Factory;

      --  Code has access to local (and outer) variables.
      --  FIXME: this is not necessary if Has_Nested is set
      Subprgs.Clear_Subprg_Instance (Prev_Subprg_Instances);

      --  There is a local scope for temporaries.
      Open_Local_Temp;

      if not Has_Suspend and not Has_Nested then
         Chap4.Translate_Declaration_Chain (Subprg);
         Rtis.Generate_Subprogram_Body (Subprg);
         Chap4.Translate_Declaration_Chain_Subprograms
           (Subprg, Subprg_Translate_Spec_And_Body);
      else
         New_Var_Decl (Frame_Ptr, Get_Identifier ("FRAMEPTR"),
                       O_Storage_Local, Frame_Ptr_Type);

         if Has_Suspend then
            New_Assign_Stmt
              (New_Obj (Frame_Ptr),
               New_Convert_Ov (New_Value_Selected_Acc_Value
                                 (New_Obj (Info.Res_Interface),
                                  Info.Subprg_Locvars_Field),
                               Frame_Ptr_Type));

            Chap8.State_Entry (Info);

            --  Initial state: allocate frame.
            New_Assign_Stmt
              (New_Obj (Frame_Ptr),
               Gen_Alloc
                 (Alloc_Return,
                  New_Lit
                    (New_Sizeof (Get_Scope_Type (Info.Subprg_Frame_Scope),
                                 Ghdl_Index_Type)),
                  Frame_Ptr_Type));
            New_Assign_Stmt
              (New_Selected_Acc_Value (New_Obj (Info.Res_Interface),
                                       Info.Subprg_Locvars_Field),
               New_Convert_Ov (New_Obj_Value (Frame_Ptr),
                               Ghdl_Ptr_Type));

            --  Allocate the return state.  This IS NOT AN ASSERTION as the
            --  State_Allocate function has a side-effect.
            if Chap8.State_Allocate /= Chap8.State_Return then
               raise Internal_Error;
            end if;
         else
            --  Allocate the frame by declaring a local variable.
            New_Var_Decl (Frame, Wki_Frame, O_Storage_Local,
                          Get_Scope_Type (Info.Subprg_Frame_Scope));

            New_Assign_Stmt (New_Obj (Frame_Ptr),
                             New_Address (New_Obj (Frame), Frame_Ptr_Type));
         end if;

         --  FIXME: use direct reference (ie Frame instead of Frame_Ptr)
         Set_Scope_Via_Param_Ptr (Info.Subprg_Frame_Scope, Frame_Ptr);

         --  Set UPFRAME.
         Subprgs.Set_Subprg_Instance_Field
           (Frame_Ptr, Upframe_Field, Info.Subprg_Instance);

         if Info.Subprg_Params_Type /= O_Tnode_Null then
            --  Initialize the PARAMS field
            New_Assign_Stmt (Get_Var (Info.Subprg_Params_Var),
                             New_Obj_Value (Info.Res_Interface));
            --  Do not reference the RESULT field in the subprogram body,
            --  directly reference the RESULT parameter.
            --  FIXME: has a flag (see below for parameters).
            Info.Subprg_Params_Var := Null_Var;
         end if;

         --  Copy parameters to FRAME.
         if Info.Subprg_Params_Ptr = O_Tnode_Null then
            declare
               Inter      : Iir;
               Inter_Info : Inter_Info_Acc;
            begin
               Inter := Get_Interface_Declaration_Chain (Spec);
               while Inter /= Null_Iir loop
                  Inter_Info := Get_Info (Inter);
                  for Mode in Object_Kind_Type loop
                     if Inter_Info.Interface_Decl (Mode) /= O_Dnode_Null then
                        New_Assign_Stmt
                          (New_Selected_Element
                             (New_Obj (Frame),
                              Inter_Info.Interface_Field (Mode)),
                           New_Obj_Value (Inter_Info.Interface_Decl (Mode)));

                        --  Forget the reference to the field in FRAME, so that
                        --  this subprogram will directly reference the
                        --  parameter (and not its copy in the FRAME).
                        Inter_Info.Interface_Field (Mode) := O_Fnode_Null;
                     end if;
                  end loop;
                  Inter := Get_Chain (Inter);
               end loop;
            end;
         end if;
      end if;

      Is_Prot := Is_Subprogram_Method (Spec);
      if Is_Prot then
         --  Lock the object.
         Chap3.Call_Ghdl_Protected_Procedure (Get_Method_Type (Spec),
                                              Ghdl_Protected_Enter);
      end if;

      Chap4.Elab_Declaration_Chain (Subprg, Final);

      if not Has_Suspend then
         Stack2_Release;
      end if;
      --  If finalization is required and if the subprogram is a function,
      --  create a variable for the result.
      if (Final or Is_Prot) and Is_Ortho_Func then
         New_Var_Decl
           (Info.Subprg_Result, Get_Identifier ("RESULT"),
            O_Storage_Local,
            Get_Ortho_Type (Get_Return_Type (Spec), Mode_Value));
      end if;

      --  If finalization is required, create a dummy loop around the
      --  body and convert returns into exit out of this loop.
      if not Has_Suspend and then (Final or Is_Prot) then
         Start_Loop_Stmt (Info.Subprg_Exit);
      end if;

      Old_Subprogram := Current_Subprogram;
      Current_Subprogram := Spec;
      Has_Return := Chap8.Translate_Statements_Chain_Has_Return
        (Get_Sequential_Statement_Chain (Subprg));
      Current_Subprogram := Old_Subprogram;

      if Has_Suspend or Final or Is_Prot then
         --  Create a barrier to catch missing return statement.
         if Get_Kind (Spec) = Iir_Kind_Procedure_Declaration then
            if Has_Suspend then
               Chap8.State_Jump (Chap8.State_Return);
            else
               New_Exit_Stmt (Info.Subprg_Exit);
            end if;
         else
            if not Has_Return then
               --  Missing return
               Chap6.Gen_Program_Error
                 (Subprg, Chap6.Prg_Err_Missing_Return);
            end if;
         end if;
         if Has_Suspend then
            Chap8.State_Start (Chap8.State_Return);
         else
            Finish_Loop_Stmt (Info.Subprg_Exit);
         end if;
         Chap4.Final_Declaration_Chain (Subprg, False);

         if Is_Prot then
            --  Unlock the object.
            Chap3.Call_Ghdl_Protected_Procedure (Get_Method_Type (Spec),
                                                 Ghdl_Protected_Leave);
         end if;

         if Has_Suspend then
            Chap8.State_Suspend (Chap8.State_Return);
            Chap8.State_Leave (Spec);
         end if;

         if Is_Ortho_Func then
            New_Return_Stmt (New_Obj_Value (Info.Subprg_Result));
         end if;
      else
         if Get_Kind (Spec) = Iir_Kind_Function_Declaration
           and then not Has_Return
         then
            --  Missing return
            Chap6.Gen_Program_Error
              (Subprg, Chap6.Prg_Err_Missing_Return);
         end if;
      end if;

      if Has_Nested then
         Clear_Scope (Info.Subprg_Frame_Scope);
      end if;

      Subprgs.Pop_Subprg_Instance (O_Ident_Nul, Prev_Subprg_Instances);
      Close_Local_Temp;
      Pop_Local_Factory;

      Finish_Subprg_Instance_Use (Spec);

      Finish_Subprogram_Body;

      Pop_Identifier_Prefix (Mark);
   end Translate_Subprogram_Body;

   procedure Push_Package_Instance_Factory (Spec : Iir)
   is
      Info : constant Ortho_Info_Acc := Get_Info (Spec);
   begin
      Push_Instance_Factory (Info.Package_Body_Scope'Access);
      Info.Package_Spec_Field := Add_Instance_Factory_Field
        (Get_Identifier ("SPEC"),
         Get_Scope_Type (Info.Package_Spec_Scope));
   end Push_Package_Instance_Factory;

   procedure Pop_Package_Instance_Factory (Spec : Iir)
   is
      Info : constant Ortho_Info_Acc := Get_Info (Spec);
   begin
      Pop_Instance_Factory (Info.Package_Body_Scope'Access);
   end Pop_Package_Instance_Factory;

   --  Translate a package declaration or a macro-expanded package
   --  instantiation.  HEADER is the node containing generic and generic_map.
   procedure Translate_Package (Decl : Iir; Header : Iir)
   is
      Is_Nested            : constant Boolean := Is_Nested_Package (Decl);
      Is_Uninstantiated    : constant Boolean :=
        Get_Kind (Decl) = Iir_Kind_Package_Declaration
        and then Is_Uninstantiated_Package (Decl);
      Mark                 : Id_Mark_Type;
      Info                 : Ortho_Info_Acc;
      Interface_List       : O_Inter_List;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Info := Add_Info (Decl, Kind_Package);

      if Is_Nested then
         Push_Identifier_Prefix (Mark, Get_Identifier (Decl));
      end if;

      --  Translate declarations.
      if Is_Uninstantiated then
         --  Create an instance for the spec.
         Push_Instance_Factory (Info.Package_Spec_Scope'Access);
         Chap4.Translate_Generic_Chain (Header);
         Chap4.Translate_Declaration_Chain (Decl);
         Info.Package_Elab_Var := Create_Var
           (Create_Var_Identifier ("ELABORATED"), Ghdl_Bool_Type);
         Pop_Instance_Factory (Info.Package_Spec_Scope'Access);

         --  Name the spec instance and create a pointer.
         New_Type_Decl (Create_Identifier ("SPECINSTTYPE"),
                        Get_Scope_Type (Info.Package_Spec_Scope));
         Declare_Scope_Acc (Info.Package_Spec_Scope,
                            Create_Identifier ("SPECINSTPTR"),
                            Info.Package_Spec_Ptr_Type);

         --  Create an instance and its pointer for the body.
         Chap2.Declare_Inst_Type_And_Ptr
           (Info.Package_Body_Scope'Access, Info.Package_Body_Ptr_Type);

         --  Each subprogram has a body instance argument (because subprogram
         --  bodys can access to body declarations).
         Subprgs.Push_Subprg_Instance
           (Info.Package_Body_Scope'Access, Info.Package_Body_Ptr_Type,
            Wki_Instance, Prev_Subprg_Instance);

         if not Is_Nested then
            --  For nested package, this will be translated when translating
            --  subprograms.
            Chap4.Translate_Declaration_Chain_Subprograms
              (Decl, Subprg_Translate_Only_Spec);
         end if;
      else
         if Header /= Null_Iir then
            Chap4.Translate_Generic_Chain (Header);
         end if;
         Chap4.Translate_Declaration_Chain (Decl);
         if not Is_Nested then
            Info.Package_Elab_Var := Create_Var
              (Create_Var_Identifier ("ELABORATED"), Ghdl_Bool_Type);
         end if;

         --  Translate subprograms declarations.
         if not Is_Nested then
            --  For nested package, this will be translated when translating
            --  subprograms.
            Chap4.Translate_Declaration_Chain_Subprograms
              (Decl, Subprg_Translate_Spec_And_Body);
         end if;
      end if;

      if not Is_Nested then
         --  Declare elaborator for the spec.
         Start_Procedure_Decl
           (Interface_List, Create_Identifier ("ELAB_SPEC"), Global_Storage);
         Subprgs.Add_Subprg_Instance_Interfaces
           (Interface_List, Info.Package_Elab_Spec_Instance);
         Finish_Subprogram_Decl
           (Interface_List, Info.Package_Elab_Spec_Subprg);

         --  Declare elaborator for the body.
         Start_Procedure_Decl
           (Interface_List, Create_Identifier ("ELAB_BODY"), Global_Storage);
         Subprgs.Add_Subprg_Instance_Interfaces
           (Interface_List, Info.Package_Elab_Body_Instance);
         Finish_Subprogram_Decl
           (Interface_List, Info.Package_Elab_Body_Subprg);

         if Flag_Rti then
            --  Generate RTI.
            Rtis.Generate_Unit (Decl);
         end if;
      end if;

      if Is_Uninstantiated then
         if not Get_Need_Body (Decl)
           and then Get_Package_Body (Decl) = Null_Iir
         then
            --  Generic package without a body.
            --  Create an empty body instance.
            Push_Package_Instance_Factory (Decl);
            Pop_Package_Instance_Factory (Decl);

            if not Is_Nested
              and then Global_Storage /= O_Storage_External
            then
               --  For nested package, this will be translated when translating
               --  subprograms.
               Set_Scope_Via_Field (Info.Package_Spec_Scope,
                                    Info.Package_Spec_Field,
                                    Info.Package_Body_Scope'Access);

               Chap4.Translate_Declaration_Chain_Subprograms
                 (Decl, Subprg_Translate_Only_Body);

               --  Create elaboration procedure for the spec
               Elab_Package (Decl, Header);

               Clear_Scope (Info.Package_Spec_Scope);
            end if;
         end if;

         Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
      else
         if not Is_Nested
           and then Global_Storage /= O_Storage_External
         then
            --  Create elaboration procedure for the spec
            Elab_Package (Decl, Header);
         end if;
      end if;
      Save_Local_Identifier (Info.Package_Local_Id);

      if Is_Nested then
         Pop_Identifier_Prefix (Mark);
      end if;
   end Translate_Package;

   procedure Translate_Package_Declaration (Decl : Iir_Package_Declaration) is
   begin
      --  Skip uninstantiated package that have to be macro-expanded.
      if Get_Macro_Expanded_Flag (Decl) then
         return;
      end if;

      Translate_Package (Decl, Get_Package_Header (Decl));
   end Translate_Package_Declaration;

   procedure Translate_Package_Body (Bod : Iir_Package_Body)
   is
      Is_Nested : constant Boolean := Is_Nested_Package (Bod);
      Spec      : constant Iir_Package_Declaration := Get_Package (Bod);

      --  True if the package spec is a package declaration.  It could be a
      --  package instantiation declaration.
      Is_Spec_Decl : constant Boolean :=
        Get_Kind (Spec) = Iir_Kind_Package_Declaration;

      Info      : constant Ortho_Info_Acc := Get_Info (Spec);
      Prev_Storage : constant O_Storage := Global_Storage;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
      Mark                 : Id_Mark_Type;
   begin
      if Is_Spec_Decl and then Get_Macro_Expanded_Flag (Spec) then
         return;
      end if;

      if Is_Nested then
         Push_Identifier_Prefix (Mark, Get_Identifier (Spec));
      end if;

      --  Translate declarations.
      if Is_Spec_Decl and then Is_Uninstantiated_Package (Spec) then
         Push_Package_Instance_Factory (Spec);

         --  Translate the specifications.
         Chap4.Translate_Declaration_Chain (Bod);

         Pop_Package_Instance_Factory (Spec);
      end if;

      --  May be called during elaboration to generate RTI.
      if Global_Storage = O_Storage_External then
         if Is_Nested then
            Pop_Identifier_Prefix (Mark);
         end if;
         return;
      end if;

      if not (Is_Spec_Decl and then Is_Uninstantiated_Package (Spec)) then
         Restore_Local_Identifier (Info.Package_Local_Id);

         Chap4.Translate_Declaration_Chain (Bod);
      end if;

      Global_Storage := O_Storage_Private;

      --  Generate RTI, but not for nested packages (RTI will be generated as
      --  a declaration by the parent).
      if not Is_Nested and then Flag_Rti then
         Rtis.Generate_Unit (Bod);
      end if;

      if Is_Spec_Decl and then Is_Uninstantiated_Package (Spec) then
         --  Add access to the specs.
         Subprgs.Push_Subprg_Instance
           (Info.Package_Body_Scope'Access, Info.Package_Body_Ptr_Type,
            Wki_Instance, Prev_Subprg_Instance);
         Set_Scope_Via_Field (Info.Package_Spec_Scope,
                              Info.Package_Spec_Field,
                              Info.Package_Body_Scope'Access);
      end if;

      if not Is_Nested then
         --  Translate subprograms.  For nested package, this has to be called
         --  when translating subprograms.
         Chap4.Translate_Declaration_Chain_Subprograms
           (Bod, Subprg_Translate_Spec_And_Body);
      end if;

      if Is_Spec_Decl and then Is_Uninstantiated_Package (Spec) then
         Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
         if not Is_Nested then
            Chap4.Translate_Declaration_Chain_Subprograms
              (Spec, Subprg_Translate_Only_Body);
            Elab_Package (Spec, Get_Package_Header (Spec));
         end if;
         Clear_Scope (Info.Package_Spec_Scope);
      end if;

      if not Is_Nested then
         Elab_Package_Body (Spec, Bod);
      end if;

      Global_Storage := Prev_Storage;

      if Is_Nested then
         Pop_Identifier_Prefix (Mark);
      end if;
   end Translate_Package_Body;

   --  Elaborate a package or a package instantiation.
   procedure Elab_Package (Spec : Iir; Header : Iir)
   is
      Is_Nested : constant Boolean := Is_Nested_Package (Spec);
      Info   : constant Ortho_Info_Acc := Get_Info (Spec);
      Final  : Boolean;
      Constr : O_Assoc_List;
   begin
      if not Is_Nested then
         Start_Subprogram_Body (Info.Package_Elab_Spec_Subprg);
         Push_Local_Factory;
         Subprgs.Start_Subprg_Instance_Use (Info.Package_Elab_Spec_Instance);

         Elab_Dependence (Get_Design_Unit (Spec));

         if not (Get_Kind (Spec) = Iir_Kind_Package_Declaration
                   and then Is_Uninstantiated_Package (Spec))
         then
            --  Register the top level package.  This is done dynamically, as
            --  we know only during elaboration that the design depends on a
            --  package (a package maybe referenced by an entity which is never
            --  instantiated due to generate statements).
            Start_Association (Constr, Ghdl_Rti_Add_Package);
            New_Association
              (Constr, Rtis.New_Rti_Address (Info.Package_Rti_Const));
            New_Procedure_Call (Constr);
         end if;

         Open_Temp;
      end if;

      if Is_Valid (Header)
        and then Is_Valid (Get_Generic_Map_Aspect_Chain (Header))
      then
         Chap5.Elab_Generic_Map_Aspect
           (Header, Header,
            (Info.Package_Spec_Scope'Access, Info.Package_Spec_Scope));
      end if;
      Chap4.Elab_Declaration_Chain (Spec, Final);
      pragma Unreferenced (Final);

      if not Is_Nested then
         Close_Temp;

         Subprgs.Finish_Subprg_Instance_Use (Info.Package_Elab_Spec_Instance);
         Pop_Local_Factory;
         Finish_Subprogram_Body;
      end if;
   end Elab_Package;

   procedure Elab_Package_Body (Spec : Iir_Package_Declaration; Bod : Iir)
   is
      Is_Spec_Decl : constant Boolean :=
        Get_Kind (Spec) = Iir_Kind_Package_Declaration;

      Info   : constant Ortho_Info_Acc := Get_Info (Spec);
      If_Blk : O_If_Block;
      Constr : O_Assoc_List;
      Final  : Boolean;
   begin
      if Is_Spec_Decl and then Get_Macro_Expanded_Flag (Spec) then
         return;
      end if;

      if Is_Spec_Decl and then Is_Uninstantiated_Package (Spec) then
         Set_Scope_Via_Field (Info.Package_Spec_Scope,
                              Info.Package_Spec_Field,
                              Info.Package_Body_Scope'Access);
      end if;

      Start_Subprogram_Body (Info.Package_Elab_Body_Subprg);
      Push_Local_Factory;
      Subprgs.Start_Subprg_Instance_Use (Info.Package_Elab_Body_Instance);

      --  If the package was already elaborated, return now,
      --  else mark the package as elaborated.
      Start_If_Stmt (If_Blk, New_Value (Get_Var (Info.Package_Elab_Var)));
      New_Return_Stmt;
      New_Else_Stmt (If_Blk);
      New_Assign_Stmt (Get_Var (Info.Package_Elab_Var),
                       New_Lit (Ghdl_Bool_True_Node));
      Finish_If_Stmt (If_Blk);

      --  Elab Spec.
      Start_Association (Constr, Info.Package_Elab_Spec_Subprg);
      Add_Subprg_Instance_Assoc (Constr, Info.Package_Elab_Spec_Instance);
      New_Procedure_Call (Constr);

      if Bod /= Null_Iir then
         Elab_Dependence (Get_Design_Unit (Bod));
         Open_Temp;
         Chap4.Elab_Declaration_Chain (Bod, Final);
         Close_Temp;
      end if;

      Subprgs.Finish_Subprg_Instance_Use (Info.Package_Elab_Body_Instance);
      Pop_Local_Factory;
      Finish_Subprogram_Body;

      if Is_Spec_Decl and then Is_Uninstantiated_Package (Spec) then
         Clear_Scope (Info.Package_Spec_Scope);
      end if;
   end Elab_Package_Body;

   procedure Instantiate_Iir_Info (N : Iir);

   procedure Instantiate_Iir_Chain_Info (Chain : Iir)
   is
      N : Iir;
   begin
      N := Chain;
      while N /= Null_Iir loop
         Instantiate_Iir_Info (N);
         N := Get_Chain (N);
      end loop;
   end Instantiate_Iir_Chain_Info;

   procedure Instantiate_Iir_List_Info (L : Iir_List)
   is
      It : List_Iterator;
   begin
      case L is
         when Null_Iir_List
            | Iir_List_All =>
            return;
         when others =>
            It := List_Iterate (L);
            while Is_Valid (It) loop
               Instantiate_Iir_Info (Get_Element (It));
               Next (It);
            end loop;
      end case;
   end Instantiate_Iir_List_Info;

   procedure Instantiate_Iir_Flist_Info (L : Iir_Flist)
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
               Instantiate_Iir_Info (El);
            end loop;
      end case;
   end Instantiate_Iir_Flist_Info;

   --  B must be passed by reference.
   procedure Adjust_Info_Basetype (B : access Ortho_Info_Basetype_Type;
                                   Orig : access Ortho_Info_Basetype_Type) is
   begin
      case B.Kind is
         when Kind_Type_Scalar =>
            null;
         when Kind_Type_Array
           | Kind_Type_Record =>
            B.Builder (Mode_Value).Builder_Instance :=
              Instantiate_Subprg_Instance
              (Orig.Builder (Mode_Value).Builder_Instance);
            B.Builder (Mode_Signal).Builder_Instance :=
              Instantiate_Subprg_Instance
              (Orig.Builder (Mode_Signal).Builder_Instance);
         when Kind_Type_File =>
            null;
         when Kind_Type_Protected =>
            B.Prot_Scope := Instantiate_Var_Scope (B.Prot_Scope);
            Push_Instantiate_Var_Scope
              (B.Prot_Scope'Unrestricted_access,
               Orig.Prot_Scope'Unrestricted_access);
            B.Prot_Prev_Scope := Instantiated_Var_Scope
              (B.Prot_Prev_Scope);
            B.Prot_Init_Instance := Instantiate_Subprg_Instance
              (B.Prot_Init_Instance);
            B.Prot_Final_Instance := Instantiate_Subprg_Instance
              (B.Prot_Final_Instance);
      end case;
   end Adjust_Info_Basetype;

   function Copy_Info_Subtype (Src : Ortho_Info_Subtype_Type)
                              return Ortho_Info_Subtype_Type
   is
      Res : Ortho_Info_Subtype_Type := Src;
   begin
      case Src.Kind is
         when Kind_Type_Scalar =>
            Res.Range_Var := Instantiate_Var (Src.Range_Var);
         when Kind_Type_Array
           | Kind_Type_Record =>
            Res.Composite_Layout := Instantiate_Var (Src.Composite_Layout);
         when Kind_Type_File =>
            null;
         when Kind_Type_Protected =>
            null;
      end case;
      return Res;
   end Copy_Info_Subtype;

   procedure Copy_Info (Dest : Ortho_Info_Acc; Src : Ortho_Info_Acc) is
   begin
      case Src.Kind is
         when Kind_Type =>
            Dest.all := (Kind => Kind_Type,
                         Mark => False,
                         Type_Mode => Src.Type_Mode,
                         Type_Incomplete => Src.Type_Incomplete,
                         Type_Locally_Constrained =>
                            Src.Type_Locally_Constrained,
                         Ortho_Type => Src.Ortho_Type,
                         Ortho_Ptr_Type => Src.Ortho_Ptr_Type,
                         B => Src.B,
                         S => Copy_Info_Subtype (Src.S),
                         Type_Rti => Src.Type_Rti);
            Adjust_Info_Basetype (Dest.B'Unrestricted_Access,
                                  Src.B'Unrestricted_Access);
         when Kind_Object =>
            Dest.all :=
              (Kind => Kind_Object,
               Mark => False,
               Object_Static => Src.Object_Static,
               Object_Var => Instantiate_Var (Src.Object_Var),
               Object_Rti => Src.Object_Rti);
         when Kind_Signal =>
            pragma Assert (Src.Signal_Driver = Null_Var);
            pragma Assert (Src.Signal_Function = O_Dnode_Null);
            Dest.all :=
              (Kind => Kind_Signal,
               Mark => False,
               Signal_Val => Instantiate_Var (Src.Signal_Val),
               Signal_Valp => Instantiate_Var (Src.Signal_Valp),
               Signal_Sig => Instantiate_Var (Src.Signal_Sig),
               Signal_Driver => Null_Var,
               Signal_Rti => Src.Signal_Rti,
               Signal_Function => O_Dnode_Null);
         when Kind_Subprg =>
            Dest.Subprg_Frame_Scope :=
              Instantiate_Var_Scope (Src.Subprg_Frame_Scope);
            Dest.all :=
              (Kind => Kind_Subprg,
               Mark => False,
               Use_Stack2 => Src.Use_Stack2,
               Subprg_Node => Src.Subprg_Node,
               Res_Interface => Src.Res_Interface,
               Subprg_Params_Var => Instantiate_Var (Src.Subprg_Params_Var),
               Subprg_Params_Type => Src.Subprg_Params_Type,
               Subprg_Params_Ptr => Src.Subprg_Params_Ptr,
               Subprg_State_Field => Src.Subprg_State_Field,
               Subprg_Locvars_Field => Src.Subprg_Locvars_Field,
               Subprg_Locvars_Scope => Src.Subprg_Locvars_Scope,
               Subprg_Frame_Scope => Dest.Subprg_Frame_Scope,
               Subprg_Instance => Instantiate_Subprg_Instance
                 (Src.Subprg_Instance),
               Subprg_Resolv => null,
               Subprg_Local_Id => Src.Subprg_Local_Id,
               Subprg_Exit => Src.Subprg_Exit,
               Subprg_Result => Src.Subprg_Result);
            Push_Instantiate_Var_Scope
              (Dest.Subprg_Frame_Scope'Access,
               Src.Subprg_Frame_Scope'Access);
         when Kind_Operator =>
            Dest.all :=
              (Kind => Kind_Operator,
               Mark => False,
               Operator_Stack2 => Src.Operator_Stack2,
               Operator_Body => Src.Operator_Body,
               Operator_Node => Src.Operator_Node,
               Operator_Instance => Instantiate_Subprg_Instance
                 (Src.Operator_Instance),
               Operator_Left => Src.Operator_Left,
               Operator_Right => Src.Operator_Right,
               Operator_Res => Src.Operator_Res);
         when Kind_Interface =>
            Dest.all := (Kind => Kind_Interface,
                         Mark => False,
                         Interface_Mechanism => Src.Interface_Mechanism,
                         Interface_Decl => Src.Interface_Decl,
                         Interface_Field => Src.Interface_Field);
         when Kind_Index =>
            Dest.all := (Kind => Kind_Index,
                         Mark => False,
                         Index_Field => Src.Index_Field);
         when Kind_Enum_Lit =>
            Dest.all := (Kind => Kind_Enum_Lit,
                         Mark => False,
                         Lit_Node => Src.Lit_Node);
         when Kind_Package_Instance =>
            Dest.all :=
              (Kind => Kind_Package_Instance,
               Mark => False,
               Package_Instance_Spec_Var =>
                 Instantiate_Var (Src.Package_Instance_Spec_Var),
               Package_Instance_Body_Var =>
                 Instantiate_Var (Src.Package_Instance_Body_Var),
               Package_Instance_Elab_Subprg =>
                 Src.Package_Instance_Elab_Subprg,
               Package_Instance_Spec_Scope => Null_Var_Scope,
               Package_Instance_Body_Scope =>
                 Instantiate_Var_Scope (Src.Package_Instance_Body_Scope));
            --  The body scope needs to be instantiated before instantiating
            --  the spec scope, as the spec scope is a field of the body
            --  scope.
            Push_Instantiate_Var_Scope
              (Dest.Package_Instance_Body_Scope'Access,
               Src.Package_Instance_Body_Scope'Access);
            Dest.Package_Instance_Spec_Scope :=
              Instantiate_Var_Scope (Src.Package_Instance_Spec_Scope);
            Push_Instantiate_Var_Scope
              (Dest.Package_Instance_Spec_Scope'Access,
               Src.Package_Instance_Spec_Scope'Access);
         when Kind_Field =>
            Dest.all := (Kind => Kind_Field,
                         Mark => False,
                         Field_Node => Src.Field_Node,
                         Field_Bound => Src.Field_Bound);
         when Kind_Component =>
            Dest.all :=
              (Kind => Kind_Component,
               Mark => False,
               Comp_Scope => Instantiate_Var_Scope (Src.Comp_Scope),
               Comp_Ptr_Type => Src.Comp_Ptr_Type,
               Comp_Link => Src.Comp_Link,
               Comp_Rti_Const => Src.Comp_Rti_Const);
         when Kind_Package =>
            Dest.all :=
              (Kind => Kind_Package,
               Mark => False,
               Package_Elab_Spec_Subprg => Src.Package_Elab_Spec_Subprg,
               Package_Elab_Body_Subprg => Src.Package_Elab_Body_Subprg,
               Package_Elab_Spec_Instance =>
                 Instantiate_Subprg_Instance (Src.Package_Elab_Spec_Instance),
               Package_Elab_Body_Instance =>
                 Instantiate_Subprg_Instance (Src.Package_Elab_Body_Instance),
               Package_Elab_Var => Instantiate_Var (Src.Package_Elab_Var),
               Package_Rti_Const => Src.Package_Rti_Const,
               Package_Spec_Scope =>
                 Instantiate_Var_Scope (Src.Package_Spec_Scope),
               Package_Spec_Ptr_Type => Src.Package_Spec_Ptr_Type,
               Package_Body_Scope =>
                 Instantiate_Var_Scope (Src.Package_Body_Scope),
               Package_Body_Ptr_Type => Src.Package_Body_Ptr_Type,
               Package_Spec_Field => Src.Package_Spec_Field,
               Package_Local_Id => Src.Package_Local_Id);

         when others =>
            raise Internal_Error;
      end case;
   end Copy_Info;

   procedure Clean_Copy_Info (Info : Ortho_Info_Acc) is
   begin
      --  Pop scope instantiations created in copy_info.
      case Info.Kind is
         when Kind_Subprg =>
            Pop_Instantiate_Var_Scope
              (Info.Subprg_Frame_Scope'Access);
         when Kind_Type =>
            case Info.B.Kind is
               when Kind_Type_Protected =>
                  Pop_Instantiate_Var_Scope
                    (Info.B.Prot_Scope'Unrestricted_access);
               when others =>
                  null;
            end case;
         when Kind_Package_Instance =>
            --  The order is important: it must be the reverse order of the
            --  push.
            Pop_Instantiate_Var_Scope
              (Info.Package_Instance_Spec_Scope'Access);
            Pop_Instantiate_Var_Scope
              (Info.Package_Instance_Body_Scope'Access);
         when others =>
            null;
      end case;
   end Clean_Copy_Info;

   procedure Instantiate_Iir_Info (N : Iir) is
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
         Orig      : constant Iir := Vhdl.Sem_Inst.Get_Origin (N);
         pragma Assert (Orig /= Null_Iir);
         Orig_Info : constant Ortho_Info_Acc := Get_Info (Orig);
         Info      : Ortho_Info_Acc;
      begin
         if Orig_Info /= null then
            Info := Add_Info (N, Orig_Info.Kind);

            Copy_Info (Info, Orig_Info);
         end if;

         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Instantiate_Iir_Info (Get_Iir (N, F));
                     when Attr_Ref
                       | Attr_Forward_Ref
                       | Attr_Maybe_Forward_Ref =>
                        null;
                     when Attr_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Instantiate_Iir_Info (Get_Iir (N, F));
                        end if;
                     when Attr_Chain =>
                        Instantiate_Iir_Chain_Info (Get_Iir (N, F));
                     when Attr_Chain_Next =>
                        null;
                     when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_List =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Instantiate_Iir_List_Info (Get_Iir_List (N, F));
                     when Attr_Of_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Instantiate_Iir_List_Info (Get_Iir_List (N, F));
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
                        Instantiate_Iir_Flist_Info (Get_Iir_Flist (N, F));
                     when Attr_Of_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Instantiate_Iir_Flist_Info (Get_Iir_Flist (N, F));
                        end if;
                     when Attr_Ref
                        | Attr_Of_Ref =>
                        null;
                     when others =>
                        raise Internal_Error;
                  end case;
               when Type_PSL_NFA
                  | Type_PSL_Node =>
                  --  TODO
                  raise Internal_Error;
               when Type_Date_Type
                  | Type_Date_State_Type
                  | Type_Time_Stamp_Id
                  | Type_File_Checksum_Id =>
                  --  Can this happen ?
                  raise Internal_Error;
               when Type_String8_Id
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

         if Info /= null then
            Clean_Copy_Info (Info);
         end if;

         --  Adjust Subtype_Owner.
         case Get_Kind (N) is
            when Iir_Kind_Array_Subtype_Definition =>
               declare
                  El_Type : constant Iir := Get_Element_Subtype (N);
                  El_Tinfo : constant Type_Info_Acc := Get_Info (El_Type);
               begin
                  if El_Tinfo.S.Kind in Kind_Type_Array .. Kind_Type_Record
                    and then El_Tinfo.S.Subtype_Owner = Orig_Info
                  then
                     pragma Assert (Info /= null);
                     El_Tinfo.S.Subtype_Owner := Info;
                  end if;
               end;
            when Iir_Kind_Record_Subtype_Definition =>
               declare
                  El : Iir;
                  El_Type : Iir;
                  El_Tinfo : Type_Info_Acc;
               begin
                  El := Get_Owned_Elements_Chain (N);
                  while El /= Null_Iir loop
                     El_Type := Get_Type (El);
                     El_Tinfo := Get_Info (El_Type);
                     if El_Tinfo.S.Kind in Kind_Type_Array .. Kind_Type_Record
                       and then El_Tinfo.S.Subtype_Owner = Orig_Info
                     then
                        pragma Assert (Info /= null);
                        El_Tinfo.S.Subtype_Owner := Info;
                     end if;
                     El := Get_Chain (El);
                  end loop;
               end;
            when others =>
               null;
         end case;
      end;
   end Instantiate_Iir_Info;

   procedure Instantiate_Iir_Generic_Chain_Info (Chain : Iir)
   is
      Inter     : Iir;
      Orig      : Iir;
      Orig_Info : Ortho_Info_Acc;
      Info      : Ortho_Info_Acc;
   begin
      Inter := Chain;
      while Inter /= Null_Iir loop
         Orig := Vhdl.Sem_Inst.Get_Origin (Inter);
         Orig_Info := Get_Info (Orig);

         Info := Add_Info (Inter, Orig_Info.Kind);
         Copy_Info (Info, Orig_Info);

         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Constant_Declaration =>
               null;

            when Iir_Kind_Interface_Package_Declaration =>
               Instantiate_Iir_Generic_Chain_Info (Get_Generic_Chain (Inter));
               Instantiate_Iir_Chain_Info (Get_Declaration_Chain (Inter));

            when others =>
               raise Internal_Error;
         end case;

         Clean_Copy_Info (Info);

         Inter := Get_Chain (Inter);
      end loop;
   end Instantiate_Iir_Generic_Chain_Info;

   --  Add info for an interface_package_declaration or a
   --  package_instantiation_declaration
   procedure Instantiate_Info_Package (Inst : Iir)
   is
      Spec     : constant Iir := Get_Uninstantiated_Package_Decl (Inst);
      Pkg_Info : constant Ortho_Info_Acc := Get_Info (Spec);
      Info     : constant Ortho_Info_Acc := Get_Info (Inst);
   begin
      --  Create the info instances.
      Push_Instantiate_Var_Scope
        (Info.Package_Instance_Spec_Scope'Access,
         Pkg_Info.Package_Spec_Scope'Access);
      Push_Instantiate_Var_Scope
        (Info.Package_Instance_Body_Scope'Access,
         Pkg_Info.Package_Body_Scope'Access);

      Instantiate_Iir_Generic_Chain_Info (Get_Generic_Chain (Inst));
      Instantiate_Iir_Chain_Info (Get_Declaration_Chain (Inst));

      Pop_Instantiate_Var_Scope
        (Info.Package_Instance_Body_Scope'Access);
      Pop_Instantiate_Var_Scope
        (Info.Package_Instance_Spec_Scope'Access);
   end Instantiate_Info_Package;

   procedure Translate_Package_Instantiation_Declaration (Inst : Iir)
   is
      Spec           : constant Iir := Get_Uninstantiated_Package_Decl (Inst);
      Pkg_Info       : constant Ortho_Info_Acc := Get_Info (Spec);
      Info           : Ortho_Info_Acc;
      Interface_List : O_Inter_List;
   begin
      if Get_Macro_Expanded_Flag (Spec) then
         --  Macro-expanded instantiations are translated like a package.
         Translate_Package (Inst, Inst);

         --  Generate code for the body.
         if Global_Storage /= O_Storage_External then
            declare
               Bod : constant Iir := Get_Instance_Package_Body (Inst);
            begin
               if Is_Valid (Bod) then
                  Translate_Package_Body (Bod);
               else
                  --  As an elaboration subprogram for the body is always
                  --  needed, generate it.
                  if not Is_Nested_Package (Inst) then
                     Elab_Package_Body (Inst, Null_Iir);
                  end if;
               end if;
            end;
         end if;

         return;
      end if;

      Info := Add_Info (Inst, Kind_Package_Instance);

      --  Create the variable containing data for the package instance.
      Info.Package_Instance_Body_Var := Create_Var
        (Create_Var_Identifier (Inst),
         Get_Scope_Type (Pkg_Info.Package_Body_Scope));

      --  FIXME: this is correct only for global instantiation, and only if
      --  there is only one.
      Set_Scope_Via_Var (Info.Package_Instance_Body_Scope,
                         Info.Package_Instance_Body_Var);
      Set_Scope_Via_Field (Info.Package_Instance_Spec_Scope,
                           Pkg_Info.Package_Spec_Field,
                           Info.Package_Instance_Body_Scope'Access);

      Instantiate_Info_Package (Inst);

      if Is_Nested_Package (Inst) then
         return;
      end if;

      --  Declare elaboration procedure
      Start_Procedure_Decl
        (Interface_List, Create_Identifier ("ELAB"), Global_Storage);
      --  Chap2.Add_Subprg_Instance_Interfaces
      --   (Interface_List, Info.Package_Instance_Elab_Instance);
      Finish_Subprogram_Decl
        (Interface_List, Info.Package_Instance_Elab_Subprg);

      if Global_Storage = O_Storage_External then
         return;
      end if;

      --  Elaborator:
      Start_Subprogram_Body (Info.Package_Instance_Elab_Subprg);
      --  Chap2.Start_Subprg_Instance_Use
      --    (Info.Package_Instance_Elab_Instance);

      Elab_Dependence (Get_Design_Unit (Inst));

      Elab_Package_Instantiation_Declaration (Inst);

      --  Chap2.Finish_Subprg_Instance_Use
      --    (Info.Package_Instance_Elab_Instance);
      Finish_Subprogram_Body;
   end Translate_Package_Instantiation_Declaration;

   procedure Elab_Package_Instantiation_Declaration (Inst : Iir)
   is
      Spec           : constant Iir := Get_Uninstantiated_Package_Decl (Inst);
      Pkg_Info       : constant Ortho_Info_Acc := Get_Info (Spec);
      Info           : constant Ortho_Info_Acc := Get_Info (Inst);
      Constr         : O_Assoc_List;
   begin
      --  Macro-expanded instances are handled like a regular package.
      if Get_Macro_Expanded_Flag (Spec) then
         Elab_Package (Inst, Inst);
         return;
      end if;

      --  Package body is reachable through the instance.
      Set_Scope_Via_Var (Pkg_Info.Package_Body_Scope,
                         Info.Package_Instance_Body_Var);
      Set_Scope_Via_Field (Pkg_Info.Package_Spec_Scope,
                           Pkg_Info.Package_Spec_Field,
                           Pkg_Info.Package_Body_Scope'Access);

      Chap5.Elab_Generic_Map_Aspect
        (Get_Package_Header (Spec), Inst,
         (Pkg_Info.Package_Body_Scope'Access, Pkg_Info.Package_Body_Scope));

      --  Call the elaborator of the generic.  The generic must be
      --  temporary associated with the instance variable.
      Start_Association (Constr, Pkg_Info.Package_Elab_Body_Subprg);
      Add_Subprg_Instance_Assoc
        (Constr, Pkg_Info.Package_Elab_Body_Instance);
      New_Procedure_Call (Constr);

      Clear_Scope (Pkg_Info.Package_Body_Scope);
      Clear_Scope (Pkg_Info.Package_Spec_Scope);
   end Elab_Package_Instantiation_Declaration;

   procedure Elab_Dependence_Package (Pkg : Iir)
   is
      Info   : Ortho_Info_Acc;
      If_Blk : O_If_Block;
      Constr : O_Assoc_List;
   begin
      --  Call the package elaborator only if not already elaborated.
      Info := Get_Info (Pkg);
      Start_If_Stmt
        (If_Blk,
         New_Monadic_Op (ON_Not,
           New_Value (Get_Var (Info.Package_Elab_Var))));
      -- Elaborates only non-elaborated packages.
      Start_Association (Constr, Info.Package_Elab_Body_Subprg);
      New_Procedure_Call (Constr);
      Finish_If_Stmt (If_Blk);
   end Elab_Dependence_Package;

   procedure Elab_Dependence_Package_Declaration
     (Pkg : Iir_Package_Declaration) is
   begin
      --  Std.Standard is pre-elaborated.
      if Pkg = Standard_Package then
         return;
      end if;

      --  Nothing to do for uninstantiated package.
      if Is_Uninstantiated_Package (Pkg) then
         return;
      end if;

      Elab_Dependence_Package (Pkg);
   end Elab_Dependence_Package_Declaration;

   procedure Elab_Dependence_Package_Instantiation (Pkg : Iir) is
   begin
      if Get_Macro_Expanded_Flag (Get_Uninstantiated_Package_Decl (Pkg)) then
         --  Handled as a normal package
         Elab_Dependence_Package (Pkg);
      else
         declare
            Info   : constant Ortho_Info_Acc := Get_Info (Pkg);
            Constr : O_Assoc_List;
         begin
            Start_Association (Constr, Info.Package_Instance_Elab_Subprg);
            New_Procedure_Call (Constr);
         end;
      end if;
   end Elab_Dependence_Package_Instantiation;

   procedure Elab_Dependence (Design_Unit: Iir_Design_Unit)
   is
      Depend_List : constant Iir_List := Get_Dependence_List (Design_Unit);
      It : List_Iterator;
      Design      : Iir;
      Library_Unit: Iir;
   begin
      It := List_Iterate (Depend_List);
      while Is_Valid (It) loop
         Design := Get_Element (It);
         if Get_Kind (Design) = Iir_Kind_Design_Unit then
            Library_Unit := Get_Library_Unit (Design);
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Package_Declaration =>
                  Elab_Dependence_Package_Declaration (Library_Unit);
               when Iir_Kind_Package_Instantiation_Declaration =>
                  Elab_Dependence_Package_Instantiation (Library_Unit);
               when Iir_Kind_Entity_Declaration =>
                  --  FIXME: architecture already elaborates its entity.
                  null;
               when Iir_Kind_Configuration_Declaration =>
                  null;
               when Iir_Kind_Architecture_Body =>
                  null;
               when Iir_Kind_Package_Body =>
                  --  A package instantiation depends on the body.
                  null;
               when Iir_Kind_Context_Declaration =>
                  --  Elab referenced packages.
                  Elab_Dependence (Design);
               when others =>
                  Error_Kind ("elab_dependence", Library_Unit);
            end case;
         end if;
         Next (It);
      end loop;
   end Elab_Dependence;

   procedure Declare_Inst_Type_And_Ptr (Scope    : Var_Scope_Acc;
                                        Ptr_Type : out O_Tnode) is
   begin
      Predeclare_Scope_Type (Scope.all, Create_Identifier ("INSTTYPE"));
      Declare_Scope_Acc
        (Scope.all, Create_Identifier ("INSTPTR"), Ptr_Type);
   end Declare_Inst_Type_And_Ptr;

end Trans.Chap2;
