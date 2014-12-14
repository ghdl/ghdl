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

with Name_Table;
with Std_Names;
with Std_Package; use Std_Package;
with Errorout; use Errorout;
with Sem_Inst;
with Nodes_Meta;
with Iirs_Utils; use Iirs_Utils;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap5;
with Trans.Chap6;
with Trans.Chap8;
with Trans.Rtis;
with Trans_Decls; use Trans_Decls;
with Translation; use Translation;

package body Trans.Chap2 is
   use Trans.Subprgs;
   use Trans.Helpers;

   procedure Elab_Package (Spec : Iir_Package_Declaration);

   type Name_String_Xlat_Array is array (Name_Id range <>) of
     String (1 .. 4);
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
      Id : Name_Id;
   begin
      --  FIXME: name_shift_operators, name_logical_operators,
      --   name_word_operators, name_mod, name_rem
      Id := Get_Identifier (Spec);
      if Id in Std_Names.Name_Id_Operators then
         Push_Identifier_Prefix
           (Mark, Operator_String_Xlat (Id), Get_Overload_Number (Spec));
      else
         Push_Identifier_Prefix (Mark, Id, Get_Overload_Number (Spec));
      end if;
   end Push_Subprg_Identifier;

   procedure Translate_Subprogram_Interfaces (Spec : Iir)
   is
      Inter : Iir;
      Mark  : Id_Mark_Type;
   begin
      --  Set the identifier prefix with the subprogram identifier and
      --  overload number if any.
      Push_Subprg_Identifier (Spec, Mark);

      --  Translate interface types.
      Inter := Get_Interface_Declaration_Chain (Spec);
      while Inter /= Null_Iir loop
         Chap3.Translate_Object_Subtype (Inter);
         Inter := Get_Chain (Inter);
      end loop;
      Pop_Identifier_Prefix (Mark);
   end Translate_Subprogram_Interfaces;

   procedure Elab_Subprogram_Interfaces (Spec : Iir)
   is
      Inter : Iir;
   begin
      --  Translate interface types.
      Inter := Get_Interface_Declaration_Chain (Spec);
      while Inter /= Null_Iir loop
         Chap3.Elab_Object_Subtype (Get_Type (Inter));
         Inter := Get_Chain (Inter);
      end loop;
   end Elab_Subprogram_Interfaces;


   --  Return the type of a subprogram interface.
   --  Return O_Tnode_Null if the parameter is passed through the
   --  interface record.
   function Translate_Interface_Type (Inter : Iir) return O_Tnode
   is
      Mode  : Object_Kind_Type;
      Tinfo : constant Type_Info_Acc := Get_Info (Get_Type (Inter));
   begin
      case Get_Kind (Inter) is
         when Iir_Kind_Interface_Constant_Declaration
            | Iir_Kind_Interface_Variable_Declaration
            | Iir_Kind_Interface_File_Declaration =>
            Mode := Mode_Value;
         when Iir_Kind_Interface_Signal_Declaration =>
            Mode := Mode_Signal;
         when others =>
            Error_Kind ("translate_interface_type", Inter);
      end case;
      case Tinfo.Type_Mode is
         when Type_Mode_Unknown =>
            raise Internal_Error;
         when Type_Mode_By_Value =>
            return Tinfo.Ortho_Type (Mode);
         when Type_Mode_By_Copy
            | Type_Mode_By_Ref =>
            return Tinfo.Ortho_Ptr_Type (Mode);
      end case;
   end Translate_Interface_Type;

   procedure Translate_Subprogram_Declaration (Spec : Iir)
   is
      Info              : constant Subprg_Info_Acc := Get_Info (Spec);
      Is_Func           : constant Boolean :=
        Get_Kind (Spec) = Iir_Kind_Function_Declaration;
      Inter             : Iir;
      Inter_Type        : Iir;
      Arg_Info          : Ortho_Info_Acc;
      Tinfo             : Type_Info_Acc;
      Interface_List    : O_Inter_List;
      Has_Result_Record : Boolean;
      El_List           : O_Element_List;
      Mark              : Id_Mark_Type;
      Rtype             : Iir;
      Id                : O_Ident;
      Storage           : O_Storage;
      Foreign           : Foreign_Info_Type := Foreign_Bad;
   begin
      --  Set the identifier prefix with the subprogram identifier and
      --  overload number if any.
      Push_Subprg_Identifier (Spec, Mark);

      if Get_Foreign_Flag (Spec) then
         --  Special handling for foreign subprograms.
         Foreign := Translate_Foreign_Id (Spec);
         case Foreign.Kind is
            when Foreign_Unknown =>
               Id := Create_Identifier;
            when Foreign_Intrinsic =>
               Id := Create_Identifier;
            when Foreign_Vhpidirect =>
               Id := Get_Identifier
                 (Name_Table.Name_Buffer (Foreign.Subprg_First
                  .. Foreign.Subprg_Last));
         end case;
         Storage := O_Storage_External;
      else
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
         --  Create info for each interface of the procedure.
         --  For parameters passed via copy and that needs a copy-out,
         --  gather them in a record.  An access to the record is then
         --  passed to the procedure.
         Has_Result_Record := False;
         Inter := Get_Interface_Declaration_Chain (Spec);
         while Inter /= Null_Iir loop
            Arg_Info := Add_Info (Inter, Kind_Interface);
            Inter_Type := Get_Type (Inter);
            Tinfo := Get_Info (Inter_Type);
            if Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
              and then Get_Mode (Inter) in Iir_Out_Modes
              and then Tinfo.Type_Mode not in Type_Mode_By_Ref
              and then Tinfo.Type_Mode /= Type_Mode_File
            then
               --  This interface is done via the result record.
               --  Note: file passed through variables are vhdl87 files,
               --        which are initialized at elaboration and thus
               --        behave like an IN parameter.
               if not Has_Result_Record then
                  --  Create the record.
                  Start_Record_Type (El_List);
                  Has_Result_Record := True;
               end if;
               --  Add a field to the record.
               New_Record_Field (El_List, Arg_Info.Interface_Field,
                                 Create_Identifier_Without_Prefix (Inter),
                                 Tinfo.Ortho_Type (Mode_Value));
            else
               Arg_Info.Interface_Field := O_Fnode_Null;
            end if;
            Inter := Get_Chain (Inter);
         end loop;
         if Has_Result_Record then
            --  Declare the record type and an access to the record.
            Finish_Record_Type (El_List, Info.Res_Record_Type);
            New_Type_Decl (Create_Identifier ("RESTYPE"),
                           Info.Res_Record_Type);
            Info.Res_Record_Ptr := New_Access_Type (Info.Res_Record_Type);
            New_Type_Decl (Create_Identifier ("RESPTR"),
                           Info.Res_Record_Ptr);
         else
            Info.Res_Interface := O_Dnode_Null;
         end if;

         Start_Procedure_Decl (Interface_List, Id, Storage);

         if Has_Result_Record then
            --  Add the record parameter.
            New_Interface_Decl (Interface_List, Info.Res_Interface,
                                Get_Identifier ("RESULT"),
                                Info.Res_Record_Ptr);
         end if;
      end if;

      --  Instance parameter if any.
      if not Get_Foreign_Flag (Spec) then
         Subprgs.Create_Subprg_Instance (Interface_List, Spec);
      end if;

      --  Translate interfaces.
      Inter := Get_Interface_Declaration_Chain (Spec);
      while Inter /= Null_Iir loop
         if Is_Func then
            --  Create the info.
            Arg_Info := Add_Info (Inter, Kind_Interface);
            Arg_Info.Interface_Field := O_Fnode_Null;
         else
            --  The info was already created (just above)
            Arg_Info := Get_Info (Inter);
         end if;

         if Arg_Info.Interface_Field = O_Fnode_Null then
            --  Not via the RESULT parameter.
            Arg_Info.Interface_Type := Translate_Interface_Type (Inter);
            New_Interface_Decl
              (Interface_List, Arg_Info.Interface_Node,
               Create_Identifier_Without_Prefix (Inter),
               Arg_Info.Interface_Type);
         end if;
         Inter := Get_Chain (Inter);
      end loop;
      Finish_Subprogram_Decl (Interface_List, Info.Ortho_Func);

      --  Call the hook for foreign subprograms.
      if Get_Foreign_Flag (Spec) and then Foreign_Hook /= null then
         Foreign_Hook.all (Spec, Foreign, Info.Ortho_Func);
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

   --  Return TRUE iif SUBPRG_BODY declares explicitely or implicitely
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
               case Iir_Kinds_Type_And_Subtype_Definition
                 (Get_Kind (Atype)) is
                  when Iir_Kinds_Scalar_Type_Definition =>
                     null;
                  when Iir_Kind_Access_Type_Definition
                     | Iir_Kind_Access_Subtype_Definition =>
                     null;
                  when Iir_Kind_File_Type_Definition =>
                     return True;
                  when Iir_Kind_Protected_Type_Declaration =>
                     raise Internal_Error;
                  when Iir_Kinds_Composite_Type_Definition =>
                     --  At least for "=".
                     return True;
                  when Iir_Kind_Incomplete_Type_Definition =>
                     null;
               end case;
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

      Old_Subprogram : Iir;
      Mark           : Id_Mark_Type;
      Final          : Boolean;
      Is_Ortho_Func  : Boolean;

      --  Set for a public method.  In this case, the lock must be acquired
      --  and retained.
      Is_Prot : Boolean := False;

      --  True if the body has local (nested) subprograms.
      Has_Nested : Boolean;

      Frame_Ptr_Type : O_Tnode;
      Upframe_Field  : O_Fnode;

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

      if Has_Nested then
         --  Unnest subprograms.
         --  Create an instance for the local declarations.
         Push_Instance_Factory (Info.Subprg_Frame_Scope'Access);
         Add_Subprg_Instance_Field (Upframe_Field);

         if Info.Res_Record_Ptr /= O_Tnode_Null then
            Info.Res_Record_Var :=
              Create_Var (Create_Var_Identifier ("RESULT"),
                          Info.Res_Record_Ptr);
         end if;

         --  Create fields for parameters.
         --  FIXME: do it only if they are referenced in nested
         --  subprograms.
         declare
            Inter      : Iir;
            Inter_Info : Inter_Info_Acc;
         begin
            Inter := Get_Interface_Declaration_Chain (Spec);
            while Inter /= Null_Iir loop
               Inter_Info := Get_Info (Inter);
               if Inter_Info.Interface_Node /= O_Dnode_Null then
                  Inter_Info.Interface_Field :=
                    Add_Instance_Factory_Field
                      (Create_Identifier_Without_Prefix (Inter),
                       Inter_Info.Interface_Type);
               end if;
               Inter := Get_Chain (Inter);
            end loop;
         end;

         Chap4.Translate_Declaration_Chain (Subprg);
         Pop_Instance_Factory (Info.Subprg_Frame_Scope'Access);

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
           (Prev_Subprg_Instances, Upframe_Field);

         Chap4.Translate_Declaration_Chain_Subprograms (Subprg);

         --  Link to previous frame
         Subprgs.Finish_Prev_Subprg_Instance_Use_Via_Field
           (Prev_Subprg_Instances, Upframe_Field);
         --  Local frame
         Subprgs.Pop_Subprg_Instance (Wki_Upframe, Prev_Subprg_Instances);
      end if;

      --  Create the body

      Start_Subprogram_Body (Info.Ortho_Func);

      Start_Subprg_Instance_Use (Spec);

      --  Variables will be created on the stack.
      Push_Local_Factory;

      --  Code has access to local (and outer) variables.
      --  FIXME: this is not necessary if Has_Nested is set
      Subprgs.Clear_Subprg_Instance (Prev_Subprg_Instances);

      --  There is a local scope for temporaries.
      Open_Local_Temp;

      if not Has_Nested then
         Chap4.Translate_Declaration_Chain (Subprg);
         Rtis.Generate_Subprogram_Body (Subprg);
         Chap4.Translate_Declaration_Chain_Subprograms (Subprg);
      else
         New_Var_Decl (Frame, Wki_Frame, O_Storage_Local,
                       Get_Scope_Type (Info.Subprg_Frame_Scope));

         New_Var_Decl (Frame_Ptr, Get_Identifier ("FRAMEPTR"),
                       O_Storage_Local, Frame_Ptr_Type);
         New_Assign_Stmt (New_Obj (Frame_Ptr),
                          New_Address (New_Obj (Frame), Frame_Ptr_Type));

         --  FIXME: use direct reference (ie Frame instead of Frame_Ptr)
         Set_Scope_Via_Param_Ptr (Info.Subprg_Frame_Scope, Frame_Ptr);

         --  Set UPFRAME.
         Subprgs.Set_Subprg_Instance_Field
           (Frame_Ptr, Upframe_Field, Info.Subprg_Instance);

         if Info.Res_Record_Type /= O_Tnode_Null then
            --  Initialize the RESULT field
            New_Assign_Stmt (Get_Var (Info.Res_Record_Var),
                             New_Obj_Value (Info.Res_Interface));
            --  Do not reference the RESULT field in the subprogram body,
            --  directly reference the RESULT parameter.
            --  FIXME: has a flag (see below for parameters).
            Info.Res_Record_Var := Null_Var;
         end if;

         --  Copy parameters to FRAME.
         declare
            Inter      : Iir;
            Inter_Info : Inter_Info_Acc;
         begin
            Inter := Get_Interface_Declaration_Chain (Spec);
            while Inter /= Null_Iir loop
               Inter_Info := Get_Info (Inter);
               if Inter_Info.Interface_Node /= O_Dnode_Null then
                  New_Assign_Stmt
                    (New_Selected_Element (New_Obj (Frame),
                     Inter_Info.Interface_Field),
                     New_Obj_Value (Inter_Info.Interface_Node));

                  --  Forget the reference to the field in FRAME, so that
                  --  this subprogram will directly reference the parameter
                  --  (and not its copy in the FRAME).
                  Inter_Info.Interface_Field := O_Fnode_Null;
               end if;
               Inter := Get_Chain (Inter);
            end loop;
         end;
      end if;

      --  Init out parameters passed by value/copy.
      declare
         Inter      : Iir;
         Inter_Type : Iir;
         Type_Info  : Type_Info_Acc;
      begin
         Inter := Get_Interface_Declaration_Chain (Spec);
         while Inter /= Null_Iir loop
            if Get_Kind (Inter) = Iir_Kind_Interface_Variable_Declaration
              and then Get_Mode (Inter) = Iir_Out_Mode
            then
               Inter_Type := Get_Type (Inter);
               Type_Info := Get_Info (Inter_Type);
               if (Type_Info.Type_Mode in Type_Mode_By_Value
                   or Type_Info.Type_Mode in Type_Mode_By_Copy)
                 and then Type_Info.Type_Mode /= Type_Mode_File
               then
                  Chap4.Init_Object
                    (Chap6.Translate_Name (Inter), Inter_Type);
               end if;
            end if;
            Inter := Get_Chain (Inter);
         end loop;
      end;

      Chap4.Elab_Declaration_Chain (Subprg, Final);

      --  If finalization is required, create a dummy loop around the
      --  body and convert returns into exit out of this loop.
      --  If the subprogram is a function, also create a variable for the
      --  result.
      Is_Prot := Is_Subprogram_Method (Spec);
      if Final or Is_Prot then
         if Is_Prot then
            --  Lock the object.
            Chap3.Call_Ghdl_Protected_Procedure (Get_Method_Type (Spec),
                                                 Ghdl_Protected_Enter);
         end if;
         Is_Ortho_Func := Is_Subprogram_Ortho_Function (Spec);
         if Is_Ortho_Func then
            New_Var_Decl
              (Info.Subprg_Result, Get_Identifier ("RESULT"),
               O_Storage_Local,
               Get_Ortho_Type (Get_Return_Type (Spec), Mode_Value));
         end if;
         Start_Loop_Stmt (Info.Subprg_Exit);
      end if;

      Old_Subprogram := Current_Subprogram;
      Current_Subprogram := Spec;
      Has_Return := Chap8.Translate_Statements_Chain_Has_Return
        (Get_Sequential_Statement_Chain (Subprg));
      Current_Subprogram := Old_Subprogram;

      if Final or Is_Prot then
         --  Create a barrier to catch missing return statement.
         if Get_Kind (Spec) = Iir_Kind_Procedure_Declaration then
            New_Exit_Stmt (Info.Subprg_Exit);
         else
            if not Has_Return then
               --  Missing return
               Chap6.Gen_Program_Error
                 (Subprg, Chap6.Prg_Err_Missing_Return);
            end if;
         end if;
         Finish_Loop_Stmt (Info.Subprg_Exit);
         Chap4.Final_Declaration_Chain (Subprg, False);

         if Is_Prot then
            --  Unlock the object.
            Chap3.Call_Ghdl_Protected_Procedure (Get_Method_Type (Spec),
                                                 Ghdl_Protected_Leave);
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

   procedure Translate_Package_Declaration (Decl : Iir_Package_Declaration)
   is
      Header               : constant Iir := Get_Package_Header (Decl);
      Info                 : Ortho_Info_Acc;
      Interface_List       : O_Inter_List;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Info := Add_Info (Decl, Kind_Package);

      --  Translate declarations.
      if Is_Uninstantiated_Package (Decl) then
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

         --  Each subprogram has a body instance argument.
         Subprgs.Push_Subprg_Instance
           (Info.Package_Body_Scope'Access, Info.Package_Body_Ptr_Type,
            Wki_Instance, Prev_Subprg_Instance);
      else
         Chap4.Translate_Declaration_Chain (Decl);
         Info.Package_Elab_Var := Create_Var
           (Create_Var_Identifier ("ELABORATED"), Ghdl_Bool_Type);
      end if;

      --  Translate subprograms declarations.
      Chap4.Translate_Declaration_Chain_Subprograms (Decl);

      --  Declare elaborator for the body.
      Start_Procedure_Decl
        (Interface_List, Create_Identifier ("ELAB_BODY"), Global_Storage);
      Subprgs.Add_Subprg_Instance_Interfaces
        (Interface_List, Info.Package_Elab_Body_Instance);
      Finish_Subprogram_Decl
        (Interface_List, Info.Package_Elab_Body_Subprg);

      if Is_Uninstantiated_Package (Decl) then
         Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);

         --  The spec elaborator has a spec instance argument.
         Subprgs.Push_Subprg_Instance
           (Info.Package_Spec_Scope'Access, Info.Package_Spec_Ptr_Type,
            Wki_Instance, Prev_Subprg_Instance);
      end if;

      Start_Procedure_Decl
        (Interface_List, Create_Identifier ("ELAB_SPEC"), Global_Storage);
      Subprgs.Add_Subprg_Instance_Interfaces
        (Interface_List, Info.Package_Elab_Spec_Instance);
      Finish_Subprogram_Decl
        (Interface_List, Info.Package_Elab_Spec_Subprg);

      if Flag_Rti then
         --  Generate RTI.
         Rtis.Generate_Unit (Decl);
      end if;

      if Global_Storage = O_Storage_Public then
         --  Create elaboration procedure for the spec
         Elab_Package (Decl);
      end if;

      if Is_Uninstantiated_Package (Decl) then
         Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
      end if;
      Save_Local_Identifier (Info.Package_Local_Id);
   end Translate_Package_Declaration;

   procedure Translate_Package_Body (Decl : Iir_Package_Body)
   is
      Spec : constant Iir_Package_Declaration := Get_Package (Decl);
      Info : constant Ortho_Info_Acc := Get_Info (Spec);
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      --  Translate declarations.
      if Is_Uninstantiated_Package (Spec) then
         Push_Instance_Factory (Info.Package_Body_Scope'Access);
         Info.Package_Spec_Field := Add_Instance_Factory_Field
           (Get_Identifier ("SPEC"),
            Get_Scope_Type (Info.Package_Spec_Scope));

         Chap4.Translate_Declaration_Chain (Decl);

         Pop_Instance_Factory (Info.Package_Body_Scope'Access);

         if Global_Storage = O_Storage_External then
            return;
         end if;
      else
         --  May be called during elaboration to generate RTI.
         if Global_Storage = O_Storage_External then
            return;
         end if;

         Restore_Local_Identifier (Get_Info (Spec).Package_Local_Id);

         Chap4.Translate_Declaration_Chain (Decl);
      end if;

      if Flag_Rti then
         Rtis.Generate_Unit (Decl);
      end if;

      if Is_Uninstantiated_Package (Spec) then
         Subprgs.Push_Subprg_Instance
           (Info.Package_Body_Scope'Access, Info.Package_Body_Ptr_Type,
            Wki_Instance, Prev_Subprg_Instance);
         Set_Scope_Via_Field (Info.Package_Spec_Scope,
                              Info.Package_Spec_Field,
                              Info.Package_Body_Scope'Access);
      end if;

      Chap4.Translate_Declaration_Chain_Subprograms (Decl);

      if Is_Uninstantiated_Package (Spec) then
         Clear_Scope (Info.Package_Spec_Scope);
         Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
      end if;

      Elab_Package_Body (Spec, Decl);
   end Translate_Package_Body;

   procedure Elab_Package (Spec : Iir_Package_Declaration)
   is
      Info   : constant Ortho_Info_Acc := Get_Info (Spec);
      Final  : Boolean;
      Constr : O_Assoc_List;
      pragma Unreferenced (Final);
   begin
      Start_Subprogram_Body (Info.Package_Elab_Spec_Subprg);
      Push_Local_Factory;
      Subprgs.Start_Subprg_Instance_Use (Info.Package_Elab_Spec_Instance);

      Elab_Dependence (Get_Design_Unit (Spec));

      if not Is_Uninstantiated_Package (Spec)
        and then Get_Kind (Get_Parent (Spec)) = Iir_Kind_Design_Unit
      then
         --  Register the top level package.  This is done dynamically, as
         --  we know only during elaboration that the design depends on a
         --  package (a package maybe referenced by an entity which is never
         --  instantiated due to generate statements).
         Start_Association (Constr, Ghdl_Rti_Add_Package);
         New_Association
           (Constr,
            New_Lit (Rtis.New_Rti_Address (Info.Package_Rti_Const)));
         New_Procedure_Call (Constr);
      end if;

      Open_Temp;
      Chap4.Elab_Declaration_Chain (Spec, Final);
      Close_Temp;

      Subprgs.Finish_Subprg_Instance_Use (Info.Package_Elab_Spec_Instance);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Elab_Package;

   procedure Elab_Package_Body (Spec : Iir_Package_Declaration; Bod : Iir)
   is
      Info   : constant Ortho_Info_Acc := Get_Info (Spec);
      If_Blk : O_If_Block;
      Constr : O_Assoc_List;
      Final  : Boolean;
   begin
      Start_Subprogram_Body (Info.Package_Elab_Body_Subprg);
      Push_Local_Factory;
      Subprgs.Start_Subprg_Instance_Use (Info.Package_Elab_Body_Instance);

      if Is_Uninstantiated_Package (Spec) then
         Set_Scope_Via_Field (Info.Package_Spec_Scope,
                              Info.Package_Spec_Field,
                              Info.Package_Body_Scope'Access);
      end if;

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

      if Is_Uninstantiated_Package (Spec) then
         Clear_Scope (Info.Package_Spec_Scope);
      end if;

      Subprgs.Finish_Subprg_Instance_Use (Info.Package_Elab_Body_Instance);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
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
      El : Iir;
   begin
      case L is
         when Null_Iir_List
            | Iir_List_All
            | Iir_List_Others =>
            return;
         when others =>
            for I in Natural loop
               El := Get_Nth_Element (L, I);
               exit when El = Null_Iir;
               Instantiate_Iir_Info (El);
            end loop;
      end case;
   end Instantiate_Iir_List_Info;

   procedure Copy_Info (Dest : Ortho_Info_Acc; Src : Ortho_Info_Acc) is
   begin
      case Src.Kind is
         when Kind_Type =>
            Dest.all := (Kind => Kind_Type,
                         Type_Mode => Src.Type_Mode,
                         Type_Incomplete => Src.Type_Incomplete,
                         Type_Locally_Constrained =>
                            Src.Type_Locally_Constrained,
                         C => null,
                         Ortho_Type => Src.Ortho_Type,
                         Ortho_Ptr_Type => Src.Ortho_Ptr_Type,
                         Type_Transient_Chain => Null_Iir,
                         T => Src.T,
                         Type_Rti => Src.Type_Rti);
            pragma Assert (Src.C = null);
            pragma Assert (Src.Type_Transient_Chain = Null_Iir);
         when Kind_Object =>
            pragma Assert (Src.Object_Driver = Null_Var);
            pragma Assert (Src.Object_Function = O_Dnode_Null);
            Dest.all :=
              (Kind => Kind_Object,
               Object_Static => Src.Object_Static,
               Object_Var => Instantiate_Var (Src.Object_Var),
               Object_Driver => Null_Var,
               Object_Rti => Src.Object_Rti,
               Object_Function => O_Dnode_Null);
         when Kind_Subprg =>
            Dest.Subprg_Frame_Scope :=
              Instantiate_Var_Scope (Src.Subprg_Frame_Scope);
            Dest.all :=
              (Kind => Kind_Subprg,
               Use_Stack2 => Src.Use_Stack2,
               Ortho_Func => Src.Ortho_Func,
               Res_Interface => Src.Res_Interface,
               Res_Record_Var => Instantiate_Var (Src.Res_Record_Var),
               Res_Record_Type => Src.Res_Record_Type,
               Res_Record_Ptr => Src.Res_Record_Ptr,
               Subprg_Frame_Scope => Dest.Subprg_Frame_Scope,
               Subprg_Instance => Instantiate_Subprg_Instance
                 (Src.Subprg_Instance),
               Subprg_Resolv => null,
               Subprg_Local_Id => Src.Subprg_Local_Id,
               Subprg_Exit => Src.Subprg_Exit,
               Subprg_Result => Src.Subprg_Result);
         when Kind_Interface =>
            Dest.all := (Kind => Kind_Interface,
                         Interface_Node => Src.Interface_Node,
                         Interface_Field => Src.Interface_Field,
                         Interface_Type => Src.Interface_Type);
         when Kind_Index =>
            Dest.all := (Kind => Kind_Index,
                         Index_Field => Src.Index_Field);
         when Kind_Expr =>
            Dest.all := (Kind => Kind_Expr,
                         Expr_Node => Src.Expr_Node);
         when others =>
            raise Internal_Error;
      end case;
   end Copy_Info;

   procedure Instantiate_Iir_Info (N : Iir) is
   begin
      --  Nothing to do for null node.
      if N = Null_Iir then
         return;
      end if;

      declare
         use Nodes_Meta;
         Kind      : constant Iir_Kind := Get_Kind (N);
         Fields    : constant Fields_Array := Get_Fields (Kind);
         F         : Fields_Enum;
         Orig      : constant Iir := Sem_Inst.Get_Origin (N);
         pragma Assert (Orig /= Null_Iir);
         Orig_Info : constant Ortho_Info_Acc := Get_Info (Orig);
         Info      : Ortho_Info_Acc;
      begin
         if Orig_Info /= null then
            Info := Add_Info (N, Orig_Info.Kind);

            Copy_Info (Info, Orig_Info);

            case Info.Kind is
               when Kind_Subprg =>
                  Push_Instantiate_Var_Scope
                    (Info.Subprg_Frame_Scope'Access,
                     Orig_Info.Subprg_Frame_Scope'Access);
               when others =>
                  null;
            end case;
         end if;

         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Instantiate_Iir_Info (Get_Iir (N, F));
                     when Attr_Ref =>
                        null;
                     when Attr_Maybe_Ref =>
                        if not Get_Is_Ref (N) then
                           Instantiate_Iir_Info (Get_Iir (N, F));
                        end if;
                     when Attr_Chain =>
                        Instantiate_Iir_Chain_Info (Get_Iir (N, F));
                     when Attr_Chain_Next =>
                        null;
                     when Attr_Of_Ref =>
                        raise Internal_Error;
                  end case;
               when Type_Iir_List =>
                  case Get_Field_Attribute (F) is
                     when Attr_None =>
                        Instantiate_Iir_List_Info (Get_Iir_List (N, F));
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
                  | Type_Time_Stamp_Id =>
                  --  Can this happen ?
                  raise Internal_Error;
               when Type_String_Id
                  | Type_Source_Ptr
                  | Type_Base_Type
                  | Type_Iir_Constraint
                  | Type_Iir_Mode
                  | Type_Iir_Index32
                  | Type_Iir_Int64
                  | Type_Boolean
                  | Type_Iir_Staticness
                  | Type_Iir_All_Sensitized
                  | Type_Iir_Signal_Kind
                  | Type_Tri_State_Type
                  | Type_Iir_Pure_State
                  | Type_Iir_Delay_Mechanism
                  | Type_Iir_Predefined_Functions
                  | Type_Iir_Direction
                  | Type_Location_Type
                  | Type_Iir_Int32
                  | Type_Int32
                  | Type_Iir_Fp64
                  | Type_Token_Type
                  | Type_Name_Id =>
                  null;
            end case;
         end loop;

         if Info /= null then
            case Info.Kind is
               when Kind_Subprg =>
                  Pop_Instantiate_Var_Scope
                    (Info.Subprg_Frame_Scope'Access);
               when others =>
                  null;
            end case;
         end if;
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
         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Constant_Declaration =>
               Orig := Sem_Inst.Get_Origin (Inter);
               Orig_Info := Get_Info (Orig);

               Info := Add_Info (Inter, Orig_Info.Kind);
               Copy_Info (Info, Orig_Info);

            when Iir_Kind_Interface_Package_Declaration =>
               null;

            when others =>
               raise Internal_Error;
         end case;

         Inter := Get_Chain (Inter);
      end loop;
   end Instantiate_Iir_Generic_Chain_Info;

   --  Add info for an interface_package_declaration or a
   --  package_instantiation_declaration
   procedure Instantiate_Info_Package (Inst : Iir)
   is
      Spec     : constant Iir :=
        Get_Named_Entity (Get_Uninstantiated_Package_Name (Inst));
      Pkg_Info : constant Ortho_Info_Acc := Get_Info (Spec);
      Info     : Ortho_Info_Acc;
   begin
      Info := Add_Info (Inst, Kind_Package_Instance);

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
      Spec           : constant Iir :=
        Get_Named_Entity (Get_Uninstantiated_Package_Name (Inst));
      Pkg_Info       : constant Ortho_Info_Acc := Get_Info (Spec);
      Info           : Ortho_Info_Acc;
      Interface_List : O_Inter_List;
      Constr         : O_Assoc_List;
   begin
      Instantiate_Info_Package (Inst);
      Info := Get_Info (Inst);

      --  FIXME: if the instantiation occurs within a package declaration,
      --  the variable must be declared extern (and public in the body).
      Info.Package_Instance_Body_Var := Create_Var
        (Create_Var_Identifier (Inst),
         Get_Scope_Type (Pkg_Info.Package_Body_Scope));

      --  FIXME: this is correct only for global instantiation, and only if
      --  there is only one.
      Set_Scope_Via_Decl (Info.Package_Instance_Body_Scope,
                          Get_Var_Label (Info.Package_Instance_Body_Var));
      Set_Scope_Via_Field (Info.Package_Instance_Spec_Scope,
                           Pkg_Info.Package_Spec_Field,
                           Info.Package_Instance_Body_Scope'Access);

      --  Declare elaboration procedure
      Start_Procedure_Decl
        (Interface_List, Create_Identifier ("ELAB"), Global_Storage);
      --  Chap2.Add_Subprg_Instance_Interfaces
      --   (Interface_List, Info.Package_Instance_Elab_Instance);
      Finish_Subprogram_Decl
        (Interface_List, Info.Package_Instance_Elab_Subprg);

      if Global_Storage /= O_Storage_Public then
         return;
      end if;

      --  Elaborator:
      Start_Subprogram_Body (Info.Package_Instance_Elab_Subprg);
      --  Chap2.Start_Subprg_Instance_Use
      --    (Info.Package_Instance_Elab_Instance);

      Elab_Dependence (Get_Design_Unit (Inst));

      Set_Scope_Via_Decl (Pkg_Info.Package_Body_Scope,
                          Get_Var_Label (Info.Package_Instance_Body_Var));
      Set_Scope_Via_Field (Pkg_Info.Package_Spec_Scope,
                           Pkg_Info.Package_Spec_Field,
                           Pkg_Info.Package_Body_Scope'Access);
      Chap5.Elab_Generic_Map_Aspect (Inst);
      Clear_Scope (Pkg_Info.Package_Spec_Scope);
      Clear_Scope (Pkg_Info.Package_Body_Scope);

      --  Call the elaborator of the generic.  The generic must be
      --  temporary associated with the instance variable.
      Start_Association (Constr, Pkg_Info.Package_Elab_Body_Subprg);
      Set_Scope_Via_Decl (Pkg_Info.Package_Body_Scope,
                          Get_Var_Label (Info.Package_Instance_Body_Var));
      Add_Subprg_Instance_Assoc
        (Constr, Pkg_Info.Package_Elab_Body_Instance);
      Clear_Scope (Pkg_Info.Package_Body_Scope);
      New_Procedure_Call (Constr);

      --  Chap2.Finish_Subprg_Instance_Use
      --    (Info.Package_Instance_Elab_Instance);
      Finish_Subprogram_Body;
   end Translate_Package_Instantiation_Declaration;

   procedure Elab_Dependence_Package (Pkg : Iir_Package_Declaration)
   is
      Info   : Ortho_Info_Acc;
      If_Blk : O_If_Block;
      Constr : O_Assoc_List;
   begin
      --  Std.Standard is pre-elaborated.
      if Pkg = Standard_Package then
         return;
      end if;

      --  Nothing to do for uninstantiated package.
      if Is_Uninstantiated_Package (Pkg) then
         return;
      end if;

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

   procedure Elab_Dependence_Package_Instantiation (Pkg : Iir)
   is
      Info   : constant Ortho_Info_Acc := Get_Info (Pkg);
      Constr : O_Assoc_List;
   begin
      Start_Association (Constr, Info.Package_Instance_Elab_Subprg);
      New_Procedure_Call (Constr);
   end Elab_Dependence_Package_Instantiation;

   procedure Elab_Dependence (Design_Unit: Iir_Design_Unit)
   is
      Depend_List : Iir_Design_Unit_List;
      Design      : Iir;
      Library_Unit: Iir;
   begin
      Depend_List := Get_Dependence_List (Design_Unit);

      for I in Natural loop
         Design := Get_Nth_Element (Depend_List, I);
         exit when Design = Null_Iir;
         if Get_Kind (Design) = Iir_Kind_Design_Unit then
            Library_Unit := Get_Library_Unit (Design);
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Package_Declaration =>
                  Elab_Dependence_Package (Library_Unit);
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
               when others =>
                  Error_Kind ("elab_dependence", Library_Unit);
            end case;
         end if;
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
