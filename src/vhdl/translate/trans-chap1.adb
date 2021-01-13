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
with Translation; use Translation;
with Trans.Chap2;
with Trans.Chap3;
with Trans.Chap4;
with Trans.Chap5;
with Trans.Chap6;
with Trans.Chap7;
with Trans.Chap9;
with Trans.Rtis;
with Trans.Helpers2; use Trans.Helpers2;
with Name_Table;

package body Trans.Chap1 is
   use Trans.Helpers;

   procedure Start_Block_Decl (Blk : Iir)
   is
      Info : constant Block_Info_Acc := Get_Info (Blk);
   begin
      Chap2.Declare_Inst_Type_And_Ptr
        (Info.Block_Scope'Access, Info.Block_Decls_Ptr_Type);
   end Start_Block_Decl;

   procedure Translate_Entity_Init_Generics (Entity : Iir)
   is
      El : Iir;
   begin
      Push_Local_Factory;

      El := Get_Generic_Chain (Entity);
      while El /= Null_Iir loop
         Open_Temp;

         declare
            Val : constant Iir := Get_Default_Value (El);
            El_Type : constant Iir := Get_Type (El);
         begin
            if Val = Null_Iir
              and then Get_Kind (El_Type) in Iir_Kinds_Array_Type_Definition
              and then Get_Constraint_State (El_Type) /= Fully_Constrained
            then
               --  Do not initialize unconstrained array.  They will have
               --  to be overriden by user.
               null;
            else
               Chap4.Elab_Object_Value (El, Val);
            end if;
         end;
         Close_Temp;
         El := Get_Chain (El);
      end loop;

      Pop_Local_Factory;
   end Translate_Entity_Init_Generics;

   procedure Translate_Entity_Init_Ports (Entity : Iir)
   is
      El      : Iir;
      El_Type : Iir;
      Default : Iir;
      Value : Iir;
   begin
      Push_Local_Factory;

      Default := Null_Iir;
      El := Get_Port_Chain (Entity);
      while El /= Null_Iir loop
         Open_Temp;
         El_Type := Get_Type (El);
         if not Is_Fully_Constrained_Type (El_Type) then
            if Default = Null_Iir then
               Default := Create_Iir (Iir_Kind_Association_Element_Open);
            end if;
            Chap5.Elab_Unconstrained_Port_Bounds (El, Default);
         end if;
         Chap4.Elab_Signal_Declaration_Storage (El, False);
         Chap4.Elab_Signal_Declaration_Object (El, Entity, False);

         Value := Get_Default_Value (El);
         if Is_Valid (Value) then
            --  Set default value.
            Chap9.Destroy_Types (Value);
            Chap4.Elab_Object_Init
              (Get_Var (Get_Info (El).Signal_Val,
                        Get_Info (Get_Type (El)), Mode_Value),
               El, Value, Alloc_System);
         end if;

         Close_Temp;

         El := Get_Chain (El);
      end loop;

      Pop_Local_Factory;
   end Translate_Entity_Init_Ports;

   procedure Translate_Entity_Declaration (Entity : Iir_Entity_Declaration)
   is
      Info                 : Block_Info_Acc;
      Interface_List       : O_Inter_List;
      type Subprg_Instance_Array is
        array (Elab_Kind) of Subprgs.Subprg_Instance_Type;
      Instance             : Subprg_Instance_Array;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      Info := Add_Info (Entity, Kind_Block);
      Start_Block_Decl (Entity);
      Push_Instance_Factory (Info.Block_Scope'Access);

      --  Entity link (RTI and pointer to parent).
      Info.Block_Link_Field := Add_Instance_Factory_Field
        (Wki_Rti, Rtis.Ghdl_Entity_Link_Type);

      --  generics, ports.
      Chap4.Translate_Generic_Chain (Entity);
      Chap4.Translate_Port_Chain (Entity);

      Chap9.Translate_Block_Declarations (Entity, Entity);

      Pop_Instance_Factory (Info.Block_Scope'Access);

      Subprgs.Push_Subprg_Instance (Info.Block_Scope'Access,
                                    Info.Block_Decls_Ptr_Type,
                                    Wki_Instance,
                                    Prev_Subprg_Instance);

      --  Entity elaborator.
      for K in Elab_Kind loop
         Start_Procedure_Decl
           (Interface_List, Create_Elab_Identifier (K), Global_Storage);
         Subprgs.Add_Subprg_Instance_Interfaces (Interface_List, Instance (K));
         Finish_Subprogram_Decl (Interface_List, Info.Block_Elab_Subprg (K));
      end loop;

      --  Entity dependences elaborator.
      Start_Procedure_Decl (Interface_List, Create_Identifier ("PKG_ELAB"),
                            Global_Storage);
      Finish_Subprogram_Decl (Interface_List, Info.Block_Elab_Pkg_Subprg);

      --  Generate RTI.
      if Flag_Rti then
         Rtis.Generate_Unit (Entity);
      end if;

      if Global_Storage = O_Storage_External then
         --  Entity declaration subprograms as they can be called by the
         --  architectures.
         Chap4.Translate_Declaration_Chain_Subprograms
           (Entity, Subprg_Translate_Spec_And_Body);
      else
         --  Entity declaration and process subprograms.
         Chap9.Translate_Block_Subprograms (Entity, Entity);

         --  Package elaborator Body.
         Start_Subprogram_Body (Info.Block_Elab_Pkg_Subprg);
         Push_Local_Factory;
         New_Debug_Line_Stmt (Get_Line_Number (Entity));
         Chap2.Elab_Dependence (Get_Design_Unit (Entity));
         Pop_Local_Factory;
         Finish_Subprogram_Body;

         --  Elaborator Body.
         for K in Elab_Kind loop
            Start_Subprogram_Body (Info.Block_Elab_Subprg (K));
            Push_Local_Factory;
            Subprgs.Start_Subprg_Instance_Use (Instance (K));
            New_Debug_Line_Stmt (Get_Line_Number (Entity));

            case K is
               when Elab_Decls =>
                  Chap9.Elab_Block_Declarations (Entity, Entity);
               when Elab_Stmts =>
                  Chap9.Elab_Block_Statements (Entity, Entity);
            end case;
            Subprgs.Finish_Subprg_Instance_Use (Instance (K));
            Pop_Local_Factory;
            Finish_Subprogram_Body;
         end loop;
      end if;
      Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);
   end Translate_Entity_Declaration;

   --  Push scope for architecture ARCH via INSTANCE, and for its
   --  entity via the entity field of the instance.
   procedure Push_Architecture_Scope (Arch : Iir; Instance : O_Dnode)
   is
      Arch_Info   : constant Block_Info_Acc := Get_Info (Arch);
      Entity      : constant Iir := Get_Entity (Arch);
      Entity_Info : constant Block_Info_Acc := Get_Info (Entity);
   begin
      Set_Scope_Via_Param_Ptr (Arch_Info.Block_Scope, Instance);
      Set_Scope_Via_Field (Entity_Info.Block_Scope,
                           Arch_Info.Block_Parent_Field,
                           Arch_Info.Block_Scope'Access);
   end Push_Architecture_Scope;

   --  Pop scopes created by Push_Architecture_Scope.
   procedure Pop_Architecture_Scope (Arch : Iir)
   is
      Arch_Info   : constant Block_Info_Acc := Get_Info (Arch);
      Entity      : constant Iir := Get_Entity (Arch);
      Entity_Info : constant Block_Info_Acc := Get_Info (Entity);
   begin
      Clear_Scope (Entity_Info.Block_Scope);
      Clear_Scope (Arch_Info.Block_Scope);
   end Pop_Architecture_Scope;

   procedure Translate_Architecture_Body (Arch : Iir)
   is
      Entity               : constant Iir := Get_Entity (Arch);
      Entity_Info          : constant Block_Info_Acc := Get_Info (Entity);
      Info                 : Block_Info_Acc;
      Interface_List       : O_Inter_List;
      Constr               : O_Assoc_List;
      Instance             : O_Dnode_Elab;
      Var_Arch_Instance    : O_Dnode;
      Prev_Subprg_Instance : Subprgs.Subprg_Instance_Stack;
   begin
      if Get_Foreign_Flag (Arch) then
         Error_Msg_Sem (+Arch, "FOREIGN architectures are not yet handled");
      end if;

      Info := Add_Info (Arch, Kind_Block);
      Start_Block_Decl (Arch);
      Push_Instance_Factory (Info.Block_Scope'Access);

      --  We cannot use Add_Scope_Field here, because the entity is not a
      --  child scope of the architecture.
      Info.Block_Parent_Field := Add_Instance_Factory_Field
        (Get_Identifier ("ENTITY"),
         Get_Scope_Type (Entity_Info.Block_Scope));

      Chap9.Translate_Block_Declarations (Arch, Arch);

      Pop_Instance_Factory (Info.Block_Scope'Access);

      --  Declare the constant containing the size of the instance.
      New_Const_Decl
        (Info.Block_Instance_Size, Create_Identifier ("INSTSIZE"),
         Global_Storage, Ghdl_Index_Type);
      if Global_Storage /= O_Storage_External then
         Start_Init_Value (Info.Block_Instance_Size);
         Finish_Init_Value
           (Info.Block_Instance_Size, Get_Scope_Size (Info.Block_Scope));
      end if;

      --  Elaborator.
      for K in Elab_Kind loop
         Start_Procedure_Decl
           (Interface_List, Create_Elab_Identifier (K), Global_Storage);
         New_Interface_Decl
           (Interface_List, Instance (K), Wki_Instance,
            Entity_Info.Block_Decls_Ptr_Type);
         Finish_Subprogram_Decl (Interface_List, Info.Block_Elab_Subprg (K));
      end loop;

      --  Generate RTI.
      if Flag_Rti then
         Rtis.Generate_Unit (Arch);
      end if;

      --  Default configuration
      declare
         Default_Config : constant Iir :=
           Get_Default_Configuration_Declaration (Arch);
         Config_Mark : Id_Mark_Type;
      begin
         --  In case of direct and recursive instantiation, the default
         --  configuration may be needed while translating the architecture.
         --  So translate the declarations of the default configuration before
         --  translating the architecture.
         if Default_Config /= Null_Iir
           and then Get_Configuration_Done_Flag (Default_Config)
         then
            Push_Identifier_Prefix
              (Config_Mark, Name_Table.Get_Identifier ("DEFAULT_CONFIG"));
            Translate_Configuration_Declaration_Decl
              (Get_Library_Unit
                 (Get_Default_Configuration_Declaration (Arch)));
            Pop_Identifier_Prefix (Config_Mark);
         end if;
      end;

      if Global_Storage = O_Storage_External then
         return;
      end if;

      --  Create process subprograms.
      Subprgs.Push_Subprg_Instance (Info.Block_Scope'Access,
                                    Info.Block_Decls_Ptr_Type,
                                    Wki_Instance,
                                    Prev_Subprg_Instance);
      Set_Scope_Via_Field (Entity_Info.Block_Scope,
                           Info.Block_Parent_Field,
                           Info.Block_Scope'Access);

      Chap9.Translate_Block_Subprograms (Arch, Arch);

      Clear_Scope (Entity_Info.Block_Scope);
      Subprgs.Pop_Subprg_Instance (Wki_Instance, Prev_Subprg_Instance);

      --  Elaborator body.
      for K in Elab_Kind loop
         Start_Subprogram_Body (Info.Block_Elab_Subprg (K));
         Push_Local_Factory;

         --  Create a variable for the architecture instance (with the right
         --  type, instead of the entity instance type).
         New_Var_Decl (Var_Arch_Instance, Wki_Arch_Instance,
                       O_Storage_Local, Info.Block_Decls_Ptr_Type);
         New_Assign_Stmt
           (New_Obj (Var_Arch_Instance),
            New_Convert_Ov (New_Value (New_Obj (Instance (K))),
                            Info.Block_Decls_Ptr_Type));

         case K is
            when Elab_Decls =>
               --  Set RTI.
               if Flag_Rti then
                  New_Assign_Stmt
                    (New_Selected_Element
                       (New_Selected_Acc_Value (New_Obj (Instance (K)),
                                                Entity_Info.Block_Link_Field),
                        Rtis.Ghdl_Entity_Link_Rti),
                     New_Unchecked_Address (New_Obj (Info.Block_Rti_Const),
                                            Rtis.Ghdl_Rti_Access));
               end if;
            when Elab_Stmts =>
               null;
         end case;

         --  Call entity elaborators.
         Start_Association (Constr, Entity_Info.Block_Elab_Subprg (K));
         New_Association (Constr, New_Value (New_Obj (Instance (K))));
         New_Procedure_Call (Constr);

         Push_Architecture_Scope (Arch, Var_Arch_Instance);

         New_Debug_Line_Stmt (Get_Line_Number (Arch));

         case K is
            when Elab_Decls =>
               Chap2.Elab_Dependence (Get_Design_Unit (Arch));
               Chap9.Elab_Block_Declarations (Arch, Arch);
            when Elab_Stmts =>
               Chap9.Elab_Block_Statements (Arch, Arch);
         end case;

         Pop_Architecture_Scope (Arch);

         Pop_Local_Factory;
         Finish_Subprogram_Body;
      end loop;
   end Translate_Architecture_Body;

   procedure Translate_Component_Configuration_Decl
     (Cfg : Iir; Blk : Iir; Base_Block : Iir; Num : in out Iir_Int32)
   is
      Inter_List  : O_Inter_List;
      Comp        : Iir_Component_Declaration;
      Comp_Info   : Comp_Info_Acc;
      Info        : Config_Info_Acc;
      Instance    : O_Dnode;
      Mark, Mark2 : Id_Mark_Type;

      Base_Info     : Block_Info_Acc;
      Base_Instance : O_Dnode;

      Block         : Iir_Block_Configuration;
      Binding       : Iir_Binding_Indication;
      Entity_Aspect : Iir;
      Conf_Override : Iir;
      Conf_Info     : Config_Info_Acc;
   begin
      --  Incremental binding.
      if Get_Nbr_Elements (Get_Instantiation_List (Cfg)) = 0 then
         --  This component configuration applies to no component
         --  instantiation, so it is not translated.
         return;
      end if;

      Binding := Get_Binding_Indication (Cfg);
      if Binding = Null_Iir then
         --  This is an unbound component configuration, since this is a
         --  no-op, it is not translated.
         return;
      end if;

      Entity_Aspect := Get_Entity_Aspect (Binding);
      if Get_Kind (Entity_Aspect) = Iir_Kind_Entity_Aspect_Open then
         --  Unbound component.
         return;
      end if;

      Comp := Get_Named_Entity (Get_Component_Name (Cfg));
      Comp_Info := Get_Info (Comp);

      if Get_Kind (Cfg) = Iir_Kind_Component_Configuration then
         Block := Get_Block_Configuration (Cfg);
      else
         Block := Null_Iir;
      end if;

      Push_Identifier_Prefix (Mark, Get_Identifier (Comp), Num);
      Num := Num + 1;

      if Block /= Null_Iir then
         Push_Identifier_Prefix (Mark2, "CONFIG");
         Translate_Configuration_Declaration_Decl (Cfg);
         Translate_Configuration_Declaration_Body (Cfg);
         Pop_Identifier_Prefix (Mark2);
         Conf_Override := Cfg;
         Conf_Info := Get_Info (Cfg);
         Clear_Info (Cfg);
      else
         Conf_Info := null;
         Conf_Override := Null_Iir;
      end if;
      Info := Add_Info (Cfg, Kind_Config);

      Base_Info := Get_Info (Base_Block);

      Chap4.Translate_Association_Subprograms
        (Binding, Blk, Base_Block,
         Get_Entity_From_Entity_Aspect (Entity_Aspect));

      Start_Procedure_Decl
        (Inter_List, Create_Identifier, O_Storage_Private);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                          Comp_Info.Comp_Ptr_Type);
      New_Interface_Decl (Inter_List, Base_Instance, Get_Identifier ("BLK"),
                          Base_Info.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Info.Config_Subprg);

      --  Extract the entity/architecture.

      Start_Subprogram_Body (Info.Config_Subprg);
      Push_Local_Factory;

      if Get_Kind (Base_Block) = Iir_Kind_Architecture_Body then
         Push_Architecture_Scope (Base_Block, Base_Instance);
      else
         Set_Scope_Via_Param_Ptr (Base_Info.Block_Scope, Base_Instance);
      end if;

      Set_Scope_Via_Param_Ptr (Comp_Info.Comp_Scope, Instance);

      if Conf_Info /= null then
         Clear_Info (Cfg);
         Set_Info (Cfg, Conf_Info);
      end if;
      Chap9.Translate_Entity_Instantiation
        (Entity_Aspect, Binding, Comp, Conf_Override);
      if Conf_Info /= null then
         Clear_Info (Cfg);
         Set_Info (Cfg, Info);
      end if;

      Clear_Scope (Comp_Info.Comp_Scope);

      if Get_Kind (Base_Block) = Iir_Kind_Architecture_Body then
         Pop_Architecture_Scope (Base_Block);
      else
         Clear_Scope (Base_Info.Block_Scope);
      end if;

      Pop_Local_Factory;
      Finish_Subprogram_Body;

      Pop_Identifier_Prefix (Mark);
   end Translate_Component_Configuration_Decl;

   --  Create subprogram specifications for each configuration_specification
   --  in BLOCK_CONFIG and its sub-blocks.
   --  BLOCK is the block being configured (initially the architecture),
   --  BASE_BLOCK is the root block giving the instance (initially the
   --  architecture)
   --  NUM is an integer used to generate uniq names.
   procedure Translate_Block_Configuration_Decls
     (Block_Config : Iir_Block_Configuration;
      Block        : Iir;
      Base_Block   : Iir;
      Num          : in out Iir_Int32)
   is
      El : Iir;
   begin
      El := Get_Configuration_Item_Chain (Block_Config);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Component_Configuration
               | Iir_Kind_Configuration_Specification =>
               Translate_Component_Configuration_Decl
                 (El, Block, Base_Block, Num);
            when Iir_Kind_Block_Configuration =>
               declare
                  Mark      : Id_Mark_Type;
                  Base_Info : constant Block_Info_Acc :=
                    Get_Info (Base_Block);
                  Blk      : constant Iir := Get_Block_From_Block_Specification
                    (Get_Block_Specification (El));
                  Blk_Info  : constant Block_Info_Acc := Get_Info (Blk);
               begin
                  Push_Identifier_Prefix (Mark, Get_Identifier (Blk));
                  case Get_Kind (Blk) is
                     when Iir_Kind_Generate_Statement_Body =>
                        Set_Scope_Via_Field_Ptr
                          (Base_Info.Block_Scope,
                           Blk_Info.Block_Origin_Field,
                           Blk_Info.Block_Scope'Access);
                        Translate_Block_Configuration_Decls
                          (El, Blk, Blk, Num);
                        Clear_Scope (Base_Info.Block_Scope);
                     when Iir_Kind_Block_Statement =>
                        Translate_Block_Configuration_Decls
                          (El, Blk, Base_Block, Num);
                     when others =>
                        Error_Kind
                          ("translate_block_configuration_decls(2)", Blk);
                  end case;
                  Pop_Identifier_Prefix (Mark);
               end;
            when others =>
               Error_Kind ("translate_block_configuration_decls(1)", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Block_Configuration_Decls;

   procedure Translate_Component_Configuration_Call
     (Cfg : Iir; Base_Block : Iir; Block_Info : Block_Info_Acc)
   is
      Binding : constant Iir := Get_Binding_Indication (Cfg);
      Aspect : Iir;
      Cfg_Info  : Config_Info_Acc;
      Base_Info : Block_Info_Acc;
   begin
      if Is_Null (Binding) then
         --  Unbound component configuration, nothing to do.
         return;
      end if;
      Aspect := Get_Entity_Aspect (Binding);
      if Is_Null (Aspect)
        or else Get_Kind (Aspect) = Iir_Kind_Entity_Aspect_Open
      then
         return;
      end if;

      Cfg_Info := Get_Info (Cfg);
      Base_Info := Get_Info (Base_Block);

      --  Call the subprogram for the instantiation list.
      declare
         List : Iir_Flist;
         El   : Iir;
      begin
         List := Get_Instantiation_List (Cfg);
         for I in Flist_First .. Flist_Last (List) loop
            El := Get_Nth_Element (List, I);
            El := Get_Named_Entity (El);
            case Get_Kind (El) is
               when Iir_Kind_Component_Instantiation_Statement =>
                  declare
                     Assoc     : O_Assoc_List;
                     Info      : constant Block_Info_Acc := Get_Info (El);
                     Comp_Info : constant Comp_Info_Acc :=
                       Get_Info (Get_Named_Entity
                                 (Get_Instantiated_Unit (El)));
                     V         : O_Lnode;
                  begin
                     --  The component is really a component and not a
                     --  direct instance.
                     Start_Association (Assoc, Cfg_Info.Config_Subprg);
                     V := Get_Instance_Ref (Block_Info.Block_Scope);
                     V := New_Selected_Element (V, Info.Block_Link_Field);
                     New_Association
                       (Assoc, New_Address (V, Comp_Info.Comp_Ptr_Type));
                     V := Get_Instance_Ref (Base_Info.Block_Scope);
                     New_Association
                       (Assoc,
                        New_Address (V, Base_Info.Block_Decls_Ptr_Type));
                     New_Procedure_Call (Assoc);
                  end;
               when others =>
                  Error_Kind ("translate_component_configuration", El);
            end case;
         end loop;
      end;
   end Translate_Component_Configuration_Call;

   procedure Translate_Block_Configuration_Calls
     (Block_Config : Iir_Block_Configuration;
      Base_Block   : Iir;
      Base_Info    : Block_Info_Acc);

   procedure Translate_For_Generate_Block_Configuration_Calls
     (Block_Config : Iir_Block_Configuration;
      Parent_Info  : Block_Info_Acc)
   is
      Spec   : constant Iir := Get_Block_Specification (Block_Config);
      Bod    : constant Iir := Get_Block_From_Block_Specification (Spec);
      Block  : constant Iir := Get_Parent (Bod);
      Info   : constant Block_Info_Acc := Get_Info (Bod);

      Iter : constant Iir := Get_Parameter_Specification (Block);
      Iter_Type : constant Iir := Get_Type (Iter);
      Type_Info : constant Type_Info_Acc :=
        Get_Info (Get_Base_Type (Iter_Type));

      --  Generate a call for a iterative generate block whose index is
      --  INDEX.
      --  FAILS is true if it is an error if the block is already
      --  configured.
      procedure Gen_Subblock_Call (Index : O_Enode; Fails : Boolean)
      is
         Var_Inst : O_Dnode;
         If_Blk   : O_If_Block;
      begin
         Open_Temp;
         Var_Inst := Create_Temp (Info.Block_Decls_Ptr_Type);
         New_Assign_Stmt
           (New_Obj (Var_Inst),
            New_Address (New_Indexed_Element
              (New_Acc_Value
                   (New_Selected_Element
                      (Get_Instance_Ref (Parent_Info.Block_Scope),
                         Info.Block_Parent_Field)),
                   Index),
              Info.Block_Decls_Ptr_Type));
         --  Configure only if not yet configured.
         Start_If_Stmt
           (If_Blk,
            New_Compare_Op (ON_Eq,
              New_Value_Selected_Acc_Value
                (New_Obj (Var_Inst),
                 Info.Block_Configured_Field),
              New_Lit (Ghdl_Bool_False_Node),
              Ghdl_Bool_Type));
         --  Mark the block as configured.
         New_Assign_Stmt
           (New_Selected_Acc_Value (New_Obj (Var_Inst),
            Info.Block_Configured_Field),
            New_Lit (Ghdl_Bool_True_Node));
         Set_Scope_Via_Param_Ptr (Info.Block_Scope, Var_Inst);
         Translate_Block_Configuration_Calls (Block_Config, Bod, Info);
         Clear_Scope (Info.Block_Scope);

         if Fails then
            New_Else_Stmt (If_Blk);
            --  Already configured.
            Chap6.Gen_Program_Error
              (Block_Config, Chap6.Prg_Err_Block_Configured);
         end if;

         Finish_If_Stmt (If_Blk);
         Close_Temp;
      end Gen_Subblock_Call;

      procedure Apply_To_All_Others_Blocks (Is_All : Boolean)
      is
         Var_I : O_Dnode;
         Label : O_Snode;
      begin
         Start_Declare_Stmt;
         New_Var_Decl (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
         Init_Var (Var_I);
         Start_Loop_Stmt (Label);
         Gen_Exit_When
           (Label,
            New_Compare_Op
              (ON_Eq,
               New_Value (New_Obj (Var_I)),
               New_Value
                 (New_Selected_Element
                      (Get_Var (Get_Info (Iter_Type).S.Range_Var),
                       Type_Info.B.Range_Length)),
               Ghdl_Bool_Type));
         --  Selected_name is for default configurations, so
         --  program should not fail if a block is already
         --  configured but continue silently.
         Gen_Subblock_Call (New_Value (New_Obj (Var_I)), Is_All);
         Inc_Var (Var_I);
         Finish_Loop_Stmt (Label);
         Finish_Declare_Stmt;
      end Apply_To_All_Others_Blocks;
   begin
      case Get_Kind (Spec) is
         when Iir_Kind_For_Generate_Statement
           | Iir_Kind_Simple_Name =>
            Apply_To_All_Others_Blocks (True);
         when Iir_Kind_Indexed_Name =>
            declare
               Index_List : constant Iir_Flist := Get_Index_List (Spec);
               Rng        : Mnode;
            begin
               if Index_List = Iir_Flist_Others then
                  Apply_To_All_Others_Blocks (False);
               else
                  Open_Temp;
                  Rng := Stabilize (Chap3.Type_To_Range (Iter_Type));
                  Gen_Subblock_Call
                    (Chap6.Translate_Index_To_Offset
                       (Rng,
                        Chap7.Translate_Expression
                          (Get_Nth_Element (Index_List, 0), Iter_Type),
                        Iter, Iter_Type, Spec),
                     True);
                  Close_Temp;
               end if;
            end;
         when Iir_Kind_Slice_Name =>
            declare
               Rng         : Mnode;
               Slice       : O_Dnode;
               Left, Right : O_Dnode;
               Index       : O_Dnode;
               High        : O_Dnode;
               If_Blk      : O_If_Block;
               Label       : O_Snode;
               Rng_Idx     : Mnode;
            begin
               Open_Temp;
               Rng := Stabilize (Chap3.Type_To_Range (Iter_Type));
               Slice := Create_Temp (Type_Info.B.Range_Type);
               Chap7.Translate_Discrete_Range
                 (Dv2M (Slice, Type_Info, Mode_Value,
                        Type_Info.B.Range_Type, Type_Info.B.Range_Ptr_Type),
                  Get_Suffix (Spec));
               Rng_Idx := Lv2M (New_Selected_Element
                                (New_Obj (Slice), Type_Info.B.Range_Left),
                                Type_Info, Mode_Value);
               Left := Create_Temp_Init
                 (Ghdl_Index_Type,
                  Chap6.Translate_Index_To_Offset
                    (Rng, Rng_Idx, Spec, Iter_Type, Spec));
               Rng_Idx := Lv2M (New_Selected_Element
                                (New_Obj (Slice), Type_Info.B.Range_Right),
                                Type_Info, Mode_Value);
               Right := Create_Temp_Init
                 (Ghdl_Index_Type,
                  Chap6.Translate_Index_To_Offset
                    (Rng, Rng_Idx, Spec, Iter_Type, Spec));
               Index := Create_Temp (Ghdl_Index_Type);
               High := Create_Temp (Ghdl_Index_Type);
               Start_If_Stmt
                 (If_Blk,
                  New_Compare_Op (ON_Eq,
                                  M2E (Chap3.Range_To_Dir (Rng)),
                                  New_Value
                                    (New_Selected_Element
                                       (New_Obj (Slice),
                                        Type_Info.B.Range_Dir)),
                                  Ghdl_Bool_Type));
               --  Same direction, so left to right.
               New_Assign_Stmt (New_Obj (Index), New_Obj_Value (Left));
               New_Assign_Stmt (New_Obj (High), New_Obj_Value (Right));
               New_Else_Stmt (If_Blk);
               --  Opposite direction, so right to left.
               New_Assign_Stmt (New_Obj (Index), New_Obj_Value (Right));
               New_Assign_Stmt (New_Obj (High), New_Obj_Value (Left));
               Finish_If_Stmt (If_Blk);

               --  Loop.
               Start_Loop_Stmt (Label);
               Gen_Exit_When
                 (Label, New_Compare_Op (ON_Gt,
                                         New_Obj_Value (Index),
                                         New_Obj_Value (High),
                                         Ghdl_Bool_Type));
               Open_Temp;
               Gen_Subblock_Call (New_Obj_Value (Index), True);
               Close_Temp;
               Inc_Var (Index);
               Finish_Loop_Stmt (Label);
               Close_Temp;
            end;
         when others =>
            Error_Kind
              ("translate_for_generate_block_configuration_calls", Spec);
      end case;
   end Translate_For_Generate_Block_Configuration_Calls;

   procedure Translate_If_Case_Generate_Block_Configuration_Calls
     (Block_Config : Iir_Block_Configuration;
      Parent_Info  : Block_Info_Acc)
   is
      Spec   : constant Iir := Get_Block_Specification (Block_Config);
      Bod    : constant Iir := Get_Block_From_Block_Specification (Spec);
      Gen    : constant Iir := Get_Parent (Bod);
      Gen_Info : constant Generate_Info_Acc := Get_Info (Gen);
      Bod_Info : constant Block_Info_Acc := Get_Info (Bod);
      Var    : O_Dnode;
      If_Blk : O_If_Block;

   begin
      --  Configure the block only if block id matches.
      Start_If_Stmt
        (If_Blk,
         New_Compare_Op
           (ON_Eq,
            New_Value (New_Selected_Element
                         (Get_Instance_Ref (Parent_Info.Block_Scope),
                          Gen_Info.Generate_Body_Id)),
            New_Lit (New_Index_Lit (Unsigned_64 (Bod_Info.Block_Id))),
            Ghdl_Bool_Type));

      Open_Temp;
      Var := Create_Temp_Init
        (Bod_Info.Block_Decls_Ptr_Type,
         New_Convert_Ov
           (New_Value (New_Selected_Element
                         (Get_Instance_Ref (Parent_Info.Block_Scope),
                          Gen_Info.Generate_Parent_Field)),
            Bod_Info.Block_Decls_Ptr_Type));
      Set_Scope_Via_Param_Ptr (Bod_Info.Block_Scope, Var);
      Translate_Block_Configuration_Calls (Block_Config, Bod, Bod_Info);
      Clear_Scope (Bod_Info.Block_Scope);
      Close_Temp;

      Finish_If_Stmt (If_Blk);
   end Translate_If_Case_Generate_Block_Configuration_Calls;

   procedure Translate_Block_Configuration_Calls
     (Block_Config : Iir_Block_Configuration;
      Base_Block   : Iir;
      Base_Info    : Block_Info_Acc)
   is
      procedure Translate_Block_Block_Configuration_Calls (Item : Iir)
      is
         Block : Iir;
      begin
         Block := Get_Block_Specification (Item);
         case Get_Kind (Block) is
            when Iir_Kind_Indexed_Name
              | Iir_Kind_Slice_Name =>
               Block := Get_Named_Entity (Get_Prefix (Block));
            when Iir_Kinds_Denoting_Name
              | Iir_Kind_Parenthesis_Name =>
               Block := Get_Named_Entity (Block);
            when others =>
               null;
         end case;

         case Get_Kind (Block) is
            when Iir_Kind_Block_Statement =>
               Translate_Block_Configuration_Calls
                 (Item, Base_Block, Get_Info (Block));
            when Iir_Kind_Generate_Statement_Body =>
               case Get_Kind (Get_Parent (Block)) is
                  when Iir_Kind_If_Generate_Statement
                    | Iir_Kind_If_Generate_Else_Clause
                    | Iir_Kind_Case_Generate_Statement => --  FIXME
                     Translate_If_Case_Generate_Block_Configuration_Calls
                       (Item, Base_Info);
                  when Iir_Kind_For_Generate_Statement =>
                     Translate_For_Generate_Block_Configuration_Calls
                       (Item, Base_Info);
                  when others =>
                     Error_Kind ("translate_block_configuration_calls(3)",
                                 Get_Parent (Block));
               end case;
            when others =>
               Error_Kind ("translate_block_configuration_calls(4)", Block);
         end case;
      end Translate_Block_Block_Configuration_Calls;

      El : Iir;
   begin
      El := Get_Configuration_Item_Chain (Block_Config);
      while El /= Null_Iir loop
         case Get_Kind (El) is
            when Iir_Kind_Component_Configuration
               | Iir_Kind_Configuration_Specification =>
               Translate_Component_Configuration_Call
                 (El, Base_Block, Base_Info);
            when Iir_Kind_Block_Configuration =>
               Translate_Block_Block_Configuration_Calls (El);
            when others =>
               Error_Kind ("translate_block_configuration_calls(2)", El);
         end case;
         El := Get_Chain (El);
      end loop;
   end Translate_Block_Configuration_Calls;

   procedure Translate_Configuration_Declaration_Decl (Config : Iir)
   is
      Block_Config   : constant Iir_Block_Configuration :=
        Get_Block_Configuration (Config);
      Arch           : constant Iir_Architecture_Body :=
        Strip_Denoting_Name (Get_Block_Specification (Block_Config));
      Arch_Info      : constant Block_Info_Acc := Get_Info (Arch);
      Interface_List : O_Inter_List;
      Config_Info    : Config_Info_Acc;
   begin
      Config_Info := Add_Info (Config, Kind_Config);

      --  Configurator.
      Start_Procedure_Decl
        (Interface_List, Create_Identifier, Global_Storage);
      New_Interface_Decl (Interface_List, Config_Info.Config_Instance,
                          Wki_Instance, Arch_Info.Block_Decls_Ptr_Type);
      Finish_Subprogram_Decl (Interface_List, Config_Info.Config_Subprg);
   end Translate_Configuration_Declaration_Decl;

   procedure Translate_Configuration_Declaration_Body (Config : Iir)
   is
      Block_Config   : constant Iir_Block_Configuration :=
        Get_Block_Configuration (Config);
      Arch           : constant Iir_Architecture_Body :=
        Strip_Denoting_Name (Get_Block_Specification (Block_Config));
      Arch_Info      : constant Block_Info_Acc := Get_Info (Arch);
      Config_Info    : constant Config_Info_Acc := Get_Info (Config);
      Num            : Iir_Int32;
      Final          : Boolean;
   begin
      if Global_Storage = O_Storage_External then
         return;
      end if;

      if Get_Kind (Config) = Iir_Kind_Configuration_Declaration then
         Chap4.Translate_Declaration_Chain (Config);
      end if;

      --  Declare subprograms for configuration.
      Num := 0;
      Translate_Block_Configuration_Decls (Block_Config, Arch, Arch, Num);

      --  Body.
      Start_Subprogram_Body (Config_Info.Config_Subprg);
      Push_Local_Factory;

      Push_Architecture_Scope (Arch, Config_Info.Config_Instance);

      if Get_Kind (Config) = Iir_Kind_Configuration_Declaration then
         --  The configuration may depend on packages.  Be sure they are
         --  elaborated.
         Chap2.Elab_Dependence (Get_Design_Unit (Config));

         Open_Temp;
         Chap4.Elab_Declaration_Chain (Config, Final);
         Close_Temp;
         if Final then
            raise Internal_Error;
         end if;
      end if;

      Translate_Block_Configuration_Calls (Block_Config, Arch, Arch_Info);

      Pop_Architecture_Scope (Arch);
      Pop_Local_Factory;
      Finish_Subprogram_Body;
   end Translate_Configuration_Declaration_Body;
end Trans.Chap1;
