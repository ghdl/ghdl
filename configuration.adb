--  Configuration generation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Libraries;
with Errorout; use Errorout;
with Std_Package;
with Sem_Names;
with Name_Table; use Name_Table;
with Flags;

package body Configuration is
   procedure Add_Design_Concurrent_Stmts (Parent : Iir);
   procedure Add_Design_Block_Configuration (Blk : Iir_Block_Configuration);
   procedure Add_Design_Aspect (Aspect : Iir; Add_Default : Boolean);

   Current_File_Dependence : Iir_List := Null_Iir_List;
   Current_Configuration : Iir_Configuration_Declaration := Null_Iir;

   --  UNIT is a design unit of a configuration declaration.
   --  Fill the DESIGN_UNITS table with all design units required to build
   --  UNIT.
   procedure Add_Design_Unit (Unit : Iir_Design_Unit; From : Iir)
   is
      List : Iir_List;
      El : Iir;
      Lib_Unit : Iir;
      File : Iir_Design_File;
      Prev_File_Dependence : Iir_List;
   begin
      if Flag_Build_File_Dependence then
         File := Get_Design_File (Unit);
         if Current_File_Dependence /= Null_Iir_List then
            Add_Element (Current_File_Dependence, File);
         end if;
      end if;

      --  If already in the table, then nothing to do.
      if Get_Elab_Flag (Unit) then
         return;
      end if;

      --  May be enabled to debug dependency construction.
      if False then
         if From = Null_Iir then
            Warning_Msg_Elab (Disp_Node (Unit) & " added", Unit);
         else
            Warning_Msg_Elab
              (Disp_Node (Unit) & " added by " & Disp_Node (From), From);
         end if;
      end if;

      Set_Elab_Flag (Unit, True);

      Lib_Unit := Get_Library_Unit (Unit);

      if Flag_Build_File_Dependence then
         Prev_File_Dependence := Current_File_Dependence;

         if Get_Kind (Lib_Unit) = Iir_Kind_Configuration_Declaration
           and then Get_Identifier (Lib_Unit) = Null_Identifier
         then
            --  Do not add dependence for default configuration.
            Current_File_Dependence := Null_Iir_List;
         else
            File := Get_Design_File (Unit);
            Current_File_Dependence := Get_File_Dependence_List (File);
            --  Create a list if not yet created.
            if Current_File_Dependence = Null_Iir_List then
               Current_File_Dependence := Create_Iir_List;
               Set_File_Dependence_List (File, Current_File_Dependence);
            end if;
         end if;
      end if;

      if Flag_Load_All_Design_Units then
         Libraries.Load_Design_Unit (Unit, From);
      end if;

      --  Add packages from depend list.
      --  If Flag_Build_File_Dependences is set, add design units of the
      --  dependence list are added, because of LRM 11.4 Analysis Order.
      --  Note: a design unit may be referenced but unused.
      --  (eg: component specification which does not apply).
      List := Get_Dependence_List (Unit);
      for I in Natural loop
         El := Get_Nth_Element (List, I);
         exit when El = Null_Iir;
         El := Libraries.Find_Design_Unit (El);
         if El /= Null_Iir then
            Lib_Unit := Get_Library_Unit (El);
            if Flag_Build_File_Dependence
              or else Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration
            then
               Add_Design_Unit (El, Unit);
            end if;
         end if;
      end loop;

      --  Lib_Unit may have changed.
      Lib_Unit := Get_Library_Unit (Unit);

      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Package_Declaration =>
            --  Analyze the package declaration, so that Set_Package below
            --  will set the full package (and not a stub).
            Libraries.Load_Design_Unit (Unit, From);
            Lib_Unit := Get_Library_Unit (Unit);
         when Iir_Kind_Configuration_Declaration =>
            --  Add entity and architecture.
            --  find all sub-configuration
            Libraries.Load_Design_Unit (Unit, From);
            Lib_Unit := Get_Library_Unit (Unit);
            Add_Design_Unit (Get_Entity (Lib_Unit), Unit);
            declare
               Blk : Iir_Block_Configuration;
               Prev_Configuration : Iir_Configuration_Declaration;
               Arch : Iir;
            begin
               Prev_Configuration := Current_Configuration;
               Current_Configuration := Lib_Unit;
               Blk := Get_Block_Configuration (Lib_Unit);
               Arch := Get_Block_Specification (Blk);
               Add_Design_Block_Configuration (Blk);
               Current_Configuration := Prev_Configuration;
               Add_Design_Unit (Get_Design_Unit (Arch), Unit);
            end;
         when Iir_Kind_Architecture_Declaration =>
            --  Add entity
            --  find all entity/architecture/configuration instantiation
            Add_Design_Unit (Get_Design_Unit (Get_Entity (Lib_Unit)), Unit);
            Add_Design_Concurrent_Stmts (Lib_Unit);
         when Iir_Kind_Entity_Declaration =>
            null;
         when Iir_Kind_Package_Body =>
            null;
         when others =>
            Error_Kind ("add_design_unit", Lib_Unit);
      end case;

      --  Add it in the table, after the dependencies.
      Design_Units.Append (Unit);

      --  Restore now the file dependence.
      --  Indeed, we may add a package body when we are in a package
      --  declaration.  However, the later does not depend on the former.
      --  The file which depends on the package declaration also depends on
      --  the package body.
      if Flag_Build_File_Dependence then
         Current_File_Dependence := Prev_File_Dependence;
      end if;

      if Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration then
         --  Add body (if any).
         declare
            Bod : Iir_Design_Unit;
         begin
            Bod := Libraries.Find_Secondary_Unit (Unit, Null_Identifier);
            if Get_Need_Body (Lib_Unit) then
               if not Flags.Flag_Elaborate_With_Outdated then
                  --  LIB_UNIT requires a body.
                  if Bod = Null_Iir then
                     Error_Msg_Elab ("body of " & Disp_Node (Lib_Unit)
                                     & " was never analyzed");
                  elsif Get_Date (Bod) < Get_Date (Unit) then
                     Error_Msg_Elab (Disp_Node (Bod) & " is outdated");
                     Bod := Null_Iir;
                  end if;
               end if;
            else
               if Bod /= Null_Iir
                 and then Get_Date (Bod) < Get_Date (Unit)
               then
                  --  There is a body for LIB_UNIT (which doesn't
                  --  require it) but it is outdated.
                  Bod := Null_Iir;
               end if;
            end if;
            if Bod /= Null_Iir then
               Set_Package (Get_Library_Unit (Bod), Lib_Unit);
               Add_Design_Unit (Bod, Unit);
            end if;
         end;
      end if;
   end Add_Design_Unit;

   procedure Add_Design_Concurrent_Stmts (Parent : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Get_Concurrent_Statement_Chain (Parent);
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Component_Instantiation_Statement =>
               declare
                  Unit : Iir;
               begin
                  Unit := Get_Instantiated_Unit (Stmt);
                  if Get_Kind (Unit) /= Iir_Kind_Component_Declaration then
                     Add_Design_Aspect (Unit, True);
                  end if;
               end;
            when Iir_Kind_Generate_Statement
              | Iir_Kind_Block_Statement =>
               Add_Design_Concurrent_Stmts (Stmt);
            when Iir_Kind_Process_Statement
              | Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Psl_Assert_Statement
              | Iir_Kind_Psl_Default_Clock
              | Iir_Kind_Psl_Declaration =>
               null;
            when others =>
               Error_Kind ("add_design_concurrent_stmts(2)", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;
   end Add_Design_Concurrent_Stmts;

   procedure Add_Design_Aspect (Aspect : Iir; Add_Default : Boolean)
   is
      use Libraries;

      Entity : Iir;
      Arch : Iir;
      Config : Iir;
      Id : Name_Id;
      Entity_Lib : Iir;
   begin
      if Aspect = Null_Iir then
         return;
      end if;
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            --  Add the entity.
            Entity := Get_Entity (Aspect);
            Add_Design_Unit (Entity, Aspect);

            --  Extract and add the architecture.
            Arch := Get_Architecture (Aspect);
            Entity_Lib := Get_Library_Unit (Entity);
            if Arch /= Null_Iir then
               case Get_Kind (Arch) is
                  when Iir_Kind_Simple_Name =>
                     Id := Get_Identifier (Arch);
                     Arch := Load_Secondary_Unit (Entity, Id, Aspect);
                     if Arch = Null_Iir then
                        Error_Msg_Elab
                          ("cannot find architecture " & Name_Table.Image (Id)
                           & " of " & Disp_Node (Entity_Lib));
                        return;
                     else
                        Set_Architecture (Aspect, Get_Library_Unit (Arch));
                     end if;
                  when Iir_Kind_Architecture_Declaration =>
                     Arch := Get_Design_Unit (Arch);
                  when others =>
                     Error_Kind ("add_design_aspect", Arch);
               end case;
            else
               Arch := Get_Latest_Architecture (Entity_Lib);
               if Arch = Null_Iir then
                  Error_Msg_Elab ("no architecture in library for "
                                  & Disp_Node (Entity_Lib), Aspect);
                  return;
               end if;
               Arch := Get_Design_Unit (Arch);
            end if;
            Load_Design_Unit (Arch, Aspect);
            Add_Design_Unit (Arch, Aspect);

            --  Add the default configuration if required.
            if Add_Default then
               Config := Get_Default_Configuration_Declaration
                 (Get_Library_Unit (Arch));
               if Config /= Null_Iir then
                  Add_Design_Unit (Config, Aspect);
               end if;
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            Add_Design_Unit (Get_Configuration (Aspect), Aspect);
         when Iir_Kind_Entity_Aspect_Open =>
            null;
         when others =>
            Error_Kind ("add_design_aspect", Aspect);
      end case;
   end Add_Design_Aspect;

   --  Return TRUE is PORT must not be open, and emit an error message only if
   --  LOC is not NULL_IIR.
   function Check_Open_Port (Port : Iir; Loc : Iir) return Boolean is
   begin
      case Get_Mode (Port) is
         when Iir_In_Mode =>
            --  LRM 1.1.1.2 Ports
            --  A port of mode IN may be unconnected or unassociated only if
            --  its declaration includes a default expression.
            if Get_Default_Value (Port) = Null_Iir then
               if Loc /= Null_Iir then
                  Error_Msg_Elab
                    ("IN " & Disp_Node (Port) & " must be connected", Loc);
               end if;
               return True;
            end if;
         when Iir_Out_Mode
           | Iir_Inout_Mode
           | Iir_Buffer_Mode
           | Iir_Linkage_Mode =>
            --  LRM 1.1.1.2  Ports
            --  A port of any mode other than IN may be unconnected or
            --  unassociated as long as its type is not an unconstrained array
            --  type.
            if Get_Kind (Get_Type (Port)) in Iir_Kinds_Array_Type_Definition
              and then (Get_Constraint_State (Get_Type (Port))
                          /= Fully_Constrained)
            then
               if Loc /= Null_Iir then
                  Error_Msg_Elab ("unconstrained " & Disp_Node (Port)
                                  & " must be connected", Loc);
               end if;
               return True;
            end if;
         when others =>
            Error_Kind ("check_open_port", Port);
      end case;
      return False;
   end Check_Open_Port;

   procedure Check_Binding_Indication (Conf : Iir)
   is
      Assoc : Iir;
      Conf_Chain : Iir;
      Inst_Chain : Iir;
      Bind : Iir_Binding_Indication;
      Err : Boolean;
      Inst : Iir;
      Inst_List : Iir_List;
      Formal : Iir;
      Assoc_1 : Iir;
      Actual : Iir;
   begin
      Bind := Get_Binding_Indication (Conf);
      Conf_Chain := Get_Port_Map_Aspect_Chain (Bind);

      Err := False;
      --  Note: the assoc chain is already canonicalized.

      --  First pass: check for open associations in configuration.
      Assoc := Conf_Chain;
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
            Formal := Get_Formal (Assoc);
            Err := Err or Check_Open_Port (Formal, Assoc);
            if Flags.Warn_Binding and then not Get_Artificial_Flag (Assoc) then
               Warning_Msg_Elab
                 (Disp_Node (Formal) & " of " & Disp_Node (Get_Parent (Formal))
                  & " is not bound", Assoc);
               Warning_Msg_Elab
                 ("(in " & Disp_Node (Current_Configuration) & ")",
                  Current_Configuration);
            end if;
         end if;
         Assoc := Get_Chain (Assoc);
      end loop;
      if Err then
         return;
      end if;

      --  Second pass: check for port connected to open in instantiation.
      Inst_List := Get_Instantiation_List (Conf);
      for I in Natural loop
         Inst := Get_Nth_Element (Inst_List, I);
         exit when Inst = Null_Iir;
         Err := False;

         --  Mark component ports not associated.
         Inst_Chain := Get_Port_Map_Aspect_Chain (Inst);
         Assoc := Inst_Chain;
         while Assoc /= Null_Iir loop
            if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
               Formal := Get_Base_Name (Get_Formal (Assoc));
               Set_Open_Flag (Formal, True);
               Err := True;
            end if;
            Assoc := Get_Chain (Assoc);
         end loop;

         --  If there is any component port open, search them in the
         --  configuration.
         if Err then
            Assoc := Conf_Chain;
            while Assoc /= Null_Iir loop
               Formal := Get_Base_Name (Get_Formal (Assoc));
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
                  Actual := Null_Iir;
               else
                  Actual := Get_Actual (Assoc);
                  Actual := Sem_Names.Name_To_Object (Actual);
               end if;
               if Actual /= Null_Iir then
                  Actual := Get_Base_Name (Actual);
               end if;
               if Actual /= Null_Iir
                 and then Get_Open_Flag (Actual)
                 and then Check_Open_Port (Formal, Null_Iir)
               then
                  --  For a better message, find the location.
                  Assoc_1 := Inst_Chain;
                  while Assoc_1 /= Null_Iir loop
                     if Get_Kind (Assoc_1) = Iir_Kind_Association_Element_Open
                       and then Actual = Get_Base_Name (Get_Formal (Assoc_1))
                     then
                        Err := Check_Open_Port (Formal, Assoc_1);
                        exit;
                     end if;
                     Assoc_1 := Get_Chain (Assoc_1);
                  end loop;
               end if;
               Assoc := Get_Chain (Assoc);
            end loop;

            --  Clear open flag.
            Assoc := Inst_Chain;
            while Assoc /= Null_Iir loop
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
                  Formal := Get_Base_Name (Get_Formal (Assoc));
                  Set_Open_Flag (Formal, False);
               end if;
               Assoc := Get_Chain (Assoc);
            end loop;
         end if;
      end loop;
   end Check_Binding_Indication;

   --  CONF is either a configuration specification or a component
   --   configuration.
   --  If ADD_DEFAULT is true, then the default configuration for the design
   --  binding must be added if required.
   procedure Add_Design_Binding_Indication (Conf : Iir; Add_Default : Boolean)
   is
      Bind : Iir_Binding_Indication;
      Inst : Iir;
   begin
      Bind := Get_Binding_Indication (Conf);
      if Bind = Null_Iir then
         if Flags.Warn_Binding then
            Inst := Get_First_Element (Get_Instantiation_List (Conf));
            Warning_Msg_Elab
              (Disp_Node (Inst) & " is not bound", Conf);
            Warning_Msg_Elab
              ("(in " & Disp_Node (Current_Configuration) & ")",
               Current_Configuration);
         end if;
         return;
      end if;
      Check_Binding_Indication (Conf);
      Add_Design_Aspect (Get_Entity_Aspect (Bind), Add_Default);
   end Add_Design_Binding_Indication;

   procedure Add_Design_Block_Configuration (Blk : Iir_Block_Configuration)
   is
      Item : Iir;
      Sub_Config : Iir;
   begin
      if Blk = Null_Iir then
         return;
      end if;
      Item := Get_Configuration_Item_Chain (Blk);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Configuration_Specification =>
               Add_Design_Binding_Indication (Item, True);
            when Iir_Kind_Component_Configuration =>
               Sub_Config := Get_Block_Configuration (Item);
               Add_Design_Binding_Indication (Item, Sub_Config = Null_Iir);
               Add_Design_Block_Configuration (Sub_Config);
            when Iir_Kind_Block_Configuration =>
               Add_Design_Block_Configuration (Item);
            when others =>
               Error_Kind ("add_design_block_configuration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Add_Design_Block_Configuration;

   --  elaboration of a design hierarchy:
   --  creates a list of design unit.
   --
   --  find top configuration (may be a default one), add it to the list.
   --  For each element of the list:
   --  add direct dependences (packages, entity, arch) if not in the list
   --  for architectures and configuration: find instantiations and add
   --  corresponding configurations
   function Configure (Primary_Id : Name_Id; Secondary_Id : Name_Id)
     return Iir
   is
      use Libraries;

      Unit : Iir_Design_Unit;
      Lib_Unit : Iir;
      Top : Iir;
   begin
      Unit := Find_Primary_Unit (Work_Library, Primary_Id);
      if Unit = Null_Iir then
         Error_Msg_Elab ("cannot find entity or configuration "
                         & Name_Table.Image (Primary_Id));
         return Null_Iir;
      end if;
      Lib_Unit := Get_Library_Unit (Unit);
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Entity_Declaration =>
            Load_Design_Unit (Unit, Null_Iir);
            Lib_Unit := Get_Library_Unit (Unit);
            if Secondary_Id /= Null_Identifier then
               Unit := Find_Secondary_Unit (Unit, Secondary_Id);
               if Unit = Null_Iir then
                  Error_Msg_Elab
                    ("cannot find architecture "
                     & Name_Table.Image (Secondary_Id)
                     & " of " & Disp_Node (Lib_Unit));
                  return Null_Iir;
               end if;
            else
               declare
                  Arch_Unit : Iir_Architecture_Declaration;
               begin
                  Arch_Unit := Get_Latest_Architecture (Lib_Unit);
                  if Arch_Unit = Null_Iir then
                     Error_Msg_Elab
                       (Disp_Node (Lib_Unit)
                        & " has no architecture in library "
                        & Name_Table.Image (Get_Identifier (Work_Library)));
                     return Null_Iir;
                  end if;
                  Unit := Get_Design_Unit (Arch_Unit);
               end;
            end if;
            Load_Design_Unit (Unit, Lib_Unit);
            if Nbr_Errors /= 0 then
               return Null_Iir;
            end if;
            Lib_Unit := Get_Library_Unit (Unit);
            Top := Get_Default_Configuration_Declaration (Lib_Unit);
            if Top = Null_Iir then
               --  No default configuration for this architecture.
               raise Internal_Error;
            end if;
         when Iir_Kind_Configuration_Declaration =>
            Top := Unit;
         when others =>
            Error_Msg_Elab (Name_Table.Image (Primary_Id)
                            & " is neither an entity nor a configuration");
            return Null_Iir;
      end case;

      Set_Elab_Flag (Std_Package.Std_Standard_Unit, True);

      Add_Design_Unit (Top, Null_Iir);
      return Top;
   end Configure;

end Configuration;
