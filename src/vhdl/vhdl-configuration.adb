--  Configuration generation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

with Name_Table; use Name_Table;
with Str_Table;
with Flags;
with Errorout; use Errorout;
with Libraries;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Nodes_Walk;
with Vhdl.Sem_Scopes;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Vhdl.Canon;
with Vhdl.Evaluation;
with Vhdl.Scanner;

package body Vhdl.Configuration is
   procedure Add_Design_Concurrent_Stmts (Parent : Iir);
   procedure Add_Verification_Unit_Items (Unit : Iir);
   procedure Add_Design_Block_Configuration (Blk : Iir_Block_Configuration);
   procedure Add_Design_Aspect (Aspect : Iir; Add_Default : Boolean);

   Current_File_Dependence : Iir_List := Null_Iir_List;
   Current_Configuration : Iir_Configuration_Declaration := Null_Iir;

   --  UNIT is a design unit of a configuration declaration.
   --  Fill the DESIGN_UNITS table with all design units required to build
   --  UNIT.
   procedure Add_Design_Unit (Unit : Iir_Design_Unit; From : Location_Type)
   is
      Loc : constant Location_Type := Get_Location (Unit);
      List : Iir_List;
      It : List_Iterator;
      El : Iir;
      Lib_Unit : Iir;
      File : Iir_Design_File;
      Prev_File_Dependence : Iir_List;
   begin
      if Flag_Build_File_Dependence then
         --  The current file depends on unit.
         File := Get_Design_File (Unit);
         if Current_File_Dependence /= Null_Iir_List then
            --  (There is no dependency for default configuration).
            Add_Element (Current_File_Dependence, File);
         end if;
      end if;

      --  If already in the table, then nothing to do.
      if Get_Configuration_Mark_Flag (Unit) then
         --  There might be some direct recursions:
         --  * the default configuration might be implicitly referenced by
         --    a direct entity instantiation
         --  * a configuration may be referenced by itself for a recursive
         --    instantiation
         pragma Assert (Get_Configuration_Done_Flag (Unit)
                          or else (Get_Kind (Get_Library_Unit (Unit))
                                     = Iir_Kind_Configuration_Declaration));
         return;
      end if;
      Set_Configuration_Mark_Flag (Unit, True);

      --  May be enabled to debug dependency construction.
      if False then
         Report_Msg (Msgid_Note, Elaboration, +From,
                        "%n added", (1 => +Unit));
      end if;

      Lib_Unit := Get_Library_Unit (Unit);

      if Flag_Build_File_Dependence then
         --  Switch current_file_dependence to the design file of Unit.
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
         --  Load and analyze UNIT.
         Load_Design_Unit (Unit, From);
         --  TODO: exit now in case of error ?
      end if;

      --  Add packages from depend list.
      --  If Flag_Build_File_Dependences is set, add design units of the
      --  dependence list are added, because of LRM 11.4 Analysis Order.
      --  Note: a design unit may be referenced but unused.
      --  (eg: component specification which does not apply).
      List := Get_Dependence_List (Unit);
      It := List_Iterate_Safe (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         El := Libraries.Find_Design_Unit (El);
         if El /= Null_Iir
           and then Get_Kind (El) = Iir_Kind_Design_Unit
         then
            Lib_Unit := Get_Library_Unit (El);
            if Flag_Build_File_Dependence then
               Add_Design_Unit (El, Loc);
            else
               case Get_Kind (Lib_Unit) is
                  when Iir_Kinds_Package_Declaration
                    | Iir_Kind_Context_Declaration =>
                     Add_Design_Unit (El, Loc);
                  when others =>
                     null;
               end case;
            end if;
         end if;
         Next (It);
      end loop;

      --  Lib_Unit may have changed.
      Lib_Unit := Get_Library_Unit (Unit);

      case Iir_Kinds_Library_Unit (Get_Kind (Lib_Unit)) is
         when Iir_Kind_Package_Declaration =>
            --  Analyze the package declaration, so that Set_Package below
            --  will set the full package (and not a stub).
            Load_Design_Unit (Unit, From);
            Lib_Unit := Get_Library_Unit (Unit);
         when Iir_Kind_Package_Instantiation_Declaration =>
            --  The uninstantiated package is part of the dependency.
            null;
         when Iir_Kind_Configuration_Declaration =>
            --  Add entity and architecture.
            --  find all sub-configuration
            Load_Design_Unit (Unit, From);
            if Nbr_Errors = 0 then
               Lib_Unit := Get_Library_Unit (Unit);
               Add_Design_Unit (Get_Design_Unit (Get_Entity (Lib_Unit)), Loc);
               declare
                  Blk : Iir_Block_Configuration;
                  Prev_Configuration : Iir_Configuration_Declaration;
                  Arch : Iir;
               begin
                  Prev_Configuration := Current_Configuration;
                  Current_Configuration := Lib_Unit;
                  Blk := Get_Block_Configuration (Lib_Unit);
                  Add_Design_Block_Configuration (Blk);
                  Current_Configuration := Prev_Configuration;
                  Arch := Strip_Denoting_Name (Get_Block_Specification (Blk));
                  if Arch /= Null_Iir then
                     Add_Design_Unit (Get_Design_Unit (Arch), Loc);
                  end if;
               end;
            end if;
         when Iir_Kind_Architecture_Body =>
            --  Add entity
            --  find all entity/architecture/configuration instantiation
            declare
               Ent : constant Iir := Get_Entity (Lib_Unit);
            begin
               if Ent /= Null_Iir then
                  --  In case of errors.
                  Add_Design_Unit (Get_Design_Unit (Ent), Loc);
               end if;
               Add_Design_Concurrent_Stmts (Lib_Unit);
            end;
         when Iir_Kinds_Verification_Unit =>
            Add_Verification_Unit_Items (Lib_Unit);
         when Iir_Kind_Entity_Declaration
           | Iir_Kind_Foreign_Module
           | Iir_Kind_Package_Body
           | Iir_Kind_Context_Declaration =>
            null;
      end case;

      --  Add it in the table, after the dependencies.
      Design_Units.Append (Unit);

      Set_Configuration_Done_Flag (Unit, True);

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
                     Error_Msg_Elab
                       (Lib_Unit, "body of %n was never analyzed", +Lib_Unit);
                  elsif Get_Date (Bod) < Get_Date (Unit) then
                     --  Cannot use BOD as location, as the location may not
                     --  exist.
                     Error_Msg_Elab (Lib_Unit, "%n is outdated", +Bod);
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
               Add_Design_Unit (Bod, Loc);
            end if;
         end;
      end if;
   end Add_Design_Unit;

   procedure Add_Design_Concurrent_Stmt (Stmt : Iir) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Component_Instantiation_Statement =>
            if Is_Entity_Instantiation (Stmt) then
               --  Entity or configuration instantiation.
               Add_Design_Aspect (Get_Instantiated_Unit (Stmt), True);
            end if;
         when Iir_Kind_Block_Statement =>
            Add_Design_Concurrent_Stmts (Stmt);
         when Iir_Kind_For_Generate_Statement =>
            Add_Design_Concurrent_Stmts (Get_Generate_Statement_Body (Stmt));
         when Iir_Kind_If_Generate_Statement =>
            declare
               Clause : Iir;
            begin
               Clause := Stmt;
               while Clause /= Null_Iir loop
                  Add_Design_Concurrent_Stmts
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
                     Add_Design_Concurrent_Stmts (Get_Associated_Block (Alt));
                  end if;
                  Alt := Get_Chain (Alt);
               end loop;
            end;
         when Iir_Kinds_Simple_Concurrent_Statement
           | Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Simple_Simultaneous_Statement =>
            null;
         when others =>
            Error_Kind ("add_design_concurrent_stmts(2)", Stmt);
      end case;
   end Add_Design_Concurrent_Stmt;

   procedure Add_Design_Concurrent_Stmts (Parent : Iir)
   is
      Stmt : Iir;
   begin
      Stmt := Get_Concurrent_Statement_Chain (Parent);
      while Stmt /= Null_Iir loop
         Add_Design_Concurrent_Stmt (Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Add_Design_Concurrent_Stmts;

   procedure Add_Verification_Unit_Items (Unit : Iir)
   is
      Item : Iir;
   begin
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Iir loop
         if Get_Kind (Item) in Iir_Kinds_Concurrent_Statement then
            Add_Design_Concurrent_Stmt (Item);
         end if;
         Item := Get_Chain (Item);
      end loop;
   end Add_Verification_Unit_Items;

   --  ASPECT is an entity_aspect_entity.
   procedure Add_Design_Aspect_Entity (Aspect : Iir; Add_Default : Boolean)
   is
      Loc : constant Location_Type := Get_Location (Aspect);
      Entity_Lib : constant Iir := Get_Entity (Aspect);
      Entity : Iir;
      Arch_Name : Iir;
      Arch : Iir;
      Config : Iir;
      Arch_Lib : Iir;
      Id : Name_Id;
   begin
      if Entity_Lib = Null_Iir then
         --  In case of error (using -c).
         return;
      end if;

      --  Add the entity.
      Entity := Get_Design_Unit (Entity_Lib);
      Add_Design_Unit (Entity, Loc);

      if Get_Kind (Entity_Lib) = Iir_Kind_Foreign_Module then
         return;
      end if;

      --  Extract and add the architecture.
      Arch_Name := Get_Architecture (Aspect);
      if Arch_Name /= Null_Iir then
         case Get_Kind (Arch_Name) is
            when Iir_Kind_Simple_Name =>
               Id := Get_Identifier (Arch_Name);
               Arch := Load_Secondary_Unit (Entity, Id, Aspect);
               if Arch = Null_Iir then
                  Error_Msg_Elab ("cannot find architecture %i of %n",
                                  (+Id, +Entity_Lib));
                  return;
               else
                  Set_Named_Entity (Arch_Name, Get_Library_Unit (Arch));
               end if;
            when Iir_Kind_Reference_Name =>
               Arch := Get_Design_Unit (Get_Named_Entity (Arch_Name));
            when others =>
               Error_Kind ("add_design_aspect", Arch_Name);
         end case;
      else
         Arch := Libraries.Get_Latest_Architecture (Entity_Lib);
         if Arch = Null_Iir then
            Error_Msg_Elab
              (Aspect, "no architecture in library for %n", +Entity_Lib);
            return;
         end if;
         Arch := Get_Design_Unit (Arch);
      end if;
      Load_Design_Unit (Arch, Aspect);

      --  Add the default configuration if required.  Must be done
      --  before the architecture in case of recursive instantiation:
      --  the configuration depends on the architecture.
      if Add_Default then
         Arch_Lib := Get_Library_Unit (Arch);

         --  The default configuration may already exist due to a
         --  previous instantiation.  Create it if it doesn't exist.
         Config := Get_Default_Configuration_Declaration (Arch_Lib);
         if Is_Null (Config) then
            Config := Vhdl.Canon.Create_Default_Configuration_Declaration
              (Arch_Lib);
            Set_Default_Configuration_Declaration (Arch_Lib, Config);
         end if;

         if Get_Configuration_Mark_Flag (Config)
           and then not Get_Configuration_Done_Flag (Config)
         then
            --  Recursive instantiation.
            return;
         else
            Add_Design_Unit (Config, Loc);
         end if;
      end if;

      --  Otherwise, simply the architecture.
      Add_Design_Unit (Arch, Loc);
   end Add_Design_Aspect_Entity;

   procedure Add_Design_Aspect (Aspect : Iir; Add_Default : Boolean) is
   begin
      if Aspect = Null_Iir then
         return;
      end if;
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Add_Design_Aspect_Entity (Aspect, Add_Default);
         when Iir_Kind_Entity_Aspect_Configuration =>
            Add_Design_Unit (Get_Design_Unit (Get_Configuration (Aspect)),
                             Get_Location (Aspect));
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
            --  LRM93 1.1.1.2 Ports
            --  A port of mode IN may be unconnected or unassociated only if
            --  its declaration includes a default expression.
            if Get_Default_Value (Port) = Null_Iir then
               if Loc /= Null_Iir then
                  Error_Msg_Elab_Relaxed
                    (Loc, Warnid_Port,
                     "IN %n must be connected (or have a default value)",
                     (1 => +Port));
               end if;
               return True;
            end if;
         when Iir_Out_Mode
           | Iir_Inout_Mode
           | Iir_Buffer_Mode
           | Iir_Linkage_Mode =>
            --  LRM93 1.1.1.2  Ports
            --  A port of any mode other than IN may be unconnected or
            --  unassociated as long as its type is not an unconstrained array
            --  type.
            if Get_Kind (Get_Type (Port)) in Iir_Kinds_Array_Type_Definition
              and then (Get_Constraint_State (Get_Type (Port))
                          /= Fully_Constrained)
            then
               if Loc /= Null_Iir then
                  Error_Msg_Elab
                    (Loc, "unconstrained %n must be connected", +Port);
               end if;
               return True;
            end if;
         when Iir_Unknown_Mode =>
            raise Internal_Error;
      end case;
      return False;
   end Check_Open_Port;

   procedure Check_Binding_Indication (Conf : Iir)
   is
      Comp : constant Iir := Get_Named_Entity (Get_Component_Name (Conf));
      Bind : constant Iir_Binding_Indication := Get_Binding_Indication (Conf);
      Aspect : constant Iir := Get_Entity_Aspect (Bind);
      Ent : constant Iir := Get_Entity_From_Entity_Aspect (Aspect);
      Assoc_Chain : constant Iir := Get_Port_Map_Aspect_Chain (Bind);
      Inter_Chain : Iir;
      Assoc : Iir;
      Inter : Iir;
      Inst_Assoc_Chain : Iir;
      Inst_Inter_Chain : Iir;
      Err : Boolean;
      Inst : Iir;
      Inst_List : Iir_Flist;
      Formal : Iir;
      Assoc_1 : Iir;
      Inter_1 : Iir;
      Actual : Iir;
   begin
      if Get_Kind (Ent) = Iir_Kind_Foreign_Module then
         return;
      end if;

      Inter_Chain := Get_Port_Chain (Ent);
      Err := False;
      --  Note: the assoc chain is already canonicalized.

      --  First pass: check for open associations in configuration.
      Assoc := Assoc_Chain;
      Inter := Inter_Chain;
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
            Formal := Get_Association_Interface (Assoc, Inter);
            Err := Err or Check_Open_Port (Formal, Assoc);
            if Is_Warning_Enabled (Warnid_Binding)
              and then not Get_Artificial_Flag (Assoc)
            then
               Report_Start_Group;
               Warning_Msg_Elab
                 (Warnid_Binding, Assoc, "%n of %n is not bound",
                  (+Formal, +Get_Parent (Formal)));
               Warning_Msg_Elab
                 (Warnid_Binding, Current_Configuration,
                  "(in %n)", +Current_Configuration);
               Report_End_Group;
            end if;
         end if;
         Next_Association_Interface (Assoc, Inter);
      end loop;
      if Err then
         return;
      end if;

      --  Second pass: check for port connected to open in instantiation.
      Inst_List := Get_Instantiation_List (Conf);
      for I in Flist_First .. Flist_Last (Inst_List) loop
         Inst := Get_Nth_Element (Inst_List, I);
         Inst := Get_Named_Entity (Inst);
         Err := False;

         --  Mark component ports not associated.
         Inst_Assoc_Chain := Get_Port_Map_Aspect_Chain (Inst);
         Inst_Inter_Chain := Get_Port_Chain (Comp);
         Assoc := Inst_Assoc_Chain;
         Inter := Inst_Inter_Chain;
         while Assoc /= Null_Iir loop
            if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
               Formal := Get_Association_Interface (Assoc, Inter);
               Set_Open_Flag (Formal, True);
               Err := True;
            end if;
            Next_Association_Interface (Assoc, Inter);
         end loop;

         --  If there is any component port open, search them in the
         --  configuration.
         if Err then
            Assoc := Assoc_Chain;
            Inter := Inter_Chain;
            while Assoc /= Null_Iir loop
               Formal := Get_Association_Interface (Assoc, Inter);
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
                  Actual := Null_Iir;
               else
                  Actual := Get_Actual (Assoc);
                  Actual := Name_To_Object (Actual);
                  if Actual /= Null_Iir then
                     Actual := Get_Object_Prefix (Actual);
                  end if;
               end if;
               if Actual /= Null_Iir
                 and then Get_Open_Flag (Actual)
                 and then Check_Open_Port (Formal, Null_Iir)
               then
                  --  For a better message, find the location.
                  Assoc_1 := Inst_Assoc_Chain;
                  Inter_1 := Inst_Inter_Chain;
                  while Assoc_1 /= Null_Iir loop
                     if Get_Kind (Assoc_1) = Iir_Kind_Association_Element_Open
                       and then
                       Actual = Get_Association_Interface (Assoc_1, Inter_1)
                     then
                        Err := Check_Open_Port (Formal, Assoc_1);
                        exit;
                     end if;
                     Next_Association_Interface (Assoc_1, Inter_1);
                  end loop;
               end if;
               Next_Association_Interface (Assoc, Inter);
            end loop;

            --  Clear open flag.
            Assoc := Inst_Assoc_Chain;
            Inter := Inst_Inter_Chain;
            while Assoc /= Null_Iir loop
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
                  Formal := Get_Association_Interface (Assoc, Inter);
                  Set_Open_Flag (Formal, False);
               end if;
               Next_Association_Interface (Assoc, Inter);
            end loop;
         end if;
      end loop;
   end Check_Binding_Indication;

   function Is_In_Vendor_Library (Inst : Iir) return Boolean
   is
      Parent : Iir;
   begin
      Parent := Strip_Denoting_Name (Inst);
      if Is_Error (Parent) then
         return False;
      end if;
      loop
         Parent := Get_Parent (Parent);
         if Get_Kind (Parent) = Iir_Kind_Library_Declaration then
            return Get_Vendor_Library_Flag (Parent);
         end if;
      end loop;
   end Is_In_Vendor_Library;

   --  CONF is either a configuration specification or a component
   --   configuration.
   --  If ADD_DEFAULT is true, then the default configuration for the design
   --  binding must be added if required.
   procedure Add_Design_Binding_Indication (Conf : Iir; Add_Default : Boolean)
   is
      Bind : constant Iir_Binding_Indication := Get_Binding_Indication (Conf);
      Aspect : Iir;
      Inst : Iir;
      Comp : Iir;
   begin
      if Bind = Null_Iir then
         if Is_Warning_Enabled (Warnid_Binding) then
            Inst := Get_Nth_Element (Get_Instantiation_List (Conf), 0);
            Inst := Strip_Denoting_Name (Inst);
            Comp := Get_Instantiated_Unit (Inst);
            if not Is_In_Vendor_Library (Comp) then
               Report_Start_Group;
               Warning_Msg_Elab (Warnid_Binding, Conf,
                                 "instance %i of component %i is not bound",
                                 (+Inst, +Comp));
               Warning_Msg_Elab (Warnid_Binding, Current_Configuration,
                                 "(in %n)", +Current_Configuration);
               Report_End_Group;
            end if;
         end if;
         return;
      end if;
      Aspect := Get_Entity_Aspect (Bind);
      if Is_Valid (Aspect)
        and then Get_Kind (Aspect) /= Iir_Kind_Entity_Aspect_Open
      then
         Check_Binding_Indication (Conf);
         Add_Design_Aspect (Aspect, Add_Default);
      end if;
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
   --  corresponding configurations.
   --
   --  Return the configuration declaration for the design.
   function Configure
     (Library_Id: Name_Id; Primary_Id : Name_Id; Secondary_Id : Name_Id)
     return Iir
   is
      use Libraries;

      Library : Iir;
      Unit : Iir_Design_Unit;
      Lib_Unit : Iir;
      Top : Iir;
   begin
      if Library_Id /= Null_Identifier then
         Library := Get_Library (Library_Id, Command_Line_Location);
         if Library = Null_Iir then
            return Null_Iir;
         end if;
      else
         Library := Work_Library;
      end if;
      Unit := Find_Primary_Unit (Library, Primary_Id);
      if Unit = Null_Iir then
         Error_Msg_Elab ("cannot find entity or configuration "
                         & Name_Table.Image (Primary_Id));
         return Null_Iir;
      end if;
      if Get_Kind (Unit) = Iir_Kind_Foreign_Module then
         return Unit;
      end if;

      Lib_Unit := Get_Library_Unit (Unit);
      case Get_Kind (Lib_Unit) is
         when Iir_Kind_Entity_Declaration =>
            --  Use WORK as location (should use a command line location ?)
            Load_Design_Unit (Unit, Work_Library);
            Lib_Unit := Get_Library_Unit (Unit);
            if Secondary_Id /= Null_Identifier then
               Unit := Find_Secondary_Unit (Unit, Secondary_Id);
               if Unit = Null_Iir then
                  Error_Msg_Elab ("cannot find architecture %i of %n",
                                  (+Secondary_Id, +Lib_Unit));
                  return Null_Iir;
               end if;
            else
               declare
                  Arch_Unit : Iir_Architecture_Body;
               begin
                  Arch_Unit := Get_Latest_Architecture (Lib_Unit);
                  if Arch_Unit = Null_Iir then
                     Error_Msg_Elab
                       ("%n has no architecture in library %i",
                        (+Lib_Unit, +Work_Library));
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
            pragma Assert
              (Is_Null (Get_Default_Configuration_Declaration (Lib_Unit)));

            Top := Vhdl.Canon.Create_Default_Configuration_Declaration
              (Lib_Unit);
            Set_Default_Configuration_Declaration (Lib_Unit, Top);
            pragma Assert (Is_Valid (Top));
         when Iir_Kind_Configuration_Declaration =>
            if Secondary_Id /= Null_Identifier then
               Error_Msg_Elab
                 ("no secondary unit allowed after configuration %i",
                  +Primary_Id);
               return Null_Iir;
            end if;
            Top := Unit;
         when Iir_Kind_Foreign_Module =>
            Top := Unit;
         when others =>
            Error_Msg_Elab ("%i is neither an entity nor a configuration",
                           +Primary_Id);
            return Null_Iir;
      end case;

      --  Exclude std.standard
      Set_Configuration_Mark_Flag (Vhdl.Std_Package.Std_Standard_Unit, True);
      Set_Configuration_Done_Flag (Vhdl.Std_Package.Std_Standard_Unit, True);

      Add_Design_Unit (Top, Command_Line_Location);
      return Top;
   end Configure;

   procedure Add_Verification_Unit (Vunit : Iir)
   is
      Hier_Name : constant Iir := Get_Hierarchical_Name (Vunit);
      Name : Iir;
   begin
      --  Not bound.
      if Hier_Name = Null_Iir then
         return;
      end if;

      Name := Get_Architecture (Hier_Name);
      if Name /= Null_Node then
         Name := Get_Named_Entity (Name);
         pragma Assert (Get_Kind (Name) = Iir_Kind_Architecture_Body);
      else
         Name := Get_Entity_Name (Hier_Name);
         Name := Get_Named_Entity (Name);
         pragma Assert (Get_Kind (Name) = Iir_Kind_Entity_Declaration);
      end if;

      if not Get_Configuration_Mark_Flag (Get_Design_Unit (Name)) then
         --  Not for a configured unit.
         return;
      end if;
      Set_Bound_Vunit_Chain (Vunit, Get_Bound_Vunit_Chain (Name));
      Set_Bound_Vunit_Chain (Name, Vunit);
      Add_Design_Unit (Get_Design_Unit (Vunit), Get_Location (Vunit));
   end Add_Verification_Unit;

   procedure Add_Verification_Units
   is
      Library : Iir;
      File : Iir;
      Unit : Iir;
      Lib : Iir;
   begin
      --  For each units:
      Library := Libraries.Get_Libraries_Chain;
      while Library /= Null_Iir loop
         File := Get_Design_File_Chain (Library);
         while File /= Null_Iir loop
            Unit := Get_First_Design_Unit (File);
            while Unit /= Null_Iir loop
               if Get_Kind (Unit) = Iir_Kind_Design_Unit then
                  Lib := Get_Library_Unit (Unit);
                  if Get_Kind (Lib) = Iir_Kind_Vunit_Declaration then
                     --  Load it.
                     Load_Design_Unit (Unit, Unit);

                     Add_Verification_Unit (Get_Library_Unit (Unit));
                  end if;
               end if;
               Unit := Get_Chain (Unit);
            end loop;
            File := Get_Chain (File);
         end loop;
         Library := Get_Chain (Library);
      end loop;
   end Add_Verification_Units;

   procedure Check_Entity_Declaration_Top
     (Entity : Iir_Entity_Declaration; Enable_Override : Boolean)
   is
      Has_Error : Boolean := False;

      --  Return TRUE if GRT supports override of generic GEN.
      function Allow_Generic_Override (Gen : Iir) return Boolean
      is
         Gen_Type : constant Iir := Get_Type (Gen);
      begin
         case Get_Kind (Gen_Type) is
            when Iir_Kind_Integer_Type_Definition
              | Iir_Kind_Integer_Subtype_Definition
              | Iir_Kind_Enumeration_Type_Definition
              | Iir_Kind_Enumeration_Subtype_Definition =>
               return True;
            when Iir_Kind_Array_Type_Definition
              | Iir_Kind_Array_Subtype_Definition =>
               --  Only one-dimensional arrays of enumeration are allowed.
               --  If unconstrained, the index must be of integer type.
               if Get_Kind (Get_Base_Type (Get_Element_Subtype (Gen_Type)))
                 /= Iir_Kind_Enumeration_Type_Definition
               then
                  --  Not an array of enumeration type.
                  return False;
               end if;
               declare
                  Indexes : constant Iir_Flist :=
                    Get_Index_Subtype_List (Gen_Type);
               begin
                  if Get_Nbr_Elements (Indexes) /= 1 then
                     --  Not a one-dimensional array.
                     return False;
                  end if;
                  if Get_Constraint_State (Gen_Type) /= Fully_Constrained
                    and then (Get_Kind (Get_Index_Type (Indexes, 0))
                                /= Iir_Kind_Integer_Subtype_Definition)
                  then
                     --  Index not constrained or not of integer subtype.
                     return False;
                  end if;
               end;
               return True;
            when others =>
               return False;
         end case;
      end Allow_Generic_Override;

      procedure Error (Loc : Iir; Msg : String; Arg1 : Earg_Type) is
      begin
         if not Has_Error then
            Error_Msg_Elab ("%n cannot be at the top of a design", +Entity);
            Has_Error := True;
         end if;
         Error_Msg_Elab (Loc, Msg, Arg1);
      end Error;

      El : Iir;
   begin
      --  Check generics.
      El := Get_Generic_Chain (Entity);
      while El /= Null_Iir loop
         case Iir_Kinds_Interface_Declaration (Get_Kind (El)) is
            when Iir_Kinds_Interface_Object_Declaration =>
               if Get_Default_Value (El) = Null_Iir then
                  if not (Enable_Override and Allow_Generic_Override (El)) then
                     Error (El, "(%n has no default value)", +El);
                  end if;
               end if;
            when Iir_Kinds_Interface_Subprogram_Declaration =>
               Error (El, "(%n is a subprogram generic)", +El);
            when Iir_Kind_Interface_Type_Declaration =>
               Error (El, "(%n is a type generic)", +El);
            when Iir_Kind_Interface_Package_Declaration =>
               Error (El, "(%n is a package generic)", +El);
            when Iir_Kind_Interface_Terminal_Declaration =>
               null;
         end case;
         El := Get_Chain (El);
      end loop;

      --  Check port.
      El := Get_Port_Chain (Entity);
      while El /= Null_Iir loop
         if not Is_Fully_Constrained_Type (Get_Type (El))
           and then Get_Default_Value (El) = Null_Iir
         then
            Error (El, "(%n is unconstrained and has no default value)", +El);
         end if;
         El := Get_Chain (El);
      end loop;
   end Check_Entity_Declaration_Top;

   package Top is
      procedure Mark_Instantiated_Units
        (Lib : Iir_Library_Declaration; Loc : Location_Type);

      Nbr_Top_Entities : Natural;
      First_Top_Entity : Iir;

      procedure Find_First_Top_Entity (Lib : Iir_Library_Declaration);
   end Top;

   package body Top is
      use Nodes_Walk;

      Loc_Err : Location_Type;

      --  Add entities to the name table (so that they easily could be found).
      function Add_Entity_Cb (Design : Iir) return Walk_Status
      is
         Lib_Unit : Iir;
      begin
         if not Flags.Flag_Elaborate_With_Outdated then
            --  Discard obsolete or non-analyzed units.
            if Get_Date (Design) < Date_Analyzed then
               return Walk_Continue;
            end if;
         end if;

         Lib_Unit := Get_Library_Unit (Design);
         case Iir_Kinds_Library_Unit (Get_Kind (Lib_Unit)) is
            when Iir_Kind_Architecture_Body
              | Iir_Kind_Configuration_Declaration
              | Iir_Kinds_Verification_Unit =>
               Load_Design_Unit (Design, Loc_Err);
            when Iir_Kind_Entity_Declaration =>
               Load_Design_Unit (Design, Loc_Err);
               --  Library unit has changed (loaded).
               Lib_Unit := Get_Library_Unit (Design);
               Vhdl.Sem_Scopes.Add_Name (Lib_Unit);
            when Iir_Kind_Foreign_Module =>
               Vhdl.Sem_Scopes.Add_Name (Lib_Unit);
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Instantiation_Declaration
              | Iir_Kind_Package_Body
              | Iir_Kind_Context_Declaration =>
               null;
         end case;

         return Walk_Continue;
      end Add_Entity_Cb;

      procedure Mark_Aspect (Aspect : Iir)
      is
         Unit : Iir;
      begin
         case Iir_Kinds_Entity_Aspect (Get_Kind (Aspect)) is
            when Iir_Kind_Entity_Aspect_Entity =>
               Unit := Get_Entity (Aspect);
               if Unit /= Null_Node then
                  --  There may be an error (unit not found).
                  Set_Elab_Flag (Get_Parent (Unit), True);
               end if;
            when Iir_Kind_Entity_Aspect_Configuration
              | Iir_Kind_Entity_Aspect_Open =>
               null;
         end case;
      end Mark_Aspect;

      function Mark_Instantiation_Cb (Stmt : Iir) return Walk_Status
      is
         Inst : Iir;
      begin
         if Get_Kind (Stmt) /= Iir_Kind_Component_Instantiation_Statement then
            return Walk_Continue;
         end if;

         Inst := Get_Instantiated_Unit (Stmt);
         case Get_Kind (Inst) is
            when Iir_Kinds_Denoting_Name =>
               --  TODO: look at default_binding_indication
               --        or configuration_specification ?
               declare
                  Config : constant Iir :=
                    Get_Configuration_Specification (Stmt);
               begin
                  if Is_Valid (Config) then
                     Mark_Aspect
                       (Get_Entity_Aspect (Get_Binding_Indication (Config)));
                     return Walk_Continue;
                  end if;
               end;
               declare
                  use Vhdl.Sem_Scopes;
                  Comp : constant Iir := Get_Named_Entity (Inst);
                  Interp : Name_Interpretation_Type;
                  Decl : Iir;
               begin
                  if Is_Error (Comp) then
                     return Walk_Continue;
                  end if;
                  Interp := Get_Interpretation (Get_Identifier (Comp));
                  if Valid_Interpretation (Interp) then
                     Decl := Get_Declaration (Interp);
                     Set_Elab_Flag (Get_Design_Unit (Decl), True);
                  else
                     --  If there is no corresponding entity name for the
                     --  component name, assume it belongs to a different
                     --  library (or will be set by a configuration unit).
                     null;
                  end if;
               end;
            when Iir_Kinds_Entity_Aspect =>
               Mark_Aspect (Inst);
            when others =>
               Error_Kind ("mark_instantiation_cb", Stmt);
         end case;

         return Walk_Continue;
      end Mark_Instantiation_Cb;

      function Mark_Units_Cb (Design : Iir) return Walk_Status
      is
         Unit : Iir;
         Status : Walk_Status;
      begin
         if not Flags.Flag_Elaborate_With_Outdated then
            if Get_Date (Design) < Date_Analyzed then
               --  Skip outdated units.
               return Walk_Continue;
            end if;
         end if;

         Unit := Get_Library_Unit (Design);
         case Iir_Kinds_Library_Unit (Get_Kind (Unit)) is
            when Iir_Kind_Architecture_Body =>
               Status := Walk_Concurrent_Statements_Chain
                 (Get_Concurrent_Statement_Chain (Unit),
                  Mark_Instantiation_Cb'Access);
               pragma Assert (Status = Walk_Continue);
            when Iir_Kind_Configuration_Declaration =>
               --  Just ignored.
               null;
            when Iir_Kinds_Verification_Unit =>
               declare
                  Item : Iir;
               begin
                  Item := Get_Vunit_Item_Chain (Unit);
                  while Item /= Null_Iir loop
                     if Get_Kind (Item) in Iir_Kinds_Concurrent_Statement
                     then
                        Status := Walk_Concurrent_Statement
                          (Item, Mark_Instantiation_Cb'Access);
                        pragma Assert (Status = Walk_Continue);
                     end if;
                     Item := Get_Chain (Item);
                  end loop;
               end;
            when Iir_Kind_Foreign_Module =>
               if Mark_Foreign_Module = null then
                  raise Internal_Error;
               end if;
               Mark_Foreign_Module.all (Get_Foreign_Node (Unit));
            when Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Instantiation_Declaration
              | Iir_Kind_Package_Body
              | Iir_Kind_Entity_Declaration
              | Iir_Kind_Context_Declaration =>
               null;
         end case;
         return Walk_Continue;
      end Mark_Units_Cb;

      procedure Mark_Instantiated_Units
        (Lib : Iir_Library_Declaration; Loc : Location_Type)
      is
         Status : Walk_Status;
      begin
         pragma Assert (Loc /= No_Location);
         Loc_Err := Loc;

         --  Name table is used to map names to entities.
         Vhdl.Sem_Scopes.Push_Interpretations;
         Vhdl.Sem_Scopes.Open_Declarative_Region;

         --  1. Add all design entities in the name table.
         Status := Walk_Design_Units (Lib, Add_Entity_Cb'Access);
         pragma Assert (Status = Walk_Continue);

         --  2. Walk architecture and configurations, and mark instantiated
         --     entities.
         Status := Walk_Design_Units (Lib, Mark_Units_Cb'Access);
         pragma Assert (Status = Walk_Continue);

         Vhdl.Sem_Scopes.Close_Declarative_Region;
         Vhdl.Sem_Scopes.Pop_Interpretations;
      end Mark_Instantiated_Units;

      function Extract_Entity_Cb (Design : Iir) return Walk_Status
      is
         Unit : Iir;
      begin
         Unit := Get_Library_Unit (Design);

         if not Kind_In (Unit,
                         Iir_Kind_Entity_Declaration, Iir_Kind_Foreign_Module)
         then
            return Walk_Continue;
         end if;

         if Get_Elab_Flag (Design) then
            --  Clean elab flag.
            Set_Elab_Flag (Design, False);
         else
            if Flags.Verbose then
               Report_Msg (Msgid_Note, Elaboration, +Unit,
                           "candidate for top entity: %n", (1 => +Unit));
            end if;
            Nbr_Top_Entities := Nbr_Top_Entities + 1;
            if Nbr_Top_Entities = 1 then
               First_Top_Entity := Unit;
            end if;
         end if;
         return Walk_Continue;
      end Extract_Entity_Cb;

      procedure Find_First_Top_Entity (Lib : Iir_Library_Declaration)
      is
         Status : Walk_Status;
      begin
         Nbr_Top_Entities := 0;
         First_Top_Entity := Null_Iir;

         Status := Walk_Design_Units (Lib, Extract_Entity_Cb'Access);
         pragma Assert (Status = Walk_Continue);
      end Find_First_Top_Entity;

   end Top;

   function Find_Top_Entity (From : Iir; Loc : Location_Type) return Iir is
   begin
      --  FROM is a library or a design file.
      Top.Mark_Instantiated_Units (From, Loc);
      Top.Find_First_Top_Entity (From);

      if Top.Nbr_Top_Entities = 1 then
         return Top.First_Top_Entity;
      else
         return Null_Iir;
      end if;
   end Find_Top_Entity;

   type Override_Entry is record
      Gen : String_Acc;
      Value : String_Acc;
   end record;

   package Override_Table is new Tables
     (Table_Component_Type => Override_Entry,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16);

   procedure Add_Generic_Override (Name : String; Value : String) is
   begin
      Override_Table.Append (Override_Entry'(Gen => new String'(Name),
                                             Value => new String'(Value)));
   end Add_Generic_Override;

   function Override_String_Generic
     (Value : String_Acc; Formal_Type : Iir) return Iir
   is
      use Str_Table;
      use Vhdl.Evaluation;
      El_Type : constant Iir :=
        Get_Base_Type (Get_Element_Subtype (Formal_Type));
      F : constant Positive := Value'First;
      Elist : Iir_Flist;
      Res : Iir;
      Str8 : String8_Id;
      Ntype : Iir;
      Len : Int32;
   begin
      if Get_Kind (El_Type) /= Iir_Kind_Enumeration_Type_Definition then
         return Null_Iir;
      end if;
      Elist := Get_Enumeration_Literal_List (El_Type);

      Str8 := Create_String8;
      if Value'Last >= F + 2
        and then (Value (F) = 'x' or Value (F) = 'X')
        and then Value (F + 1) = '"'
        and then Value (Value'Last) = '"'
      then
         --  Hexa string.
         declare
            C : Character;
            V : Natural;
            E0, E1 : Iir;
            E : Iir;
         begin
            E0 := Find_Name_In_Flist (Elist, Get_Identifier ('0'));
            E1 := Find_Name_In_Flist (Elist, Get_Identifier ('1'));
            if E0 = Null_Iir or E1 = Null_Iir then
               return Null_Iir;
            end if;
            Len := 0;
            for I in F + 2 .. Value'Last - 1 loop
               C := Value (I);
               case C is
                  when '0' .. '9' =>
                     V := Character'Pos (C) - Character'Pos ('0');
                  when 'A' .. 'F' =>
                     V := Character'Pos (C) - Character'Pos ('A') + 10;
                  when 'a' .. 'f' =>
                     V := Character'Pos (C) - Character'Pos ('a') + 10;
                  when '_' =>
                     V := 16;
                  when others =>
                     Error_Msg_Elab ("incorrect character in bit string");
                     V := 16;
               end case;
               if V < 16 then
                  Len := Len + 4;
                  for J in 1 .. 4 loop
                     if V >= 8 then
                        E := E1;
                        V := V - 8;
                     else
                        E := E0;
                     end if;
                     Append_String8 (Nat8 (Get_Enum_Pos (E)));
                     V := V * 2;
                  end loop;
               end if;
            end loop;
         end;
      else
         declare
            Eid : Name_Id;
            Elit : Iir;
         begin
            for I in Value'Range loop
               Eid := Get_Identifier (Value (I));
               Elit := Find_Name_In_Flist (Elist, Eid);
               if Elit = Null_Iir then
                  Error_Msg_Elab ("incorrect character %i in string", +Eid);
                  Elit := Get_Nth_Element (Elist, 0);
               end if;
               Append_String8 (Nat8 (Get_Enum_Pos (Elit)));
            end loop;
         end;
         Len := Value'Length;
      end if;
      Res := Create_Iir (Iir_Kind_String_Literal8);
      Set_String8_Id (Res, Str8);
      --  FIXME: check characters are in the type.
      Set_String_Length (Res, Len);
      Set_Expr_Staticness (Res, Locally);
      Ntype := Create_Unidim_Array_By_Length
        (Get_Base_Type (Formal_Type), Value'Length, Res);
      Set_Type (Res, Ntype);
      Set_Literal_Subtype (Res, Ntype);

      return Res;
   end Override_String_Generic;

   procedure Override_Generic (Gen : Iir; Value : String_Acc)
   is
      use Vhdl.Evaluation;
      Formal_Type : constant Iir := Get_Type (Gen);
      Formal_Btype : constant Iir := Get_Base_Type (Formal_Type);
      Res : Iir;
   begin
      case Get_Kind (Formal_Btype) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            Res := Eval_Value_Attribute (Value.all, Formal_Type, Gen);
            if not Eval_Is_In_Bound (Res, Formal_Type) then
               Error_Msg_Elab ("override for %n is out of bounds", +Gen);
               return;
            end if;
            Set_Literal_Origin (Res, Null_Iir);
         when Iir_Kind_Array_Type_Definition =>
            if Is_One_Dimensional_Array_Type (Formal_Btype) then
               Res := Override_String_Generic (Value, Formal_Type);
            else
               Res := Null_Iir;
            end if;
         when others =>
            Res := Null_Iir;
      end case;
      if Res = Null_Iir then
         Error_Msg_Elab ("unhandled override for %n", +Gen);
         return;
      end if;

      if Get_Is_Ref (Gen) then
         Set_Is_Ref (Gen, False);
      else
         if Get_Has_Identifier_List (Gen) then
            --  Transfer ownership to the next interface.
            Set_Is_Ref (Get_Chain (Gen), False);
         end if;
      end if;
      Set_Location (Res, No_Location);
      Set_Default_Value (Gen, Res);
   end Override_Generic;

   procedure Apply_Generic_Override (Ent : Iir)
   is
   begin
      for I in Override_Table.First .. Override_Table.Last loop
         declare
            Over : constant Override_Entry := Override_Table.Table (I);
         begin
            case Get_Kind (Ent) is
               when Iir_Kind_Entity_Declaration =>
                  declare
                     Inter_Chain : constant Iir := Get_Generic_Chain (Ent);
                     Gen_Name : String := Over.Gen.all;
                     Gen_Id : Name_Id;
                     Inter : Iir;
                     Err : Boolean;
                  begin
                     Vhdl.Scanner.Convert_Identifier (Gen_Name, Err);
                     if Err then
                        Error_Msg_Option
                          ("incorrect name in generic override option");
                        Gen_Id := Null_Identifier;
                     else
                        Gen_Id := Name_Table.Get_Identifier (Gen_Name);

                        Inter := Inter_Chain;
                        while Inter /= Null_Iir loop
                           exit when Get_Identifier (Inter) = Gen_Id;
                           Inter := Get_Chain (Inter);
                        end loop;
                     end if;

                     if Gen_Id = Null_Identifier then
                        --  Skip it
                        null;
                     elsif Inter = Null_Iir then
                        Error_Msg_Elab ("no generic %i for -g", +Gen_Id);
                     elsif (Get_Kind (Inter)
                              /= Iir_Kind_Interface_Constant_Declaration)
                     then
                        --  Could be a generic package, a generic type...
                        Error_Msg_Elab
                          ("generic %n cannot be overriden (not a constant)",
                           +Gen_Id);
                     else
                        Override_Generic (Inter, Over.Value);
                     end if;
                  end;
               when Iir_Kind_Foreign_Module =>
                  Apply_Foreign_Override
                    (Get_Foreign_Node (Ent), Over.Gen.all, Over.Value.all);
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end loop;
   end Apply_Generic_Override;
end Vhdl.Configuration;
