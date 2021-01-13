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

with Vhdl.Configuration;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package; use Vhdl.Std_Package;
with Vhdl.Utils; use Vhdl.Utils;
with Libraries;
with Flags;
with Vhdl.Sem;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Trans.Chap1;
with Trans.Chap2;
with Trans.Chap6;
with Trans.Rtis;
with Trans.Helpers2; use Trans.Helpers2;
with Translation; use Translation;
with Trans_Decls; use Trans_Decls;

package body Trans.Chap12 is
   Elab_Nbr_Pkgs : Natural;
   Pkgs_Arr : O_Dnode;

   --  Declare top RTIARRAY and ghdl_ELABORATE.
   procedure Gen_Elab_Decls
   is
      Inter_List : O_Inter_List;
      Arr_Type : O_Tnode;
   begin
      --  We need to create code.
      Set_Global_Storage (O_Storage_Private);

      Rtis.Generate_Top (Elab_Nbr_Pkgs);

      declare
         Cst : O_Dnode;
         pragma Unreferenced (Cst);
      begin
         Cst := Create_String (Flags.Flag_String,
                               Get_Identifier ("__ghdl_flag_string"),
                               O_Storage_Public);
      end;

      --  Create the array of RTIs for packages (as a variable, initialized
      --  during elaboration).
      Arr_Type := New_Array_Subtype
        (Rtis.Ghdl_Rti_Array,
         Rtis.Ghdl_Rti_Access,
         Helpers.New_Index_Lit (Unsigned_64 (Elab_Nbr_Pkgs)));
      New_Var_Decl (Pkgs_Arr, Get_Identifier ("__ghdl_top_RTIARRAY"),
                    O_Storage_Private, Arr_Type);

      --  The elaboration entry point.
      Start_Procedure_Decl (Inter_List, Get_Identifier ("__ghdl_ELABORATE"),
                            O_Storage_Public);
      Finish_Subprogram_Decl (Inter_List, Ghdl_Elaborate);
   end Gen_Elab_Decls;

   procedure Call_Elab_Decls (Arch : Iir; Arch_Instance : O_Enode)
   is
      Assoc : O_Assoc_List;
   begin
      --  Set top instances and RTI.
      --  Do it before the elaboration code, since it may be used to
      --  diagnose errors.
      --  Call ghdl_rti_add_top
      Start_Association (Assoc, Ghdl_Rti_Add_Top);
      New_Association
        (Assoc, New_Lit (New_Unsigned_Literal (Ghdl_Index_Type,
                                               Unsigned_64 (Elab_Nbr_Pkgs))));
      New_Association
        (Assoc, New_Address (New_Obj (Pkgs_Arr), Rtis.Ghdl_Rti_Arr_Acc));
      New_Association
        (Assoc, Rtis.New_Rti_Address (Get_Info (Arch).Block_Rti_Const));
      New_Association
        (Assoc, New_Convert_Ov (Arch_Instance, Ghdl_Ptr_Type));
      New_Procedure_Call (Assoc);

      --  Add std.standard rti
      Start_Association (Assoc, Ghdl_Rti_Add_Package);
      New_Association
        (Assoc,
         Rtis.New_Rti_Address (Get_Info (Standard_Package).Package_Rti_Const));
      New_Procedure_Call (Assoc);
   end Call_Elab_Decls;

   --  Create __ghdl_ELABORATE
   procedure Gen_Main (Entity : Iir_Entity_Declaration;
                       Arch : Iir_Architecture_Body;
                       Config_Subprg : O_Dnode)
   is
      Entity_Info : constant Block_Info_Acc := Get_Info (Entity);
      Arch_Info : constant Block_Info_Acc := Get_Info (Arch);
      Assoc : O_Assoc_List;
      Instance : O_Dnode;
      Arch_Instance : O_Dnode;
      Mark : Id_Mark_Type;
   begin
      Start_Subprogram_Body (Ghdl_Elaborate);
      New_Var_Decl (Arch_Instance, Wki_Arch_Instance,
                    O_Storage_Local, Arch_Info.Block_Decls_Ptr_Type);

      New_Var_Decl (Instance, Wki_Instance, O_Storage_Local,
                    Entity_Info.Block_Decls_Ptr_Type);

      --  Allocate instance for the architecture.
      New_Assign_Stmt
        (New_Obj (Arch_Instance),
         Gen_Alloc (Alloc_System,
                    New_Lit (Get_Scope_Size (Arch_Info.Block_Scope)),
                    Arch_Info.Block_Decls_Ptr_Type));

      --  Set the top instance.
      New_Assign_Stmt
        (New_Obj (Instance),
         New_Address (New_Selected_Acc_Value (New_Obj (Arch_Instance),
                                              Arch_Info.Block_Parent_Field),
                      Entity_Info.Block_Decls_Ptr_Type));

      --  Clear parent field of entity link.
      New_Assign_Stmt
        (New_Selected_Element
           (New_Selected_Acc_Value (New_Obj (Instance),
                                    Entity_Info.Block_Link_Field),
            Rtis.Ghdl_Entity_Link_Parent),
         New_Lit (New_Null_Access (Rtis.Ghdl_Component_Link_Acc)));

      Call_Elab_Decls (Arch, New_Obj_Value (Arch_Instance));

      Gen_Filename (Get_Design_File (Get_Design_Unit (Entity)));

      --  Elab package dependences of top entity (so that default
      --  expressions can be evaluated).
      Start_Association (Assoc, Entity_Info.Block_Elab_Pkg_Subprg);
      New_Procedure_Call (Assoc);

      --  Init instance: assign default values to generic and create ports.
      --  Allow user override of generics.
      Set_Scope_Via_Param_Ptr (Entity_Info.Block_Scope, Instance);
      Push_Identifier_Prefix (Mark, "");
      Chap1.Translate_Entity_Init_Generics (Entity);
      Start_Association (Assoc, Ghdl_Init_Top_Generics);
      New_Procedure_Call (Assoc);
      Chap1.Translate_Entity_Init_Ports (Entity);

      --  Elab instance.
      for K in Elab_Kind loop
         Start_Association (Assoc, Arch_Info.Block_Elab_Subprg (K));
         New_Association (Assoc, New_Obj_Value (Instance));
         New_Procedure_Call (Assoc);
      end loop;

      --  Configure instance.
      Start_Association (Assoc, Config_Subprg);
      New_Association (Assoc, New_Obj_Value (Arch_Instance));
      New_Procedure_Call (Assoc);

      Pop_Identifier_Prefix (Mark);
      Clear_Scope (Entity_Info.Block_Scope);
      Finish_Subprogram_Body;

      Current_Filename_Node := O_Dnode_Null;
   end Gen_Main;

   procedure Gen_Last_Arch (Entity : Iir_Entity_Declaration)
   is
      Entity_Info : Block_Info_Acc;

      Arch : Iir_Architecture_Body;
      Arch_Info : Block_Info_Acc;

      Lib : Iir_Library_Declaration;
      Lib_Mark, Entity_Mark, Arch_Mark : Id_Mark_Type;

      Config : Iir_Configuration_Declaration;
      Config_Info : Config_Info_Acc;

      Const : O_Dnode;
      Instance : O_Dnode;
      Inter_List : O_Inter_List;
      Constr : O_Assoc_List;
      Subprg : O_Dnode;
   begin
      Arch := Libraries.Get_Latest_Architecture (Entity);
      if Arch = Null_Iir then
         Error_Msg_Elab ("no architecture for %n", +Entity);
      end if;
      Arch_Info := Get_Info (Arch);
      if Arch_Info = null then
         --  Nothing to do here, since the architecture is not used.
         return;
      end if;
      Entity_Info := Get_Info (Entity);

      --  Create trampoline for elab, default_architecture
      --  re-create instsize.
      Reset_Identifier_Prefix;
      Lib := Get_Library (Get_Design_File (Get_Design_Unit (Entity)));
      Push_Identifier_Prefix (Lib_Mark, Get_Identifier (Lib));
      Push_Identifier_Prefix (Entity_Mark, Get_Identifier (Entity));
      Push_Identifier_Prefix (Arch_Mark, "LASTARCH");

      --  Instance size.
      New_Const_Decl
        (Const, Create_Identifier ("INSTSIZE"), O_Storage_Public,
         Ghdl_Index_Type);
      Start_Init_Value (Const);
      Finish_Init_Value (Const, Get_Scope_Size (Arch_Info.Block_Scope));

      --  Elaborator.
      for K in Elab_Kind loop
         Start_Procedure_Decl
           (Inter_List, Create_Elab_Identifier (K), O_Storage_Public);
         New_Interface_Decl
           (Inter_List, Instance, Wki_Instance,
            Entity_Info.Block_Decls_Ptr_Type);
         Finish_Subprogram_Decl (Inter_List, Subprg);

         Start_Subprogram_Body (Subprg);
         Start_Association (Constr, Arch_Info.Block_Elab_Subprg (K));
         New_Association (Constr, New_Obj_Value (Instance));
         New_Procedure_Call (Constr);
         Finish_Subprogram_Body;
      end loop;

      --  Default config.
      Config := Get_Default_Configuration_Declaration (Arch);
      if Is_Valid (Config) then
         Config_Info := Get_Info (Get_Library_Unit (Config));
         if Config_Info /= null then
            --  Do not create a trampoline for the default_config if it is not
            --  used.
            Start_Procedure_Decl
              (Inter_List, Create_Identifier ("DEFAULT_CONFIG"),
               O_Storage_Public);
            New_Interface_Decl (Inter_List, Instance, Wki_Instance,
                                Arch_Info.Block_Decls_Ptr_Type);
            Finish_Subprogram_Decl (Inter_List, Subprg);

            Start_Subprogram_Body (Subprg);
            Start_Association (Constr, Config_Info.Config_Subprg);
            New_Association (Constr, New_Obj_Value (Instance));
            New_Procedure_Call (Constr);
         Finish_Subprogram_Body;
         end if;
      end if;

      Pop_Identifier_Prefix (Arch_Mark);
      Pop_Identifier_Prefix (Entity_Mark);
      Pop_Identifier_Prefix (Lib_Mark);
   end Gen_Last_Arch;

   procedure Gen_Dummy_Default_Config (Arch : Iir_Architecture_Body)
   is
      Entity : Iir_Entity_Declaration;
      Lib : Iir_Library_Declaration;
      Lib_Mark, Entity_Mark, Sep_Mark, Arch_Mark : Id_Mark_Type;

      Inter_List : O_Inter_List;

      Subprg : O_Dnode;
   begin
      Reset_Identifier_Prefix;
      Entity := Get_Entity (Arch);
      Lib := Get_Library (Get_Design_File (Get_Design_Unit (Arch)));
      Push_Identifier_Prefix (Lib_Mark, Get_Identifier (Lib));
      Push_Identifier_Prefix (Entity_Mark, Get_Identifier (Entity));
      Push_Identifier_Prefix (Sep_Mark, "ARCH");
      Push_Identifier_Prefix (Arch_Mark, Get_Identifier (Arch));

      --  Elaborator.
      Start_Procedure_Decl
        (Inter_List, Create_Identifier ("DEFAULT_CONFIG"),
         O_Storage_Public);
      Finish_Subprogram_Decl (Inter_List, Subprg);

      Start_Subprogram_Body (Subprg);
      Chap6.Gen_Program_Error (Arch, Chap6.Prg_Err_Dummy_Config);
      Finish_Subprogram_Body;

      Pop_Identifier_Prefix (Arch_Mark);
      Pop_Identifier_Prefix (Sep_Mark);
      Pop_Identifier_Prefix (Entity_Mark);
      Pop_Identifier_Prefix (Lib_Mark);
   end Gen_Dummy_Default_Config;

   procedure Gen_Dummy_Entity_Declaration (Entity : Iir_Entity_Declaration)
   is
      Lib : Iir_Library_Declaration;
      Lib_Mark, Entity_Mark, Arch_Mark : Id_Mark_Type;

      Const : O_Dnode;
      Instance : O_Dnode;
      Inter_List : O_Inter_List;
      Subprg : O_Dnode;
   begin
      --  Create trampoline for elab, default_architecture
      --  re-create instsize.
      Reset_Identifier_Prefix;
      Lib := Get_Library (Get_Design_File (Get_Design_Unit (Entity)));
      Push_Identifier_Prefix (Lib_Mark, Get_Identifier (Lib));
      Push_Identifier_Prefix (Entity_Mark, Get_Identifier (Entity));
      Push_Identifier_Prefix (Arch_Mark, "LASTARCH");

      --  Instance size.
      New_Const_Decl
        (Const, Create_Identifier ("INSTSIZE"), O_Storage_Public,
         Ghdl_Index_Type);
      Start_Init_Value (Const);
      Finish_Init_Value (Const, Ghdl_Index_0);

      --  Elaborator.
      for K in Elab_Kind loop
         Start_Procedure_Decl
           (Inter_List, Create_Elab_Identifier (K), O_Storage_Public);
         New_Interface_Decl
           (Inter_List, Instance, Wki_Instance, Ghdl_Ptr_Type);
         Finish_Subprogram_Decl (Inter_List, Subprg);

         Start_Subprogram_Body (Subprg);
         Finish_Subprogram_Body;
      end loop;

      --  Default config.
      Start_Procedure_Decl
        (Inter_List, Create_Identifier ("DEFAULT_CONFIG"), O_Storage_Public);
      New_Interface_Decl (Inter_List, Instance, Wki_Instance, Ghdl_Ptr_Type);
      Finish_Subprogram_Decl (Inter_List, Subprg);

      Start_Subprogram_Body (Subprg);
      Finish_Subprogram_Body;

      Pop_Identifier_Prefix (Arch_Mark);
      Pop_Identifier_Prefix (Entity_Mark);
      Pop_Identifier_Prefix (Lib_Mark);
   end Gen_Dummy_Entity_Declaration;

   --  Generate dummy subprograms for a package declaration.
   procedure Gen_Dummy_Package_Declaration (Unit : Iir_Design_Unit)
   is
      Pkg : Iir_Package_Declaration;
      Lib : Iir_Library_Declaration;
      Lib_Mark, Pkg_Mark : Id_Mark_Type;

      Decl : Iir;
   begin
      Load_Design_Unit (Unit, Libraries.Command_Line_Location);
      Pkg := Get_Library_Unit (Unit);
      Reset_Identifier_Prefix;
      Lib := Get_Library (Get_Design_File (Get_Design_Unit (Pkg)));
      Push_Identifier_Prefix (Lib_Mark, Get_Identifier (Lib));
      Push_Identifier_Prefix (Pkg_Mark, Get_Identifier (Pkg));

      if Get_Need_Body (Pkg) then
         Decl := Get_Declaration_Chain (Pkg);
         while Decl /= Null_Iir loop
            case Get_Kind (Decl) is
               when Iir_Kind_Function_Declaration
                 | Iir_Kind_Procedure_Declaration =>
                  --  Generate empty body.

                  --  Never a second spec, as this is within a package
                  --  declaration.
                  pragma Assert
                    (not Is_Second_Subprogram_Specification (Decl));

                  if not Get_Foreign_Flag (Decl) then
                     declare
                        Mark : Id_Mark_Type;
                        Inter_List : O_Inter_List;
                        Proc : O_Dnode;
                     begin
                        Chap2.Push_Subprg_Identifier (Decl, Mark);
                        Start_Procedure_Decl
                          (Inter_List, Create_Identifier, O_Storage_Public);
                        Finish_Subprogram_Decl (Inter_List, Proc);
                        Start_Subprogram_Body (Proc);
                        Finish_Subprogram_Body;
                        Pop_Identifier_Prefix (Mark);
                     end;
                  end if;
               when others =>
                  null;
            end case;
            Decl := Get_Chain (Decl);
         end loop;
      end if;

      --  Create the body elaborator.
      declare
         Inter_List : O_Inter_List;
         Proc : O_Dnode;
      begin
         Start_Procedure_Decl
           (Inter_List, Create_Identifier ("ELAB_BODY"), O_Storage_Public);
         Finish_Subprogram_Decl (Inter_List, Proc);
         Start_Subprogram_Body (Proc);
         Finish_Subprogram_Body;
      end;

      Pop_Identifier_Prefix (Pkg_Mark);
      Pop_Identifier_Prefix (Lib_Mark);
   end Gen_Dummy_Package_Declaration;

   --  Write to file FILELIST all the files that are needed to link the design.
   procedure Gen_Stubs
   is
      use Vhdl.Configuration;

      --  Add all dependences of UNIT.
      --  UNIT is not used, but added during link.
      procedure Add_Unit_Dependences (Unit : Iir_Design_Unit)
      is
         Dep_List : Iir_List;
         Dep : Iir;
         Dep_Unit : Iir_Design_Unit;
         Dep_It : List_Iterator;
         Lib_Unit : Iir;
      begin
         --  Load the unit in memory to compute the dependence list.
         Load_Design_Unit (Unit, Libraries.Command_Line_Location);
         Update_Node_Infos;

         Set_Elab_Flag (Unit, True);
         Design_Units.Append (Unit);

         if Flag_Rti then
            Rtis.Generate_Library
              (Get_Library (Get_Design_File (Unit)), True);
         end if;

         Lib_Unit := Get_Library_Unit (Unit);
         case Get_Kind (Lib_Unit) is
            when Iir_Kind_Package_Declaration =>
               --  The body may be required due to incomplete constant
               --  declarations, or to call to a subprogram.
               declare
                  Pack_Body : Iir;
               begin
                  Pack_Body := Libraries.Find_Secondary_Unit
                    (Unit, Null_Identifier);
                  if Pack_Body /= Null_Iir then
                     Add_Unit_Dependences (Pack_Body);
                  else
                     Gen_Dummy_Package_Declaration (Unit);
                  end if;
               end;
            when Iir_Kind_Entity_Declaration =>
               Gen_Dummy_Entity_Declaration (Lib_Unit);
            when Iir_Kind_Architecture_Body =>
               Gen_Dummy_Default_Config (Lib_Unit);
            when others =>
               null;
         end case;

         Dep_List := Get_Dependence_List (Unit);
         Dep_It := List_Iterate (Dep_List);
         while Is_Valid (Dep_It) loop
            Dep := Get_Element (Dep_It);
            Dep_Unit := Libraries.Find_Design_Unit (Dep);
            if Dep_Unit = Null_Iir then
               Error_Msg_Elab ("could not find design unit %n", +Dep);
            elsif not Get_Elab_Flag (Dep_Unit) then
               Add_Unit_Dependences (Dep_Unit);
            end if;
            Next (Dep_It);
         end loop;
      end Add_Unit_Dependences;

      --  Add not yet added units of FILE.
      procedure Add_File_Units (File : Iir_Design_File)
      is
         Unit : Iir_Design_Unit;
      begin
         Unit := Get_First_Design_Unit (File);
         while Unit /= Null_Iir loop
            if not Get_Elab_Flag (Unit) then
               --  Unit is not used for the design, but is present in the final
               --  link.  As it may import dependencies, generate dummy
               --  subprograms and variables for these dependencies.
               Add_Unit_Dependences (Unit);
            end if;
            Unit := Get_Chain (Unit);
         end loop;
      end Add_File_Units;

      File : Iir_Design_File;
      Unit : Iir_Design_Unit;
      J : Natural;
   begin
      --  Set elab flags on units, and remove it on design files.
      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         Set_Elab_Flag (Unit, True);
         File := Get_Design_File (Unit);
         Set_Elab_Flag (File, False);
      end loop;

      J := Design_Units.First;
      while J <= Design_Units.Last loop
         Unit := Design_Units.Table (J);
         File := Get_Design_File (Unit);
         if not Get_Elab_Flag (File) then
            Set_Elab_Flag (File, True);

            --  Add dependences of unused design units, otherwise the object
            --  link case failed.
            Add_File_Units (File);
         end if;
         J := J + 1;
      end loop;
   end Gen_Stubs;

   procedure Elaborate (Config : Iir_Design_Unit; Whole : Boolean)
   is
      use Vhdl.Configuration;

      Unit : Iir_Design_Unit;
      Lib_Unit : Iir;
      Config_Lib : Iir_Configuration_Declaration;
      Entity : Iir_Entity_Declaration;
      Arch : Iir_Architecture_Body;
      Conf_Info : Config_Info_Acc;
      Last_Design_Unit : Natural;
   begin
      Config_Lib := Get_Library_Unit (Config);
      Entity := Get_Entity (Config_Lib);
      Arch := Strip_Denoting_Name
        (Get_Block_Specification (Get_Block_Configuration (Config_Lib)));

      --  Be sure the entity can be at the top of a design.
      Check_Entity_Declaration_Top (Entity, True);

      --  If all design units are loaded, late semantic checks can be
      --  performed.
      if Flag_Load_All_Design_Units then
         for I in Design_Units.First .. Design_Units.Last loop
            Unit := Design_Units.Table (I);
            Vhdl.Sem.Sem_Analysis_Checks_List (Unit, False);
            --  There cannot be remaining checks to do.
            pragma Assert
              (Get_Analysis_Checks_List (Unit) = Null_Iir_List);
         end loop;
      end if;

      --  Return now in case of errors.
      if Nbr_Errors /= 0 then
         return;
      end if;

      if Flags.Verbose then
         Report_Msg (Msgid_Note, Elaboration, No_Source_Coord,
                     "List of units in the hierarchy design:");
         for I in Design_Units.First .. Design_Units.Last loop
            Unit := Design_Units.Table (I);
            Lib_Unit := Get_Library_Unit (Unit);
            Report_Msg (Msgid_Note, Elaboration, No_Source_Coord,
                        " %n", (1 => +Lib_Unit));
         end loop;
      end if;

      if Whole then
         --  In compile-and-elaborate mode, do not generate code for
         --  unused subprograms.
         --  FIXME: should be improved by creating a span-tree.
         Flag_Discard_Unused := True;
         Flag_Discard_Unused_Implicit := True;
      end if;

      --  Generate_Library add infos, therefore the info array must be
      --  adjusted.
      Update_Node_Infos;
      Rtis.Generate_Library (Libraries.Std_Library, True);
      Translate_Standard (Whole);

      --  Std.Standard has no body and is always in the closure.  Exclude it
      --  from the stub and filelist generation.
      Set_Elab_Flag (Std_Standard_Unit, True);

      --  Translate all configurations needed.
      --  Also, set the ELAB_FLAG on package with body.
      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         Lib_Unit := Get_Library_Unit (Unit);

         if Whole then
            --  In whole compilation mode, force to generate RTIS of
            --  libraries.
            Rtis.Generate_Library (Get_Library (Get_Design_File (Unit)), True);
         end if;

         case Get_Kind (Lib_Unit) is
            when Iir_Kind_Configuration_Declaration =>
               if Get_Identifier (Lib_Unit) /= Null_Identifier then
                  --  Always generate code for configuration.
                  --  Because default binding may be changed between analysis
                  --  and elaboration.
                  Translate (Unit, True);
               end if;
            when Iir_Kind_Entity_Declaration
              | Iir_Kind_Architecture_Body
              | Iir_Kind_Package_Declaration
              | Iir_Kind_Package_Instantiation_Declaration =>
               --  For package spec, mark it as 'body is not present', this
               --  flag will be set below when the body is translated.
               Set_Elab_Flag (Unit, False);
               Translate (Unit, Whole);
            when Iir_Kind_Package_Body =>
               --  Mark the spec with 'body is present' flag.
               Set_Elab_Flag (Get_Design_Unit (Get_Package (Lib_Unit)), True);
               Translate (Unit, Whole);
            when Iir_Kind_Context_Declaration =>
               null;
            when others =>
               Error_Kind ("elaborate", Lib_Unit);
         end case;
      end loop;

      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         Lib_Unit := Get_Library_Unit (Unit);
         if Get_Kind (Lib_Unit) = Iir_Kind_Configuration_Declaration
           and then Get_Identifier (Lib_Unit) = Null_Identifier
         then
            --  Because of possible indirect recursion, translate default
            --  configuration at the end.
            Translate (Unit, True);
         end if;
      end loop;

      --  Generate code to elaboration body-less package.
      --
      --  When a package is analyzed, we don't know whether there is body
      --  or not.  Therefore, we assume there is always a body, and will
      --  elaborate the body (which elaborates its spec).  If a package
      --  has no body, create the body elaboration procedure.
      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         Lib_Unit := Get_Library_Unit (Unit);
         case Get_Kind (Lib_Unit) is
            when Iir_Kind_Package_Declaration =>
               if not Get_Elab_Flag (Unit) then
                  Chap2.Elab_Package_Body (Lib_Unit, Null_Iir);
               end if;
            when Iir_Kind_Entity_Declaration =>
               Gen_Last_Arch (Lib_Unit);
            when Iir_Kind_Architecture_Body
              | Iir_Kind_Package_Body
              | Iir_Kind_Configuration_Declaration
              | Iir_Kind_Package_Instantiation_Declaration
              | Iir_Kind_Context_Declaration =>
               null;
            when others =>
               Error_Kind ("elaborate(2)", Lib_Unit);
         end case;
      end loop;

      Gen_Elab_Decls;

      --  Create main code.
      Conf_Info := Get_Info (Config_Lib);
      Gen_Main (Entity, Arch, Conf_Info.Config_Subprg);

      --  Index of the last design unit, required by the design.
      Last_Design_Unit := Design_Units.Last;

      --  A design file may contain unused units that depends on other files.
      --  In order to have all symbols resolved, also add unused packages and
      --  generate stubs for referenced (but unused) entities and
      --  configurations.
      if not Whole then
         Gen_Stubs;
      end if;

      --  Disp list of files needed.
      if Flags.Verbose then
         Report_Msg (Msgid_Note, Elaboration, No_Source_Coord,
                     "List of units not used:");
         for I in Last_Design_Unit + 1 .. Design_Units.Last loop
            Unit := Design_Units.Table (I);
            Lib_Unit := Get_Library_Unit (Unit);
            Report_Msg (Msgid_Note, Elaboration, No_Source_Coord,
                        " %n", (1 => +Lib_Unit));
         end loop;
      end if;
   end Elaborate;
end Trans.Chap12;
