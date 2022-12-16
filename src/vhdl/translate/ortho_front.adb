--  Ortho entry point for translation.
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
with System;
with Interfaces.C_Streams;
with GNAT.OS_Lib;

with Types; use Types;
with Name_Table;
with Hash;
with Interning;
with Flags;
with Libraries;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Configuration;
with Translation;
with Vhdl.Sem;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Errorout; use Errorout;
with Errorout.Console;
with Vhdl.Errors; use Vhdl.Errors;
with Bug;
with Trans_Be;
with Options; use Options;

package body Ortho_Front is
   --  The action to be performed by the compiler.
   type Action_Type is
     (
      --  Normal mode: compile a design file.
      Action_Compile,

      --  Generate code to elaborate a design unit.
      Action_Elaborate,

      --  Elaborate a design.
      Action_Pre_Elaborate,

      --  Analyze files and elaborate unit.
      Action_Anaelab,

      --  Generate code for std.package.
      Action_Compile_Std_Package
      );
   Action : Action_Type := Action_Compile;

   --  Name of the library/entity/architecture to elaborate.
   Elab_Library : Name_Id;
   Elab_Entity : Name_Id;
   Elab_Architecture : Name_Id;
   --  Filename for the list of files to link.
   Elab_Filelist : String_Acc;

   Flag_Expect_Failure : Boolean;

   type Id_Link;
   type Id_Link_Acc is access Id_Link;
   type Id_Link is record
      --  If true, ID is the name of a library (for --work=LIB)
      --  If false, ID is the name of a file.
      Is_Library : Boolean;
      Id : Name_Id;
      Link : Id_Link_Acc;
   end record;
   Anaelab_Files : Id_Link_Acc := null;
   Anaelab_Files_Last : Id_Link_Acc := null;

   procedure Init is
   begin
      --  Set program name for error message.
      Errorout.Console.Install_Handler;

      -- Initialize.
      Trans_Be.Register_Translation_Back_End;

      Options.Initialize;

      Elab_Filelist := null;
      Elab_Library := Null_Identifier;
      Elab_Entity := Null_Identifier;
      Elab_Architecture := Null_Identifier;
      Flag_Expect_Failure := False;
   end Init;

   function Decode_Elab_Option (Arg : String_Acc; Cmd : String)
                               return Natural
   is
      Dot : Natural;
   begin
      Elab_Architecture := Null_Identifier;
      --  Entity (+ architecture) to elaborate
      if Arg = null then
         Error_Msg_Option
           ("entity or configuration name required after " & Cmd);
         return 0;
      end if;

      Dot := Arg'First - 1;
      if Arg (Arg'First) /= '\' then
         for I in Arg'Range loop
            if Arg (I) = '.' then
               Dot := I;
               Elab_Library :=
                 Name_Table.Get_Identifier (Arg (Arg'First .. I - 1));
               exit;
            end if;
         end loop;
      end if;

      if Arg (Arg.all'Last) = ')' then
         --  Name is ENTITY(ARCH).
         --  Split.
         declare
            P : Natural;
            Len : Natural;
            Is_Ext : Boolean;
         begin
            P := Arg.all'Last - 1;
            Len := P - Arg.all'First + 1;
            --  Must be at least 'e(a)'.
            if Len < 4 then
               Error_Msg_Option ("ill-formed name after " & Cmd);
               return 0;
            end if;
            --  Handle extended name.
            if Arg (P) = '\' then
               P := P - 1;
               Is_Ext := True;
            else
               Is_Ext := False;
            end if;
            loop
               if P = Dot + 1 then
                  Error_Msg_Option ("ill-formed name after " & Cmd);
                  return 0;
               end if;
               exit when Arg (P) = '(' and Is_Ext = False;
               if Arg (P) = '\' then
                  if Arg (P - 1) = '\' then
                     P := P - 2;
                  elsif Arg (P - 1) = '(' then
                     P := P - 1;
                     exit;
                  else
                     Error_Msg_Option ("ill-formed name after " & Cmd);
                     return 0;
                  end if;
               else
                  P := P - 1;
               end if;
            end loop;
            Elab_Architecture :=
              Name_Table.Get_Identifier (Arg (P + 1 .. Arg'Last - 1));
            Elab_Entity :=
              Name_Table.Get_Identifier (Arg (Dot + 1 .. P - 1));
         end;
      else
         Elab_Entity := Name_Table.Get_Identifier (Arg (Dot + 1 .. Arg'Last));
         Elab_Architecture := Null_Identifier;
      end if;
      return 2;
   end Decode_Elab_Option;

   function Decode_Option (Opt : String_Acc; Arg: String_Acc) return Natural
   is
      pragma Assert (Opt'First = 1);
   begin
      if Opt.all = "--compile-standard" then
         Action := Action_Compile_Std_Package;
         Flags.Bootstrap := True;
         return 1;
      elsif Opt.all = "--elab" then
         if Action /= Action_Compile then
            Error_Msg_Option ("several --elab options");
            return 0;
         end if;
         Action := Action_Elaborate;
         return Decode_Elab_Option (Arg, "--elab");
      elsif Opt.all = "--pre-elab" then
         if Action /= Action_Compile then
            Error_Msg_Option ("several --pre-elab options");
            return 0;
         end if;
         Action := Action_Pre_Elaborate;
         return Decode_Elab_Option (Arg, "--pre-elab");
      elsif Opt.all = "--anaelab" then
         if Action /= Action_Compile then
            Error_Msg_Option ("several --anaelab options");
            return 0;
         end if;
         Action := Action_Anaelab;
         return Decode_Elab_Option (Arg, "--anaelab");
      elsif Opt'Length > 14
        and then Opt (Opt'First .. Opt'First + 13) = "--ghdl-source="
      then
         if Action /= Action_Anaelab then
            Error_Msg_Option
              ("--ghdl-source option allowed only after --anaelab options");
            return 0;
         end if;
         declare
            L : Id_Link_Acc;
         begin
            if Opt'Length > 15
              and then Opt (Opt'First + 14 .. Opt'First + 20) = "--work="
            then
               L := new Id_Link' (Is_Library => True,
                                  Id => Libraries.Decode_Work_Option
                                    (Opt (Opt'First + 14 .. Opt'Last)),
                                  Link => null);
               if L.Id = Null_Identifier then
                  return 0;
               end if;
            else
               L := new Id_Link'(Is_Library => False,
                                 Id => Name_Table.Get_Identifier
                                   (Opt (Opt'First + 14 .. Opt'Last)),
                                 Link => null);
            end if;

            if Anaelab_Files = null then
               Anaelab_Files := L;
            else
               Anaelab_Files_Last.Link := L;
            end if;
            Anaelab_Files_Last := L;
         end;
         return 1;
      elsif Opt.all = "-l" then
         if Arg = null then
            Error_Msg_Option ("filename required after -l");
         end if;
         if Elab_Filelist /= null then
            Error_Msg_Option ("several -l options");
         else
            Elab_Filelist := new String'(Arg.all);
         end if;
         return 2;
      elsif Opt.all = "--help" then
         Options.Disp_Options_Help;
         return 1;
      elsif Opt.all = "--expect-failure" then
         Flag_Expect_Failure := True;
         return 1;
      elsif Opt'Length > 7 and then Opt (1 .. 7) = "--ghdl-" then
         declare
            subtype Str_Type is String (1 .. Opt'Last - 6);
         begin
            --  The option parameter must be normalized (starts at index 1).
            if Parse_Option (Str_Type (Opt (7 .. Opt'Last))) = Option_Ok then
               return 1;
            else
               return 0;
            end if;
         end;
      elsif Options.Parse_Option (Opt.all) = Option_Ok then
         return 1;
      else
         return 0;
      end if;
   end Decode_Option;

   --  Add dependencies of UNIT to DEP_LIST.  UNIT is not added to DEP_LIST.
   procedure Add_Dependence (Unit : Iir_Design_Unit; Dep_List : Iir_List)
   is
      List : constant Iir_List := Get_Dependence_List (Unit);
      It : List_Iterator;
      El : Iir;
   begin
      It := List_Iterate_Safe (List);
      while Is_Valid (It) loop
         El := Get_Element (It);
         El := Get_Unit_From_Dependence (El);

         if not Get_Configuration_Mark_Flag (El) then
            --  EL is not in the list.
            Add_Dependence (El, Dep_List);

            --  Add to the list (only once).
            Set_Configuration_Mark_Flag (El, True);
            Append_Element (Dep_List, El);
         end if;
         Next (It);
      end loop;
   end Add_Dependence;

   procedure Do_Compile (Vhdl_File : Name_Id)
   is
      Res : Iir_Design_File;
      New_Design_File : Iir_Design_File;
      Design : Iir_Design_Unit;
      Next_Design : Iir_Design_Unit;
      Prev_Design : Iir_Design_Unit;

      --  List of dependencies.
      Dep_List : Iir_List;
      Dep_It : List_Iterator;
   begin
      --  Do not elaborate.
      Flags.Flag_Elaborate := False;

      --  Read and parse the file.
      Res := Load_File_Name (Vhdl_File);
      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      --  Analyze all design units.
      --  FIXME: outdate the design file?
      New_Design_File := Null_Iir;
      Design := Get_First_Design_Unit (Res);
      while Is_Valid (Design) loop
         --  Analyze and canon a design unit.
         Finish_Compilation (Design, True);

         Next_Design := Get_Chain (Design);
         if Errorout.Nbr_Errors = 0 then
            Set_Chain (Design, Null_Iir);
            Libraries.Add_Design_Unit_Into_Library (Design);
            New_Design_File := Get_Design_File (Design);
         end if;

         Design := Next_Design;
      end loop;

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      --  Must have at least one design unit
      pragma Assert (Is_Valid (New_Design_File));

      --  Do late analysis checks.
      Design := Get_First_Design_Unit (New_Design_File);
      while Is_Valid (Design) loop
         Vhdl.Sem.Sem_Analysis_Checks_List
           (Design, Is_Warning_Enabled (Warnid_Delayed_Checks));
         Design := Get_Chain (Design);
      end loop;

      --  Gather dependencies
      pragma Assert (Flags.Flag_Elaborate = False);
      Vhdl.Configuration.Flag_Load_All_Design_Units := False;

      --  Exclude std.standard
      Set_Configuration_Mark_Flag (Vhdl.Std_Package.Std_Standard_Unit, True);
      Set_Configuration_Done_Flag (Vhdl.Std_Package.Std_Standard_Unit, True);

      Dep_List := Create_Iir_List;

      Design := Get_First_Design_Unit (New_Design_File);
      Prev_Design := Null_Iir;
      Set_First_Design_Unit (New_Design_File, Null_Iir);
      Set_Last_Design_Unit (New_Design_File, Null_Iir);
      while Is_Valid (Design) loop
         --  Unlink.
         Next_Design := Get_Chain (Design);
         Set_Chain (Design, Null_Iir);

         --  Discard obsolete units.
         if Get_Date (Design) /= Date_Obsolete then
            if Prev_Design = Null_Iir then
               Set_First_Design_Unit (New_Design_File, Design);
            else
               Set_Last_Design_Unit (New_Design_File, Design);
               Set_Chain (Prev_Design, Design);
            end if;
            Prev_Design := Design;

            Add_Dependence (Design, Dep_List);
         end if;

         Design := Next_Design;
      end loop;

      if Errorout.Nbr_Errors > 0 then
         --  Errors can happen (missing package body for instantiation).
         raise Compilation_Error;
      end if;

      --  Translate declarations of dependencies.
      Translation.Translate_Standard (False);
      Dep_It := List_Iterate (Dep_List);
      while Is_Valid (Dep_It) loop
         Design := Get_Element (Dep_It);
         if Get_Design_File (Design) /= New_Design_File then
            --  Do not yet translate units to be compiled.  They can appear as
            --  dependencies.
            Translation.Translate (Design, False);
         end if;
         Next (Dep_It);
      end loop;

      --  Compile only now.
      --  Note: the order of design unit is kept.
      Design := Get_First_Design_Unit (New_Design_File);
      while Is_Valid (Design) loop
         if Get_Kind (Get_Library_Unit (Design))
           = Iir_Kind_Configuration_Declaration
         then
            --  Defer code generation of configuration declaration.
            --  (default binding may change between analysis and
            --   elaboration).
            Translation.Translate (Design, False);
         else
            Translation.Translate (Design, True);
         end if;

         if Errorout.Nbr_Errors > 0 then
            --  This can happen (foreign attribute).
            raise Compilation_Error;
         end if;

         Design := Get_Chain (Design);
      end loop;

      --  Save the working library.
      Libraries.Save_Work_Library;
   end Do_Compile;

   --  Table of libraries gathered from vhpidirect.
   function Shlib_Build (Name : String) return String_Acc is
   begin
      return new String'(Name);
   end Shlib_Build;

   function Shlib_Equal (Obj : String_Acc; Param : String) return Boolean is
   begin
      return Obj.all = Param;
   end Shlib_Equal;

   package Shlib_Interning is new Interning
     (Params_Type => String,
      Object_Type => String_Acc,
      Hash => Hash.String_Hash,
      Build => Shlib_Build,
      Equal => Shlib_Equal);

   procedure Sem_Foreign_Hook
     (Decl : Iir; Info : Translation.Foreign_Info_Type)
   is
      pragma Unreferenced (Decl);
      use Translation;
   begin
      case Info.Kind is
         when Foreign_Vhpidirect =>
            declare
               Lib : constant String :=
                 Info.Lib_Name (1 .. Info.Lib_Len);
               Shlib : String_Acc;
               pragma Unreferenced (Shlib);
            begin
               if Info.Lib_Len /= 0 and then Lib /= "null" then
                  Shlib := Shlib_Interning.Get (Lib);
               end if;
            end;
         when Foreign_Intrinsic =>
            null;
         when Foreign_Unknown =>
            null;
      end case;
   end Sem_Foreign_Hook;

   --  Write to file FILELIST all the files that are needed to link the design.
   procedure Write_File_List (Filelist : String)
   is
      use Interfaces.C_Streams;
      use System;
      use Vhdl.Configuration;
      use Name_Table;

      Nul : constant Character := Character'Val (0);
      Fname : String := Filelist & Nul;
      Mode : constant String := "wt" & Nul;
      F : FILEs;
      R : int;
      S : size_t;
      pragma Unreferenced (R, S); -- FIXME
      Id : Name_Id;
      Lib : Iir_Library_Declaration;
      File : Iir_Design_File;
      Unit : Iir_Design_Unit;
   begin
      F := fopen (Fname'Address, Mode'Address);
      if F = NULL_Stream then
         Error_Msg_Elab ("cannot open " & Filelist);
         return;
      end if;

      --  Clear elab flags on design files.
      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         File := Get_Design_File (Unit);
         Set_Elab_Flag (File, False);
      end loop;

      for J in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (J);
         File := Get_Design_File (Unit);
         if not Get_Elab_Flag (File) then
            Set_Elab_Flag (File, True);

            --  Write '>LIBRARY_DIRECTORY'.
            Lib := Get_Library (File);
            R := fputc (Character'Pos ('>'), F);
            Id := Get_Library_Directory (Lib);
            S := fwrite (Get_Address (Id),
                         size_t (Get_Name_Length (Id)), 1, F);
            R := fputc (10, F);

            --  Write 'FILENAME'.
            Id := Get_Design_File_Filename (File);
            S := fwrite (Get_Address (Id),
                         size_t (Get_Name_Length (Id)), 1, F);
            R := fputc (10, F);
         end if;
      end loop;

      for I in Shlib_Interning.First_Index .. Shlib_Interning.Last_Index loop
         declare
            Str : constant String_Acc := Shlib_Interning.Get_By_Index (I);
         begin
            R := fputc (Character'Pos ('+'), F);
            S := fwrite (Str.all'Address, size_t (Str'Length), 1, F);
            R := fputc (10, F);
         end;
      end loop;

      R := fclose (F);
   end Write_File_List;

   Nbr_Parse : Natural := 0;

   function Parse (Filename : String_Acc) return Boolean
   is
      Res : Iir_Design_File;
      Design : Iir_Design_Unit;
      Next_Design : Iir_Design_Unit;
      Config : Iir;
   begin
      if Nbr_Parse = 0 then
         --  Initialize only once...
         if not Libraries.Load_Std_Library then
            raise Option_Error;
         end if;

         --  Here, time_base can be set.
         Translation.Initialize;

         if Action = Action_Anaelab and then Anaelab_Files /= null then
            Libraries.Load_Work_Library (True);
         else
            Libraries.Load_Work_Library (False);
         end if;
      end if;
      Nbr_Parse := Nbr_Parse + 1;

      case Action is
         when Action_Elaborate =>
            Flags.Flag_Elaborate := True;
            Flags.Flag_Only_Elab_Warnings := True;
            if Elab_Filelist = null then
               Error_Msg_Option ("missing -l for --elab");
               raise Option_Error;
            end if;

            --  Be sure to collect libraries used for vhpidirect.
            Trans_Be.Sem_Foreign_Hook := Sem_Foreign_Hook'Access;
            Shlib_Interning.Init;

            Config := Vhdl.Configuration.Configure
              (Elab_Library, Elab_Entity, Elab_Architecture);
            if Errorout.Nbr_Errors > 0 then
               --  This may happen (bad entity for example).
               raise Compilation_Error;
            end if;

            Translation.Elaborate (Config, False);

            Write_File_List (Elab_Filelist.all);

            if Errorout.Nbr_Errors > 0 then
               --  This may happen (bad entity for example).
               raise Compilation_Error;
            end if;
         when Action_Pre_Elaborate =>
            Flags.Flag_Elaborate := True;
            Flags.Flag_Only_Elab_Warnings := True;
            if Elab_Filelist = null then
               Error_Msg_Option ("missing -l for --pre-elab");
               raise Option_Error;
            end if;
            raise Program_Error;
         when Action_Anaelab =>
            --  Parse files.
            if Anaelab_Files = null then
               Flags.Flag_Elaborate_With_Outdated := False;
            else
               Flags.Flag_Elaborate_With_Outdated := True;
               declare
                  L : Id_Link_Acc;
               begin
                  L := Anaelab_Files;
                  while L /= null loop
                     if L.Is_Library then
                        Libraries.Work_Library_Name := L.Id;
                        Libraries.Load_Work_Library (True);
                     else
                        Res := Load_File_Name (L.Id);
                        if Errorout.Nbr_Errors > 0 then
                           raise Compilation_Error;
                        end if;

                        --  Put units into library.
                        Design := Get_First_Design_Unit (Res);
                        while not Is_Null (Design) loop
                           Next_Design := Get_Chain (Design);
                           Set_Chain (Design, Null_Iir);
                           Libraries.Add_Design_Unit_Into_Library (Design);
                           Design := Next_Design;
                        end loop;
                     end if;
                     L := L.Link;
                  end loop;
               end;
            end if;

            Flags.Flag_Elaborate := True;
            Flags.Flag_Only_Elab_Warnings := False;
            Config := Vhdl.Configuration.Configure
              (Elab_Library, Elab_Entity, Elab_Architecture);

            if Errorout.Nbr_Errors > 0 then
               raise Compilation_Error;
            end if;

            Translation.Elaborate (Config, True);

            if Errorout.Nbr_Errors > 0 then
               --  This may happen (bad entity for example).
               raise Compilation_Error;
            end if;
         when Action_Compile_Std_Package =>
            if Filename /= null
              and then Filename.all /= "std_standard.vhdl"
            then
               Error_Msg_Option
                 ("--compile-standard is not compatible with a filename");
               return False;
            end if;
            Translation.Translate_Standard (True);

         when Action_Compile =>
            if Filename = null then
               Error_Msg_Option ("no input file");
               return False;
            end if;
            if Nbr_Parse > 1 then
               Error_Msg_Option ("can compile only one file (file """ &
                                 Filename.all & """ ignored)");
               return False;
            end if;
            Do_Compile (Name_Table.Get_Identifier (Filename.all));
      end case;

      if Flag_Expect_Failure then
         return False;
      else
         return True;
      end if;
   exception
      when Compilation_Error =>
         if Flag_Expect_Failure then
            --  Very brutal...
            GNAT.OS_Lib.OS_Exit (0);
         end if;
         return False;
      when Option_Error =>
         return False;
      when E: others =>
         Bug.Disp_Bug_Box (E);
         raise;
   end Parse;
end Ortho_Front;
