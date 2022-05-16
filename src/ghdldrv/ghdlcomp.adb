--  GHDL driver - compile commands.
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
with Ada.Command_Line;

with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Options; use Options;

with Types; use Types;
with Flags;
with Simple_IO;
with Name_Table;
with Files_Map;

with Vhdl.Std_Package;
with Vhdl.Sem;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Vhdl.Utils;
with Vhdl.Configuration;
with Errorout; use Errorout;
with Libraries;

package body Ghdlcomp is

   Flag_Expect_Failure : Boolean := False;

   --  Commands which use the mcode compiler.
   type Command_Comp is abstract new Command_Lib with null record;

   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Disp_Long_Help (Cmd : Command_Comp);

   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Assert (Option'First = 1);
   begin
      if Option = "--expect-failure" then
         Flag_Expect_Failure := True;
         Res := Option_Ok;
      elsif Option = "--check-ast" then
         Flags.Check_Ast_Level := Flags.Check_Ast_Level + 1;
         Res := Option_Ok;
      elsif Hooks.Decode_Option.all (Option) then
         Res := Option_Ok;
      elsif Option'Length > 18
        and then Option (1 .. 18) = "--time-resolution="
      then
         Res := Option_Ok;
         if Option (19 .. Option'Last) = "fs" then
            Time_Resolution := 'f';
         elsif Option (19 .. Option'Last) = "ps" then
            Time_Resolution := 'p';
         elsif Option (19 .. Option'Last) = "ns" then
            Time_Resolution := 'n';
         elsif Option (19 .. Option'Last) = "us" then
            Time_Resolution := 'u';
         elsif Option (19 .. Option'Last) = "ms" then
            Time_Resolution := 'm';
         elsif Option (19 .. Option'Last) = "sec" then
            Time_Resolution := 's';
         elsif Option (19 .. Option'Last) = "auto" then
            Time_Resolution := 'a';
         else
            Error ("unknown unit name for --time-resolution");
            Res := Option_Err;
         end if;
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;


   procedure Disp_Long_Help (Cmd : Command_Comp)
   is
      use Simple_IO;
   begin
      Disp_Long_Help (Command_Lib (Cmd));
      Hooks.Disp_Long_Help.all;
      Put_Line (" --expect-failure  Expect analysis/elaboration failure");
      Put_Line (" --time-resolution=UNIT   Set the resolution of type time");
      Put_Line ("            UNIT can be fs, ps, ns, us, ms, sec or auto");
   end Disp_Long_Help;

   --  Command -r
   type Command_Run is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Run; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Run) return String;

   procedure Perform_Action (Cmd : in out Command_Run;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Run; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "elab-run"
        or else Name = "--elab-run"
        or else Name = "-r"
        or else Name = "run";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Run) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "elab-run [OPTS] UNIT [ARCH] [RUNOPTS]"
        & ASCII.LF & "  Elaborate and run design UNIT"
        & ASCII.LF & "  aliases: --elab-run, -r, run";
   end Get_Short_Help;


   procedure Perform_Action (Cmd : in out Command_Run;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Opt_Arg : Natural;
   begin
      begin
         Hooks.Compile_Init.all (False);

         Libraries.Load_Work_Library (False);
         Flags.Flag_Elaborate_With_Outdated := False;
         Flags.Flag_Only_Elab_Warnings := True;

         Hooks.Compile_Elab.all ("-r", Args, Opt_Arg);
      exception
         when Compilation_Error =>
            if Flag_Expect_Failure then
               return;
            else
               raise;
            end if;
      end;
      Hooks.Set_Run_Options (Args (Opt_Arg .. Args'Last));
      Hooks.Run.all;
   end Perform_Action;


   --  Command -c xx -r/-e
   type Command_Compile is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Compile; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Compile) return String;
   procedure Decode_Option (Cmd : in out Command_Compile;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Perform_Action (Cmd : in out Command_Compile;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Compile; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "compile"
        or else Name = "-c";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Compile) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "compile [OPTS] FILEs -e|-r UNIT [ARCH] [RUNOPTS]"
        & ASCII.LF & "  Compile, elaborate (and run) design UNIT"
        & ASCII.LF & "  alias: -c";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Compile;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
   begin
      if Option = "-r" or else Option = "-e" then
         Res := Option_End;
      else
         Decode_Option (Command_Comp (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Compile_Analyze_Init (Load_Work : Boolean := True) is
   begin
      Hooks.Compile_Init.all (False);

      Flags.Flag_Elaborate_With_Outdated := True;
      Flags.Flag_Only_Elab_Warnings := False;

      if Load_Work then
         Libraries.Load_Work_Library (False);
         --  Also, load all libraries and files, so that every design unit
         --  is known.
         Load_All_Libraries_And_Files;
      else
         Libraries.Load_Work_Library (True);
      end if;
   end Compile_Analyze_Init;

   procedure Compile_Load_Vhdl_File (File : String)
   is
      Res : Iir_Design_File;
      Design : Iir;
      Next_Design : Iir;
   begin
      Res := Load_File_Name (Name_Table.Get_Identifier (File));
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
   end Compile_Load_Vhdl_File;

   function Compile_Analyze_File (File : String) return Iir
   is
      Id : constant Name_Id := Name_Table.Get_Identifier (File);
      Design_File : Iir_Design_File;
      New_Design_File : Iir_Design_File;
      Unit : Iir;
      Next_Unit : Iir;
   begin
      --  Load file and parse.
      Design_File := Load_File_Name (Id);
      if Design_File = Null_Iir or else Errorout.Nbr_Errors > 0 then
         --  Stop now in case of error (file not found or parse error).
         return Design_File;
      end if;

      --  Analyze and add to the work library.
      Unit := Get_First_Design_Unit (Design_File);
      while Unit /= Null_Iir loop
         Finish_Compilation (Unit, True);

         Next_Unit := Get_Chain (Unit);

         if Errorout.Nbr_Errors = 0 then
            Set_Chain (Unit, Null_Iir);
            Libraries.Add_Design_Unit_Into_Library (Unit);
            New_Design_File := Get_Design_File (Unit);
         end if;

         Unit := Next_Unit;
      end loop;

      if Errorout.Nbr_Errors > 0 then
         return Design_File;
      end if;

      Free_Iir (Design_File);

      --  Do late analysis checks.
      Unit := Get_First_Design_Unit (New_Design_File);
      while Unit /= Null_Iir loop
         Vhdl.Sem.Sem_Analysis_Checks_List
           (Unit, Is_Warning_Enabled (Warnid_Delayed_Checks));
         Unit := Get_Chain (Unit);
      end loop;

      return New_Design_File;
   end Compile_Analyze_File;

   procedure Compile_Elaborate (Unit_Name : String_Access)
   is
      Run_Arg : Natural;
   begin
      Hooks.Compile_Elab.all ("-c", (1 => Unit_Name), Run_Arg);
      pragma Unreferenced (Run_Arg);
   end Compile_Elaborate;

   procedure Compile_Run
   is
      No_Arg : constant Argument_List := (1 .. 0 => null);
   begin
      Hooks.Set_Run_Options (No_Arg);
      Hooks.Run.all;
   end Compile_Run;

   procedure Common_Compile_Init (Analyze_Only : Boolean) is
   begin
      if Analyze_Only then
         if not Setup_Libraries (True) then
            raise Option_Error;
         end if;
      else
         if not Setup_Libraries (False)
           or else not Libraries.Load_Std_Library
         then
            raise Option_Error;
         end if;
         --  WORK library is not loaded.  FIXME: why ?
      end if;

      if Time_Resolution /= 'a' then
         Vhdl.Std_Package.Set_Time_Resolution (Time_Resolution);
      end if;
   end Common_Compile_Init;

   procedure Common_Compile_Elab (Cmd_Name : String;
                                  Args : Argument_List;
                                  Allow_Undef_Generic : Boolean;
                                  Opt_Arg : out Natural;
                                  Config : out Iir)
   is
      Lib_Id : Name_Id;
      Prim_Id : Name_Id;
      Sec_Id : Name_Id;
   begin
      Extract_Elab_Unit
        (Cmd_Name, True, Args, Opt_Arg, Lib_Id, Prim_Id, Sec_Id);
      if Prim_Id = Null_Identifier then
         raise Option_Error;
      end if;

      Flags.Flag_Elaborate := True;

      Config := Vhdl.Configuration.Configure (Lib_Id, Prim_Id, Sec_Id);
      if Config = Null_Iir
        or else Errorout.Nbr_Errors > 0
      then
         raise Compilation_Error;
      end if;

      --  Check (and possibly abandon) if entity can be at the top of the
      --  hierarchy.
      declare
         Conf_Unit : constant Iir := Get_Library_Unit (Config);
         Arch : constant Iir := Get_Named_Entity
           (Get_Block_Specification (Get_Block_Configuration (Conf_Unit)));
         Entity : constant Iir := Vhdl.Utils.Get_Entity (Arch);
      begin
         Vhdl.Configuration.Check_Entity_Declaration_Top
           (Entity, Allow_Undef_Generic);
         if Nbr_Errors > 0 then
            raise Compilation_Error;
         end if;
      end;
   end Common_Compile_Elab;

   procedure Perform_Action (Cmd : in out Command_Compile;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Elab_Arg : Natural;
      Run_Arg : Natural;
   begin
      begin
         if Args'Length > 1 and then
           (Args (Args'First).all = "-r" or else Args (Args'First).all = "-e")
         then
            --  If there is no files, then load the work library, all the
            --  libraries referenced and all the files.
            Compile_Analyze_Init (True);
            Elab_Arg := Args'First + 1;
         else
            --  If there is at least one file, do not load the work library.
            Compile_Analyze_Init (False);
            Elab_Arg := Natural'Last;
            for I in Args'Range loop
               declare
                  Arg : constant String := Args (I).all;
               begin
                  if Arg = "-r" or else Arg = "-e" then
                     Elab_Arg := I + 1;
                     exit;
                  elsif Arg'Last > 7 and then Arg (1 .. 7) = "--work=" then
                     Libraries.Work_Library_Name :=
                       Libraries.Decode_Work_Option (Arg);
                     if Libraries.Work_Library_Name = Null_Identifier then
                        raise Compilation_Error;
                     end if;
                     Libraries.Load_Work_Library (True);
                  else
                     Compile_Load_Vhdl_File (Arg);
                  end if;
               end;
            end loop;

            --  Save the library (and do not elaborate) if there is neither
            --  '-e' nor '-r'.
            if Elab_Arg = Natural'Last then
               Libraries.Save_Work_Library;
               return;
            end if;
         end if;

         Hooks.Compile_Elab.all ("-c", Args (Elab_Arg .. Args'Last), Run_Arg);
      exception
         when Compilation_Error =>
            if Flag_Expect_Failure then
               return;
            else
               raise;
            end if;
      end;
      if Args (Elab_Arg - 1).all = "-r" then
         Hooks.Set_Run_Options (Args (Run_Arg .. Args'Last));
         Hooks.Run.all;
      else
         if Run_Arg <= Args'Last then
            Error_Msg_Option ("options after unit are ignored");
            raise Option_Error;
         end if;
      end if;
   end Perform_Action;

   --  Command -a
   type Command_Analyze is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Analyze; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Analyze) return String;

   procedure Perform_Action (Cmd : in out Command_Analyze;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Analyze; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "analyze"
        or else Name = "-a"
        or else Name = "analyse";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Analyze) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "analyze [OPTS] FILEs"
        & ASCII.LF & "  Analyze one or multiple VHDL files"
        & ASCII.LF & "  aliases: -a, analyse";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Analyze;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Id : Name_Id;
      Design_File : Iir_Design_File;
      New_Design_File : Iir_Design_File;
      Unit : Iir;
      Next_Unit : Iir;
   begin
      if Args'Length = 0 then
         Error ("no file to analyze");
         raise Compilation_Error;
      end if;

      Expect_Filenames (Args);

      Hooks.Compile_Init.all (True);

      --  Parse all files.
      for I in Args'Range loop
         Id := Name_Table.Get_Identifier (Args (I).all);

         --  Parse file.
         Design_File := Load_File_Name (Id);
         if Errorout.Nbr_Errors > 0
           and then not Flags.Flag_Force_Analysis
         then
            raise Compilation_Error;
         end if;

         New_Design_File := Null_Iir;

         if False then
            --  Speed up analysis: remove all previous designs.
            --  However, this is not in the LRM...
            Libraries.Purge_Design_File (Design_File);
         end if;

         if Design_File /= Null_Iir then
            Unit := Get_First_Design_Unit (Design_File);
            while Unit /= Null_Iir loop
               --  Analyze unit.
               Finish_Compilation (Unit, True);

               Next_Unit := Get_Chain (Unit);

               if Errorout.Nbr_Errors = 0
                 or else (Flags.Flag_Force_Analysis
                            and then Get_Library_Unit (Unit) /= Null_Iir)
               then
                  Set_Chain (Unit, Null_Iir);
                  Libraries.Add_Design_Unit_Into_Library (Unit);
                  New_Design_File := Get_Design_File (Unit);
               end if;

               Unit := Next_Unit;
            end loop;

            if Errorout.Nbr_Errors > 0
              and then not Flags.Flag_Force_Analysis
            then
               raise Compilation_Error;
            end if;

            if New_Design_File = Design_File then
               pragma Assert (Flags.Flag_Force_Analysis);
               null;
            else
               Free_Iir (Design_File);
            end if;

            --  Do late analysis checks.
            if New_Design_File /= Null_Iir then
               Unit := Get_First_Design_Unit (New_Design_File);
               while Unit /= Null_Iir loop
                  Vhdl.Sem.Sem_Analysis_Checks_List
                    (Unit, Is_Warning_Enabled (Warnid_Delayed_Checks));
                  Unit := Get_Chain (Unit);
               end loop;

               if Errorout.Nbr_Errors > 0
                 and then not Flags.Flag_Force_Analysis
               then
                  raise Compilation_Error;
               end if;
            end if;
         end if;
      end loop;

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      if Flag_Expect_Failure then
         raise Compilation_Error;
      end if;

      Libraries.Save_Work_Library;

   exception
      when Compilation_Error =>
         if Flag_Expect_Failure and Errorout.Nbr_Errors /= 0 then
            return;
         else
            raise;
         end if;
   end Perform_Action;

   --  Command -e
   type Command_Elab is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Elab; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Elab) return String;
   procedure Decode_Option (Cmd : in out Command_Elab;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);

   procedure Perform_Action (Cmd : in out Command_Elab;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Elab; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "elaborate"
        or else Name = "-e";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Elab) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "elaborate [OPTS] UNIT [ARCH]"
        & ASCII.LF & "  Elaborate design UNIT"
        & ASCII.LF & "  alias: -e";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Elab;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Assert (Option'First = 1);
   begin
      if Option = "-o" then
         if Arg'Length = 0 then
            Res := Option_Arg_Req;
         else
            --  Silently accepted.
            Res := Option_Arg;
         end if;
      elsif Option'Length >= 4 and then Option (1 .. 4) = "-Wl," then
         Error_Msg_Option ("option -Wl is not available when ghdl "
                             & "is not configured with gcc or llvm");
         Res := Option_Err;
      else
         Decode_Option (Command_Comp (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Elab;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Run_Arg : Natural;
   begin
      Hooks.Compile_Init.all (False);

      Libraries.Load_Work_Library (False);
      Flags.Flag_Elaborate_With_Outdated := False;
      Flags.Flag_Only_Elab_Warnings := True;

      Hooks.Compile_Elab.all ("-e", Args, Run_Arg);
      if Run_Arg <= Args'Last then
         Error_Msg_Option ("options after unit are ignored");
         raise Option_Error;
      end if;
      if Flag_Expect_Failure then
         raise Compilation_Error;
      end if;
   exception
      when Compilation_Error =>
         if Flag_Expect_Failure and then Errorout.Nbr_Errors > 0 then
            return;
         else
            raise;
         end if;
   end Perform_Action;

   --  Command dispconfig.
   type Command_Dispconfig is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Dispconfig; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Dispconfig) return String;
   procedure Perform_Action (Cmd : in out Command_Dispconfig;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Dispconfig; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "disp-config"
        or else Name = "--disp-config"
        or else Name = "dispconfig"
        or else Name = "--dispconfig";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Dispconfig) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "disp-config"
        & ASCII.LF & "  Display tools path"
        & ASCII.LF & "  aliases: --disp-config, dispconfig, --dispconfig";
   end Get_Short_Help;

   procedure Disp_Config
   is
      use Simple_IO;
      use Libraries;
   begin
      Disp_Config_Prefixes;

      Put_Line ("default library paths:");
      for I in 2 .. Get_Nbr_Paths loop
         Put (' ');
         Put_Line (Name_Table.Image (Get_Path (I)));
      end loop;
   end Disp_Config;

   procedure Perform_Action (Cmd : in out Command_Dispconfig;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Simple_IO;
   begin
      if Args'Length /= 0 then
         Error ("--disp-config does not accept any argument");
         raise Option_Error;
      end if;
      Put_Line ("command_name: " & Ada.Command_Line.Command_Name);

      Disp_Config;
   end Perform_Action;

   --  Command Make.
   type Command_Make is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Make; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Make) return String;
   procedure Perform_Action (Cmd : in out Command_Make;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Make; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "make"
        or else Name = "-m";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Make) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "make [OPTS] UNIT [ARCH]"
        & ASCII.LF & "  Make design UNIT"
        & ASCII.LF & "  alias: -m";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Make; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);

      Lib_Id : Name_Id;
      Prim_Id : Name_Id;
      Sec_Id : Name_Id;
      Files_List : Iir_List;
      File : Iir_Design_File;
      It : List_Iterator;

      Next_Arg : Natural;
      Date : Date_Type;
      Unit : Iir_Design_Unit;
      Lib : Iir_Library_Declaration;
   begin
      Extract_Elab_Unit ("-m", False, Args, Next_Arg, Lib_Id, Prim_Id, Sec_Id);
      if not Setup_Libraries (True) then
         return;
      end if;

      --  Create list of files.
      Files_List := Build_Dependence (Lib_Id, Prim_Id, Sec_Id);

      if Errorout.Nbr_Errors /= 0 then
         raise Errorout.Compilation_Error;
      end if;

      --  Unmark all libraries.
      Lib := Libraries.Std_Library;
      while Lib /= Null_Iir loop
         Set_Elab_Flag (Lib, False);
         Lib := Get_Chain (Lib);
      end loop;

      Date := Get_Date (Libraries.Work_Library);
      It := List_Iterate (Files_List);
      while Is_Valid (It) loop
         File := Get_Element (It);

         if File = Vhdl.Std_Package.Std_Standard_File then
            null;
         elsif Source_File_Modified (File)
           or else Is_File_Outdated (File)
         then
            Lib := Get_Library (File);
            Date := Get_Date (Lib);

            --  Mark this file as analyzed.
            Set_Analysis_Time_Stamp (File, Files_Map.Get_Os_Time_Stamp);

            Unit := Get_First_Design_Unit (File);
            while Unit /= Null_Iir loop
               if Get_Date (Unit) = Date_Analyzed
                 or else Get_Date (Unit) in Date_Valid
               then
                  Date := Date + 1;
                  Set_Date (Unit, Date);
               end if;
               Unit := Get_Chain (Unit);
            end loop;

            Set_Date (Lib, Date);

            --  Need to be written to disk.
            Set_Elab_Flag (Lib, True);
         end if;

         Next (It);
      end loop;

      --  Save modified libraries.
      if Get_Elab_Flag (Libraries.Work_Library) then
         Libraries.Save_Work_Library;
         Set_Elab_Flag (Libraries.Work_Library, False);
      end if;

      declare
         use Libraries;
         Old_Work_Library : constant Iir_Library_Declaration := Work_Library;
         Old_Work_Library_Name : constant Name_Id := Work_Library_Name;
         Old_Work_Directory : constant Name_Id := Work_Directory;
      begin
         Lib := Libraries.Std_Library;
         while Lib /= Null_Iir loop
            if Get_Elab_Flag (Lib) then
               if Lib = Std_Library then
                  Error ("need to rebuild std library");
                  raise Compile_Error;
               end if;
               Work_Library := Lib;
               Work_Library_Name := Get_Identifier (Lib);
               Work_Directory := Get_Library_Directory (Lib);
               Libraries.Save_Work_Library;
               Set_Elab_Flag (Lib, False);
            end if;
            Lib := Get_Chain (Lib);
         end loop;
         Work_Library := Old_Work_Library;
         Work_Library_Name := Old_Work_Library_Name;
         Work_Directory := Old_Work_Directory;
      end;
   exception
      when Compilation_Error =>
         if Flag_Expect_Failure then
            return;
         else
            raise;
         end if;
   end Perform_Action;

   --  Command Gen_Makefile.
   type Command_Gen_Makefile is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Gen_Makefile; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Gen_Makefile) return String;
   procedure Perform_Action (Cmd : in out Command_Gen_Makefile;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Gen_Makefile; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "gen-makefile"
        or else Name = "--gen-makefile";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Gen_Makefile) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "gen-makefile [OPTS] UNIT [ARCH]"
        & ASCII.LF & "  Generate a Makefile for UNIT"
        & ASCII.LF & "  alias: --gen-makefile";
   end Get_Short_Help;

   function Is_Makeable_File (File : Iir_Design_File) return Boolean is
   begin
      if File = Vhdl.Std_Package.Std_Standard_File then
         return False;
      end if;
      return True;
   end Is_Makeable_File;

   procedure Perform_Action (Cmd : in out Command_Gen_Makefile;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Simple_IO;
      use Name_Table;

      HT : constant Character := ASCII.HT;
      Lib_Id : Name_Id;
      Prim_Id : Name_Id;
      Sec_Id : Name_Id;
      Files_List : Iir_List;
      File : Iir_Design_File;
      Files_It : List_Iterator;

      Lib : Iir_Library_Declaration;
      Dir_Id : Name_Id;

      Next_Arg : Natural;
   begin
      Extract_Elab_Unit
        ("--gen-makefile", False, Args, Next_Arg, Lib_Id, Prim_Id, Sec_Id);
      if not Setup_Libraries (True) then
         return;
      end if;
      Files_List := Build_Dependence (Lib_Id, Prim_Id, Sec_Id);

      Ghdllocal.Gen_Makefile_Disp_Header;

      New_Line;

      Ghdllocal.Gen_Makefile_Disp_Variables;

      Put ("GHDLRUNFLAGS=");
      for I in Next_Arg .. Args'Last loop
         Put (' ');
         Put (Args (I).all);
      end loop;
      New_Line;
      New_Line;

      Put_Line ("# Default target : elaborate");
      Put_Line ("all : elab");
      New_Line;

      Put_Line ("# Elaborate target.  Almost useless");
      Put_Line ("elab : force");
      Put (HT & "$(GHDL) -c $(GHDLFLAGS) -e ");
      Put (Image (Prim_Id));
      if Sec_Id /= Null_Identifier then
         Put (' ');
         Put (Image (Sec_Id));
      end if;
      New_Line;
      New_Line;

      Put_Line ("# Run target");
      Put_Line ("run : force");
      Put (HT & "$(GHDL) -c $(GHDLFLAGS) -r ");
      Put (Image (Prim_Id));
      if Sec_Id /= Null_Identifier then
         Put (' ');
         Put (Image (Sec_Id));
      end if;
      Put (" $(GHDLRUNFLAGS)");
      New_Line;
      New_Line;

      Put_Line ("# Targets to analyze libraries");
      Put_Line ("init: force");
      Files_It := List_Iterate (Files_List);
      while Is_Valid (Files_It) loop
         File := Get_Element (Files_It);
         Dir_Id := Get_Design_File_Directory (File);
         if not Is_Makeable_File (File) then
            --  Builtin file.
            null;
         elsif Dir_Id /= Files_Map.Get_Home_Directory then
            --  Not locally built file.
            Put (HT & "# ");
            Put (Image (Dir_Id));
            Put (Image (Get_Design_File_Filename (File)));
            New_Line;
         else

            Put (HT & "$(GHDL) -a $(GHDLFLAGS)");
            Lib := Get_Library (File);
            if Lib /= Libraries.Work_Library then
               --  Overwrite some options.
               Put (" --work=");
               Put (Image (Get_Identifier (Lib)));
               Dir_Id := Get_Library_Directory (Lib);
               Put (" --workdir=");
               if Dir_Id = Libraries.Local_Directory then
                  Put (".");
               else
                  Put (Image (Dir_Id));
               end if;
            end if;
            Put (' ');
            Put (Image (Get_Design_File_Filename (File)));
            New_Line;
         end if;
         Next (Files_It);
      end loop;
      New_Line;

      Put_Line ("force:");
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Analyze);
      Register_Command (new Command_Elab);
      Register_Command (new Command_Run);
      Register_Command (new Command_Compile);
      Register_Command (new Command_Make);
      Register_Command (new Command_Gen_Makefile);
      Register_Command (new Command_Dispconfig);
   end Register_Commands;

end Ghdlcomp;
