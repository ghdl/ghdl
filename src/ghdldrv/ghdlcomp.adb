--  GHDL driver - compile commands.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;

with Ada.Command_Line;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Types;
with Flags;
with Sem;
with Name_Table;
with Errorout; use Errorout;
with Libraries;
with Std_Package;
with Files_Map;
with Version;

package body Ghdlcomp is

   Flag_Expect_Failure : Boolean := False;

   --  Commands which use the mcode compiler.
   type Command_Comp is abstract new Command_Lib with null record;
   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);
   procedure Disp_Long_Help (Cmd : Command_Comp);

   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
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
      use Ada.Text_IO;
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
      return Name = "-r" or Name = "--elab-run";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Run) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-r,--elab-run [OPTS] UNIT [ARCH] [RUNOPTS]  Run UNIT";
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


   --  Command -c xx -r
   type Command_Compile is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Compile; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Compile) return String;
   procedure Decode_Option (Cmd : in out Command_Compile;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);
   procedure Perform_Action (Cmd : in out Command_Compile;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Compile; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-c";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Compile) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-c [OPTS] FILEs -r UNIT [ARCH] [RUNOPTS]  "
        & "Compile, elaborate and run UNIT";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Compile;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
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

   procedure Compile_Analyze_File (File : String)
   is
      Res : Iir_Design_File;
      Design : Iir;
      Next_Design : Iir;
   begin
      Res := Libraries.Load_File (Name_Table.Get_Identifier (File));
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
   end Compile_Analyze_File;

   function Compile_Analyze_File2 (File : String) return Iir
   is
      use Types;
      Id : constant Name_Id := Name_Table.Get_Identifier (File);
      Design_File : Iir_Design_File;
      New_Design_File : Iir_Design_File;
      Unit : Iir;
      Next_Unit : Iir;
   begin
      Design_File := Libraries.Load_File (Id);
      if Design_File = Null_Iir or else Errorout.Nbr_Errors > 0 then
         --  Stop now in case of error (file not found or parse error).
         return Design_File;
      end if;

      Unit := Get_First_Design_Unit (Design_File);
      while Unit /= Null_Iir loop
         Libraries.Finish_Compilation (Unit, True);

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
         Sem.Sem_Analysis_Checks_List
           (Unit, Is_Warning_Enabled (Warnid_Delayed_Checks));
         Unit := Get_Chain (Unit);
      end loop;

      return New_Design_File;
   end Compile_Analyze_File2;

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
                  else
                     Compile_Analyze_File (Arg);
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
      return Name = "-a";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Analyze) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-a [OPTS] FILEs    Analyze FILEs";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Analyze;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Types;
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

      Hooks.Compile_Init.all (True);

      --  Parse all files.
      for I in Args'Range loop
         Id := Name_Table.Get_Identifier (Args (I).all);
         Design_File := Libraries.Load_File (Id);
         if Errorout.Nbr_Errors > 0 then
            raise Compilation_Error;
         end if;

         if False then
            --  Speed up analysis: remove all previous designs.
            --  However, this is not in the LRM...
            Libraries.Purge_Design_File (Design_File);
         end if;

         if Design_File /= Null_Iir then
            Unit := Get_First_Design_Unit (Design_File);
            while Unit /= Null_Iir loop
               Libraries.Finish_Compilation (Unit, True);

               Next_Unit := Get_Chain (Unit);

               if Errorout.Nbr_Errors = 0 then
                  Set_Chain (Unit, Null_Iir);
                  Libraries.Add_Design_Unit_Into_Library (Unit);
                  New_Design_File := Get_Design_File (Unit);
               end if;

               Unit := Next_Unit;
            end loop;

            if Errorout.Nbr_Errors > 0 then
               raise Compilation_Error;
            end if;

            Free_Iir (Design_File);

            --  Do late analysis checks.
            Unit := Get_First_Design_Unit (New_Design_File);
            while Unit /= Null_Iir loop
               Sem.Sem_Analysis_Checks_List
                 (Unit, Is_Warning_Enabled (Warnid_Delayed_Checks));
               Unit := Get_Chain (Unit);
            end loop;

            if Errorout.Nbr_Errors > 0 then
               raise Compilation_Error;
            end if;
         end if;
      end loop;

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
   type Command_Elab is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Elab; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Elab) return String;
   procedure Decode_Option (Cmd : in out Command_Elab;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   procedure Perform_Action (Cmd : in out Command_Elab;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Elab; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-e";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Elab) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-e [OPTS] UNIT [ARCH]  Elaborate UNIT";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Elab;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      pragma Assert (Option'First = 1);
   begin
      if Option = "--expect-failure" then
         Flag_Expect_Failure := True;
         Res := Option_Ok;
      elsif Option = "-o" then
         if Arg'Length = 0 then
            Res := Option_Arg_Req;
         else
            --  Silently accepted.
            Res := Option_Arg;
         end if;
      elsif Option'Length >= 4
        and then Option (1 .. 4) = "-Wl," then
         Error_Msg_Option ("option -Wl is not available when ghdl "
                             & "is not configured with gcc or llvm");
      else
         Decode_Option (Command_Lib (Cmd), Option, Arg, Res);
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
      return Name = "--dispconfig" or else Name = "--disp-config";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Dispconfig) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--disp-config      Disp tools path";
   end Get_Short_Help;

   procedure Disp_Config
   is
      use Ada.Text_IO;
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
      use Ada.Text_IO;
   begin
      if Args'Length /= 0 then
         Error ("--disp-config does not accept any argument");
         raise Errorout.Option_Error;
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
      return Name = "-m";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Make) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-m [OPTS] UNIT [ARCH]  Make UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Make; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Types;

      Files_List : Iir_List;
      File : Iir_Design_File;
      It : List_Iterator;

      Next_Arg : Natural;
      Date : Date_Type;
      Unit : Iir_Design_Unit;
      Lib : Iir_Library_Declaration;
   begin
      Extract_Elab_Unit ("-m", Args, Next_Arg);
      Setup_Libraries (True);

      --  Create list of files.
      Files_List := Build_Dependence (Prim_Name, Sec_Name);

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

         if File = Std_Package.Std_Standard_File then
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
      return Name = "--gen-makefile";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Gen_Makefile) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--gen-makefile [OPTS] UNIT [ARCH]  Generate a Makefile for UNIT";
   end Get_Short_Help;

   function Is_Makeable_File (File : Iir_Design_File) return Boolean is
   begin
      if File = Std_Package.Std_Standard_File then
         return False;
      end if;
      return True;
   end Is_Makeable_File;

   procedure Perform_Action (Cmd : in out Command_Gen_Makefile;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Types;
      use Ada.Text_IO;
      use Ada.Command_Line;
      use Name_Table;

      HT : constant Character := Ada.Characters.Latin_1.HT;
      Files_List : Iir_List;
      File : Iir_Design_File;
      Files_It : List_Iterator;

      Lib : Iir_Library_Declaration;
      Dir_Id : Name_Id;

      Next_Arg : Natural;
   begin
      Extract_Elab_Unit ("--gen-makefile", Args, Next_Arg);
      Setup_Libraries (True);
      Files_List := Build_Dependence (Prim_Name, Sec_Name);

      Put_Line ("# Makefile automatically generated by ghdl");
      Put ("# Version: GHDL ");
      Put (Version.Ghdl_Ver);
      Put (' ');
      Put (Version.Ghdl_Release);
      Put (" - ");
      if Version_String /= null then
         Put (Version_String.all);
      end if;
      New_Line;
      Put_Line ("# Command used to generate this makefile:");
      Put ("# ");
      Put (Command_Name);
      for I in 1 .. Argument_Count loop
         Put (' ');
         Put (Argument (I));
      end loop;
      New_Line;

      New_Line;

      Put ("GHDL=");
      Put_Line (Command_Name);

      --  Extract options for command line.
      Put ("GHDLFLAGS=");
      for I in 2 .. Argument_Count loop
         declare
            Arg : constant String := Argument (I);
         begin
            if Arg (1) = '-' then
               if (Arg'Length > 10 and then Arg (1 .. 10) = "--workdir=")
                 or else (Arg'Length > 7 and then Arg (1 .. 7) = "--ieee=")
                 or else (Arg'Length > 6 and then Arg (1 .. 6) = "--std=")
                 or else (Arg'Length > 7 and then Arg (1 .. 7) = "--work=")
                 or else (Arg'Length > 2 and then Arg (1 .. 2) = "-P")
                 or else (Arg'Length > 2 and then Arg (1 .. 2) = "-f")
                 or else (Arg'Length > 6 and then Arg (1 .. 6) = "--std=")
               then
                  Put (" ");
                  Put (Arg);
               end if;
            end if;
         end;
      end loop;
      New_Line;

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
      Put (Prim_Name.all);
      if Sec_Name /= null then
         Put (' ');
         Put (Sec_Name.all);
      end if;
      New_Line;
      New_Line;

      Put_Line ("# Run target");
      Put_Line ("run : force");
      Put (HT & "$(GHDL) -c $(GHDLFLAGS) -r ");
      Put (Prim_Name.all);
      if Sec_Name /= null then
         Put (' ');
         Put (Sec_Name.all);
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
