--  GHDL driver - commands invoking gcc.
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
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Table;
with GNAT.Dynamic_Tables;
with Libraries;
with Name_Table; use Name_Table;
with Std_Package;
with Types; use Types;
with Iirs; use Iirs;
with Files_Map;
with Flags;
with Configuration;
--with Disp_Tree;
with Default_Pathes;
with Interfaces.C_Streams;
with System;
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Version;
with Options;

package body Ghdldrv is
   --  Name of the tools used.
   Compiler_Cmd : String_Access := null;
   Post_Processor_Cmd : String_Access := null;
   Assembler_Cmd : constant String := "as";
   Linker_Cmd : constant String := "gcc";
   Llvm_Linker_Cmd : constant String := "llvm-ld";

   --  Path of the tools.
   Compiler_Path : String_Access;
   Post_Processor_Path : String_Access;
   Assembler_Path : String_Access;
   Linker_Path : String_Access;

   --  Set by the '-o' option: the output filename.  If the option is not
   --  present, then null.
   Output_File : String_Access;

   --  "-o" string.
   Dash_O : String_Access;

   --  "-quiet" option.
   Dash_Quiet : String_Access;


   --  If set, do not assmble
   Flag_Asm : Boolean;

   --  If true, executed commands are displayed.
   Flag_Disp_Commands : Boolean;

   --  Flag not quiet
   Flag_Not_Quiet : Boolean;

   --  True if failure expected.
   Flag_Expect_Failure : Boolean;

   --  Argument table for the tools.
   --  Each table low bound is 1 so that the length of a table is equal to
   --  the last bound.
   package Argument_Table_Pkg is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type => Integer,
      Table_Low_Bound => 1,
      Table_Initial => 4,
      Table_Increment => 100);
   use Argument_Table_Pkg;

   --  Arguments for tools.
   Compiler_Args : Argument_Table_Pkg.Instance;
   Postproc_Args : Argument_Table_Pkg.Instance;
   Assembler_Args : Argument_Table_Pkg.Instance;
   Linker_Args : Argument_Table_Pkg.Instance;

   --  Display the program spawned in Flag_Disp_Commands is TRUE.
   --  Raise COMPILE_ERROR in case of failure.
   procedure My_Spawn (Program_Name : String; Args : Argument_List)
   is
      Status : Integer;
   begin
      if Flag_Disp_Commands then
         Put (Program_Name);
         for I in Args'Range loop
            Put (' ');
            Put (Args (I).all);
         end loop;
         New_Line;
      end if;
      Status := Spawn (Program_Name, Args);
      if Status = 0 then
         return;
      elsif Status = 1 then
         Error ("compilation error");
         raise Compile_Error;
      else
         Error ("exec error");
         raise Exec_Error;
      end if;
   end My_Spawn;

   --  Compile FILE with additional argument OPTS.
   procedure Do_Compile (Options : Argument_List; File : String)
   is
      Obj_File : String_Access;
      Asm_File : String_Access;
      Post_File : String_Access;
      Success : Boolean;
   begin
      --  Create post file.
      case Compile_Kind is
         when Compile_Debug =>
            Post_File := Append_Suffix (File, Post_Suffix);
         when others =>
            null;
      end case;

      --  Create asm file.
      case Compile_Kind is
         when Compile_Gcc
           | Compile_Debug =>
            Asm_File := Append_Suffix (File, Asm_Suffix);
         when Compile_Llvm =>
            Asm_File := Append_Suffix (File, Llvm_Suffix);
         when Compile_Mcode =>
            null;
      end case;

      --  Create obj file.
      if Compile_Kind = Compile_Mcode or else not Flag_Asm
      then
         Obj_File := Append_Suffix (File, Get_Object_Suffix.all);
      end if;

      --  Compile.
      declare
         P : Natural;
         Nbr_Args : constant Natural :=
           Last (Compiler_Args) + Options'Length + 4;
         Args : Argument_List (1 .. Nbr_Args);
      begin
         P := 0;
         for I in First .. Last (Compiler_Args) loop
            P := P + 1;
            Args (P) := Compiler_Args.Table (I);
         end loop;
         for I in Options'Range loop
            P := P + 1;
            Args (P) := Options (I);
         end loop;

         --  Add -quiet.
         if not Flag_Not_Quiet then
            P := P + 1;
            Args (P) := Dash_Quiet;
         end if;

         Args (P + 1) := Dash_O;
         case Compile_Kind is
            when Compile_Debug =>
               Args (P + 2) := Post_File;
            when Compile_Gcc
              | Compile_Llvm =>
               Args (P + 2) := Asm_File;
            when Compile_Mcode =>
               Args (P + 2) := Obj_File;
         end case;
         Args (P + 3) := new String'(File);

         My_Spawn (Compiler_Path.all, Args (1 .. P + 3));
         Free (Args (P + 3));
      exception
         when Compile_Error =>
            --  Delete temporary file in case of error.
            Delete_File (Args (P + 2).all, Success);
            --  FIXME: delete object file too ?
            raise;
      end;

      --  Post-process.
      if Compile_Kind = Compile_Debug then
         declare
            P : Natural;
            Nbr_Args : constant Natural := Last (Postproc_Args) + 4;
            Args : Argument_List (1 .. Nbr_Args);
         begin
            P := 0;
            for I in First .. Last (Postproc_Args) loop
               P := P + 1;
               Args (P) := Postproc_Args.Table (I);
            end loop;

            if not Flag_Not_Quiet then
               P := P + 1;
               Args (P) := Dash_Quiet;
            end if;

            Args (P + 1) := Dash_O;
            Args (P + 2) := Asm_File;
            Args (P + 3) := Post_File;
            My_Spawn (Post_Processor_Path.all, Args (1 .. P + 3));
         end;

         Free (Post_File);
      end if;

      --  Assemble.
      if Compile_Kind >= Compile_Gcc then
         if Flag_Expect_Failure then
            Delete_File (Asm_File.all, Success);
         elsif not Flag_Asm then
            declare
               P : Natural;
               Nbr_Args : constant Natural := Last (Assembler_Args) + 4;
               Args : Argument_List (1 .. Nbr_Args);
               Success : Boolean;
            begin
               P := 0;
               for I in First .. Last (Assembler_Args) loop
                  P := P + 1;
                  Args (P) := Assembler_Args.Table (I);
               end loop;

               Args (P + 1) := Dash_O;
               Args (P + 2) := Obj_File;
               Args (P + 3) := Asm_File;
               My_Spawn (Assembler_Path.all, Args (1 .. P + 3));
               Delete_File (Asm_File.all, Success);
            end;
         end if;
      end if;

      Free (Asm_File);
      Free (Obj_File);
   end Do_Compile;

   package Filelist is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1,
      Table_Initial => 16,
      Table_Increment => 100);

   Link_Obj_Suffix : String_Access;

   --  Read a list of files from file FILENAME.
   --  Lines starting with a '#' are ignored (comments)
   --  Lines starting with a '>' are directory lines
   --  If first character of a line is a '@', it is replaced with
   --    the prefix_path.
   --  If TO_OBJ is true, then each file is converted to an object file name
   --   (suffix is replaced by the object file extension).
   procedure Add_File_List (Filename : String; To_Obj : Boolean)
   is
      use Interfaces.C_Streams;
      use System;
      use Ada.Characters.Latin_1;

      --  Replace the first '@' with the machine path.
      function Substitute (Str : String) return String
      is
      begin
         for I in Str'Range loop
            if Str (I) = '@' then
               return Str (Str'First .. I - 1)
                 & Get_Machine_Path_Prefix
                 & Str (I + 1 .. Str'Last);
            end if;
         end loop;
         return Str;
      end Substitute;

      Dir : String (1 .. max_path_len);
      Dir_Len : Natural;
      Line : String (1 .. max_path_len);
      Stream : Interfaces.C_Streams.FILEs;
      Mode : constant String := "rt" & Ghdllocal.Nul;
      L : Natural;
      File : String_Access;
   begin
      Line (1 .. Filename'Length) := Filename;
      Line (Filename'Length + 1) := Ghdllocal.Nul;
      Stream := fopen (Line'Address, Mode'Address);
      if Stream = NULL_Stream then
         Error ("cannot open " & Filename);
         raise Compile_Error;
      end if;
      Dir_Len := 0;
      loop
         exit when fgets (Line'Address, Line'Length, Stream) = NULL_Stream;
         if Line (1) /= '#' then
            --  Compute string length.
            L := 0;
            while Line (L + 1) /= Ghdllocal.Nul loop
               L := L + 1;
            end loop;

            --  Remove trailing NL.
            while L > 0 and then (Line (L) = LF or Line (L) = CR) loop
               L := L - 1;
            end loop;

            if Line (1) = '>' then
               Dir_Len := L - 1;
               Dir (1 .. Dir_Len) := Line (2 .. L);
            else
               if To_Obj then
                  File := new String'(Dir (1 .. Dir_Len)
                                      & Get_Base_Name (Line (1 .. L))
                                      & Link_Obj_Suffix.all);
               else
                  File := new String'(Substitute (Line (1 .. L)));
               end if;

               Filelist.Increment_Last;
               Filelist.Table (Filelist.Last) := File;

               Dir_Len := 0;
            end if;
         end if;
      end loop;
      if fclose (Stream) /= 0 then
         Error ("cannot close " & Filename);
      end if;
   end Add_File_List;

   function Get_Object_Filename (File : Iir_Design_File) return String
   is
      Dir : Name_Id;
      Name : Name_Id;
   begin
      Dir := Get_Library_Directory (Get_Library (File));
      Name := Get_Design_File_Filename (File);
      return Image (Dir) & Get_Base_Name (Image (Name))
        & Get_Object_Suffix.all;
   end Get_Object_Filename;

   Last_Stamp : Time_Stamp_Id;
   Last_Stamp_File : Iir;

   function Is_File_Outdated (Design_File : Iir_Design_File) return Boolean
   is
      use Files_Map;

      Name : Name_Id;

      File : Source_File_Entry;
   begin
      --  Std.Standard is never outdated.
      if Design_File = Std_Package.Std_Standard_File then
         return False;
      end if;

      Name := Get_Design_File_Filename (Design_File);
      declare
         Obj_Pathname : String := Get_Object_Filename (Design_File) & Nul;
         Stamp : Time_Stamp_Id;
      begin
         Stamp := Get_File_Time_Stamp (Obj_Pathname'Address);

         --  If the object file does not exist, recompile the file.
         if Stamp = Null_Time_Stamp then
            if Flag_Verbose then
               Put_Line ("no object file for " & Image (Name));
            end if;
            return True;
         end if;

         --  Keep the time stamp of the most recently analyzed unit.
         if Last_Stamp = Null_Time_Stamp
           or else Is_Gt (Stamp, Last_Stamp)
         then
            Last_Stamp := Stamp;
            Last_Stamp_File := Design_File;
         end if;
      end;

      --  2) file has been modified.
      File := Load_Source_File (Get_Design_File_Directory (Design_File),
                                Get_Design_File_Filename (Design_File));
      if not Is_Eq (Get_File_Time_Stamp (File),
                    Get_File_Time_Stamp (Design_File))
      then
         if Flag_Verbose then
            Put_Line ("file " & Image (Get_File_Name (File))
                 & " has been modified");
         end if;
         return True;
      end if;

      return False;
   end Is_File_Outdated;

   function Is_Unit_Outdated (Unit : Iir_Design_Unit) return Boolean
   is
      Design_File : Iir_Design_File;
   begin
      --  Std.Standard is never outdated.
      if Unit = Std_Package.Std_Standard_Unit then
         return False;
      end if;

      Design_File := Get_Design_File (Unit);

      --  1) not yet analyzed:
      if Get_Date (Unit) not in Date_Valid then
         if Flag_Verbose then
            Disp_Library_Unit (Get_Library_Unit (Unit));
            Put_Line (" was not analyzed");
         end if;
         return True;
      end if;

      --  3) the object file does not exist.
      --  Already checked.

      --  4) one of the dependence is newer
      declare
         Depends : Iir_List;
         El : Iir;
         Dep : Iir_Design_Unit;
         Stamp : Time_Stamp_Id;
         Dep_File : Iir_Design_File;
      begin
         Depends := Get_Dependence_List (Unit);
         Stamp := Get_Analysis_Time_Stamp (Design_File);
         if Depends /= Null_Iir_List then
            for I in Natural loop
               El := Get_Nth_Element (Depends, I);
               exit when El = Null_Iir;
               Dep := Libraries.Find_Design_Unit (El);
               if Dep = Null_Iir then
                  if Flag_Verbose then
                     Disp_Library_Unit (Unit);
                     Put (" depends on an unknown unit ");
                     Disp_Library_Unit (El);
                     New_Line;
                  end if;
                  return True;
               end if;
               Dep_File := Get_Design_File (Dep);
               if Dep /= Std_Package.Std_Standard_Unit
                 and then Files_Map.Is_Gt (Get_Analysis_Time_Stamp (Dep_File),
                                           Stamp)
               then
                  if Flag_Verbose then
                     Disp_Library_Unit (Get_Library_Unit (Unit));
                     Put (" depends on: ");
                     Disp_Library_Unit (Get_Library_Unit (Dep));
                     Put (" (more recently analyzed)");
                     New_Line;
                  end if;
                  return True;
               end if;
            end loop;
         end if;
      end;

      return False;
   end Is_Unit_Outdated;

   procedure Add_Argument (Inst : in out Instance; Arg : String_Access)
   is
   begin
      Increment_Last (Inst);
      Inst.Table (Last (Inst)) := Arg;
   end Add_Argument;

   --  Convert option "-Wx,OPTIONS" to arguments for tool X.
   procedure Add_Arguments (Inst : in out Instance; Opt : String) is
   begin
      Add_Argument (Inst, new String'(Opt (Opt'First + 4 .. Opt'Last)));
   end Add_Arguments;

   procedure Tool_Not_Found (Name : String) is
   begin
      Error ("installation problem: " & Name & " not found");
      raise Option_Error;
   end Tool_Not_Found;

   procedure Set_Tools_Name
   is
   begin
      --  Set tools name.
      if Compiler_Cmd = null then
         case Compile_Kind is
            when Compile_Debug =>
               Compiler_Cmd := new String'(Default_Pathes.Compiler_Debug);
            when Compile_Gcc =>
               Compiler_Cmd := new String'(Default_Pathes.Compiler_Gcc);
            when Compile_Mcode =>
               Compiler_Cmd := new String'(Default_Pathes.Compiler_Mcode);
            when Compile_Llvm =>
               Compiler_Cmd := new String'(Default_Pathes.Compiler_Llvm);
         end case;
      end if;
      if Post_Processor_Cmd = null then
         Post_Processor_Cmd := new String'(Default_Pathes.Post_Processor);
      end if;
   end Set_Tools_Name;

   procedure Locate_Tools
   is
   begin
      Compiler_Path := Locate_Exec_On_Path (Compiler_Cmd.all);
      if Compiler_Path = null then
         Tool_Not_Found (Compiler_Cmd.all);
      end if;
      if Compile_Kind >= Compile_Debug then
         Post_Processor_Path := Locate_Exec_On_Path (Post_Processor_Cmd.all);
         if Post_Processor_Path = null then
            Tool_Not_Found (Post_Processor_Cmd.all);
         end if;
      end if;
      if Compile_Kind >= Compile_Gcc then
         Assembler_Path := Locate_Exec_On_Path (Assembler_Cmd);
         if Assembler_Path = null and not Flag_Asm then
            Tool_Not_Found (Assembler_Cmd);
         end if;
      end if;
      if Compile_Kind = Compile_Llvm then
         Linker_Path := Locate_Exec_On_Path (Llvm_Linker_Cmd);
         if Linker_Path = null then
            Tool_Not_Found (Llvm_Linker_Cmd);
         end if;
      else
         Linker_Path := Locate_Exec_On_Path (Linker_Cmd);
         if Linker_Path = null then
            Tool_Not_Found (Linker_Cmd);
         end if;
      end if;
      Dash_O := new String'("-o");
      Dash_Quiet := new String'("-quiet");
   end Locate_Tools;

   procedure Setup_Compiler (Load : Boolean)
   is
      use Libraries;
   begin
      Set_Tools_Name;
      Locate_Tools;
      Setup_Libraries (Load);
      for I in 2 .. Get_Nbr_Pathes loop
         Add_Argument (Compiler_Args,
                       new String'("-P" & Image (Get_Path (I))));
      end loop;
   end Setup_Compiler;

   type Command_Comp is abstract new Command_Lib with null record;

   --  Setup GHDL.
   procedure Init (Cmd : in out Command_Comp);

   --  Handle:
   --  all ghdl flags.
   --  some GCC flags.
   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   procedure Disp_Long_Help (Cmd : Command_Comp);

   procedure Init (Cmd : in out Command_Comp)
   is
   begin
      --  Init options.
      Flag_Not_Quiet := False;
      Flag_Disp_Commands := False;
      Flag_Asm := False;
      Flag_Expect_Failure := False;
      Output_File := null;

      --  Initialize argument tables.
      Init (Compiler_Args);
      Init (Postproc_Args);
      Init (Assembler_Args);
      Init (Linker_Args);
      Init (Command_Lib (Cmd));
   end Init;

   procedure Decode_Option (Cmd : in out Command_Comp;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      Str : String_Access;
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      Res := Option_Bad;
      if Opt = "-v" and then Flag_Verbose = False then
         --  Note: this is also decoded for command_lib, but we set
         --  Flag_Disp_Commands too.
         Flag_Verbose := True;
         --Flags.Verbose := True;
         Flag_Disp_Commands := True;
         Res := Option_Ok;
      elsif Opt'Length > 8 and then Opt (1 .. 8) = "--GHDL1=" then
         Compiler_Cmd := new String'(Opt (9 .. Opt'Last));
         Res := Option_Ok;
      elsif Opt = "-S" then
         Flag_Asm := True;
         Res := Option_Ok;
      elsif Opt = "--post" then
         Compile_Kind := Compile_Debug;
         Res := Option_Ok;
      elsif Opt = "--mcode" then
         Compile_Kind := Compile_Mcode;
         Res := Option_Ok;
      elsif Opt = "--llvm" then
         Compile_Kind := Compile_Llvm;
         Res := Option_Ok;
      elsif Opt = "-o" then
         if Arg'Length = 0 then
            Res := Option_Arg_Req;
         else
            Output_File := new String'(Arg);
            Res := Option_Arg;
         end if;
      elsif Opt = "-m32" then
         Add_Argument (Compiler_Args, new String'("-m32"));
         Add_Argument (Assembler_Args, new String'("--32"));
         Add_Argument (Linker_Args, new String'("-m32"));
         Decode_Option (Command_Lib (Cmd), Opt, Arg, Res);
      elsif Opt'Length > 4
        and then Opt (2) = 'W' and then Opt (4) = ','
      then
         if Opt (3) = 'c' then
            Add_Arguments (Compiler_Args, Opt);
         elsif Opt (3) = 'a' then
            Add_Arguments (Assembler_Args, Opt);
         elsif Opt (3) = 'p' then
            Add_Arguments (Postproc_Args, Opt);
         elsif Opt (3) = 'l' then
            Add_Arguments (Linker_Args, Opt);
         else
            Error
              ("unknown tool name in '-W" & Opt (3) & ",' option");
            raise Option_Error;
         end if;
         Res := Option_Ok;
      elsif Opt'Length >= 2 and then Opt (2) = 'g' then
         --  Debugging option.
         Str := new String'(Opt);
         Add_Argument (Compiler_Args, Str);
         Add_Argument (Linker_Args, Str);
         Res := Option_Ok;
      elsif Opt = "-Q" then
         Flag_Not_Quiet := True;
         Res := Option_Ok;
      elsif Opt = "--expect-failure" then
         Add_Argument (Compiler_Args, new String'(Opt));
         Flag_Expect_Failure := True;
         Res := Option_Ok;
      elsif Opt = "-C" then
         --  Translate -C into --mb-comments, as gcc already has a definition
         --  for -C.  Done before Flags.Parse_Option.
         Add_Argument (Compiler_Args, new String'("--mb-comments"));
         Res := Option_Ok;
      elsif Options.Parse_Option (Opt) then
         Add_Argument (Compiler_Args, new String'(Opt));
         Res := Option_Ok;
      elsif Opt'Length >= 2
        and then (Opt (2) = 'O' or Opt (2) = 'f')
      then
         --  Optimization option.
         --  This is put after Flags.Parse_Option, since it may catch -fxxx
         --  options.
         Add_Argument (Compiler_Args, new String'(Opt));
         Res := Option_Ok;
      else
         Decode_Option (Command_Lib (Cmd), Opt, Arg, Res);
      end if;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Comp) is
   begin
      Disp_Long_Help (Command_Lib (Cmd));
      Put_Line (" -v             Be verbose");
      Put_Line (" --GHDL1=PATH   Set the path of the ghdl1 compiler");
      Put_Line (" -S             Do not assemble");
      Put_Line (" -o FILE        Set the name of the output file");
   -- Put_Line (" -m32           Generate 32bit code on 64bit machines");
      Put_Line (" -WX,OPTION     Pass OPTION to X, where X is one of");
      Put_Line ("                 c: compiler, a: assembler, l: linker");
      Put_Line (" -g[XX]         Pass debugging option to the compiler");
      Put_Line (" -O[XX]/-f[XX]  Pass optimization option to the compiler");
      Put_Line (" -Q             Do not add -quiet option to compiler");
      Put_Line (" --expect-failure  Expect analysis/elaboration failure");
   end Disp_Long_Help;

   --  Command dispconfig.
   type Command_Dispconfig is new Command_Comp with null record;
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
      return Name = "--dispconfig";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Dispconfig) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--dispconfig       Disp tools path";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Dispconfig;
                             Args : Argument_List)
   is
      use Libraries;
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error ("--dispconfig does not accept any argument");
         raise Option_Error;
      end if;

      Set_Tools_Name;
      Put ("compiler command: ");
      Put_Line (Compiler_Cmd.all);
      if Compile_Kind >= Compile_Debug then
         Put ("post-processor command: ");
         Put_Line (Post_Processor_Cmd.all);
      end if;
      if Compile_Kind >= Compile_Gcc then
         Put ("assembler command: ");
         Put_Line (Assembler_Cmd);
      end if;
      Put ("linker command: ");
      Put_Line (Linker_Cmd);

      Put ("command line prefix (--PREFIX): ");
      if Prefix_Path = null then
         Put_Line ("(not set)");
      else
         Put_Line (Prefix_Path.all);
      end if;
      Setup_Libraries (False);

      Put ("environment prefix (GHDL_PREFIX): ");
      if Prefix_Env = null then
         Put_Line ("(not set)");
      else
         Put_Line (Prefix_Env.all);
      end if;

      Put_Line ("default prefix: " & Default_Pathes.Prefix);
      Put_Line ("actual prefix: " & Prefix_Path.all);

      Put ("library directory: ");
      Put_Line (Get_Machine_Path_Prefix);
      Locate_Tools;
      Put ("compiler path: ");
      Put_Line (Compiler_Path.all);
      if Compile_Kind >= Compile_Debug then
         Put ("post-processor path: ");
         Put_Line (Post_Processor_Path.all);
      end if;
      if Compile_Kind >= Compile_Gcc then
         Put ("assembler path: ");
         Put_Line (Assembler_Path.all);
      end if;
      Put ("linker path: ");
      Put_Line (Linker_Path.all);
      Put_Line ("default library pathes:");
      for I in 2 .. Get_Nbr_Pathes loop
         Put (' ');
         Put_Line (Image (Get_Path (I)));
      end loop;
   end Perform_Action;

   --  Command Analyze.
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
      Nil_Opt : Argument_List (2 .. 1);
   begin
      if Args'Length = 0 then
         Error ("no file to analyze");
         raise Option_Error;
      end if;
      Setup_Compiler (False);

      for I in Args'Range loop
         Do_Compile (Nil_Opt, Args (I).all);
      end loop;
   end Perform_Action;

   --  Elaboration.

   Base_Name : String_Access;
   Elab_Name : String_Access;
   Filelist_Name : String_Access;
   Unit_Name : String_Access;

   procedure Set_Elab_Units (Cmd_Name : String;
                             Args : Argument_List;
                             Run_Arg : out Natural)
   is
   begin
      Extract_Elab_Unit (Cmd_Name, Args, Run_Arg);
      if Sec_Name = null then
         Base_Name := Prim_Name;
         Unit_Name := Prim_Name;
      else
         Base_Name := new String'(Prim_Name.all & '-' & Sec_Name.all);
         Unit_Name := new String'(Prim_Name.all & '(' & Sec_Name.all & ')');
      end if;

      Elab_Name := new String'(Elab_Prefix & Base_Name.all);
      Filelist_Name := null;

      if Output_File = null then
         Output_File := new String'(Base_Name.all);
      end if;
   end Set_Elab_Units;

   procedure Set_Elab_Units (Cmd_Name : String; Args : Argument_List)
   is
      Next_Arg : Natural;
   begin
      Set_Elab_Units (Cmd_Name, Args, Next_Arg);
      if Next_Arg <= Args'Last then
         Error ("too many unit names for command '" & Cmd_Name & "'");
         raise Option_Error;
      end if;
   end Set_Elab_Units;

   procedure Bind
   is
      Comp_List : Argument_List (1 .. 4);
   begin
      Filelist_Name := new String'(Elab_Name.all & List_Suffix);

      Comp_List (1) := new String'("--elab");
      Comp_List (2) := Unit_Name;
      Comp_List (3) := new String'("-l");
      Comp_List (4) := Filelist_Name;
      Do_Compile (Comp_List, Elab_Name.all);
      Free (Comp_List (3));
      Free (Comp_List (1));
   end Bind;

   procedure Bind_Anaelab (Files : Argument_List)
   is
      Comp_List : Argument_List (1 .. Files'Length + 2);
      Index : Natural;
   begin
      Comp_List (1) := new String'("--anaelab");
      Comp_List (2) := Unit_Name;
      Index := 3;
      for I in Files'Range loop
         Comp_List (Index) := new String'("--ghdl-source=" & Files (I).all);
         Index := Index + 1;
      end loop;
      Do_Compile (Comp_List, Elab_Name.all);
      Free (Comp_List (1));
      for I in 3 .. Comp_List'Last loop
         Free (Comp_List (I));
      end loop;
   end Bind_Anaelab;

   procedure Link (Add_Std : Boolean;
                   Disp_Only : Boolean)
   is
      Last_File : Natural;
      Final_Output_File : String_Access;
   begin
      case Compile_Kind is
         when Compile_Llvm =>
            Link_Obj_Suffix := new String'(Llvm_Suffix);
            --  Hacks for llvm:
            --  1. Generate a native executable.
            Add_Argument (Linker_Args, new String'("-native"));
            --  2. Use an intermediate file.
            Final_Output_File := Output_File;
            Output_File := new String'(Output_File.all & "~e");
         when others =>
            Link_Obj_Suffix := Get_Object_Suffix;
      end case;

      --  read files list
      if Filelist_Name /= null then
         Add_File_List (Filelist_Name.all, True);
      end if;
      Last_File := Filelist.Last;
      Add_File_List (Get_Machine_Path_Prefix & "grt" & List_Suffix, False);

      --  call the linker
      declare
         P : Natural;
         Nbr_Args : constant Natural := Last (Linker_Args) + Filelist.Last + 4;
         Args : Argument_List (1 .. Nbr_Args);
         Obj_File : String_Access;
         Std_File : String_Access;
         Status : Boolean;
      begin
         Obj_File := Append_Suffix (Elab_Name.all, Link_Obj_Suffix.all);
         P := 0;
         Args (P + 1) := Dash_O;
         Args (P + 2) := Output_File;
         Args (P + 3) := Obj_File;
         P := P + 3;
         if Add_Std then
            Std_File := new
              String'(Get_Machine_Path_Prefix
                      & Get_Version_Path & Directory_Separator
                      & "std" & Directory_Separator
                      & "std_standard" & Link_Obj_Suffix.all);
            P := P + 1;
            Args (P) := Std_File;
         else
            Std_File := null;
         end if;

         --  Object files of the design.
         for I in Filelist.First .. Last_File loop
            P := P + 1;
            Args (P) := Filelist.Table (I);
         end loop;
         --  User added options.
         for I in First .. Last (Linker_Args) loop
            P := P + 1;
            Args (P) := Linker_Args.Table (I);
         end loop;
         --  GRT files (should be the last one, since it contains an
         --  optional main).
         for I in Last_File + 1 .. Filelist.Last loop
            P := P + 1;
            Args (P) := Filelist.Table (I);
         end loop;

         if Disp_Only then
            for I in 3 .. P loop
               Put_Line (Args (I).all);
            end loop;
         else
            My_Spawn (Linker_Path.all, Args (1 .. P));
            if Compile_Kind = Compile_Llvm then
               Rename_File (Output_File.all, Final_Output_File.all, Status);
               if not Status then
                  raise Compile_Error;
               end if;
               Free (Output_File);
               Output_File := Final_Output_File;
            end if;
         end if;

         Free (Obj_File);
         Free (Std_File);
      end;

      for I in Filelist.First .. Filelist.Last loop
         Free (Filelist.Table (I));
      end loop;
   end Link;

   --  Command Elab.
   type Command_Elab is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Elab; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Elab) return String;
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
      return "-e [OPTS] UNIT [ARCH]      Elaborate UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Elab; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Set_Elab_Units ("-e", Args);
      Setup_Compiler (False);

      Bind;
      if not Flag_Expect_Failure then
         Link (Add_Std => True, Disp_Only => False);
      end if;
      Delete_File (Filelist_Name.all, Success);
   end Perform_Action;

   --  Command Run.
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
      return Name = "-r";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Run) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-r UNIT [ARCH] [OPTS]      Run UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Run; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Opt_Arg : Natural;
   begin
      Extract_Elab_Unit ("-r", Args, Opt_Arg);
      if Sec_Name = null then
         Base_Name := Prim_Name;
      else
         Base_Name := new String'(Prim_Name.all & '-' & Sec_Name.all);
      end if;
      if not Is_Regular_File (Base_Name.all & Nul) then
         Error ("file '" & Base_Name.all & "' does not exists");
         Error ("Please elaborate your design.");
         raise Exec_Error;
      end if;
      My_Spawn ('.' & Directory_Separator & Base_Name.all,
                Args (Opt_Arg .. Args'Last));
   end Perform_Action;

   --  Command Elab_Run.
   type Command_Elab_Run is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Elab_Run; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Elab_Run) return String;
   procedure Perform_Action (Cmd : in out Command_Elab_Run;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Elab_Run; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--elab-run";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Elab_Run) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--elab-run [OPTS] UNIT [ARCH] [OPTS]  Elaborate and run UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Elab_Run;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Success : Boolean;
      Run_Arg : Natural;
   begin
      Set_Elab_Units ("-elab-run", Args, Run_Arg);
      Setup_Compiler (False);

      Bind;
      if Flag_Expect_Failure then
         Delete_File (Filelist_Name.all, Success);
      else
         Link (Add_Std => True, Disp_Only => False);
         Delete_File (Filelist_Name.all, Success);
         My_Spawn ('.' & Directory_Separator & Output_File.all,
                   Args (Run_Arg .. Args'Last));
      end if;
   end Perform_Action;

   --  Command Bind.
   type Command_Bind is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Bind; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Bind) return String;
   procedure Perform_Action (Cmd : in out Command_Bind;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Bind; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--bind";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Bind) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--bind [OPTS] UNIT [ARCH]  Bind UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Bind; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      Set_Elab_Units ("--bind", Args);
      Setup_Compiler (False);

      Bind;
   end Perform_Action;

   --  Command Link.
   type Command_Link is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Link; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Link) return String;
   procedure Perform_Action (Cmd : in out Command_Link; Args : Argument_List);

   function Decode_Command (Cmd : Command_Link; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--link";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Link) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--link [OPTS] UNIT [ARCH]  Link UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Link; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      Set_Elab_Units ("--link", Args);
      Setup_Compiler (False);

      Filelist_Name := new String'(Elab_Name.all & List_Suffix);
      Link (Add_Std => True, Disp_Only => False);
   end Perform_Action;


   --  Command List_Link.
   type Command_List_Link is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_List_Link; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_List_Link) return String;
   procedure Perform_Action (Cmd : in out Command_List_Link;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_List_Link; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--list-link";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_List_Link) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--list-link [OPTS] UNIT [ARCH]  List objects file to link UNIT";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_List_Link;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      Set_Elab_Units ("--list-link", Args);
      Setup_Compiler (False);

      Filelist_Name := new String'(Elab_Name.all & List_Suffix);
      Link (Add_Std => True, Disp_Only => True);
   end Perform_Action;


   --  Command analyze and elaborate
   type Command_Anaelab is new Command_Comp with null record;
   function Decode_Command (Cmd : Command_Anaelab; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Anaelab) return String;
   procedure Decode_Option (Cmd : in out Command_Anaelab;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   procedure Perform_Action (Cmd : in out Command_Anaelab;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Anaelab; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-c";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Anaelab) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-c [OPTS] FILEs -e UNIT [ARCH]  "
        & "Generate whole code to elab UNIT from FILEs";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Anaelab;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
   begin
      if Option = "-e" then
         Res := Option_End;
         return;
      else
         Decode_Option (Command_Comp (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Anaelab;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      Elab_Index : Integer;
   begin
      Elab_Index := -1;
      for I in Args'Range loop
         if Args (I).all = "-e" then
            Elab_Index := I;
            exit;
         end if;
      end loop;
      if Elab_Index < 0 then
         Analyze_Files (Args, True);
      else
         Flags.Flag_Whole_Analyze := True;
         Set_Elab_Units ("-c", Args (Elab_Index + 1 .. Args'Last));
         Setup_Compiler (False);

         Bind_Anaelab (Args (Args'First .. Elab_Index - 1));
         Link (Add_Std => False, Disp_Only => False);
      end if;
   end Perform_Action;

   --  Command Make.
   type Command_Make is new Command_Comp with record
      --  Disp dependences during make.
      Flag_Depend_Unit : Boolean;

      --  Force recompilation of units in work library.
      Flag_Force : Boolean;
   end record;

   function Decode_Command (Cmd : Command_Make; Name : String)
                           return Boolean;
   procedure Init (Cmd : in out Command_Make);
   procedure Decode_Option (Cmd : in out Command_Make;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   function Get_Short_Help (Cmd : Command_Make) return String;
   procedure Disp_Long_Help (Cmd : Command_Make);

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

   procedure Disp_Long_Help (Cmd : Command_Make)
   is
   begin
      Disp_Long_Help (Command_Comp (Cmd));
      Put_Line (" -f             Force recompilation of work units");
      Put_Line (" -Mu            Disp unit dependences (humna format)");
   end Disp_Long_Help;

   procedure Init (Cmd : in out Command_Make) is
   begin
      Init (Command_Comp (Cmd));
      Cmd.Flag_Depend_Unit := False;
      Cmd.Flag_Force := False;
   end Init;


   procedure Decode_Option (Cmd : in out Command_Make;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
   begin
      if Option = "-Mu" then
         Cmd.Flag_Depend_Unit := True;
         Res := Option_Ok;
      elsif Option = "-f" then
         Cmd.Flag_Force := True;
         Res := Option_Ok;
      else
         Decode_Option (Command_Comp (Cmd), Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Make; Args : Argument_List)
   is
      use Configuration;

      File : Iir_Design_File;
      Unit : Iir;
      Lib_Unit : Iir;
      Lib : Iir_Library_Declaration;
      In_Work : Boolean;

      Files_List : Iir_List;

      --  Set when a design file has been compiled.
      Has_Compiled : Boolean;

      Need_Analyze : Boolean;

      Need_Elaboration : Boolean;

      Stamp : Time_Stamp_Id;
      File_Id : Name_Id;

      Nil_Args : Argument_List (2 .. 1);
      Success : Boolean;
   begin
      Set_Elab_Units ("-m", Args);
      Setup_Compiler (True);

      --  Create list of files.
      Files_List := Build_Dependence (Prim_Name, Sec_Name);

      if Cmd.Flag_Depend_Unit then
         Put_Line ("Units analysis order:");
         for I in Design_Units.First .. Design_Units.Last loop
            Unit := Design_Units.Table (I);
            Put ("  ");
            Disp_Library_Unit (Get_Library_Unit (Unit));
            New_Line;
--             Put (" file: ");
--             File := Get_Design_File (Unit);
--             Image (Get_Design_File_Filename (File));
--             Put_Line (Name_Buffer (1 .. Name_Length));
         end loop;
      end if;
      if Cmd.Flag_Depend_Unit then
         Put_Line ("File analysis order:");
         for I in Natural loop
            File := Get_Nth_Element (Files_List, I);
            exit when File = Null_Iir;
            Image (Get_Design_File_Filename (File));
            Put ("  ");
            Put (Name_Buffer (1 .. Name_Length));
            if Flag_Verbose then
               Put_Line (":");
               declare
                  Dep_List : Iir_List;
                  Dep_File : Iir;
               begin
                  Dep_List := Get_File_Dependence_List (File);
                  if Dep_List /= Null_Iir_List then
                     for J in Natural loop
                        Dep_File := Get_Nth_Element (Dep_List, J);
                        exit when Dep_File = Null_Iir;
                        Image (Get_Design_File_Filename (Dep_File));
                        Put ("    ");
                        Put_Line (Name_Buffer (1 .. Name_Length));
                     end loop;
                  end if;
               end;
            else
               New_Line;
            end if;
         end loop;
      end if;

      Has_Compiled := False;
      Last_Stamp := Null_Time_Stamp;

      for I in Natural loop
         File := Get_Nth_Element (Files_List, I);
         exit when File = Null_Iir;

         Need_Analyze := False;
         if Is_File_Outdated (File) then
            Need_Analyze := True;
         else
            Unit := Get_First_Design_Unit (File);
            while Unit /= Null_Iir loop
               Lib_Unit := Get_Library_Unit (Unit);
               if not (Get_Kind (Lib_Unit) = Iir_Kind_Configuration_Declaration
                       and then Get_Identifier (Lib_Unit) = Null_Identifier)
               then
                  if Is_Unit_Outdated (Unit) then
                     Need_Analyze := True;
                     exit;
                  end if;
               end if;
               Unit := Get_Chain (Unit);
            end loop;
         end if;

         Lib := Get_Library (File);
         In_Work := Lib = Libraries.Work_Library;

         if Need_Analyze or else (Cmd.Flag_Force and In_Work) then
            File_Id := Get_Design_File_Filename (File);
            if not Flag_Verbose then
               Put ("analyze ");
               Put (Image (File_Id));
               --Disp_Library_Unit (Get_Library_Unit (Unit));
               New_Line;
            end if;

            if In_Work then
               Do_Compile (Nil_Args, Image (File_Id));
            else
               declare
                  use Libraries;
                  Lib_Args : Argument_List (1 .. 2);
                  Prev_Workdir : Name_Id;
               begin
                  Prev_Workdir := Work_Directory;

                  --  Must be set, since used to build the object filename.
                  Work_Directory := Get_Library_Directory (Lib);

                  --  Always overwrite --work and --workdir.
                  Lib_Args (1) := new String'
                    ("--work=" & Image (Get_Identifier (Lib)));
                  if Work_Directory = Libraries.Local_Directory then
                     Lib_Args (2) := new String'("--workdir=.");
                  else
                     Lib_Args (2) := new String'
                       ("--workdir=" & Image (Work_Directory));
                  end if;
                  Do_Compile (Lib_Args, Image (File_Id));

                  Work_Directory := Prev_Workdir;

                  Free (Lib_Args (1));
                  Free (Lib_Args (2));
               end;
            end if;

            Has_Compiled := True;
            --  Set the analysis time stamp since the file has just been
            --  analyzed.
            Set_Analysis_Time_Stamp (File, Files_Map.Get_Os_Time_Stamp);
         end if;
      end loop;

      Need_Elaboration := False;
      --  Elaboration.
      --  if libgrt is more recent than the executable (FIXME).
      if Has_Compiled then
         if Flag_Verbose then
            Put_Line ("link due to a file compilation");
         end if;
         Need_Elaboration := True;
      else
         declare
            Exec_File : String := Output_File.all & Nul;
         begin
            Stamp := Files_Map.Get_File_Time_Stamp (Exec_File'Address);
         end;

         if Stamp = Null_Time_Stamp then
            if Flag_Verbose then
               Put_Line ("link due to no binary file");
            end if;
            Need_Elaboration := True;
         else
            if Files_Map.Is_Gt (Last_Stamp, Stamp) then
               --  if a file is more recent than the executable.
               if Flag_Verbose then
                  Put ("link due to outdated binary file: ");
                  Put (Image (Get_Design_File_Filename (Last_Stamp_File)));
                  Put (" (");
                  Put (Files_Map.Get_Time_Stamp_String (Last_Stamp));
                  Put (" > ");
                  Put (Files_Map.Get_Time_Stamp_String (Stamp));
                  Put (")");
                  New_Line;
               end if;
               Need_Elaboration := True;
            end if;
         end if;
      end if;
      if Need_Elaboration then
         if not Flag_Verbose then
            Put ("elaborate ");
            Put (Prim_Name.all);
            --Disp_Library_Unit (Get_Library_Unit (Unit));
            New_Line;
         end if;
         Bind;
         Link (Add_Std => True, Disp_Only => False);
         Delete_File (Filelist_Name.all, Success);
      end if;
   end Perform_Action;

   --  Command Gen_Makefile.
   type Command_Gen_Makefile is new Command_Comp with null record;
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

      HT : constant Character := Ada.Characters.Latin_1.HT;
      Files_List : Iir_List;
      File : Iir_Design_File;

      Lib : Iir_Library_Declaration;
      Dir_Id : Name_Id;

      Dep_List : Iir_List;
      Dep_File : Iir;
   begin
      Set_Elab_Units ("--gen-makefile", Args);
      Setup_Libraries (True);
      Files_List := Build_Dependence (Prim_Name, Sec_Name);

      Put_Line ("# Makefile automatically generated by ghdl");
      Put ("# Version: ");
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
               then
                  Put (" ");
                  Put (Arg);
               end if;
            end if;
         end;
      end loop;
      New_Line;

      New_Line;

      Put_Line ("# Default target");
      Put ("all: ");
      Put_Line (Base_Name.all);
      New_Line;

      Put_Line ("# Elaboration target");
      Put (Base_Name.all);
      Put (":");
      for I in Natural loop
         File := Get_Nth_Element (Files_List, I);
         exit when File = Null_Iir;
         if Is_Makeable_File (File) then
            Put (" ");
            Put (Get_Object_Filename (File));
         end if;
      end loop;
      New_Line;
      Put_Line (HT & "$(GHDL) -e $(GHDLFLAGS) $@");
      New_Line;

      Put_Line ("# Run target");
      Put_Line ("run: " & Base_Name.all);
      Put_Line (HT & "$(GHDL) -r " & Base_Name.all & " $(GHDLRUNFLAGS)");
      New_Line;

      Put_Line ("# Targets to analyze files");
      for I in Natural loop
         File := Get_Nth_Element (Files_List, I);
         exit when File = Null_Iir;
         Dir_Id := Get_Design_File_Directory (File);
         if not Is_Makeable_File (File) then
            --  Builtin file.
            null;
         else
            Put (Get_Object_Filename (File));
            Put (": ");
            if Dir_Id /= Files_Map.Get_Home_Directory then
               Put (Image (Dir_Id));
               Put (Image (Get_Design_File_Filename (File)));
               New_Line;

               Put_Line
                 (HT & "@echo ""This file was not locally built ($<)""");
               Put_Line (HT & "exit 1");
            else
               Put (Image (Get_Design_File_Filename (File)));
               New_Line;

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
               Put_Line (" $<");
            end if;
         end if;
      end loop;
      New_Line;

      Put_Line ("# Files dependences");
      for I in Natural loop
         File := Get_Nth_Element (Files_List, I);
         exit when File = Null_Iir;
         if Is_Makeable_File (File) then
            Put (Get_Object_Filename (File));
            Put (": ");
            Dep_List := Get_File_Dependence_List (File);
            if Dep_List /= Null_Iir_List then
               for J in Natural loop
                  Dep_File := Get_Nth_Element (Dep_List, J);
                  exit when Dep_File = Null_Iir;
                  if Dep_File /= File and then Is_Makeable_File (Dep_File)
                  then
                     Put (" ");
                     Put (Get_Object_Filename (Dep_File));
                  end if;
                  end loop;
            end if;
            New_Line;
         end if;
      end loop;
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Analyze);
      Register_Command (new Command_Elab);
      Register_Command (new Command_Run);
      Register_Command (new Command_Elab_Run);
      Register_Command (new Command_Bind);
      Register_Command (new Command_Link);
      Register_Command (new Command_List_Link);
      Register_Command (new Command_Anaelab);
      Register_Command (new Command_Make);
      Register_Command (new Command_Gen_Makefile);
      Register_Command (new Command_Dispconfig);
   end Register_Commands;
end Ghdldrv;
