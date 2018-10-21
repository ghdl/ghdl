--  GHDL driver - local commands.
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
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Directory_Operations;
with Types; use Types;
with Libraries;
with Std_Package;
with Flags;
with Name_Table;
with Std_Names;
with Disp_Vhdl;
with Default_Paths;
with Scanner;
with Errorout;
with Configuration;
with Files_Map;
with Options;
with Iirs_Utils; use Iirs_Utils;

package body Ghdllocal is
   --  Version of the IEEE library to use.  This just change paths.
   type Ieee_Lib_Kind is (Lib_Standard, Lib_None, Lib_Synopsys, Lib_Mentor);
   Flag_Ieee : Ieee_Lib_Kind;

   --  If TRUE, generate 32bits code on 64bits machines.
   Flag_32bit : Boolean := False;

   procedure Compile_Init is
   begin
      Options.Initialize;
      Flag_Ieee := Lib_Standard;
      Flag_Verbose := False;
   end Compile_Init;

   procedure Init (Cmd : in out Command_Lib)
   is
      pragma Unreferenced (Cmd);
   begin
      Compile_Init;
   end Init;

   function Decode_Driver_Option (Option : String) return Boolean
   is
      subtype Opt_String is String (1 .. Option'Length);
      Opt : Opt_String renames Option;
   begin
      if Opt = "-v" and then Flag_Verbose = False then
         Flag_Verbose := True;
         return True;
      elsif Opt'Length > 9 and then Opt (1 .. 9) = "--PREFIX=" then
         Switch_Prefix_Path := new String'(Opt (10 .. Opt'Last));
         return True;
      elsif Opt = "--ieee=synopsys" then
         Flag_Ieee := Lib_Synopsys;
         return True;
      elsif Opt = "--ieee=mentor" then
         Flag_Ieee := Lib_Mentor;
         return True;
      elsif Opt = "--ieee=none" then
         Flag_Ieee := Lib_None;
         return True;
      elsif Opt = "--ieee=standard" then
         Flag_Ieee := Lib_Standard;
         return True;
      elsif Opt = "-m32" then
         Flag_32bit := True;
         return True;
      elsif Opt'Length >= 2
        and then (Opt (2) = 'g' or Opt (2) = 'O')
      then
         --  Silently accept -g and -O.
         return True;
      else
         return Options.Parse_Option (Opt);
      end if;
   end Decode_Driver_Option;

   procedure Decode_Option (Cmd : in out Command_Lib;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      pragma Unreferenced (Cmd);
      pragma Unreferenced (Arg);
   begin
      if Decode_Driver_Option (Option) then
         Res := Option_Ok;
      else
         Res := Option_Bad;
      end if;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Lib)
   is
      pragma Unreferenced (Cmd);
      procedure P (Str : String) renames Put_Line;
   begin
      P ("Main options (try --options-help for details):");
      P (" --std=XX       Use XX as VHDL standard (87,93c,93,00,02 or 08)");
      P (" --work=NAME    Set the name of the WORK library");
      P (" -PDIR          Add DIR in the library search path");
      P (" --workdir=DIR  Specify the directory of the WORK library");
      P (" --PREFIX=DIR   Specify installation prefix");
      P (" --ieee=NAME    Use NAME as ieee library, where name is:");
      P ("    standard: standard version (default)");
      P ("    synopsys, mentor: vendor version (not advised)");
      P ("    none: do not use a predefined ieee library");
   end Disp_Long_Help;

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      return C = '/' or else C = Directory_Separator;
   end Is_Directory_Separator;

   function Get_Basename_Pos (Pathname : String) return Natural is
   begin
      for I in reverse Pathname'Range loop
         if Is_Directory_Separator (Pathname (I)) then
            return I;
         end if;
      end loop;
      return Pathname'First - 1;
   end Get_Basename_Pos;

   function Is_Basename (Pathname : String) return Boolean is
   begin
      return Get_Basename_Pos (Pathname) < Pathname'First;
   end Is_Basename;

   --  Simple lower case conversion, used to compare with "bin".
   function To_Lower (S : String) return String
   is
      Res : String (S'Range);
      C : Character;
   begin
      for I in S'Range loop
         C := S (I);
         if C >= 'A' and then C <= 'Z' then
            C := Character'Val
              (Character'Pos (C) - Character'Pos ('A') + Character'Pos ('a'));
         end if;
         Res (I) := C;
      end loop;
      return Res;
   end To_Lower;

   procedure Set_Prefix_From_Program_Path (Prog_Path : String)
   is
      Last : Natural;
   begin
      Last := Get_Basename_Pos (Prog_Path);
      if Last < Prog_Path'First then
         --  No directory in Prog_Path.  This is not expected.
         return;
      end if;

      declare
         Pathname : String :=
           Normalize_Pathname (Prog_Path (Last + 1 .. Prog_Path'Last),
                               Prog_Path (Prog_Path'First .. Last - 1));
         Pos : Natural;
      begin
         --  Stop now in case of error.
         if Pathname'Length = 0 then
            return;
         end if;

         --  Skip executable name
         Last := Get_Basename_Pos (Pathname);
         if Last < Pathname'First then
            return;
         end if;

         --  Simplify path:
         --    /./ => /
         --    // => /
         Pos := Last - 1;
         while Pos >= Pathname'First loop
            if Is_Directory_Separator (Pathname (Pos)) then
               if Is_Directory_Separator (Pathname (Pos + 1)) then
                  --  // => /
                  Pathname (Pos .. Last - 1) := Pathname (Pos + 1 .. Last);
                  Last := Last - 1;
               elsif Pos + 2 <= Last
                 and then Pathname (Pos + 1) = '.'
                 and then Is_Directory_Separator (Pathname (Pos + 2))
               then
                  --  /./ => /
                  Pathname (Pos .. Last - 2) := Pathname (Pos + 2 .. Last);
                  Last := Last - 2;
               end if;
            end if;
            Pos := Pos - 1;
         end loop;

         --  Simplify path:
         --    /xxx/../ => /
         --  Do it forward as xxx/../../ must not be simplified as xxx/
         --  This is done after the previous simplification to avoid to deal
         --  with cases like /xxx//../ or /xxx/./../
         Pos := Pathname'First;
         while Pos <= Last - 3 loop
            if Is_Directory_Separator (Pathname (Pos))
              and then Pathname (Pos + 1) = '.'
              and then Pathname (Pos + 2) = '.'
              and then Is_Directory_Separator (Pathname (Pos + 3))
            then
               declare
                  Last_Dir : Natural;
                  Len : Natural;
               begin
                  --  Search backward
                  Last_Dir := Pos;
                  loop
                     if Last_Dir = Pathname'First then
                        Last_Dir := Pos;
                        exit;
                     end if;
                     Last_Dir := Last_Dir - 1;
                     exit when Is_Directory_Separator (Pathname (Last_Dir));
                  end loop;

                  --  /xxxxxxxxxx/../
                  --  ^          ^
                  --  Last_Dir   Pos
                  Len := Pos + 3 - Last_Dir;
                  Pathname (Last_Dir + 1 .. Last - Len) :=
                    Pathname (Pos + 4 .. Last);
                  Last := Last - Len;
                  Pos := Last_Dir;
               end;
            else
               Pos := Pos + 1;
            end if;
         end loop;

         --  Remove last '/'
         Last := Last - 1;

         --  Skip '/bin' directory if present
         Pos := Get_Basename_Pos (Pathname (Pathname'First .. Last));
         if Pos < Pathname'First then
            return;
         end if;
         if To_Lower (Pathname (Pos + 1 .. Last)) = "bin" then
            Last := Pos - 1;
         end if;

         Exec_Prefix := new String'(Pathname (Pathname'First .. Last));
      end;
   end Set_Prefix_From_Program_Path;

   --  Extract Exec_Prefix from executable name.
   procedure Set_Exec_Prefix
   is
      use GNAT.Directory_Operations;
      Prog_Path : constant String := Ada.Command_Line.Command_Name;
      Exec_Path : String_Access;
   begin
      --  If the command name is an absolute path, deduce prefix from it.
      if Is_Absolute_Path (Prog_Path) then
         Set_Prefix_From_Program_Path (Prog_Path);
         return;
      end if;

      --  If the command name is a relative path, deduce prefix from it
      --  and current path.
      if not Is_Basename (Prog_Path) then
         if Is_Executable_File (Prog_Path) then
            Set_Prefix_From_Program_Path
              (Get_Current_Dir & Directory_Separator & Prog_Path);
         end if;
         return;
      end if;

      --  Look for program name on the path.
      Exec_Path := Locate_Exec_On_Path (Prog_Path);
      if Exec_Path /= null then
         Set_Prefix_From_Program_Path (Exec_Path.all);
         Free (Exec_Path);
      end if;
   end Set_Exec_Prefix;

   function Get_Version_Path return String
   is
      use Flags;
   begin
      case Vhdl_Std is
         when Vhdl_87 =>
            return "v87";
         when Vhdl_93c
           | Vhdl_93
           | Vhdl_00
           | Vhdl_02 =>
            return "v93";
         when Vhdl_08 =>
            return "v08";
      end case;
   end Get_Version_Path;

   function Get_Machine_Path_Prefix return String is
   begin
      if Flag_32bit then
         return Lib_Prefix_Path.all & "32";
      else
         return Lib_Prefix_Path.all;
      end if;
   end Get_Machine_Path_Prefix;

   procedure Add_Library_Name (Name : String)
   is
      Path : constant String := Get_Machine_Path_Prefix & Directory_Separator
        & Name & Directory_Separator & Get_Version_Path & Directory_Separator;
   begin
      if not Is_Directory (Path) then
         Warning
           ("library " & Name & " does not exists for " & Get_Version_Path);
      end if;
      Libraries.Add_Library_Path (Path);
   end Add_Library_Name;

   procedure Setup_Libraries (Load : Boolean)
   is
      use Flags;
   begin
      --  Get environment variable.
      Prefix_Env := GNAT.OS_Lib.Getenv ("GHDL_PREFIX");
      if Prefix_Env = null or else Prefix_Env.all = "" then
         Prefix_Env := null;
      end if;

      --  Compute Exec_Prefix.
      Set_Exec_Prefix;

      --  Set prefix path.
      --  If not set by command line, try environment variable.
      if Switch_Prefix_Path /= null then
         Lib_Prefix_Path := Switch_Prefix_Path;
      else
         Lib_Prefix_Path := Prefix_Env;
      end if;
      --  Else try default path.
      if Lib_Prefix_Path = null then
         if Is_Absolute_Path (Default_Paths.Lib_Prefix) then
            Lib_Prefix_Path := new String'(Default_Paths.Lib_Prefix);
         else
            if Exec_Prefix /= null then
               Lib_Prefix_Path := new
                 String'(Exec_Prefix.all & Directory_Separator
                           & Default_Paths.Lib_Prefix);
            end if;
            if Lib_Prefix_Path = null
              or else not Is_Directory (Lib_Prefix_Path.all)
            then
               Free (Lib_Prefix_Path);
               Lib_Prefix_Path := new
                 String'(Default_Paths.Install_Prefix
                           & Directory_Separator
                           & Default_Paths.Lib_Prefix);
            end if;
         end if;
      else
         -- Assume the user has set the correct path, so do not insert 32.
         Flag_32bit := False;
      end if;

      --  Add paths for predefined libraries.
      if not Flags.Bootstrap then
         case Flag_Ieee is
            when Lib_Standard =>
               Add_Library_Name ("ieee");
            when Lib_Synopsys =>
               Add_Library_Name ("synopsys");
            when Lib_Mentor =>
               if Vhdl_Std >= Vhdl_08 then
                  Warning ("--ieee=mentor is ignored for --std=08");
               else
                  Add_Library_Name ("mentor");
               end if;
            when Lib_None =>
               null;
         end case;

         --  For std: just add the library prefix.
         Libraries.Add_Library_Path
           (Get_Machine_Path_Prefix & Directory_Separator);
      end if;
      if Load then
         Libraries.Load_Std_Library;
         Libraries.Load_Work_Library;
      end if;
   end Setup_Libraries;

   procedure Disp_Config_Prefixes is
   begin
      Put ("command line prefix (--PREFIX): ");
      if Switch_Prefix_Path = null then
         Put_Line ("(not set)");
      else
         Put_Line (Switch_Prefix_Path.all);
      end if;

      Put ("environment prefix (GHDL_PREFIX): ");
      if Prefix_Env = null then
         Put_Line ("(not set)");
      else
         Put_Line (Prefix_Env.all);
      end if;

      Setup_Libraries (False);

      Put ("exec prefix (from program name): ");
      if Exec_Prefix = null then
         Put_Line ("(not found)");
      else
         Put_Line (Exec_Prefix.all);
      end if;

      New_Line;

      Put_Line ("library prefix: " & Lib_Prefix_Path.all);
      Put ("library directory: ");
      Put_Line (Get_Machine_Path_Prefix);
   end Disp_Config_Prefixes;

   procedure Disp_Library_Unit (Unit : Iir)
   is
      use Name_Table;
      Id : Name_Id;
   begin
      Id := Get_Identifier (Unit);
      case Get_Kind (Unit) is
         when Iir_Kind_Entity_Declaration =>
            Put ("entity ");
         when Iir_Kind_Architecture_Body =>
            Put ("architecture ");
         when Iir_Kind_Configuration_Declaration =>
            Put ("configuration ");
         when Iir_Kind_Package_Declaration =>
            Put ("package ");
         when Iir_Kind_Package_Instantiation_Declaration =>
            Put ("package instance ");
         when Iir_Kind_Package_Body =>
            Put ("package body ");
         when Iir_Kind_Context_Declaration =>
            Put ("context ");
         when others =>
            Put ("???");
            return;
      end case;
      Put (Image (Id));
      case Get_Kind (Unit) is
         when Iir_Kind_Architecture_Body =>
            Put (" of ");
            Put (Image (Get_Entity_Identifier_Of_Architecture (Unit)));
         when Iir_Kind_Configuration_Declaration =>
            if Id = Null_Identifier then
               Put ("<default> of entity ");
               Put (Image (Get_Entity_Identifier_Of_Architecture (Unit)));
            end if;
         when others =>
            null;
      end case;
   end Disp_Library_Unit;

   procedure Disp_Library (Name : Name_Id)
   is
      use Libraries;
      Lib : Iir_Library_Declaration;
      File : Iir_Design_File;
      Unit : Iir;
   begin
      if Name = Std_Names.Name_Work then
         Lib := Work_Library;
      elsif Name = Std_Names.Name_Std then
         Lib := Std_Library;
      else
         Lib := Get_Library (Name, Command_Line_Location);
      end if;

      --  Disp contents of files.
      File := Get_Design_File_Chain (Lib);
      while File /= Null_Iir loop
         Unit := Get_First_Design_Unit (File);
         while Unit /= Null_Iir loop
            Disp_Library_Unit (Get_Library_Unit (Unit));
            New_Line;
            Unit := Get_Chain (Unit);
         end loop;
         File := Get_Chain (File);
      end loop;
   end Disp_Library;

   --  Return FILENAME without the extension.
   function Get_Base_Name (Filename : String; Remove_Dir : Boolean := True)
                           return String
   is
      First : Natural;
      Last : Natural;
   begin
      First := Filename'First;
      Last := Filename'Last;
      for I in Filename'Range loop
         if Filename (I) = '.' then
            Last := I - 1;
         elsif Remove_Dir and then Is_Directory_Separator (Filename (I)) then
            First := I + 1;
            Last := Filename'Last;
         end if;
      end loop;
      return Filename (First .. Last);
   end Get_Base_Name;

   function Append_Suffix
     (File : String; Suffix : String; In_Work : Boolean := True)
     return String_Access
   is
      use Name_Table;
   begin
      if In_Work then
         return new String'(Image (Libraries.Work_Directory)
                              & Get_Base_Name (File) & Suffix);
      else
         return new String'(File & Suffix);
      end if;
   end Append_Suffix;


   --  Command Dir.
   type Command_Dir is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Dir; Name : String) return Boolean;
   function Get_Short_Help (Cmd : Command_Dir) return String;
   procedure Perform_Action (Cmd : in out Command_Dir; Args : Argument_List);

   function Decode_Command (Cmd : Command_Dir; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      --  '-d' is for compatibility.
      return Name = "-d" or else Name = "--dir";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Dir) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--dir [LIBs]       Disp contents of the libraries";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Dir; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      Setup_Libraries (True);

      if Args'Length = 0 then
         Disp_Library (Std_Names.Name_Work);
      else
         for I in Args'Range loop
            Disp_Library (Name_Table.Get_Identifier (Args (I).all));
         end loop;
      end if;
   end Perform_Action;

   --  Command Find.
   type Command_Find is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Find; Name : String) return Boolean;
   function Get_Short_Help (Cmd : Command_Find) return String;
   procedure Perform_Action (Cmd : in out Command_Find; Args : Argument_List);

   function Decode_Command (Cmd : Command_Find; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-f";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Find) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-f FILEs           Disp units in FILES";
   end Get_Short_Help;

   --  Return TRUE is UNIT can be at the apex of a design hierarchy.
   function Is_Top_Entity (Unit : Iir) return Boolean
   is
   begin
      if Get_Kind (Unit) /= Iir_Kind_Entity_Declaration then
         return False;
      end if;
      if Get_Port_Chain (Unit) /= Null_Iir then
         return False;
      end if;
      if Get_Generic_Chain (Unit) /= Null_Iir then
         return False;
      end if;
      return True;
   end Is_Top_Entity;

   --  Disp contents design files FILES.
   procedure Perform_Action (Cmd : in out Command_Find; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);

      use Name_Table;
      Id : Name_Id;
      Design_File : Iir_Design_File;
      Unit : Iir;
      Lib : Iir;
      Flag_Add : constant Boolean := False;
   begin
      Flags.Bootstrap := True;
      Libraries.Load_Std_Library;
      Libraries.Load_Work_Library;

      for I in Args'Range loop
         Id := Get_Identifier (Args (I).all);
         Design_File := Libraries.Load_File (Id);
         if Design_File /= Null_Iir then
            Unit := Get_First_Design_Unit (Design_File);
            while Unit /= Null_Iir loop
               Lib := Get_Library_Unit (Unit);
               Disp_Library_Unit (Lib);
               if Is_Top_Entity (Lib) then
                  Put (" **");
               end if;
               New_Line;
               if Flag_Add then
                  Libraries.Add_Design_Unit_Into_Library (Unit);
               end if;
               Unit := Get_Chain (Unit);
            end loop;
         end if;
      end loop;
      if Flag_Add then
         Libraries.Save_Work_Library;
      end if;
   end Perform_Action;

   --  Command Import.
   type Command_Import is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Import; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Import) return String;
   procedure Perform_Action (Cmd : in out Command_Import;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Import; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-i";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Import) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-i [OPTS] FILEs    Import units of FILEs";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Import; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Errorout;

      Id : Name_Id;
      Design_File : Iir_Design_File;
      Unit : Iir;
      Next_Unit : Iir;
      Lib : Iir;
   begin
      Setup_Libraries (True);

      --  Parse all files.
      for I in Args'Range loop
         Id := Name_Table.Get_Identifier (Args (I).all);
         Design_File := Libraries.Load_File (Id);
         if Design_File /= Null_Iir then
            Unit := Get_First_Design_Unit (Design_File);
            while Unit /= Null_Iir loop
               if Flag_Verbose then
                  Lib := Get_Library_Unit (Unit);
                  Disp_Library_Unit (Lib);
                  if Is_Top_Entity (Lib) then
                     Put (" **");
                  end if;
                  New_Line;
               end if;
               Next_Unit := Get_Chain (Unit);
               Set_Chain (Unit, Null_Iir);
               Libraries.Add_Design_Unit_Into_Library (Unit);
               Unit := Next_Unit;
            end loop;
         end if;
      end loop;

      if Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      --  Analyze all files.
      if False then
         Design_File := Get_Design_File_Chain (Libraries.Work_Library);
         while Design_File /= Null_Iir loop
            Unit := Get_First_Design_Unit (Design_File);
            while Unit /= Null_Iir loop
               case Get_Date (Unit) is
                  when Date_Valid
                    | Date_Analyzed =>
                     null;
                  when Date_Parsed =>
                     Libraries.Finish_Compilation (Unit, False);
                  when others =>
                     raise Internal_Error;
               end case;
               Unit := Get_Chain (Unit);
            end loop;
            Design_File := Get_Chain (Design_File);
         end loop;
      end if;

      Libraries.Save_Work_Library;
   exception
      when Compilation_Error =>
         Error ("importation has failed due to compilation error");
         raise;
   end Perform_Action;

   --  Command Check_Syntax.
   type Command_Check_Syntax is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Check_Syntax; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Check_Syntax) return String;
   procedure Perform_Action (Cmd : in out Command_Check_Syntax;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Check_Syntax; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "-s";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Check_Syntax) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "-s [OPTS] FILEs    Check syntax of FILEs";
   end Get_Short_Help;

   procedure Analyze_One_File (File_Name : String)
   is
      Id : Name_Id;
      Design_File : Iir_Design_File;
      Unit : Iir;
      Next_Unit : Iir;
   begin
      Id := Name_Table.Get_Identifier (File_Name);
      if Flag_Verbose then
         Put (File_Name);
         Put_Line (":");
      end if;
      Design_File := Libraries.Load_File (Id);
      if Design_File = Null_Iir then
         raise Errorout.Compilation_Error;
      end if;

      Unit := Get_First_Design_Unit (Design_File);
      while Unit /= Null_Iir loop
         if Flag_Verbose then
            Put (' ');
            Disp_Library_Unit (Get_Library_Unit (Unit));
            New_Line;
         end if;
         -- Sem, canon, annotate a design unit.
         Libraries.Finish_Compilation (Unit, True);

         Next_Unit := Get_Chain (Unit);
         if Errorout.Nbr_Errors = 0 then
            Set_Chain (Unit, Null_Iir);
            Libraries.Add_Design_Unit_Into_Library (Unit);
         end if;

         Unit := Next_Unit;
      end loop;

      if Errorout.Nbr_Errors > 0 then
         raise Errorout.Compilation_Error;
      end if;
   end Analyze_One_File;

   procedure Analyze_Files (Files : Argument_List; Save_Library : Boolean) is
   begin
      Setup_Libraries (True);

      --  Parse all files.
      for I in Files'Range loop
         Analyze_One_File (Files (I).all);
      end loop;

      if Save_Library then
         Libraries.Save_Work_Library;
      end if;
   end Analyze_Files;

   procedure Perform_Action (Cmd : in out Command_Check_Syntax;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      Analyze_Files (Args, False);
   end Perform_Action;

   --  Command --clean: remove object files.
   type Command_Clean is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Clean; Name : String) return Boolean;
   function Get_Short_Help (Cmd : Command_Clean) return String;
   procedure Perform_Action (Cmd : in out Command_Clean; Args : Argument_List);

   function Decode_Command (Cmd : Command_Clean; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--clean";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Clean) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--clean            Remove generated files";
   end Get_Short_Help;

   procedure Delete (Str : String)
   is
      Status : Boolean;
   begin
      Delete_File (Str'Address, Status);
      if Flag_Verbose and Status then
         Put_Line ("delete " & Str (Str'First .. Str'Last - 1));
      end if;
   end Delete;

   procedure Perform_Action (Cmd : in out Command_Clean; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Name_Table;

      Obj_Suffix : constant String_Access := Get_Object_Suffix;
      Exec_Suffix : constant String_Access := Get_Executable_Suffix;

      procedure Delete_Asm_Obj (Str : String) is
      begin
         Delete (Str & Obj_Suffix.all & Nul);
         Delete (Str & Asm_Suffix & Nul);
         if Flag_Postprocess then
            Delete (Str & Post_Suffix & Nul);
         end if;
      end Delete_Asm_Obj;

      procedure Delete_Top_Unit (Str : String) is
      begin
         --  Delete elaboration file
         Delete_Asm_Obj (Elab_Prefix & Str);

         --  Delete file list.
         Delete (Str & List_Suffix & Nul);

         --  Delete executable.
         Delete (Str & Exec_Suffix.all & Nul);
      end Delete_Top_Unit;

      File : Iir_Design_File;
      Design_Unit : Iir_Design_Unit;
      Lib_Unit : Iir;
      Str : String_Access;
   begin
      if Args'Length /= 0 then
         Error ("command '--clean' does not accept any argument");
         raise Option_Error;
      end if;

      Flags.Bootstrap := True;
      --  Load libraries.
      Libraries.Load_Std_Library;
      Libraries.Load_Work_Library;

      File := Get_Design_File_Chain (Libraries.Work_Library);
      while File /= Null_Iir loop
         --  Delete compiled file.
         Str := Append_Suffix (Image (Get_Design_File_Filename (File)), "");
         Delete_Asm_Obj (Str.all);
         Free (Str);

         --  Try any possible top-level names
         Design_Unit := Get_First_Design_Unit (File);
         while Design_Unit /= Null_Iir loop
            Lib_Unit := Get_Library_Unit (Design_Unit);
            case Get_Kind (Lib_Unit) is
               when Iir_Kind_Entity_Declaration
                 | Iir_Kind_Configuration_Declaration =>
                  Delete_Top_Unit (Image (Get_Identifier (Lib_Unit)));
               when Iir_Kind_Architecture_Body =>
                  Delete_Top_Unit
                    (Image (Get_Entity_Identifier_Of_Architecture (Lib_Unit))
                       & '-'
                       & Image (Get_Identifier (Lib_Unit)));
               when others =>
                  null;
            end case;
            Design_Unit := Get_Chain (Design_Unit);
         end loop;
         File := Get_Chain (File);
      end loop;
   end Perform_Action;

   --  Command --remove: remove object file and library file.
   type Command_Remove is new Command_Clean with null record;
   function Decode_Command (Cmd : Command_Remove; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Remove) return String;
   procedure Perform_Action (Cmd : in out Command_Remove;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Remove; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--remove";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Remove) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--remove           Remove generated files and library file";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Remove; Args : Argument_List)
   is
      use Name_Table;
   begin
      if Args'Length /= 0 then
         Error ("command '--remove' does not accept any argument");
         raise Option_Error;
      end if;
      Perform_Action (Command_Clean (Cmd), Args);
      Delete (Image (Libraries.Work_Directory)
              & Libraries.Library_To_File_Name (Libraries.Work_Library)
              & Nul);
   end Perform_Action;

   --  Command --copy: copy work library to current directory.
   type Command_Copy is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Copy; Name : String) return Boolean;
   function Get_Short_Help (Cmd : Command_Copy) return String;
   procedure Perform_Action (Cmd : in out Command_Copy; Args : Argument_List);

   function Decode_Command (Cmd : Command_Copy; Name : String) return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--copy";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Copy) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--copy             Copy work library to current directory";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Copy; Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
      use Name_Table;
      use Libraries;

      File : Iir_Design_File;
      Dir : Name_Id;
   begin
      if Args'Length /= 0 then
         Error ("command '--copy' does not accept any argument");
         raise Option_Error;
      end if;

      Setup_Libraries (False);
      Libraries.Load_Std_Library;
      Dir := Work_Directory;
      Work_Directory := Null_Identifier;
      Libraries.Load_Work_Library;
      Work_Directory := Dir;

      Dir := Get_Library_Directory (Libraries.Work_Library);
      if Dir = Name_Nil or else Dir = Files_Map.Get_Home_Directory then
         Error ("cannot copy library on itself (use --remove first)");
         raise Option_Error;
      end if;

      File := Get_Design_File_Chain (Libraries.Work_Library);
      while File /= Null_Iir loop
         --  Copy object files (if any).
         declare
            Basename : constant String :=
              Get_Base_Name (Image (Get_Design_File_Filename (File)));
            Src : String_Access;
            Dst : String_Access;
            Success : Boolean;
            pragma Unreferenced (Success);
         begin
            Src := new String'(Image (Dir) & Basename & Get_Object_Suffix.all);
            Dst := new String'(Basename & Get_Object_Suffix.all);
            Copy_File (Src.all, Dst.all, Success, Overwrite, Full);
            --  Be silent in case of error.
            Free (Src);
            Free (Dst);
         end;
         if Get_Design_File_Directory (File) = Name_Nil then
            Set_Design_File_Directory (File, Dir);
         end if;

         File := Get_Chain (File);
      end loop;
      Libraries.Work_Directory := Name_Nil;
      Libraries.Save_Work_Library;
   end Perform_Action;

   --  Command --disp-standard.
   type Command_Disp_Standard is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Disp_Standard; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Disp_Standard) return String;
   procedure Perform_Action (Cmd : in out Command_Disp_Standard;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Disp_Standard; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--disp-standard";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Disp_Standard) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--disp-standard    Disp std.standard in pseudo-vhdl";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Disp_Standard;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd);
   begin
      if Args'Length /= 0 then
         Error ("command '--disp-standard' does not accept any argument");
         raise Option_Error;
      end if;
      Flags.Bootstrap := True;
      Libraries.Load_Std_Library;
      Disp_Vhdl.Disp_Vhdl (Std_Package.Std_Standard_Unit);
   end Perform_Action;

   --  Command --find-top.
   type Command_Find_Top is new Command_Lib with null record;
   function Decode_Command (Cmd : Command_Find_Top; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Find_Top) return String;
   procedure Perform_Action (Cmd : in out Command_Find_Top;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Find_Top; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--find-top";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Find_Top) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--find-top         Disp possible top entity in work library";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Find_Top;
                             Args : Argument_List)
   is
      use Libraries;
      pragma Unreferenced (Cmd);
      From : Iir;
      Top : Iir;
   begin
      Setup_Libraries (True);

      if Args'Length = 0 then
         From := Work_Library;
      elsif Args'Length = 1 then
         From := Find_Design_File
           (Work_Library, Name_Table.Get_Identifier (Args (Args'First).all));
         if not Is_Valid (From) then
            Error ("cannot find '" & Args (Args'First).all & "' in library");
            raise Option_Error;
         end if;
      else
         Error ("command '--find-top' accepts at most one argument");
         raise Option_Error;
      end if;

      Top := Configuration.Find_Top_Entity (From);

      if Top = Null_Iir then
         Error ("no top entity found");
      else
         Put_Line (Name_Table.Image (Get_Identifier (Top)));
      end if;
   end Perform_Action;

   --  Command --bug-box.
   type Command_Bug_Box is new Command_Type with null record;
   function Decode_Command (Cmd : Command_Bug_Box; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Bug_Box) return String;
   procedure Perform_Action (Cmd : in out Command_Bug_Box;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Bug_Box; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--bug-box";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Bug_Box) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "!--bug-box          Crash and emit a bug-box";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Bug_Box;
                             Args : Argument_List)
   is
      pragma Unreferenced (Cmd, Args);
   begin
      raise Program_Error;
   end Perform_Action;

   procedure Load_All_Libraries_And_Files
   is
      use Files_Map;
      use Libraries;
      use Errorout;

      procedure Extract_Library_Clauses (Unit : Iir_Design_Unit)
      is
         Lib1 : Iir_Library_Declaration;
         pragma Unreferenced (Lib1);
         Ctxt_Item : Iir;
      begin
         --  Extract library clauses.
         Ctxt_Item := Get_Context_Items (Unit);
         while Ctxt_Item /= Null_Iir loop
            if Get_Kind (Ctxt_Item) = Iir_Kind_Library_Clause then
               Lib1 := Get_Library (Get_Identifier (Ctxt_Item),
                                    Get_Location (Ctxt_Item));
            end if;
            Ctxt_Item := Get_Chain (Ctxt_Item);
         end loop;
      end Extract_Library_Clauses;

      Lib : Iir_Library_Declaration;
      Fe : Source_File_Entry;
      File, Next_File : Iir_Design_File;
      Unit, Next_Unit : Iir_Design_Unit;
      Design_File : Iir_Design_File;

      Old_Work : Iir_Library_Declaration;
   begin
      Lib := Std_Library;
      Lib := Get_Chain (Lib);
      Old_Work := Work_Library;
      while Lib /= Null_Iir loop
         --  Design units are always put in the work library.
         Work_Library := Lib;

         File := Get_Design_File_Chain (Lib);
         while File /= Null_Iir loop
            Next_File := Get_Chain (File);
            Fe := Read_Source_File (Get_Design_File_Directory (File),
                                    Get_Design_File_Filename (File));
            if Fe = No_Source_File_Entry then
               --  FIXME: should remove all the design file from the library.
               null;
            elsif Is_Eq (Get_File_Checksum (Fe),
                         Get_File_Checksum (File))
            then
               --  File has not been modified.
               --  Extract libraries.
               --  Note: we can't parse it only, since we need to keep the
               --    date.
               Unit := Get_First_Design_Unit (File);
               while Unit /= Null_Iir loop
                  Load_Parse_Design_Unit (Unit, Null_Iir);
                  Extract_Library_Clauses (Unit);
                  Unit := Get_Chain (Unit);
               end loop;
            else
               --  File has been modified.
               --  Parse it.
               Design_File := Load_File (Fe);

               --  Exit now in case of parse error.
               if Design_File = Null_Iir
                 or else Nbr_Errors > 0
               then
                  raise Compilation_Error;
               end if;

               Unit := Get_First_Design_Unit (Design_File);
               while Unit /= Null_Iir loop
                  Extract_Library_Clauses (Unit);

                  Next_Unit := Get_Chain (Unit);
                  Set_Chain (Unit, Null_Iir);
                  Add_Design_Unit_Into_Library (Unit);
                  Unit := Next_Unit;
               end loop;
            end if;
            File := Next_File;
         end loop;
         Lib := Get_Chain (Lib);
      end loop;
      Work_Library := Old_Work;
   end Load_All_Libraries_And_Files;

   --  Check the Elab_Flag is not set on design units of LIB.
   procedure Check_No_Elab_Flag (Lib : Iir_Library_Declaration)
   is
      File : Iir_Design_File;
      Unit : Iir_Design_Unit;
   begin
      File := Get_Design_File_Chain (Lib);
      while File /= Null_Iir loop
         Unit := Get_First_Design_Unit (File);
         while Unit /= Null_Iir loop
            if Get_Elab_Flag (Unit) then
               raise Internal_Error;
            end if;
            Unit := Get_Chain (Unit);
         end loop;
         File := Get_Chain (File);
      end loop;
   end Check_No_Elab_Flag;

   function Build_Dependence (Prim : String_Access; Sec : String_Access)
     return Iir_List
   is
      procedure Build_Dependence_List (File : Iir_Design_File; List : Iir_List)
      is
         El : Iir_Design_File;
         Depend_List : Iir_List;
         Depend_It : List_Iterator;
      begin
         if Get_Elab_Flag (File) then
            return;
         end if;

         Set_Elab_Flag (File, True);
         Depend_List := Get_File_Dependence_List (File);
         if Depend_List /= Null_Iir_List then
            Depend_It := List_Iterate (Depend_List);
            while Is_Valid (Depend_It) loop
               El := Get_Element (Depend_It);
               Build_Dependence_List (El, List);
               Next (Depend_It);
            end loop;
         end if;
         Append_Element (List, File);
      end Build_Dependence_List;

      use Configuration;
      use Name_Table;

      Top : Iir;
      Primary_Id : Name_Id;
      Secondary_Id : Name_Id;

      File : Iir_Design_File;
      Unit : Iir;

      Files_List : Iir_List;
   begin
      Check_No_Elab_Flag (Libraries.Work_Library);

      Primary_Id := Get_Identifier (Prim.all);
      if Sec /= null then
         Secondary_Id := Get_Identifier (Sec.all);
      else
         Secondary_Id := Null_Identifier;
      end if;

      if True then
         --  Load the world.
         Load_All_Libraries_And_Files;
      else
         --  Re-parse modified files in order configure could find all design
         --  units.
         declare
            use Files_Map;
            Fe : Source_File_Entry;
            Next_File : Iir_Design_File;
            Design_File : Iir_Design_File;
         begin
            File := Get_Design_File_Chain (Libraries.Work_Library);
            while File /= Null_Iir loop
               Next_File := Get_Chain (File);
               Fe := Read_Source_File (Get_Design_File_Directory (File),
                                       Get_Design_File_Filename (File));
               if Fe = No_Source_File_Entry then
                  --  FIXME: should remove all the design file from
                  --  the library.
                  null;
               else
                  if not Is_Eq (Get_File_Checksum (Fe),
                                Get_File_Checksum (File))
                  then
                     --  FILE has been modified.
                     Design_File := Libraries.Load_File (Fe);
                     if Design_File /= Null_Iir then
                        Libraries.Add_Design_File_Into_Library (Design_File);
                     end if;
                  end if;
               end if;
               File := Next_File;
            end loop;
         end;
      end if;

      Flags.Flag_Elaborate := True;
      Flags.Flag_Elaborate_With_Outdated := True;
      Flag_Load_All_Design_Units := True;
      Flag_Build_File_Dependence := True;

      Top := Configure (Primary_Id, Secondary_Id);
      if Top = Null_Iir then
         --  Error during configuration (primary unit not found).
         raise Option_Error;
      end if;

      --  Add unused design units (and their dependencies).
      declare
         N : Natural;
      begin
         N := Design_Units.First;
         while N <= Design_Units.Last loop
            Unit := Design_Units.Table (N);
            N := N + 1;
            File := Get_Design_File (Unit);
            if not Get_Elab_Flag (File) then
               Set_Elab_Flag (File, True);
               Unit := Get_First_Design_Unit (File);
               while Unit /= Null_Iir loop
                  if not Get_Elab_Flag (Unit) then
                     Add_Design_Unit (Unit, Null_Iir);
                  end if;
                  Unit := Get_Chain (Unit);
               end loop;
            end if;
         end loop;
      end;

      --  Clear elab flag on design files (as it is used by below by
      --  Build_Dependence_List).
      for I in reverse Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         File := Get_Design_File (Unit);
         Set_Elab_Flag (File, False);
      end loop;

      --  Create a list of files, from the last to the first.
      Files_List := Create_Iir_List;
      for I in Design_Units.First .. Design_Units.Last loop
         Unit := Design_Units.Table (I);
         File := Get_Design_File (Unit);
         Build_Dependence_List (File, Files_List);
      end loop;

      return Files_List;
   end Build_Dependence;

   function Source_File_Modified (File : Iir_Design_File) return Boolean
   is
      use Files_Map;

      Fe : Source_File_Entry;
   begin
      --  2) file has been modified.
      Fe := Read_Source_File (Get_Design_File_Directory (File),
                              Get_Design_File_Filename (File));
      if not Is_Eq (Get_File_Checksum (Fe),
                    Get_File_Checksum (File))
      then
         if Flag_Verbose then
            Put_Line ("file " & Name_Table.Image (Get_File_Name (Fe))
                        & " has been modified");
         end if;
         return True;
      else
         return False;
      end if;
   end Source_File_Modified;

   function Is_File_Outdated (File : Iir_Design_File) return Boolean
   is
      Unit : Iir;
      Lib_Unit : Iir;
   begin
      Unit := Get_First_Design_Unit (File);
      while Unit /= Null_Iir loop
         Lib_Unit := Get_Library_Unit (Unit);
         if Get_Kind (Lib_Unit) = Iir_Kind_Configuration_Declaration
           and then Get_Identifier (Lib_Unit) = Null_Identifier
         then
            --  Do not consider default configurations (there is no user code
            --  for them).
            null;
         elsif Get_Date (Unit) not in Date_Valid then
            --  Unit not yet analyzed:
            if Flag_Verbose then
               Disp_Library_Unit (Get_Library_Unit (Unit));
               Put_Line (" was not analyzed");
            end if;
            return True;
         else
            --  Check if one of the dependence is newer
            declare
               Depends : constant Iir_List := Get_Dependence_List (Unit);
               Stamp : constant Time_Stamp_Id :=
                 Get_Analysis_Time_Stamp (File);
               Depends_It : List_Iterator;
               El : Iir;
               Dep : Iir_Design_Unit;
               Dep_File : Iir_Design_File;
            begin
               if Depends /= Null_Iir_List then
                  Depends_It := List_Iterate (Depends);
                  while Is_Valid (Depends_It) loop
                     El := Get_Element (Depends_It);
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
                       and then
                       Files_Map.Is_Gt (Get_Analysis_Time_Stamp (Dep_File),
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
                     Next (Depends_It);
                  end loop;
               end if;
            end;
         end if;
         Unit := Get_Chain (Unit);
      end loop;

      return False;
   end Is_File_Outdated;

   --  Convert NAME to lower cases, unless it is an extended identifier.
   function Convert_Name (Name : String_Access) return String_Access
   is
      function Is_Bad_Unit_Name return Boolean is
      begin
         if Name'Length = 0 then
            return True;
         end if;
         --  Don't try to handle extended identifier.
         if Name (Name'First) = '\' then
            return False;
         end if;
         --  Look for suspicious characters.
         --  Do not try to be exhaustive as the correct check will be done
         --  by convert_identifier.
         for I in Name'Range loop
            case Name (I) is
               when '.' | '/' | '\' =>
                  return True;
               when others =>
                  null;
            end case;
         end loop;
         return False;
      end Is_Bad_Unit_Name;

      function Is_A_File_Name return Boolean is
      begin
         --  Check .vhd
         if Name'Length > 4
           and then Name (Name'Last - 3 .. Name'Last) = ".vhd"
         then
            return True;
         end if;
         --  Check .vhdl
         if Name'Length > 5
           and then Name (Name'Last - 4 .. Name'Last) = ".vhdl"
         then
            return True;
         end if;
         --  Check ../
         if Name'Length > 3
           and then Name (Name'First .. Name'First + 2) = "../"
         then
            return True;
         end if;
         --  Check ..\
         if Name'Length > 3
           and then Name (Name'First .. Name'First + 2) = "..\"
         then
            return True;
         end if;
         --  Should try to find the file ?
         return False;
      end Is_A_File_Name;

      Res : String_Access;
   begin
      --  Try to identifier bad names (such as file names), so that
      --  friendly message can be displayed.
      if Is_Bad_Unit_Name then
         Errorout.Error_Msg_Option_NR ("bad unit name '" & Name.all & "'");
         if Is_A_File_Name then
            Errorout.Error_Msg_Option_NR
              ("(a unit name is required instead of a filename)");
         end if;
         raise Option_Error;
      end if;
      Res := new String'(Name.all);
      Scanner.Convert_Identifier (Res.all);
      return Res;
   end Convert_Name;

   procedure Extract_Elab_Unit
     (Cmd_Name : String; Args : Argument_List; Next_Arg : out Natural)
   is
   begin
      if Args'Length = 0 then
         Error ("command '" & Cmd_Name & "' required an unit name");
         raise Option_Error;
      end if;

      Prim_Name := Convert_Name (Args (Args'First));
      Next_Arg := Args'First + 1;
      Sec_Name := null;

      if Args'Length >= 2 then
         declare
            Sec : constant String_Access := Args (Next_Arg);
         begin
            if Sec (Sec'First) /= '-' then
               Sec_Name := Convert_Name (Sec);
               Next_Arg := Args'First + 2;
            end if;
         end;
      end if;
   end Extract_Elab_Unit;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Import);
      Register_Command (new Command_Check_Syntax);
      Register_Command (new Command_Dir);
      Register_Command (new Command_Find);
      Register_Command (new Command_Clean);
      Register_Command (new Command_Remove);
      Register_Command (new Command_Copy);
      Register_Command (new Command_Disp_Standard);
      Register_Command (new Command_Find_Top);
      Register_Command (new Command_Bug_Box);
   end Register_Commands;
end Ghdllocal;
