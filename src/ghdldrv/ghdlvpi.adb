--  GHDL driver - compile and link wrappers for VPI and VHPI.
--  Copyright (C) 2016 Tristan Gingold
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

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Simple_IO; use Simple_IO;
with Options; use Options;

with Ghdlmain; use Ghdlmain;
with Ghdllocal;
with Default_Paths; use Default_Paths;

package body Ghdlvpi is

   --  Useful flags for target dependent operations.
   --  So, we only support unix, darwin and windows.  Might need a little bit
   --  of tuning for another OS.
   Is_Unix : constant Boolean := Shared_Library_Extension = ".so";
   Is_Darwin : constant Boolean := Shared_Library_Extension = ".dylib";

   --  Return the include directory.
   function Get_Vpi_Include_Dir return String is
   begin
      --  Compute install path
      Ghdllocal.Set_Exec_Prefix_From_Program_Name;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator
        & Default_Paths.IncDir_Suffix & Directory_Separator
        & "ghdl";
   end Get_Vpi_Include_Dir;

   --  Return the lib directory.
   function Get_Vpi_Lib_Dir return String is
   begin
      if Ghdllocal.Exec_Prefix = null then
         --  Compute install path (only once).
         Ghdllocal.Set_Exec_Prefix_From_Program_Name;
      end if;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator & LibDir_Suffix;
   end Get_Vpi_Lib_Dir;

   --  Return the lib directory, but unixify the path (for a unix shell in
   --  windows).
   function Get_Vpi_Lib_Dir_Unix return String is
   begin
      return Convert_Path_To_Unix (Get_Vpi_Lib_Dir);
   end Get_Vpi_Lib_Dir_Unix;

   function Get_Vpi_Cflags return Argument_List
   is
      Extra_Args : Argument_List (1 .. 2);
      Nbr : Natural;
   begin
      Extra_Args (1) := new String'("-I" & Get_Vpi_Include_Dir);
      Nbr := 1;

      if Is_Unix then
         --  PIC is the default on Darwin and Windows.
         Nbr := Nbr + 1;
         Extra_Args (Nbr) := new String'("-fPIC");
      end if;
      return Extra_Args (1 .. Nbr);
   end Get_Vpi_Cflags;

   function Get_Vpi_Ldflags return Argument_List
   is
      Extra_Args : Argument_List (1 .. 4);
      Nbr : Natural;
   begin
      Extra_Args (1) := new String'("--shared");
      Extra_Args (2) := new String'("-L" & Get_Vpi_Lib_Dir);
      Extra_Args (3) := new String'("-lghdlvpi");
      Nbr := 3;

      if Is_Unix or Is_Darwin then
         --  On linux/unix, add rpath.
         --  No such concept on windows.
         Nbr := Nbr + 1;
         Extra_Args (Nbr) := new String'("-Wl,-rpath," & Get_Vpi_Lib_Dir);
      end if;

      return Extra_Args (1 .. Nbr);
   end Get_Vpi_Ldflags;

   --  Display ARGS on a single line.
   procedure Disp (Args : Argument_List) is
   begin
      for I in Args'Range loop
         if I /= Args'First then
            Put (' ');
         end if;
         Put (Args (I).all);
      end loop;
   end Disp;

   procedure Spawn_Compile (User_Args : Argument_List;
                            Extra_Args : Argument_List;
                            Verbose : Boolean)
   is
      Cargs : Argument_List (1 .. User_Args'Length + Extra_Args'Length);
      Program_Name : String_Access;
      Nbr_Args : Natural;
      Status : Integer;
   begin
      Nbr_Args := 0;

      --  Extract compiler name.
      if User_Args'First > User_Args'Last then
         Error ("missing compiler name");
      else
         Program_Name := User_Args (User_Args'First);
         if Ghdllocal.Is_Basename (Program_Name.all) then
            --  For a command name (without path component), search on the
            --  path.
            Program_Name := Locate_Exec_On_Path (Program_Name.all);
         else
            --  For a relative or absolute path, use the path directly.
            null;
         end if;
      end if;

      --  Copy user args.
      for I in User_Args'First + 1 .. User_Args'Last loop
         Nbr_Args := Nbr_Args + 1;
         Cargs (Nbr_Args) := User_Args (I);
      end loop;

      --  Copy extra args.
      for I in Extra_Args'Range loop
         Nbr_Args := Nbr_Args + 1;
         Cargs (Nbr_Args) := Extra_Args (I);
      end loop;

      --  Display command (if verbose)
      if Verbose then
         Put (Program_Name.all);
         for I in Cargs'First .. Nbr_Args loop
            Put (' ');
            Put (Cargs (I).all);
         end loop;
         New_Line;
      end if;

      --  Execute command
      Status := Spawn (Program_Name.all, Cargs (Cargs'First .. Nbr_Args));
      Set_Exit_Status (Exit_Status (Status));
   end Spawn_Compile;

   --  A command that spawn with extra_args
   type Extra_Args_Func is access function return Argument_List;
   type Command_Spawn_Type is new Command_Str_Type with record
      Flag_Verbose : Boolean := False;
      Extra_Args : Extra_Args_Func;
   end record;

   procedure Perform_Action (Cmd : in out Command_Spawn_Type;
                             Args : Argument_List);
   procedure Decode_Option (Cmd : in out Command_Spawn_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);


   procedure Decode_Option (Cmd : in out Command_Spawn_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Unreferenced (Arg);
   begin
      if Option = "-v" then
         Cmd.Flag_Verbose := True;
         Res := Option_Ok;
      else
         Res := Option_Unknown;
      end if;
   end Decode_Option;

   procedure Perform_Action (Cmd : in out Command_Spawn_Type;
                             Args : Argument_List) is
   begin
      Spawn_Compile (Args, Cmd.Extra_Args.all, Cmd.Flag_Verbose);
   end Perform_Action;


   --  A command that display flags.
   type Command_Vpi_Flags is new Command_Str_Type with record
      Flags : Extra_Args_Func;
   end record;
   procedure Perform_Action (Cmd : in out Command_Vpi_Flags;
                             Args : Argument_List);

   procedure Perform_Action (Cmd : in out Command_Vpi_Flags;
                             Args : Argument_List)
   is
      pragma Unreferenced (Args);
   begin
      Disp (Cmd.Flags.all);
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command
        (new Command_Spawn_Type'
           (Command_Type with
            Flag_Verbose => False,
            Cmd_Str => new String'
              ("--vpi-compile"),
            Help_Str => new String'
              ("--vpi-compile CMD ARGS"
              & ASCII.LF & "  Compile with VPI/VHPI include path"),
            Extra_Args => Get_Vpi_Cflags'Access));
      Register_Command
        (new Command_Spawn_Type'
           (Command_Type with
            Flag_Verbose => False,
            Cmd_Str => new String'
              ("--vpi-link"),
            Help_Str => new String'
              ("--vpi-link CMD ARGS"
              & ASCII.LF & "  Link with VPI/VHPI library"),
            Extra_Args => Get_Vpi_Ldflags'Access));

      Register_Command
        (new Command_Vpi_Flags'
           (Command_Type with
            Cmd_Str => new String'
              ("--vpi-cflags"),
            Help_Str => new String'
              ("--vpi-cflags"
              & ASCII.LF & "  Display VPI/VHPI compile flags"),
            Flags => Get_Vpi_Cflags'Access));
      Register_Command
        (new Command_Vpi_Flags'
           (Command_Type with
            Cmd_Str => new String'
              ("--vpi-ldflags"),
            Help_Str => new String'
              ("--vpi-ldflags"
              & ASCII.LF & "  Display VPI/VHPI link flags"),
            Flags => Get_Vpi_Ldflags'Access));

      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--vpi-include-dir"),
            Help_Str => new String'
              ("--vpi-include-dir"
              & ASCII.LF & "  Display VPI/VHPI include directory"),
            Disp => Get_Vpi_Include_Dir'Access));
      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--vpi-library-dir"),
            Help_Str => new String'
              ("--vpi-library-dir"
              & ASCII.LF & "  Display VPI/VHPI library directory"),
            Disp => Get_Vpi_Lib_Dir'Access));
      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--vpi-library-dir-unix"),
            Help_Str => new String'
              ("--vpi-library-dir-unix"
              & ASCII.LF & "  Display VPI/VHPI library directory (unix form)"),
            Disp => Get_Vpi_Lib_Dir_Unix'Access));

   end Register_Commands;
end Ghdlvpi;
