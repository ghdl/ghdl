--  GHDL driver - compile and link wrappers for VPI.
--  Copyright (C) 2016 Tristan Gingold
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Ghdlmain; use Ghdlmain;
with Ghdllocal;
with Default_Pathes;

package body Ghdlvpi is

   --  A command that accepts '-v'.
   type Command_Flag_Type is abstract new Command_Type with record
      Flag_Verbose : Boolean := False;
   end record;

   procedure Decode_Option (Cmd : in out Command_Flag_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res);

   procedure Decode_Option (Cmd : in out Command_Flag_Type;
                            Option : String;
                            Arg : String;
                            Res : out Option_Res)
   is
      pragma Unreferenced (Arg);
   begin
      if Option = "-v" then
         Cmd.Flag_Verbose := True;
         Res := Option_Ok;
      else
         Res := Option_Bad;
      end if;
   end Decode_Option;

   --  Return the include directory.
   function Get_Vpi_Include_Dir return String is
   begin
      --  Compute install path
      Ghdllocal.Set_Exec_Prefix;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator & "include";
   end Get_Vpi_Include_Dir;

   --  Return the include directory.
   function Get_Vpi_Lib_Dir return String is
   begin
      if Ghdllocal.Exec_Prefix = null then
         --  Compute install path (only once).
         Ghdllocal.Set_Exec_Prefix;
      end if;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator & "lib";
   end Get_Vpi_Lib_Dir;

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

   --  Command --vpi-compile
   type Command_Vpi_Compile is new Command_Flag_Type with null record;
   function Decode_Command (Cmd : Command_Vpi_Compile; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Vpi_Compile) return String;
   procedure Perform_Action (Cmd : in out Command_Vpi_Compile;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Vpi_Compile; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--vpi-compile";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Vpi_Compile) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--vpi-compile CMD ARGS     Compile with VPI include path";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Vpi_Compile;
                             Args : Argument_List)
   is
      Extra_Args : Argument_List (1 .. 1);
   begin
      Extra_Args (1) := new String'("-I" & Get_Vpi_Include_Dir);

      Spawn_Compile (Args, Extra_Args, Cmd.Flag_Verbose);
   end Perform_Action;

   --  Command --vpi-link
   type Command_Vpi_Link is new Command_Flag_Type with null record;
   function Decode_Command (Cmd : Command_Vpi_Link; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Vpi_Link) return String;
   procedure Perform_Action (Cmd : in out Command_Vpi_Link;
                             Args : Argument_List);

   function Decode_Command (Cmd : Command_Vpi_Link; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "--vpi-link";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Vpi_Link) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "--vpi-link CMD ARGS     Link with VPI library";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_Vpi_Link;
                             Args : Argument_List)
   is
      use Default_Pathes;
      Is_Unix : constant Boolean := Shared_Library_Extension = ".so";
      Is_Darwin : constant Boolean := Shared_Library_Extension = ".dylib";
      Extra_Args : Argument_List (1 .. 4);
      Nbr : Natural;
   begin
      Extra_Args (1) := new String'("--shared");
      Extra_Args (2) := new String'("-L" & Get_Vpi_Lib_Dir);
      Extra_Args (3) := new String'("-lghdlvpi");
      Nbr := 3;

      if Is_Unix or Is_Darwin then
         --  On linux/unix, add rpath.
         Nbr := Nbr + 1;
         Extra_Args (Nbr) := new String'
           ("-Wl,-rpath," & Get_Vpi_Lib_Dir);
      end if;

      Spawn_Compile (Args, Extra_Args (1 .. Nbr), Cmd.Flag_Verbose);
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Vpi_Compile);
      Register_Command (new Command_Vpi_Link);
   end Register_Commands;
end Ghdlvpi;
