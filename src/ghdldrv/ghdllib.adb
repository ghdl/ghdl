--  GHDL driver for libraries commands
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

with Ghdllocal; use Ghdllocal;
with Ghdlmain; use Ghdlmain;
with Version;
with Default_Paths;

package body Ghdllib is

   function Get_Libghdl_Name return String
   is
      Libghdl_Version : String := Version.Ghdl_Ver;
   begin
      for I in Libghdl_Version'Range loop
         if Libghdl_Version (I) = '.' or Libghdl_Version (I) = '-' then
            Libghdl_Version (I) := '_';
         end if;
      end loop;
      return "libghdl-" & Libghdl_Version
        & Default_Paths.Shared_Library_Extension;
   end Get_Libghdl_Name;

   function Get_Libghdl_Path return String is
   begin
      if Ghdllocal.Exec_Prefix = null then
         --  Compute install path (only once).
         Ghdllocal.Set_Exec_Prefix_From_Program_Name;
      end if;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator
        & Default_Paths.LibDir_Suffix
        & Directory_Separator & Get_Libghdl_Name;
   end Get_Libghdl_Path;

   function Get_Libghdl_Include_Dir return String is
   begin
      --  Compute install path
      Ghdllocal.Set_Exec_Prefix_From_Program_Name;

      return Ghdllocal.Exec_Prefix.all & Directory_Separator
        & Default_Paths.IncDir_Suffix;
   end Get_Libghdl_Include_Dir;

   procedure Register_Commands is
   begin
      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--libghdl-name"),
            Help_Str => new String'
              ("--libghdl-name"
              & ASCII.LF & "  Display libghdl name"),
            Disp => Get_Libghdl_Name'Access));
      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--libghdl-library-path"),
            Help_Str => new String'
              ("--libghdl-library-path"
              & ASCII.LF & "  Display libghdl library path"),
            Disp => Get_Libghdl_Path'Access));
      Register_Command
        (new Command_Str_Disp'
           (Command_Type with
            Cmd_Str => new String'
              ("--libghdl-include-dir"),
            Help_Str => new String'
              ("--libghdl-include-dir"
              & ASCII.LF & "  Display libghdl include directory"),
            Disp => Get_Libghdl_Include_Dir'Access));
   end Register_Commands;
end Ghdllib;
