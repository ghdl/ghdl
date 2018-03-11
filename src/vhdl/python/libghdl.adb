--  Library interface for the analyzer.
--  Copyright (C) 2017 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ghdllocal;
with Ghdlcomp;

package body Libghdl is
   function Set_Option (Opt : Thin_String_Ptr; Len : Natural) return Integer is
   begin
      if Ghdllocal.Decode_Driver_Option (Opt (1 .. Len)) then
         --  Ok.
         return 0;
      else
         --  Error.
         return 1;
      end if;
   end Set_Option;

   procedure Compile_Init (Analyze_Only : Boolean) is
   begin
      if Analyze_Only then
         return;
      end if;

      Ghdllocal.Setup_Libraries (True);
   end Compile_Init;

   procedure Compile_Elab
     (Cmd_Name : String; Args : Argument_List; Opt_Arg : out Natural) is
   begin
      null;
   end Compile_Elab;

   --  Set options.
   procedure Set_Run_Options (Args : Argument_List) is
   begin
      null;
   end Set_Run_Options;

   procedure Run is
   begin
      null;
   end Run;

   function Decode_Option (Option : String) return Boolean
   is
      pragma Unreferenced (Option);
   begin
      return False;
   end Decode_Option;

   procedure Disp_Long_Help is
   begin
      null;
   end Disp_Long_Help;

   procedure Set_Hooks is
   begin
      Ghdlcomp.Hooks := (Compile_Init'Access,
                         Compile_Elab'Access,
                         Set_Run_Options'Access,
                         Run'Access,
                         Decode_Option'Access,
                         Disp_Long_Help'Access);
   end Set_Hooks;

   procedure Analyze_Init is
   begin
      --  Load libraries...
      Compile_Init (False);
   end Analyze_Init;

   function Analyze_File (File : Thin_String_Ptr; Len : Natural) return Iir is
   begin
      return Ghdlcomp.Compile_Analyze_File2 (File (1 .. Len));
   end Analyze_File;

   Gnat_Version : constant String := "unknown compiler version" & ASCII.NUL;
   pragma Export (C, Gnat_Version, "__gnat_version");
begin
   --  TODO: set program name.
   Ghdllocal.Compile_Init;
   Set_Hooks;
end Libghdl;
