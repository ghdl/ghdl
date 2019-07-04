--  Ghdlsynth as a library.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Ghdlmain; use Ghdlmain;
with Ghdlsynth;
with Options; use Options;
with Errorout.Console;

package body Libghdlsynth is
   function Synth (Argc : Natural; Argv : C_String_Array_Acc) return Module
   is
      Args : Argument_List (1 .. Argc);
      Res : Module;
      Cmd : Command_Acc;
      First_Arg : Natural;
   begin
      --  Create arguments list.
      for I in 0 .. Argc - 1 loop
         declare
            Arg : constant Ghdl_C_String := Argv (I);
         begin
            Args (I + 1) := new String'(Arg (1 .. strlen (Arg)));
         end;
      end loop;

      --  Find the command.  This is a little bit convoluted...
      Decode_Command_Options ("--synth", Cmd, Args, First_Arg);

      --  Do the real work!
      Res := Ghdlsynth.Ghdl_Synth (Args (First_Arg .. Args'Last));

      return Res;
   exception
      when Option_Error =>
         return No_Module;
      when others =>
         --  Avoid possible issues with exceptions...
         return No_Module;
   end Synth;

   Gnat_Version : constant String := "unknown compiler version" & ASCII.NUL;
   pragma Export (C, Gnat_Version, "__gnat_version");
begin
   Ghdlsynth.Register_Commands;
   Options.Initialize;
   Errorout.Console.Install_Handler;
end Libghdlsynth;
