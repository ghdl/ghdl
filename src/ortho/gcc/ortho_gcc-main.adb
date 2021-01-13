--  GCC back-end for ortho
--  Copyright (C) 2002-1014 Tristan Gingold
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
with Ortho_Gcc_Front;
with Ada.Command_Line; use Ada.Command_Line;

procedure Ortho_Gcc.Main
is
   gnat_argc : Integer;
   gnat_argv : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);

   function Toplev_Main (Argc : Integer; Argv : System.Address)
                        return Integer;
   pragma Import (C, Toplev_Main, "toplev_main_c");

   Status : Exit_Status;
begin
   Ortho_Gcc_Front.Init;

   --  Note: GCC set signal handlers...
   Status := Exit_Status (Toplev_Main (gnat_argc, gnat_argv));
   Set_Exit_Status (Status);
end Ortho_Gcc.Main;
