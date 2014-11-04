--  GCC back-end for ortho
--  Copyright (C) 2002-1014 Tristan Gingold
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
