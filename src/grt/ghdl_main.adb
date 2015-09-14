--  GHDL Run Time (GRT) entry point.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Ada.Unchecked_Conversion;
with Grt.Options; use Grt.Options;
with Grt.Main;
with Grt.Types; use Grt.Types;

--  Some files are only referenced from compiled code.  With it here so that
--  they get compiled during build (and elaborated).
pragma Warnings (Off);
with Grt.Rtis_Binding;
with Grt.Std_Logic_1164;
with Grt.Errors;
pragma Warnings (On);

function Ghdl_Main (Argc : Integer; Argv : System.Address) return Integer
is
   --  Grt_Init corresponds to the 'adainit' subprogram for grt.
   procedure Grt_Init;
   pragma Import (C, Grt_Init, "grt_init");

   function To_Argv_Type is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Grt.Options.Argv_Type);

   Default_Progname : constant String := "ghdl_design" & NUL;
begin
   --  Set program name.
   if Argc > 0 then
      Grt.Options.Progname := To_Argv_Type (Argv)(0);
   else
      Grt.Options.Progname := To_Ghdl_C_String (Default_Progname'Address);
   end if;
   Grt.Options.Argc := Argc;
   Grt.Options.Argv := To_Argv_Type (Argv);

   Grt_Init;
   Grt.Main.Run;
   return Grt.Errors.Exit_Status;
end Ghdl_Main;
