--  GHDL Run Time (GRT) -  entry point.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Grt.Types; use Grt.Types;
with Grt.Options;

package Grt.Main is
   --  Set options.
   procedure Run_Options (Progname : Ghdl_C_String;
                          Argc : Integer;
                          Argv : Grt.Options.Argv_Type);
   pragma Export (C, Run_Options, "grt_main_options");

   --  Do everything: initialize, elaborate and simulate the design.
   procedure Run;

   --  What Run does.

   --  Initialize and elaborate the design.  Return False in case of error.
   function Run_Elab return C_Boolean;
   pragma Export (C, Run_Elab, "grt_main_elab");

   --  Do the whole simulation.
   function Run_Simul return Integer;

   --  Finalization.
   procedure Run_Finish (Status : Integer);

   --  This function is called by elaboration code once default values have
   --  been assigned to generics, but before being used.
   procedure Ghdl_Init_Top_Generics;
   pragma Export (C, Ghdl_Init_Top_Generics, "__ghdl_init_top_generics");

   type Run_Handler is access function return Integer;

   --  Run HAND through a wrapper that catch some errors (in particular on
   --  windows).  Returns < 0 in case of error.
   function Run_Through_Longjump (Hand : Run_Handler) return Integer;
   pragma Import (Ada, Run_Through_Longjump, "__ghdl_run_through_longjump");
end Grt.Main;
