--  GHDL Run Time (GRT) -  entry point.
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

package Grt.Main is
   --  Elaborate and simulate the design.
   procedure Run;

   --  What Run does.

   --  Elaborate the design.
   procedure Run_Elab (Stop : out Boolean);

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
