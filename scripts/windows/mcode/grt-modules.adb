--  GHDL Run Time (GRT) -  Modules.
--  Copyright (C) 2005 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
with Grt.Vcd;
with Grt.Vpi;
with Grt.Waves;
with Grt.Vital_Annotate;
with Grt.Disp_Tree;
with Grt.Disp_Rti;
with Grt.Backtraces;

package body Grt.Modules is
   procedure Register_Modules is
   begin
      --  List of modules to be registered.
      Grt.Disp_Tree.Register;
      Grt.Vcd.Register;
      Grt.Waves.Register;
      Grt.Vpi.Register;
      Grt.Vital_Annotate.Register;
      Grt.Disp_Rti.Register;
      Grt.Backtraces.Register;
   end Register_Modules;
end Grt.Modules;
