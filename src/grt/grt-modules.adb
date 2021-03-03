--  GHDL Run Time (GRT) -  Modules.
--  Copyright (C) 2005 - 2014 Tristan Gingold
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
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Vcd;
with Grt.Vcdz;
with Grt.Vpi;
with Grt.Vhpi;
with Grt.Fst;
with Grt.Waves;
with Grt.Vital_Annotate;
with Grt.Disp_Tree;
with Grt.Disp_Rti;
with Grt.Psl;
with Grt.Backtraces;

package body Grt.Modules is
   procedure Register_Modules is
   begin
      --  List of modules to be registered.
      Grt.Disp_Tree.Register;
      Grt.Vcd.Register;
      Grt.Vcdz.Register;
      Grt.Waves.Register;
      Grt.Fst.Register;
      Grt.Vpi.Register;
      Grt.Vhpi.Register;
      Grt.Vital_Annotate.Register;
      Grt.Disp_Rti.Register;
      Grt.Psl.Register;
      Grt.Backtraces.Register;
   end Register_Modules;
end Grt.Modules;
