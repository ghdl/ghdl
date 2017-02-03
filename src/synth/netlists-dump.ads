--  Routine to dump (for debugging purpose) a netlist.
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

package Netlists.Dump is
   procedure Dump_Net_Name (N : Net; With_Id : Boolean := False);
   procedure Disp_Driver (Drv : Net);
   procedure Disp_Instance (Inst : Instance; With_Name : Boolean);

   --  Raw dump.
   procedure Dump_Module (M : Module; Indent : Natural := 0);

   --  More humain readable output.
   procedure Disp_Module (M : Module; Indent : Natural := 0);
end Netlists.Dump;
