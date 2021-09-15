--  Routine to dump (for debugging purpose) a netlist.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

package Netlists.Dump is
   --  If set, compact print.
   Flag_Disp_Inline : Boolean := True;

   --  If set, print nets/instances/modules identifier.
   Flag_Disp_Id : Boolean := True;

   Bchar : constant array (Uns32 range 0 .. 3) of Character := "01ZX";

   procedure Put_Id (N : Name_Id);
   procedure Disp_Binary_Digits (Va : Uns32; Zx : Uns32; W : Natural);

   --  Display the digits of binary value PV.
   procedure Disp_Pval_Binary_Digits (Pv : Pval);

   --  Display PV within double quotes.
   procedure Disp_Pval_Binary (Pv : Pval);

   procedure Disp_Pval_String (Pv : Pval);

   procedure Dump_Name (N : Sname);

   procedure Dump_Net_Name (N : Net; With_Id : Boolean := False);
   procedure Disp_Driver (Drv : Net; Indent : Natural);
   procedure Disp_Instance
     (Inst : Instance; With_Name : Boolean; Indent : Natural);

   --  Raw dump.
   procedure Dump_Module (M : Module; Indent : Natural := 0);

   --  More humain readable output.
   procedure Disp_Module (M : Module; Indent : Natural := 0);
end Netlists.Dump;
