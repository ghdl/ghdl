--  Netlist utilities (composed of a few calls).
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

package Netlists.Utils is
   function Get_Nbr_Inputs (Inst : Instance) return Port_Nbr;
   function Get_Nbr_Outputs (Inst : Instance) return Port_Nbr;
   function Get_Nbr_Params (Inst : Instance) return Param_Nbr;

   function Get_Param_Desc
     (Inst : Instance; Param : Param_Idx) return Param_Desc;

   function Get_Id (Inst : Instance) return Module_Id;

   function Get_Input_Name (M : Module; I : Port_Idx) return Sname;
   function Get_Output_Name (M : Module; I : Port_Idx) return Sname;

   --  Return True iff O has at least one sink (ie is connected to at least one
   --  input).
   function Is_Connected (O : Net) return Boolean;

   --  Return True iff O has one sink (is connected to one input).
   function Has_One_Connection (O : Net) return Boolean;

   --  Disconnect input I.  If the driver of I has no output(s) connected,
   --  disconnect and free it.
   procedure Disconnect_And_Free (I : Input);

   --  Unlink all free instances of M.
   procedure Remove_Free_Instances (M : Module);

   --  Unlink all unused instances of M.
   procedure Remove_Unused_Instances (M : Module);
end Netlists.Utils;
