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

with Ada.Unchecked_Deallocation;

with Dyn_Tables;

package Netlists.Utils is
   type Net_Array_Acc is access Net_Array;
   procedure Free_Net_Array is new Ada.Unchecked_Deallocation
     (Net_Array, Net_Array_Acc);

   function Get_Nbr_Inputs (Inst : Instance) return Port_Nbr;
   function Get_Nbr_Outputs (Inst : Instance) return Port_Nbr;
   function Get_Nbr_Params (Inst : Instance) return Param_Nbr;

   function Get_Param_Desc
     (Inst : Instance; Param : Param_Idx) return Param_Desc;

   function Get_Id (Inst : Instance) return Module_Id;

   function Get_Input_Name (M : Module; I : Port_Idx) return Sname;
   function Get_Output_Name (M : Module; I : Port_Idx) return Sname;

   function Get_Input_Width (M : Module; I : Port_Idx) return Width;
   function Get_Output_Width (M : Module; I : Port_Idx) return Width;

   --  Return the net (driver) connected to input IDX of INSTANCE.
   function Get_Input_Net (Inst : Instance; Idx : Port_Idx) return Net;

   --  Return the instance that drives input IDX of INST.
   function Get_Input_Instance (Inst : Instance; Idx : Port_Idx)
                               return Instance;

   --  Return True iff ID describe a constant.
   function Is_Const_Module (Id : Module_Id) return Boolean;
   function Is_Const_Net (N : Net) return Boolean;

   --  Assuming that N is a const net, return the value (for small values).
   function Get_Net_Uns64 (N : Net) return Uns64;

   function Get_Net_Int64 (N : Net) return Int64;
   pragma Inline (Get_Net_Int64);

   --  Assuming that N is a const net, return the value at offset OFF.
   procedure Get_Net_Element
     (N : Net; Off : Uns32; Va : out Uns32; Zx : out Uns32);

   --  Return True iff O has at least one sink (ie is connected to at least one
   --  input).
   function Is_Connected (O : Net) return Boolean;

   --  Return True iff O has one sink (is connected to one input).
   function Has_One_Connection (O : Net) return Boolean;

   --  Disconnect input I.  If the driver of I has no output(s) connected,
   --  disconnect and free it.
   procedure Disconnect_And_Free (I : Input);

   function Clog2 (W : Width) return Width;

   --  Used at many places.
   package Net_Tables is new Dyn_Tables
     (Table_Component_Type => Net,
      Table_Index_Type => Int32,
      Table_Low_Bound => 1);

   package Instance_Tables is new Dyn_Tables
     (Table_Component_Type => Instance,
      Table_Index_Type => Int32,
      Table_Low_Bound => 1);
end Netlists.Utils;
