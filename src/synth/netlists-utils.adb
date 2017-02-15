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

package body Netlists.Utils is
   function Get_Nbr_Inputs (Inst : Instance) return Port_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      if Is_Self_Instance (Inst) then
         return Get_Nbr_Outputs (M);
      else
         return Get_Nbr_Inputs (M);
      end if;
   end Get_Nbr_Inputs;

   function Get_Nbr_Outputs (Inst : Instance) return Port_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      if Is_Self_Instance (Inst) then
         return Get_Nbr_Inputs (M);
      else
         return Get_Nbr_Outputs (M);
      end if;
   end Get_Nbr_Outputs;

   function Get_Nbr_Params (Inst : Instance) return Param_Nbr
   is
      M : constant Module := Get_Module (Inst);
   begin
      return Get_Nbr_Params (M);
   end Get_Nbr_Params;

   function Get_Param_Desc
     (Inst : Instance; Param : Param_Idx) return Param_Desc is
   begin
      return Get_Param_Desc (Get_Module (Inst), Param);
   end Get_Param_Desc;

   function Get_Id (Inst : Instance) return Module_Id is
   begin
      return Get_Id (Get_Module (Inst));
   end Get_Id;

   function Get_Input_Name (M : Module; I : Port_Idx) return Sname is
   begin
      return Get_Input_Desc (M, I).Name;
   end Get_Input_Name;

   function Get_Output_Name (M : Module; I : Port_Idx) return Sname is
   begin
      return Get_Output_Desc (M, I).Name;
   end Get_Output_Name;

   function Is_Connected (O : Net) return Boolean is
   begin
      return Get_First_Sink (O) /= No_Input;
   end Is_Connected;

   function Has_One_Connection (O : Net) return Boolean
   is
      Inp : Input;
   begin
      Inp := Get_First_Sink (O);
      if Inp = No_Input then
         --  No connection.
         return False;
      end if;
      Inp := Get_Next_Sink (Inp);
      return Inp = No_Input;
   end Has_One_Connection;

   procedure Disconnect_And_Free (I : Input)
   is
      I_Net : constant Net := Get_Driver (I);
      Inst : constant Instance := Get_Net_Parent (I_Net);
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      --  First disconnect.
      Disconnect (I);

      --  Quick check: is output (of I) still used ?
      if Is_Connected (I_Net) then
         return;
      end if;

      --  Check that all outputs are unused.
      Nbr_Outputs := Get_Nbr_Outputs (Inst);
      if Nbr_Outputs > 1 then
         for K in 0 .. Nbr_Outputs - 1 loop
            if Is_Connected (Get_Output (Inst, K)) then
               return;
            end if;
         end loop;
      end if;

      --  First disconnect inputs.
      Nbr_Inputs := Get_Nbr_Inputs (Inst);
      if Nbr_Inputs > 0 then
         for K in 0 .. Nbr_Inputs - 1 loop
            Disconnect_And_Free (Get_Input (Inst, K));
         end loop;
      end if;

      --  Free Inst
      Free_Instance (Inst);
   end Disconnect_And_Free;

   function Is_Unused_Instance (Inst : Instance) return Boolean
   is
      Nbr_Outputs : constant Port_Idx := Get_Nbr_Outputs (Inst);
      N : Net;
   begin
      --  An instance without outputs is considered as used.
      if Nbr_Outputs = 0 then
         return False;
      end if;

      for Idx in 0 .. Nbr_Outputs - 1 loop
         N := Get_Output (Inst, Idx);
         if Is_Connected (N) then
            --  Connected output.
            return False;
         end if;
      end loop;

      --  All outputs are unconnected.
      return True;
   end Is_Unused_Instance;

   procedure Remove_Unused_Instances (M : Module)
   is
      pragma Assert (Is_Valid (M));
      Inst : Instance;
   begin
      Extract_All_Instances (M, Inst);

      --  Add the self instance (the first one).
      Append_Instance (M, Inst);
      Inst := Get_Next_Instance (Inst);

      while Inst /= No_Instance loop
         if not (Get_Id (Inst) = Id_Free
                   or else Is_Unused_Instance (Inst))
         then
            --  Keep this used instance.
            Append_Instance (M, Inst);
         end if;
         Inst := Get_Next_Instance (Inst);
      end loop;
   end Remove_Unused_Instances;

   procedure Remove_Free_Instances (M : Module)
   is
      pragma Assert (Is_Valid (M));
      Inst : Instance;
   begin
      Extract_All_Instances (M, Inst);

      while Inst /= No_Instance loop
         if Get_Id (Inst) /= Id_Free then
            Append_Instance (M, Inst);
         end if;
         Inst := Get_Next_Instance (Inst);
      end loop;
   end Remove_Free_Instances;
end Netlists.Utils;
