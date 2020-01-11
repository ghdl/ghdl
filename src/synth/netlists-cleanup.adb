--  Netlist cleanup.
--  Copyright (C) 2019 Tristan Gingold
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

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;

package body Netlists.Cleanup is
   --  Return False iff INST has no outputs (and INST is not Id_Free).
   --  Return True iff all outputs of INST are unconnected.
   --  Return False otherwise.
   function Is_Unused_Instance (Inst : Instance) return Boolean
   is
      Nbr_Outputs : constant Port_Idx := Get_Nbr_Outputs (Inst);
      N : Net;
   begin
      --  An instance without outputs is considered as used.
      if Nbr_Outputs = 0 then
         return Get_Module (Inst) = Free_Module;
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

   --  Move INST on LIST iff INST is unused.
   procedure Extract_If_Unused (Inst : Instance; List : in out Instance) is
   begin
      if Is_Unused_Instance (Inst) then
         Extract_Instance (Inst);
         Set_Next_Instance (Inst, List);
         List := Inst;
      end if;
   end Extract_If_Unused;

   --  TODO: use mark & sweep to remove unused assignments ?
   procedure Remove_Unconnected_Instances (M : Module)
   is
      Inst : Instance;
      Next_Inst : Instance;
      List : Instance;
   begin
      --  Extract from instances of M all unconnected instances.  Put them
      --  in a list.
      List := No_Instance;

      Inst := Get_First_Instance (M);
      --  But keep the self-instance.
      Inst := Get_Next_Instance (Inst);
      while Inst /= No_Instance loop
         Next_Inst := Get_Next_Instance (Inst);
         Extract_If_Unused (Inst, List);
         Inst := Next_Inst;
      end loop;

      --  While the list is not empty:
      --    extract the first instance of the list.
      --    unconnect all inputs of the instance, free the instance.
      --    if unconnected an input resulted in an instance without connected
      --      output, extract it from M and append it in the list.
      while List /= No_Instance loop
         Inst := List;
         List := Get_Next_Instance (Inst);

         declare
            Nbr_Inputs : constant Port_Nbr := Get_Nbr_Inputs (Inst);
            Inp : Input;
            Drv : Net;
            Inst2 : Instance;
         begin
            if Nbr_Inputs > 0 then
               for K in 0 .. Nbr_Inputs - 1 loop
                  Inp := Get_Input (Inst, K);
                  Drv := Get_Driver (Inp);
                  if Drv /= No_Net then
                     --  Disconnect the input.
                     Disconnect (Inp);
                     --  Possibly consider the driver as unconnected if was
                     --  the last input connected.
                     if Get_First_Sink (Drv) = No_Input then
                        Inst2 := Get_Net_Parent (Drv);
                        Extract_If_Unused (Inst2, List);
                     end if;
                  end if;
               end loop;
            end if;
         end;

         Free_Instance (Inst);
      end loop;
   end Remove_Unconnected_Instances;

   procedure Remove_Output_Gates (M : Module)
   is
      use Netlists.Gates;
      Inst : Instance;
      Next_Inst : Instance;
      Inp : Input;
      O : Net;
   begin
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         Next_Inst := Get_Next_Instance (Inst);

         if Get_Id (Inst) = Id_Output then
            Inp := Get_Input (Inst, 0);
            O := Get_Driver (Inp);
            if O /= No_Net then
               --  Only when the output is driven.
               Disconnect (Inp);
               Redirect_Inputs (Get_Output (Inst, 0), O);
            end if;
            Remove_Instance (Inst);
         end if;

         Inst := Next_Inst;
      end loop;
   end Remove_Output_Gates;

end Netlists.Cleanup;
