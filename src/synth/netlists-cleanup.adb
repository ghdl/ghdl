--  Netlist cleanup.
--  Copyright (C) 2019 Tristan Gingold
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

with Std_Names;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;
with Netlists.Locations; use Netlists.Locations;

with Synth.Errors; use Synth.Errors;

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

   procedure Remove_Output_Gate (Inst : Instance)
   is
      use Netlists.Gates;
      Inp : constant Input := Get_Input (Inst, 0);
      In_Drv : constant Net := Get_Driver (Inp);
      O : constant Net := Get_Output (Inst, 0);
   begin
      if In_Drv = O then
         --  Connected to itself.
         --  TODO: convert to initial value or to X.
         return;
      end if;

      if In_Drv /= No_Net then
         --  Only when the output is driven.
         Disconnect (Inp);
         Redirect_Inputs (O, In_Drv);
      else
         Disconnect (Get_First_Sink (O));
      end if;

      if Get_Id (Inst) = Id_Ioutput then
         --  Disconnect the initial value.
         Disconnect (Get_Input (Inst, 1));
      end if;

      Remove_Instance (Inst);
   end Remove_Output_Gate;

   procedure Remove_Output_Gates (M : Module)
   is
      use Netlists.Gates;
      Inst : Instance;
      Next_Inst : Instance;
   begin
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         Next_Inst := Get_Next_Instance (Inst);

         case Get_Id (Inst) is
            when Id_Output
              |  Id_Ioutput
              |  Id_Port
              |  Id_Enable
              |  Id_Nop =>
               --  Keep gates with an attribute.
               if not Has_Instance_Attribute (Inst) then
                  Remove_Output_Gate (Inst);
               end if;
            when others =>
               null;
         end case;

         Inst := Next_Inst;
      end loop;
   end Remove_Output_Gates;

   function Has_Keep (Inst : Instance) return Boolean
   is
      Attr : Attribute;
      Val : Pval;
      V, V1 : Logic_32;
   begin
      if not Has_Instance_Attribute (Inst) then
         return False;
      end if;

      Attr := Get_Instance_First_Attribute (Inst);
      while Attr /= No_Attribute loop
         if Get_Attribute_Name (Attr) = Std_Names.Name_Keep then
            Val := Get_Attribute_Pval (Attr);
            case Get_Attribute_Type (Attr) is
               when Param_Pval_Boolean
                  | Param_Pval_Vector =>
                  pragma Assert (Get_Pval_Length (Val) = 1);
                  return Read_Pval (Val, 0) = (1, 0);
               when Param_Pval_String =>
                  if Get_Pval_Length (Val) = 4 * 8 then
                     --  Compare with "true" (case insensitive).
                     V := Read_Pval (Val, 0);
                     V.Val := V.Val and 16#df_df_df_df#;
                     if V = (16#54_52_55_45#, 0) then
                        return True;
                     end if;
                  elsif Get_Pval_Length (Val) = 5 * 8 then
                     --  Compare with "false" (case insensitive).
                     V := Read_Pval (Val, 0);
                     V.Val := V.Val and 16#df_df_df_df#;
                     V1 := Read_Pval (Val, 1);
                     V1.Val := V1.Val and 16#df#;
                     if V = (16#41_4c_53_45#, 0) and then V1 = (16#46#, 0) then
                        return False;
                     end if;
                  end if;
                  Warning_Msg_Synth
                    (Get_Location (Inst),
                     "keep attribute must be 'true' or 'false'");
                  return False;
               when Param_Invalid =>
                  raise Internal_Error;
               when Param_Uns32
                  | Param_Pval_Integer
                  | Param_Pval_Real
                  | Param_Pval_Time_Ps =>
                  raise Internal_Error;
            end case;
         end if;
         Attr := Get_Attribute_Next (Attr);
      end loop;

      return False;
   end Has_Keep;

   procedure Insert_Mark_And_Sweep (Inspect : in out Instance_Tables.Instance;
                                    Inst : Instance) is
   begin
      if not Get_Mark_Flag (Inst) then
         Set_Mark_Flag (Inst, True);
         Instance_Tables.Append (Inspect, Inst);
      end if;
   end Insert_Mark_And_Sweep;

   procedure Mark_And_Sweep (M : Module)
   is
      use Netlists.Gates;
      --  Table of new gates to be inspected.
      Inspect : Instance_Tables.Instance;

      Inst : Instance;
      Inp : Input;
   begin
      Instance_Tables.Init (Inspect, 64);

      --  1.  Check instances are not marked.
      --  1.1 Insert assertion gates.
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         pragma Assert (Get_Mark_Flag (Inst) = False);

         case Get_Id (Inst) is
            when Id_Assert
               | Id_Assume
               | Id_Cover
               | Id_Assert_Cover =>
               Insert_Mark_And_Sweep (Inspect, Inst);
            when Id_User_None
               | Id_User_Parameters =>
               --  Always keep user modules.
               Insert_Mark_And_Sweep (Inspect, Inst);
            when Id_Signal
              | Id_Isignal =>
               if Has_Keep (Inst) then
                  Insert_Mark_And_Sweep (Inspect, Inst);
               end if;
            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;

      --  1.2 Insert output drivers.
      --      This corresponds to the input of the self instance.
      Insert_Mark_And_Sweep (Inspect, Get_Self_Instance (M));

      --  2. While the table is not empty, extract an element and insert
      --     all the input drivers.
      loop
         declare
            Idx : Int32;
            N : Net;
         begin
            Idx := Instance_Tables.Last (Inspect);
            exit when Idx < Instance_Tables.First;
            Inst := Inspect.Table (Idx);
            Instance_Tables.Decrement_Last (Inspect);

            for I in 1 .. Get_Nbr_Inputs (Inst) loop
               N := Get_Input_Net (Inst, I - 1);
               if N /= No_Net then
                  Insert_Mark_And_Sweep (Inspect, Get_Net_Parent (N));
               end if;
            end loop;

            case Get_Id (Inst) is
               when Id_Mem_Rd
                 | Id_Mem_Rd_Sync =>
                  --  When a memory read port is found, mark the whole
                  --  memory.
                  --  FIXME: free unused read ports.
                  declare
                     Inp : Input;
                  begin
                     loop
                        N := Get_Output (Inst, 0);
                        Inp := Get_First_Sink (N);
                        exit when Inp = No_Input;
                        pragma Assert (Get_Next_Sink (Inp) = No_Input);
                        Inst := Get_Input_Parent (Inp);
                        exit when Get_Mark_Flag (Inst);
                        Insert_Mark_And_Sweep (Inspect, Inst);
                        N := Get_Output (Inst, 0);
                     end loop;
                  end;
               when others =>
                  null;
            end case;
         end;
      end loop;

      --  3.  Remove unused instances; unmark used instances.
      Instance_Tables.Free (Inspect);
      declare
         Next_Inst : Instance;
         First_Unused : Instance;
         Last_Unused : Instance;
      begin
         First_Unused := No_Instance;
         Last_Unused := No_Instance;

         Extract_All_Instances (M, Inst);
         --  But keep the self-instance.
         pragma Assert (Get_Mark_Flag (Inst));
         Set_Mark_Flag (Inst, False);
         Next_Inst := Get_Next_Instance (Inst);
         Append_Instance (M, Inst);
         Inst := Next_Inst;
         while Inst /= No_Instance loop
            Next_Inst := Get_Next_Instance (Inst);
            if Get_Mark_Flag (Inst) then
               --  Instance was marked, keep it.
               Set_Mark_Flag (Inst, False);
               Append_Instance (M, Inst);
            else
               --  Instance was not marked, disconnect it.
               for I in 1 .. Get_Nbr_Inputs (Inst) loop
                  Inp := Get_Input (Inst, I - 1);
                  if Get_Driver (Inp) /= No_Net then
                     --  Disconnect the input.
                     Disconnect (Inp);
                  end if;
               end loop;

               if First_Unused = No_Instance then
                  First_Unused := Inst;
               else
                  Set_Next_Instance (Last_Unused, Inst);
               end if;
               Last_Unused := Inst;
            end if;
            Inst := Next_Inst;
         end loop;

         --  Free unused instances.  This must be done at the end so that
         --  their outputs are disconnected.
         if First_Unused /= No_Instance then
            Set_Next_Instance (Last_Unused, No_Instance);
            loop
               Inst := First_Unused;
               exit when Inst = No_Instance;
               First_Unused := Get_Next_Instance (Inst);
               Set_Next_Instance (Inst, No_Instance);
               Set_Prev_Instance (Inst, No_Instance);
               Free_Instance (Inst);
            end loop;
         end if;
      end;
   end Mark_And_Sweep;

end Netlists.Cleanup;
