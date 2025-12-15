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
with Simple_IO;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Concats;

with Synth.Errors; use Synth.Errors;
with Synth.Flags;

package body Netlists.Cleanup is
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

      Set_Location (Inst, No_Location);
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

      Num : Uns32;

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
      Num := 0;
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
               Num := Num + 1;
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

      if Synth.Flags.Flag_Debug_Stats then
         Simple_IO.Put_Line_Err
           ("clean up:" & Uns32'Image(Num) & " instances removed");
      end if;
   end Mark_And_Sweep;

   procedure Replace_Concat_Null_Inputs (Ctxt : Context_Acc; Inst : Instance)
   is
      use Netlists.Concats;
      Conc : Concat_Type;
      Inp : Input;
      Drv : Net;
      Res : Net;
   begin
      for I in reverse 1 .. Get_Nbr_Inputs (Inst) loop
         Inp := Get_Input (Inst, I - 1);
         Drv := Get_Driver (Inp);
         if Drv /= No_Net then
            Disconnect (Inp);
            if Get_Width (Drv) /= 0 then
               Append (Conc, Drv);
            end if;
         end if;
      end loop;

      Build (Ctxt, Conc, Get_Location (Inst), Res);
      Redirect_Inputs (Get_Output (Inst, 0), Res);
      Set_Location (Inst, No_Location);
      Remove_Instance (Inst);
   end Replace_Concat_Null_Inputs;

   procedure Replace_Null_Inputs (Ctxt : Context_Acc; M : Module)
   is
      Inst, Next_Inst : Instance;
      Drv : Net;
      Inp : Input;
      Null_X : Net;
      Has_Null : Boolean;
   begin
      Null_X := No_Net;

      --  For each sub-instance, including the self one (in order to deal
      --  with null outputs):
      Inst := Get_Self_Instance (M);
      while Inst /= No_Instance loop
         Has_Null := False;
         Next_Inst := Get_Next_Instance (Inst);

         --  For each inputs of the sub-instance:
         for I in 1 .. Get_Nbr_Inputs (Inst) loop
            Inp := Get_Input (Inst, I - 1);
            Drv := Get_Driver (Inp);
            if Drv /= No_Net and then Get_Width (Drv) = 0 then
               if Null_X = No_Net then
                  Null_X := Build_Const_X (Ctxt, 0);
               end if;
               Disconnect (Inp);
               Connect (Inp, Null_X);
               Has_Null := True;
            end if;
         end loop;

         if Has_Null and then Get_Id (Inst) in Gates.Concat_Module_Id then
            Replace_Concat_Null_Inputs (Ctxt, Inst);
         end if;

         Inst := Next_Inst;
      end loop;
   end Replace_Null_Inputs;

end Netlists.Cleanup;
