--  Inference in synthesis.
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

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Gates_Ports; use Netlists.Gates_Ports;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Errors; use Netlists.Errors;
with Netlists.Internings;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Memories; use Netlists.Memories;

with Synth.Errors; use Synth.Errors;
with Synth.Flags;

package body Netlists.Inference is
   --  DFF inference.
   --  As an initial implementation, the following 'styles' must be
   --  supported:
   --  Note: rising_edge is any clock_edge; '<=' can be ':='.
   --
   --  1)
   --  if rising_edge(clk) then
   --    r <= x;
   --  end if;
   --
   --  2)
   --  if rst = '0' then
   --    r <= x;
   --  elsif rising_edge (clk) then
   --    r <= y;
   --  end if;
   --
   --  3)
   --  wait until rising_edge(clk);
   --   r <= x;
   --  Which is equivalent to 1) when the wait statement is the only and first
   --  statement, as it can be converted to an if statement.
   --
   --  Netlist derived from 1)
   --      +------+
   --      |      |
   --      |   /| |
   --      |  |0+-+
   --  Q --+--+ |
   --         |1+--- D
   --          \|
   --         CLK
   --  This is a memorizing element as there is a loop, the value is changed
   --  to D on a rising edge of the clock.
   --
   --  Netlist derived from 2)
   --      +------------+
   --      |         /| |
   --      |   /|   |0+-+
   --      |  |0+---+ |
   --  Q --+--+ |   |1+----- D
   --         |1+-+  \|
   --          \| | CLK
   --         RST +--------- '0'
   --  This is a memorizing element as there is a loop.  It is an asynchronous
   --  reset as Q is forced to '0' when RST is asserted.

   function Has_Clock (N : Net) return Boolean
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      case Get_Id (Inst) is
         when Edge_Module_Id =>
            return True;
         when Id_And =>
            --  Assume the condition is canonicalized, ie of the form:
            --  CLK and EXPR.
            --  FIXME: do it!
            return Has_Clock (Get_Input_Net (Inst, 0));
         when others =>
            return False;
      end case;
   end Has_Clock;

   --  Find the longest chain of mux starting from VAL with a final input
   --  of PREV_VAL.  Such a chain means this is a memorising element.
   --  RES is the last mux in the chain, DIST the number of mux in the chain.
   procedure Find_Longest_Loop
     (Val : Net; Prev_Val : Net; Res : out Instance; Dist : out Integer)
   is
      Inst : constant Instance := Get_Net_Parent (Val);
   begin
      if Get_Id (Inst) = Id_Mux2 then
         declare
            Res0, Res1 : Instance;
            Dist0, Dist1 : Integer;
         begin
            if Has_Clock (Get_Driver (Get_Mux2_Sel (Inst))) then
               Res := Inst;
               Dist := 1;
            else
               Find_Longest_Loop
                 (Get_Driver (Get_Mux2_I0 (Inst)), Prev_Val, Res0, Dist0);
               Find_Longest_Loop
                 (Get_Driver (Get_Mux2_I1 (Inst)), Prev_Val, Res1, Dist1);
               --  Input1 has an higher priority than input0 in case
               --  the selector is a clock.
               --  FIXME: improve algorithm.
               if Dist1 > Dist0 then
                  Dist := Dist1 + 1;
                  if Dist1 > 0 then
                     Res := Res1;
                  else
                     Res := Inst;
                  end if;
               elsif Dist0 >= 0 then
                  Dist := Dist0 + 1;
                  if Dist0 > 0 then
                     Res := Res0;
                  else
                     Res := Inst;
                  end if;
               else
                  pragma Assert (Dist1 < 0 and Dist0 < 0);
                  Res := No_Instance;
                  Dist := -1;
               end if;
            end if;
         end;
      elsif Val = Prev_Val then
         Res := No_Instance;
         Dist := 0;
      else
         Res := No_Instance;
         Dist := -1;
      end if;
   end Find_Longest_Loop;

   procedure Extract_Clock_And (Ctxt : Context_Acc; Inst : Instance)
   is
   begin
      pragma Assert (Get_Id (Inst) = Id_And);

      declare
         I0 : constant Input := Get_Input (Inst, 0);
         N0 : constant Net := Get_Driver (I0);
         Inst0 : constant Instance := Get_Net_Parent (N0);
      begin
         case Get_Id (Inst0) is
            when Edge_Module_Id =>
               null;
            when Id_And =>
               Extract_Clock_And (Ctxt, Inst0);

               --  If we have:       AND      convert to:     AND
               --                    / \                      / \
               --                  N1  AND0       ==>     AND0   EDGE
               --                      /  \               /  \
               --                     N2  EDGE           N1   N2
               declare
                  I3 : constant Input := Get_Input (Inst0, 0);
                  N3 : constant Net := Get_Driver (I3);
                  Inst3 : constant Instance := Get_Net_Parent (N3);
               begin
                  if Get_Id (Inst3) in Edge_Module_Id then
                     declare
                        Can_Rotate : constant Boolean :=
                          Has_One_Connection (N0);
                        I2 : constant Input := Get_Input (Inst0, 1);
                        N2 : constant Net := Get_Driver (I2);
                        I1 : constant Input := Get_Input (Inst, 1);
                        N1 : constant Net := Get_Driver (I1);
                        N4 : Net;
                     begin
                        Disconnect (I0);
                        Disconnect (I1);
                        Connect (I0, N3);
                        if Can_Rotate then
                           Disconnect (I2);
                           Disconnect (I3);

                           Connect (I1, N0);
                           Connect (I3, N2);
                           Connect (I2, N1);
                        else
                           N4 := Build_Dyadic (Ctxt, Id_And, N2, N1);
                           Copy_Location (N4, Inst);
                           Connect (I1, N4);
                        end if;
                     end;
                  end if;
               end;
            when others =>
               null;
         end case;
      end;

      declare
         I0 : constant Input := Get_Input (Inst, 1);
         N0 : constant Net := Get_Driver (I0);
         Inst0 : constant Instance := Get_Net_Parent (N0);
      begin
         case Get_Id (Inst0) is
            when Edge_Module_Id =>
               --  Swap inputs 0 and 1.
               declare
                  I1 : constant Input := Get_Input (Inst, 0);
                  N1 : constant Net := Get_Driver (I1);
               begin
                  Disconnect (I0);
                  Disconnect (I1);
                  Connect (I1, N0);
                  Connect (I0, N1);
               end;
            when Id_And =>
               Extract_Clock_And (Ctxt, Inst0);

               --  If we have:       AND      convert to:     AND
               --                    / \                      / \
               --                 AND0  N1     ==>         AND0  EDGE
               --                 /  \                     /  \
               --                N2  EDGE                N2   N1
               declare
                  I3 : constant Input := Get_Input (Inst0, 0);
                  N3 : constant Net := Get_Driver (I3);
               begin
                  if Get_Id (Get_Net_Parent (N3)) in Edge_Module_Id then
                     declare
                        Can_Rotate : constant Boolean :=
                          Has_One_Connection (N0);
                        I1 : constant Input := Get_Input (Inst, 0);
                        N1 : constant Net := Get_Driver (I1);
                        N4 : Net;
                     begin
                        Disconnect (I3);
                        Disconnect (I1);
                        Connect (I1, N3);
                        if Can_Rotate then
                           Connect (I3, N1);
                        else
                           N4 := Build_Dyadic
                             (Ctxt, Id_And, N1, Get_Input_Net (Inst0, 1));
                           Connect (I3, N4);
                        end if;
                     end;
                  end if;
               end;
            when others =>
               null;
         end case;
      end;
   end Extract_Clock_And;

   --  Walk the And-net N, and extract clock (posedge/negedge) if found.
   --  ENABLE is N without the clock.
   --  If not found, CLK and ENABLE are set to No_Net.
   procedure Extract_Clock
     (Ctxt : Context_Acc; N : Net; Clk : out Net; Enable : out Net)
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      Clk := No_Net;
      Enable := No_Net;

      case Get_Id (Inst) is
         when Edge_Module_Id =>
            Clk := N;
         when Id_And =>
            --  Canonicalize conditions.
            Extract_Clock_And (Ctxt, Inst);

            --  Condition should be in the form: CLK and EXPR
            declare
               I0 : constant Net := Get_Input_Net (Inst, 0);
               Inst0 : constant Instance := Get_Net_Parent (I0);
            begin
               if Get_Id (Inst0) in Edge_Module_Id then
                  --  INST is clearly not synthesizable (boolean operation on
                  --  an edge).  Will be removed at the end by
                  --  remove_unused_instances.  Do not remove it now as its
                  --  output may be used by other nets.
                  Clk := I0;
                  Enable := Get_Input_Net (Inst, 1);
                  return;
               end if;
            end;
         when others =>
            null;
      end case;
   end Extract_Clock;

   function Is_Prev_FF_Value (V : Net; Prev_Val : Net; Off : Uns32)
                             return Boolean
   is
      Inst : Instance;
   begin
      if V = Prev_Val then
         pragma Assert (Off = 0);
         return True;
      end if;
      Inst := Get_Net_Parent (V);
      return Get_Id (Inst) = Id_Extract
        and then Get_Param_Uns32 (Inst, 0) = Off
        and then Get_Input_Net (Inst, 0) = Prev_Val;
   end Is_Prev_FF_Value;

   --  Build the FF or the RAM according to the inputs.
   function Infere_FF_Create (Ctxt       : Context_Acc;
                              Prev_Val   : Net;
                              Off        : Uns32;
                              Last_Mux   : Instance;
                              Init       : Net;
                              Rst        : Net;
                              Rst_Val    : Net;
                              Data       : Net;
                              Els        : Net;
                              Clk        : Net;
                              Clk_Enable : Net;
                              Loc        : Location_Type) return Net
   is
      Ndata : Net;
      Res   : Net;
      Els_Net : Net;
      Els_Inst : Instance;
   begin
      Els_Net := Els;
      pragma Unreferenced (Els);

      if Off = 0
        and then Rst = No_Net
        and then not Synth.Flags.Flag_Debug_Nomemory1
        and then Can_Infere_RAM (Data, Prev_Val)
      then
         --  Maybe it is a RAM.
         Res := Infere_RAM (Ctxt, Data, Els_Net, Clk, Clk_Enable);
      else
         if Clk_Enable /= No_Net then
            --  If there is a condition with the clock, that's an enable which
            --  keep the previous value if the condition is false.  Add the mux
            --  for it, to create a synchronous enable.
            declare
               Prev : Net;
            begin
               Prev := Build2_Extract (Ctxt, Prev_Val, Off, Get_Width (Data));

               Ndata := Build_Mux2 (Ctxt, Clk_Enable, Prev, Data);
               Set_Location (Ndata, Loc);
            end;
         else
            Ndata := Data;
         end if;

         --  Create the FF.
         if Rst = No_Net then
            --  No async reset
            pragma Assert (Rst_Val = No_Net);

            if Els_Net /= No_Net then
               Els_Inst := Get_Net_Parent (Els_Net);
               if Get_Id (Els_Inst) in Dff_Module_Id
                 and then Same_Clock (Clk, Get_Input_Net (Els_Inst, 0))
               then
                  Els_Net := No_Net;
               end if;
            end if;

            if Els_Net = No_Net then
               if Init /= No_Net then
                  Res := Build_Idff (Ctxt, Clk, D => Ndata, Init => Init);
               else
                  Res := Build_Dff (Ctxt, Clk, D => Ndata);
               end if;
            else
               if Init /= No_Net then
                  Res := Build_Midff (Ctxt, Clk, D => Ndata,
                                      Els => Els_Net, Init => Init);
               else
                  Res := Build_Mdff (Ctxt, Clk, D => Ndata, Els => Els_Net);
               end if;
            end if;
         else
            if Els_Net /= No_Net then
               Error_Msg_Synth
                 (Loc, "synchronous code does not expect else part");
            end if;

            if Init /= No_Net then
               Res := Build_Iadff (Ctxt, Clk, D => Ndata,
                                   Rst => Rst, Rst_Val => Rst_Val,
                                   Init => Init);
            else
               Res := Build_Adff (Ctxt, Clk, D => Ndata,
                                  Rst => Rst, Rst_Val => Rst_Val);
            end if;
         end if;

         Set_Location (Res, Loc);
      end if;

      --  The output may already be used (if the target is a variable that
      --  is read).  So redirect the net.
      Redirect_Inputs (Get_Output (Last_Mux, 0), Res);
      return Res;
   end Infere_FF_Create;

   --  Remove the Mux2 and handle the 'else' branch.
   procedure Infere_FF_Mux (Ctxt : Context_Acc;
                            Prev_Val : Net;
                            Off : Uns32;
                            Last_Mux : Instance;
                            Els : out Net;
                            Data : out Net)
   is
      Mux_Loc  : constant Location_Type := Get_Location (Last_Mux);
      Sel      : constant Input := Get_Mux2_Sel (Last_Mux);
      I0       : constant Input := Get_Mux2_I0 (Last_Mux);
      I1       : constant Input := Get_Mux2_I1 (Last_Mux);
      Els_Inst : Instance;
      Els_Clk  : Net;
      Els_En   : Net;
      Els_Data : Net;
      Els_Els  : Net;
   begin
      Els := Get_Driver (I0);
      if Is_Prev_FF_Value (Els, Prev_Val, Off) then
         --  The 'else' part of the mux2 is the logical loop.
         Els := No_Net;
      else
         --  The 'else' part is not a loop.  It should be a second FF for a
         --  DDR (not yet supported) or a true-dual-port RAM.
         Els_Inst := Get_Net_Parent (Els);
         if Get_Id (Els_Inst) = Id_Mux2 then
            Extract_Clock (Ctxt, Get_Driver (Get_Mux2_Sel (Els_Inst)),
                           Els_Clk, Els_En);
         else
            Els_Clk := No_Net;
         end if;
         if Els_Clk = No_Net then
            Error_Msg_Synth
              (Mux_Loc, "clocked logic requires clocked logic on else part");
            Els := No_Net;
         else
            --  Create and return the DFF.

            --  1. Remove the mux that creates the loop (will be replaced by
            --     the dff).
            Infere_FF_Mux (Ctxt, Prev_Val, Off, Els_Inst, Els_Els, Els_Data);

            Els := Infere_FF_Create (Ctxt, Prev_Val, Off, Els_Inst, No_Net,
                                     No_Net, No_Net, Els_Data, Els_Els,
                                     Els_Clk, Els_En, Get_Location (Els_Inst));
            Remove_Instance (Els_Inst);
         end if;
      end if;

      Disconnect (Sel);
      --  Don't try to free driver of I0 as this is Prev_Val or a selection
      --  of it.
      Disconnect (I0);
      Data := Get_Driver (I1);
      --  Don't try to free driver of I1 as it is reconnected.
      Disconnect (I1);
   end Infere_FF_Mux;

   --  A Mux2 with a logical loop and a clock has been found.
   --  Determine the kind of FF and extract the asynchronous reset.
   --  Build the FF (or the RAM).
   --
   --  CLOCK_MUX is the mux whose input 0 is the loop and clock for selector.
   function Infere_FF (Ctxt : Context_Acc;
                       Val : Net;
                       Prev_Val : Net;
                       Off : Uns32;
                       Clock_Mux : Instance;
                       Clk : Net;
                       Clk_Enable : Net;
                       Loc : Location_Type) return Net
   is
      O : constant Net := Get_Output (Clock_Mux, 0);
      Mux_Loc : constant Location_Type := Get_Location (Clock_Mux);
      Data : Net;
      Res : Net;
      Sig : Instance;
      Init : Net;
      Rst : Net;
      Rst_Val : Net;
      Enable : Net;
      Els : Net;
      Last_Mux : Instance;
      --  Previous mux to be free.
      Prev_Mux : Instance;
   begin
      --  Create and return the DFF.

      --  1. Remove the mux that creates the loop (will be replaced by the
      --     dff).
      Infere_FF_Mux (Ctxt, Prev_Val, Off, Clock_Mux, Els, Data);

      --  If the signal declaration has an initial value, get it.
      Sig := Get_Net_Parent (Prev_Val);
      case Get_Id (Get_Module (Sig)) is
         when Id_Isignal
           | Id_Ioutput =>
            Init := Get_Input_Net (Sig, 1);
            Init := Build2_Extract (Ctxt, Init, Off, Get_Width (O));
         when others =>
            Init := No_Net;
      end case;

      --  As an enable signal, start with the enable extracted from the clock
      --  to handle conditions like: `rising_edge(clk) and en`
      Enable := Clk_Enable;

      --  Look for asynchronous set/reset.  They are muxes after the loop
      --  mux.  In theory, there can be many set/reset with a defined order.
      Rst_Val := No_Net;
      Rst := No_Net;
      declare
         Mux : Instance;
         Sel : Net;
         Last_Out : Net;
         Mux_Not_Rst : Net;
         Mux_Rst : Net;
         Mux_Rst_Val : Net;
         Prev_Input : Input;
      begin
         Prev_Mux := Clock_Mux;

         --  LAST_MUX is the last handled mux and LAST_OUT its output.
         Last_Mux := Clock_Mux;
         Last_Out := O;

         --  Initially, the final output is not connected.  So walk from the
         --  clocked mux until reaching the final output.
         while Last_Out /= Val loop
            if not Has_One_Connection (Last_Out)
              and then not Is_Const_Net (Last_Out)
            then
               --  TODO.
               raise Internal_Error;
            end if;

            --  The parent must be a mux (it's a chain of muxes).
            Mux := Get_Input_Parent (Get_First_Sink (Last_Out));
            if Get_Id (Mux) = Id_Nop then
               --  Should have stopped.
               exit;
            end if;
            pragma Assert (Get_Id (Mux) = Id_Mux2);

            --  Extract the reset condition and the reset value.
            Sel := Get_Driver (Get_Mux2_Sel (Mux));
            Prev_Input := Get_Mux2_I0 (Mux);
            if Get_Driver (Prev_Input) = Last_Out then
               --  Normal reset
               Mux_Rst_Val := Get_Driver (Get_Mux2_I1 (Mux));
               Mux_Rst := Sel;
            else
               --  Inverted reset.
               Prev_Input := Get_Mux2_I1 (Mux);
               pragma Assert (Get_Driver (Prev_Input) = Last_Out);
               Mux_Rst_Val := Get_Driver (Get_Mux2_I0 (Mux));
               Mux_Rst := Build_Monadic (Ctxt, Id_Not, Sel);
            end if;

            --  Disconnect this mux.
            Disconnect (Get_Mux2_I0 (Mux));
            Disconnect (Get_Mux2_I1 (Mux));
            Disconnect (Get_Mux2_Sel (Mux));

            --  Next net to be handled.
            Last_Mux := Mux;
            Last_Out := Get_Output (Mux, 0);

            if Is_Prev_FF_Value (Mux_Rst_Val, Prev_Val, Off) then
               --  The mux is like an enable.  Like in this example, q2 is not
               --  assigned when RST is true:
               --    if rst then
               --      q1 <= '0';
               --    elsif rising_edge(clk) then
               --      q2 <= d2;
               --      q1 <= d1;
               --    end if;

               --  Add the negation of the condition to the enable signal.
               --  Negate the condition for the current reset.
               Mux_Not_Rst := Build_Monadic (Ctxt, Id_Not, Mux_Rst);
               Set_Location (Mux_Not_Rst, Loc);
               if Rst /= No_Net then
                  Rst := Build_Dyadic (Ctxt, Id_And, Rst, Mux_Not_Rst);
                  Set_Location (Rst, Loc);
               end if;
               if Enable = No_Net then
                  Enable := Mux_Not_Rst;
               else
                  Enable := Build_Dyadic (Ctxt, Id_And, Enable, Mux_Not_Rst);
                  Set_Location (Enable, Loc);
               end if;

               if Prev_Mux /= No_Instance then
                  Remove_Instance (Prev_Mux);
               end if;
               Prev_Mux := Mux;
            else
               --  Assume this is a reset value.
               --  FIXME: check for no logical loop.

               if Rst = No_Net then
                  --  First async reset condition.

                  --  Keep reset value and condition
                  Rst := Mux_Rst;
                  Rst_Val := Mux_Rst_Val;

                  --  Remove the last mux.  Will free this mux.
                  if Prev_Mux /= No_Instance then
                     Remove_Instance (Prev_Mux);
                  end if;
                  Prev_Mux := Mux;
               else
                  --  New async reset condition.
                  Rst := Build_Dyadic (Ctxt, Id_Or, Mux_Rst, Rst);
                  Copy_Location (Rst, Mux_Rst);

                  --  Use prev_mux to select the reset value.
                  Connect (Get_Mux2_Sel (Prev_Mux), Mux_Rst);
                  Connect (Get_Mux2_I0 (Prev_Mux), Rst_Val);
                  Connect (Get_Mux2_I1 (Prev_Mux), Mux_Rst_Val);

                  --  The reset value is the output of prev_mux.
                  Rst_Val := Get_Output (Prev_Mux, 0);

                  --  Allow to free this mux.
                  Prev_Mux := Mux;
               end if;
            end if;
         end loop;

         pragma Assert (Prev_Mux = No_Instance or else Prev_Mux = Last_Mux);
      end;

      Res := Infere_FF_Create (Ctxt, Prev_Val, Off, Last_Mux, Init,
                               Rst, Rst_Val, Data, Els, Clk, Enable, Mux_Loc);

      if Prev_Mux /= No_Instance then
         Remove_Instance (Prev_Mux);
      end if;

      return Res;
   end Infere_FF;

   --  Detect false combinational loop.  They can easily appear when variables
   --  are only used in one branch:
   --    process (all)
   --      variable a : std_logic;
   --    begin
   --      r <= '1';
   --      if sel = '1' then
   --        a := '1';
   --        r <= '0';
   --      end if;
   --    end process;
   --  There is a combinational path from 'a' to 'a' as
   --    a := (sel = '1') ? '1' : a;
   --  But this is a false loop because the value of 'a' is never used.  In
   --  that case, 'a' is assigned to 'x' and all the unused logic will be
   --  removed during clean-up.
   --
   --  Detection is very simple: the closure of readers of 'a' must be only
   --  muxes (which were inserted by controls).
   function Is_False_Loop (Prev_Val : Net) return Boolean
   is
      package Inst_Interning renames
        Netlists.Internings.Dyn_Instance_Interning;
      use Inst_Interning;
      T : Inst_Interning.Instance;

      function Add_From_Net (N : Net) return Boolean
      is
         Inst : Netlists.Instance;
         Inp : Input;
      begin
         Inp := Get_First_Sink (N);
         while Inp /= No_Input loop
            Inst := Get_Input_Parent (Inp);
            case Get_Id (Inst) is
               when Mux_Module_Id
                 | Id_Pmux =>
                  null;
               when others =>
                  return False;
            end case;

            --  Add to T (if not already).
            Get (T, Inst, Inst);

            Inp := Get_Next_Sink (Inp);
         end loop;

         return True;
      end Add_From_Net;

      function Walk_Nets (N : Net) return Boolean
      is
         Inst : Netlists.Instance;
      begin
         --  Put gates that read the value.
         if not Add_From_Net (N) then
            return False;
         end if;

         --  Follow the outputs.
         for I in First_Index .. Index_Type'Last loop
            exit when I > Inst_Interning.Last_Index (T);
            Inst := Get_By_Index (T, I);
            if not Add_From_Net (Get_Output (Inst, 0)) then
               return False;
            end if;
         end loop;

         --  No external readers.
         return True;
      end Walk_Nets;

      Res : Boolean;
   begin
      Inst_Interning.Init (T);

      Res := Walk_Nets (Prev_Val);

      Inst_Interning.Free (T);

      return Res;
   end Is_False_Loop;

   function Infere_Latch (Ctxt : Context_Acc;
                          Val : Net;
                          Prev_Val : Net;
                          Loc : Location_Type) return Net
   is
      Name : Sname;
   begin
      --  In case of false loop, do not close the loop but assign X.
      if Is_False_Loop (Prev_Val) then
         return Build_Const_X (Ctxt, Get_Width (Val));
      end if;

      --  Latch or combinational loop.
      if Get_Id (Get_Net_Parent (Prev_Val)) = Id_Output then
         --  Outputs are connected to a port.  The port is the first connection
         --  made, so it is the last sink.  Be more tolerant and look for
         --  the (only) port connected to the output.
         declare
            Inp : Input;
            Inst : Instance;
         begin
            Inp := Get_First_Sink (Prev_Val);
            loop
               pragma Assert (Inp /= No_Input);
               Inst := Get_Input_Parent (Inp);
               if Get_Id (Inst) >= Id_User_None then
                  Name := Get_Output_Desc (Get_Module (Inst),
                                           Get_Port_Idx (Inp)).Name;
                  exit;
               end if;
               Inp := Get_Next_Sink (Inp);
            end loop;
         end;
      else
         Name := Get_Instance_Name (Get_Net_Parent (Prev_Val));
      end if;

      if not Flag_Latches then
         Error_Msg_Synth
           (Loc, "latch infered for net %n (use --latches)", +Name);
      end if;

      return Val;
   end Infere_Latch;

   --  VAL is the value to be assigned to a wire at offset OFF.
   --  Note: PREV_VAL is the wire gate, so with full width and no offset.
   function Infere (Ctxt : Context_Acc;
                    Val : Net;
                    Off : Uns32;
                    Prev_Val : Net;
                    Loc : Location_Type;
                    Last_Use : Boolean) return Net
   is
      pragma Assert (Val /= No_Net);
      pragma Assert (Prev_Val /= No_Net);
      First_Mux, Last_Mux : Instance;
      Len : Integer;
      Sel : Input;
      Clk : Net;
      Enable : Net;
      Res : Net;
   begin
      if Get_First_Sink (Prev_Val) = No_Input then
         --  PREV_VAL is never read, so there cannot be any loop.
         --  This is an important optimization for control signals.
         return Val;
      end if;

      --  Infere tri-buf.
      First_Mux := Get_Net_Parent (Val);
      if Get_Id (First_Mux) = Id_Mux2 then
         declare
            Nsel, N0, N1 : Net;
         begin
            --  Check for VAL <= SEL ? N1 : 'Z'
            if Get_Id (Get_Input_Instance (First_Mux, 1)) = Id_Const_Z then
               --  Disconnect the mux.
               Nsel := Disconnect_And_Get (First_Mux, 0);
               N0 := Disconnect_And_Get (First_Mux, 1);
               N1 := Disconnect_And_Get (First_Mux, 2);
               --  Build the tri buf.
               Res := Build_Tri (Ctxt, Nsel, N1);
               --  Remove the 'Z' (shouldn't be connected).
               Remove_Instance (Get_Net_Parent (N0));
               --  Copy location.
               Copy_Location (Res, First_Mux);
               --  Redirect tri output.
               Redirect_Inputs (Get_Output (First_Mux, 0), Res);
               Remove_Instance (First_Mux);
               return Res;
            end if;
         end;
      end if;

      Find_Longest_Loop (Val, Prev_Val, Last_Mux, Len);
      if Len <= 0 then
         --  No logical loop or self assignment.
         return Val;
      end if;
      if Last_Use
        and then Has_One_Connection (Prev_Val)
        and then not Is_Connected (Val)
      then
         --  Value is not used, to be removed.  Do not try to infere anything.
         --  Conditions:
         --   * last_use must be true: the signal won't be use after the call
         --     to infere (because it goes out of scope).
         --   * Prev_val must be connected once (to create a loop).
         --   * Val must not be connected (for variables).
         return Val;
      end if;

      --  So there is a logical loop.
      Sel := Get_Mux2_Sel (Last_Mux);
      Extract_Clock (Ctxt, Get_Driver (Sel), Clk, Enable);
      if Clk = No_Net then
         --  No clock -> latch or combinational loop
         Res := Infere_Latch (Ctxt, Val, Prev_Val, Loc);
      else
         --  Clock -> FF
         First_Mux := Get_Net_Parent (Val);
         pragma Assert (Get_Id (First_Mux) = Id_Mux2);

         Res := Infere_FF (Ctxt, Val, Prev_Val, Off, Last_Mux,
                           Clk, Enable, Loc);
      end if;

      return Res;
   end Infere;

   --  INST is a mux2 of a condition chain.
   --  Return the input that is not 0.  Could be either a mux2 or a const.
   function Find_Condition_Chain_Next (Inst : Instance) return Instance
   is
      Mux_In0, Mux_In1 : Net;
      In0_Inst, In1_Inst : Instance;
   begin
      Mux_In0 := Get_Input_Net (Inst, 1);
      In0_Inst := Get_Net_Parent (Mux_In0);

      Mux_In1 := Get_Input_Net (Inst, 2);
      In1_Inst := Get_Net_Parent (Mux_In1);

      if Get_Id (In0_Inst) /= Id_Const_UB32 then
         --  The other input must be const 0.
         pragma Assert (Get_Id (In1_Inst) = Id_Const_UB32
                          and then Get_Param_Uns32 (In1_Inst, 0) = 0);
         return In0_Inst;
      else
         --  Either both are const, or the other input must be const 0.
         if Get_Id (In1_Inst) = Id_Const_UB32 then
            --  Both are const.  Return the const 1.
            if Get_Param_Uns32 (In1_Inst, 0) = 0 then
               pragma Assert (Get_Param_Uns32 (In0_Inst, 0) = 1);
               return In0_Inst;
            else
               pragma Assert (Get_Param_Uns32 (In1_Inst, 0) = 1);
               pragma Assert (Get_Param_Uns32 (In0_Inst, 0) = 0);
               return In1_Inst;
            end if;
         end if;

         pragma Assert (Get_Param_Uns32 (In0_Inst, 0) = 0);
         return In1_Inst;
      end if;
   end Find_Condition_Chain_Next;

   --  VAL is a chain of mux2 that define the conditions to enable assertions.
   function Infere_Assert (Ctxt : Context_Acc;
                           Val : Net;
                           En_Gate : Net;
                           Loc : Location_Type) return Net
   is
      Inst       : Instance;
      First_Inst : Instance;
      Last_Inst  : Instance;
      Clk, En    : Net;
      Areset     : Net;
      One        : Net;
   begin
      --  Extract clock (if any) from VAL.  Return VAL is no clock.
      First_Inst := Get_Net_Parent (Val);
      Inst := First_Inst;
      loop
         case Get_Id (Inst) is
            when Id_Mux2 =>
               null;
            when Id_Const_UB32
               | Id_Pmux =>
               return Val;
            when others =>
               raise Internal_Error;
         end case;
         Extract_Clock (Ctxt, Get_Input_Net (Inst, 0), Clk, En);
         exit when Clk /= No_Net;

         --  No clock.  Try the father.
         Inst := Find_Condition_Chain_Next (Inst);
      end loop;

      --  INST is the mux2 with clock CLK.

      --  Extract enable and asynchronous reset (if any).
      Last_Inst := Inst;
      Areset := No_Net;
      Inst := First_Inst;
      while Inst /= Last_Inst loop
         declare
            Cond : Net;
            Next_Inst : Instance;
         begin
            Cond := Get_Input_Net (Inst, 0);

            --  Find the next mux.
            Next_Inst := Find_Condition_Chain_Next (Inst);

            --  If the next mux is in1, negate COND.
            if Next_Inst = Get_Net_Parent (Get_Input_Net (Inst, 2)) then
               Cond := Build_Monadic (Ctxt, Id_Not, Cond);
               Set_Location (Cond, Loc);
            end if;

            --  'And' COND to Areset.
            Areset := Build2_And (Ctxt, Areset, Cond, Loc);

            Inst := Next_Inst;
         end;
      end loop;

      --  Same for LAST_INST, but check it is on in1.
      declare
         Next_Inst : Instance;
      begin
         Next_Inst := Find_Condition_Chain_Next (Last_Inst);
         if Next_Inst /= Get_Net_Parent (Get_Input_Net (Inst, 2)) then
            Error_Msg_Synth
              (+Last_Inst, "assertion checked on else branch of an edge");
            return Val;
         end if;

         En := Build2_And (Ctxt, En, Get_Output (Next_Inst, 0), Loc);
      end;

      One := Build_Const_UB32 (Ctxt, 1, 1);

      --  Build an idff/iadff for each condition of the assertions.
      --  The caller will connect the returned value (En) to the enable gate.
      declare
         En_Inp : Input;
         Assert_Inp : Input;
         N : Net;
         Dff : Net;
      begin
         En_Inp := Get_First_Sink (En_Gate);
         pragma Assert (En_Inp /= No_Input);
         while En_Inp /= No_Input loop
            --  The Enable gate is connected to an implication.
            Inst := Get_Input_Parent (En_Inp);
            pragma Assert (Get_Id (Inst) = Id_Not);
            N := Get_Output (Inst, 0);
            pragma Assert (Has_One_Connection (N));
            Inst := Get_Input_Parent (Get_First_Sink (N));
            pragma Assert (Get_Id (Inst) = Id_Or);

            N := Get_Output (Inst, 0);
            pragma Assert (Has_One_Connection (N));
            Inst := Get_Input_Parent (Get_First_Sink (N));

            pragma Assert (Get_Id (Inst) = Id_Assert);

            Assert_Inp := Get_Input (Inst, 0);
            Disconnect (Assert_Inp);

            if Areset = No_Net then
               Dff := Build_Idff (Ctxt, Clk, N, One);
            else
               Dff := Build_Iadff (Ctxt, Clk, N, Areset, One, One);
            end if;
            Set_Location (Dff, Loc);

            Connect (Assert_Inp, Dff);

            En_Inp := Get_Next_Sink (En_Inp);
         end loop;
      end;

      return En;
   end Infere_Assert;

end Netlists.Inference;
