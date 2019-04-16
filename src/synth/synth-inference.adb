--  Inference in synthesis.
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

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Gates_Ports; use Netlists.Gates_Ports;
with Types; use Types;

package body Synth.Inference is
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

   --  Find the longest chain of mux starting from VAL with a final input
   --  of PREV_VAL.  Such a chain means this is a memorising element.
   --  RES is the last mux in the chain, DIST the number of mux in the chain.
   procedure Find_Longest_Loop
     (Val : Net; Prev_Val : Net; Res : out Instance; Dist : out Integer)
   is
      Inst : constant Instance := Get_Parent (Val);
   begin
      if Get_Id (Inst) = Id_Mux2 then
         declare
            Res0, Res1 : Instance;
            Dist0, Dist1 : Integer;
         begin
            Find_Longest_Loop
              (Get_Driver (Get_Mux2_I0 (Inst)), Prev_Val, Res0, Dist0);
            Find_Longest_Loop
              (Get_Driver (Get_Mux2_I1 (Inst)), Prev_Val, Res1, Dist1);
            --  Input1 has an higher priority than input0 in case the selector
            --  is a clock.
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
         end;
      elsif Val = Prev_Val then
         Res := No_Instance;
         Dist := 0;
      else
         Res := No_Instance;
         Dist := -1;
      end if;
   end Find_Longest_Loop;

   --  Walk the And-net N, and extract clock (posedge/negedge) if found.
   --  ENABLE is N without the clock.
   procedure Extract_Clock (N : Net; Clk : out Net; Enable : out Net)
   is
      Inst : constant Instance := Get_Net_Parent (N);
   begin
      Clk := No_Net;
      Enable := No_Net;

      case Get_Id (Inst) is
         when Edge_Module_Id =>
            Clk := N;
         when Id_And =>
            --  Assume the condition is canonicalized, ie of the form:
            --  CLK and EXPR.
            --  FIXME: do it!
            declare
               I0 : constant Input := Get_Input (Inst, 0);
               I1 : Input;
               Drv : Net;
            begin
               Drv := Get_Driver (I0);
               if Get_Id (Get_Net_Parent (Drv)) in Edge_Module_Id then
                  --  INST is clearly not synthesizable (boolean operation on
                  --  an edge).  Will be removed at the end by
                  --  remove_unused_instances.  Do not remove it now as its
                  --  output may be used by other nets.
                  Clk := Drv;
                  I1 := Get_Input (Inst, 1);
                  Enable := Get_Driver (I1);
               end if;
            end;
         when others =>
            null;
      end case;
   end Extract_Clock;

   function Infere (Ctxt : Context_Acc; Val : Net; Prev_Val : Net) return Net
   is
      pragma Assert (Val /= No_Net);
      pragma Assert (Prev_Val /= No_Net);
      Last_Mux : Instance;
      Len : Integer;
   begin
      Find_Longest_Loop (Val, Prev_Val, Last_Mux, Len);
      if Len < 0 then
         --  No logical loop
         return Val;
      elsif Len = 0 then
         --  Self assignment.
         return Val;
      end if;

      declare
         Sel : constant Input := Get_Mux2_Sel (Last_Mux);
         I0 : constant Input := Get_Mux2_I0 (Last_Mux);
         I1 : constant Input := Get_Mux2_I1 (Last_Mux);
         O : constant Net := Get_Output (Last_Mux, 0);
         Data : Net;
         Clk : Net;
         Enable : Net;
         Res : Net;
         Sig : Instance;
         Init : Net;
         Init_Input : Input;
         Rst : Net;
         Rst_Val : Net;
      begin
         Extract_Clock (Get_Driver (Sel), Clk, Enable);
         if Clk = No_Net then
            --  No clock -> latch
            raise Internal_Error;
         else
            --  Create and return the DFF.
            Disconnect (Sel);
            if Get_Driver (I0) /= Prev_Val then
               --  There must be no 'else' part for clock expression.
               raise Internal_Error;
            end if;
            --  Don't try to free driver of I0 as this is Prev_Val.
            Disconnect (I0);
            Data := Get_Driver (I1);
            --  Don't try to free driver of I1 as it is reconnected.
            Disconnect (I1);
            if Enable /= No_Net then
               Data := Build_Mux2 (Ctxt, Enable, Prev_Val, Data);
            end if;

            --  If the signal declaration has an initial value, move it
            --  to the dff.
            Sig := Get_Parent (Prev_Val);
            if Get_Id (Get_Module (Sig)) = Id_Isignal then
               Init_Input := Get_Input (Sig, 1);
               Init := Get_Driver (Init_Input);
               Disconnect (Init_Input);
            else
               Init := No_Net;
            end if;


            Rst_Val := No_Net;
            Rst := No_Net;

            declare
               Mux : Instance;
               Sel : Net;
               Last_Out : Net;
               Mux_Rst : Net;
               Mux_Rst_Val : Net;
            begin
               Last_Out := O;

               while Is_Connected (Last_Out) loop
                  if not Has_One_Connection (Last_Out) then
                     --  TODO.
                     raise Internal_Error;
                  end if;

                  Mux := Get_Parent (Get_First_Sink (Last_Out));
                  if Get_Id (Mux) /= Id_Mux2 then
                     raise Internal_Error;
                  end if;

                  Sel := Get_Driver (Get_Mux2_Sel (Mux));
                  if Get_Driver (Get_Mux2_I0 (Mux)) = O then
                     Mux_Rst_Val := Get_Driver (Get_Mux2_I1 (Mux));
                     Mux_Rst := Sel;
                  elsif Get_Driver (Get_Mux2_I1 (Mux)) = O then
                     Mux_Rst_Val := Get_Driver (Get_Mux2_I0 (Mux));
                     Mux_Rst := Build_Monadic (Ctxt, Id_Not, Sel);
                  else
                     --  Cannot happen.
                     raise Internal_Error;
                  end if;

                  Last_Out := Get_Output (Mux, 0);

                  if Rst = No_Net then
                     --  Remove the last mux.
                     Disconnect (Get_Mux2_I0 (Mux));
                     Disconnect (Get_Mux2_I1 (Mux));
                     Disconnect (Get_Mux2_Sel (Mux));

                     Redirect_Inputs (Last_Out, Mux_Rst_Val);
                     Free_Instance (Mux);

                     Rst := Mux_Rst;
                     Rst_Val := Mux_Rst_Val;
                  else
                     Rst := Build_Dyadic (Ctxt, Id_Or, Mux_Rst, Rst);
                     Rst_Val := Last_Out;
                  end if;
               end loop;
            end;

            if Rst = No_Net then
               pragma Assert (Rst_Val = No_Net);
               if Init /= No_Net then
                  Res := Build_Idff (Ctxt, Clk, D => Data, Init => Init);
               else
                  Res := Build_Dff (Ctxt, Clk, D => Data);
               end if;
            else
               if Init /= No_Net then
                  Res := Build_Iadff (Ctxt, Clk, D => Data,
                                      Rst => Rst, Rst_Val => Rst_Val,
                                      Init => Init);
               else
                  Res := Build_Adff (Ctxt, Clk, D => Data,
                                     Rst => Rst, Rst_Val => Rst_Val);
               end if;
            end if;

            --  The output of the mux may be read later in the process,
            --  like this:
            --    if clk'event and clk = '1' then
            --       d := i + 1;
            --    end if;
            --    d1 := d + 1;
            --  So connections to the mux output are redirected to dff
            --  output.
            Redirect_Inputs (O, Res);

            Free_Instance (Last_Mux);
            return Res;
         end if;
      end;
   end Infere;
end Synth.Inference;
