--  Extract memories.
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

with Ada.Unchecked_Deallocation;
with Errorout; use Errorout;

with Grt.Algos;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Errors; use Netlists.Errors;
with Netlists.Concats;
with Netlists.Folds; use Netlists.Folds;
with Netlists.Inference;

with Synth.Errors; use Synth.Errors;

package body Netlists.Memories is
   --  If set, be verbose why a memory is not found.  But the messages are
   --  a little bit cryptic.
   Flag_Memory_Verbose : constant Boolean := False;

   --  What is a memory ?
   --
   --  A memory is obviously a memorizing element.  This means there is a
   --  logical loop between input and output.  Because there is a loop, a
   --  name is required in the HDL input to create a loop.  You cannot create
   --  a memory without a signal/variable name (but you can create a ROM
   --  without it).
   --  TODO: can it be proved ?
   --
   --  A memory is not a flip-flop nor a latch.  The reason is that only a
   --  part of the memory is worked on.  Only a part of the memory is read,
   --  and only a part of the memory is written (but a variable part).
   --  So, the logical loop is modified by using dyn_insert and read by
   --  using dyn_extract.  And muxes.
   --
   --  HDL structure of a memory (RAM).
   --
   --  A memory can be only be read or written partially, using either an
   --  indexed name of a slice.
   --
   --  Example1:
   --    val1 := mem (addr1)
   --  Example2:
   --    mem (addr2) <= val2;
   --
   --  A read generates a dyn_extract, while a write generates a dyn_insert.
   --
   --  It is possible to use a write enable, which is synthesized as a mux.
   --
   --  Example3:
   --   if en then
   --     mem (addr3) <= val3;
   --   end if;
   --
   --  So a dyn_insert can be followed by a mux, using these connections:
   --            _
   --           / |----- dyn_insert ----+----+
   --    out --|  |                     |    +---- inp
   --           \_|---------------------/
   --
   --  There might be several muxes, but using the same input when not
   --  selecting the dyn_insert.  They could be merged.
   --
   --  Additionally, a mux can also select between two writes.
   --
   --  Example4:
   --  if sel then
   --    mem (addr4a) <= val4a;
   --  else
   --    mem (addr4b) <= val4b;
   --  end if;
   --
   --  The netlist generated for this structure is:
   --            _
   --           / |----- dyn_insert ----\
   --    out --|  |                     +--------- inp
   --           \_|----- dyn_insert ----/
   --
   --  Note: a Dff may have replaced a mux if the enable is a clock edge.
   --
   --  Any write can be followed by another write.  Can be a dual-port memory,
   --  of write to different bytes.
   --
   --  Example5:
   --    mem(addr5a) <= val5a;
   --    mem(addr5b) <= val5b;
   --
   --  So, there can be a combination any of these elements, each having
   --  one input and one output.
   --  - O := dyn_insert(I)
   --  - O := mux(sel, el(I), I)
   --  - O := mux(sel, el1(I), el2(I))
   --  - O := el1(el2(I))
   --
   --
   --  Reads can happen anywhere.  But we will first consider only reads
   --  that occurs just after the dff (so synchronous reads) or just before
   --  the dff (asynchronous reads).
   --
   --  If there is another logical element, then this is not a memory.
   --
   --  How rams/roms are detected ?
   --  All dyn_extract/dyn_insert are gathered, and walked to the signal.
   --  Then all those signals are gathered: that's the candidate memories.
   --
   --  How rams/roms are qualified (from candidate memories to memories) ?
   --  There must be only dyn_insert/dyn_extract + muxes on the logical loop.
   --  Use a mark algorithm.
   --
   --  Once qualified:
   --  Merge muxes to the dyn_inserts.
   --  FIXME: position of dyn_extract wrt dyn_insert:
   --    if en then
   --      mem(ad1) := val1;
   --      val2 := mem(ad2);
   --    end if;
   --
   --  Strategy: merge muxes until the logical loop is only composed of
   --  dyn_insert/dyn_extract (+ signal and maybe dff).

   --  Follow signal from ORIG to discover memory ports size.
   --  Should be the same.
   procedure Check_Memory_Read_Ports (Orig : Instance;
                                      Data_W : out Width;
                                      Size : out Width)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      W : Width;
   begin
      --  By default, error.
      Data_W := 0;
      Size := 0;

      --  Check readers.
      declare
         Inp : Input;
         Extr_Inst : Instance;
         Idx : Instance;
         Step : Uns32;
      begin
         Inp := Get_First_Sink (Orig_Net);
         while Inp /= No_Input loop
            Extr_Inst := Get_Input_Parent (Inp);
            case Get_Id (Extr_Inst) is
               when Id_Dyn_Extract =>
                  --  Extract step from memidx gate.
                  Idx := Get_Net_Parent (Get_Input_Net (Extr_Inst, 1));
                  while Get_Id (Idx) = Id_Addidx loop
                     --  Multi-dim arrays, lowest index is the last one.
                     Idx := Get_Net_Parent (Get_Input_Net (Idx, 1));
                  end loop;
                  pragma Assert (Get_Id (Idx) = Id_Memidx);
                  Step := Get_Param_Uns32 (Idx, 0);

                  --  Check offset
                  if Get_Param_Uns32 (Extr_Inst, 0) /= 0 then
                     Info_Msg_Synth
                       (+Extr_Inst, "partial read from memory %n",
                        (1 => +Orig));
                     Data_W := 0;
                     return;
                  end if;
                  --  Check data width.
                  W := Get_Width (Get_Output (Extr_Inst, 0));
                  pragma Assert (W > 0);
                  if W > Step then
                     Info_Msg_Synth
                       (+Extr_Inst, "overlapping read from memory %n",
                        (1 => +Orig));
                     Data_W := 0;
                     return;
                  end if;
                  if Data_W = 0 then
                     pragma Assert (Step /= 0);
                     Data_W := Step;
                  elsif Data_W /= Step then
                     Info_Msg_Synth
                       (+Extr_Inst, "read from memory %n with different size",
                        (1 => +Orig));
                     Data_W := 0;
                     return;
                  end if;
               when Id_Dyn_Insert
                  | Id_Dyn_Insert_En
                  | Id_Mux2 =>
                  --  Probably a writer.
                  --  FIXME: check it has already been by writes.
                  null;
               when others =>
                  Info_Msg_Synth
                    (+Extr_Inst, "full read from memory %n", (1 => +Orig));
                  Data_W := 0;
                  return;
            end case;

            Inp := Get_Next_Sink (Inp);
         end loop;
      end;

      if Data_W = 0 then
         Info_Msg_Synth (+Orig, "memory %n is never read", (1 => +Orig));
         Data_W := 0;
      else
         Size := Get_Width (Orig_Net) / Data_W;
      end if;
   end Check_Memory_Read_Ports;

   --  Count the number of memidx in a memory address.
   function Count_Memidx (Addr : Net) return Natural
   is
      N : Net;
      Inst : Instance;
      Res : Natural;
   begin
      N := Addr;
      Res := 0;
      loop
         Inst := Get_Net_Parent (N);
         case Get_Id (Inst) is
            when Id_Memidx =>
               return Res + 1;
            when Id_Addidx =>
               if Get_Id (Get_Input_Instance (Inst, 1)) /= Id_Memidx then
                  raise Internal_Error;
               end if;
               Res := Res + 1;
               N := Get_Input_Net (Inst, 0);
            when others =>
               raise Internal_Error;
         end case;
      end loop;
   end Count_Memidx;

   --  Lower memidx/addidx to simpler gates (concat).
   --  MEM_SIZE: size of the memory (in bits).
   --  ADDR is the address net with memidx/addidx gates.
   --  VAL_WD is the width of the data port.
   procedure Convert_Memidx (Ctxt : Context_Acc;
                             Mem_Size : Uns32;
                             Addr : in out Net;
                             Val_Wd : Width)
   is
      --  Number of memidx.
      Nbr_Idx : constant Positive := Count_Memidx (Addr);
      Can_Free : constant Boolean := not Is_Connected (Addr);

      Mem_Depth : Uns32;
      Last_Size : Uns32;
      Low_Addr : Net;

      type Idx_Data is record
         Inst : Instance;
         Addr : Net;
      end record;
      type Idx_Array is array (Natural range <>) of Idx_Data;
      Indexes : Idx_Array (1 .. Nbr_Idx);
   begin
      --  Fill the INDEXES array.
      --  The convention is that input 0 of addidx is a memidx.
      declare
         P : Natural;
         N : Net;
         Inst : Instance;
         Inst2 : Instance;
      begin
         N := Addr;
         P := 0;
         loop
            Inst := Get_Net_Parent (N);
            case Get_Id (Inst) is
               when Id_Memidx =>
                  P := P + 1;
                  Indexes (P) := (Inst => Inst, Addr => No_Net);
                  exit;
               when Id_Addidx =>
                  Inst2 := Get_Input_Instance (Inst, 0);
                  if Get_Id (Inst2) /= Id_Memidx then
                     raise Internal_Error;
                  end if;
                  P := P + 1;
                  Indexes (P) := (Inst => Inst2, Addr => No_Net);
                  N := Get_Input_Net (Inst, 1);
               when others =>
                  raise Internal_Error;
            end case;
         end loop;
         pragma Assert (P = Nbr_Idx);
      end;

      --  Memory size is a multiple of data width.
      --  FIXME: doesn't work if only a part of the reg is a memory.
      if Mem_Size mod Val_Wd /= 0 then
         raise Internal_Error;
      end if;
      Mem_Depth := Mem_Size / Val_Wd;
      pragma Unreferenced (Mem_Depth);

      --  Do checks on memidx.
      Last_Size := Mem_Size;
      for I in Indexes'Range loop
         declare
            Inst : constant Instance := Indexes (I).Inst;
            Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
            Sub_Addr : constant Net := Get_Input_Net (Inst, 0);
            Addr_W : constant Width := Get_Width (Sub_Addr);
            Max : constant Uns32 := Get_Param_Uns32 (Inst, 1);
            Max_W : constant Width := Clog2 (Max + 1);
            Sub_Addr1 : Net;
            Sz : Uns32;
         begin
            --  Check max (from previous dimension).
            --  Check the memidx can index its whole input.
            pragma Assert (Max /= 0);
            Sz := (Max + 1) * Step;
            if Sz /= Last_Size then
               raise Internal_Error;
            end if;
            Last_Size := Step;

            if I = Indexes'Last then
               if Step /= Val_Wd then
                  raise Internal_Error;
               end if;
            end if;

            --  Check addr width.
            if Addr_W = 0 then
               raise Internal_Error;
            end if;
            if Addr_W > Max_W then
               --  Need to truncate.
               Sub_Addr1 := Build2_Trunc
                 (Ctxt, Id_Utrunc, Sub_Addr, Max_W, Get_Location (Inst));
            else
               Sub_Addr1 := Sub_Addr;
            end if;
            Indexes (I).Addr := Sub_Addr1;
         end;
      end loop;

      --  Lower (just concat addresses).
      declare
         use Netlists.Concats;
         Concat : Concat_Type;
      begin
         for I in reverse Indexes'Range loop
            Append (Concat, Indexes (I).Addr);
         end loop;

         Build (Ctxt, Concat, Low_Addr);
      end;

      --  Free addidx and memidx.
      if Can_Free then
         declare
            N : Net;
            Inp : Input;
            Inst : Instance;
            Inst2 : Instance;
         begin
            N := Addr;
            loop
               Inst := Get_Net_Parent (N);
               case Get_Id (Inst) is
                  when Id_Memidx =>
                     Inp := Get_Input (Inst, 0);
                     Disconnect (Inp);
                     Remove_Instance (Inst);
                     exit;
                  when Id_Addidx =>
                     --  Remove the first input (a memidx).
                     Inp := Get_Input (Inst, 0);
                     Inst2 := Get_Net_Parent (Get_Driver (Inp));
                     pragma Assert (Get_Id (Inst2) = Id_Memidx);
                     Disconnect (Inp);
                     Inp := Get_Input (Inst2, 0);
                     Disconnect (Inp);
                     Remove_Instance (Inst2);

                     --  Continue with the second input.
                     Inp := Get_Input (Inst, 1);
                     N := Get_Driver (Inp);
                     Disconnect (Inp);

                     --  Remove the addidx.
                     Remove_Instance (Inst);
                  when others =>
                     raise Internal_Error;
               end case;
            end loop;
         end;
      end if;

      Addr := Low_Addr;
   end Convert_Memidx;

   procedure Convert_Memidx (Ctxt : Context_Acc;
                             Mem : Instance;
                             Addr : in out Net;
                             Val_Wd : Width)
   is
      Mem_Size : constant Uns32 := Get_Width (Get_Output (Mem, 0));
   begin
      Convert_Memidx (Ctxt, Mem_Size, Addr, Val_Wd);
   end Convert_Memidx;

   --  Return True iff MUX_INP is a mux2 input whose output is connected to a
   --  dff to create a DFF with enable (the other mux2 input is connected to
   --  the dff output).
   function Is_Enable_Dff (Mux_Inp : Input) return Boolean
   is
      Mux_Inst : constant Instance := Get_Input_Parent (Mux_Inp);
      pragma Assert (Get_Id (Mux_Inst) = Id_Mux2);
      Mux_Out : constant Net := Get_Output (Mux_Inst, 0);
      Inp : Input;
      Dff_Inst : Instance;
      Dff_Out : Net;
   begin
      Inp := Get_First_Sink (Mux_Out);
      if Inp = No_Input or else Get_Next_Sink (Inp) /= No_Input then
         --  The output of the mux must be connected to one input.
         return False;
      end if;
      Dff_Inst := Get_Input_Parent (Inp);
      if Get_Id (Dff_Inst) /= Id_Dff then
         return False;
      end if;
      Dff_Out := Get_Output (Dff_Inst, 0);

      if Mux_Inp = Get_Input (Mux_Inst, 1) then
         return Skip_Signal (Get_Input_Net (Mux_Inst, 2)) = Dff_Out;
      else
         return Skip_Signal (Get_Input_Net (Mux_Inst, 1)) = Dff_Out;
      end if;
   end Is_Enable_Dff;

   --  If INST is followed by a dff or a dff+enable (with mux2), return the
   --  dff in LAST_INST, the clock in CLK and the enable in EN.
   procedure Extract_Extract_Dff (Ctxt : Context_Acc;
                                  Inst : Instance;
                                  Last_Inst : out Instance;
                                  Clk : out Net;
                                  En : out Net)
   is
      Val : constant Net := Get_Output (Inst, 0);
      Inp : Input;
      Iinst : Instance;
   begin
      Inp := Get_First_Sink (Val);
      if Get_Next_Sink (Inp) = No_Input then
         --  There is a single input.
         Iinst := Get_Input_Parent (Inp);
         if Get_Id (Iinst) = Id_Dff then
            --  The output of the dyn_extract is directly connected to a dff.
            --  So this is a synchronous read without enable.
            declare
               Clk_Inp : Input;
            begin
               Clk_Inp := Get_Input (Iinst, 0);
               Clk := Get_Driver (Clk_Inp);
               Disconnect (Clk_Inp);
               En := No_Net;
               Disconnect (Inp);
               Last_Inst := Iinst;
               return;
            end;
         elsif Get_Id (Iinst) = Id_Mux2 and then Is_Enable_Dff (Inp) then
            declare
               Mux_Out : constant Net := Get_Output (Iinst, 0);
               Mux_En_Inp : constant Input := Get_Input (Iinst, 0);
               Mux_I0_Inp : constant Input := Get_Input (Iinst, 1);
               Mux_I1_Inp : constant Input := Get_Input (Iinst, 2);
               Dff_Din : constant Input := Get_First_Sink (Mux_Out);
               Dff_Inst : constant Instance := Get_Input_Parent (Dff_Din);
               Dff_Out : constant Net := Get_Output (Dff_Inst, 0);
               Clk_Inp : constant Input := Get_Input (Dff_Inst, 0);
            begin
               Clk := Get_Driver (Clk_Inp);
               En := Get_Driver (Mux_En_Inp);
               if Dff_Out = Get_Driver (Mux_I1_Inp) then
                  En := Build_Monadic (Ctxt, Id_Not, En);
                  Copy_Location (En, Iinst);
               end if;
               Disconnect (Mux_En_Inp);
               Disconnect (Mux_I0_Inp);
               Disconnect (Mux_I1_Inp);
               Disconnect (Dff_Din);
               Disconnect (Clk_Inp);
               Remove_Instance (Iinst);
               Last_Inst := Dff_Inst;
               return;
            end;
         end if;
      end if;

      Last_Inst := Inst;
      Clk := No_Net;
      En := No_Net;
   end Extract_Extract_Dff;

   --  If dyn_extract gate EXTRACT is followed by a concat and a dff, then
   --  swap the dff and the concat.  This will allow to merge the dff during
   --  the build of mem_rd_sync.
   --  This creates new gates (the dff is replicated) that will be removed.
   procedure Maybe_Swap_Concat_Mux_Dff (Ctxt : Context_Acc; Extract : Instance)
   is
      Extr_Out : constant Net := Get_Output (Extract, 0);
      Concat : Instance;
      Concat_Out : Net;
      Dff : Instance;
      Clk, En : Net;
      Loc : Location_Type;
   begin
      if not Has_One_Connection (Extr_Out) then
         --  The dyn_extract is connected to more than one gate.
         return;
      end if;

      Concat := Get_Input_Parent (Get_First_Sink (Extr_Out));
      case Get_Id (Concat) is
         when Concat_Module_Id
           |  Id_Concatn =>
            null;
         when others =>
            --  Not a concat.
            return;
      end case;

      Concat_Out := Get_Output (Concat, 0);
      if not Has_One_Connection (Concat_Out) then
         --  The concat is connected to more than one gate.
         return;
      end if;
      for I in 1 .. Get_Nbr_Inputs (Concat) loop
         declare
            Src : constant Net := Get_Input_Net (Concat, I - 1);
         begin
            if Get_Id (Get_Net_Parent (Src)) /= Id_Dyn_Extract then
               --  A source of concat is not a dyn_extract.
               return;
            end if;
            if not Has_One_Connection (Src) then
               --  A source of concat drives something else!
               return;
            end if;
         end;
      end loop;

      Extract_Extract_Dff (Ctxt, Concat, Dff, Clk, En);
      if Clk = No_Net then
         return;
      end if;

      --  Replicate the dff.
      Loc := Get_Location (Dff);
      for I in 1 .. Get_Nbr_Inputs (Concat) loop
         declare
            Inp : constant Input := Get_Input (Concat, I - 1);
            Dff2 : Net;
            Mux : Net;
            Dff2_Inp : Input;
            Src : Net;
         begin
            Src := Disconnect_And_Get (Inp);

            Dff2 := Build_Dff (Ctxt, Clk, Src);
            Set_Location (Dff2, Loc);
            Connect (Inp, Dff2);

            if En /= No_Net then
               Dff2_Inp := Get_Input (Get_Net_Parent (Dff2), 1);
               Mux := Build_Mux2 (Ctxt, En, Dff2, Src);
               Set_Location (Mux, Loc);
               Disconnect (Dff2_Inp);
               Connect (Dff2_Inp, Mux);
            end if;
         end;
      end loop;

      --  Reconnect the concat.
      Redirect_Inputs (Get_Output (Dff, 0), Concat_Out);
      Remove_Instance (Dff);
   end Maybe_Swap_Concat_Mux_Dff;

   procedure Maybe_Swap_Mux_Concat_Dff (Ctxt : Context_Acc; Extract : Instance)
   is
      Concat     : Instance;
      Concat_Out : Net;
      Dff        : Instance;
      Dff_Inp    : Input;
      Dff_Out    : Net;
      Dff_Off    : Uns32;
      Clk, En    : Net;
      Loc        : Location_Type;
   begin
      declare
         Extr_Out   : constant Net := Get_Output (Extract, 0);
         Mux_Inp    : Input;
         Mux        : Instance;
         Mux_Out    : Net;
         Concat_Inp : Input;
      begin
         if not Has_One_Connection (Extr_Out) then
            --  The dyn_extract is connected to more than one gate.
            return;
         end if;

         --  The output is connected to a Mux2.
         Mux_Inp := Get_First_Sink (Extr_Out);
         Mux := Get_Input_Parent (Mux_Inp);
         if Get_Id (Mux) /= Id_Mux2 then
            --  Not a mux2.
            return;
         end if;
         Mux_Out := Get_Output (Mux, 0);

         if not Has_One_Connection (Mux_Out) then
            return;
         end if;

         --  The Mux2 output is connected to a concat.
         Concat_Inp := Get_First_Sink (Mux_Out);
         Concat := Get_Input_Parent (Concat_Inp);
         case Get_Id (Concat) is
            when Concat_Module_Id
               | Id_Concatn =>
               null;
            when others =>
               --  Not a concat.
               return;
         end case;

         --  The concat is connected to a dff.
         Concat_Out := Get_Output (Concat, 0);
         if not Has_One_Connection (Concat_Out) then
            --  The concat is connected to more than one gate.
            return;
         end if;
         Dff_Inp := Get_First_Sink (Concat_Out);
         Dff := Get_Input_Parent (Dff_Inp);
         if Get_Id (Dff) /= Id_Dff then
            return;
         end if;
      end;

      --  Check all concat inputs are connected to a mux2, which is
      --  connected to a dyn_extract.
      Dff_Out := Get_Output (Dff, 0);
      Dff_Off := 0;
      for I in reverse 1 .. Get_Nbr_Inputs (Concat) loop
         declare
            Mux_Net   : constant Net := Get_Input_Net (Concat, I - 1);
            Mux_Inst  : constant Instance := Get_Net_Parent (Mux_Net);
            Extr_Net  : Net;
            Extr_Inst : Instance;
         begin
            if Get_Id (Mux_Inst) /= Id_Mux2 then
               return;
            end if;
            if not Has_One_Connection (Mux_Net) then
               --  A source of concat drives something else!
               return;
            end if;

            Extr_Net := Get_Input_Net (Mux_Inst, 2);
            if Get_Id (Get_Net_Parent (Extr_Net)) /= Id_Dyn_Extract then
               --  A source of concat is not a dyn_extract.
               return;
            end if;
            if not Has_One_Connection (Extr_Net) then
               --  A source of concat drives something else!
               return;
            end if;

            --  Check the Mux2 is a enable for the dff.
            Extr_Net := Get_Input_Net (Mux_Inst, 1);
            Extr_Inst := Get_Net_Parent (Extr_Net);
            if Get_Id (Extr_Inst) /= Id_Extract then
               return;
            end if;
            if Get_Param_Uns32 (Extr_Inst, 0) /= Dff_Off then
               return;
            end if;
            if Get_Input_Net (Extr_Inst, 0) /= Dff_Out then
               return;
            end if;
            Dff_Off := Dff_Off + Get_Width (Mux_Net);
         end;
      end loop;

      Extract_Extract_Dff (Ctxt, Concat, Dff, Clk, En);
      if Clk = No_Net then
         return;
      end if;
      --  There is no additional enabler for the dff.
      pragma Assert (En = No_Net);

      --  Replicate the dff.
      Loc := Get_Location (Dff);
      for I in 1 .. Get_Nbr_Inputs (Concat) loop
         declare
            Inp       : constant Input := Get_Input (Concat, I - 1);
            Dff2      : Net;
            Mux_Inst2 : Instance;
            Mux_Inp2  : Input;
            Src       : Net;
            Extr_Out2 : Net;
            Extr_Inst2 : Instance;
         begin
            --  Disconnect the mux2.
            Src := Disconnect_And_Get (Inp);

            Dff2 := Build_Dff (Ctxt, Clk, Src);
            Set_Location (Dff2, Loc);
            Connect (Inp, Dff2);

            Mux_Inst2 := Get_Net_Parent (Src);
            Mux_Inp2 := Get_Input (Mux_Inst2, 1);
            Extr_Out2 := Disconnect_And_Get (Mux_Inp2);
            Connect (Mux_Inp2, Dff2);

            Extr_Inst2 := Get_Net_Parent (Extr_Out2);
            Disconnect (Get_Input (Extr_Inst2, 0));
            Remove_Instance (Extr_Inst2);
         end;
      end loop;

      --  Reconnect the concat.
      Redirect_Inputs (Get_Output (Dff, 0), Concat_Out);
      Remove_Instance (Dff);
   end Maybe_Swap_Mux_Concat_Dff;

   --  Generic procedure to call CB on each memory future port (dyn_insert
   --  or dyn_extract).
   generic
      type Data_Type is private;
      with procedure Cb (Inst : Instance;
                         Data : in out Data_Type;
                         Fail : out Boolean);
   procedure Foreach_Port (Sig : Instance; Data : in out Data_Type);

   procedure Foreach_Port (Sig : Instance; Data : in out Data_Type)
   is
      Fail : Boolean;
      Inst, Inst2 : Instance;
      Inp2        : Input;
   begin
      --  Top-level loop, for each parallel path of multiport RAMs.
      Inp2 := Get_First_Sink (Get_Output (Sig, 0));
      while Inp2 /= No_Input loop
         Inst2 := Get_Input_Parent (Inp2);
         case Get_Id (Inst2) is
            when Id_Dyn_Extract =>
               Cb (Inst2, Data, Fail);
               if Fail then
                  return;
               end if;
            when Id_Dyn_Insert
               | Id_Dyn_Insert_En =>
               Cb (Inst2, Data, Fail);
               if Fail then
                  return;
               end if;
               --  Walk till the signal.
               Inst := Inst2;
               loop
                  declare
                     Inp : Input;
                     N_Inst : Instance;
                     In_Inst : Instance;
                  begin
                     --  Check gates connected to the output.
                     Inp := Get_First_Sink (Get_Output (Inst, 0));
                     N_Inst := No_Instance;
                     while Inp /= No_Input loop
                        In_Inst := Get_Input_Parent (Inp);
                        case Get_Id (In_Inst) is
                           when Id_Dyn_Extract =>
                              Cb (In_Inst, Data, Fail);
                              if Fail then
                                 return;
                              end if;
                           when Id_Dyn_Insert_En
                              | Id_Dyn_Insert =>
                              Cb (In_Inst, Data, Fail);
                              if Fail then
                                 return;
                              end if;
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when Id_Signal
                              | Id_Isignal
                              | Id_Mem_Multiport =>
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when others =>
                              raise Internal_Error;
                        end case;
                        Inp := Get_Next_Sink (Inp);
                     end loop;
                     Inst := N_Inst;
                     exit when Inst = Sig;
                  end;
               end loop;
            when others =>
               raise Internal_Error;
         end case;
         Inp2 := Get_Next_Sink (Inp2);
      end loop;
   end Foreach_Port;

   type Gather_Ports_Type is record
      Ports : Instance_Array_Acc;
      Nports : Nat32;
   end record;

   procedure Gather_Ports_Cb
     (Inst : Instance; Data : in out Gather_Ports_Type; Fail : out Boolean) is
   begin
      Data.Nports := Data.Nports + 1;
      Data.Ports (Data.Nports) := Inst;
      Fail := False;
   end Gather_Ports_Cb;

   procedure Gather_Ports_Foreach is
     new Foreach_Port (Data_Type => Gather_Ports_Type,
                       Cb => Gather_Ports_Cb);

   --  Fill PORTS with all the ports from the SIG chain.
   procedure Gather_Ports (Sig : Instance; Ports : Instance_Array_Acc)
   is
      Data : Gather_Ports_Type;
   begin
      Data := (Ports, 0);
      Gather_Ports_Foreach (Sig, Data);
      pragma Assert (Data.Nports = Ports'Last);
   end Gather_Ports;

   --  Check if the index of Memidx MIDX is of the form: MAX - off,
   --  where MAX is the maximum value of off.
   function Is_Reverse_Range (Midx : Instance) return Boolean
   is
      pragma Assert (Get_Id (Midx) = Id_Memidx);
      Sub : constant Instance := Get_Input_Instance (Midx, 0);
      Val : Instance;
   begin
      if Get_Id (Sub) /= Id_Sub then
         return False;
      end if;
      Val := Get_Input_Instance (Sub, 0);
      if Get_Id (Val) /= Id_Const_UB32 then
         return False;
      end if;
      return Get_Param_Uns32 (Val, 0) = Get_Param_Uns32 (Midx, 1);
   end Is_Reverse_Range;

   procedure Maybe_Remap_Address
     (Ctxt : Context_Acc; Sig : Instance; Nbr_Ports : Nat32)
   is
      pragma Unreferenced (Ctxt);
      Ports : Instance_Array_Acc;
   begin
      Ports := new Instance_Array (1 .. Nbr_Ports);

      --  1. Gather all ports.
      Gather_Ports (Sig, Ports);

      --  2. From ports, get the index.
      for I in Ports'Range loop
         declare
            P   : constant Instance := Ports (I);
            Idx : Input;
         begin
            case Get_Id (P) is
               when Id_Dyn_Extract =>
                  Idx := Get_Input (P, 1);
               when Id_Dyn_Insert
                  | Id_Dyn_Insert_En =>
                  Idx := Get_Input (P, 2);
               when others =>
                  raise Internal_Error;
            end case;
            Ports (I) := Get_Net_Parent (Get_Driver (Idx));
         end;
      end loop;

      --  3.  For each dimension
      loop
         declare
            M          : Instance;
            Done       : Boolean;
            Idx        : Net;
            Is_Reverse : Boolean;
            W          : Width;
            Step       : Uns32;
            Max        : Uns32;
         begin
            Done := False;
            for I in Ports'Range loop
               --  Get the index (memidx gate).
               M := Ports (I);
               case Get_Id (M) is
                  when Id_Memidx =>
                     null;
                  when Id_Addidx =>
                     M := Get_Net_Parent (Get_Output (M, 0));
                     pragma Assert (Get_Id (M) = Id_Memidx);
                  when others =>
                     raise Internal_Error;
               end case;

               Idx := Get_Input_Net (M, 0);
               if I = 1 then
                  W := Get_Width (Idx);
                  Step := Get_Param_Uns32 (M, 0);
                  Max := Get_Param_Uns32 (M, 1);
                  Is_Reverse := Is_Reverse_Range (M);
               else
                  if Get_Width (Idx) /= W
                    or else Get_Param_Uns32 (M, 0) /= Step
                    or else Get_Param_Uns32 (M, 1) /= Max
                    or else Is_Reverse_Range (M) /= Is_Reverse
                  then
                     --  Different width, steps or direction.
                     Done := True;
                     exit;
                  end if;
               end if;
            end loop;

            exit when Done;

            --  Update ports.
            for I in Ports'Range loop
               M := Ports (I);
               case Get_Id (M) is
                  when Id_Memidx =>
                     Ports (I) := No_Instance;
                     Done := True;
                  when Id_Addidx =>
                     M := Get_Net_Parent (Get_Output (M, 0));
                     pragma Assert (Get_Id (M) = Id_Memidx);
                     Ports (I) := Get_Net_Parent (Get_Output (M, 1));
                  when others =>
                     raise Internal_Error;
               end case;

               if Is_Reverse then
                  declare
                     Inp : constant Input := Get_Input (M, 0);
                     Sub : constant Instance :=
                       Get_Net_Parent (Get_Driver (Inp));
                     Addr_Inp : constant Input := Get_Input (Sub, 1);
                     Val : Net;
                  begin
                     --  Disconnect the sub, and connect the address directly.
                     Disconnect (Inp);
                     Connect (Inp, Disconnect_And_Get (Addr_Inp));
                     --  Remove the sub and the constant.
                     Val := Disconnect_And_Get (Get_Input (Sub, 0));
                     Remove_Instance (Get_Net_Parent (Val));
                     Remove_Instance (Sub);
                  end;
               end if;
            end loop;
            exit when Done;
         end;
      end loop;

      Free_Instance_Array (Ports);
   end Maybe_Remap_Address;

   --  Create a mem_rd/mem_rd_sync from a dyn_extract gate.
   --  LAST is the last memory port on the chain.
   --  ADDR is the address (from the dyn_extract).
   --  VAL is the output of the dyn_extract.
   --
   --  Infere a synchronous read if the dyn_extract is connected to a dff.
   function Create_Read_Port
     (Ctxt : Context_Acc; Last : Net; Addr : Net; Val : Net; Step : Width)
     return Instance
   is
      W : constant Width := Get_Width (Val);
      Res : Instance;
      Inp : Input;
      Iinst : Instance;
      N : Net;
   begin
      Inp := Get_First_Sink (Val);
      if Get_Next_Sink (Inp) = No_Input then
         --  There is a single input.
         Iinst := Get_Input_Parent (Inp);
         if Get_Id (Iinst) = Id_Dff then
            --  The output of the dyn_extract is directly connected to a dff.
            --  So this is a synchronous read without enable.
            declare
               Clk_Inp : Input;
               Clk : Net;
               En : Net;
            begin
               Clk_Inp := Get_Input (Iinst, 0);
               Clk := Get_Driver (Clk_Inp);
               Disconnect (Clk_Inp);
               En := Build_Const_UB32 (Ctxt, 1, 1);
               Disconnect (Inp);
               Res := Build_Mem_Rd_Sync (Ctxt, Last, Addr, Clk, En, Step);

               --  Slice the output.
               N := Get_Output (Res, 1);
               N := Build2_Extract (Ctxt, N, 0, W);

               Redirect_Inputs (Get_Output (Iinst, 0), N);
               Remove_Instance (Iinst);
               return Res;
            end;
         elsif Get_Id (Iinst) = Id_Mux2 and then Is_Enable_Dff (Inp) then
            declare
               Mux_Out : constant Net := Get_Output (Iinst, 0);
               Mux_En_Inp : constant Input := Get_Input (Iinst, 0);
               Mux_I0_Inp : constant Input := Get_Input (Iinst, 1);
               Mux_I1_Inp : constant Input := Get_Input (Iinst, 2);
               Dff_Din : constant Input := Get_First_Sink (Mux_Out);
               Dff_Inst : constant Instance := Get_Input_Parent (Dff_Din);
               Dff_Out : constant Net := Get_Output (Dff_Inst, 0);
               Clk_Inp : constant Input := Get_Input (Dff_Inst, 0);
               Clk : constant Net := Get_Driver (Clk_Inp);
               En : Net;
            begin
               En := Get_Driver (Mux_En_Inp);
               if Dff_Out = Get_Driver (Mux_I1_Inp) then
                  En := Build_Monadic (Ctxt, Id_Not, En);
                  Copy_Location (En, Dff_Inst);
               end if;
               Disconnect (Mux_En_Inp);
               Disconnect (Mux_I0_Inp);
               Disconnect (Mux_I1_Inp);
               Disconnect (Dff_Din);
               Disconnect (Clk_Inp);
               Remove_Instance (Iinst);
               Res := Build_Mem_Rd_Sync (Ctxt, Last, Addr, Clk, En, Step);
               Set_Location (Res, Get_Location (Dff_Inst));

               --  Slice the output.
               N := Get_Output (Res, 1);
               N := Build2_Extract (Ctxt, N, 0, W);

               Redirect_Inputs (Dff_Out, N);
               Remove_Instance (Dff_Inst);
               return Res;
            end;
         end if;
      end if;

      --  Replace Dyn_Extract with mem_rd.
      Res := Build_Mem_Rd (Ctxt, Last, Addr, Step);

      --  Slice the output.
      N := Get_Output (Res, 1);
      N := Build2_Extract (Ctxt, N, 0, W);

      Redirect_Inputs (Val, N);

      return Res;
   end Create_Read_Port;

   --  MEM_INST is the memory instance.
   procedure Replace_Read_Ports
     (Ctxt : Context_Acc; Orig : Instance; Mem_Inst : Instance; Step : Width)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Last : Net;
      Inp : Input;
      Next_Inp : Input;
      Extr_Inst : Instance;
      Addr_Inp : Input;
      Addr : Net;
      Val : Net;
      Port_Inst : Instance;
   begin
      Last := Get_Output (Mem_Inst, 0);

      --  Convert readers.
      Inp := Get_First_Sink (Orig_Net);
      while Inp /= No_Input loop
         Next_Inp := Get_Next_Sink (Inp);
         Extr_Inst := Get_Input_Parent (Inp);
         case Get_Id (Extr_Inst) is
            when Id_Memory_Init =>
               null;
            when Id_Dyn_Extract =>
               Disconnect (Inp);

               --  Check offset
               if Get_Param_Uns32 (Extr_Inst, 0) /= 0 then
                  raise Internal_Error;
               end if;

               --  Convert memidx.
               Addr_Inp := Get_Input (Extr_Inst, 1);
               Addr := Get_Driver (Addr_Inp);
               Disconnect (Addr_Inp);
               Val := Get_Output (Extr_Inst, 0);
               Convert_Memidx (Ctxt, Orig, Addr, Step);

               --  Replace Dyn_Extract with mem_rd.
               Port_Inst := Create_Read_Port (Ctxt, Last, Addr, Val, Step);

               Remove_Instance (Extr_Inst);

               Last := Get_Output (Port_Inst, 0);
            when others =>
               raise Internal_Error;
         end case;
         Inp := Next_Inp;
      end loop;

      --  Close the loop.
      Connect (Get_Input (Mem_Inst, 0), Last);
   end Replace_Read_Ports;

   --  ORIG (the memory) must be Const.
   procedure Replace_ROM_Memory
     (Ctxt : Context_Acc; Orig : Instance; Step : Width)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Name : constant Sname := New_Internal_Name (Ctxt);
      Inst : Instance;
   begin
      Inst := Build_Memory_Init (Ctxt, Name, Get_Width (Orig_Net), Orig_Net);

      Replace_Read_Ports (Ctxt, Orig, Inst, Step);
   end Replace_ROM_Memory;

   type Get_Next_Status is
     (
      Status_None,
      Status_One,
      Status_Multiple
     );

   --  O is the output of a gate.  Returns the gate driven by O, ignoring
   --  Dyn_Extract or muxes to Dyn_Extract.
   --  Return No_Instance if there is no output or multiple outputs.
   procedure Get_Next_Non_Extract (O : Net;
                                   Status : out Get_Next_Status;
                                   Res : out Instance)
   is
      Inp : Input;
   begin
      Status := Status_None;
      Res := No_Instance;

      --  Scan all the gates driven by the output.
      Inp := Get_First_Sink (O);
      while Inp /= No_Input loop
         declare
            Pinst : constant Instance := Get_Input_Parent (Inp);
            This_Next_Inst : Instance;
         begin
            This_Next_Inst := No_Instance;

            case Get_Id (Pinst) is
               when Id_Dyn_Extract =>
                  --  Ignore dyn_extract
                  null;
               when Id_Mux2 =>
                  --  It is OK to have mux2, provided it is connected to
                  --  a dyn_extract.
                  declare
                     Mux_Out : constant Net := Get_Output (Pinst, 0);
                     Sub_Status : Get_Next_Status;
                     Sub_Res : Instance;
                  begin
                     if Mux_Out = O then
                        --  Avoid simple infinite recursion
                        Status := Status_None;
                        Res := No_Instance;
                        return;
                     end if;
                     Get_Next_Non_Extract (Mux_Out, Sub_Status, Sub_Res);
                     --  Expect Dyn_Extract, so no next.
                     if Sub_Status /= Status_None then
                        Status := Status_Multiple;
                        Res := No_Instance;
                        return;
                     end if;
                  end;
               when others =>
                  This_Next_Inst := Pinst;
            end case;
            if This_Next_Inst /= No_Instance then
               if Res /= No_Instance then
                  --  More than one next gate.
                  Status := Status_Multiple;
                  Res := No_Instance;
                  return;
               end if;
               Status := Status_One;
               Res := This_Next_Inst;
            end if;
         end;
         Inp := Get_Next_Sink (Inp);
      end loop;
   end Get_Next_Non_Extract;

   --  Try to reach Id_Signal/Id_Isignal (TODO: Id_Output) from dyn_insert
   --  gate FIRST_INST.  Can only walk through dyn_insert and muxes.
   --  Return the memory if found.
   function Walk_From_Insert (First_Inst : Instance) return Instance
   is
      Status : Get_Next_Status;
      Inst : Instance;
      Next_Inst : Instance;
      Last : Instance;
      O : Net;
   begin
      --  LAST is the last interesting gate (dyn_insert) which has a
      --  meaningful location.
      Last := First_Inst;

      Inst := First_Inst;
      loop
         case Get_Id (Inst) is
            when Id_Dyn_Insert
               | Id_Dyn_Insert_En =>
               if Get_Mark_Flag (Inst) then
                  --  Already seen.
                  return No_Instance;
               end if;
               Set_Mark_Flag (Inst, True);
               Last := Inst;
               O := Get_Output (Inst, 0);
            when Id_Mux2
               | Id_Mux4 =>
               O := Get_Output (Inst, 0);
            when Id_Isignal
              | Id_Signal =>
               return Inst;
            when Id_Mem_Multiport =>
               O := Get_Output (Inst, 0);
            when others =>
               if Flag_Memory_Verbose then
                  Info_Msg_Synth (+Last, "gate %i cannot be part of a memory",
                                  (1 => +Inst));
               end if;
               return No_Instance;
         end case;

         --  Next gate.
         Get_Next_Non_Extract (O, Status, Next_Inst);
         case Status is
            when Status_Multiple =>
                     --  More than one next gate.
               if Flag_Memory_Verbose then
                  Info_Msg_Synth
                    (+Last, "gate %i drives several gates", (1 => +Inst));
               end if;
               return No_Instance;
            when Status_None =>
               if Flag_Memory_Verbose then
                  Info_Msg_Synth
                    (+Last, "gate %i drives no gate", (1 => +Inst));
               end if;
               return No_Instance;
            when Status_One =>
               Inst := Next_Inst;
         end case;
      end loop;
   end Walk_From_Insert;

   function Walk_From_Extract (First_Inst : Instance) return Instance
   is
      Inst : Instance;
      Last : Instance;
   begin
      --  LAST is the last interesting gate (dyn_extract) which has a
      --  meaningful location.
      Last := First_Inst;

      Inst := First_Inst;
      loop
         case Get_Id (Inst) is
            when Id_Dyn_Extract =>
               if Get_Mark_Flag (Inst) then
                  --  Already seen.
                  return No_Instance;
               end if;
               Set_Mark_Flag (Inst, True);
               Last := Inst;
               Inst := Get_Input_Instance (Inst, 0);
            when Id_Isignal
               | Id_Signal
               | Id_Const_Bit =>
               return Inst;
            when others =>
               if Flag_Memory_Verbose then
                  Info_Msg_Synth (+Last, "gate %i cannot be part of a memory",
                                  (1 => +Last));
               end if;
               return No_Instance;
         end case;
      end loop;
   end Walk_From_Extract;

   procedure Unmark_Table (Els : Instance_Tables.Instance)
   is
      Inst : Instance;
   begin
      for I in Instance_Tables.First .. Instance_Tables.Last (Els) loop
         Inst := Els.Table (I);
         Set_Mark_Flag (Inst, False);
      end loop;
   end Unmark_Table;

   --  INSERT is a Dyn_Insert[_En].  Get the next gates until reaching a
   --  signal.
   --  Validate that signal SIG is a RAM.  It must be a loop of inserts
   --  and extracts.
   function Validate_RAM_Simple (Insert : Instance) return Instance
   is
      Inst : Instance;
      N : Net;
      Inp : Input;
   begin
      --  For each gate of the chain, starting from LAST and going forward
      --  until the signal.
      N := Get_Output (Insert, 0);
      while N /= No_Net loop
         Inp := Get_First_Sink (N);
         N := No_Net;

         while Inp /= No_Input loop
            Inst := Get_Input_Parent (Inp);
            case Get_Id (Inst) is
               when Id_Dyn_Insert_En
                  | Id_Mem_Multiport =>
                  if N /= No_Net then
                     --  There must be only one such gate per stage.
                     return No_Instance;
                  end if;
                  N := Get_Output (Inst, 0);
               when Id_Dyn_Extract =>
                  null;
               when Id_Isignal
                  | Id_Signal =>
                  return Inst;
               when others =>
                  return No_Instance;
            end case;
            Inp := Get_Next_Sink (Inp);
         end loop;
      end loop;
      return No_Instance;
   end Validate_RAM_Simple;

   --  Validate that signal SIG is a RAM.  It must be a loop of inserts
   --  and extracts.
   function Validate_RAM_Multiple (Sig : Instance) return Boolean
   is
      Ok : Boolean;
      Inst : Instance;
      N : Net;
      Inp : Input;
   begin
      Ok := False;
      N := Get_Output (Sig, 0);
      Inp := Get_First_Sink (N);

      --  For multiple ports, there can be parallel pathes.
      while Inp /= No_Input loop
         Inst := Get_Input_Parent (Inp);
         case Get_Id (Inst) is
            when Id_Dyn_Insert_En
               | Id_Dyn_Insert =>
               --  Look.
               if Validate_RAM_Simple (Inst) /= Sig then
                  return False;
               end if;
               Ok := True;
            when Id_Dyn_Extract =>
               null;
            when others =>
               return False;
         end case;
         Inp := Get_Next_Sink (Inp);
      end loop;

      --  Need at least one dyn_insert.
      return Ok;
   end Validate_RAM_Multiple;

   --  Test if V is part of the conjunction CONJ generated by mux2 controls.
   function In_Conjunction (Conj : Net; V : Net; Negate : Boolean)
                           return Boolean
   is
      Inst : Instance;
      N : Net;
   begin
      if Negate then
         --  TODO.
         raise Internal_Error;
      end if;

      --  Simple case (but important for the memories)
      if V = Conj then
         return True;
      end if;

      N := Conj;
      Inst := Get_Net_Parent (N);
      loop
         Inst := Get_Net_Parent (N);
         if Get_Id (Inst) /= Id_And then
            return N = V;
         end if;

         --  Inst is AND2.
         if Get_Input_Net (Inst, 0) = V then
            return True;
         end if;
         N := Get_Input_Net (Inst, 1);
      end loop;
   end In_Conjunction;

   --  Subroutine of Reduce_Extract_Muxes.
   --  MUX is a mux2 that is removed if possible.
   procedure Reduce_Extract_Muxes_Mux2 (Mux : Instance; Port : Port_Idx)
   is
      pragma Assert (Get_Id (Mux) = Id_Mux2);
      Sel : constant Net := Get_Input_Net (Mux, 0);
      Val : constant Net := Get_Input_Net (Mux, 1 + Port);
      Old : constant Net := Get_Input_Net (Mux, 1 + (1 - Port));
      First_Parent, Last_Parent : Instance;
      P : Instance;
      N : Net;
   begin
      --  Search the parent.
      First_Parent := Get_Net_Parent (Val);
      P := First_Parent;
      loop
         if Get_Id (P) /= Id_Dyn_Insert_En then
            if Flag_Memory_Verbose then
               Info_Msg_Synth
                 (+Mux, "mux %i before extract is not a bypass", (1 => +Mux));
            end if;
            return;
         end if;
         --  Get the MEM input.
         N := Get_Input_Net (P, 0);
         exit when N = Old;
         P := Get_Net_Parent (N);
      end loop;
      Last_Parent := P;

      --  Check not SEL (resp. SEL) implies disable for all dyn_insert_en
      --  parents.
      P := First_Parent;
      loop
         --  Get the enable of Dyn_Insert_En parent.
         N := Get_Input_Net (P, 3);
         if not In_Conjunction (N, Sel, Port = 0) then
            if Flag_Memory_Verbose then
               Info_Msg_Synth
                 (+Mux, "mux %i before extract is required",
                  (1 => +Mux));
            end if;
            return;
         end if;
         exit when P = Last_Parent;
         P := Get_Net_Parent (Get_Input_Net (P, 0));
      end loop;

      --  So Mux2 is not required.
      Disconnect (Get_Input (Mux, 0));
      Disconnect (Get_Input (Mux, 1));
      Disconnect (Get_Input (Mux, 2));
      Redirect_Inputs (Get_Output (Mux, 0), Val);
      Remove_Instance (Mux);
   end Reduce_Extract_Muxes_Mux2;

   --  SIG is a signal/isignal at the start of a memory, which consists of
   --  one or more chain of Dyn_Insert.  Dyn_Extract are also allowed on this
   --  chain.  It is also possible to have Mux2 before Dyn_Extract because of
   --  enable signals.
   --  Try to remove these mux2.
   --  * They should be between the output of a dyn_insert and the input of
   --    a dyn_extract
   --  * The other input of the mux2 must be a parent of the dyn_insert input
   --    (in the chain of dyn_insert).
   --  * All the dyn_insert until the parent must be disabled when the mux2 is
   --    disabled.
   procedure Reduce_Extract_Muxes (Sig : Instance)
   is
      N : Net;
      Inp : Input;
      Next_Inp : Input;
      Inst : Instance;
   begin
      N := Get_Output (Sig, 0);
      Inp := Get_First_Sink (N);
      while Inp /= No_Input loop
         --  INP can be removed, so get the next input now.
         Next_Inp := Get_Next_Sink (Inp);

         Inst := Get_Input_Parent (Inp);
         case Get_Id (Inst) is
            when Id_Dyn_Insert
               | Id_Dyn_Insert_En =>
               --  Recurse on it.
               Reduce_Extract_Muxes (Inst);
               Next_Inp := Get_Next_Sink (Inp);

            when Id_Mux2 =>
               if Inp = Get_Input (Inst, 1) then
                  --  Selected when Sel = 0
                  Reduce_Extract_Muxes_Mux2 (Inst, 0);
               elsif Inp = Get_Input (Inst, 2) then
                  --  Selected when Sel = 1
                  Reduce_Extract_Muxes_Mux2 (Inst, 1);
               else
                  raise Internal_Error;
               end if;
            when Id_Isignal
               | Id_Signal
               | Id_Mem_Multiport =>
               --  Stop here: do not recurse.
               null;
            when Id_Dyn_Extract =>
               --  Ignore.
               null;
            when others =>
               null;
         end case;
         Inp := Next_Inp;
      end loop;
   end Reduce_Extract_Muxes;

   --  Extract the step (equivalent to data width) of a dyn_insert/dyn_extract
   --  address.  This is either a memidx or an addidx gate.
   function Extract_Memidx_Step (Memidx : Instance) return Width
   is
      Inst : Instance;
   begin
      Inst := Memidx;
      loop
         case Get_Id (Inst) is
            when Id_Addidx =>
               Inst := Get_Input_Instance (Inst, 1);
            when Id_Memidx =>
               return Get_Param_Uns32 (Inst, 0);
            when others =>
               raise Internal_Error;
         end case;
      end loop;
   end Extract_Memidx_Step;

   type Off_Array is array (Int32 range <>) of Uns32;
   type Off_Array_Acc is access Off_Array;

   procedure Free_Off_Array is new Ada.Unchecked_Deallocation
     (Off_Array, Off_Array_Acc);

   function Off_Array_Search (Arr : Off_Array; Off : Uns32) return Int32 is
   begin
      for I in Arr'Range loop
         if Arr (I) = Off then
            return I;
         end if;
      end loop;
      raise Internal_Error;
   end Off_Array_Search;

   procedure Off_Array_To_Idx (Arr: Off_Array;
                               Off : Uns32;
                               Wd : Uns32;
                               Idx : out Int32;
                               Len : out Int32)
   is
      Idx2 : Int32;
   begin
      Idx := Off_Array_Search (Arr, Off);
      Idx2 := Off_Array_Search (Arr (Idx + 1 .. Arr'Last), Off + Wd);
      Len := Idx2 - Idx;
   end Off_Array_To_Idx;

   type Copy_Mode_Type is (Copy_Mode_Bit, Copy_Mode_Val, Copy_Mode_Zx);

   procedure Copy_Const_Content (Src : Instance;
                                 Src_Off : Width;
                                 Src_Wd : Width;
                                 Dst : Instance;
                                 Dst_Wd : Width;
                                 Depth : Uns32;
                                 Mode : Copy_Mode_Type)
   is
      function Off_To_Param (Off : Uns32) return Param_Idx
      is
         Res : constant Param_Idx := Param_Idx (Off / 32);
      begin
         case Mode is
            when Copy_Mode_Bit =>
               return Res;
            when Copy_Mode_Val =>
               return Res * 2;
            when Copy_Mode_Zx =>
               return Res * 2 + 1;
         end case;
      end Off_To_Param;

      Boff : Uns32;
      Nbits : Uns32;
      Word_Idx : Param_Idx;
      Word_Off : Uns32;

      Soff : Uns32;
      Slen : Uns32;
      Sval : Uns32;

      Doff : Uns32;
      Dlen : Uns32;
      Dval : Uns32;
   begin
      Boff := Src_Off;
      Doff := 0;
      for I in 0 .. Depth - 1 loop
         Nbits := Dst_Wd;
         Soff := Boff;
         while Nbits > 0 loop
            --  Try to read as much as possible.
            Word_Idx := Off_To_Param (Soff);
            Word_Off := Soff mod 32;
            Slen := 32 - Word_Off;
            if Slen > Nbits then
               Slen := Nbits;
            end if;
            Sval := Get_Param_Uns32 (Src, Word_Idx);
            --  Reframe (put at bit 0, mask extra bits).
            Sval := Shift_Right (Sval, Natural (Word_Off));
            Sval := Sval and Shift_Right (16#ffff_ffff#,
                                          Natural (32 - Slen));

            Soff := Soff + Slen;
            Nbits := Nbits - Slen;

            --  Store.
            while Slen > 0 loop
               Word_Idx := Off_To_Param (Doff);
               Word_Off := Doff mod 32;
               Dlen := 32 - Word_Off;
               if Dlen > Slen then
                  Dlen := Slen;
               end if;
               Dval := Sval and Shift_Right (16#ffff_ffff#,
                                             Natural (32 - Dlen));
               Dval := Shift_Left (Dval, Natural (Word_Off));
               Dval := Dval or Get_Param_Uns32 (Dst, Word_Idx);
               Set_Param_Uns32 (Dst, Word_Idx, Dval);

               Sval := Shift_Right (Sval, Natural (Dlen));
               Slen := Slen - Dlen;
               Doff := Doff + Dlen;
            end loop;
         end loop;
         Boff := Boff + Src_Wd;
      end loop;
   end Copy_Const_Content;

   --  From constant net CST (used to initialize a memory), extract DEPTH sub
   --  words (bits OFF:OFF + WD - 1).
   --  Used when memories are split.
   function Extract_Sub_Constant (Ctxt : Context_Acc;
                                  Cst : Instance;
                                  Cst_Wd : Uns32;
                                  Off : Uns32;
                                  Wd : Uns32;
                                  Depth : Uns32) return Net
   is
      pragma Assert (Depth /= 0);
      Mem_Wd : constant Width := Wd * Depth;
      Res : Instance;
   begin
      case Get_Id (Cst) is
         when Id_Const_Bit =>
            Res := Build_Const_Bit (Ctxt, Mem_Wd);
            Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                Copy_Mode_Bit);
            return Get_Output (Res, 0);
         when Id_Const_Log =>
            Res := Build_Const_Log (Ctxt, Mem_Wd);
            Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                Copy_Mode_Val);
            Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                Copy_Mode_Zx);
            return Get_Output (Res, 0);
         when Id_Const_UB32 =>
            declare
               N : Net;
            begin
               N := Build_Const_UB32 (Ctxt, 0, Mem_Wd);
               --  Optimize: no need to copy if the value is 0.
               if Get_Param_Uns32 (Cst, 0) /= 0 then
                  Res := Get_Net_Parent (N);
                  Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                      Copy_Mode_Bit);
               end if;
               return N;
            end;
         when Id_Const_UL32 =>
            declare
               N : Net;
            begin
               N := Build_Const_UL32 (Ctxt, 0, 0, Mem_Wd);
               --  Optimize: no need to copy if the value is 0.
               Res := Get_Net_Parent (N);
               Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                   Copy_Mode_Val);
               Copy_Const_Content (Cst, Off, Cst_Wd, Res, Wd, Depth,
                                   Copy_Mode_Zx);
               return N;
            end;
         when Id_Const_X =>
            return Build_Const_X (Ctxt, Mem_Wd);
         when others =>
            raise Internal_Error;
      end case;
   end Extract_Sub_Constant;

   --  Subroutine of Convert_To_Memory.
   --
   --  Compute the number of ports (dyn_extract and dyn_insert) and the width
   --  of the memory.  Just walk all the gates.
   procedure Compute_Ports_And_Width
     (Sig : Instance; Nbr_Ports : out Int32; Wd : out Width)
   is
      procedure Add_Port_And_Width (Memidx : Instance) is
      begin
         Nbr_Ports := Nbr_Ports + 1;
         if Wd = 0 then
            Wd := Extract_Memidx_Step (Memidx);
         elsif Wd /= Extract_Memidx_Step (Memidx) then
            Info_Msg_Synth (+Sig, "memory %n uses different widths",
                            (1 => +Sig));
            Nbr_Ports := 0;
            return;
         end if;
      end Add_Port_And_Width;

      Inst, Inst2 : Instance;
      Inp2 : Input;
   begin
      Nbr_Ports := 0;
      Wd := 0;

      --  Top-level loop, for each parallel path of multiport RAMs.
      Inp2 := Get_First_Sink (Get_Output (Sig, 0));
      while Inp2 /= No_Input loop
         Inst2 := Get_Input_Parent (Inp2);
         case Get_Id (Inst2) is
            when Id_Dyn_Extract =>
               Add_Port_And_Width (Get_Input_Instance (Inst2, 1));
               if Nbr_Ports = 0 then
                  return;
               end if;
            when Id_Dyn_Insert
              | Id_Dyn_Insert_En =>
               Add_Port_And_Width (Get_Input_Instance (Inst2, 2));
               if Nbr_Ports = 0 then
                  return;
               end if;
               --  Walk till the signal.
               Inst := Inst2;
               loop
                  declare
                     Inp : Input;
                     N_Inst : Instance;
                     In_Inst : Instance;
                  begin
                     --  Check gates connected to the output.
                     Inp := Get_First_Sink (Get_Output (Inst, 0));
                     N_Inst := No_Instance;
                     while Inp /= No_Input loop
                        In_Inst := Get_Input_Parent (Inp);
                        case Get_Id (In_Inst) is
                           when Id_Dyn_Extract =>
                              Add_Port_And_Width
                                (Get_Input_Instance (In_Inst, 1));
                              if Nbr_Ports = 0 then
                                 return;
                              end if;
                           when Id_Dyn_Insert_En
                              | Id_Dyn_Insert =>
                              Add_Port_And_Width
                                (Get_Input_Instance (In_Inst, 2));
                              if Nbr_Ports = 0 then
                                 return;
                              end if;
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when Id_Signal
                              | Id_Isignal
                              | Id_Mem_Multiport =>
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when others =>
                              raise Internal_Error;
                        end case;
                        Inp := Get_Next_Sink (Inp);
                     end loop;
                     Inst := N_Inst;
                     exit when Inst = Sig;
                  end;
               end loop;
            when others =>
               raise Internal_Error;
         end case;
         Inp2 := Get_Next_Sink (Inp2);
      end loop;
   end Compute_Ports_And_Width;

   --  Subroutine of Convert_To_Memory.
   --
   --  Extract offsets/width of each port.
   procedure Extract_Ports_Offsets
     (Sig : Instance; Offs : Off_Array_Acc; Nbr_Offs : out Int32)
   is
      procedure Add_Offset (Off : Uns32; Wd : Uns32)
      is
         Ow : Off_Array (1 .. 2);
      begin
         Ow := (Off, Off + Wd);
         if Nbr_Offs = 0 or else Ow /= Offs (1 .. 2) then
            Nbr_Offs := Nbr_Offs + 2;
            Offs (Nbr_Offs -1 .. Nbr_Offs) := Ow;
         end if;
      end Add_Offset;

      procedure Add_Extract_Offset (Inst : Instance) is
      begin
         Add_Offset (Get_Param_Uns32 (Inst, 0),
                     Get_Width (Get_Output (Inst, 0)));
      end Add_Extract_Offset;

      procedure Add_Insert_Offset (Inst : Instance) is
      begin
         Add_Offset (Get_Param_Uns32 (Inst, 0),
                     Get_Width (Get_Input_Net (Inst, 1)));
      end Add_Insert_Offset;

      Inst, Inst2 : Instance;
      Inp2 : Input;
   begin
      Nbr_Offs := 0;

      --  Top-level loop, for each parallel path of multiport RAMs.
      Inp2 := Get_First_Sink (Get_Output (Sig, 0));
      while Inp2 /= No_Input loop
         Inst2 := Get_Input_Parent (Inp2);
         case Get_Id (Inst2) is
            when Id_Dyn_Extract =>
               Add_Extract_Offset (Inst2);
            when Id_Dyn_Insert_En
              | Id_Dyn_Insert =>
               Add_Insert_Offset (Inst2);
               Inst := Inst2;
               loop
                  declare
                     Inp : Input;
                     N_Inst : Instance;
                     In_Inst : Instance;
                  begin
                     --  Check gates connected to the output.
                     Inp := Get_First_Sink (Get_Output (Inst, 0));
                     N_Inst := No_Instance;
                     while Inp /= No_Input loop
                        In_Inst := Get_Input_Parent (Inp);
                        case Get_Id (In_Inst) is
                           when Id_Dyn_Extract =>
                              Add_Extract_Offset (In_Inst);
                           when Id_Dyn_Insert_En
                              | Id_Dyn_Insert =>
                              Add_Insert_Offset (In_Inst);
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when Id_Signal
                              | Id_Isignal
                              | Id_Mem_Multiport =>
                              pragma Assert (N_Inst = No_Instance);
                              N_Inst := In_Inst;
                           when others =>
                              raise Internal_Error;
                        end case;
                        Inp := Get_Next_Sink (Inp);
                     end loop;
                     Inst := N_Inst;
                     exit when Inst = Sig;
                  end;
               end loop;
            when others =>
               raise Internal_Error;
         end case;
         Inp2 := Get_Next_Sink (Inp2);
      end loop;
   end Extract_Ports_Offsets;

   procedure Convert_Memory_Read_Port (Ctxt : Context_Acc;
                                       In_Inst : Instance;
                                       Mem_Sz : Uns32;
                                       Mem_W : Width;
                                       Offs : Off_Array_Acc;
                                       Tails : Net_Array_Acc;
                                       Outs : Net_Array_Acc)
   is
      Off : constant Uns32 := Get_Param_Uns32 (In_Inst, 0);
      Wd : constant Width := Get_Width (Get_Output (In_Inst, 0));
      Idx : Int32;
      Len : Int32;
      Addr : Net;
      Rd_Inst : Instance;
      Rd : Net;
      Inp2 : Input;
      En : Net;
      Clk : Net;
      Last_Inst : Instance;
   begin
      Off_Array_To_Idx (Offs.all, Off, Wd, Idx, Len);
      Inp2 := Get_Input (In_Inst, 1);
      Addr := Get_Driver (Inp2);
      Disconnect (Inp2);
      Convert_Memidx (Ctxt, Mem_Sz, Addr, Mem_W);

      Maybe_Swap_Concat_Mux_Dff (Ctxt, In_Inst);
      Maybe_Swap_Mux_Concat_Dff (Ctxt, In_Inst);

      Extract_Extract_Dff (Ctxt, In_Inst, Last_Inst, Clk, En);
      if Clk /= No_Net and then En = No_Net then
         En := Build_Const_UB32 (Ctxt, 1, 1);
      end if;
      --  iterate to build mem_rd/mem_rd_sync
      for I in Idx .. Idx + Len - 1 loop
         if Clk /= No_Net then
            Rd_Inst := Build_Mem_Rd_Sync (Ctxt, Tails (I), Addr, Clk, En,
                                          Offs (Idx + 1) - Offs (Idx));
         else
            Rd_Inst := Build_Mem_Rd (Ctxt, Tails (I), Addr,
                                     Offs (Idx + 1) - Offs (Idx));
         end if;
         Tails (I) := Get_Output (Rd_Inst, 0);
         Outs (I) := Get_Output (Rd_Inst, 1);
      end loop;
      Rd := Build2_Concat (Ctxt, Outs (Idx .. Idx + Len - 1));
      Redirect_Inputs (Get_Output (Last_Inst, 0), Rd);
      if Last_Inst /= In_Inst then
         Remove_Instance (Last_Inst);
      end if;
   end Convert_Memory_Read_Port;

   --  Subroutine of Convert_To_Memory.
   --
   --  Convert dyn_insert/dyn_extract to memory write/read ports.
   --  TAILS is the output of memories (so the next value to be read).
   --  OUTS is a temporary array.
   procedure Create_Memory_Ports (Ctxt : Context_Acc;
                                  Sig : Instance;
                                  Mem_Sz : Uns32;
                                  Mem_W : Width;
                                  Offs : Off_Array_Acc;
                                  Tails : Net_Array_Acc;
                                  Outs : Net_Array_Acc)
   is
      Inst, Inst2 : Instance;
      Inp, Inp2 : Input;
      N_Inp, N_Inp2 : Input;
      N_Inst : Instance;
      In_Inst : Instance;
   begin
      --  Start from the end.
      --  First: the read ports at the end.
      Inp2 := Get_First_Sink (Get_Output (Sig, 0));
      while Inp2 /= No_Input loop
         N_Inp2 := Get_Next_Sink (Inp2);
         Inst2 := Get_Input_Parent (Inp2);
         case Get_Id (Inst2) is
            when Id_Dyn_Extract =>
               Convert_Memory_Read_Port
                 (Ctxt, Inst2, Mem_Sz, Mem_W, Offs, Tails, Outs);
               Disconnect (Get_Input (Inst2, 0));
               Remove_Instance (Inst2);
            when Id_Dyn_Insert_En
              | Id_Dyn_Insert =>
               null;
            when others =>
               raise Internal_Error;
         end case;
         Inp2 := N_Inp2;
      end loop;

      --  Second, the chains.
      Inp2 := Get_First_Sink (Get_Output (Sig, 0));
      while Inp2 /= No_Input loop
         N_Inp2 := Get_Next_Sink (Inp2);
         Inst2 := Get_Input_Parent (Inp2);

         --  Do the real work: transform gates to ports.
         Disconnect (Get_Input (Inst2, 0));
         Inst := Inst2;
         loop
            --  Handle Inst.  If the output is connected to a write port,
            --  add it (after the read ports).
            case Get_Id (Inst) is
               when Id_Dyn_Insert_En
                 | Id_Dyn_Insert =>
                  declare
                     Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
                     Wd : constant Width :=
                       Get_Width (Get_Input_Net (Inst, 1));
                     Idx : Int32;
                     Len : Int32;
                     Addr : Net;
                     Wr_Inst : Instance;
                     Inp2 : Input;
                     Dat : Net;
                     En : Net;
                     Clk : Net;
                  begin
                     Off_Array_To_Idx (Offs.all, Off, Wd, Idx, Len);
                     Inp2 := Get_Input (Inst, 2);
                     Addr := Get_Driver (Inp2);
                     Disconnect (Inp2);
                     Convert_Memidx (Ctxt, Mem_Sz, Addr, Mem_W);
                     if Get_Id (Inst) = Id_Dyn_Insert_En then
                        Inp2 := Get_Input (Inst, 3);
                        Inference.Extract_Clock
                          (Ctxt, Get_Driver (Inp2), Clk, En);
                        Disconnect (Inp2);
                     else
                        Clk := No_Net;
                        En := No_Net;
                     end if;
                     pragma Assert (Clk /= No_Net);
                     if En = No_Net then
                        En := Build_Const_UB32 (Ctxt, 1, 1);
                     end if;
                     Inp2 := Get_Input (Inst, 1);
                     Dat := Get_Driver (Inp2);
                     for I in Idx .. Idx + Len - 1 loop
                        Wr_Inst := Build_Mem_Wr_Sync
                          (Ctxt, Tails (I), Addr, Clk, En,
                           Build2_Extract (Ctxt, Dat, Offs (I) - Offs (Idx),
                                           Offs (I + 1) - Offs (I)));
                        Tails (I) := Get_Output (Wr_Inst, 0);
                     end loop;
                     Disconnect (Inp2);
                  end;
               when Id_Signal
                  | Id_Isignal =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;

            --  Check gates connected to the output.
            --  First the read ports (dyn_extract), and also find the next
            --  gate in the loop.
            N_Inst := No_Instance;
            Inp := Get_First_Sink (Get_Output (Inst, 0));
            while Inp /= No_Input loop
               In_Inst := Get_Input_Parent (Inp);
               N_Inp := Get_Next_Sink (Inp);
               case Get_Id (In_Inst) is
                  when Id_Dyn_Extract =>
                     Convert_Memory_Read_Port
                       (Ctxt, In_Inst, Mem_Sz, Mem_W, Offs, Tails, Outs);
                     pragma Assert (Inp = Get_Input (In_Inst, 0));
                     Disconnect (Inp);
                     Remove_Instance (In_Inst);
                  when Id_Dyn_Insert_En
                     | Id_Dyn_Insert
                     | Id_Signal
                     | Id_Isignal =>
                     pragma Assert (Inp = Get_Input (In_Inst, 0));
                     Disconnect (Inp);
                     --  This is the next instance (and there must be only
                     --  one next instance).
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when Id_Mem_Multiport =>
                     Disconnect (Inp);
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when others =>
                     raise Internal_Error;
               end case;
               Inp := N_Inp;
            end loop;

            --  Remove INST.
            case Get_Id (Inst) is
               when Id_Dyn_Insert_En
                  | Id_Dyn_Insert =>
                  Remove_Instance (Inst);
               when Id_Signal
                  | Id_Isignal =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;

            Inst := N_Inst;
            case Get_Id (Inst) is
               when Id_Signal
                  | Id_Isignal
                  | Id_Mem_Multiport =>
                  exit;
               when others =>
                  null;
            end case;
         end loop;
         Inp2 := N_Inp2;
      end loop;
   end Create_Memory_Ports;

   --  Return True iff the initial value of SIG is uniform (same value for
   --  all bits).
   function Is_Simple_Init (Sig : Instance) return Boolean
   is
      pragma Assert (Get_Id (Sig) = Id_Isignal);
      Cst : constant Instance := Get_Input_Instance (Sig, 1);
   begin
      case Get_Id (Cst) is
         when Id_Const_0
            | Id_Const_X =>
            return True;
         when Id_Const_UB32 =>
            return Get_Param_Uns32 (Cst, 0) = 0;
         when others =>
            return False;
      end case;
   end Is_Simple_Init;

   procedure Convert_To_Memory (Ctxt : Context_Acc; Sig : Instance)
   is
      --  Size of RAM (in bits).
      Mem_Sz : constant Uns32 := Get_Width (Get_Output (Sig, 0));

      Sig_Name : constant Sname := Get_Instance_Name (Sig);

      --  Width of the RAM, computed from the step of memidx.
      Mem_W : Width;

      --  Number of addresses of the memory.
      --  Sz = W * Depth.
      Mem_Depth : Uns32;

      Nbr_Ports : Int32;
      Inst : Instance;
      Name : Sname;

      --  Table of offsets.
      --  The same RAM can be partially read or written: not all the bits of
      --  the data bus are read or written.  The RAM is split in several
      --  sub-rams which are fully read/written.
      --  This table will contain the offset of each sub-rams.
      Offs : Off_Array_Acc;
      Nbr_Offs : Int32;

      Heads : Instance_Array_Acc;
      Tails : Net_Array_Acc;
      Outs : Net_Array_Acc;
   begin
      --  1. Walk to count number of insert/extract instances + extract width
      Nbr_Ports := 0;
      Mem_W := 0;
      Inst := Sig;
      Compute_Ports_And_Width (Sig, Nbr_Ports, Mem_W);
      if Nbr_Ports = 0 then
         return;
      end if;

      if Mem_W = 0 then
         --  No ports ?
         raise Internal_Error;
      end if;

      Mem_Depth := Mem_Sz / Mem_W;

      Info_Msg_Synth
        (+Sig, "found RAM %n, width: %v bits, depth: %v",
         (1 => +Sig, 2 => +Mem_W, 3 => +Mem_Depth));

      if Get_Id (Sig) = Id_Signal
        or else (Get_Id (Sig) = Id_Isignal and then Is_Simple_Init (Sig))
      then
         Maybe_Remap_Address (Ctxt, Sig, Nbr_Ports);
      end if;

      --  2. Walk to extract offsets/width
      Offs := new Off_Array (1 .. 2 * Nbr_Ports);
      Extract_Ports_Offsets (Sig, Offs, Nbr_Offs);

      --  2.1 Sort the offsets.
      declare
         function Lt (Op1, Op2 : Natural) return Boolean is
         begin
            return Offs (Nat32 (Op1)) < Offs (Nat32 (Op2));
         end Lt;

         procedure Swap (From : Natural; To : Natural)
         is
            T : Uns32;
         begin
            T := Offs (Nat32 (From));
            Offs (Nat32 (From)) := Offs (Nat32 (To));
            Offs (Nat32 (To)) := T;
         end Swap;

         procedure Heap_Sort is new Grt.Algos.Heap_Sort
           (Lt => Lt, Swap => Swap);
      begin
         Heap_Sort (Natural (Nbr_Offs));
      end;

      --  2.2 Remove duplicates.
      declare
         P : Nat32;
      begin
         P := 1;
         for I in 2 .. Nbr_Offs loop
            if Offs (I) /= Offs (P) then
               P := P + 1;
               if P /= I then
                  Offs (P) := Offs (I);
               end if;
            end if;
         end loop;
         Nbr_Offs := P;
      end;

      if Offs (Nbr_Offs) < Mem_W then
         --  Be sure the whole data width is covered.
         --  FIXME: simply discard unused data bits ?
         Nbr_Offs := Nbr_Offs + 1;
         Offs (Nbr_Offs) := Mem_W;
      end if;

      --  3. Create array of instances
      Heads := new Instance_Array (1 .. Nbr_Offs - 1);
      Tails := new Net_Array (1 .. Nbr_Offs - 1);
      Outs := new Net_Array (1 .. Nbr_Offs - 1);

      --  4. Create Memory/Memory_Init from signal/isignal.
      for I in 1 .. Nbr_Offs - 1 loop
         --  Reuse signal name for the memory name.
         if Nbr_Offs = 2 then
            Name := Sig_Name;
         else
            Name := New_Sname_Version (Uns32 (I), Sig_Name);
         end if;

         declare
            Data_Wd : constant Width := Offs (I + 1) - Offs (I);
            Mem_Wd : constant Width := Data_Wd * Mem_Depth;
         begin
            case Get_Id (Sig) is
               when Id_Isignal =>
                  Heads (I) := Build_Memory_Init
                    (Ctxt, Name, Mem_Wd,
                     Extract_Sub_Constant
                       (Ctxt, Get_Input_Instance (Sig, 1),
                        Mem_W, Offs (I), Data_Wd, Mem_Depth));
               when Id_Signal =>
                  Heads (I) := Build_Memory (Ctxt, Name, Mem_Wd);
               when others =>
                  raise Internal_Error;
            end case;
            Copy_Instance_Attributes (Heads (I), Sig);
            Tails (I) := Get_Output (Heads (I), 0);
         end;
      end loop;

      --  5. For each part of the data, create memory ports
      Create_Memory_Ports (Ctxt, Sig, Mem_Sz, Mem_W, Offs, Tails, Outs);

      --  Close loops.
      for I in Heads'Range loop
         Connect (Get_Input (Heads (I), 0), Tails (I));
      end loop;

      --  Finish to remove the signal/isignal.
      case Get_Id (Inst) is
         when Id_Isignal =>
            Disconnect (Get_Input (Inst, 1));
         when Id_Signal =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      declare
         Inst2 : Instance;
         Inp2 : Input;
         N2 : Net;
      begin
         --  The multiport.
         Inst2 := Inst;
         Inp2 := Get_Input (Inst2, 0);
         loop
            N2 := Get_Driver (Inp2);
            if N2 /= No_Net then
               Disconnect (Inp2);
               Remove_Instance (Inst2);
            else
               Remove_Instance (Inst2);
               exit;
            end if;
            Inst2 := Get_Net_Parent (N2);
            pragma Assert (Get_Id (Inst2) = Id_Mem_Multiport);
            pragma Assert (Get_Driver (Get_Input (Inst2, 0)) = No_Net);
            Inp2 := Get_Input (Inst2, 1);
         end loop;
      end;

      --  6. Cleanup.
      Free_Off_Array (Offs);
      Free_Instance_Array (Heads);
      Free_Net_Array (Tails);
      Free_Net_Array (Outs);
   end Convert_To_Memory;

   function Is_Const_Input (Inst : Instance) return Boolean is
   begin
      case Get_Id (Inst) is
         when Constant_Module_Id =>
            return True;
         when Id_Signal
           | Id_Isignal =>
            declare
               Inp : constant Net := Get_Input_Net (Inst, 0);
            begin
               if Inp = No_Net then
                  return False;
               else
                  return Is_Const_Input (Get_Net_Parent (Inp));
               end if;
            end;
         when others =>
            --  FIXME: handle other consts ?
            return False;
      end case;
   end Is_Const_Input;

   procedure Extract_Memories (Ctxt : Context_Acc; M : Module)
   is
      Dyns : Instance_Tables.Instance;
      Mems : Instance_Tables.Instance;
      Inst : Instance;
   begin
      Instance_Tables.Init (Dyns, 16);

      --  Gather all Dyn_Insert/Dyn_Extract.
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         --  Walk all the instances of M:
         case Get_Id (Inst) is
            when Id_Dyn_Insert_En
              | Id_Dyn_Insert
              | Id_Dyn_Extract =>
               Instance_Tables.Append (Dyns, Inst);
               pragma Assert (Get_Mark_Flag (Inst) = False);
            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;

      if Instance_Tables.Last (Dyns) < Instance_Tables.First then
         --  No dyn gates so no memory.  Early return.
         Instance_Tables.Free (Dyns);
         return;
      end if;

      Instance_Tables.Init (Mems, 16);

      --  Extract memories from dyn gates:
      --   get the isignal/signal/const gate at the origin of the dyn gate.
      for I in Instance_Tables.First .. Instance_Tables.Last (Dyns) loop
         Inst := Dyns.Table (I);
         if not Get_Mark_Flag (Inst) then
            case Get_Id (Inst) is
               when Id_Dyn_Insert
                 | Id_Dyn_Insert_En =>
                  Inst := Walk_From_Insert (Inst);
               when Id_Dyn_Extract =>
                  Inst := Walk_From_Extract (Inst);
               when others =>
                  raise Internal_Error;
            end case;
            if Inst /= No_Instance
              and then not Get_Mark_Flag (Inst)
            then
               --  New (candidate) memory !
               Set_Mark_Flag (Inst, True);
               Instance_Tables.Append (Mems, Inst);
            end if;
         end if;
      end loop;

      --  Unmark dyn gates.
      Unmark_Table (Dyns);
      Instance_Tables.Free (Dyns);

      --  Unmark memory gates.
      Unmark_Table (Mems);

      for I in Instance_Tables.First .. Instance_Tables.Last (Mems) loop
         --  INST is the memorizing instance, ie isignal/signal.
         Inst := Mems.Table (I);
         declare
            Data_W : Width;
            Size : Width;
         begin
            case Get_Id (Inst) is
               when Id_Isignal
                 | Id_Signal
                 | Id_Const_Bit =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;

            if Is_Const_Input (Inst) then
               Check_Memory_Read_Ports (Inst, Data_W, Size);
               if Data_W /= 0 then
                  Info_Msg_Synth
                    (+Inst, "found ROM %n, width: %v bits, depth: %v",
                     (1 => +Inst, 2 => +Data_W, 3 => +Size));
                  Replace_ROM_Memory (Ctxt, Inst, Data_W);
               end if;
            else
               Reduce_Extract_Muxes (Inst);
               if Validate_RAM_Multiple (Inst) then
                  Convert_To_Memory (Ctxt, Inst);
               end if;
            end if;
         end;
      end loop;

      Instance_Tables.Free (Mems);
   end Extract_Memories;

   --  Return True iff O is to MUX and any number of Dyn_Extract (possibly
   --  through mux2).
   function One_Write_Connection (O : Net; Mux : Instance) return Boolean
   is
      Inp : Input;
      Parent : Instance;
   begin
      Inp := Get_First_Sink (O);
      while Inp /= No_Input loop
         Parent := Get_Input_Parent (Inp);
         case Get_Id (Parent) is
            when Id_Dyn_Extract =>
               null;
            when Id_Mux2 =>
               if Parent /= Mux then
                  --  Can be a mux for a dyn_extract.
                  declare
                     In2 : Input;
                  begin
                     loop
                        In2 := Get_First_Sink (Get_Output (Parent, 0));
                        if In2 = No_Input
                          or else Get_Next_Sink (In2) /= No_Input
                        then
                           --  Drives more than one gate.
                           return False;
                        end if;
                        Parent := Get_Input_Parent (In2);
                        case Get_Id (Parent) is
                           when Id_Dyn_Extract =>
                              exit;
                           when Id_Mux2 =>
                              null;
                           when others =>
                              return False;
                        end case;
                     end loop;
                  end;
               end if;
            when others =>
               return False;
         end case;
         Inp := Get_Next_Sink (Inp);
      end loop;
      return True;
   end One_Write_Connection;

   procedure Reduce_Muxes_Mux2 (Ctxt : Context_Acc;
                                Clk  : Net;
                                Psel : Net;
                                Head : in out Instance;
                                Tail : out Instance);

   --  Remove the mux2 MUX (by adding enable to dyn_insert).
   --  Return the new head.
   procedure Reduce_Muxes (Ctxt : Context_Acc;
                           Clk : Net;
                           Sel : Net;
                           Head_In : Net;
                           Tail_In : Net;
                           Head_Out : out Instance;
                           Tail_Out : out Instance)
   is
      Inst : Instance;
      N : Net;
   begin
      --  Reduce Drv until Src.
      --  Transform dyn_insert to dyn_insert_en by adding SEL, or simply add
      --  SEL to existing dyn_insert_en.
      --  RES is the head of the result chain.
      N := Head_In;
      Head_Out := No_Instance;
      while N /= Tail_In loop
         Inst := Get_Net_Parent (N);
         case Get_Id (Inst) is
            when Id_Mux2 =>
               --  Recurse on the mux.
               Reduce_Muxes_Mux2 (Ctxt, Clk, Sel, Inst, Tail_Out);
            when Id_Dyn_Insert =>
               --  Transform dyn_insert to dyn_insert_en.
               declare
                  En : Net;
               begin
                  if Sel /= No_Net then
                     En := Build_Dyadic (Ctxt, Id_And, Clk, Sel);
                     Copy_Location (En, Sel);
                  else
                     En := Clk;
                  end if;
                  Tail_Out := Add_Enable_To_Dyn_Insert (Ctxt, Inst, En);
                  Inst := Tail_Out;
               end;
            when Id_Dyn_Insert_En =>
               --  Simply add SEL to the enable input.
               declare
                  En_Inp : constant Input := Get_Input (Inst, 3);
                  En     : Net;
               begin
                  En := Get_Driver (En_Inp);
                  Disconnect (En_Inp);
                  if Sel /= No_Net then
                     En := Build_Dyadic (Ctxt, Id_And, En, Sel);
                     Copy_Location (En, Sel);
                  end if;
                  En := Build_Dyadic (Ctxt, Id_And, Clk, En);
                  Copy_Location (En, Inst);
                  Connect (En_Inp, En);
               end;
               Tail_Out := Inst;
            when Id_Signal
              | Id_Isignal =>
               pragma Assert (Tail_In = No_Net);
               Tail_Out := Inst;
               exit;
            when others =>
               raise Internal_Error;
         end case;
         --  If this is the head, keep it.
         if Head_Out = No_Instance then
            Head_Out := Inst;
         end if;
         --  Continue the walk with the next element.
         N := Get_Input_Net (Tail_Out, 0);
      end loop;

      --  For memories described by a single process like this:
      --      if wen then
      --        mem (addr) := din;
      --      end if;
      --      if rden then
      --        dout := mem (addr);
      --      end if;
      --  the writer has just been reduced, but the reader can also be reduced.
      --                                         _
      --             _                          / |0-----------------\
      --            / |1-- dyn_extract ---+----|  |                  |
      --   dout ---|  |                   |     \_|1--- dyn_insert --+--- mem
      --            \_|0-- dout           |      |                   |
      --             |                    |     wen                  |
      --            rden            _     |                          |
      --                           / |1---/                          |
      --   mem ----- isignal -----|  |                               |
      --                           \_|0------------------------------/
      --                            |
      --                           +clk
      --  Was just reduced to:
      --             _
      --            / |1-- dyn_extract ---+--- dyn_insert_en --+--- mem
      --   dout ---|  |                   |                    |
      --            \_|0-- dout           |                    |
      --             |                    |                    |
      --            rden            _     |                    |
      --                           / |1---/                    |
      --   mem ----- isignal -----|  |                         |
      --                           \_|0------------------------/
      --                            |
      --                           +clk

      --  Note: Previously, `+clk` and `wen` were fused to the same mux (as an
      --  optimization), requiring extraction.  Now the optimization is not
      --  performed when a wire is read, thus simplifying the reduction here.
   end Reduce_Muxes;

   --  Remove the mux2 HEAD (by adding enable to dyn_insert).
   --  Return the new head.
   procedure Reduce_Muxes_Mux2 (Ctxt : Context_Acc;
                                Clk : Net;
                                Psel : Net;
                                Head : in out Instance;
                                Tail : out Instance)
   is
      Mux : constant Instance := Head;
      Muxout : constant Net := Get_Output (Mux, 0);
      Sel_Inp : constant Input := Get_Input (Mux, 0);
      In0 : constant Input := Get_Input (Mux, 1);
      In1 : constant Input := Get_Input (Mux, 2);
      Sel : Net;
      Drv0 : Net;
      Drv1 : Net;
      Drv : Net;
      Src : Net;
      Res : Instance;
   begin
      Drv0 := Get_Driver (In0);
      Drv1 := Get_Driver (In1);
      Sel := Get_Driver (Sel_Inp);

      --  An enable mux has this shape:
      --            _
      --           / |----- dyn_insert ----+----+
      --    out --|  |                     |    +---- inp
      --           \_|---------------------/
      --
      --  The dyn_insert can be on one input or the other of the mux.
      --  The important point is that the output of the dyn_insert is connected
      --  only to the mux, while the other mux input is connected to two nodes.
      --
      --  There can be several dyn_inserts in a raw, like this:
      --            _
      --           / |-- dyn_insert --- dyn_insert ---+----+
      --    out --|  |                                |    +---- inp
      --           \_|--------------------------------/
      --
      --  Or even nested muxes:
      --                 _
      --           _    / |----- dyn_insert ----+----+
      --          / |--|  |                     |    |
      --   out --|  |   \_|---------------------/    |
      --          \_|--------------------------------+----- inp
      if One_Write_Connection (Drv0, Mux)
        and then not Has_One_Connection (Drv1)
      then
         Disconnect (In0);
         Disconnect (In1);
         Disconnect (Sel_Inp);
         Drv := Drv0;
         Src := Drv1;
         Sel := Build_Monadic (Ctxt, Id_Not, Sel);
         Copy_Location (Sel, Mux);
      elsif Has_One_Connection (Drv1) and then not Has_One_Connection (Drv0)
      then
         Disconnect (In0);
         Disconnect (In1);
         Disconnect (Sel_Inp);
         Drv := Drv1;
         Src := Drv0;
      else
         --  Not an enable mux.
         raise Internal_Error;
      end if;

      if Psel /= No_Net then
         Sel := Build_Dyadic (Ctxt, Id_And, Psel, Sel);
         Copy_Location (Sel, Psel);
      end if;

      --  Reduce Drv until Src.
      --  Transform dyn_insert to dyn_insert_en by adding SEL, or simply add
      --  SEL to existing dyn_insert_en.
      --  RES is the head of the result chain.
      Reduce_Muxes (Ctxt, Clk, Sel, Drv, Src, Res, Tail);

      Redirect_Inputs (Muxout, Get_Output (Res, 0));
      Remove_Instance (Mux);

      Head := Res;
   end Reduce_Muxes_Mux2;

   function Infere_RAM
     (Ctxt : Context_Acc; Val : Net; Tail : Net; Clk : Net; En : Net)
      return Net
   is
      pragma Assert (Clk /= No_Net);
      --  pragma Assert (not Is_Connected (Val));
      New_Tail : Instance;
      Res : Instance;
   begin
      --  From VAL, move all the muxes to the dyn_insert.  The dyn_insert may
      --  be transformed to dyn_insert_en.
      --  At the end, the loop is linear and without muxes.
      --  Return the new head.
      Reduce_Muxes (Ctxt, Clk, En, Val, Tail, Res, New_Tail);
      return Get_Output (Res, 0);
   end Infere_RAM;

   function Can_Infere_RAM_Mux2 (Mux : Instance) return Instance
   is
      Drv0 : Net;
      Drv1 : Net;
      Drv : Net;
      Src : Net;
      Inst : Instance;
   begin
      --  An enable mux has this shape:
      --            _
      --           / |----- dyn_insert ----+----+
      --    out --|  |                     |    +---- inp
      --           \_|---------------------/
      --
      --  The dyn_insert can be on one input or the other of the mux.
      --  The important point is that the output of the dyn_insert is connected
      --  only to the mux, while the other mux input is connected to two nodes.
      --
      --  There can be several dyn_inserts in a raw, like this:
      --            _
      --           / |-- dyn_insert --- dyn_insert ---+----+
      --    out --|  |                                |    +---- inp
      --           \_|--------------------------------/
      --
      --  Or even nested muxes:
      --                 _
      --           _    / |----- dyn_insert ----+----+
      --          / |--|  |                     |    |
      --   out --|  |   \_|---------------------/    |
      --          \_|--------------------------------+----- inp
      --
      --  But there can be dyn_extract almost anywhere.
      Drv0 := Get_Input_Net (Mux, 1);
      Drv1 := Get_Input_Net (Mux, 2);
      if One_Write_Connection (Drv0, Mux)
        and then not One_Write_Connection (Drv1, Mux)
      then
         Drv := Drv0;
         Src := Drv1;
      elsif One_Write_Connection (Drv1, Mux)
        and then not One_Write_Connection (Drv0, Mux)
      then
         Drv := Drv1;
         Src := Drv0;
      else
         --  Not an enable mux.
         return No_Instance;
      end if;

      --  Walk Drv until Src.
      while Drv /= Src loop
         Inst := Get_Net_Parent (Drv);
         case Get_Id (Inst) is
            when Id_Mux2 =>
               --  Recurse on the mux.
               Inst := Can_Infere_RAM_Mux2 (Inst);
               if Inst = No_Instance then
                  return No_Instance;
               end if;
               --  But continue with the result: still need to add the SEL.
               Drv := Get_Output (Inst, 0);
            when Id_Dyn_Insert
               | Id_Dyn_Insert_En =>
               --  Continue the walk with the next element.
               Drv := Get_Input_Net (Inst, 0);
            when others =>
               return No_Instance;
         end case;
      end loop;

      return Get_Net_Parent (Src);
   end Can_Infere_RAM_Mux2;

   function Can_Infere_RAM (Val : Net; Prev_Val : Net) return Boolean
   is
      Inst : Instance;
   begin
      Inst := Get_Net_Parent (Val);

      --  Walk until the reaching Prev_Val.
      loop
         case Get_Id (Inst) is
            when Id_Mux2 =>
               --  Reduce the mux.
               Inst := Can_Infere_RAM_Mux2 (Inst);
               if Inst = No_Instance then
                  return False;
               end if;
            when Id_Dyn_Insert
              | Id_Dyn_Insert_En =>
               --  Skip the dyn_insert.
               Inst := Get_Input_Instance (Inst, 0);
            when Id_Signal
              | Id_Isignal =>
               return Get_Output (Inst, 0) = Prev_Val;
            when others =>
               return False;
         end case;
      end loop;
   end Can_Infere_RAM;
end Netlists.Memories;
