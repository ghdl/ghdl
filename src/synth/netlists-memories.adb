--  Extract memories.
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

with Dyn_Tables;
with Errorout; use Errorout;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Errors; use Netlists.Errors;
with Netlists.Concats;
with Netlists.Folds; use Netlists.Folds;

with Synth.Errors; use Synth.Errors;

package body Netlists.Memories is
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
      begin
         Inp := Get_First_Sink (Orig_Net);
         while Inp /= No_Input loop
            Extr_Inst := Get_Input_Parent (Inp);
            case Get_Id (Extr_Inst) is
               when Id_Dyn_Extract =>
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
                  if Data_W = 0 then
                     Data_W := W;
                  elsif Data_W /= W then
                     Info_Msg_Synth
                       (+Extr_Inst, "read from memory %n with different size",
                        (1 => +Orig));
                     Data_W := 0;
                     return;
                  end if;
               when Id_Dyn_Insert
                 | Id_Dyn_Insert_En
                 |  Id_Mux2 =>
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

      Size := Get_Width (Orig_Net) / Data_W;
   end Check_Memory_Read_Ports;

   procedure Check_Memory_Write_Ports (Orig : Instance;
                                       Data_W : out Width;
                                       Size : out Width)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Orig_Loc : constant Location_Type := Get_Location (Orig);
      Mem_Sz : Width;
      W : Width;
      Dff : Net;
      Dff_Inst : Instance;
      Data : Net;
      Data_Inst : Instance;
      Data_Inp : Input;
   begin
      --  By default, error.
      Data_W := 0;
      Size := 0;

      --  The memory must come from signal/isignal.
      case Get_Id (Orig) is
         when Id_Isignal
           | Id_Signal =>
            null;
         when others =>
            raise Internal_Error;
      end case;

      --  The driver of the signal must be a dff/idff.
      Dff := Get_Input_Net (Orig, 0);
      Dff_Inst := Get_Net_Parent (Dff);

      case Get_Id (Dff_Inst) is
         when Id_Idff
           | Id_Dff =>
            Mem_Sz := Get_Width (Dff);
         when others =>
            Info_Msg_Synth
              (Orig_Loc,
               "write to memory %n is not synchronous", (1 => +Orig));
            return;
      end case;

      --  Check write ports.
      Data_Inp := Get_Input (Dff_Inst, 1);
      Data := Get_Driver (Data_Inp);
      loop
         Data_Inst := Get_Net_Parent (Data);

         --  Skip muxes (that's enables).
         while Get_Id (Data_Inst) = Id_Mux2 loop
            declare
               I0_Inp : constant Input := Get_Input (Data_Inst, 1);
               I1_Inp : constant Input := Get_Input (Data_Inst, 2);
            begin
               if Get_Driver (I0_Inp) = Orig_Net then
                  Data := Get_Driver (I1_Inp);
               elsif Get_Driver (I1_Inp) = Orig_Net then
                  Data := Get_Driver (I0_Inp);
               else
                  Info_Msg_Synth
                    (+Data_Inst, "write to a non-memory %n", (1 => +Orig));
                  Data_W := 0;
                  return;
               end if;
               Data_Inst := Get_Net_Parent (Data);
            end;
         end loop;

         if Get_Id (Data_Inst) in Dyn_Insert_Module_Id then
            --  Write port.
            W := Get_Width (Get_Input_Net (Data_Inst, 1));
            if Data_W = 0 then
               Data_W := W;
            elsif Data_W /= W then
               --  Different size.
               Info_Msg_Synth
                 (+Data_Inst, "write to memory %n with different size",
                  (1 => +Orig));
               Data_W := 0;
               return;
            end if;

            Data := Get_Input_Net (Data_Inst, 0);
         elsif Data = Orig_Net then
            exit;
         else
            Info_Msg_Synth
              (+Data_Inst, "full write to memory %n", (1 => +Orig));
            Data_W := 0;
            return;
         end if;
      end loop;

      Size := Mem_Sz / Data_W;
   end Check_Memory_Write_Ports;

   procedure Check_RAM_Ports (Orig : Instance;
                              Data_W : out Width;
                              Size : out Width)
   is
      Write_Dw : Width;
      Write_Size : Width;

      Read_Dw : Width;
      Read_Size : Width;
   begin
      --  By default, error.
      Data_W := 0;
      Size := 0;

      Check_Memory_Write_Ports (Orig, Write_Dw, Write_Size);
      if Write_Dw = 0 then
         return;
      end if;

      Check_Memory_Read_Ports (Orig, Read_Dw, Read_Size);
      if Read_Dw = 0 then
         return;
      end if;
      if Read_Dw /= Write_Dw then
         Info_Msg_Synth (+Orig, "different read/write width for memory %n",
                         (1 => +Orig));
         return;
      end if;
      if Read_Size /= Write_Size then
         Info_Msg_Synth (+Orig, "different read/write size for memory %n",
                         (1 => +Orig));
         return;
      end if;

      Data_W := Write_Dw;
      Size := Write_Size;
   end Check_RAM_Ports;

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
               if Get_Id (Get_Net_Parent (Get_Input_Net (Inst, 1)))
                 /= Id_Memidx
               then
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
   --  MEM is the memory (used to get its size).
   --  ADDR is the address net with memidx/addidx gates.
   --  VAL_WD is the width of the data port.
   procedure Convert_Memidx (Ctxt : Context_Acc;
                             Mem : Instance;
                             Addr : in out Net;
                             Val_Wd : Width)
   is
      --  Number of memidx.
      Nbr_Idx : constant Positive := Count_Memidx (Addr);

      --  Size of the memory (in bits).
      Mem_Size : constant Uns32 := Get_Width (Get_Output (Mem, 0));
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
                  Inst2 := Get_Net_Parent (Get_Input_Net (Inst, 0));
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
         begin
            --  Check max
            pragma Assert (Max /= 0);
            if (Max + 1) * Step /= Last_Size then
               raise Internal_Error;
            end if;

            --  Check addr width.
            if Addr_W = 0 then
               raise Internal_Error;
            end if;
            if Addr_W > Max_W then
               --  Need to truncate.
               Sub_Addr1 := Build2_Trunc
                 (Ctxt, Id_Utrunc, Sub_Addr, Max_W, No_Location);
            else
               Sub_Addr1 := Sub_Addr;
            end if;
            Indexes (I).Addr := Sub_Addr1;

            if I = Indexes'Last then
               if Step /= Val_Wd then
                  raise Internal_Error;
               end if;
            end if;
            Last_Size := Step;
         end;
      end loop;

      --  Lower (just concat addresses).
      declare
         use Netlists.Concats;
         Concat : Concat_Type;
         Inp : Input;
      begin
         for I in Indexes'Range loop
            Inp := Get_Input (Indexes (I).Inst, 0);
            Disconnect (Inp);
            Append (Concat, Indexes (I).Addr);
         end loop;

         Build (Ctxt, Concat, Low_Addr);
      end;

      --  Free addidx and memidx.
      declare
         N : Net;
         Inst : Instance;
         Inst2 : Instance;
      begin
         N := Addr;
         loop
            Inst := Get_Net_Parent (N);
            case Get_Id (Inst) is
               when Id_Memidx =>
                  Remove_Instance (Inst);
                  exit;
               when Id_Addidx =>
                  Inst2 := Get_Net_Parent (Get_Input_Net (Inst, 0));
                  if Get_Id (Inst2) /= Id_Memidx then
                     raise Internal_Error;
                  end if;
                  Remove_Instance (Inst2);
                  N := Get_Input_Net (Inst, 1);
                  Remove_Instance (Inst);
               when others =>
                  raise Internal_Error;
            end case;
         end loop;
      end;

      Addr := Low_Addr;
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
         return Get_Input_Net (Mux_Inst, 2) = Dff_Out;
      else
         return Get_Input_Net (Mux_Inst, 1) = Dff_Out;
      end if;
   end Is_Enable_Dff;

   --  Create a mem_rd/mem_rd_sync from a dyn_extract gate.
   --  LAST is the last memory port on the chain.
   --  ADDR is the address (from the dyn_extract).
   --  VAL is the output of the dyn_extract.
   --
   --  Infere a synchronous read if the dyn_extract is connected to a dff.
   function Create_Read_Port
     (Ctxt : Context_Acc; Last : Net; Addr : Net; Val : Net) return Instance
   is
      W : constant Width := Get_Width (Val);
      Res : Instance;
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
               Clk : Net;
               En : Net;
            begin
               Clk_Inp := Get_Input (Iinst, 0);
               Clk := Get_Driver (Clk_Inp);
               Disconnect (Clk_Inp);
               En := Build_Const_UB32 (Ctxt, 1, 1);
               Disconnect (Inp);
               Res := Build_Mem_Rd_Sync (Ctxt, Last, Addr, Clk, En, W);
               Redirect_Inputs (Get_Output (Iinst, 0), Get_Output (Res, 1));
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
               end if;
               Disconnect (Mux_En_Inp);
               Disconnect (Mux_I0_Inp);
               Disconnect (Mux_I1_Inp);
               Disconnect (Dff_Din);
               Disconnect (Clk_Inp);
               Remove_Instance (Iinst);
               Res := Build_Mem_Rd_Sync (Ctxt, Last, Addr, Clk, En, W);
               Redirect_Inputs (Dff_Out, Get_Output (Res, 1));
               Remove_Instance (Dff_Inst);
               return Res;
            end;
         end if;
      end if;

      --  Replace Dyn_Extract with mem_rd.
      Res := Build_Mem_Rd (Ctxt, Last, Addr, W);

      Redirect_Inputs (Val, Get_Output (Res, 1));

      return Res;
   end Create_Read_Port;

   --  MEM_LINK is the link from the last port (or from the memory).
   procedure Replace_Read_Ports
     (Ctxt : Context_Acc; Orig : Instance; Mem_Link : Net)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Last : Net;
      Inp : Input;
      Next_Inp : Input;
      Extr_Inst : Instance;
      Addr_Inp : Input;
      Addr : Net;
      Val : Net;
      Val_W : Width;
      Port_Inst : Instance;
   begin
      Last := Mem_Link;

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
               Val_W := Get_Width (Val);
               Convert_Memidx (Ctxt, Orig, Addr, Val_W);

               --  Replace Dyn_Extract with mem_rd.
               Port_Inst := Create_Read_Port (Ctxt, Last, Addr, Val);

               Remove_Instance (Extr_Inst);

               Last := Get_Output (Port_Inst, 0);
            when others =>
               raise Internal_Error;
         end case;
         Inp := Next_Inp;
      end loop;
   end Replace_Read_Ports;

   --  ORIG (the memory) must be signal/isignal.
   procedure Replace_RAM_Memory (Ctxt : Context_Acc; Orig : Instance)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Dff : Net;
      Dff_Inst : Instance;
      En : Net;
      Data : Net;
      Data_Inst : Instance;
      Last : Net;
      Data_Inp : Input;
   begin
      --  The driver of the signal must be a dff/idff.
      Dff := Get_Input_Net (Orig, 0);
      Dff_Inst := Get_Net_Parent (Dff);

      case Get_Id (Dff_Inst) is
         when Id_Idff =>
            --  Disconnect the init input to isignal.
            Disconnect (Get_Input (Orig, 1));
            declare
               Init_Inp : constant Input := Get_Input (Dff_Inst, 2);
               Init_Net : constant Net := Get_Driver (Init_Inp);
            begin
               Disconnect (Init_Inp);
               Last := Build_Memory_Init (Ctxt, Get_Width (Dff), Init_Net);
            end;
         when Id_Dff =>
            Last := Build_Memory (Ctxt, Get_Width (Dff));
         when others =>
            Info_Msg_Synth
              (+Orig, "write to memory %n is not synchronous", (1 => +Orig));
            return;
      end case;

      --  TODO: set name, set location.

      --  Extract enable from the value.  Follow muxes (and free them).
      Data_Inp := Get_Input (Dff_Inst, 1);
      Data := Get_Driver (Data_Inp);
      Disconnect (Data_Inp);
      En := No_Net;
      loop
         Data_Inst := Get_Net_Parent (Data);

         --  Stop when the dyn_insert is reached.
         exit when Get_Id (Data_Inst) in Dyn_Insert_Module_Id;

         if Get_Id (Data_Inst) /= Id_Mux2 then
            raise Internal_Error;
         end if;
         --  Should have only one connection to the parent (removed above).
         if Is_Connected (Data) then
            raise Internal_Error;
         end if;

         declare
            Sel_Inp : constant Input := Get_Input (Data_Inst, 0);
            I0_Inp : constant Input := Get_Input (Data_Inst, 1);
            I1_Inp : constant Input := Get_Input (Data_Inst, 2);
            Sel : constant Net := Get_Driver (Sel_Inp);
            En1_Val : Net;
            En1_Sel : Net;
         begin
            if Get_Driver (I0_Inp) = Orig_Net then
               En1_Val := Get_Driver (I1_Inp);
               En1_Sel := Sel;
            elsif Get_Driver (I1_Inp) = Orig_Net then
               En1_Val := Get_Driver (I0_Inp);
               En1_Sel := Build_Monadic (Ctxt, Id_Not, Sel);
            else
               raise Internal_Error;
            end if;

            --  Disconnect and remove the mux.
            Disconnect (Sel_Inp);
            Disconnect (I0_Inp);
            Disconnect (I1_Inp);
            Remove_Instance (Data_Inst);

            --  AND the enable.
            if En = No_Net then
               En := En1_Sel;
            else
               En := Build_Dyadic (Ctxt, Id_And, En, En1_Sel);
            end if;

            Data := En1_Val;
         end;
      end loop;

      if Get_Id (Data_Inst) = Id_Dyn_Insert_En then
         declare
            Ins_En_Inp : constant Input := Get_Input (Data_Inst, 3);
            Ins_En : constant Net := Get_Driver (Ins_En_Inp);
         begin
            if En = No_Net then
               En := Ins_En;
            else
               En := Build_Dyadic (Ctxt, Id_And, En, Ins_En);
            end if;
            Disconnect (Ins_En_Inp);
         end;
      end if;

      --  Dummy enable if enable is not present.
      if En = No_Net then
         En := Build_Const_UB32 (Ctxt, 1, 1);
      end if;

      --  DATA is now the dyn_insert.
      --  Convert memidx/addidx, check size and step.
      declare
         Data_Inp : constant Input := Get_Input (Data_Inst, 1);
         Addr_Inp : constant Input := Get_Input (Data_Inst, 2);
         Clk_Inp : Input;
         Clk : Net;
         Addr : Net;
         Val : Net;
         Port_Inst : Instance;
      begin
         Addr := Get_Driver (Addr_Inp);
         Disconnect (Addr_Inp);
         Val := Get_Input_Net (Data_Inst, 1);
         if Get_Input_Net (Data_Inst, 0) /= Orig_Net then
            raise Internal_Error;
         end if;
         Convert_Memidx (Ctxt, Orig, Addr, Get_Width (Val));

         --  Replace dyn_insert with mem_wr_sync
         Clk_Inp := Get_Input (Dff_Inst, 0);
         Clk := Get_Driver (Clk_Inp);
         Disconnect (Clk_Inp);
         Data := Get_Driver (Data_Inp);
         Disconnect (Data_Inp);
         Port_Inst := Build_Mem_Wr_Sync (Ctxt, Last, Addr, Clk, En, Data);
         Last := Get_Output (Port_Inst, 0);

         --  Free the dff/idff.
         Disconnect (Get_Input (Orig, 0));
         --  Remove_Instance (Dff_Inst);

         --  Free the dyn_extract
         Disconnect (Get_Input (Data_Inst, 0));
         Remove_Instance (Data_Inst);
      end;

      Remove_Instance (Dff_Inst);

      Replace_Read_Ports (Ctxt, Orig, Last);

      Remove_Instance (Orig);
   end Replace_RAM_Memory;

   --  ORIG (the memory) must be Const.
   procedure Replace_ROM_Memory (Ctxt : Context_Acc; Orig : Instance)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Last : Net;
   begin
      Last := Build_Memory_Init (Ctxt, Get_Width (Orig_Net), Orig_Net);

      Replace_Read_Ports (Ctxt, Orig, Last);
   end Replace_ROM_Memory;

   --  Try to reach Id_Signal/Id_Isignal (TODO: Id_Output) from dyn_insert
   --  gate FIRST_INST.  Can only walk through dyn_insert and muxes.
   --  Return the memory if found.
   function Walk_From_Insert (First_Inst : Instance) return Instance
   is
      Inst : Instance;
      Next_Inst : Instance;
      Last : Instance;
      O : Net;
      Inp : Input;
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
            when Id_Dff
              | Id_Idff =>
               O := Get_Output (Inst, 0);
            when others =>
               Info_Msg_Synth
                 (+Last, "gate %i cannot be part of a memory", (1 => +Inst));
               return No_Instance;
         end case;

         Next_Inst := No_Instance;
         Inp := Get_First_Sink (O);
         while Inp /= No_Input loop
            declare
               Pinst : constant Instance := Get_Input_Parent (Inp);
            begin
               if Get_Id (Pinst) /= Id_Dyn_Extract then
                  if Next_Inst /= No_Instance then
                     Info_Msg_Synth
                       (+Last, "gate %i drives several gates", (1 => +Inst));
                     return No_Instance;
                  end if;
                  Next_Inst := Pinst;
               end if;
            end;
            Inp := Get_Next_Sink (Inp);
         end loop;
         Inst := Next_Inst;
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
               Inst := Get_Net_Parent (Get_Input_Net (Inst, 0));
            when Id_Isignal
              | Id_Signal
              | Id_Const_Bit =>
               return Inst;
            when others =>
               Info_Msg_Synth
                 (+Last, "gate %i cannot be part of a memory", (1 => +Last));
               return No_Instance;
         end case;
      end loop;
   end Walk_From_Extract;

   --  FIRST_INST is a dyn_instance gate.
   function Maybe_Add_Enable_To_Dyn_Insert
     (Ctxt : Context_Acc; First_Inst : Instance) return Instance
   is
      --  Maybe transform as Dyn_Insert_En.
      O : constant Net := Get_Output (First_Inst, 0);
      O_Inp : constant Input := Get_First_Sink (O);
      Mux_Inst : constant Instance := Get_Input_Parent (O_Inp);
      Mem : constant Net := Get_Input_Net (First_Inst, 0);
      Mux_Sel, Mux_I0, Mux_I1 : Input;
      Sel : Net;
      New_Inst : Instance;
      New_Net : Net;
   begin
      if Get_Id (Mux_Inst) /= Id_Mux2 then
         --  Not followed by mux2
         return First_Inst;
      end if;

      if Get_Next_Sink (O_Inp) /= No_Input then
         --  Multiple outputs after the dyn_insert.
         return First_Inst;
      end if;

      Mux_Sel := Get_Input (Mux_Inst, 0);
      Mux_I0 := Get_Input (Mux_Inst, 1);
      Mux_I1 := Get_Input (Mux_Inst, 2);
      Sel := Get_Driver (Mux_Sel);
      if Get_Driver (Mux_I0) = Mem then
         pragma Assert (Get_Driver (Mux_I1) = O);
         null;
      elsif Get_Driver (Mux_I1) = Mem then
         pragma Assert (Get_Driver (Mux_I0) = O);
         Sel := Build_Monadic (Ctxt, Id_Not, Sel);
      else
         --  Does not by-pass the dyn_insert.
         return First_Inst;
      end if;

      --  Replace
      declare
         In_V : constant Input := Get_Input (First_Inst, 1);
         In_Idx : constant Input := Get_Input (First_Inst, 2);
         Off : constant Uns32 := Get_Param_Uns32 (First_Inst, 0);
      begin
         New_Net := Build_Dyn_Insert_En
           (Ctxt, Mem, Get_Driver (In_V), Get_Driver (In_Idx), Sel, Off);
         New_Inst := Get_Net_Parent (New_Net);
         Set_Location (New_Inst, Get_Location (First_Inst));

         Disconnect (Get_Input (First_Inst, 0));
         Disconnect (In_V);
         Disconnect (In_Idx);
         Disconnect (Mux_Sel);
         Disconnect (Mux_I0);
         Disconnect (Mux_I1);
         Redirect_Inputs (Get_Output (Mux_Inst, 0), New_Net);

         Remove_Instance (Mux_Inst);
         Remove_Instance (First_Inst);
      end;

      return New_Inst;
   end Maybe_Add_Enable_To_Dyn_Insert;

   procedure Append_If_Not_Marked (Els : in out Instance_Tables.Instance;
                                   Inst : Instance) is
   begin
      if not Get_Mark_Flag (Inst) then
         Set_Mark_Flag (Inst, True);
         Instance_Tables.Append (Els, Inst);
      end if;
   end Append_If_Not_Marked;

   procedure Append_If_Not_Marked (Els : in out Instance_Tables.Instance;
                                   N : Net) is
   begin
      Append_If_Not_Marked (Els, Get_Net_Parent (N));
   end Append_If_Not_Marked;

   --  Append the gate which owns INP if not marked.
   procedure Append_If_Not_Marked (Els : in out Instance_Tables.Instance;
                                   Inp : Input) is
   begin
      Append_If_Not_Marked (Els, Get_Input_Parent (Inp));
   end Append_If_Not_Marked;

   --  Append all non-marked gates that are driven by net N.
   procedure Append_Driven_If_Not_Marked
     (Els : in out Instance_Tables.Instance; N : Net)
   is
      Inp : Input;
   begin
      Inp := Get_First_Sink (N);
      while Inp /= No_Input loop
         Append_If_Not_Marked (Els, Inp);
         Inp := Get_Next_Sink (Inp);
      end loop;
   end Append_Driven_If_Not_Marked;

   procedure Unmark_Table (Els : Instance_Tables.Instance)
   is
      Inst : Instance;
   begin
      for I in Instance_Tables.First .. Instance_Tables.Last (Els) loop
         Inst := Els.Table (I);
         Set_Mark_Flag (Inst, False);
      end loop;
   end Unmark_Table;

   --  INST is signal/isignal.  Check that the logical loop is composed of
   --  dyn + muxes.
   function Validate_RAM0 (Sig : Instance) return Boolean
   is
      Els : Instance_Tables.Instance;
      Inst : Instance;
      Last : Nat32;
      Ok : Boolean;
   begin
      Instance_Tables.Init (Els, 16);
      Set_Mark_Flag (Sig, True);
      Instance_Tables.Append (Els, Sig);

      Ok := True;
      Last := 0;
      while Instance_Tables.Last (Els) > Last loop
         Last := Last + 1;
         Inst := Els.Table (Last);
         case Get_Id (Inst) is
            when Id_Mux2 =>
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 1));
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 2));
               Append_Driven_If_Not_Marked (Els, Get_Output (Inst, 0));
            when Id_Isignal
              | Id_Signal =>
               Append_Driven_If_Not_Marked (Els, Get_Output (Inst, 0));
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 0));
            when Id_Dyn_Insert
              | Id_Dyn_Insert_En =>
               Append_Driven_If_Not_Marked (Els, Get_Output (Inst, 0));
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 0));
            when Id_Dyn_Extract =>
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 0));
            when Id_Dff
              | Id_Idff =>
               Append_Driven_If_Not_Marked (Els, Get_Output (Inst, 0));
               Append_If_Not_Marked (Els, Get_Input_Net (Inst, 1));
            when others =>
               Info_Msg_Synth
                 (+Sig, "gate %i cannot be part of a memory", (1 => +Inst));
               Ok := False;
               exit;
         end case;
      end loop;

      --  Clear all mark flags.
      Unmark_Table (Els);

      --  Free.
      Instance_Tables.Free (Els);

      return Ok;
   end Validate_RAM0;

   pragma Unreferenced (Validate_RAM0);

   package Input_Tables is new Dyn_Tables
     (Table_Component_Type => Input,
      Table_Index_Type => Int32,
      Table_Low_Bound => 1);

   type Validate_RAM_Result is
     (
      --  The last gate is signal/isignal output.
      --  The end of the structure was reached.
      Validate_RAM_Signal,

      --  An element was recognized.
      Validate_RAM_OK,

      --  Error in the netlist: invalid gate.
      Validate_RAM_Error,

      --  Error: not a RAM.
      Validate_RAM_None,

      --  The input is a mux.
      Validate_RAM_Mux
     );

   type Validate_RAM_Type (Res : Validate_RAM_Result := Validate_RAM_Signal) is
      record
      case Res is
         when Validate_RAM_Signal =>
            Sig : Instance;
         when Validate_RAM_OK =>
            Outp : Instance;
         when Validate_RAM_Error =>
            Err : Instance;
         when Validate_RAM_Mux =>
            Mux : Instance;
         when Validate_RAM_None =>
            null;
      end case;
   end record;

   --  Return O for input FIRST, or No_Net in case of error.
   --  - O := dyn_insert(I)
   --  - O := mux(sel, el(I), I)
   --  - O := mux(sel, el1(I), el2(I))
   --  - O := el1(el2(I))
   function Validate_RAM_Element (First : Net) return Validate_RAM_Type
   is
      Nbr_Muxes : Nat32;
      Nbr_Inserts : Nat32;
      Nbr_Sig : Nat32;
      Inp : Input;
      Inst : Instance;
      Res : Instance;
      Last_Mux : Instance;
   begin
      --  Count number of muxes, inserts, signals.
      Nbr_Muxes := 0;
      Nbr_Inserts := 0;
      Nbr_Sig := 0;
      Last_Mux := No_Instance;
      Res := No_Instance;
      Inp := Get_First_Sink (First);
      while Inp /= No_Input loop
         Inst := Get_Input_Parent (Inp);
         case Get_Id (Inst) is
            when Id_Mux2 =>
               Nbr_Muxes := Nbr_Muxes + 1;
               Last_Mux := Inst;
            when Id_Dyn_Insert =>
               Res := Inst;
               Nbr_Inserts := Nbr_Inserts + 1;
            when Id_Dyn_Extract =>
               null;
            when Id_Signal
              | Id_Isignal =>
               Res := Inst;
               Nbr_Sig := Nbr_Sig + 1;
            when others =>
               --  The whole content of the RAM is directly used.
               return Validate_RAM_Type'
                 (Res => Validate_RAM_Error, Err => Inst);
         end case;
         Inp := Get_Next_Sink (Inp);
      end loop;

      if Nbr_Sig /= 0 then
         if Nbr_Sig /= 1 or Nbr_Inserts /= 0 or Nbr_Muxes /= 0 then
            --  Not sure this is even possible.
            raise Internal_Error;
         end if;
         --  So only the signal was reached.
         return Validate_RAM_Type'(Res => Validate_RAM_Signal, Sig => Res);
      end if;

      if Nbr_Inserts = 0 then
         if Nbr_Muxes = 1 then
            return Validate_RAM_Type'
              (Res => Validate_RAM_Mux, Mux => Last_Mux);
         end if;
         --  Invalide shape.
         return Validate_RAM_Type'(Res => Validate_RAM_None);
      end if;

      if Nbr_Muxes = 0 and Nbr_Inserts = 1 then
         --  First case.
         return Validate_RAM_Type'(Res => Validate_RAM_OK, Outp => Res);
      end if;

      --  Multiple muxes and/or multiple inserts.
      declare
         Muxes : Instance_Tables.Table_Type (1 .. Nbr_Muxes + Nbr_Inserts);
         El : Validate_RAM_Type;
         N : Net;
      begin
         --  Fill the tables.
         Nbr_Muxes := 0;
         Inp := Get_First_Sink (First);
         while Inp /= No_Input loop
            Inst := Get_Input_Parent (Inp);
            case Get_Id (Inst) is
               when Id_Mux2 =>
                  Nbr_Muxes := Nbr_Muxes + 1;
                  Muxes (Nbr_Muxes) := Inst;
               when Id_Dyn_Insert =>
                  loop
                     El := Validate_RAM_Element (Get_Output (Inst, 0));
                     case El.Res is
                        when Validate_RAM_Error
                          | Validate_RAM_None =>
                           return El;
                        when Validate_RAM_Mux =>
                           Nbr_Muxes := Nbr_Muxes + 1;
                           Muxes (Nbr_Muxes) := El.Mux;
                           exit;
                        when Validate_RAM_OK =>
                           --  Continue.
                           Inst := El.Outp;
                        when Validate_RAM_Signal =>
                           raise Internal_Error;
                     end case;
                  end loop;
               when others =>
                  null;
            end case;
            Inp := Get_Next_Sink (Inp);
         end loop;
         pragma Assert (Nbr_Muxes = Muxes'Last);

         --  Reduce muxes.
         --  Each mux must appear twice in the list (but maybe not
         --  immediately).
         for I in Muxes'Range loop
            Inst := Muxes (I);
            if Get_Mark_Flag (Inst) then
               loop
                  --  The mux was already marked.  Unmark it.
                  Set_Mark_Flag (Inst, False);
                  Nbr_Muxes := Nbr_Muxes - 1;
                  if Nbr_Muxes = 1 then
                     --  This was the last one.
                     return Validate_RAM_Type'
                       (Res => Validate_RAM_OK, Outp => Inst);
                  end if;
                  N := Get_Output (Inst, 0);
                  if not Has_One_Connection (N) then
                     --  TODO: skip dyn_extract
                     raise Internal_Error;
                  end if;
                  Inst := Get_Input_Parent (Get_First_Sink (N));
                  Muxes (I) := Inst;
                  if not Get_Mark_Flag (Inst) then
                     --  Never seen.
                     Set_Mark_Flag (Inst, True);
                     exit;
                  end if;
               end loop;
            else
               --  Mark it.
               Set_Mark_Flag (Inst, True);
            end if;
         end loop;

         --  This is not a mux tree.  Cleanup and return error.
         for I in Muxes'Range loop
            Inst := Muxes (I);
            if not Get_Mark_Flag (Inst) then
               Set_Mark_Flag (Inst, False);
               Last_Mux := Inst;
            end if;
         end loop;

         return Validate_RAM_Type'
           (Res => Validate_RAM_Error, Err => Inst);
      end;
   end Validate_RAM_Element;

   function Validate_RAM (Sig : Instance) return Boolean
   is
      Res : Validate_RAM_Type;
   begin
      Res := Validate_RAM_Element (Get_Output (Sig, 0));
      case Res.Res is
         when Validate_RAM_Signal =>
            if Res.Sig /= Sig then
               raise Internal_Error;
            end if;
            return True;
         when Validate_RAM_OK =>
            return True;
         when Validate_RAM_Mux =>
            Info_Msg_Synth (+Sig, "RAM is written in whole with mux %n",
                            (1 => +Res.Mux));
            return False;
         when Validate_RAM_None =>
            --  Not a RAM, but without a specific gate (wrong shape).
            return False;
         when Validate_RAM_Error =>
            Info_Msg_Synth (+Sig, "gate %n not allowed in a RAM",
                            (1 => +Res.Err));
            return False;
      end case;
   end Validate_RAM;

   function Find_First_Dyn_Insert (Stack : Input_Tables.Instance;
                                   N : Net) return Int32
   is
      Inp : Input;
      Inst : Instance;
   begin
      for I in reverse Input_Tables.First + 1 .. Input_Tables.Last (Stack) loop
         Inp := Stack.Table (I);
         Inst := Get_Net_Parent (Get_Driver (Inp));
         pragma Assert (Get_Id (Inst) in Dyn_Insert_Module_Id);
         if Get_Input_Net (Inst, 0) = N then
            return I;
         end if;
      end loop;
      return 0;
   end Find_First_Dyn_Insert;

   procedure Add_Enable_To_Dyn_Insert (Ctxt : Context_Acc;
                                       Stack : in out Input_Tables.Instance;
                                       First : Int32;
                                       En : Net)
   is
      Cur_Inp : Input;
      Cur_Inst : Instance;
   begin
      for I in reverse First .. Input_Tables.Last (Stack) loop
         Cur_Inp := Stack.Table (I);
         Cur_Inst := Get_Input_Parent (Cur_Inp);

         case Get_Id (Cur_Inst) is
            when Id_Dyn_Insert_En =>
               declare
                  Inp : constant Input := Get_Input (Cur_Inst, 3);
                  Prev : Net;
               begin
                  Prev := Get_Driver (Inp);
                  Prev := Build_Dyadic (Ctxt, Id_And, Prev, En);
                  Disconnect (Inp);
                  Connect (Inp, Prev);
               end;
            when Id_Dyn_Insert =>
               raise Internal_Error;
            when others =>
               raise Internal_Error;
         end case;
      end loop;
   end Add_Enable_To_Dyn_Insert;

   --  SIG is a signal/isignal.
   procedure Merge_RAM_Muxes (Ctxt : Context_Acc; Sig : Instance)
   is
      Sig_Out : constant Net := Get_Output (Sig, 0);
      --  Stack is a table of inputs of dyn_insert* gates.  The first one is
      --  driven by SIG.
      Stack : Input_Tables.Instance;
      Last : Int32;
      Inp : Input;
      Inst : Instance;
   begin
      --  Gor forward, skip dyn_extract
      Input_Tables.Init (Stack, 16);
      Input_Tables.Append (Stack, Get_First_Sink (Sig_Out));

      loop
         Last := Input_Tables.Last (Stack);
         exit when Last < Input_Tables.First;
         Inp := Stack.Table (Last);

         Inst := Get_Input_Parent (Inp);
         case Get_Id (Inst) is
            when Id_Dyn_Extract =>
               --  Skip.
               raise Internal_Error;  -- TODO
            when Id_Dyn_Insert =>
               --  Continue with the output.
               Input_Tables.Append
                 (Stack, Get_First_Sink (Get_Output (Inst, 0)));
               Set_Mark_Flag (Inst, True);
            when Id_Mux2 =>
               declare
                  Sel : Net;
                  Prev : Net;
               begin
                  Sel := Get_Input_Net (Inst, 0);
                  if Get_Input (Inst, 1) = Inp then
                     --  Comes from input 1, so selected when Sel = 0.
                     Sel := Build_Monadic (Ctxt, Id_Not, Sel);
                     Prev := Get_Input_Net (Inst, 2);
                  else
                     pragma Assert (Get_Input (Inst, 2) = Inp);
                     --  Comes from input 2, so selected when sel = 1.
                     Prev := Get_Input_Net (Inst, 1);
                  end if;

                  --  If Prev is a marked Dyn_Insert then:
                  --    Look in Stack for the Dyn_Insert using Prev as input.
                  --    If found:
                  --      Append Sel to the enables to all Dyn_Insert from the
                  --      one found.
                  --    If not found:
                  --      side dyn_insert that has been handled.
                  --  If Prev is a mux or a not marked Dyn_Insert
                  --    Side dyn_insert not yet handled.
                  declare
                     Prev_Inst : Instance;
                     First : Int32;
                  begin
                     if Prev = Sig_Out then
                        First := Input_Tables.First;
                     else
                        Prev_Inst := Get_Net_Parent (Prev);
                        if Get_Id (Prev_Inst) not in Dyn_Insert_Module_Id then
                           raise Internal_Error;
                        end if;
                        if not Get_Mark_Flag (Prev_Inst) then
                           raise Internal_Error;
                        end if;
                        First := Find_First_Dyn_Insert (Stack, Prev);
                        if First = 0 then
                           --  Not found.
                           raise Internal_Error;
                        end if;
                     end if;

                     --  Add enable to all dyn_insert.
                     for I in First .. Last loop
                        Add_Enable_To_Dyn_Insert (Ctxt, Stack, First, Sel);
                     end loop;

                     --  Continue with the new input.
                  end;
               end;
            when others =>
               --  Cannot happen.
               raise Internal_Error;
         end case;
      end loop;
   end Merge_RAM_Muxes;

   procedure Extract_Memories2 (Ctxt : Context_Acc; M : Module)
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
            when Id_Dyn_Insert
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

      --  Try to merge dyn_insert with their enable mux.
      --  Must be done before extracting memories to simplify the walk.
      if False then
         for I in Instance_Tables.First .. Instance_Tables.Last (Dyns) loop
            Inst := Dyns.Table (I);
            if Get_Id (Inst) = Id_Dyn_Insert then
               --  Maybe transform as Dyn_Insert_En.
               --  Cannot be done before as it changes the list of gates.
               Inst := Maybe_Add_Enable_To_Dyn_Insert (Ctxt, Inst);
               Dyns.Table (I) := Inst;
            end if;
         end loop;
      end if;

      Instance_Tables.Init (Mems, 16);

      --  Extract memories (isignal/signal/const) from dyn gates.
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
                 | Id_Signal =>
                  if False then
                     Merge_RAM_Muxes (Ctxt, Inst);
                  end if;
                  if Validate_RAM (Inst) then
                     Check_RAM_Ports (Inst, Data_W, Size);
                     if Data_W /= 0 then
                        Info_Msg_Synth
                          (+Inst, "found RAM %n, width: %v bits, depth: %v",
                           (1 => +Inst, 2 => +Data_W, 3 => +Size));
                        Replace_RAM_Memory (Ctxt, Inst);
                     end if;
                  end if;
               when Id_Const_Bit =>
                  Check_Memory_Read_Ports (Inst, Data_W, Size);
                  if Data_W /= 0 then
                     Info_Msg_Synth
                       (+Inst, "found ROM %n, width: %v bits, depth: %v",
                        (1 => +Inst, 2 => +Data_W, 3 => +Size));
                     Replace_ROM_Memory (Ctxt, Inst);
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
         end;
      end loop;

      Instance_Tables.Free (Mems);
   end Extract_Memories2;
end Netlists.Memories;
