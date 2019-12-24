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

with Ada.Unchecked_Deallocation;
with Errorout; use Errorout;

with Grt.Algos;

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
                     pragma Assert (W /= 0);
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
   --  MEM is the memory (used to get its size).
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
                  Inst2 := Get_Input_Instance (Inst, 0);
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
         return Get_Input_Net (Mux_Inst, 2) = Dff_Out;
      else
         return Get_Input_Net (Mux_Inst, 1) = Dff_Out;
      end if;
   end Is_Enable_Dff;

   procedure Extract_Extract_Dff
     (Ctxt : Context_Acc;
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
               Inst := Get_Input_Instance (Inst, 0);
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

   procedure Unmark_Table (Els : Instance_Tables.Instance)
   is
      Inst : Instance;
   begin
      for I in Instance_Tables.First .. Instance_Tables.Last (Els) loop
         Inst := Els.Table (I);
         Set_Mark_Flag (Inst, False);
      end loop;
   end Unmark_Table;

   type Validate_RAM_Result is
     (
      --  The last gate is signal/isignal output.
      --  The end of the structure was reached.
      Validate_RAM_Signal,

      --  An element was recognized.
      Validate_RAM_Ok,

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
         when Validate_RAM_Ok =>
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
      --  Count number of muxes, inserts and signals that are connected to
      --  the output of FIRST (the Id_Signal/Id_Isignal).
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
            when Id_Dff =>
               if Get_Next_Sink (Inp) = No_Input
                 and then Get_First_Sink (First) = Inp
               then
                  --  The dff has only one output, hopefully a signal.
                  return Validate_RAM_Element (Get_Output (Inst, 0));
               else
                  return Validate_RAM_Type'
                    (Res => Validate_RAM_Error, Err => Inst);
               end if;
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
         elsif Nbr_Muxes = 0 then
            --  No insert, no muxes.  Just a normal dynamic extraction/mux.
            return Validate_RAM_Type'(Res => Validate_RAM_None);
         end if;
         --  Invalide shape.
         return Validate_RAM_Type'(Res => Validate_RAM_None);
      end if;

      if Nbr_Muxes = 0 and Nbr_Inserts = 1 then
         --  First case.
         return Validate_RAM_Type'(Res => Validate_RAM_Ok, Outp => Res);
      end if;

      declare
         subtype Instance_Array is Instance_Tables.Table_Type;
         --  Reserve one more slot.
         Muxes : Instance_Array (1 .. Nbr_Muxes + Nbr_Inserts + 1);
         First_Non_Mux : Nat32;
         El : Validate_RAM_Type;
         K : Nat32;
      begin
         --  Fill the tables.
         Nbr_Muxes := 0;
         First_Non_Mux := Muxes'Last + 1;
         Inp := Get_First_Sink (First);
         while Inp /= No_Input loop
            Inst := Get_Input_Parent (Inp);
            case Get_Id (Inst) is
               when Id_Mux2 =>
                  Nbr_Muxes := Nbr_Muxes + 1;
                  Muxes (Nbr_Muxes) := Inst;
               when Id_Dyn_Insert =>
                  First_Non_Mux := First_Non_Mux - 1;
                  Muxes (First_Non_Mux) := Inst;
               when Id_Dyn_Extract =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;
            Inp := Get_Next_Sink (Inp);
         end loop;
         pragma Assert (Nbr_Muxes + 2 = First_Non_Mux);

         loop
            --  Reduce non-mux.
            for I in First_Non_Mux .. Muxes'Last loop
               Inst := Muxes (I);
               loop
                  case Get_Id (Inst) is
                     when Id_Signal
                       | Id_Isignal =>
                        if Nbr_Muxes = 0 and then First_Non_Mux = Muxes'Last
                        then
                           --  The only gate.
                           return Validate_RAM_Type'
                             (Res => Validate_RAM_Signal, Sig => Inst);
                        else
                           return Validate_RAM_Type'
                             (Res => Validate_RAM_Error, Err => Inst);
                        end if;
                     when Id_Dyn_Insert
                       | Id_Mux2 =>
                        El := Validate_RAM_Element (Get_Output (Inst, 0));
                        case El.Res is
                           when Validate_RAM_Error
                             | Validate_RAM_None =>
                              --  A sub-element does not describe a RAM.
                              --  Returns the error.
                              return El;
                           when Validate_RAM_Mux =>
                              --  A sub-element ended with a mux.  This mux is
                              --  part of this sub-element.
                              Nbr_Muxes := Nbr_Muxes + 1;
                              Muxes (Nbr_Muxes) := El.Mux;
                              exit;
                           when Validate_RAM_Ok =>
                              --  Continue.
                              Inst := El.Outp;
                           when Validate_RAM_Signal =>
                              --  Continue.
                              Inst := El.Sig;
                        end case;
                     when Id_Dff =>
                        declare
                           O : constant Net := Get_Output (Inst, 0);
                           Inp : Input;
                        begin
                           Inp := Get_First_Sink (O);
                           if Inp = No_Input
                             or else Get_Next_Sink (Inp) /= No_Input
                           then
                              return Validate_RAM_Type'
                                (Res => Validate_RAM_Error, Err => Inst);
                           end if;
                           Inst := Get_Input_Parent (Inp);
                        end;
                     when others =>
                        raise Internal_Error;
                  end case;
               end loop;
            end loop;

            if Nbr_Muxes = 1 then
               --  So reduced to only one mux.  Return it.
               return Validate_RAM_Type'
                 (Res => Validate_RAM_Mux, Mux => Muxes (1));
            end if;

            --  Each muxes that appear twice are removed from muxes list,
            --  moved to the reduce list.
            First_Non_Mux := Muxes'Last + 1;
            K := 1;
            while K <= Nbr_Muxes loop
               Inst := Muxes (K);
               loop
                  if Get_Mark_Flag (Inst) then
                     --  Second input of the mux.
                     Set_Mark_Flag (Inst, False);
                     --  Move it to the non-mux list.
                     First_Non_Mux := First_Non_Mux - 1;
                     Muxes (First_Non_Mux) := Inst;
                     --  Remove it from mux list.
                     Inst := Muxes (Nbr_Muxes);
                     Nbr_Muxes := Nbr_Muxes - 1;
                     --  Exit if this was the last one.
                     exit when K > Nbr_Muxes;
                     Muxes (K) := Inst;
                  else
                     --  First input of the mux.  Mark it.
                     Set_Mark_Flag (Inst, True);
                     exit;
                  end if;
               end loop;
               K := K + 1;
            end loop;
            pragma Assert (First_Non_Mux > Nbr_Muxes);

            if First_Non_Mux = Muxes'Last + 1 then
               --  Nothing was done.
               if Nbr_Muxes = 0 then
                  --  Very wrong state.
                  raise Internal_Error;
               else
                  --  There are muxes with external inputs, like a reset value.
                  --  Not a RAM.
                  return Validate_RAM_Type'
                    (Res => Validate_RAM_Error, Err => Muxes (1));
               end if;
            end if;

            --  Remove them from Muxes array, unmark all of them.
            K := 1;
            while K <= Nbr_Muxes loop
               Inst := Muxes (K);
               loop
                  if Get_Mark_Flag (Inst) then
                     Set_Mark_Flag (Inst, False);
                     exit;
                  else
                     --  Remove.
                     pragma Assert (Get_Id (Inst) = Id_Mux2);
                     Inst := Muxes (Nbr_Muxes);
                     Nbr_Muxes := Nbr_Muxes - 1;
                     --  Exit if this was the last one.
                     exit when K > Nbr_Muxes;
                     Muxes (K) := Inst;
                  end if;
               end loop;
               K := K + 1;
            end loop;
         end loop;
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
         when Validate_RAM_Ok =>
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

   function Add_Enable_To_Dyn_Insert
     (Ctxt : Context_Acc; Inst : Instance; Sel : Net) return Instance
   is
      In_Mem : constant Input := Get_Input (Inst, 0);
      In_V : constant Input := Get_Input (Inst, 1);
      In_Idx : constant Input := Get_Input (Inst, 2);
      Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
      Dest : constant Input := Get_First_Sink (Get_Output (Inst, 0));
      pragma Assert (Has_One_Connection (Get_Output (Inst, 0)));
      Res : Net;
   begin
      Res := Build_Dyn_Insert_En
        (Ctxt, Get_Driver (In_Mem), Get_Driver (In_V), Get_Driver (In_Idx),
         Sel, Off);
      Set_Location (Res, Get_Location (Inst));

      Disconnect (In_Mem);
      Disconnect (In_V);
      Disconnect (In_Idx);
      Disconnect (Dest);
      Connect (Dest, Res);

      Remove_Instance (Inst);

      return Get_Net_Parent (Res);
   end Add_Enable_To_Dyn_Insert;

   --  Remove the mux2 MUX (by adding enable to dyn_insert).
   --  Return the new head.
   function Reduce_Muxes_Mux2 (Ctxt : Context_Acc; Mux : Instance)
                              return Instance
   is
      Dest : constant Input := Get_First_Sink (Get_Output (Mux, 0));
      Sel_Inp : constant Input := Get_Input (Mux, 0);
      In0 : constant Input := Get_Input (Mux, 1);
      In1 : constant Input := Get_Input (Mux, 2);
      Sel : Net;
      Drv0 : Net;
      Drv1 : Net;
      Drv : Net;
      Src : Net;
      Res : Instance;
      Inst : Instance;
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
      --  The important point is that the output od the dyn_insert is connected
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
      if Has_One_Connection (Drv0) and then not Has_One_Connection (Drv1) then
         Disconnect (In0);
         Disconnect (In1);
         Disconnect (Sel_Inp);
         Disconnect (Dest);
         Connect (Dest, Drv0);
         Drv := Drv0;
         Src := Drv1;
         Sel := Build_Monadic (Ctxt, Id_Not, Sel);
      elsif Has_One_Connection (Drv1) and then not Has_One_Connection (Drv0)
      then
         Disconnect (In0);
         Disconnect (In1);
         Disconnect (Sel_Inp);
         Disconnect (Dest);
         Connect (Dest, Drv1);
         Drv := Drv1;
         Src := Drv0;
      else
         --  Not an enable mux.
         raise Internal_Error;
      end if;

      Remove_Instance (Mux);

      --  Reduce Drv until Src.
      --  Transform dyn_insert to dyn_insert_en by adding SEL, or simply add
      --  SEL to existing dyn_insert_en.
      --  RES is the head of the result chain.
      Res := No_Instance;
      while Drv /= Src loop
         Inst := Get_Net_Parent (Drv);
         case Get_Id (Inst) is
            when Id_Mux2 =>
               --  Recurse on the mux.
               Inst := Reduce_Muxes_Mux2 (Ctxt, Inst);
               --  But continue with the result: still need to add the SEL.
               Drv := Get_Output (Inst, 0);
            when Id_Dyn_Insert =>
               --  Transform dyn_insert to dyn_insert_en.
               Inst := Add_Enable_To_Dyn_Insert (Ctxt, Inst, Sel);
               --  If this is the head, keep it.
               if Res = No_Instance then
                  Res := Inst;
               end if;
               --  Continue the walk with the next element.
               Drv := Get_Input_Net (Inst, 0);
            when Id_Dyn_Insert_En =>
               --  Simply add SEL to the enable input.
               declare
                  En_Inp : constant Input := Get_Input (Inst, 3);
                  En : Net;
               begin
                  En := Get_Driver (En_Inp);
                  Disconnect (En_Inp);
                  En := Build_Dyadic (Ctxt, Id_And, En, Sel);
                  Connect (En_Inp, En);
               end;
               --  If this is the head, keep it.
               if Res = No_Instance then
                  Res := Inst;
               end if;
               --  Continue the walk with the next element.
               Drv := Get_Input_Net (Inst, 0);
            when others =>
               raise Internal_Error;
         end case;
      end loop;

      return Res;
   end Reduce_Muxes_Mux2;

   --  From SIG (the signal/isignal for the RAM), move all the muxes to the
   --  dyn_insert.  The dyn_insert may be transformed to dyn_insert_en.
   --  At the end, the loop is linear and without muxes.
   procedure Reduce_Muxes (Ctxt : Context_Acc; Sig : Instance)
   is
      Inst : Instance;
   begin
      Inst := Get_Input_Instance (Sig, 0);

      --  Skip dff/idff.
      --  FIXME: that should be considered as an implicit mux.
      --  FIXME: what about dual-port RAMS with two different clocks ?
      case Get_Id (Inst) is
         when Id_Dff
           | Id_Idff =>
            Inst := Get_Input_Instance (Inst, 1);
         when others =>
            null;
      end case;

      --  Walk until the reaching SIG again.
      loop
         case Get_Id (Inst) is
            when Id_Mux2 =>
               --  Reduce the mux.
               Inst := Reduce_Muxes_Mux2 (Ctxt, Inst);
            when Id_Dyn_Insert
              | Id_Dyn_Insert_En =>
               --  Skip the dyn_insert.
               Inst := Get_Input_Instance (Inst, 0);
            when Id_Signal
              | Id_Isignal =>
               --  Should be done.
               if Inst /= Sig then
                  raise Internal_Error;
               end if;
               return;
            when others =>
               raise Internal_Error;
         end case;
      end loop;
   end Reduce_Muxes;

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

   procedure Convert_To_Memory (Ctxt : Context_Acc; Sig : Instance)
   is
      --  Size of RAM (in bits).
      Mem_Sz : constant Uns32 := Get_Width (Get_Output (Sig, 0));

      --  Width of the RAM, computed from the step of memidx.
      Mem_W : Width;

      --  Number of addresses of the memory.
      --  Sz = W * Depth.
      Mem_Depth : Uns32;

      Nbr_Ports : Int32;
      Inst : Instance;

      Offs : Off_Array_Acc;
      Nbr_Offs : Int32;
      Heads : Net_Array_Acc;
      Tails : Net_Array_Acc;
      Outs : Net_Array_Acc;
   begin
      --  1. Walk to count number of insert/extract instances + extract width
      Nbr_Ports := 0;
      Mem_W := 0;
      Inst := Sig;
      loop
         declare
            Inp : Input;
            N_Inst : Instance;
            In_Inst : Instance;
            Memidx : Instance;
         begin
            --  Check gates connected to the output.
            Inp := Get_First_Sink (Get_Output (Inst, 0));
            N_Inst := No_Instance;
            while Inp /= No_Input loop
               In_Inst := Get_Input_Parent (Inp);
               Memidx := No_Instance;
               case Get_Id (In_Inst) is
                  when Id_Dyn_Extract =>
                     Nbr_Ports := Nbr_Ports + 1;
                     Memidx := Get_Input_Instance (In_Inst, 1);
                  when Id_Dyn_Insert_En
                    | Id_Dyn_Insert =>
                     Nbr_Ports := Nbr_Ports + 1;
                     Memidx := Get_Input_Instance (In_Inst, 2);
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when Id_Dff
                    | Id_Idff
                    | Id_Signal
                    | Id_Isignal =>
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when others =>
                     raise Internal_Error;
               end case;
               if Memidx /= No_Instance then
                  if Mem_W = 0 then
                     Mem_W := Extract_Memidx_Step (Memidx);
                  elsif Mem_W /= Extract_Memidx_Step (Memidx) then
                     Info_Msg_Synth
                       (+Inst, "memory %n uses different widths",
                        (1 => +Inst));
                     return;
                  end if;
               end if;
               Inp := Get_Next_Sink (Inp);
            end loop;

            Inst := N_Inst;
            exit when Inst = Sig;
         end;
      end loop;

      if Mem_W = 0 then
         --  No ports ?
         raise Internal_Error;
      end if;

      Mem_Depth := Mem_Sz / Mem_W;

      Info_Msg_Synth
        (+Sig, "found RAM %n, width: %v bits, depth: %v",
         (1 => +Sig, 2 => +Mem_W, 3 => +Mem_Depth));

      --  2. Walk to extract offsets/width
      Offs := new Off_Array (1 .. 2 * Nbr_Ports);
      Nbr_Offs := 0;
      Inst := Sig;
      loop
         declare
            Inp : Input;
            N_Inst : Instance;
            In_Inst : Instance;
            Ow : Off_Array (1 .. 2);
         begin
            --  Check gates connected to the output.
            Inp := Get_First_Sink (Get_Output (Inst, 0));
            N_Inst := No_Instance;
            while Inp /= No_Input loop
               In_Inst := Get_Input_Parent (Inp);
               Ow := (0, 0);
               case Get_Id (In_Inst) is
                  when Id_Dyn_Extract =>
                     Ow := (1 => Get_Param_Uns32 (In_Inst, 0),
                            2 => Get_Width (Get_Output (In_Inst, 0)));
                  when Id_Dyn_Insert_En
                    | Id_Dyn_Insert =>
                     Ow := (1 => Get_Param_Uns32 (In_Inst, 0),
                            2 => Get_Width (Get_Input_Net (In_Inst, 1)));
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when Id_Dff
                    | Id_Idff
                    | Id_Signal
                    | Id_Isignal =>
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when others =>
                     raise Internal_Error;
               end case;
               if Ow (2) /= 0 then
                  --  Ow (2) was just a width, convert it to an offset.
                  Ow (2) := Ow (1) + Ow (2);
                  if Nbr_Offs = 0 or else Ow /= Offs (1 .. 2) then
                     Nbr_Offs := Nbr_Offs + 2;
                     Offs (Nbr_Offs -1 .. Nbr_Offs) := Ow;
                  end if;
               end if;
               Inp := Get_Next_Sink (Inp);
            end loop;

            Inst := N_Inst;
            exit when Inst = Sig;
         end;
      end loop;

      --  2.1 Sort
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

         P : Nat32;
      begin
         Heap_Sort (Natural (Nbr_Offs));

         --  Remove duplicates.
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
      Heads := new Net_Array (1 .. Nbr_Offs - 1);
      Tails := new Net_Array (1 .. Nbr_Offs - 1);
      Outs := new Net_Array (1 .. Nbr_Offs - 1);

      --  4. Create Memory/Memory_Init from signal/isignal.
      for I in 1 .. Nbr_Offs - 1 loop
         declare
            Wd : constant Width := (Offs (I + 1) - Offs (I)) * Mem_Depth;
         begin
            case Get_Id (Sig) is
               when Id_Isignal =>
                  Heads (I) := Build_Memory_Init
                    (Ctxt, Wd, Build2_Extract (Ctxt, Get_Input_Net (Sig, 1),
                                               Offs (I), Wd));
               when Id_Signal =>
                  Heads (I) := Build_Memory (Ctxt, Wd);
               when others =>
                  raise Internal_Error;
            end case;
            Tails (I) := Heads (I);
         end;
      end loop;

      --  5. For each part of the data, create memory ports
      declare
         Inp : Input;
         N_Inp : Input;
         N_Inst : Instance;
         In_Inst : Instance;
         Dff_Clk : Net;
      begin
         --  Try to extract clock from dff.
         Dff_Clk := No_Net;
         Inst := Get_Input_Instance (Sig, 0);
         case Get_Id (Inst) is
            when Id_Dff
              | Id_Idff =>
               Dff_Clk := Get_Input_Net (Inst, 0);
            when others =>
               null;
         end case;

         --  Do the real work: transform gates to ports.
         Inst := Sig;
         loop
            --  Check gates connected to the output.
            N_Inst := No_Instance;
            Inp := Get_First_Sink (Get_Output (Inst, 0));
            while Inp /= No_Input loop
               In_Inst := Get_Input_Parent (Inp);
               N_Inp := Get_Next_Sink (Inp);
               case Get_Id (In_Inst) is
                  when Id_Dyn_Extract =>
                     declare
                        Off : constant Uns32 := Get_Param_Uns32 (In_Inst, 0);
                        Wd : constant Width := Get_Width (Get_Output
                                                            (In_Inst, 0));
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
                        Extract_Extract_Dff
                          (Ctxt, In_Inst, Last_Inst, Clk, En);
                        if Clk /= No_Net and then En = No_Net then
                           En := Build_Const_UB32 (Ctxt, 1, 1);
                        end if;
                        --  iterate to build mem_rd/mem_rd_sync
                        for I in Idx .. Idx + Len - 1 loop
                           if Clk /= No_Net then
                              Rd_Inst := Build_Mem_Rd_Sync
                                (Ctxt, Tails (I), Addr, Clk, En,
                                 Offs (Idx + 1) - Offs (Idx));
                           else
                              Rd_Inst := Build_Mem_Rd
                                (Ctxt, Tails (I), Addr,
                                 Offs (Idx + 1) - Offs (Idx));
                           end if;
                           Tails (I) := Get_Output (Rd_Inst, 0);
                           Outs (I) := Get_Output (Rd_Inst, 1);
                        end loop;
                        if Len = 1 then
                           Rd := Outs (Idx);
                        else
                           Rd := Build2_Concat
                             (Ctxt, Outs (Idx .. Idx + Len - 1));
                        end if;
                        Redirect_Inputs (Get_Output (Last_Inst, 0), Rd);
                        Disconnect (Get_Input (In_Inst, 0));
                        if Last_Inst /= In_Inst then
                           Remove_Instance (Last_Inst);
                        end if;
                        Remove_Instance (In_Inst);
                     end;
                  when Id_Dyn_Insert_En
                    | Id_Dyn_Insert
                    | Id_Dff
                    | Id_Idff
                    | Id_Signal
                    | Id_Isignal =>
                     pragma Assert (N_Inst = No_Instance);
                     N_Inst := In_Inst;
                  when others =>
                     raise Internal_Error;
               end case;
               Inp := N_Inp;
            end loop;

            --  Handle INST.
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
                  begin
                     Off_Array_To_Idx (Offs.all, Off, Wd, Idx, Len);
                     Inp2 := Get_Input (Inst, 2);
                     Addr := Get_Driver (Inp2);
                     Disconnect (Inp2);
                     Convert_Memidx (Ctxt, Mem_Sz, Addr, Mem_W);
                     pragma Assert (Dff_Clk /= No_Net);
                     if Get_Id (Inst) = Id_Dyn_Insert_En then
                        Inp2 := Get_Input (Inst, 3);
                        En := Get_Driver (Inp2);
                        Disconnect (Inp2);
                     else
                        En := Build_Const_UB32 (Ctxt, 1, 1);
                     end if;
                     for I in Idx .. Idx + Len - 1 loop
                        Inp2 := Get_Input (Inst, 1);
                        Dat := Get_Driver (Inp2);
                        Wr_Inst := Build_Mem_Wr_Sync
                          (Ctxt, Tails (I), Addr, Dff_Clk, En, Dat);
                        Disconnect (Inp2);
                        Tails (I) := Get_Output (Wr_Inst, 0);
                     end loop;
                  end;
                  Inp := Get_Input (Inst, 0);
                  Disconnect (Inp);
                  Remove_Instance (Inst);
               when Id_Dff
                 | Id_Idff =>
                  Disconnect (Get_Input (Inst, 0));
                  Disconnect (Get_Input (Inst, 1));
                  if Get_Id (Inst) = Id_Idff then
                     Disconnect (Get_Input (Inst, 2));
                  end if;
                  Remove_Instance (Inst);
               when Id_Signal
                 | Id_Isignal =>
                  null;
               when others =>
                  raise Internal_Error;
            end case;

            Inst := N_Inst;
            case Get_Id (Inst) is
               when Id_Signal =>
                  exit;
               when Id_Isignal =>
                  Disconnect (Get_Input (Inst, 1));
                  exit;
               when others =>
                  null;
            end case;
         end loop;

         --  Finish to remove the signal/isignal.
         Disconnect (Get_Input (Inst, 0));
         Remove_Instance (Inst);
      end;

      --  6. Cleanup.
      Free_Off_Array (Offs);
      Free_Net_Array (Heads);
      Free_Net_Array (Tails);
      Free_Net_Array (Outs);
   end Convert_To_Memory;

   function Is_Const_Input (Inst : Instance) return Boolean is
   begin
      case Get_Id (Inst) is
         when Id_Const_Bit =>
            return True;
         when Id_Signal
           | Id_Isignal =>
            return Is_Const_Input (Get_Input_Instance (Inst, 0));
         when others =>
            --  FIXME: handle other consts ?
            return False;
      end case;
   end Is_Const_Input;

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
                  Replace_ROM_Memory (Ctxt, Inst);
               end if;
            else
               if Validate_RAM (Inst) then
                  Reduce_Muxes (Ctxt, Inst);
                  Convert_To_Memory (Ctxt, Inst);
               end if;
            end if;
         end;
      end loop;

      Instance_Tables.Free (Mems);
   end Extract_Memories2;
end Netlists.Memories;
