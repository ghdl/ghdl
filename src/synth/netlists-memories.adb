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

with Errorout; use Errorout;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Internings;
with Netlists.Errors; use Netlists.Errors;
with Netlists.Concats;

with Synth.Errors; use Synth.Errors;

package body Netlists.Memories is
   function Is_A_Memory (Mem : Instance) return Boolean is
   begin
      case Get_Id (Mem) is
         when Id_Signal
           | Id_Isignal
           | Id_Const_Bit =>
            return True;
         when others =>
            return False;
      end case;
   end Is_A_Memory;

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
                    (+Data_Inst, "write to a non memory %n", (1 => +Orig));
                  Data_W := 0;
                  return;
               end if;
               Data_Inst := Get_Net_Parent (Data);
            end;
         end loop;

         if Get_Id (Data_Inst) = Id_Dyn_Insert then
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
            Max : Uns32;
         begin
            --  Check max
            Max := Get_Param_Uns32 (Inst, 1);
            pragma Assert (Max /= 0);
            if (Max + 1) * Step /= Last_Size then
               raise Internal_Error;
            end if;

            --  Check addr width.
            if Addr_W > 31 then
               raise Internal_Error;
            end if;
            if Addr_W = 0 then
               raise Internal_Error;
            end if;
            if 2**Natural (Addr_W - 1) > Max then
               --  Need to truncate.
               raise Internal_Error;
            end if;
            Indexes (I).Addr := Sub_Addr;

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
         Idx : Net;
      begin
         for I in Indexes'Range loop
            Inp := Get_Input (Indexes (I).Inst, 0);
            Idx := Get_Driver (Inp);
            Disconnect (Inp);
            Append (Concat, Idx);
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

   --  MEM_LINK is the link from the last port (or from the memory).
   procedure Replace_Read_Ports
     (Ctxt : Context_Acc; Orig : Instance; Mem_Link : Net)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Last : Net;
   begin
      Last := Mem_Link;

      --  Convert readers.
      declare
         Inp : Input;
         Extr_Inst : Instance;
         Addr_Inp : Input;
         Addr : Net;
         Val : Net;
         Val_W : Width;
         Port_Inst : Instance;
      begin
         Inp := Get_First_Sink (Orig_Net);
         while Inp /= No_Input loop
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
                  Port_Inst := Build_Mem_Rd (Ctxt, Last, Addr, Val_W);

                  Redirect_Inputs (Val, Get_Output (Port_Inst, 1));

                  Remove_Instance (Extr_Inst);

                  Last := Get_Output (Port_Inst, 0);
               when others =>
                  raise Internal_Error;
            end case;
            Inp := Get_Next_Sink (Inp);
         end loop;
      end;
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
         exit when Get_Id (Data_Inst) = Id_Dyn_Insert;

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

   procedure Extract_Memories (Ctxt : Context_Acc; M : Module)
   is
      package Inst_Interning renames
        Netlists.Internings.Dyn_Instance_Interning;
      use Inst_Interning;
      Memories : Inst_Interning.Instance;

      Inst : Instance;
      Data : Instance;
      Add : Net;
   begin
      Inst_Interning.Init (Memories);

      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         --  Walk all the instances of M:
         case Get_Id (Inst) is
            when Id_Dyn_Insert =>
               --  * For dyn_insert gates:
               --    - The data input is the memory
               --    - The output is enable muxes followed by a flipfop.

               Data := Get_Net_Parent (Get_Input_Net (Inst, 0));
               --  If the data input is not a memory, emit a warning message.
               --  If the data input is a memory:
               --    * Save the memory, and the insert.
               if not Is_A_Memory (Data) then
                  Info_Msg_Synth (+Inst, "dynamic write to a non-memory");
               elsif Get_Param_Uns32 (Inst, 0) /= 0 then
                  --  Write from offset /= 0.
                  Info_Msg_Synth (+Inst, "complex memory not handled");
               else
                  Get (Memories, Data, Data);
               end if;

            when Id_Dyn_Extract =>
               --  * For dyn_extract gates:
               --    - The data input is the memory
               --      Or a another dyn_extract -> merge both dyn_extract
               --      A const
               --      A slice of mem
               loop
                  Data := Get_Net_Parent (Get_Input_Net (Inst, 0));
                  exit when Get_Id (Data) /= Id_Dyn_Extract;

                  if Get_Param_Uns32 (Data, 0) /= 0 then
                     raise Internal_Error;
                  end if;

                  --  Merge id_dyn_extract.
                  --  The order for Addix is important: from larger steps
                  --  to smaller ones.
                  Disconnect (Get_Input (Inst, 0));
                  Connect (Get_Input (Inst, 0), Get_Input_Net (Data, 0));
                  Disconnect (Get_Input (Data, 0));
                  Add := Build_Addidx
                    (Ctxt, Get_Input_Net (Data, 1), Get_Input_Net (Inst, 1));
                  Disconnect (Get_Input (Data, 1));
                  Disconnect (Get_Input (Inst, 1));
                  Connect (Get_Input (Inst, 1), Add);

                  Extract_Instance (Data);
                  Free_Instance (Data);
               end loop;

               if not Is_A_Memory (Data) then
                  Info_Msg_Synth (+Inst, "dynamic read from a non-memory");
               else
                  Get (Memories, Data, Data);
               end if;

            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;

      for I in First_Index .. Inst_Interning.Last_Index (Memories) loop
         --  INST is the memorizing instance, ie isignal/signal.
         Inst := Get_By_Index (Memories, I);
         declare
            Data_W : Width;
            Size : Width;
         begin
            case Get_Id (Inst) is
               when Id_Isignal
                 | Id_Signal =>
                  Check_RAM_Ports (Inst, Data_W, Size);
                  if Data_W /= 0 then
                     Info_Msg_Synth
                       (+Inst, "found RAM %n, width: %v bits, depth: %v",
                        (1 => +Inst, 2 => +Data_W, 3 => +Size));
                     Replace_RAM_Memory (Ctxt, Inst);
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

      Inst_Interning.Free (Memories);
   end Extract_Memories;
end Netlists.Memories;
