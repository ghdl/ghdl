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
            Max : Uns32;
         begin
            --  Check max
            Max := Get_Param_Uns32 (Inst, 1);
            if Max = 0 then
               Max := Last_Size / Step;
            end if;
            if Max * Step /= Last_Size then
               raise Internal_Error;
            end if;

            --  Check addr width.
            if Get_Width (Sub_Addr) > 31 then
               raise Internal_Error;
            end if;
            if 2**Natural (Get_Width (Sub_Addr)) > Max then
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

   procedure Replace_RAM_Memory (Ctxt : Context_Acc; Orig : Instance)
   is
      Orig_Net : constant Net := Get_Output (Orig, 0);
      Orig_Loc : constant Location_Type := Get_Location (Orig);
      Dff : Net;
      Dff_Inst : Instance;
      En : Net;
      Data : Net;
      Data_Inst : Instance;
      Last : Net;
      Data_Inp : Input;
   begin
      Warning_Msg_Synth (Orig_Loc, "found memory for %n", (1 => +Orig));

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
         when Id_Idff =>
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
            Error_Msg_Synth
              (Get_Location (Orig), "write to memory %n is not synchronous",
               (1 => +Orig));
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

      --  Convert readers.
      loop
         declare
            Inp : constant Input := Get_First_Sink (Orig_Net);
            Extr_Inst : Instance;
            Addr_Inp : Input;
            Addr : Net;
            Val : Net;
            Val_W : Width;
            Dest : Input;
            Port_Inst : Instance;
         begin
            exit when Inp = No_Input;

            Extr_Inst := Get_Input_Parent (Inp);
            pragma Assert (Get_Id (Extr_Inst) = Id_Dyn_Extract);
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

            if not Has_One_Connection (Val) then
               raise Internal_Error;
            end if;
            Dest := Get_First_Sink (Val);
            Disconnect (Dest);
            Connect (Dest, Get_Output (Port_Inst, 1));
            Remove_Instance (Extr_Inst);

            Last := Get_Output (Port_Inst, 0);
         end;
      end loop;

      Remove_Instance (Orig);
   end Replace_RAM_Memory;

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
                  Warning_Msg_Synth
                    (Get_Location (Inst), "dynamic write to a non-memory");
               elsif Get_Param_Uns32 (Inst, 0) /= 0 then
                  --  Write from offset /= 0.
                  Warning_Msg_Synth
                    (Get_Location (Inst), "complex memory not handled");
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
                  Disconnect (Get_Input (Inst, 0));
                  Connect (Get_Input (Inst, 0), Get_Input_Net (Data, 0));
                  Disconnect (Get_Input (Data, 0));
                  Add := Build_Addidx
                    (Ctxt, Get_Input_Net (Inst, 1), Get_Input_Net (Data, 1));
                  Disconnect (Get_Input (Data, 1));
                  Disconnect (Get_Input (Inst, 1));
                  Connect (Get_Input (Inst, 1), Add);

                  Extract_Instance (Data);
                  Free_Instance (Data);
               end loop;

               if not Is_A_Memory (Data) then
                  Warning_Msg_Synth
                    (Get_Location (Inst), "dynamic read from a non-memory");
               end if;

            when others =>
               null;
         end case;
         Inst := Get_Next_Instance (Inst);
      end loop;

      for I in First_Index .. Inst_Interning.Last_Index (Memories) loop
         --  INST is the memorizing instance, ie isignal/signal.
         Inst := Get_By_Index (Memories, I);
         if Get_Id (Inst) /= Id_Const_Bit then
            Replace_RAM_Memory (Ctxt, Inst);
         end if;
      end loop;

      Inst_Interning.Free (Memories);
   end Extract_Memories;
end Netlists.Memories;
