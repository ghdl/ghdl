--  Expand dyn gates.
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

with Mutils; use Mutils;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Memories; use Netlists.Memories;
with Netlists.Concats; use Netlists.Concats;
with Netlists.Folds; use Netlists.Folds;

package body Netlists.Expands is
   --  Extract Memidx from ADDR_NET and return the number of
   --   elements NBR_ELS (which is usually 2**width(ADDR_NET) / data_w).
   --  Memidx are ordered from the one with the largest step to the one
   --   with the smallest step.
   --  Addridx are removed.
   procedure Gather_Memidx (Addr_Net : Net;
                            Memidx_Arr : out Instance_Array;
                            Nbr_Els : out Natural)
   is
      N : Net;
      P : Nat32;
      Ninst : Instance;
      Memidx : Instance;
      Max : Uns32;
      Inp_Net : Net;
   begin
      N := Addr_Net;
      Nbr_Els := 1;
      P := Memidx_Arr'First;
      if P = 0 then
         return;
      end if;
      loop
         Ninst := Get_Net_Parent (N);
         case Get_Id (Ninst) is
            when Id_Memidx =>
               Memidx := Ninst;
            when Id_Addidx =>
               --  Extract memidx.
               Inp_Net := Get_Input_Net (Ninst, 0);
               Memidx := Get_Net_Parent (Inp_Net);
               pragma Assert (Get_Id (Memidx) = Id_Memidx);
               --  Extract next element in the chain
               N := Get_Input_Net (Ninst, 1);
            when others =>
               raise Internal_Error;
         end case;

         Memidx_Arr (P) := Memidx;

         --  Check memidx are ordered by increasing step.
         pragma Assert
           (P = Memidx_Arr'First
              or else (Get_Param_Uns32 (Memidx, 0)
                         >= Get_Param_Uns32 (Memidx_Arr (P - 1), 0)));

         Max := Get_Param_Uns32 (Memidx, 1);
         Nbr_Els := Nbr_Els * Natural (Max + 1);

         P := P + 1;

         exit when Memidx = Ninst;
      end loop;
   end Gather_Memidx;

   procedure Remove_Memidx (Addr_Net : Net)
   is
      Inst : Instance;
   begin
      Inst := Get_Net_Parent (Addr_Net);

      --  Still used by another dyn_insert/dyn_extract (subprogram interface)
      if Is_Connected (Addr_Net) then
         return;
      end if;

      loop
         case Get_Id (Inst) is
            when Id_Memidx =>
               Disconnect (Get_Input (Inst, 0));
               Remove_Instance (Inst);
               exit;
            when Id_Addidx =>
               declare
                  Inp_Net : Net;
                  Memidx : Instance;
               begin
                  --  Extract memidx.
                  Inp_Net := Disconnect_And_Get (Inst, 1);
                  Memidx := Get_Net_Parent (Inp_Net);
                  pragma Assert (Get_Id (Memidx) = Id_Memidx);

                  Disconnect (Get_Input (Memidx, 0));
                  Remove_Instance (Memidx);

                  --  Extract next element in the chain
                  Inp_Net := Disconnect_And_Get (Inst, 0);
                  Remove_Instance (Inst);

                  Inst := Get_Net_Parent (Inp_Net);
               end;
            when others =>
               raise Internal_Error;
         end case;
      end loop;
   end Remove_Memidx;

   --  Extract address from memidx/addidx and disconnect those gates.
   procedure Extract_Address
     (Ctxt : Context_Acc; Memidx_Arr : Instance_Array; Addr : out Net)
   is
      Inst : Instance;
      Max : Uns32;
      Max_Width : Width;
      Part_Addr : Net;
   begin
      --  Avoid warnings
      Max := 0;
      Max_Width := 0;

      --  Dimension 1 is the least significant part of the address
      for I in Memidx_Arr'Range loop
         Inst := Memidx_Arr (I);

         --  INST1 is a memidx.
         Part_Addr := Get_Input_Net (Inst, 0);
         if I = Memidx_Arr'First then
            --  First memidx, which is the LSB.  Nothing to do.
            Addr := Part_Addr;
         else
            --  Following memidx, need to be multiplied by (max+1) of the
            --  previous memidx.
            if 2**Natural (Max_Width) /= Max + 1 then
               --  Not a power of 2!
               --  ADDR := ADDR + PART_ADDR * (MAX + 1)
               declare
                  W : constant Width := Get_Width (Part_Addr) + Max_Width;
                  Loc : constant Location_Type := Get_Location (Inst);
                  Mul : Net;
               begin
                  Mul := Build2_Const_Uns (Ctxt, Uns64 (Max + 1), W);
                  Part_Addr := Build2_Uresize (Ctxt, Part_Addr, W, Loc);
                  Part_Addr := Build_Dyadic (Ctxt, Id_Umul, Part_Addr, Mul);
                  Set_Location (Part_Addr, Loc);
                  Addr := Build2_Uresize (Ctxt, Addr, W, Loc);
                  Addr := Build_Dyadic (Ctxt, Id_Add, Addr, Part_Addr);
                  Set_Location (Addr, Loc);
               end;
            else
               Addr := Build_Concat2 (Ctxt, Part_Addr, Addr);
               Copy_Location (Addr, Inst);
            end if;
         end if;
         Max := Get_Param_Uns32 (Inst, 1);
         Max_Width := Clog2 (Max + 1);
      end loop;
   end Extract_Address;

   procedure Truncate_Address
     (Ctxt : Context_Acc; Addr : in out Net; Nbr_Els : Natural)
   is
      Loc : Location_Type;
      Addr_Len : Width;
   begin
      Addr_Len := Uns32 (Clog2 (Uns64 (Nbr_Els)));
      if Get_Width (Addr) > Addr_Len then
         --  Truncate the address.  This is requied so that synth_case doesn't
         --  use default value.
         Loc := Get_Location (Get_Net_Parent (Addr));
         Addr := Build_Trunc (Ctxt, Id_Utrunc, Addr, Addr_Len);
         Set_Location (Addr, Loc);
      end if;
   end Truncate_Address;

   --  Extract address from memidx/addidx and disconnect those gates.
   function Extract_Flat_Address
     (Ctxt : Context_Acc; Idx_Arr : Instance_Array) return Net
   is
      Inst : Instance;
      Res : Net;
      Res_W : Width;
      Inp_Net : Net;
      Out_W : Width;
      Loc : Location_Type;
      Step : Uns32;
      Cst : Net;
      Log2_Step : Width;
   begin
      Res := No_Net;
      Res_W := 0;

      for I in Idx_Arr'Range loop
         --  Extract parameters from memidx.
         Inst := Idx_Arr (I);
         Step := Get_Param_Uns32 (Inst, 0);
         Loc := Get_Location (Inst);

         --  Extract input of memidx.
         Inp_Net := Get_Input_Net (Inst, 0);

         Out_W := Get_Width (Get_Output (Inst, 0));

         if Step = 1 then
            Res := Inp_Net;
            Res_W := Out_W;
         elsif Is_Pow2 (Step) then
            --  Step is a power of 2, simply concatenate
            Log2_Step := Clog2 (Step);
            if Log2_Step = Res_W then
               Res := Build_Concat2 (Ctxt, Inp_Net, Res);
               Set_Location (Res, Loc);
            else
               Cst := Build2_Const_Uns (Ctxt, 0, Log2_Step - Res_W);
               if Res = No_Net then
                  Res := Build_Concat2 (Ctxt, Inp_Net, Cst);
               else
                  Res := Build_Concat3 (Ctxt, Inp_Net, Cst, Res);
               end if;
            end if;
            Res_W := Out_W;
         else
            --  RES := RES + INP*STEP
            Res_W := Out_W;
            Cst := Build2_Const_Uns (Ctxt, Uns64 (Step), Res_W);
            Inp_Net := Build2_Uresize (Ctxt, Inp_Net, Res_W, Loc);

            Inp_Net := Build_Dyadic (Ctxt, Id_Umul, Inp_Net, Cst);
            Set_Location (Inp_Net, Loc);

            if Res = No_Net then
               Res := Inp_Net;
            else
               Res := Build2_Uresize (Ctxt, Res, Res_W, Loc);
               Res := Build_Dyadic (Ctxt, Id_Add, Res, Inp_Net);
               Set_Location (Res, Loc);
            end if;
         end if;
      end loop;

      return Res;
   end Extract_Flat_Address;

   function Memidx_No_Overlap (Idx_Arr : Instance_Array; Wd : Width)
                              return Boolean is
   begin
      for I in Idx_Arr'Range loop
         declare
            Inst : constant Instance := Idx_Arr (I);
            Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
         begin
            if Step mod Wd /= 0 then
               return False;
            end if;
         end;
      end loop;
      return True;
   end Memidx_No_Overlap;

   --  Extract non-overlaping slices from INP using bmux...
   --  ... or build: INP[W*addr + (W-1):W*addr]
   function Extract_Bmux
     (Ctxt : Context_Acc; Idx_Arr : Instance_Array; Inp : Net) return Net
   is
      Out_Wd : constant Uns32 := Get_Param_Uns32 (Idx_Arr (Idx_Arr'First), 0);
      Inst : Instance;
      I, Il : Nat32;
      Res : Net;
      Res_Wd : Width;
      N_Wd : Width;
      Step : Uns32;
      Stepl : Uns32;
      Addr : Net;
   begin
      --  Start from the whole 'memory'.  Slices will be extracted from it.
      Res := Inp;
      Res_Wd := Get_Width (Res);

      --  Start from the last memidx (so the one with the largest step).
      I := Idx_Arr'Last;
      loop
         --  Extract parameters from memidx.
         Inst := Idx_Arr (I);

         --  We will work with memidx whose step is STEP
         Step := Get_Param_Uns32 (Inst, 0);

         --  Try to gather several memidx to reduce the number of bmux.
         --  Gather memidx whose step is a divider of the largest one.
         --  Scan from larger steps to smaller one.
         Il := I;
         Stepl := Step;
         while Il > Idx_Arr'First loop
            declare
               Inst1 : constant Instance := Idx_Arr (Il - 1);
               Step1 : constant Uns32 := Get_Param_Uns32 (Inst1, 0);
            begin
               exit when Step mod Step1 /= 0;
               Stepl := Step1;
               Il := Il - 1;
            end;
         end loop;

         --  Combine index.
         Addr := No_Net;
         for J in Il .. I loop
            declare
               Inst1 : constant Instance := Idx_Arr (J);
               Step1 : constant Uns32 := Get_Param_Uns32 (Inst1, 0);
               Len1 : constant Uns32 := Get_Param_Uns32 (Inst1, 1) + 1;
               Log2_Len : constant Uns32 := Clog2 (Len1 * Step1);
               Idx : Net;
            begin
               Idx := Get_Input_Net (Inst1, 0);

               if Get_Width (Idx) > Log2_Len then
                  Idx := Build2_Trunc
                    (Ctxt, Id_Utrunc, Idx, Log2_Len, Get_Location (Inst1));
               end if;

               Addr := Build2_Addmul
                 (Ctxt, Idx, Step1 / Out_Wd, Addr, Get_Location (Inst1));
            end;
         end loop;

         --  Resize the input.
         N_Wd := Stepl * 2**Natural(Get_Width (Addr));
         if Res_Wd /= N_Wd then
            Res := Build2_Xresize (Ctxt, Res, N_Wd, Get_Location (Inst));
         end if;

         Res := Build_Bmux (Ctxt, Stepl, Res, Addr);
         Set_Location (Res, Get_Location (Inst));

         Res_Wd := Stepl;

         exit when Il = Idx_Arr'First;
         I := Il - 1;
      end loop;

      return Res;
   end Extract_Bmux;

   procedure Expand_Dyn_Extract (Ctxt : Context_Acc; Inst : Instance)
   is
      Loc : constant Location_Type := Get_Location (Inst);
      Mem : constant Net := Get_Input_Net (Inst, 0);
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      Addr_Net : constant Net := Disconnect_And_Get (Inst, 1);
      --  1. compute number of dims, check order.
      Ndims : constant Nat32 := Nat32 (Count_Memidx (Addr_Net));
      Nbr_Els : Natural;

      Memidx_Arr : Instance_Array (1 .. Ndims);

      Res : Net;
      Addr : Net;
   begin
      --  1.1  Fill memidx_arr.
      --  2. compute number of cells.
      Gather_Memidx (Addr_Net, Memidx_Arr, Nbr_Els);

      if Nbr_Els = 1 then
         --  There is only one element, so it's not really dynamic.
         --  Just return the value.
         Res := Mem;
      elsif Memidx_No_Overlap (Memidx_Arr, W) then
         --  Use Bmux if no overlap: every mem element can be selected by
         --   only one index.
         --  This is not the case for slices like:
         --   mem(idx + 1 donwto idx)
         declare
            Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
         begin
            Res := Mem;
            if Off /= 0 then
               Res := Build2_Extract
                 (Ctxt, Res, Off, Get_Width (Mem) - Off, Loc);
            end if;
            Res := Extract_Bmux (Ctxt, Memidx_Arr, Res);
            if Get_Width (Res) /= W then
               Res := Build2_Extract (Ctxt, Res, 0, W, Loc);
            end if;
         end;
      else
         --  2. Compute index
         Addr := Extract_Flat_Address (Ctxt, Memidx_Arr);

         Remove_Memidx (Addr_Net);
         --  Keep Dyn_Extract, but the index is a single value
         Connect (Get_Input (Inst, 1), Addr);
         return;
      end if;

      Remove_Memidx (Addr_Net);

      --  Remove old dyn_extract.
      Disconnect (Get_Input (Inst, 0));
      Redirect_Inputs (Get_Output (Inst, 0), Res);
      Remove_Instance (Inst);
   end Expand_Dyn_Extract;

   --  Generate address decoder so that:
   --  Net_Arr(N) = (addr = N)
   procedure Generate_Decoder (Ctxt : Context_Acc;
                               Addr : Net;
                               Net_Arr : out Net_Array;
                               Loc : Location_Type)
   is
      W : constant Width := Get_Width (Addr);
      V0, V1 : Net;
      V : Net;
      J : Int32;
      Step : Int32;
   begin
      if W = 0 then
         Net_Arr (0) := Build_Const_UB32 (Ctxt, 1, 1);
         return;
      end if;

      for I in reverse 0 .. W - 1 loop
         V1 := Build_Extract_Bit (Ctxt, Addr, I);
         Set_Location (V1, Loc);
         V0 := Build_Monadic (Ctxt, Id_Not, V1);
         Set_Location (V0, Loc);
         Step := 2**Natural (I);
         if I = W - 1 then
            Net_Arr (0) := V0;
            Net_Arr (Step) := V1;
         else
            J := 0;
            loop
               V := Net_Arr (J);
               Net_Arr (J) := Build_Dyadic (Ctxt, Id_And, V, V0);
               Set_Location (Net_Arr (J), Loc);
               J := J + Step;
               exit when J > Net_Arr'Last;
               Net_Arr (J) := Build_Dyadic (Ctxt, Id_And, V, V1);
               Set_Location (Net_Arr (J), Loc);
               J := J + Step;
               exit when J > Net_Arr'Last;
            end loop;
         end if;
      end loop;
   end Generate_Decoder;

   procedure Generate_Muxes (Ctxt : Context_Acc;
                             Concat : in out Concat_Type;
                             Mem : Net;
                             Off : in out Uns32;
                             Dat : Net;
                             Memidx_Arr : Instance_Array;
                             Net_Arr : Net_Array;
                             Loc : Location_Type;
                             En : Net := No_Net)
   is
      Dat_W : constant Width := Get_Width (Dat);
      type Count_Type is record
         Step : Uns32;
         Max : Uns32;
         Val : Uns32;
      end record;
      type Count_Array is array (Memidx_Arr'Range) of Count_Type;
      Count : Count_Array;

      V : Net;
      Sel : Int32;
      Next_Off : Uns32;
      Prev_Net : Net;
      Step : Uns32;
      S : Net;
   begin
      --  Initialize count.
      for I in Memidx_Arr'Range loop
         declare
            Inst : constant Instance := Memidx_Arr (I);
         begin
            Count (I) := (Step => Get_Param_Uns32 (Inst, 0),
                          Max => Get_Param_Uns32 (Inst, 1),
                          Val => 0);
         end;
      end loop;

      Sel := 0;

      Prev_Net := No_Net;
      Next_Off := 0;

      if Off /= 0 then
         Append (Concat, Build2_Extract (Ctxt, Mem, 0, Off, Loc));
         Next_Off := Off;
      end if;

      loop
         if Next_Off > Off then
            --  Partial overlap.
            --  Append previous net partially, extract from previous net and
            --  mem.
            --
            --  |<----------- Dat_W ------------>|
            --  |<- Step ->|
            --             Off                    Next_Off
            --  +----------+----------+----------++
            --  | Prev                           |
            --  +----------+----------+----------+
            --  +----------+----------+----------+----------+
            --  | Mem                                       |
            --  +----------+----------+----------+----------+
            --             +----------+----------+----------+
            --             | Dat                            |
            --             +----------+----------+----------+
            Step := Dat_W - (Next_Off - Off);
            Append (Concat, Build2_Extract (Ctxt, Prev_Net, 0, Step, Loc));
            V := Build_Concat2
              (Ctxt,
               Build2_Extract (Ctxt, Mem, Next_Off, Step, Loc),
               Build2_Extract (Ctxt, Prev_Net, Step, Dat_W - Step, Loc));
            Set_Location (V, Loc);
         else
            --  No overlap.
            if Prev_Net /= No_Net then
               Append (Concat, Prev_Net);
            end if;

            if Next_Off < Off then
               --  But there is a gap.
               Append (Concat, Build2_Extract (Ctxt, Mem, Next_Off,
                                               Off - Next_Off, Loc));
            end if;
            V := Build2_Extract (Ctxt, Mem, Off, Dat_W, Loc);
         end if;

         S := Net_Arr (Sel);
         if En /= No_Net then
            S := Build_Dyadic (Ctxt, Id_And, S, En);
            Set_Location (S, Loc);
         end if;

         V := Build_Mux2 (Ctxt, S, V, Dat);
         Set_Location (V, Loc);
         Prev_Net := V;
         Next_Off := Off + Dat_W;

         Sel := Sel + 1;

         --  Increase Off.
         for I in Memidx_Arr'Range loop
            declare
               C : Count_Type renames Count (I);
            begin
               C.Val := C.Val + C.Step;
               Off := Off + C.Step;
               exit when C.Val <= C.Max * C.Step;
               if I = Memidx_Arr'Last then
                  --  End.
                  Append (Concat, Prev_Net);
                  Off := Next_Off;
                  return;
               end if;
               Off := Off - C.Val;
               C.Val := 0;
            end;
         end loop;
      end loop;
   end Generate_Muxes;

   procedure Expand_Dyn_Insert
     (Ctxt : Context_Acc; Inst : Instance; En : Net)
   is
      Loc : constant Location_Type := Get_Location (Inst);
      Mem : constant Net := Get_Input_Net (Inst, 0);
      Dat : constant Net := Get_Input_Net (Inst, 1);
      Addr_Net : constant Net := Disconnect_And_Get (Inst, 2);
      O : constant Net := Get_Output (Inst, 0);
      O_W : constant Width := Get_Width (O);
      --  1. compute number of dims, check order.
      Ndims : constant Nat32 := Nat32 (Count_Memidx (Addr_Net));
      Nbr_Els : Natural;

      Memidx_Arr : Instance_Array (1 .. Ndims);

      Net_Arr : Net_Array_Acc;

      Addr : Net;

      Concat : Concat_Type;
      Res : Net;
   begin
      Gather_Memidx (Addr_Net, Memidx_Arr, Nbr_Els);

      Extract_Address (Ctxt, Memidx_Arr, Addr);

      --  Generate decoder.
      Net_Arr := new Net_Array(0 .. Int32 (Nbr_Els - 1));
      Truncate_Address (Ctxt, Addr, Nbr_Els);
      Generate_Decoder (Ctxt, Addr, Net_Arr.all, Loc);

      --  Build muxes
      declare
         Off : Uns32;
      begin
         Off := Get_Param_Uns32 (Inst, 0);
         Generate_Muxes
           (Ctxt, Concat, Mem, Off, Dat, Memidx_Arr, Net_Arr.all, Loc, En);
         if Off < O_W then
            Append (Concat, Build2_Extract (Ctxt, Mem, Off, O_W - Off, Loc));
         end if;
      end;
      Build (Ctxt, Concat, Loc, Res);
      pragma Assert (Get_Width (Res) = O_W);

      Free_Net_Array (Net_Arr);

      --  Replace gate.
      Redirect_Inputs (O, Res);
      Disconnect (Get_Input (Inst, 0));
      Disconnect (Get_Input (Inst, 1));
      if En /= No_Net then
         Disconnect (Get_Input (Inst, 3));
      end if;
      Remove_Instance (Inst);

      Remove_Memidx (Addr_Net);
   end Expand_Dyn_Insert;

   --  Replace instance INST a ROT b by: S (a, b) | C (a, l - b)
   --  (S for shifted, C for counter-shifted)
   procedure Expand_Rot (Ctxt : Context_Acc;
                         Inst : Instance;
                         Id_S, Id_C : Shift_Module_Id)
   is
      Loc : constant Location_Type := Get_Location (Inst);
      Val : constant Input := Get_Input (Inst, 0);
      Amt : constant Input := Get_Input (Inst, 1);
      Val_N : constant Net := Get_Driver (Val);
      Amt_N : constant Net := Get_Driver (Amt);
      W_Val : constant Width := Get_Width (Val_N);
      W_Amt : constant Width := Clog2 (W_Val);
      Sh_S : Net;
      R_Amt : Net;
      Sh_C : Net;
      Res : Net;
   begin
      Sh_S := Build_Shift_Rotate (Ctxt, Id_S, Val_N, Amt_N);
      Set_Location (Sh_S, Loc);
      R_Amt := Build_Dyadic (Ctxt, Id_Sub,
                             Build_Const_UB32 (Ctxt, W_Val, W_Amt),
                             Build2_Uresize (Ctxt, Amt_N, W_Amt, Loc));
      Set_Location (R_Amt, Loc);
      Sh_C := Build_Shift_Rotate (Ctxt, Id_C, Val_N, R_Amt);
      Set_Location (Sh_C, Loc);
      Res := Build_Dyadic (Ctxt, Id_Or, Sh_S, Sh_C);
      Set_Location (Res, Loc);

      Redirect_Inputs (Get_Output (Inst, 0), Res);
      Disconnect (Val);
      Disconnect (Amt);
      Remove_Instance (Inst);
   end Expand_Rot;

   procedure Expand_Rol (Ctxt : Context_Acc; Inst : Instance) is
   begin
      Expand_Rot (Ctxt, Inst, Id_Lsl, Id_Lsr);
   end Expand_Rol;

   procedure Expand_Ror (Ctxt : Context_Acc; Inst : Instance) is
   begin
      Expand_Rot (Ctxt, Inst, Id_Lsr, Id_Lsl);
   end Expand_Ror;

   procedure Expand_Gates (Ctxt : Context_Acc; M : Module)
   is
      Inst : Instance;
      Ninst : Instance;
   begin
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         --  Walk all the instances of M:
         Ninst := Get_Next_Instance (Inst);
         case Get_Id (Inst) is
            when Id_Dyn_Extract =>
               Expand_Dyn_Extract (Ctxt, Inst);

            when Id_Dyn_Insert =>
               Expand_Dyn_Insert (Ctxt, Inst, No_Net);
            when Id_Dyn_Insert_En =>
               Expand_Dyn_Insert (Ctxt, Inst, Get_Input_Net (Inst, 3));

            when Id_Rol =>
               --  a rol b == shl (a, b) | shr (a, l - b)  [if b < l]
               Expand_Rol (Ctxt, Inst);
            when Id_Ror =>
               --  a ror b == shr (a, b) | shl (a, l - b)  [if b < l]
               Expand_Ror (Ctxt, Inst);

            when others =>
               null;
         end case;

         Inst := Ninst;
      end loop;
   end Expand_Gates;
end Netlists.Expands;
