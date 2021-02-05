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
with Netlists.Butils; use Netlists.Butils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Memories; use Netlists.Memories;
with Netlists.Concats; use Netlists.Concats;
with Netlists.Folds; use Netlists.Folds;

package body Netlists.Expands is
   type Memidx_Array_Type is array (Natural range <>) of Instance;

   --  Extract Memidx from ADDR_NET and return the number of
   --  elements NBR_ELS (which is usually 2**width(ADDR_NET)).
   --  Memidx are ordered from the one with the largest step to the one
   --   with the smallest step.
   procedure Gather_Memidx (Addr_Net : Net;
                            Memidx_Arr : out Memidx_Array_Type;
                            Nbr_Els : out Natural)
   is
      N : Net;
      P : Natural;
      Ninst : Instance;
      Memidx : Instance;
      Max : Uns32;
   begin
      N := Addr_Net;
      Nbr_Els := 1;
      P := Memidx_Arr'Last;
      loop
         Ninst := Get_Net_Parent (N);
         case Get_Id (Ninst) is
            when Id_Memidx =>
               Memidx := Ninst;
            when Id_Addidx =>
               --  Extract memidx.
               Memidx := Get_Net_Parent (Get_Input_Net (Ninst, 1));
               pragma Assert (Get_Id (Memidx) = Id_Memidx);
               N := Get_Input_Net (Ninst, 0);
            when others =>
               raise Internal_Error;
         end case;

         Memidx_Arr (P) := Memidx;

         --  Check memidx are ordered by decreasing step.
         pragma Assert
           (P = Memidx_Arr'Last
              or else (Get_Param_Uns32 (Memidx, 0)
                         >= Get_Param_Uns32 (Memidx_Arr (P + 1), 0)));

         P := P - 1;

         Max := Get_Param_Uns32 (Memidx, 1);
         Nbr_Els := Nbr_Els * Natural (Max + 1);

         exit when Memidx = Ninst;
      end loop;
   end Gather_Memidx;

   procedure Remove_Memidx (Memidx_Arr : Memidx_Array_Type)
   is
      Inst : Instance;
   begin
      for I in Memidx_Arr'Range loop
         Inst := Memidx_Arr (I);
         if not Is_Connected (Get_Output (Inst, 0)) then
            --  A memidx can be shared between several insert/extract.
            --  FIXME: what about memidx ?
            Disconnect (Get_Input (Inst, 0));
            Remove_Instance (Inst);
         end if;
      end loop;
   end Remove_Memidx;

   --  IDX is the next index to be fill in ELS.
   --  OFF is offset for extraction from VAL.
   --  ADDR_OFF is the address offset.
   procedure Fill_Els (Ctxt : Context_Acc;
                       Memidx_Arr : Memidx_Array_Type;
                       Arr_Idx : Natural;
                       Val : Net;
                       Els : Case_Element_Array_Acc;
                       Idx : in out Positive;
                       Addr : Net;
                       Init_Off : Uns32;
                       W : Width;
                       Sel : in out Uns64)
   is
      Inst : constant Instance := Memidx_Arr (Arr_Idx);
      Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
      Max : constant Uns32 := Get_Param_Uns32 (Inst, 1);
      Off : Uns32;
   begin
      Off := Init_Off;
      for I in 0 .. Max loop
         if Arr_Idx < Memidx_Arr'Last then
            --  Recurse.
            Fill_Els (Ctxt, Memidx_Arr, Arr_Idx + 1,
                      Val, Els, Idx, Addr, Off, W, Sel);
         else
            Els (Idx) := (Sel => Sel,
                          Val => Build_Extract (Ctxt, Val, Off, W));
            Idx := Idx + 1;
            Sel := Sel + 1;
         end if;
         Off := Off + Step;
      end loop;
   end Fill_Els;

   --  Extract address from memidx/addidx and disconnect those gates.
   procedure Extract_Address
     (Ctxt : Context_Acc; Addr_Net : Net; Ndims : Natural; Addr : out Net)
   is
      Res_Arr : Net_Array (1 .. Int32 (Ndims));
      P : Int32;
      Inst, Inst1 : Instance;
      Inp : Input;
      N : Net;
   begin
      P := 1;
      N := Addr_Net;
      loop
         Inst := Get_Net_Parent (N);
         case Get_Id (Inst) is
            when Id_Memidx =>
               --  Must be the last one!
               Inst1 := Inst;
            when Id_Addidx =>
               --  Extract memidx.
               Inp := Get_Input (Inst, 1);
               Inst1 := Get_Net_Parent (Get_Driver (Inp));
               pragma Assert (Get_Id (Inst1) = Id_Memidx);
               Disconnect (Inp);

               --  Extract next.
               Inp := Get_Input (Inst, 0);
               N := Get_Driver (Inp);
               Disconnect (Inp);

               --  Remove the Addidx.
               Remove_Instance (Inst);

            when others =>
               raise Internal_Error;
         end case;

         --  INST1 is a memidx.
         Inp := Get_Input (Inst1, 0);
         Res_Arr (P) := Get_Driver (Inp);
         P := P + 1;

         exit when Inst1 = Inst;
      end loop;
      pragma Assert (P = Res_Arr'Last + 1);

      Addr := Build2_Concat (Ctxt, Res_Arr);
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

   procedure Expand_Dyn_Extract (Ctxt : Context_Acc; Inst : Instance)
   is
      Val : constant Net := Get_Input_Net (Inst, 0);
      Addr_Net : constant Net := Get_Input_Net (Inst, 1);
      Loc : constant Location_Type := Get_Location (Inst);
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      --  1. compute number of dims, check order.
      Ndims : constant Natural := Count_Memidx (Addr_Net);
      Nbr_Els : Natural;

      Memidx_Arr : Memidx_Array_Type (1 .. Ndims);

      Els : Case_Element_Array_Acc;
      Res : Net;
      Addr : Net;
      Def : Net;
   begin
      --  1.1  Fill memidx_arr.
      --  2. compute number of cells.
      Gather_Memidx (Addr_Net, Memidx_Arr, Nbr_Els);

      --  2. build extract gates
      Els := new Case_Element_Array (1 .. Nbr_Els);
      declare
         Idx : Positive;
         Off : Uns32;
         Sel : Uns64;
      begin
         Idx := 1;
         Off := Get_Param_Uns32 (Inst, 0);
         Sel := 0;
         Fill_Els (Ctxt, Memidx_Arr, 1, Val, Els, Idx, Addr_Net, Off, W, Sel);
      end;

      --  3. build mux tree
      Disconnect (Get_Input (Inst, 1));
      Extract_Address (Ctxt, Addr_Net, Ndims, Addr);
      Truncate_Address (Ctxt, Addr, Nbr_Els);
      Def := No_Net;
      Synth_Case (Ctxt, Addr, Els.all, Def, Res, Loc);

      --  4. remove old dyn_extract.
      Disconnect (Get_Input (Inst, 0));
      Redirect_Inputs (Get_Output (Inst, 0), Res);
      Remove_Instance (Inst);

      Remove_Memidx (Memidx_Arr);

      Free_Case_Element_Array (Els);
   end Expand_Dyn_Extract;

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
                             Memidx_Arr : Memidx_Array_Type;
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
         Append (Concat, Build_Extract (Ctxt, Mem, 0, Off));
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
            Append (Concat, Build_Extract (Ctxt, Prev_Net, 0, Step));
            V := Build_Concat2
              (Ctxt,
               Build_Extract (Ctxt, Mem, Next_Off, Step),
               Build_Extract (Ctxt, Prev_Net, Step, Dat_W - Step));
         else
            --  No overlap.
            if Prev_Net /= No_Net then
               Append (Concat, Prev_Net);
            end if;

            if Next_Off < Off then
               --  But there is a gap.
               Append (Concat, Build_Extract (Ctxt, Mem, Next_Off,
                                              Off - Next_Off));
            end if;
            V := Build_Extract (Ctxt, Mem, Off, Dat_W);
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
         for I in reverse Memidx_Arr'Range loop
            declare
               C : Count_Type renames Count (I);
            begin
               C.Val := C.Val + C.Step;
               Off := Off + C.Step;
               exit when C.Val <= C.Max * C.Step;
               if I = Memidx_Arr'First then
                  --  End.
                  Append (Concat, Prev_Net);
                  Off := Next_Off;
                  return;
               end if;
               Count (I).Val := 0;
               Off := Count (I - 1).Val;
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
      Addr_Net : constant Net := Get_Input_Net (Inst, 2);
      O : constant Net := Get_Output (Inst, 0);
      O_W : constant Width := Get_Width (O);
      --  1. compute number of dims, check order.
      Ndims : constant Natural := Count_Memidx (Addr_Net);
      Nbr_Els : Natural;

      Memidx_Arr : Memidx_Array_Type (1 .. Ndims);

      Net_Arr : Net_Array_Acc;

      Addr : Net;

      Concat : Concat_Type;
      Res : Net;
   begin
      Gather_Memidx (Addr_Net, Memidx_Arr, Nbr_Els);

      --  Generate decoder.
      Net_Arr := new Net_Array(0 .. Int32 (Nbr_Els - 1));
      Disconnect (Get_Input (Inst, 2));  --  Disconnect address
      Extract_Address (Ctxt, Addr_Net, Ndims, Addr);
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
            Append (Concat, Build_Extract (Ctxt, Mem, Off, O_W - Off));
         end if;
      end;
      Build (Ctxt, Concat, Res);
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

      Remove_Memidx (Memidx_Arr);
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
