--  Expand dyn gates.
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

with Mutils; use Mutils;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Butils; use Netlists.Butils;
with Netlists.Locations; use Netlists.Locations;
with Netlists.Memories; use Netlists.Memories;

package body Netlists.Expands is
   function Count_Nbr_Elements (Addr : Net) return Natural
   is
      N : Net;
      Inst, Inst1 : Instance;
      Res : Natural;
      Max : Uns32;
   begin
      N := Addr;
      Res := 1;
      loop
         Inst := Get_Net_Parent (N);
         case Get_Id (Inst) is
            when Id_Memidx =>
               Inst1 := Inst;
            when Id_Addidx =>
               Inst1 := Get_Net_Parent (Get_Input_Net (Inst, 1));
               pragma Assert (Get_Id (Inst1) = Id_Memidx);

               N := Get_Input_Net (Inst, 0);
            when others =>
               raise Internal_Error;
         end case;

         Max := Get_Param_Uns32 (Inst1, 1);
         pragma Assert (Max /= 0);
         Res := Res * Natural (Max + 1);

         exit when Inst1 = Inst;
      end loop;

      return Res;
   end Count_Nbr_Elements;

   --  IDX is the next index to be fill in ELS.
   --  OFF is offset for extraction from VAL.
   --  ADDR_OFF is the address offset.
   procedure Fill_Els (Ctxt : Context_Acc;
                       Val : Net;
                       Els : Case_Element_Array_Acc;
                       Idx : in out Positive;
                       Addr : Net;
                       Off : in out Uns32;
                       W : Width;
                       Sel : in out Uns64)
   is
      Inst : Instance;
      Sub_Inst : Instance;
   begin
      --  Extract memidx.
      Inst := Get_Net_Parent (Addr);
      case Get_Id (Inst) is
         when Id_Memidx =>
            --  Found.
            Sub_Inst := No_Instance;
            null;
         when Id_Addidx =>
            --  Will recurse.
            Sub_Inst := Get_Net_Parent (Get_Input_Net (Inst, 0));

            Inst := Get_Net_Parent (Get_Input_Net (Inst, 1));
            pragma Assert (Get_Id (Inst) = Id_Memidx);

         when others =>
            raise Internal_Error;
      end case;

      declare
         Step : constant Uns32 := Get_Param_Uns32 (Inst, 0);
         Max : constant Uns32 := Get_Param_Uns32 (Inst, 1);
      begin
         pragma Assert (Max /= 0);

         for I in 0 .. Max loop
            if Sub_Inst /= No_Instance then
               --  recurse.
               raise Internal_Error;
            else
               Els (Idx) := (Sel => Sel,
                             Val => Build_Extract (Ctxt, Val, Off, W));
               Idx := Idx + 1;
               Sel := Sel + 1;
               Off := Off + Step;
            end if;
         end loop;
      end;
   end Fill_Els;

   --  Extract address from memidx/addidx and remove those gates.
   procedure Extract_Address
     (Ctxt : Context_Acc; Addr_Net : Net; Ndims : Natural; Addr : out Net)
   is
      Res_Arr : Net_Array (1 .. Int32 (Ndims));
      P : Int32;
      Inst, Inst1 : Instance;
      Inp : Input;
      N : Net;
   begin
      P := Res_Arr'Last;
      N := Addr_Net;
      loop
         Inst := Get_Net_Parent (N);
         case Get_Id (Inst) is
            when Id_Memidx =>
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

               Remove_Instance (Inst);
            when others =>
               raise Internal_Error;
         end case;

         --  INST1 is a memidx.
         Inp := Get_Input (Inst1, 0);
         Res_Arr (P) := Get_Driver (Inp);
         P := P - 1;

         Disconnect (Inp);
         Remove_Instance (Inst1);

         exit when Inst1 = Inst;
      end loop;
      pragma Assert (P = 0);

      Addr := Build2_Concat (Ctxt, Res_Arr);
   end Extract_Address;

   procedure Expand_Dyn_Extract (Ctxt : Context_Acc; Inst : Instance)
   is
      Val : constant Net := Get_Input_Net (Inst, 0);
      Addr_Net : constant Net := Get_Input_Net (Inst, 1);
      Loc : constant Location_Type := Get_Location (Inst);
      W : constant Width := Get_Width (Get_Output (Inst, 0));
      --  1. compute number of dims, check order.
      Ndims : constant Natural := Count_Memidx (Addr_Net);
      --  2. compute number of cells.
      Nbr_Els : constant Natural := Count_Nbr_Elements (Addr_Net);

      Addr_Len : Uns32;

      Els : Case_Element_Array_Acc;
      Idx : Positive;
      Off : Uns32;
      Sel : Uns64;
      Res : Net;
      Addr : Net;
      Def : Net;
   begin
      --  2. build extract gates
      Els := new Case_Element_Array (1 .. Nbr_Els);
      Idx := 1;
      Off := Get_Param_Uns32 (Inst, 0);
      Sel := 0;
      Fill_Els (Ctxt, Val, Els, Idx, Addr_Net, Off, W, Sel);

      --  3. build mux tree
      Extract_Address (Ctxt, Addr_Net, Ndims, Addr);
      Addr_Len := Uns32 (Clog2 (Uns64 (Nbr_Els)));
      if Get_Width (Addr) > Addr_Len then
         --  Truncate the address.  This is requied so that synth_case doesn't
         --  use default value.
         Addr := Build_Trunc (Ctxt, Id_Utrunc, Addr, Addr_Len);
      end if;
      Def := No_Net;
      Synth_Case (Ctxt, Addr, Els.all, Def, Res, Loc);

      --  4. remove old dyn_extract.
      Disconnect (Get_Input (Inst, 0));
      Disconnect (Get_Input (Inst, 1));
      Redirect_Inputs (Get_Output (Inst, 0), Res);
      Remove_Instance (Inst);

      Free_Case_Element_Array (Els);
   end Expand_Dyn_Extract;

   procedure Expand_Gates (Ctxt : Context_Acc; M : Module)
   is
      Inst : Instance;
   begin
      Inst := Get_First_Instance (M);
      while Inst /= No_Instance loop
         --  Walk all the instances of M:
         case Get_Id (Inst) is
            when Id_Dyn_Extract =>
               Expand_Dyn_Extract (Ctxt, Inst);

            when others =>
               null;
         end case;

         Inst := Get_Next_Instance (Inst);
      end loop;
   end Expand_Gates;
end Netlists.Expands;
