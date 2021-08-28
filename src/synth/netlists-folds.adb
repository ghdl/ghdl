--  Highler level API to build a netlist - do some optimizations.
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

with Types_Utils; use Types_Utils;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Locations; use Netlists.Locations;

package body Netlists.Folds is

   function Build2_Const_Uns (Ctxt : Context_Acc; Val : Uns64; W : Width)
                             return Net is
   begin
      if Val < 2**32 then
         return Build_Const_UB32 (Ctxt, Uns32 (Val), W);
      else
         pragma Assert (W > 32);
         declare
            Inst : Instance;
         begin
            Inst := Build_Const_Bit (Ctxt, W);
            Set_Param_Uns32 (Inst, 0, Uns32 (Val and 16#ffff_ffff#));
            Set_Param_Uns32 (Inst, 1, Uns32 (Shift_Right (Val, 32)));
            for I in 2 .. (W + 31) / 32 - 1 loop
               Set_Param_Uns32 (Inst, Param_Idx (I), 0);
            end loop;
            return Get_Output (Inst, 0);
         end;
      end if;
   end Build2_Const_Uns;

   function Build2_Const_Vec (Ctxt : Context_Acc; W : Width; V : Uns32_Arr)
                             return Net is
   begin
      if W <= 32 then
         return Build_Const_UB32 (Ctxt, V (V'First), W);
      else
         declare
            Inst : Instance;
         begin
            Inst := Build_Const_Bit (Ctxt, W);
            for I in V'Range loop
               Set_Param_Uns32 (Inst, Param_Idx (I - V'First), V (I));
            end loop;
            return Get_Output (Inst, 0);
         end;
      end if;
   end Build2_Const_Vec;

   function Build2_Const_Int (Ctxt : Context_Acc; Val : Int64; W : Width)
                             return Net is
   begin
      if Val in -2**31 .. 2**31 - 1 then
         return Build_Const_SB32 (Ctxt, Int32 (Val), W);
      else
         pragma Assert (W > 32);
         declare
            V : constant Uns64 := To_Uns64 (Val);
            S : constant Uns32 :=
              Uns32 (Shift_Right_Arithmetic (V, 63) and 16#ffff_ffff#);
            Inst : Instance;
         begin
            Inst := Build_Const_Bit (Ctxt, W);
            Set_Param_Uns32 (Inst, 0, Uns32 (V and 16#ffff_ffff#));
            Set_Param_Uns32 (Inst, 1, Uns32 (Shift_Right (V, 32)));
            for I in 2 .. (W + 31) / 32 - 1 loop
               Set_Param_Uns32 (Inst, Param_Idx (I), S);
            end loop;
            return Get_Output (Inst, 0);
         end;
      end if;
   end Build2_Const_Int;

   function Build2_Concat2 (Ctxt : Context_Acc; L, R : Net) return Net is
   begin
      if Get_Width (L) = 0 then
         return R;
      elsif Get_Width (R) = 0 then
         return L;
      else
         return Build_Concat2 (Ctxt, L, R);
      end if;
   end Build2_Concat2;

   function Build2_Concat (Ctxt : Context_Acc; Els : Net_Array) return Net
   is
      F : constant Int32 := Els'First;
      Len : constant Natural := Els'Length;
      Wd : Width;
      Inst : Instance;
      N : Net;
   begin
      case Len is
         when 0 =>
            raise Internal_Error;
         when 1 =>
            N := Els (F);
         when 2 =>
            N := Build_Concat2 (Ctxt, Els (F + 1), Els (F));
         when 3 =>
            N := Build_Concat3 (Ctxt, Els (F + 2), Els (F + 1), Els (F));
         when 4 =>
            N := Build_Concat4
              (Ctxt, Els (F + 3), Els (F + 2), Els (F + 1), Els (F));
         when 5 .. Natural'Last =>
            --  Compute length.
            Wd := 0;
            for I in Els'Range loop
               Wd := Wd + Get_Width (Els (I));
            end loop;

            N := Build_Concatn (Ctxt, Wd, Uns32 (Len));
            Inst := Get_Net_Parent (N);
            for I in Els'Range loop
               Connect (Get_Input (Inst, Port_Idx (Els'Last - I)), Els (I));
            end loop;
      end case;
      return N;
   end Build2_Concat;

   function Build2_Trunc (Ctxt : Context_Acc;
                          Id : Module_Id;
                          I : Net;
                          W : Width;
                          Loc : Location_Type)
                         return Net
   is
      I_Inst : constant Instance := Get_Net_Parent (I);
      I_Id : constant Module_Id := Get_Id (I_Inst);
      Res : Net;
   begin
      if I_Id not in Extend_Module_Id then
         Res := Build_Trunc (Ctxt, Id, I, W);
         Set_Location (Res, Loc);
         return Res;
      end if;

      --  So there are 3 widths:
      --    W: the width of the result
      --    Iw : the width of the input
      --    Vw : the width of the original value.
      --  And we have:
      --    Iw > W (as we truncate)
      --    Iw > Vw  (as V was extended)
      declare
         Iw : constant Width := Get_Width (I);
         V : constant Net := Get_Input_Net (I_Inst, 0);
         Vw : constant Width := Get_Width (V);
         pragma Assert (Iw > W);
         pragma Assert (Iw > Vw);
      begin
         if W = Vw then
            --  Truncation of an extension with no size change -> no-op.
            return V;
         elsif W < Vw then
            --  At the end, the initial value is truncated.
            Res := Build_Trunc (Ctxt, Id, V, W);
         else
            pragma Assert (W > Vw);
            --  Just extend less.
            Res := Build_Extend (Ctxt, I_Id, V, W);
         end if;
         Set_Location (Res, Loc);
         return Res;
      end;
   end Build2_Trunc;

   function Build2_Uresize (Ctxt : Context_Acc;
                            I : Net;
                            W : Width;
                            Loc : Location_Type)
                           return Net
   is
      Wn : constant Width := Get_Width (I);
      Res : Net;
   begin
      if Wn = W then
         return I;
      else
         if W <= 64 and then Is_Const_Net (I) then
            declare
               V : Uns64;
            begin
               V := Get_Net_Uns64 (I);
               if Wn < W then
                  --  Extend.
                  pragma Assert (Shift_Right (V, Natural (Wn)) = 0);
                  null;
               else
                  --  Truncate
                  V := Shift_Left (V, Natural (64 - Wn));
                  V := Shift_Right (V, Natural (64 - Wn));
               end if;
               Res := Build2_Const_Uns (Ctxt, V, W);
            end;
         else
            if Wn > W then
               return Build2_Trunc (Ctxt, Id_Utrunc, I, W, Loc);
            else
               pragma Assert (Wn < W);
               Res := Build_Extend (Ctxt, Id_Uextend, I, W);
            end if;
         end if;
         Locations.Set_Location (Res, Loc);
         return Res;
      end if;
   end Build2_Uresize;

   function Build2_Sresize (Ctxt : Context_Acc;
                            I : Net;
                            W : Width;
                            Loc : Location_Type)
                           return Net
   is
      Wn : constant Width := Get_Width (I);
      Res : Net;
   begin
      if Wn = W then
         return I;
      else
         if W <= 64 and then Is_Const_Net (I) then
            declare
               V : Uns64;
               Sh : constant Natural := Natural (Width'Min (Wn, W));
            begin
               V := Get_Net_Uns64 (I);
               V := Sext (V, Sh);
               Res := Build2_Const_Int (Ctxt, To_Int64 (V), W);
            end;
         else
            if Wn > W then
               return Build2_Trunc (Ctxt, Id_Strunc, I, W, Loc);
            else
               pragma Assert (Wn < W);
               Res := Build_Extend (Ctxt, Id_Sextend, I, W);
            end if;
         end if;
         Set_Location (Res, Loc);
         return Res;
      end if;
   end Build2_Sresize;

   function Build2_Resize (Ctxt : Context_Acc;
                           I : Net;
                           W : Width;
                           Is_Signed : Boolean;
                           Loc : Location_Type)
                          return Net is
   begin
      if Is_Signed then
         return Build2_Sresize (Ctxt, I, W, Loc);
      else
         return Build2_Uresize (Ctxt, I, W, Loc);
      end if;
   end Build2_Resize;

   function Build2_Extract
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net is
   begin
      if Off = 0 and then W = Get_Width (I) then
         return I;
      else
         return Build_Extract (Ctxt, I, Off, W);
      end if;
   end Build2_Extract;

   function Build2_Extract_Push
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net
   is
      Inst : constant Instance := Get_Net_Parent (I);
      Res : Net;
   begin
      if Off = 0 and then W = Get_Width (I) then
         return I;
      end if;

      case Get_Id (Inst) is
         when Id_Extract =>
            return Build2_Extract_Push
              (Ctxt, Get_Input_Net (Inst, 0),
               Off + Get_Param_Uns32 (Inst, 0), W);
         when Id_Mux2 =>
            Res := Build_Mux2
              (Ctxt,
               Get_Input_Net (Inst, 0),
               Build2_Extract_Push (Ctxt, Get_Input_Net (Inst, 1), Off, W),
               Build2_Extract_Push (Ctxt, Get_Input_Net (Inst, 2), Off, W));
            Set_Location (Res, Get_Location (Inst));
            return Res;
         when others =>
            return Build_Extract (Ctxt, I, Off, W);
      end case;
   end Build2_Extract_Push;

   function Build2_Imp (Ctxt : Context_Acc; A, B : Net; Loc : Location_Type)
                       return Net
   is
      N : Net;
   begin
      N := Build_Monadic (Ctxt, Id_Not, A);
      Set_Location (N, Loc);
      N := Build_Dyadic (Ctxt, Id_Or, N, B);
      Set_Location (N, Loc);
      return N;
   end Build2_Imp;

   function Build2_And (Ctxt : Context_Acc; A, B : Net; Loc : Location_Type)
                       return Net
   is
      pragma Assert (B /= No_Net);
      N : Net;
   begin
      if A = No_Net then
         return B;
      end if;
      N := Build_Dyadic (Ctxt, Id_And, A, B);
      Set_Location (N, Loc);
      return N;
   end Build2_And;

   function Build2_Compare (Ctxt : Context_Acc;
                            Id   : Compare_Module_Id;
                            L, R : Net) return Net
   is
      W : constant Width := Get_Width (L);
   begin
      if W > 0 then
         --  A real gate.
         return Build_Compare (Ctxt, Id, L, R);
      end if;

      pragma Assert (Get_Width (R) = 0);
      case Id is
         when Id_Eq
            | Id_Sle
            | Id_Ule
            | Id_Sge
            | Id_Uge =>
            return Build_Const_UB32 (Ctxt, 1, 1);
         when Id_Ne
            | Id_Slt
            | Id_Ult
            | Id_Sgt
            | Id_Ugt =>
            return Build_Const_UB32 (Ctxt, 0, 1);
      end case;
   end Build2_Compare;

   function Add_Enable_To_Dyn_Insert
     (Ctxt : Context_Acc; Inst : Instance; Sel : Net) return Instance
   is
      In_Mem : constant Input := Get_Input (Inst, 0);
      In_V : constant Input := Get_Input (Inst, 1);
      In_Idx : constant Input := Get_Input (Inst, 2);
      Off : constant Uns32 := Get_Param_Uns32 (Inst, 0);
      Res : Net;
   begin
      Res := Build_Dyn_Insert_En
        (Ctxt, Get_Driver (In_Mem), Get_Driver (In_V), Get_Driver (In_Idx),
         Sel, Off);
      Set_Location (Res, Get_Location (Inst));

      Disconnect (In_Mem);
      Disconnect (In_V);
      Disconnect (In_Idx);
      Redirect_Inputs (Get_Output (Inst, 0), Res);

      Remove_Instance (Inst);

      return Get_Net_Parent (Res);
   end Add_Enable_To_Dyn_Insert;

end Netlists.Folds;
