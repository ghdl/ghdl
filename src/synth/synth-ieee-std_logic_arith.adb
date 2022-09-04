--  std_logic_arith
--  Copyright (C) 2022 Tristan Gingold
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

with Elab.Memtype; use Elab.Memtype;

with Synth.Errors; use Synth.Errors;
with Synth.Ieee.Utils; use Synth.Ieee.Utils;
with Synth.Ieee.Std_Logic_1164; use Synth.Ieee.Std_Logic_1164;

package body Synth.Ieee.Std_Logic_Arith is

   function Create_Res_Type (Otyp : Type_Acc; Len : Uns32) return Type_Acc is
   begin
      if Otyp.Abound.Len = Len
        and then Otyp.Abound.Right = 0
        and then Otyp.Abound.Dir = Dir_Downto
        and then not Otyp.Is_Global
      then
         --  Try to reuse the same type as the parameter.
         --  But the result type must be allocated on the expr_pool.
         --  FIXME: is this code ever executed ?
         pragma Assert (Otyp.Abound.Left = Int32 (Len) - 1);
         return Otyp;
      end if;
      return Create_Vec_Type_By_Length (Len, Otyp.Arr_El);
   end Create_Res_Type;

   procedure Fill (Res : Memory_Ptr; Len : Uns32; V : Std_Ulogic) is
   begin
      for I in 1 .. Len loop
         Write_Std_Logic (Res, I - 1, V);
      end loop;
   end Fill;

   procedure Add_Sub_Vec_Vec (Res : Memory_Ptr;
                              Len : Uns32;
                              L, R : Memory_Ptr;
                              Llen, Rlen : Uns32;
                              Lsign, Rsign : Boolean;
                              Is_Sub : Boolean)
   is
      Lb, Rb, Carry : Sl_X01;
      R_Ext, L_Ext : Sl_X01;
   begin

      if Lsign and Llen > 0 then
         --  Extend with the sign bit.
         L_Ext := Sl_To_X01 (Read_Std_Logic (L, 0));
      else
         --  Extend with '0'.
         L_Ext := '0';
      end if;
      if Rsign and Rlen > 0 then
         R_Ext := Sl_To_X01 (Read_Std_Logic (R, 0));
      else
         R_Ext := '0';
      end if;

      if Is_Sub then
         Carry := '1';
      else
         Carry := '0';
      end if;

      for I in 1 .. Len loop
         if I > Llen then
            Lb := L_Ext;
         else
            Lb := Sl_To_X01 (Read_Std_Logic (L, Llen - I));
         end if;
         if I > Rlen then
            Rb := R_Ext;
         else
            Rb := Sl_To_X01 (Read_Std_Logic (R, Rlen - I));
         end if;
         if Is_Sub then
            Rb := Not_Table (Rb);
         end if;

         if Lb = 'X' or Rb = 'X' then
            Fill (Res, Len, 'X');
            exit;
         end if;
         Write_Std_Logic (Res, Len - I, Compute_Sum (Carry, Rb, Lb));
         Carry := Compute_Carry (Carry, Rb, Lb);
      end loop;
   end Add_Sub_Vec_Vec;

   procedure Warn_X (Loc : Location_Type) is
   begin
      Warning_Msg_Synth
        (Loc,
         "There is an 'U'|'X'|'W'|'Z'|'-' in an arithmetic operand, "
           & "the result will be 'X'(es).");
   end Warn_X;

   function Add_Sub_Uns_Sgn_Sgn (L, R : Memtyp;
                                 Is_Sub : Boolean;
                                 Loc : Location_Type) return Memtyp
   is
      Llen : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := R.Typ.Abound.Len;
      Len : constant Uns32 := Uns32'Max (Llen + 1, Rlen);
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Add_Sub_Vec_Vec
        (Res.Mem, Len, L.Mem, R.Mem, Llen, Rlen, False, True, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Uns_Sgn_Sgn;

   function Add_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Uns_Sgn_Sgn (L, R, False, Loc);
   end Add_Uns_Sgn_Sgn;

   function Sub_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Uns_Sgn_Sgn (L, R, True, Loc);
   end Sub_Uns_Sgn_Sgn;

   function Add_Sub_Sgn_Uns_Sgn (L, R : Memtyp;
                                 Is_Sub : Boolean;
                                 Loc : Location_Type) return Memtyp
   is
      Llen : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := R.Typ.Abound.Len;
      Len : constant Uns32 := Uns32'Max (Llen, Rlen + 1);
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Add_Sub_Vec_Vec
        (Res.Mem, Len, L.Mem, R.Mem, Llen, Rlen, True, False, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Sgn_Uns_Sgn;

   function Add_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Sgn_Uns_Sgn (L, R, False, Loc);
   end Add_Sgn_Uns_Sgn;

   function Sub_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Sgn_Uns_Sgn (L, R, True, Loc);
   end Sub_Sgn_Uns_Sgn;

   --  Convert integer V to a std logic vector of length LEN at M.
   procedure To_Unsigned (M : Memory_Ptr; Len : Uns32; V : Uns64)
   is
      R : Uns64;
   begin
      R := V;
      for I in reverse 1 .. Len loop
         Write_Std_Logic (M, I - 1, Uns_To_01 (R and 1));
         R := Shift_Right (R, 1);
      end loop;
   end To_Unsigned;

   procedure To_Signed (M : Memory_Ptr; Len : Uns32; V : Uns64)
   is
      R : Uns64;
   begin
      R := V;
      for I in reverse 1 .. Len loop
         Write_Std_Logic (M, I - 1, Uns_To_01 (R and 1));
         R := Shift_Right_Arithmetic (R, 1);
      end loop;
   end To_Signed;

   function Add_Sub_Vec_Int (L : Memtyp;
                             R : Int64;
                             Signed : Boolean;
                             Is_Sub : Boolean;
                             Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := Uns32'Min (Len, 64);
      Rm : aliased Memory_Array (1 .. Size_Type (Rlen));
      Rmem : constant Memory_Ptr := To_Memory_Ptr (Rm'Address);
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Signed then
         To_Signed (Rmem, Rlen, To_Uns64 (R));
      else
         To_Unsigned (Rmem, Rlen, To_Uns64 (R));
      end if;
      Add_Sub_Vec_Vec
        (Res.Mem, Len, L.Mem, Rmem, Len, Rlen, False, Signed, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Vec_Int;

   function Add_Uns_Int_Uns (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Int (L, R, True, False, Loc);
   end Add_Uns_Int_Uns;

   function Sub_Uns_Int_Uns (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Int (L, R, True, True, Loc);
   end Sub_Uns_Int_Uns;

   function Add_Sgn_Int_Sgn (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Int (L, R, True, False, Loc);
   end Add_Sgn_Int_Sgn;

   function Sub_Sgn_Int_Sgn (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Int (L, R, True, True, Loc);
   end Sub_Sgn_Int_Sgn;

   function Add_Sub_Int_Vec (L : Int64;
                             R : Memtyp;
                             Signed : Boolean;
                             Is_Sub : Boolean;
                             Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := R.Typ.Abound.Len;
      Llen : constant Uns32 := Uns32'Min (Len, 64);
      Lm : aliased Memory_Array (1 .. Size_Type (Llen));
      Lmem : constant Memory_Ptr := To_Memory_Ptr (Lm'Address);
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (R.Typ, Len);
      Res := Create_Memory (Res.Typ);

      if Signed then
         To_Signed (Lmem, Llen, To_Uns64 (L));
      else
         To_Unsigned (Lmem, Llen, To_Uns64 (L));
      end if;
      Add_Sub_Vec_Vec
        (Res.Mem, Len, Lmem, R.Mem, Llen, Len, Signed, False, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Int_Vec;

   function Sub_Int_Uns_Uns (L : Int64; R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Int_Vec (L, R, False, True, Loc);
   end Sub_Int_Uns_Uns;

   function Sub_Int_Sgn_Sgn (L : Int64; R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Int_Vec (L, R, True, True, Loc);
   end Sub_Int_Sgn_Sgn;

   function Add_Sub_Vec_Log (L, R : Memtyp;
                             Is_Sub : Boolean;
                             Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Add_Sub_Vec_Vec
        (Res.Mem, Len, L.Mem, R.Mem, Len, 1, False, False, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Vec_Log;

   function Add_Uns_Log_Uns (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Log (L, R, False, Loc);
   end Add_Uns_Log_Uns;

   function Add_Sgn_Log_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Log (L, R, False, Loc);
   end Add_Sgn_Log_Sgn;

   function Sub_Uns_Log_Uns (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Log (L, R, True, Loc);
   end Sub_Uns_Log_Uns;

   function Sub_Sgn_Log_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Vec_Log (L, R, True, Loc);
   end Sub_Sgn_Log_Sgn;

   function Add_Sub_Log_Vec (L, R : Memtyp;
                             Is_Sub : Boolean;
                             Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := R.Typ.Abound.Len;
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (R.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Add_Sub_Vec_Vec
        (Res.Mem, Len, L.Mem, R.Mem, 1, Len, False, False, Is_Sub);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Add_Sub_Log_Vec;

   function Sub_Log_Uns_Uns (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Log_Vec (L, R, True, Loc);
   end Sub_Log_Uns_Uns;

   function Sub_Log_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Add_Sub_Log_Vec (L, R, True, Loc);
   end Sub_Log_Sgn_Sgn;

   function Neg_Sgn_Sgn (L : Memtyp; Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Neg_Vec (L.Mem, Res.Mem, Len);

      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Neg_Sgn_Sgn;

   function Abs_Sgn_Sgn (L : Memtyp; Loc : Location_Type) return Memtyp
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Abs_Vec (L.Mem, Res.Mem, Len);

      --  Humm, there is no warning if the MSB is '0'.
      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Abs_Sgn_Sgn;

   function Mul_Vec_Vec (L, R : Memtyp;
                         L_Sign, R_Sign : Boolean;
                         Loc : Location_Type) return Memtyp
   is
      Llen : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := R.Typ.Abound.Len;
      Len : constant Uns32 := Llen + Rlen + Boolean'Pos (L_Sign xor R_Sign);
      Res : Memtyp;
   begin
      Res.Typ := Create_Res_Type (L.Typ, Len);
      Res := Create_Memory (Res.Typ);

      Mul_Vec (L.Mem, R.Mem, Llen, Rlen, L_Sign, R_Sign, Res.Mem);
      if Read_Std_Logic (Res.Mem, 0) = 'X' then
         Warn_X (Loc);
      end if;

      return Res;
   end Mul_Vec_Vec;

   function Mul_Uns_Uns_Uns (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Mul_Vec_Vec (L, R, False, False, Loc);
   end Mul_Uns_Uns_Uns;

   function Mul_Sgn_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Mul_Vec_Vec (L, R, True, True, Loc);
   end Mul_Sgn_Sgn_Sgn;

   function Mul_Uns_Sgn_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Mul_Vec_Vec (L, R, False, True, Loc);
   end Mul_Uns_Sgn_Sgn;

   function Mul_Sgn_Uns_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Memtyp is
   begin
      return Mul_Vec_Vec (L, R, True, False, Loc);
   end Mul_Sgn_Uns_Sgn;

   function Has_X (V : Memtyp) return Boolean is
   begin
      for I in 1 .. V.Typ.Abound.Len loop
         if Sl_To_X01 (Read_Std_Logic (V.Mem, I - 1)) = 'X' then
            return True;
         end if;
      end loop;
      return False;
   end Has_X;

   function Compare_Uns_Sgn (L, R : Memtyp; Loc : Location_Type)
                            return Order_Type
   is
      X_In_L : constant Boolean := Has_X (L);
      X_In_R : constant Boolean := Has_X (R);
   begin
      if X_In_L or X_In_R then
         Warn_X (Loc);
         if X_In_L and X_In_R then
            return Equal;
         elsif X_In_L then
            return Less;
         else
            return Greater;
         end if;
      end if;

      return Compare_Vec (L.Mem, R.Mem,
                          L.Typ.Abound.Len, R.Typ.Abound.Len,
                          False, True);
   end Compare_Uns_Sgn;

   function Compare_Uns_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Order_Type
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := Uns32'Min (Len + 1, 64);
      Rm : aliased Memory_Array (1 .. 64);
      Rmem : constant Memory_Ptr := To_Memory_Ptr (Rm'Address);
   begin
      if Has_X (L) then
         Warn_X (Loc);
         return Less;
      end if;

      To_Signed (Rmem, Rlen, To_Uns64 (R));
      return Compare_Vec (L.Mem, Rmem, Len, Rlen, False, True);
   end Compare_Uns_Int;

   function Compare_Sgn_Int (L : Memtyp; R : Int64; Loc : Location_Type)
                            return Order_Type
   is
      Len : constant Uns32 := L.Typ.Abound.Len;
      Rlen : constant Uns32 := Uns32'Min (Len, 64);
      Rm : aliased Memory_Array (1 .. 64);
      Rmem : constant Memory_Ptr := To_Memory_Ptr (Rm'Address);
   begin
      if Has_X (L) then
         Warn_X (Loc);
         return Less;
      end if;

      To_Signed (Rmem, Rlen, To_Uns64 (R));
      return Compare_Vec (L.Mem, Rmem, Len, Rlen, True, True);
   end Compare_Sgn_Int;

end Synth.Ieee.Std_Logic_Arith;
