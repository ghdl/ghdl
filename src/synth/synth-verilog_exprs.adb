--  Expressions synthesis for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates; use Netlists.Gates;
with Netlists.Folds; use Netlists.Folds;

with Synth.Verilog_Sources; use Synth.Verilog_Sources;
with Synth.Verilog_Environment; use Synth.Verilog_Environment.Env;

with Verilog.Errors; use Verilog.Errors;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Standard;
with Verilog.Executions; use Verilog.Executions;
with Verilog.Sem_Utils;
with Verilog.Allocates;

package body Synth.Verilog_Exprs is

   Bit_0 : constant Logic_Type := V_0;
   Bit_1 : constant Logic_Type := V_1;
   Bit_X : constant Logic_Type := V_X;

   function Read_Logic (Mem : Memory_Ptr) return Logic_Type is
   begin
      return To_Logic_Ptr (Mem).all;
   end Read_Logic;

   function Synth_Extract (Inst : Synth_Instance_Acc;
                           Val : Valtyp;
                           Off : Uns32;
                           Res_Typ : Node) return Valtyp
   is
      Wd : constant Width_Type := Get_Type_Width (Res_Typ);
   begin
      case Val.Kind is
         when Value_Net
            | Value_Wire =>
            declare
               Ctxt : constant Context_Acc := Get_Build (Inst);
               N : Net;
            begin
               --  Extract sub-value from the expression.
               N := Get_Net (Ctxt, Val);
               N := Build2_Extract (Ctxt, N, Off, Uns32 (Wd));
               return Create_Value_Net (N, Res_Typ);
            end;
         when Value_Memory =>
            declare
               Res : Valtyp;
            begin
               Res := (Kind => Value_Memory, Typ => Res_Typ, Mem => null);
               Res.Mem := Allocate_Memory (Inst, Res_Typ);
               case Get_Kind (Res_Typ) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Part_Extract
                       (To_Logvec_Ptr (Res.Mem),
                        0, Wd,
                        To_Logvec_Ptr (Val.Mem),
                        Bit_Offset (Off), Wd + Width_Type (Off));
                  when others =>
                     Error_Kind ("value_extract", Res_Typ);
               end case;
               return Res;
            end;
         when Value_None =>
            raise Internal_Error;
      end case;
   end Synth_Extract;

   function Get_Type_Bitwidth (N : Node) return Width is
   begin
      case Get_Kind (N) is
         when N_Logic_Type =>
            return 1;
         when N_Typedef =>
            return Get_Type_Bitwidth (Get_Type_Data_Type (N));
         when N_Log_Packed_Array_Cst =>
            return Uns32 (Get_Type_Width (N));
         when N_Array_Cst =>
            return Uns32 (Verilog.Sem_Utils.Compute_Length (N))
              * Get_Type_Bitwidth (Get_Type_Element_Type (N));
         when others =>
            Verilog.Errors.Error_Kind ("get_type_bitwidth", N);
      end case;
   end Get_Type_Bitwidth;

   procedure Memory2logvec (Mem : Memory_Ptr;
                            Typ : Node;
                            Vec : Logvec_Ptr;
                            Vec_Off : in out Bit_Offset;
                            Has_Zx : in out Boolean) is
   begin
      case Get_Kind (Typ) is
         when N_Logic_Type =>
            declare
               L : constant Logic_Type := To_Logic_Ptr (Mem).all;
               Idx : constant Digit_Index := Digit_Index (Vec_Off / 32);
               Pos : constant Natural := Natural (Vec_Off mod 32);
               Va : Uns32;
               Zx : Uns32;
            begin
               Zx := Boolean'Pos (L = V_Z or L = V_X);
               Va := Boolean'Pos (L = V_1 or L = V_X);
               Has_Zx := Has_Zx or (Zx /= 0);
               Va := Shift_Left (Va, Pos);
               Zx := Shift_Left (Zx, Pos);
               Vec (Idx).Val := Vec (Idx).Val or Va;
               Vec (Idx).Zx := Vec (Idx).Zx or Zx;
               Vec_Off := Vec_Off + 1;
            end;
         when N_Log_Packed_Array_Cst =>
            declare
               W : constant Width_Type := Get_Type_Width (Typ);
               Mem_Ptr : constant Logvec_Ptr := To_Logvec_Ptr (Mem);
               Change : Boolean;
            begin
               Compute_Part_Insert (Vec, Vec_Off, Mem_Ptr, 0, W, Change);
               Vec_Off := Vec_Off + Bit_Offset (W);
               if not Has_Zx then
                  for I in 1 .. Digit_Index (W / Digit_Width) loop
                     if Mem_Ptr (I - 1).Zx /= 0 then
                        Has_Zx := True;
                        exit;
                     end if;
                  end loop;
               end if;
               if not Has_Zx and then (W mod Digit_Width) /= 0 then
                  if Shift_Left (Mem_Ptr (Digit_Index (W / Digit_Width)).Zx,
                                 32 - Natural (W mod Digit_Width)) /= 0
                  then
                     Has_Zx := True;
                  end if;
               end if;
            end;
         when N_Array_Cst =>
            declare
               El_Typ : constant Node := Get_Type_Element_Type (Typ);
               El_Sz : constant Size_Type :=
                 Size_Type (Verilog.Allocates.Get_Storage_Size (El_Typ));
               Arr_Len : constant Int32 :=
                 Verilog.Sem_Utils.Compute_Length (Typ);
               Mem_Off : Size_Type;
            begin
               Mem_Off := 0;
               for I in 1 .. Arr_Len loop
                  Memory2logvec (Mem + Mem_Off, El_Typ, Vec, Vec_Off, Has_Zx);
                  Mem_Off := Mem_Off + El_Sz;
               end loop;
            end;
         when others =>
            Error_Kind ("memory2logvec", Typ);
      end case;
   end Memory2logvec;

   function Logvec2net (Ctxt : Context_Acc;
                        Vec : Logvec_Array;
                        W : Width;
                        Has_Zx : Boolean) return Net is
   begin
      pragma Assert (W > 0);
      if W <= 32 then
         if Vec (0).Zx = 0 then
            pragma Assert (not Has_Zx);
            return Build_Const_UB32 (Ctxt, Vec (0).Val, W);
         else
            pragma Assert (Has_Zx);
            declare
               All1 : constant Uns32 := Shift_Right (not 0, Natural (32 - W));
            begin
               if Vec (0).Zx = All1 then
                  if Vec (0).Val = All1 then
                     return Build_Const_X (Ctxt, W);
                  elsif Vec (0).Val = 0 then
                     return Build_Const_Z (Ctxt, W);
                  end if;
               end if;
               return Build_Const_UL32 (Ctxt, Vec (0).Val, Vec (0).Zx, W);
            end;
         end if;
      else
         declare
            Last : constant Digit_Index := To_Last (Width_Type (W));
            Inst : Instance;
            Is_0 : Boolean;
            Is_X : Boolean;
            Is_Z : Boolean;
            Has_Zx : Boolean;
         begin
            Is_0 := True;
            Is_X := True;
            Is_Z := True;
            Has_Zx := False;
            for I in 1 .. Last loop
               Is_0 := Is_0 and Vec (I) = (0, 0);
               Is_X := Is_X and Vec (I) = Logic_32_X;
               Is_Z := Is_Z and Vec (I) = Logic_32_X;
               Has_Zx := Has_Zx or Vec (I).Zx /= 0;
               exit when not (Is_0 or Is_X or Is_Z) and Has_Zx;
            end loop;
            if Is_0 then
               return Build_Const_UB32 (Ctxt, 0, W);
            elsif Is_X then
               return Build_Const_X (Ctxt, W);
            elsif Is_Z then
               return Build_Const_Z (Ctxt, W);
            elsif not Has_Zx then
               Inst := Build_Const_Bit (Ctxt, W);
               for I in 0 .. Last loop
                  Set_Param_Uns32 (Inst, Param_Idx (I), Vec (I).Val);
               end loop;
               return Get_Output (Inst, 0);
            else
               Inst := Build_Const_Log (Ctxt, W);
               for I in 0 .. Last loop
                  Set_Param_Uns32 (Inst, Param_Idx (2 * I), Vec (I).Val);
                  Set_Param_Uns32 (Inst, Param_Idx (2 * I + 1), Vec (I).Zx);
               end loop;
               return Get_Output (Inst, 0);
            end if;
         end;
      end if;
   end Logvec2net;

   type Logvec_Array_Acc is access Logvec_Array;
   procedure Free_Logvec_Array is new Ada.Unchecked_Deallocation
     (Logvec_Array, Logvec_Array_Acc);

   function Memory2net (Ctxt : Context_Acc; Mem : Memory_Ptr; Typ : Node)
                       return Net
   is
      W : constant Width := Get_Type_Bitwidth (Typ);
      Nd : constant Digit_Index := Digit_Index ((W + 31) / 32);
      Has_Zx : Boolean;
      Res : Net;
      Vec_Off : Bit_Offset;
   begin
      Vec_Off := 0;
      Has_Zx := False;
      if Nd > 64 then
         declare
            Vecp : Logvec_Array_Acc;
         begin
            Vecp := new Logvec_Array'(0 .. Nd - 1 => (0, 0));
            Memory2logvec (Mem, Typ,
                           To_Logvec_Ptr (Vecp.all'Address), Vec_Off, Has_Zx);
            pragma Assert (Vec_Off = Bit_Offset (W));
            Res := Logvec2net (Ctxt, Vecp.all, W, Has_Zx);
            Free_Logvec_Array (Vecp);
            return Res;
         end;
      else
         declare
            Vec : Logvec_Array (0 .. Nd - 1) := (others => (0, 0));
         begin
            Memory2logvec (Mem, Typ,
                           To_Logvec_Ptr (Vec'Address), Vec_Off, Has_Zx);
            pragma Assert (Vec_Off = Bit_Offset (W));
            Res := Logvec2net (Ctxt, Vec, W, Has_Zx);
            return Res;
         end;
      end if;
   end Memory2net;

   function Synth_Binary_Op (Inst : Synth_Instance_Acc; N : Node)
                            return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Lv, Rv : Valtyp;
   begin
      Lv := Synth_Expression (Inst, Get_Left (N));
      Rv := Synth_Expression (Inst, Get_Right (N));

      if Is_Static (Lv) and then Is_Static (Rv) then
         declare
            Rtyp : constant Node := Get_Expr_Type (N);
            Res : Valtyp;
         begin
            --  Allocate memory (for the result)
            Res := (Kind => Value_Memory, Typ => Rtyp, Mem => null);
            Res.Mem := Allocate_Memory (Inst, Rtyp);
            Verilog.Executions.Execute_Binary_Expression
              (N,
               To_Address (Lv.Mem), To_Address (Rv.Mem),
               To_Address (Res.Mem));
            return Res;
         end;
      end if;

      declare
         Ln : Net;
         Rn : Net;
         Res : Net;
      begin
         Ln := Get_Net (Ctxt, Lv);
         Rn := Get_Net (Ctxt, Rv);

         case Get_Binary_Op (N) is
            when Binop_Bit_Or =>
               Res := Build_Dyadic (Ctxt, Id_Or, Ln, Rn);
            when Binop_Bit_And =>
               Res := Build_Dyadic (Ctxt, Id_And, Ln, Rn);
            when Binop_Bit_Xor =>
               Res := Build_Dyadic (Ctxt, Id_Xor, Ln, Rn);

            when Binop_Add =>
               Res := Build_Dyadic (Ctxt, Id_Add, Ln, Rn);
            when Binop_Sub =>
               Res := Build_Dyadic (Ctxt, Id_Sub, Ln, Rn);
            when Binop_Smul =>
               Res := Build_Dyadic (Ctxt, Id_Smul, Ln, Rn);

            when Binop_Log_Ne =>
               Res := Build_Compare (Ctxt, Id_Ne, Ln, Rn);
            when Binop_Log_Eq =>
               --  Reduce 'N == 1'b0' to '!N'
               --  That's the canonical form for clocks.
               if Get_Width (Ln) = 1
                 and then Is_Const_Net (Rn)
                 and then Get_Net_Uns64 (Rn) = 0
               then
                  Res := Build_Monadic (Ctxt, Id_Not, Ln);
               else
                  Res := Build_Compare (Ctxt, Id_Eq, Ln, Rn);
               end if;
            when Binop_Slt =>
               Res := Build_Compare (Ctxt, Id_Slt, Ln, Rn);
            when Binop_Ult =>
               Res := Build_Compare (Ctxt, Id_Ult, Ln, Rn);
            when Binop_Uge =>
               Res := Build_Compare (Ctxt, Id_Uge, Ln, Rn);

            when Binop_Left_Lshift =>
               Res := Build_Shift_Rotate (Ctxt, Id_Lsl, Ln, Rn);
            when Binop_Left_Ashift =>
               Res := Build_Shift_Rotate (Ctxt, Id_Lsl, Ln, Rn);
            when Binop_Right_Ashift =>
               Res := Build_Shift_Rotate (Ctxt, Id_Asr, Ln, Rn);
            when Binop_Right_Lshift =>
               Res := Build_Shift_Rotate (Ctxt, Id_Lsr, Ln, Rn);

            when others =>
               Error_Kind ("synth_binary_op: "
                             & Binary_Ops'Image (Get_Binary_Op (N)), N);
         end case;

         Set_Location (Res, N);

         return Create_Value_Net (Res, Get_Expr_Type (N));
      end;
   end Synth_Binary_Op;

   function Synth_Unary_Op (Inst : Synth_Instance_Acc; N : Node)
                            return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      V : Valtyp;
   begin
      V := Synth_Expression (Inst, Get_Expression (N));

      if Is_Static (V) then
         declare
            Rtyp : constant Node := Get_Expr_Type (N);
            Res : Valtyp;
         begin
            --  Allocate memory (for the result)
            Res := (Kind => Value_Memory, Typ => Rtyp, Mem => null);
            Res.Mem := Allocate_Memory (Inst, Rtyp);
            Verilog.Executions.Execute_Unary_Expression
              (N, To_Address (V.Mem), To_Address (Res.Mem));
            return Res;
         end;
      end if;

      declare
         Vn : Net;
         Res : Net;
      begin
         Vn := Get_Net (Ctxt, V);

         case Get_Unary_Op (N) is
            when Unop_Plus =>
               return V;
            when Unop_Bit_Neg =>
               Res := Build_Monadic (Ctxt, Id_Not, Vn);
            when Unop_Logic_Neg =>
               declare
                  W : constant Uns32 := Get_Width (Vn);
                  N0 : Net;
               begin
                  if W > 1 then
                     N0 := Build_Const_UB32 (Ctxt, 0, W);
                     Res := Build_Compare (Ctxt, Id_Eq, Vn, N0);
                  else
                     Res := Build_Monadic (Ctxt, Id_Not, Vn);
                  end if;
               end;
            when Unop_Red_Or =>
               Res := Build_Reduce (Ctxt, Id_Red_Or, Vn);
            when Unop_Red_Nor =>
               Res := Build_Reduce (Ctxt, Id_Red_Or, Vn);
               Res := Build_Monadic (Ctxt, Id_Not, Res);
            when Unop_Red_And =>
               Res := Build_Reduce (Ctxt, Id_Red_And, Vn);
            when Unop_Red_Xor =>
               Res := Build_Reduce (Ctxt, Id_Red_Xor, Vn);
            when others =>
               Error_Kind ("synth_unary_op: "
                             & Unary_Ops'Image (Get_Unary_Op (N)), N);
         end case;
         Set_Location (Res, N);

         return Create_Value_Net (Res, Get_Expr_Type (N));
      end;
   end Synth_Unary_Op;

   function Synth_Short_Circuit_Op (Inst : Synth_Instance_Acc; N : Node)
                                   return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Left : constant Node := Get_Left (N);
      Lv, Rv : Valtyp;
   begin
      --  IEEE 1800-2017 11.4.7 Logical operators
      --  - The first operand expression shall always be evaluated.
      Lv := Synth_Condition (Inst, Left);
      if Is_Static (Lv) then
         declare
            Val : Tri_State_Type;
         begin
            Val := Execute_Condition (To_Address (Lv.Mem), Left);
            case Get_Binary_Op (N) is
               when Binop_Logic_And =>
                  --  - For &&, if the first operand value is logically false,
                  --    then the second operand shall not be evaluated.
                  if Val = False then
                     return Lv;
                  end if;
               when Binop_Logic_Or =>
                  --  - For ||, if the first operand value is logically true,
                  --    then the second operand shall not be evaluated.
                  if Val = True then
                     return Lv;
                  end if;
               when others =>
                  raise Internal_Error;
            end case;
            Rv := Synth_Condition (Inst, Get_Right (N));
            return Rv;
         end;
      end if;

      Rv := Synth_Condition (Inst, Get_Right (N));

      declare
         Ln : Net;
         Rn : Net;
         Res : Net;
      begin
         Ln := Get_Net (Ctxt, Lv);
         Rn := Get_Net (Ctxt, Rv);
         case Get_Binary_Op (N) is
            when Binop_Logic_Or =>
               Res := Build_Dyadic (Ctxt, Id_Or, Ln, Rn);
            when Binop_Logic_And =>
               Res := Build_Dyadic (Ctxt, Id_And, Ln, Rn);
            when others =>
               Error_Kind ("synth_short_circuit_op: "
                             & Binary_Ops'Image (Get_Binary_Op (N)), N);
         end case;
         Set_Location (Res, N);

         return Create_Value_Net (Res, Get_Expr_Type (N));
      end;
   end Synth_Short_Circuit_Op;

   function Synth_Conversion (Inst : Synth_Instance_Acc; N : Node)
                             return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Rtyp : constant Node := Get_Expr_Type (N);
      Val : Valtyp;
      Nv : Net;
      Res : Net;
   begin
      Val := Synth_Expression (Inst, Get_Expression (N));

      if Is_Static (Val) then
         declare
            Res : Valtyp;
         begin
            --  Allocate memory (for the result)
            Res := (Kind => Value_Memory, Typ => Rtyp, Mem => null);
            Res.Mem := Allocate_Memory (Inst, Rtyp);
            Compute_Conversion (To_Address (Res.Mem), N, To_Address (Val.Mem));
            return Res;
         end;
      end if;

      Nv := Get_Net (Ctxt, Val);
      case Get_Conversion_Op (N) is
         when Convop_Lv_Zext
            | Convop_Log_Ulv
            | Convop_Lv_Trunc =>
            Res := Build2_Uresize
              (Ctxt, Nv, Get_Type_Bitwidth (Rtyp), Get_Location (N));
         when Convop_Lv_Sext =>
            Res := Build2_Sresize
              (Ctxt, Nv, Get_Type_Bitwidth (Rtyp), Get_Location (N));
         when Convop_Lv_Log =>
            Res := Build2_Extract (Ctxt, Nv, 0, 1);
         when Convop_Lv_Nop =>
            Res := Nv;
         when others =>
            Error_Kind ("synth_conversion - "
                          & Conv_Ops'Image (Get_Conversion_Op (N)), N);
      end case;
      return Create_Value_Net (Res, Rtyp);
   end Synth_Conversion;

   function Synth_Condition (Inst : Synth_Instance_Acc; N : Node)
                            return Valtyp
   is
      Res_Typ : constant Node := Verilog.Standard.Unsigned_Logic_Type;
      Res : Valtyp;
   begin
      Res := Synth_Expression (Inst, N);
      if Is_Static (Res) then
         case Execute_Condition (To_Address (Res.Mem), N) is
            when True =>
               Res.Mem := To_Memory_Ptr (Bit_1'Address);
            when False =>
               Res.Mem := To_Memory_Ptr (Bit_0'Address);
            when Unknown =>
               Res.Mem := To_Memory_Ptr (Bit_X'Address);
         end case;
         Res.Typ := Res_Typ;
         return Res;
      else
         --  Convert it to single bit
         declare
            Ctxt : constant Context_Acc := Get_Build (Inst);
            W : constant Width_Type := Get_Type_Width (Get_Expr_Type (N));
            N0, Ncmp : Net;
         begin
            if W = 1 then
               --  Nothing to do.
               return Res;
            end if;
            N0 := Build_Const_UB32 (Ctxt, 0, Uns32 (W));
            Ncmp := Build_Compare (Ctxt, Id_Ne, Get_Net (Ctxt, Res), N0);
            Set_Location (Ncmp, N);
            return Create_Value_Net (Ncmp, Res_Typ);
         end;
      end if;
   end Synth_Condition;

   function Synth_Cond_Op (Inst : Synth_Instance_Acc; N : Node)
                          return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Val : Valtyp;
      Tv, Fv : Valtyp;
      Res : Net;
   begin
      Val := Synth_Condition (Inst, Get_Condition (N));

      if Is_Static (Val) then
         case Execute_Condition (To_Address (Val.Mem), N) is
            when True =>
               return Synth_Expression (Inst, Get_Cond_True (N));
            when False =>
               return Synth_Expression (Inst, Get_Cond_False (N));
            when Unknown =>
               Tv := Synth_Expression (Inst, Get_Cond_True (N));
               Fv := Synth_Expression (Inst, Get_Cond_False (N));
               raise Internal_Error;
         end case;
      else
         Tv := Synth_Expression (Inst, Get_Cond_True (N));
         Fv := Synth_Expression (Inst, Get_Cond_False (N));

         Res := Build_Mux2 (Ctxt, Get_Net (Ctxt, Val),
                            Get_Net (Ctxt, Fv), Get_Net (Ctxt, Tv));
         Set_Location (Res, N);
         return Create_Value_Net (Res, Get_Expr_Type (N));
      end if;
   end Synth_Cond_Op;

   procedure Synth_Static_Concatenation (Res : Valtyp;
                                         Wd : Width_Type;
                                         N : Node;
                                         Arr : Valtyp_Array_Acc)
   is
      Off : Width_Type;
      Expr : Node;
   begin
      Off := Wd;
      Expr := Get_Expressions (N);
      for I in Arr'Range loop
         declare
            E : constant Node := Get_Expression (Expr);
            Etype : constant Node := Get_Expr_Type (E);
            Ewidth : constant Width_Type := Get_Type_Width (Etype);
            Change : Boolean;
         begin
            pragma Assert (Off >= Ewidth);
            Off := Off - Ewidth;
            case Get_Kind (Etype) is
               when N_Log_Packed_Array_Cst =>
                  Compute_Part_Insert
                    (To_Logvec_Ptr (Res.Mem), Bit_Offset (Off),
                     To_Logvec_Ptr (Arr (I).Mem), 0, Ewidth, Change);
               when N_Logic_Type =>
                  Compute_Log_Insert
                    (To_Logvec_Ptr (Res.Mem), Bit_Offset (Off),
                     To_Logic_Ptr (Arr (I).Mem).all, Change);
               when others =>
                  Error_Kind ("synth_static_concatenation", Etype);
            end case;
         end;
         Expr := Get_Chain (Expr);
      end loop;
      pragma Assert (Off = 0);
   end Synth_Static_Concatenation;

   function Synth_Dynamic_Concatenation (Inst : Synth_Instance_Acc;
                                         Arr : Valtyp_Array_Acc) return Net
   is
      pragma Assert (Arr'First = 1);
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Net_Arr : Net_Array_Acc;
      Res_Net : Net;
   begin
      Net_Arr := new Net_Array (1 .. Arr'Last);
      for I in Arr'Range loop
         Net_Arr (Arr'Last - I + 1) := Get_Net (Ctxt, Arr (I));
      end loop;

      Res_Net := Build2_Concat (Ctxt, Net_Arr.all);

      Free_Net_Array (Net_Arr);

      return Res_Net;
   end Synth_Dynamic_Concatenation;

   --  Pre-compute values.  ARR is filled from left to right.
   procedure Synth_Concatenation_Precompute (Inst : Synth_Instance_Acc;
                                             N : Node;
                                             Arr : out Valtyp_Array_Acc;
                                             Static : out Boolean)
   is
      use Verilog.Sem_Utils;
      Chain : constant Node := Get_Expressions (N);
      Num : Nat32;
      Expr : Node;
      Val : Node;
   begin
      --  1. Count number of concats
      Num := 0;
      Expr := Chain;
      while Expr /= Null_Node loop
         if not Is_Null_Replication (Get_Expression (Expr)) then
            Num := Num + 1;
         end if;
         Expr := Get_Chain (Expr);
      end loop;

      --  2. Allocate.
      Arr := new Valtyp_Array (1 .. Num);

      --  2. Synth expressions, check staticness
      Static := True;
      Num := 0;
      Expr := Chain;
      while Expr /= Null_Node loop
         Val := Get_Expression (Expr);
         if not Is_Null_Replication (Val) then
            Num := Num + 1;
            Arr (Num) := Synth_Expression (Inst, Val);
            if Static and then not Is_Static (Arr (Num)) then
               Static := False;
            end if;
         end if;
         Expr := Get_Chain (Expr);
      end loop;
      pragma Assert (Num = Arr'Last);
   end Synth_Concatenation_Precompute;

   function Synth_Concatenation (Inst : Synth_Instance_Acc; N : Node)
                                return Valtyp
   is
      Rtyp : constant Node := Get_Expr_Type (N);
      Res : Valtyp;
      Is_Static : Boolean;
      Arr : Valtyp_Array_Acc;
   begin
      Synth_Concatenation_Precompute (Inst, N, Arr, Is_Static);

      --  3. Concat
      if Is_Static then
         --  Allocate memory (for the result)
         Res := (Kind => Value_Memory, Typ => Rtyp, Mem => null);
         Res.Mem := Allocate_Memory (Inst, Rtyp);
         Synth_Static_Concatenation (Res, Get_Type_Width (Rtyp), N, Arr);
      else
         declare
            Res_Net : Net;
         begin
            Res_Net := Synth_Dynamic_Concatenation (Inst, Arr);
            Res := Create_Value_Net (Res_Net, Rtyp);
         end;
      end if;

      Free_Valtyp_Array (Arr);
      return Res;
   end Synth_Concatenation;

   function Synth_Replication_Cst (Inst : Synth_Instance_Acc; N : Node)
                                  return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Count : constant Int32 := Get_Replication_Cst (N);
      Rtyp : constant Node := Get_Expr_Type (N);
      Res : Valtyp;
      Static : Boolean;
      Arr : Valtyp_Array_Acc;
   begin
      Synth_Concatenation_Precompute (Inst, N, Arr, Static);

      --  3. Concat
      if Static then
         --  Allocate memory (for the result)
         declare
            Wd : constant Width_Type := Get_Type_Width (Rtyp);
            Inner_Wd : constant Width_Type := Wd / Width_Type (Count);
            --  Check there is no truncation.
            pragma Assert (Inner_Wd * Width_Type (Count) = Wd);
            Off : Width_Type;
            Change : Boolean;
         begin
            Res := (Kind => Value_Memory, Typ => Rtyp, Mem => null);
            Res.Mem := Allocate_Memory (Inst, Rtyp);
            Synth_Static_Concatenation (Res, Inner_Wd, N, Arr);
            --  Replicate
            Off := Inner_Wd;
            for I in 2 .. Count loop
               case Get_Kind (Rtyp) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Part_Insert
                       (To_Logvec_Ptr (Res.Mem), Bit_Offset (Off),
                        To_Logvec_Ptr (Res.Mem), 0, Inner_Wd, Change);
                  when others =>
                     Error_Kind ("synth_replication_cc", Rtyp);
               end case;
               Off := Off + Inner_Wd;
            end loop;
            pragma Assert (Off = Wd);
         end;
      else
         declare
            Nt : Net;
            Res_Net : Net;
         begin
            Nt := Synth_Dynamic_Concatenation (Inst, Arr);

            if Count < 8 then
               declare
                  Arr2 : constant Net_Array (1 .. 8) := (others => Nt);
               begin
                  Res_Net := Build2_Concat (Ctxt, Arr2 (1 .. Count));
               end;
            else
               declare
                  Arr2 : Net_Array_Acc;
               begin
                  Arr2 := new Net_Array'(1 .. Count => Nt);
                  Res_Net := Build2_Concat (Ctxt, Arr2.all);
                  Free_Net_Array (Arr2);
               end;
            end if;

            Res := Create_Value_Net (Res_Net, Rtyp);
         end;
      end if;

      Free_Valtyp_Array (Arr);
      return Res;
   end Synth_Replication_Cst;

   function Synth_System_Call (Inst : Synth_Instance_Acc; N : Node)
                              return Valtyp is
   begin
      case Get_Sys_Tf_Id (N) is
         when Sys_Tf_Signed_Id
           | Sys_Tf_Unsigned_Id =>
            declare
               Typ : constant Node := Get_Expr_Type (N);
               Arg : Node;
               Res : Valtyp;
            begin
               Arg := Get_Arguments (N);
               pragma Assert (Get_Chain (Arg) = Null_Node);
               Arg := Get_Expression (Arg);
               Res := Synth_Expression (Inst, Arg);
               if Is_Static (Res) then
                  Res := (Kind => Value_Memory, Typ => Typ, Mem => Res.Mem);
               else
                  Res := Create_Value_Net
                    (Get_Net (Get_Build (Inst), Res), Typ);
               end if;
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Synth_System_Call;

   function Synth_Name_To_Expression (Inst : Synth_Instance_Acc; N : Node)
                                     return Valtyp
   is
      Ctxt : constant Context_Acc := Get_Build (Inst);
      Res : Valtyp;
      Doff : Net;
      Off : Name_Offsets;
      Nt : Net;
      Typ : Node;
      W : Width;
   begin
      Synth_Name (Inst, N, Res, Doff, Off);
      if Is_Static (Res) and then Doff = No_Net then
         if Off.Net_Off /= 0 then
            raise Internal_Error;
         end if;
      elsif Res.Kind = Value_Wire and then Is_Static_Wire (Res.W) then
         if Off.Mem_Off /= 0 or else Off.Bit_Off /= 0 then
            raise Internal_Error;
         end if;
         return (Kind => Value_Memory,
                 Typ => Res.Typ, Mem => Get_Static_Wire (Res.W).Mem);
      else
         Nt := Get_Net (Ctxt, Res);
         Typ := Get_Expr_Type (N);
         W := Get_Type_Bitwidth (Typ);

         if Doff /= No_Net then
            if Off.Net_Off /= 0 then
               raise Internal_Error;
            end if;
            Nt := Build_Dyn_Extract (Ctxt, Nt, Doff, Off.Net_Off, W);
            Set_Location (Nt, N);
         else
            Nt := Build2_Extract (Ctxt, Nt, Off.Net_Off, W);
         end if;
         Res := Create_Value_Net (Nt, Typ);
      end if;
      return Res;
   end Synth_Name_To_Expression;

   function Synth_Expression (Inst : Synth_Instance_Acc; N : Node)
                             return Valtyp is
   begin
      case Get_Kind (N) is
         when N_Binary_Op =>
            return Synth_Binary_Op (Inst, N);
         when N_Unary_Op =>
            return Synth_Unary_Op (Inst, N);
         when N_Short_Circuit_Op =>
            return Synth_Short_Circuit_Op (Inst, N);
         when N_Conversion =>
            return Synth_Conversion (Inst, N);
         when N_Cond_Op =>
            return Synth_Cond_Op (Inst, N);
         when N_Concatenation =>
            --  Replications must be N_Replication_Cst
            pragma Assert (Get_Replication (N) = Null_Node);
            return Synth_Concatenation (Inst, N);
         when N_Replication_Cst =>
            return Synth_Replication_Cst (Inst, N);
         when N_Name
            | N_Bit_Select
            | N_Part_Select_Cst
            | N_Indexed_Name =>
            return Synth_Name_To_Expression (Inst, N);
         when N_Number
            | N_Computed_Number =>
            declare
               Res : Valtyp;
            begin
               --  Allocate memory (for the result)
               Res := (Kind => Value_Memory,
                       Typ => Get_Expr_Type (N),
                       Mem => null);
               Res.Mem := Allocate_Memory (Inst, Res.Typ);
               --  Fill with the value
               case Get_Kind (Res.Typ) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Number (To_Logvec_Ptr (Res.Mem), N);
                  when N_Bit_Packed_Array_Cst =>
                     Compute_Number (To_Bitvec_Ptr (Res.Mem), N);
                  when others =>
                     Error_Kind ("synth_expression(number)", Res.Typ);
               end case;
               return Res;
            end;
         when N_Unbased_Literal =>
            declare
               Res : Valtyp;
            begin
               --  Allocate memory (for the result)
               Res := (Kind => Value_Memory,
                       Typ => Get_Expr_Type (N),
                       Mem => null);
               Res.Mem := Allocate_Memory (Inst, Res.Typ);
               --  Fill with the value
               case Get_Kind (Res.Typ) is
                  when N_Log_Packed_Array_Cst =>
                     Compute_Unbased_Literal (To_Logvec_Ptr (Res.Mem), N);
                  when others =>
                     Error_Kind ("synth_expression(unbased_lit)", Res.Typ);
               end case;
               return Res;
            end;
         when N_String_Literal =>
            declare
               Expr_Typ : constant Node := Get_Expr_Type (N);
               pragma Assert (Get_Kind (Expr_Typ) = N_Log_Packed_Array_Cst);
               Res : Valtyp;
            begin
               --  Allocate memory (for the result)
               Res := (Kind => Value_Memory,
                       Typ => Get_Expr_Type (N),
                       Mem => null);
               Res.Mem := Allocate_Memory (Inst, Res.Typ);
               Execute_String_Literal (To_Address (Res.Mem), N);
               return Res;
            end;
         when N_Real_Number =>
            declare
               Res : Valtyp;
            begin
               --  Allocate memory (for the result)
               Res := (Kind => Value_Memory,
                       Typ => Get_Expr_Type (N),
                       Mem => null);
               Res.Mem := Allocate_Memory (Inst, Res.Typ);
               Execute_Real_Number (To_Address (Res.Mem), N);
               return Res;
            end;
         when N_System_Call =>
            return Synth_System_Call (Inst, N);
         when others =>
            Error_Kind ("synth_expression", N);
            return No_Valtyp;
      end case;
   end Synth_Expression;

   procedure Valtyp_To_Int32 (V : Valtyp; Res : out Int32; Err : out Boolean)
   is
      W : constant Width_Type := Get_Type_Width (V.Typ);
   begin
      pragma Assert (V.Kind = Value_Memory);
      Res := 0;
      Err := False;
      case Get_Kind (V.Typ) is
         when N_Log_Packed_Array_Cst =>
            declare
               Vec : constant Logvec_Ptr := To_Logvec_Ptr (V.Mem);
            begin
               if not In_Int32 (Vec, W) then
                  Err := True;
               else
                  Res := To_Int32 (Vec, W);
               end if;
            end;
         when others =>
            Error_Kind ("valtyp_to_int32", V.Typ);
      end case;
   end Valtyp_To_Int32;

   procedure Synth_Object_Name (Inst : Synth_Instance_Acc;
                                N : Node;
                                Base : out Valtyp)
   is
      Scope_Node : Node;
   begin
      --  Find the scope of N.
      Scope_Node := Get_Parent (N);
      loop
         case Get_Kind (Scope_Node) is
            when N_Compilation_Unit
              | N_Module =>
               exit;
            when others =>
               Scope_Node := Get_Parent (Scope_Node);
         end case;
      end loop;

      --  Find the corresponding scope frame.
      pragma Unreferenced (Scope_Node);

      Base := Verilog_Context.Get_Obj_Value (Inst, N);
   end Synth_Object_Name;

   procedure Synth_Indexed_Name (Inst : Synth_Instance_Acc;
                                 N : Node;
                                 Base : out Valtyp;
                                 Doff : out Net;
                                 Off : out Name_Offsets)
   is
      Name : constant Node := Get_Name (N);
      Name_Type : constant Node := Get_Expr_Type (Name);
      Msb : constant Int32 := Get_Msb_Cst (Name_Type);
      Lsb : constant Int32 := Get_Lsb_Cst (Name_Type);
      El_Typ : constant Node := Get_Type_Element_Type (Name_Type);
      El_Sz : constant Uns32 := Get_Type_Bitwidth (El_Typ);
      Base_Doff : Net;
      Base_Off : Name_Offsets;
      Expr : Valtyp;
      Idx : Int32;
      Err : Boolean;
      Res_Off : Uns32;
   begin
      Base := No_Valtyp;
      Doff := No_Net;

      Synth_Name (Inst, Name, Base, Base_Doff, Base_Off);
      Expr := Synth_Expression (Inst, Get_Expression (N));

      if Is_Static (Expr) then
         --  Static index.
         Valtyp_To_Int32 (Expr, Idx, Err);
         if Err then
            --  Incorrect Index (with z or x)
            raise Internal_Error;
         end if;
         if Msb >= Lsb then
            --  Big endian
            if Idx < Lsb or Idx > Msb then
               --  Out of bounds
               raise Internal_Error;
            end if;
            Res_Off := Uns32 (Idx - Lsb);
         else
            --  Little endian
            if Idx > Lsb or Idx < Msb then
               --  Out of bounds
               raise Internal_Error;
            end if;
            Res_Off := Uns32 (Lsb - Idx);
         end if;
         if Base_Off.Mem_Off /= 0 or Base_Off.Bit_Off /= 0 then
            raise Internal_Error;
         end if;
         if Base_Doff /= No_Net then
            raise Internal_Error;
         end if;

         case Get_Kind (N) is
            when N_Bit_Select =>
               Off := (Net_Off => Base_Off.Net_Off + Res_Off * El_Sz,
                       Mem_Off => Base_Off.Mem_Off,
                       Bit_Off =>
                         Base_Off.Bit_Off + Bit_Offset (Res_Off * El_Sz));
            when N_Indexed_Name =>
               pragma Assert (Base_Off.Bit_Off = 0);
               Off := (Net_Off => Base_Off.Net_Off + Res_Off * El_Sz,
                       Mem_Off =>
                         (Base_Off.Mem_Off + Size_Type (Res_Off) * Size_Type
                            (Verilog.Allocates.Get_Storage_Size (El_Typ))),
                       Bit_Off => 0);
            when others =>
               raise Internal_Error;
         end case;
      else
         --  Dynamic index.
         declare
            Ctxt : constant Context_Acc := Get_Build (Inst);
            Idx_Net : Net;
            Lsb_Net : Net;
            Len : Uns32;
         begin
            --  Convert to offset
            Idx_Net := Get_Net (Ctxt, Expr);
            Lsb_Net := Build_Const_UB32 (Ctxt, To_Uns32 (Lsb),
                                         Get_Width (Idx_Net));

            if Msb > Lsb then
               --  M >= I >= L  -->  off = I - L
               Doff := Build_Dyadic (Ctxt, Id_Sub, Idx_Net, Lsb_Net);
               Len := Uns32 (Msb - Lsb + 1);
            else
               --  M <= I <= L  -->  off = L - I
               Doff := Build_Dyadic (Ctxt, Id_Sub, Lsb_Net, Idx_Net);
               Len := Uns32 (Lsb - Msb + 1);
            end if;
            Set_Location (Doff, N);

            --  Create memidx.
            Doff := Build_Memidx
              (Ctxt, Doff, El_Sz, Len - 1, Clog2 (El_Sz * Len));
            Set_Location (Doff, N);

            Off := (Net_Off => 0, Mem_Off => 0, Bit_Off => 0);

            if Base_Off.Mem_Off /= 0 or Base_Off.Bit_Off /= 0 then
               raise Internal_Error;
            end if;
            if Base_Doff /= No_Net then
               raise Internal_Error;
            end if;
         end;
      end if;
   end Synth_Indexed_Name;

   procedure Synth_Name (Inst : Synth_Instance_Acc;
                         N : Node;
                         Base : out Valtyp;
                         Doff : out Net;
                         Off : out Name_Offsets) is
   begin
      Base := No_Valtyp;
      Doff := No_Net;
      case Get_Kind (N) is
         when N_Name =>
            Synth_Name (Inst, Get_Declaration (N), Base, Doff, Off);
         when N_Bit_Select
            | N_Indexed_Name =>
            Synth_Indexed_Name (Inst, N, Base, Doff, Off);
         when N_Part_Select_Cst =>
            declare
               Name : constant Node := Get_Name (N);
               Name_Type : constant Node := Get_Expr_Type (Name);
               Name_Msb : constant Int32 := Get_Msb_Cst (Name_Type);
               Name_Lsb : constant Int32 := Get_Lsb_Cst (Name_Type);
               Part_Msb : constant Int32 := Get_Msb_Cst (N);
               Part_Lsb : constant Int32 := Get_Lsb_Cst (N);
               Base_Doff : Net;
               Base_Off : Name_Offsets;
               Res_Off : Bit_Offset;
            begin
               Synth_Name (Inst, Name, Base, Base_Doff, Base_Off);
               if Name_Msb >= Name_Lsb then
                  --  Big endian
                  pragma Assert (Part_Msb >= Part_Lsb);
                  if Part_Lsb < Name_Lsb or else Part_Msb > Name_Msb then
                     --  Out of bounds
                     raise Internal_Error;
                  end if;
                  Res_Off := Bit_Offset (Part_Lsb - Name_Lsb);
               else
                  --  Little endian.
                  pragma Assert (Part_Msb <= Part_Lsb);
                  if Part_Lsb > Name_Lsb or else Part_Msb < Name_Msb then
                     --  Out of bounds
                     raise Internal_Error;
                  end if;
                  Res_Off := Bit_Offset (Name_Lsb - Part_Lsb);
               end if;
               if Base_Off.Mem_Off /= 0 or else Base_Off.Bit_Off /= 0 then
                  raise Internal_Error;
               end if;
               if Base_Doff /= No_Net then
                  raise Internal_Error;
               end if;
               Off := (Net_Off => Base_Off.Net_Off + Uns32 (Res_Off),
                       Mem_Off => Base_Off.Mem_Off,
                       Bit_Off => Base_Off.Bit_Off + Res_Off);
            end;
         when N_Wire
            | N_Wire_Direct
            | N_Var =>
            Synth_Object_Name (Inst, N, Base);
            Off := (Net_Off => 0, Mem_Off => 0, Bit_Off => 0);
         when N_Parameter
           | N_Localparam =>
            declare
               Data : constant Memory_Ptr := Elab.Memtype.To_Memory_Ptr
                 (Verilog.Allocates.Get_Parameter_Data (N));
            begin
               Base := Create_Value_Memory (Data, Get_Param_Type (N));
               Off := (Net_Off => 0, Mem_Off => 0, Bit_Off => 0);
            end;
         when others =>
            Error_Kind ("synth_name", N);
      end case;
   end Synth_Name;

end Synth.Verilog_Exprs;
