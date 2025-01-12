--  Compute verilog expressions (with the verilog semantic).
--  Copyright (C) 2023 Tristan Gingold
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

with Simple_IO;

with Verilog.Bn_Tables;
with Verilog.Errors; use Verilog.Errors;


package body Verilog.Bignums is
   Digit_Mask : constant Uns32 := not 0;

   function To_Logic (Val : Uns32; Zx : Uns32) return Logic_Type is
   begin
      return Logic_Type'Val ((Val and 1) or Shift_Left (Zx and 1, 1));
   end To_Logic;

   function To_Last (W : Width_Type) return Digit_Index is
   begin
      return Digit_Index ((W - 1) / Digit_Width);
   end To_Last;

   function Shift_Left (Val : Logic_32; Amount : Natural) return Logic_32 is
   begin
      return Logic_32'(Val => Shift_Left (Val.Val, Amount),
                       Zx => Shift_Left (Val.Zx, Amount));
   end Shift_Left;

   function Shift_Right (Val : Logic_32; Amount : Natural) return Logic_32 is
   begin
      return Logic_32'(Val => Shift_Right (Val.Val, Amount),
                       Zx => Shift_Right (Val.Zx, Amount));
   end Shift_Right;

   function Shift_Right_Arithmetic
     (Val : Logic_32; Amount : Natural) return Logic_32 is
   begin
      return Logic_32'(Val => Shift_Right_Arithmetic (Val.Val, Amount),
                       Zx => Shift_Right_Arithmetic (Val.Zx, Amount));
   end Shift_Right_Arithmetic;

   function "or" (L, R : Logic_32) return Logic_32 is
   begin
      return (Val => L.Val or R.Val,
              Zx => L.Zx or R.Zx);
   end "or";

   function "and" (L, R : Logic_32) return Logic_32 is
   begin
      return (Val => L.Val and R.Val,
              Zx => L.Zx and R.Zx);
   end "and";

   function "and" (L : Logic_32; R : Uns32) return Logic_32 is
   begin
      return (Val => L.Val and R,
              Zx => L.Zx and R);
   end "and";

   function Uns64_Hi (V : Uns64) return Uns32 is
   begin
      return Uns32 (Shift_Right (V, 32));
   end Uns64_Hi;

   function Uns64_Lo (V : Uns64) return Uns32 is
   begin
      return Uns32 (V and 16#ffff_ffff#);
   end Uns64_Lo;

   function To_Logic_32 (V : Logic_Type) return Logic_32
   is
      Res : Logic_32;
   begin
      Res := (Val => Logic_Type'Pos (V) and 1,
              Zx => Shift_Right (Logic_Type'Pos (V), 1));
      return Res;
   end To_Logic_32;

   function Get_Bin_Digit (V : Logic_32; P : Natural) return Character
   is
      To_Char : constant array (Uns32 range 0 .. 3) of Character := "01zx";
      B : Uns32;
   begin
      B := (Shift_Right (V.Val, P) and 1) + (Shift_Right (V.Zx, P) and 1) * 2;
      return To_Char (B);
   end Get_Bin_Digit;

   Hex : constant array (Uns32 range 0 .. 15) of Character :=
     "0123456789abcdef";

   function Get_Hex_Digit (V : Logic_32; P : Natural) return Character
   is
      Va : Uns32;
      Zx : Uns32;
   begin
      Zx := Shift_Right (V.Zx, P) and 15;
      Va := Shift_Right (V.Val, P) and 15;
      if Zx = 0 then
         return Hex (Va);
      elsif Zx = 15 then
         if Va = 15 then
            return 'x';
         elsif Va = 0 then
            return 'z';
         else
            return 'X';
         end if;
      elsif (Va and Zx) = 0 then
         --  0, 1 and Z.
         return 'Z';
      else
         return 'X';
      end if;
   end Get_Hex_Digit;

   --  Disp LV.
   procedure Dlv (Lv : Logvec_Ptr; Width : Width_Type)
   is
      use Simple_IO;
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in reverse 0 .. Last loop
         for J in reverse 0 .. Digit_Width - 1 loop
            Put (Get_Bin_Digit (Lv (I), J));
         end loop;
      end loop;
      New_Line;
   end Dlv;

   procedure Assign (Dest : Logvec_Ptr; Src : Logvec_Ptr; Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in 0 .. Last loop
         Dest (I) := Src (I);
      end loop;
   end Assign;

   procedure Assign (Dest : Bitvec_Ptr; Src : Bitvec_Ptr; Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in 0 .. Last loop
         Dest (I) := Src (I);
      end loop;
   end Assign;

   procedure Compute_Number (Dest : Logvec_Ptr; Num : Node)
   is
      Etype : constant Node := Get_Expr_Type (Num);
      pragma Assert (Get_Kind (Etype) = N_Log_Packed_Array_Cst
                       or else Get_Kind (Etype) = N_Enum_Type);
      Sz : constant Width_Type := Get_Type_Width (Etype);
      Eval, Ezx : Uns32;
   begin
      pragma Assert (Digit_Width = 32);
      Dest (0) := (Val => Get_Number_Lo_Val (Num),
                   Zx => Get_Number_Lo_Zx (Num));
      if Sz <= Digit_Width then
         return;
      end if;

      Eval := Get_Number_Hi_Val (Num);
      Ezx := Get_Number_Hi_Zx (Num);
      Dest (1) := (Val => Eval,
                   Zx => Ezx);
      if Sz <= 2 * Digit_Width then
         return;
      end if;

      --  Sign extend
      --  FIXME: We assume the number has already been extended.
      if (Ezx and 1) = 1 then
         Ezx := not 0;
         Eval := Shift_Right_Arithmetic
           (Shift_Left (Eval, Digit_Width - 1), Digit_Width - 1);
      else
         Ezx := 0;
         if Get_Signed_Flag (Num) then
            Eval := Shift_Right_Arithmetic
              (Shift_Left (Eval, Digit_Width - 1), Digit_Width - 1);
         else
            Eval := 0;
         end if;
      end if;
      for I in 2 .. To_Last (Sz) loop
         Dest (I) := (Val => Eval, Zx => Ezx);
      end loop;
   end Compute_Number;

   procedure Compute_Number (Dest : Bitvec_Ptr; Num : Node)
   is
      Etype : constant Node := Get_Expr_Type (Num);
      pragma Assert (Get_Kind (Etype) = N_Bit_Packed_Array_Cst
                       or else Get_Kind (Etype) = N_Enum_Type);
      Sz : constant Width_Type := Get_Type_Width (Etype);
   begin
      pragma Assert (Digit_Width = 32);
      if Sz <= Digit_Width then
         Dest (0) := Get_Number_Lo_Val (Num);
      elsif Sz <= 2 * Digit_Width then
         Dest (0) := Get_Number_Lo_Val (Num);
         Dest (1) := Get_Number_Hi_Val (Num);
      else
         raise Internal_Error;
      end if;
   end Compute_Number;

   procedure Compute_Bignum (Dest : Logvec_Ptr; Num : Node)
   is
      Etype : constant Node := Get_Expr_Type (Num);
      pragma Assert (Get_Kind (Etype) = N_Log_Packed_Array_Cst
                       or else Get_Kind (Etype) = N_Enum_Type);
      Sz : constant Width_Type := Get_Type_Width (Etype);
      Len : constant Uns32 := Get_Bignum_Len (Num);
      Idx : constant Bn_Index := Get_Bignum_Index (Num);
      Last : constant Digit_Index := To_Last (Sz);
      pragma Assert (Sz = Width_Type (Len));
   begin
      for I in 0 .. Last loop
         Dest (I) := Verilog.Bn_Tables.Bn_Table.Table (Idx + Bn_Index (I));
      end loop;
   end Compute_Bignum;

   procedure Compute_Unbased_Literal (Dest : Logvec_Ptr; Num : Node)
   is
      Etype : constant Node := Get_Expr_Type (Num);
   begin
      case Get_Kind (Etype) is
         when N_Log_Packed_Array_Cst =>
            declare
               pragma Assert
                 (Get_Kind (Get_Type_Element_Type (Etype)) = N_Logic_Type);
               Sz : constant Width_Type := Get_Type_Width (Etype);
               V : constant Logic_32 := (Val => Get_Number_Lo_Val (Num),
                                         Zx => Get_Number_Lo_Zx (Num));
            begin
               pragma Assert (Digit_Width = 32);
               for I in 0 .. To_Last (Sz) loop
                  Dest (I) := V;
               end loop;
            end;
         when others =>
            Error_Kind ("compute_unbased_literal", Etype);
      end case;
   end Compute_Unbased_Literal;

   procedure Compute_Unbased_Literal (Dest : Bitvec_Ptr; Num : Node)
   is
      Etype : constant Node := Get_Expr_Type (Num);
      pragma Assert (Get_Kind (Etype) = N_Bit_Packed_Array_Cst);
      Sz : constant Width_Type := Get_Type_Width (Etype);
      V : constant Bitvec_Digit := Get_Number_Lo_Val (Num);
   begin
      for I in 0 .. To_Last (Sz) loop
         Dest (I) := V;
      end loop;
   end Compute_Unbased_Literal;

   procedure Compute_Unbased_Literal (Dest : Logic_Ptr; Num : Node) is
   begin
      pragma Assert (Get_Kind (Get_Expr_Type (Num)) = N_Logic_Type);
      Dest.all := To_Logic (Get_Number_Lo_Val (Num), Get_Number_Lo_Zx (Num));
   end Compute_Unbased_Literal;

   --  1800-2017 11.4.11 Conditional operator
   function Compute_Predicate (Val : Logvec_Ptr; Width : Width_Type)
                              return Tri_State_Type
   is
      W_Bits : constant Width_Type := Width mod Digit_Width;
      Last : constant Digit_Index := To_Last (Width);
      Mask : Uns32;
      Res : Tri_State_Type;
   begin
      if W_Bits /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - W_Bits));
      else
         Mask := not 0;
      end if;
      Res := False;
      for I in reverse 0 .. Last loop
         if ((Val (I).Val and not Val (I).Zx) and Mask) /= 0 then
            --  At least one bit is 1.
            return True;
         elsif (Val (I).Zx and Mask) /= 0 then
            --  At least one bit is X or Z.
            Res := Unknown;
         end if;
         Mask := not 0;
      end loop;
      return Res;
   end Compute_Predicate;

   function Compute_Predicate (Val : Bitvec_Ptr; Width : Width_Type)
                              return Tri_State_Type
   is
      W_Bits : constant Width_Type := Width mod Digit_Width;
      Last : constant Digit_Index := To_Last (Width);
      Mask : Uns32;
   begin
      if W_Bits /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - W_Bits));
      else
         Mask := not 0;
      end if;
      for I in reverse 0 .. Last loop
         if (Val (I) and Mask) /= 0 then
            --  At least one bit is 1.
            return True;
         end if;
         Mask := not 0;
      end loop;
      return False;
   end Compute_Predicate;

   procedure Compute_Conditional_Mixed_Lv (Dest : Logvec_Ptr;
                                           T : Logvec_Ptr;
                                           F : Logvec_Ptr;
                                           Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
      Is_X : Uns32;
   begin
      for I in 0 .. Last loop
         Is_X := T (I).Zx or F (I).Zx or (T (I).Val xor F (I).Val);
         Dest (I) := (Zx => Is_X,
                      Val => Is_X or T (I).Val);
      end loop;
   end Compute_Conditional_Mixed_Lv;

   procedure Compute_Conditional_Mixed_Log (Dest : Logic_Ptr;
                                            T : Logic_Type;
                                            F : Logic_Type) is
   begin
      if (T = V_0 and F = V_0) or (T = V_1 and F = V_1) then
         Dest.all := T;
      else
         Dest.all := V_X;
      end if;
   end Compute_Conditional_Mixed_Log;

   procedure Compute_Trunc (Dest : Logvec_Ptr;
                            Dwidth : Width_Type;
                            Val : Logvec_Ptr;
                            Width : Width_Type)
   is
      pragma Assert (Dwidth <= Width);
      Last : constant Digit_Index := To_Last (Dwidth);
   begin
      for I in 0 .. Last loop
         Dest (I) := Val (I);
      end loop;
   end Compute_Trunc;

   procedure Compute_Lv_Bv_Trunc (Dest : Bitvec_Ptr;
                                  Dwidth : Width_Type;
                                  Val : Logvec_Ptr;
                                  Width : Width_Type)
   is
      pragma Assert (Dwidth <= Width);
      Last : constant Digit_Index := To_Last (Dwidth);
   begin
      for I in 0 .. Last loop
         --  X or Z are converted to 0.
         Dest (I) := Val (I).Val and not Val (I).Zx;
      end loop;
   end Compute_Lv_Bv_Trunc;

   procedure Compute_Lv_Bv (Dest : Bitvec_Ptr;
                            Val : Logvec_Ptr;
                            Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in 0 .. Last loop
         --  X or Z are converted to 0.
         Dest (I) := Val (I).Val and not Val (I).Zx;
      end loop;
   end Compute_Lv_Bv;

   procedure Compute_Bv_Lv_Trunc (Dest : Logvec_Ptr;
                                  Dwidth : Width_Type;
                                  Val : Bitvec_Ptr;
                                  Width : Width_Type)
   is
      pragma Assert (Dwidth <= Width);
      Last : constant Digit_Index := To_Last (Dwidth);
   begin
      for I in 0 .. Last loop
         Dest (I) := (Val => Val (I), Zx => 0);
      end loop;
   end Compute_Bv_Lv_Trunc;

   function Sext (V : Logic_32; Width : Width_Type) return Logic_32
   is
      T : Logic_32;
   begin
      pragma Assert (Width /= 0);
      T := Shift_Left (V, Natural (Digit_Width - Width));
      T := Shift_Right_Arithmetic (T, Natural (Digit_Width - Width));
      return T;
   end Sext;

   function Sext (V : Uns32; Width : Width_Type) return Uns32
   is
      T : Uns32;
   begin
      pragma Assert (Width /= 0);
      T := Shift_Left (V, Natural (Digit_Width - Width));
      T := Shift_Right_Arithmetic (T, Natural (Digit_Width - Width));
      return T;
   end Sext;

   procedure Compute_Bv_Lv_Sext (Dest : Logvec_Ptr;
                                 Dwidth : Width_Type;
                                 Val : Bitvec_Ptr;
                                 Width : Width_Type)
   is
      pragma Assert (Dwidth >= Width);
      Dlast : constant Digit_Index := To_Last (Dwidth);
      Last : constant Digit_Index := To_Last (Width);
      Pw : constant Width_Type := Width mod Digit_Width;
      V : Uns32;
   begin
      for I in 0 .. Last - 1 loop
         Dest (I) := (Val => Val (I), Zx => 0);
      end loop;
      V := Val (Last);
      if Pw > 0 then
         V := Sext (V, Pw);
      end if;
      Dest (Last) := (Val => V, Zx => 0);

      V := Shift_Right_Arithmetic (V, Natural (Digit_Width - 1));
      for I in Last + 1 .. Dlast loop
         Dest (I) := (Val => V, Zx => 0);
      end loop;
   end Compute_Bv_Lv_Sext;

   procedure Compute_Bv_Lv_Zext (Dest : Logvec_Ptr;
                                 Dwidth : Width_Type;
                                 Val : Bitvec_Ptr;
                                 Width : Width_Type)
   is
      pragma Assert (Dwidth >= Width);
      Dlast : constant Digit_Index := To_Last (Dwidth);
      Last : constant Digit_Index := To_Last (Width);
      Pw : constant Width_Type := Width mod Digit_Width;
      V : Uns32;
   begin
      for I in 0 .. Last - 1 loop
         Dest (I) := (Val => Val (I), Zx => 0);
      end loop;
      V := Val (Last);
      if Pw > 0 then
         V := Shift_Left (V, Natural (Digit_Width - Pw));
         V := Shift_Right (V, Natural (Digit_Width - Pw));
      end if;
      Dest (Last) := (Val => V, Zx => 0);

      for I in Last + 1 .. Dlast loop
         Dest (I) := (Val => 0, Zx => 0);
      end loop;
   end Compute_Bv_Lv_Zext;

   procedure Compute_Bv_Lv (Dest : Logvec_Ptr;
                            Val : Bitvec_Ptr;
                            Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in 0 .. Last loop
         Dest (I) := (Val => Val (I), Zx => 0);
      end loop;
   end Compute_Bv_Lv;

   procedure Compute_Zext (Dest : Logvec_Ptr;
                           Dwidth : Width_Type;
                           Val : Logvec_Ptr;
                           Width : Width_Type)
   is
      pragma Assert (Dwidth >= Width);
      Dlast : constant Digit_Index := To_Last (Dwidth);
      Last : constant Digit_Index := To_Last (Width);
      Pw : constant Width_Type := Width mod Digit_Width;
      V : Logic_32;
   begin
      for I in 0 .. Last - 1 loop
         Dest (I) := Val (I);
      end loop;
      V := Val (Last);
      if Pw > 0 then
         V := Shift_Left (V, Natural (Digit_Width - Pw));
         V := Shift_Right (V, Natural (Digit_Width - Pw));
      end if;
      Dest (Last) := V;

      for I in Last + 1 .. Dlast loop
         Dest (I) := (0, 0);
      end loop;
   end Compute_Zext;

   procedure Compute_Sext (Dest : Logvec_Ptr;
                           Dwidth : Width_Type;
                           Val : Logvec_Ptr;
                           Width : Width_Type)
   is
      pragma Assert (Dwidth >= Width);
      Dlast : constant Digit_Index := To_Last (Dwidth);
      Last : constant Digit_Index := To_Last (Width);
      Pw : constant Width_Type := Width mod Digit_Width;
      V : Logic_32;
   begin
      for I in 0 .. Last - 1 loop
         Dest (I) := Val (I);
      end loop;
      V := Val (Last);
      if Pw > 0 then
         V := Sext (V, Pw);
      end if;
      Dest (Last) := V;

      V := Shift_Right_Arithmetic (V, Natural (Digit_Width - 1));
      for I in Last + 1 .. Dlast loop
         Dest (I) := V;
      end loop;
   end Compute_Sext;

   procedure Compute_Conversion
     (Dest : Data_Ptr; Expr : Node; Src : Data_Ptr)
   is
      Arg : constant Node := Get_Expression (Expr);
      Arg_Type : constant Node := Get_Expr_Type (Arg);
      Expr_Type : constant Node := Get_Expr_Type (Expr);
   begin
      case Get_Conversion_Op (Expr) is
         when Convop_Lv_Log =>
            declare
               pragma Assert (Get_Kind (Arg_Type) = N_Log_Packed_Array_Cst);
               L : constant Logic_Ptr := To_Logic_Ptr (Dest);
               R : constant Logvec_Ptr := To_Logvec_Ptr (Src);
            begin
               L.all := To_Logic (R (0).Val, R (0).Zx);
            end;
         when Convop_Lv_Bit =>
            declare
               pragma Assert (Get_Kind (Arg_Type) = N_Log_Packed_Array_Cst);
               L : constant Bit_Ptr := To_Bit_Ptr (Dest);
               R : constant Logvec_Ptr := To_Logvec_Ptr (Src);
            begin
               L.all := Bit_Type'Val
                 ((R (0).Val and 1) and not (R (0).Zx and 1));
            end;
         when Convop_Lv_Trunc =>
            Compute_Trunc
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Logvec_Ptr (Src), Get_Type_Width (Arg_Type));
         when Convop_Lv_Zext =>
            Compute_Zext
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Logvec_Ptr (Src), Get_Type_Width (Arg_Type));
         when Convop_Lv_Sext =>
            Compute_Sext
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Logvec_Ptr (Src), Get_Type_Width (Arg_Type));
         when Convop_Lv_Nop =>
            Assign (To_Logvec_Ptr (Dest), To_Logvec_Ptr (Src),
                    Get_Type_Width (Expr_Type));

         when Convop_Log_Bit =>
            case To_Logic_Ptr (Src).all is
               when V_1 =>
                  To_Bit_Ptr (Dest).all := B_1;
               when V_0 | V_X | V_Z  =>
                  To_Bit_Ptr (Dest).all := B_0;
            end case;

         when Convop_Log_Ulv =>
            declare
               A : constant Logic_Type := To_Logic_Ptr (Src).all;
               D : constant Logvec_Ptr := To_Logvec_Ptr (Dest);
            begin
               D (0) := To_Logic_32 (A);
               for I in 1 .. To_Last (Get_Type_Width (Expr_Type)) loop
                  D (I) := (0, 0);
               end loop;
            end;

         when Convop_Log_Sbv =>
            declare
               A : constant Logic_Type := To_Logic_Ptr (Src).all;
               D : constant Bitvec_Ptr := To_Bitvec_Ptr (Dest);
               V : Uns32;
            begin
               case A is
                  when V_1 =>
                     V := All_1;
                  when V_0 | V_X | V_Z =>
                     V := 0;
               end case;
               for I in 0 .. To_Last (Get_Type_Width (Expr_Type)) loop
                  D (I) := V;
               end loop;
            end;

         when Convop_Log_Real =>
            --  IEEE1800-2017 6.12.2 Conversion
            --  Individual bits that are x or z in the net of the variable
            --  shall be treated as zero upon conversion.
            case To_Logic_Ptr (Src).all is
               when V_1 =>
                  To_Fp64_Ptr (Dest).all := 1.0;
               when V_0 | V_X | V_Z  =>
                  To_Fp64_Ptr (Dest).all := 0.0;
            end case;

         when Convop_Lv_Bv_Trunc =>
            Compute_Lv_Bv_Trunc
              (To_Bitvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Logvec_Ptr (Src), Get_Type_Width (Arg_Type));

         when Convop_Lv_Bv =>
            pragma Assert
              (Get_Type_Width (Expr_Type) = Get_Type_Width (Arg_Type));
            Compute_Lv_Bv (To_Bitvec_Ptr (Dest), To_Logvec_Ptr (Src),
                           Get_Type_Width (Expr_Type));

         when Convop_Bit_Log =>
            declare
               S : constant Bit_Type := To_Bit_Ptr (Src).all;
               D : constant Logic_Ptr := To_Logic_Ptr (Dest);
            begin
               D.all := Logic_Type'Val (Bit_Type'Pos (S));
            end;
         when Convop_Bit_Ulv =>
            declare
               A : constant Bit_Type := To_Bit_Ptr (Src).all;
               D : constant Logvec_Ptr := To_Logvec_Ptr (Dest);
            begin
               D (0) := (Val => Bit_Type'Pos (A), Zx => 0);
               for I in 1 .. To_Last (Get_Type_Width (Expr_Type)) loop
                  D (I) := (0, 0);
               end loop;
            end;
         when Convop_Bit_Ubv =>
            declare
               A : constant Bit_Type := To_Bit_Ptr (Src).all;
               D : constant Bitvec_Ptr := To_Bitvec_Ptr (Dest);
            begin
               D (0) := Bit_Type'Pos (A);
               for I in 1 .. To_Last (Get_Type_Width (Expr_Type)) loop
                  D (I) := 0;
               end loop;
            end;

         when Convop_Bv_Lv_Sext =>
            Compute_Bv_Lv_Sext
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Bitvec_Ptr (Src), Get_Type_Width (Arg_Type));

         when Convop_Bv_Lv_Zext =>
            Compute_Bv_Lv_Zext
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Bitvec_Ptr (Src), Get_Type_Width (Arg_Type));

         when Convop_Bv_Lv_Trunc =>
            Compute_Bv_Lv_Trunc
              (To_Logvec_Ptr (Dest), Get_Type_Width (Expr_Type),
               To_Bitvec_Ptr (Src), Get_Type_Width (Arg_Type));

         when Convop_Bv_Lv =>
            pragma Assert
              (Get_Type_Width (Expr_Type) = Get_Type_Width (Arg_Type));
            Compute_Bv_Lv (To_Logvec_Ptr (Dest), To_Bitvec_Ptr (Src),
                           Get_Type_Width (Expr_Type));

         when Convop_Fp32_Fp64 =>
            To_Fp64_Ptr (Dest).all := Fp64 (To_Fp32_Ptr (Src).all);
         when Convop_Fp64_Fp32 =>
            To_Fp32_Ptr (Dest).all := Fp32 (To_Fp64_Ptr (Src).all);
         when Convop_Fp64_Ulv =>
            declare
               Val : constant Fp64 := To_Fp64_Ptr (Src).all;
               --  TODO.
               pragma Assert (Val < 2.0**31 and Val > -2.0**31);
               Wd : constant Width_Type := Get_Type_Width (Expr_Type);
               Ival : constant Int32 := Int32 (Val);
               Lv : constant Logvec_Ptr := To_Logvec_Ptr (Dest);
            begin
               Lv (0) := (Val => To_Uns32 (Ival), Zx => 0);
               --  Sign extend ?
               for I in 1 .. To_Last (Wd) loop
                  Lv (I) := (0, 0);
               end loop;
            end;
         when Convop_Fp64_Sbv =>
            declare
               Val : constant Fp64 := To_Fp64_Ptr (Src).all;
               --  TODO.
               pragma Assert (Val < 2.0**31 and Val > -2.0**31);
               Wd : constant Width_Type := Get_Type_Width (Expr_Type);
               Ival : constant Int32 := Int32 (Val);
               Bv : constant Bitvec_Ptr := To_Bitvec_Ptr (Dest);
            begin
               Bv (0) := To_Uns32 (Ival);
               --  Sign extend ?
               for I in 1 .. To_Last (Wd) loop
                  Bv (I) := 0;
               end loop;
            end;
         when others =>
            Error_Kind ("compute_conversion: " &
                          Conv_Ops'Image (Get_Conversion_Op (Expr)),
                        Expr);
      end case;
   end Compute_Conversion;

   --  Return true iff BV has X or Z.
   function Has_Unknowns (Lv : Logvec_Ptr; Width : Width_Type) return Boolean
   is
      Last : constant Digit_Index := To_Last (Width);
      Dw : constant Width_Type := Width mod Digit_Width;
   begin
      if Dw = 0 then
         for I in 0 .. Last loop
            if Lv (I).Zx /= 0 then
               return True;
            end if;
         end loop;
         return False;
      else
         for I in 0 .. Last - 1 loop
            if Lv (I).Zx /= 0 then
               return True;
            end if;
         end loop;
         return Shift_Left (Lv (Last).Zx, Natural (Digit_Width - Dw)) /= 0;
      end if;
   end Has_Unknowns;

   procedure Clean_Uns (Val : Logvec_Ptr; Width : Width_Type)
   is
      W : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      Last : Digit_Index;
   begin
      if W /= 0 then
         Last := To_Last (Width);
         Mask := Shift_Left (1, Natural (W)) - 1;
         Val (Last) := Val (Last) and Mask;
      end if;
   end Clean_Uns;
   pragma Unreferenced (Clean_Uns);

   procedure Compute_Div_Clean (Val : Logvec_Ptr;
                                Width : Width_Type;
                                Div : Uns32;
                                Remain : out Uns32)
   is
      Last : constant Digit_Index := To_Last (Width);
      Div64 : constant Uns64 := Uns64 (Div);
      V : Uns64;
      Q : Uns32;
   begin
      V := 0;
      for I in reverse 0 .. Last loop
         V := Shift_Left (V, Digit_Width) + Uns64 (Val (I).Val);
         Q := Uns32 (V / Div64);
         V := V - Div64 * Uns64 (Q);
         Val (I).Val := Q;
      end loop;
      Remain := Uns32 (V);
   end Compute_Div_Clean;

   procedure Compute_Mul_Add_Clean (Val : Logvec_Ptr;
                                    Width : Width_Type;
                                    Mul : Uns32;
                                    Carry : in out Uns32)
   is
      Last : constant Digit_Index := To_Last (Width);
      V : Uns64;
   begin
      V := Uns64 (Carry);
      for I in 0 .. Last loop
         V := Uns64 (Val (I).Val) * Uns64 (Mul) + V;
         Val (I).Val := Uns64_Lo (V);
         V := Shift_Right (V, Digit_Width);
      end loop;
      Carry := Uns64_Lo (V);
   end Compute_Mul_Add_Clean;

   function In_Uns32 (Lv : Logvec_Ptr; Width : Width_Type) return Boolean
   is
      Last : constant Digit_Index := To_Last (Width);
      Mask : Uns32;
   begin
      --  Check no ZX and VAL=0 in all the words but the first and the last.
      if Last > 0 then
         if Lv (0).Zx /= 0 then
            return False;
         end if;
         for I in 1 .. Last  - 1 loop
            if Lv (I).Val /= 0 or Lv (I).Zx /= 0 then
               return False;
            end if;
         end loop;
      end if;

      --  Check no ZX in the last word
      Mask := not Shift_Left (not 0, Natural (Width mod Digit_Width));
      return (Lv (Last).Zx and Mask) = 0;
   end In_Uns32;

   function In_Uns32 (Bv : Bitvec_Ptr; Width : Width_Type) return Boolean is
   begin
      for I in 1 .. To_Last (Width) loop
         if Bv (I) /= 0 then
            return False;
         end if;
      end loop;
      return True;
   end In_Uns32;

   function To_Uns32 (Lv : Logvec_Ptr; Width : Width_Type) return Uns32
   is
      pragma Assert (In_Uns32 (Lv, Width));
      Mask : Uns32;
   begin
      if Width >= Digit_Width then
         Mask := not 0;
      else
         Mask := not Shift_Left (not 0, Natural (Width));
      end if;
      return Lv (0).Val and Mask;
   end To_Uns32;

   function To_Uns32 (Bv : Bitvec_Ptr; Width : Width_Type) return Uns32
   is
      pragma Assert (In_Uns32 (Bv, Width));
      Mask : Uns32;
   begin
      if Width >= Digit_Width then
         Mask := not 0;
      else
         Mask := not Shift_Left (not 0, Natural (Width));
      end if;
      return Bv (0) and Mask;
   end To_Uns32;

   function In_Int32 (Lv : Logvec_Ptr; Width : Width_Type) return Boolean
   is
      Last : constant Digit_Index := To_Last (Width);
      Mask : Uns32;
      Sign : Uns32;
   begin
      --  Check no ZX in all the words (but the last).
      --  Check all the words (but the first) are the sign extension of the
      --  first word.
      if Last > 0 then
         Sign := Shift_Right_Arithmetic (Lv (0).Val, Digit_Width - 1);

         if Lv (0).Zx /= 0 then
            return False;
         end if;
         for I in 1 .. Last  - 1 loop
            if Lv (I).Val /= Sign or Lv (I).Zx /= 0 then
               return False;
            end if;
         end loop;
      end if;

      --  Handle the last word.
      Mask := not Shift_Left (not 0, Natural (Width mod Digit_Width));
      return (Lv (Last).Zx and Mask) = 0
        and then (Lv (Last).Val and Mask) = (Sign and Mask);
   end In_Int32;

   function To_Int32 (Lv : Logvec_Ptr; Width : Width_Type) return Int32
   is
      pragma Assert (In_Int32 (Lv, Width));
      V : Uns32;
   begin
      V := Lv (0).Val;
      if Width < Digit_Width then
         V := Shift_Left (V, Natural (Digit_Width - Width));
         V := Shift_Right_Arithmetic (V, Natural (Width));
      end if;
      return To_Int32 (V);
   end To_Int32;

   procedure Set_X (Res : Logvec_Ptr; Width : Width_Type) is
   begin
      for I in 0 .. To_Last (Width) loop
         Res (I) := (Val => Digit_Mask, Zx => Digit_Mask);
      end loop;
   end Set_X;

   procedure Set_0 (Res : Logvec_Ptr; Width : Width_Type) is
   begin
      for I in 0 .. To_Last (Width) loop
         Res (I) := (Val => 0, Zx => 0);
      end loop;
   end Set_0;

   procedure Set_0 (Res : Bitvec_Ptr; Width : Width_Type) is
   begin
      for I in 0 .. To_Last (Width) loop
         Res (I) := 0;
      end loop;
   end Set_0;

   function Is_Zero_Clean (Val : Logvec_Ptr; Width : Width_Type) return Boolean
   is
      Last : constant Digit_Index := To_Last (Width);
   begin
      for I in 0 .. Last loop
         if Val (I) /= (0, 0) then
            return False;
         end if;
      end loop;
      return True;
   end Is_Zero_Clean;

   procedure Compute_Or
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Is_1 : Uns32;
      Is_X : Uns32;
   begin
      for I in 0 .. To_Last (Width) loop
         Is_1 := (Left (I).Val and not Left (I).Zx)
           or (Right (I).Val and not Right (I).Zx);
         Is_X := Left (I).Zx or Right (I).Zx;
         Res (I) := (Zx => Is_X and not Is_1,
                     Val => Is_X or Is_1);
      end loop;
   end Compute_Or;

   procedure Compute_Or
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type) is
   begin
      for I in 0 .. To_Last (Width) loop
         Res (I) := Left (I) or Right (I);
      end loop;
   end Compute_Or;

   procedure Compute_Xor
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Is_1 : Uns32;
      Is_X : Uns32;
   begin
      for I in 0 .. To_Last (Width) loop
         Is_1 := Left (I).Val xor Right (I).Val;
         Is_X := Left (I).Zx or Right (I).Zx;
         Res (I) := (Zx => Is_X,
                     Val => Is_1 or Is_X);
      end loop;
   end Compute_Xor;

   procedure Compute_Xnor
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Is_1 : Uns32;
      Is_X : Uns32;
   begin
      for I in 0 .. To_Last (Width) loop
         Is_1 := not (Left (I).Val xor Right (I).Val);
         Is_X := Left (I).Zx or Right (I).Zx;
         Res (I) := (Zx => Is_X,
                     Val => Is_1 or Is_X);
      end loop;
   end Compute_Xnor;

   procedure Compute_And
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Is_n0 : Uns32;
      Is_X : Uns32;
   begin
      for I in 0 .. To_Last (Width) loop
         Is_n0 := (Left (I).Zx or Left (I).Val)
           and (Right (I).Zx or Right (I).Val);
         Is_X := Left (I).Zx or Right (I).Zx;
         Res (I) := (Zx => Is_n0 and Is_X,
                     Val => Is_n0);
      end loop;
   end Compute_And;

   procedure Compute_Add
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      --  IEEE 1364-2005 4.1.5 Arithmetic operators
      --  For the arithmetic operators, if any operand bit value is the
      --  unknown value x or the high-impedance value z, then the entire
      --  result value shall be x.
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
      else
         Carry := 0;
         for I in 0 .. To_Last (Width) loop
            T := Uns64 (Left (I).Val) + Uns64 (Right (I).Val) + Uns64 (Carry);
            Res (I) := (Val => Uns32 (T and Uns64 (Digit_Mask)),
                        Zx => 0);
            Carry := Uns32 (Shift_Right (T, Digit_Width));
         end loop;
      end if;
   end Compute_Add;

   procedure Compute_Add
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      Carry := 0;
      for I in 0 .. To_Last (Width) loop
         T := Uns64 (Left (I)) + Uns64 (Right (I)) + Uns64 (Carry);
         Res (I) := Uns32 (T and Uns64 (Digit_Mask));
         Carry := Uns32 (Shift_Right (T, Digit_Width));
      end loop;
   end Compute_Add;


   --  Do an unsigned multiplication; assuming not unknown bits.
   procedure Do_Umul
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
      T : Uns64;
      Carry : Uns32;
   begin
      Set_0 (Res, Width);
      for I in 0 .. Last loop
         Carry := 0;
         for J in 0 .. Last - I loop
            T := Uns64 (Left (I).Val) * Uns64 (Right (I).Val)
              + Uns64 (Res (I + J).Val) + Uns64 (Carry);
            Res (I + J).Val := Uns64_Lo (T);
            Carry := Uns64_Hi (T);
         end loop;
      end loop;
   end Do_Umul;

   procedure Compute_Umul
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      if Width <= Digit_Width then
         Res (0) := (Val => Left (0).Val * Right (0).Val, Zx => 0);
      else
         Do_Umul (Res, Left, Right, Width);
      end if;
   end Compute_Umul;

   --  Return True iff V is negative.
   function Is_Neg (V : Logvec_Ptr; W : Width_Type) return Boolean is
   begin
      return (Shift_Right (V (To_Last (W)).Val,
                           Natural (W mod Digit_Width)) and 1) = 1;
   end Is_Neg;

   procedure Compute_Smul
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      if Width <= Digit_Width then
         Res (0) := (Val => To_Uns32 (To_Int32 (Left (0).Val)
                                        * To_Int32 (Right (0).Val)), Zx => 0);
      else
         if Is_Neg (Left, Width) then
            raise Internal_Error;
         else
            Do_Umul (Res, Left, Right, Width);
         end if;
      end if;
   end Compute_Smul;

   procedure Compute_Sdiv
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      --  FIXME: check div / 0.

      if Width = Digit_Width then
         Res (0) := (Val => To_Uns32
                       (To_Int32 (Left (0).Val) / To_Int32 (Right (0).Val)),
                     Zx => 0);
      else
         raise Internal_Error;
      end if;
   end Compute_Sdiv;

   procedure Compute_Udiv
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      --  FIXME: check div / 0.

      if Width = Digit_Width then
         Res (0) := (Val => Left (0).Val / Right (0).Val, Zx => 0);
      else
         raise Internal_Error;
      end if;
   end Compute_Udiv;

   procedure Compute_Udiv
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type) is
   begin
      --  FIXME: check div / 0.

      if Width = Digit_Width then
         Res (0) := Left (0) / Right (0);
      else
         raise Internal_Error;
      end if;
   end Compute_Udiv;

   procedure Compute_Smod
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      --  FIXME: check div / 0.

      if Width = Digit_Width then
         Res (0) := (Val => To_Uns32
                       (To_Int32 (Left (0).Val) rem To_Int32 (Right (0).Val)),
                     Zx => 0);
      else
         raise Internal_Error;
      end if;
   end Compute_Smod;

   procedure Compute_Umod
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type) is
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
         return;
      end if;

      --  FIXME: check div / 0.

      if Width = Digit_Width then
         Res (0) := (Val => Left (0).Val rem Right (0).Val, Zx => 0);
      else
         raise Internal_Error;
      end if;
   end Compute_Umod;

   procedure Compute_Smod
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type) is
   begin
      --  FIXME: check div / 0.

      if Width = Digit_Width then

         Res (0) := To_Uns32 (To_Int32 (Left (0)) rem To_Int32 (Right (0)));
      else
         raise Internal_Error;
      end if;
   end Compute_Smod;

   procedure Compute_Inc
     (Res : Logvec_Ptr; Expr : Logvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      --  IEEE 1364-2005 4.1.5 Arithmetic operators
      --  For the arithmetic operators, if any operand bit value is the
      --  unknown value x or the high-impedance value z, then the entire
      --  result value shall be x.
      if Has_Unknowns (Expr, Width) then
         Set_X (Res, Width);
      else
         Carry := 1;
         for I in 0 .. To_Last (Width) loop
            T := Uns64 (Expr (I).Val) + Uns64 (Carry);
            Res (I) := (Val => Uns64_Lo (T),
                        Zx => 0);
            Carry := Uns64_Hi (T);
         end loop;
      end if;
   end Compute_Inc;

   procedure Compute_Inc
     (Res : Bitvec_Ptr; Expr : Bitvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      Carry := 1;
      for I in 0 .. To_Last (Width) loop
         T := Uns64 (Expr (I)) + Uns64 (Carry);
         Res (I) := Uns64_Lo (T);
         Carry := Uns64_Hi (T);
      end loop;
   end Compute_Inc;

   procedure Compute_Sub
     (Res : Logvec_Ptr; Left, Right : Logvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      --  IEEE 1364-2005 4.1.5 Arithmetic operators
      --  For the arithmetic operators, if any operand bit value is the
      --  unknown value x or the high-impedance value z, then the entire
      --  result value shall be x.
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         Set_X (Res, Width);
      else
         Carry := 0;
         for I in 0 .. To_Last (Width) loop
            T := Uns64 (Left (I).Val) - Uns64 (Right (I).Val) - Uns64 (Carry);
            Res (I) := (Val => Uns32 (T and Uns64 (Digit_Mask)),
                        Zx => 0);
            Carry := Uns32 (Shift_Right (T, Digit_Width) and 1);
         end loop;
      end if;
   end Compute_Sub;

   procedure Compute_Sub
     (Res : Bitvec_Ptr; Left, Right : Bitvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      Carry := 0;
      for I in 0 .. To_Last (Width) loop
         T := Uns64 (Left (I)) - Uns64 (Right (I)) - Uns64 (Carry);
         Res (I) := Uns32 (T and Uns64 (Digit_Mask));
         Carry := Uns32 (Shift_Right (T, Digit_Width) and 1);
      end loop;
   end Compute_Sub;

   procedure Check_Lshift_Amount (Res : Logvec_Ptr;
                                  Width : Width_Type;
                                  Right : Logvec_Ptr;
                                  Right_Width : Width_Type;
                                  Amt : out Width_Type;
                                  Done : out Boolean) is
   begin
      --  IEEE 1364-2005 4.1.12 Shift operators
      --  If the right operand has an x or z value, the the result shall be
      --  unknown.
      if Has_Unknowns (Right, Right_Width) then
         Set_X (Res, Width);
         Done := True;
         return;
      end if;

      if not In_Uns32 (Right, Right_Width) then
         Set_0 (Res, Width);
         Done := True;
         return;
      end if;

      Amt := Width_Type (To_Uns32 (Right, Right_Width));
      if Amt > Width then
         --  All 0.
         Set_0 (Res, Width);
         Done := True;
         return;
      end if;

      Done := False;
   end Check_Lshift_Amount;

   procedure Check_Lshift_Amount (Res : Bitvec_Ptr;
                                  Width : Width_Type;
                                  Right : Bitvec_Ptr;
                                  Right_Width : Width_Type;
                                  Amt : out Width_Type;
                                  Done : out Boolean) is
   begin
      if not In_Uns32 (Right, Right_Width) then
         Set_0 (Res, Width);
         Done := True;
         return;
      end if;

      Amt := Width_Type (To_Uns32 (Right, Right_Width));
      if Amt > Width then
         --  All 0.
         Set_0 (Res, Width);
         Done := True;
         return;
      end if;

      Done := False;
   end Check_Lshift_Amount;

   procedure Compute_Shr (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type)
   is
      Done : Boolean;
      Last : constant Digit_Index := To_Last (Width);
      Amt : Width_Type;
      Am_Digits : Digit_Index;
      Am_Bits : Natural;
      V1, V2 : Logic_32;
   begin
      Check_Lshift_Amount (Res, Width, Right, Right_Width, Amt, Done);
      if Done then
         return;
      end if;

      Am_Digits := Digit_Index (Amt / Digit_Width);
      Am_Bits := Natural (Amt mod Digit_Width);

      if Am_Bits = 0 then
         --  Shift by packets.
         for I in 0 .. Last - Am_Digits loop
            Res (I) := Left (I + Am_Digits);
         end loop;
      else
         --  Shift by bits.
         V1 := (0, 0);
         for I in 0 .. Last - Am_Digits loop
            V2 := Left (I + Am_Digits);
            Res (I) := Shift_Right (V2, Am_Bits) or V1;
            V1 := Shift_Left (V2, Digit_Width - Am_Bits);
         end loop;
      end if;

      declare
         W_Bits : constant Natural :=
           (Natural (Width) - Am_Bits) mod Digit_Width;
         Mask : Uns32;
      begin
         if W_Bits /= 0 then
            --  Clean the most significant digit.
            Mask := Shift_Right (not 0, Digit_Width - W_Bits);
            Res (Last - Am_Digits) := Res (Last - Am_Digits) and (Mask, Mask);
         end if;
      end;

      --  Zext.
      --  1800-2017 11.4.10 Shift operators
      --  The logical right shift shall fill the vacated bit positions with
      --  zeros.
      for I in Last - Am_Digits + 1 .. Last loop
         Res (I) := (0, 0);
      end loop;
   end Compute_Shr;

   procedure Compute_Asr (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type)
   is
      Last : constant Digit_Index := To_Last (Width);
      Amt : Width_Type;
      Am_Digits : Digit_Index;
      Am_Bits : Natural;
      V1, V2 : Logic_32;
      M : Logic_32;
   begin
      --  1800-2017 11.4.10 Shift operators
      --  If the right operand has an x or z value, then the result shall be
      --  unknown.
      if Has_Unknowns (Right, Right_Width) then
         Set_X (Res, Width);
         return;
      end if;

      --  Extract the MSB.
      declare
         Pw : constant Width_Type := Width mod Digit_Width;
         V : constant Logic_32 := Left (Last);
      begin
         M.Val := Shift_Left (V.Val, Natural (Digit_Width - Pw));
         M.Val := Shift_Right_Arithmetic (M.Val, Digit_Width - 1);
         M.Zx := Shift_Left (V.Zx, Natural (Digit_Width - Pw));
         M.Zx := Shift_Right_Arithmetic (M.Zx, Digit_Width - 1);
      end;

      if not In_Uns32 (Right, Right_Width) then
         --  Shift is way too large; set all bits.
         for I in 0 .. Last loop
            Res (I) := M;
         end loop;
         return;
      end if;

      Amt := Width_Type (To_Uns32 (Right, Right_Width));
      if Amt > Width then
         --  Shift is too large, set all bits.
         for I in 0 .. Last loop
            Res (I) := M;
         end loop;
         return;
      end if;

      Am_Digits := Digit_Index (Amt / Digit_Width);
      Am_Bits := Natural (Amt mod Digit_Width);

      if Am_Bits = 0 then
         --  Shift by packets.
         for I in 0 .. Last - Am_Digits loop
            Res (I) := Left (I + Am_Digits);
         end loop;
      else
         --  Shift by bits.
         V1 := (0, 0);
         for I in 0 .. Last - Am_Digits loop
            V2 := Left (I + Am_Digits);
            Res (I) := Shift_Right (V2, Am_Bits) or V1;
            V1 := Shift_Left (V2, Digit_Width - Am_Bits);
         end loop;
      end if;

      declare
         W_Bits : constant Natural :=
           (Natural (Width) - Am_Bits) mod Digit_Width;
         Mask : Uns32;
      begin
         if W_Bits /= 0 then
            --  Clean the most significant digit.
            Mask := Shift_Right (not 0, Digit_Width - W_Bits);
            Res (Last - Am_Digits) :=
              (Res (Last - Am_Digits) and (Mask, Mask))
              or (M and (not Mask, not Mask));
         end if;
      end;

      --  1800-2017 11.4.10 Shift operators
      --  The arithmetic right shift [it] shall fill the vacated bit positions
      --  with the value of the most significant (i.e. sign) bit of the left
      --  operand if the result type is signed.
      for I in Last - Am_Digits + 1 .. Last loop
         Res (I) := M;
      end loop;
   end Compute_Asr;

   procedure Compute_Shl (Res : Logvec_Ptr;
                          Left : Logvec_Ptr;
                          Width : Width_Type;
                          Right : Logvec_Ptr;
                          Right_Width : Width_Type)
   is
      Done : Boolean;
      Last : constant Digit_Index := To_Last (Width);
      Amt : Width_Type;
      Am_Digits : Digit_Index;
      Am_Bits : Natural;
      V1, V2 : Logic_32;
   begin
      Check_Lshift_Amount (Res, Width, Right, Right_Width, Amt, Done);
      if Done then
         return;
      end if;

      Am_Digits := Digit_Index (Amt / Digit_Width);
      Am_Bits := Natural (Amt mod Digit_Width);

      if Am_Bits = 0 then
         for I in 0 .. Last - Am_Digits loop
            Res (I + Am_Digits) := Left (I);
         end loop;
      else
         V1 := (0, 0);
         for I in 0 .. Last - Am_Digits loop
            V2 := Left (I);
            Res (I + Am_Digits) := Shift_Left (V2, Am_Bits) or V1;
            V1 := Shift_Right (V2, Digit_Width - Am_Bits);
         end loop;
      end if;

      declare
         W_Bits : constant Natural := Natural (Width) mod Digit_Width;
         Mask : Uns32;
      begin
         if W_Bits /= 0 then
            --  Clean the most significant digit.
            Mask := not Shift_Left (not 0, Digit_Width - W_Bits);
            Res (Last) := Res (Last) and (Mask, Mask);
         end if;
      end;

      --  Clear lowest part.
      for I in 0 .. Am_Digits - 1 loop
         Res (I) := (0, 0);
      end loop;
   end Compute_Shl;

   procedure Compute_Shl (Res : Bitvec_Ptr;
                          Left : Bitvec_Ptr;
                          Width : Width_Type;
                          Right : Bitvec_Ptr;
                          Right_Width : Width_Type)
   is
      Done : Boolean;
      Last : constant Digit_Index := To_Last (Width);
      Amt : Width_Type;
      Am_Digits : Digit_Index;
      Am_Bits : Natural;
      V1, V2 : Bitvec_Digit;
   begin
      Check_Lshift_Amount (Res, Width, Right, Right_Width, Amt, Done);
      if Done then
         return;
      end if;

      Am_Digits := Digit_Index (Amt / Digit_Width);
      Am_Bits := Natural (Amt mod Digit_Width);

      if Am_Bits = 0 then
         for I in 0 .. Last - Am_Digits loop
            Res (I + Am_Digits) := Left (I);
         end loop;
      else
         V1 := 0;
         for I in 0 .. Last - Am_Digits loop
            V2 := Left (I);
            Res (I + Am_Digits) := Shift_Left (V2, Am_Bits) or V1;
            V1 := Shift_Right (V2, Digit_Width - Am_Bits);
         end loop;
      end if;

      declare
         W_Bits : constant Natural := Natural (Width) mod Digit_Width;
         Mask : Uns32;
      begin
         if W_Bits /= 0 then
            --  Clean the most significant digit.
            Mask := not Shift_Left (not 0, Digit_Width - W_Bits);
            Res (Last) := Res (Last) and Mask;
         end if;
      end;

      --  Clear lowest part.
      for I in 0 .. Am_Digits - 1 loop
         Res (I) := 0;
      end loop;
   end Compute_Shl;

   procedure Compute_Not
     (Res : Logvec_Ptr; Val : Logvec_Ptr; Width : Width_Type) is
   begin
      for I in 0 .. To_Last (Width) loop
         Res (I).Zx := Val (I).Zx;
         Res (I).Val := (not Val (I).Val) or Val (I).Zx;
      end loop;
   end Compute_Not;

   procedure Compute_Neg
     (Res : Logvec_Ptr; Val : Logvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      --  1800-2017 11.4.3 Arithmetic operators
      --  For the arithmetic operators, if any operand bit value is the unknown
      --  value x or the high-impedance value z, then the entire result value
      --  shall be x.
      if Has_Unknowns (Val, Width) then
         Set_X (Res, Width);
      else
         Carry := 1;
         for I in 0 .. To_Last (Width) loop
            T := Uns64 (not Val (I).Val) + Uns64 (Carry);
            Res (I) := (Val => Uns32 (T and Uns64 (Digit_Mask)),
                        Zx => 0);
            Carry := Uns32 (Shift_Right (T, Digit_Width) and 1);
         end loop;
      end if;
   end Compute_Neg;

   procedure Compute_Neg
     (Res : Bitvec_Ptr; Val : Bitvec_Ptr; Width : Width_Type)
   is
      Carry : Uns32;
      T : Uns64;
   begin
      Carry := 1;
      for I in 0 .. To_Last (Width) loop
         T := Uns64 (not Val (I)) + Uns64 (Carry);
         Res (I) := Uns64_Lo (T);
         Carry := Uns64_Hi (T) and 1;
      end loop;
   end Compute_Neg;

   function Compute_Log_Neg (Val : Logvec_Ptr; Width : Width_Type)
                            return Logic_Type
   is
      Sz : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      P : Digit_Index;
   begin
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
         if (Val (P).Zx and Mask) /= 0 then
            return V_X;
         elsif (Val (P).Val and Mask) /= 0 then
            return V_0;
         elsif P = 0 then
            return V_1;
         else
            P := P - 1;
         end if;
      end if;

      loop
         if Val (P).Zx /= 0 then
            return V_X;
         elsif Val (P).Val /= 0 then
            return V_0;
         elsif P = 0 then
            return V_1;
         else
            P := P - 1;
         end if;
      end loop;
   end Compute_Log_Neg;

   --  IEEE1800-2017 11.4.9 Reduction operators.
   function Compute_Log_Red_Or_Nor
     (Val : Logvec_Ptr; Width : Width_Type; V, D : Logic_Type)
     return Logic_Type
   is
      Sz : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      Res : Logic_Type;
   begin
      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
      else
         Mask := not 0;
      end if;

      Res := D;
      for I in reverse 0 .. To_Last (Width) loop
         --  Result is 1 if at least one bit is 1, ie:
         --  (not xz) and val != 0.
         if (((not Val (I).Zx) and Val (I).Val) and Mask) /= 0 then
            return V;
         end if;

         --  Result is X if no bit is 1 and at least one bit is X or Z.
         if (Val (I).Zx and Mask) /= 0 then
            Res := V_X;
         end if;

         Mask := not 0;
      end loop;
      return Res;
   end Compute_Log_Red_Or_Nor;

   function Compute_Log_Red_Or (Val : Logvec_Ptr; Width : Width_Type)
                               return Logic_Type is
   begin
      return Compute_Log_Red_Or_Nor (Val, Width, V_1, V_0);
   end Compute_Log_Red_Or;

   function Compute_Log_Red_Nor (Val : Logvec_Ptr; Width : Width_Type)
                               return Logic_Type is
   begin
      return Compute_Log_Red_Or_Nor (Val, Width, V_0, V_1);
   end Compute_Log_Red_Nor;

   --  IEEE1800-2017 11.4.9 Reduction operators.
   function Compute_Log_Red_And_Nand
     (Val : Logvec_Ptr; Width : Width_Type; V, D : Logic_Type)
     return Logic_Type
   is
      Sz : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      Res : Logic_Type;
   begin
      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
      else
         Mask := not 0;
      end if;

      Res := D;
      for I in reverse 0 .. To_Last (Width) loop
         --  Result is 0 if at least one bit is 0, ie:
         --  (not xz) and val = 0.
         if (((not Val (I).Zx) and not Val (I).Val) and Mask) /= 0 then
            return V;
         end if;

         --  Result is X if no bit is 0 and at least one bit is X or Z.
         if (Val (I).Zx and Mask) /= 0 then
            Res := V_X;
         end if;

         Mask := not 0;
      end loop;
      return Res;
   end Compute_Log_Red_And_Nand;

   --  IEEE1800-2017 11.4.9 Reduction operators.
   function Compute_Log_Red_And (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type is
   begin
      return Compute_Log_Red_And_Nand (Val, Width, V_0, V_1);
   end Compute_Log_Red_And;

   function Compute_Log_Red_Nand (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type is
   begin
      return Compute_Log_Red_And_Nand (Val, Width, V_1, V_0);
   end Compute_Log_Red_Nand;

   function Compute_Log_Red_Xor_Xnor
     (Val : Logvec_Ptr; Width : Width_Type; V : Uns32) return Logic_Type
   is
      Sz : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      Res : Uns32;
   begin
      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
      else
         Mask := not 0;
      end if;

      Res := 0;
      for I in reverse 0 .. To_Last (Width) loop
         --  Result is X if at least one bit is X or Z.
         if ((Val (I).Zx and Mask)) /= 0 then
            return V_X;
         end if;

         Res := Res xor (Val (I).Val and Mask);
         Mask := not 0;
      end loop;

      --  FIXME: use popcount ?

      --  Reduce to 16 bits.
      Res := Res xor Shift_Right (Res, 16);
      --  Reduce to 8 bits.
      Res := Res xor Shift_Right (Res, 8);
      --  Reduce to 4 bits.
      Res := Res xor Shift_Right (Res, 4);
      --  Reduce to 2 bits.
      Res := Res xor Shift_Right (Res, 2);
      --  Reduce to 1 bit.
      Res := Res xor Shift_Right (Res, 1);
      if (Res and 1) = V then
         return V_1;
      else
         return V_0;
      end if;
   end Compute_Log_Red_Xor_Xnor;

   function Compute_Log_Red_Xor (Val : Logvec_Ptr; Width : Width_Type)
                                return Logic_Type is
   begin
      return Compute_Log_Red_Xor_Xnor (Val, Width, 1);
   end Compute_Log_Red_Xor;

   function Compute_Log_Red_Xnor (Val : Logvec_Ptr; Width : Width_Type)
                                 return Logic_Type is
   begin
      return Compute_Log_Red_Xor_Xnor (Val, Width, 0);
   end Compute_Log_Red_Xnor;

   function Compute_To_Logic (Val : Logvec_Ptr; Width : Width_Type)
                             return Logic_Type
   is
      Sz : constant Width_Type := Width mod Digit_Width;
      Mask : Uns32;
      P : Digit_Index;
   begin
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
         if (Val (P).Zx and Mask) /= 0 then
            return V_X;
         elsif (Val (P).Val and Mask) /= 0 then
            return V_1;
         elsif P = 0 then
            return V_0;
         else
            P := P - 1;
         end if;
      end if;

      loop
         if Val (P).Zx /= 0 then
            return V_X;
         elsif Val (P).Val /= 0 then
            return V_1;
         elsif P = 0 then
            return V_0;
         else
            P := P - 1;
         end if;
      end loop;
   end Compute_To_Logic;

   procedure Compute_Part_Extract (Dest : Logvec_Ptr;
                                   Dest_Off : Bit_Offset;
                                   Dest_Wd : Width_Type;
                                   Src : Logvec_Ptr;
                                   Src_Off : Bit_Offset;
                                   Src_Wd : Width_Type)
   is
      pragma Assert (Dest_Wd >= Src_Wd);
   begin
      if Dest_Off = 0 and then Src_Wd = Dest_Wd then
         --  Normal case: no overflow
         declare
            Bit_Shift : constant Natural := Natural (Src_Off mod Digit_Width);
            Words_Shift : constant Digit_Index :=
              Digit_Index (Src_Off / Digit_Width);
            Last : constant Digit_Index := To_Last (Dest_Wd);
         begin
            if Bit_Shift = 0 then
               for I in 0 .. Last loop
                  Dest (I) := Src (I + Words_Shift);
               end loop;
            else
               for I in 0 .. Last loop
                  Dest (I) := (Shift_Right (Src (I + Words_Shift), Bit_Shift)
                                or Shift_Left (Src (I + Words_Shift + 1),
                                               Digit_Width - Bit_Shift));
               end loop;
            end if;
         end;
      else
         --  Some overflow or truncation.
         declare
            Last : constant Digit_Index := To_Last (Dest_Wd);
            L : Logic_32;
            M : Uns32;
            Di : Digit_Index;
            Db : Natural;
         begin
            for I in 0 .. Last loop
               Dest (I) := Logic_32_X;
            end loop;
            if Src_Wd = 0 then
               return;
            end if;

            for I in 0 .. Bit_Offset (Src_Wd - 1) loop
               --  Extract bit #I
               L := Shift_Right (Src (Digit_Index (Src_Off + I) / Digit_Width),
                                 Natural (Src_Off + I) mod Digit_Width)
                 and (1, 1);
               --  Insert it
               Di := Digit_Index (Dest_Off + I) / Digit_Width;
               Db := Natural (Dest_Off + I) mod Digit_Width;
               M := Shift_Left (1, Db);
               Dest (Di) := (Dest (Di) and (not M, not M))
                 or Shift_Left (L, Db);
            end loop;
         end;
      end if;
   end Compute_Part_Extract;

   procedure Compute_Bit_Part_Extract (Dest : Bitvec_Ptr;
                                       Dest_Off : Bit_Offset;
                                       Dest_Wd : Width_Type;
                                       Src : Bitvec_Ptr;
                                       Src_Off : Bit_Offset;
                                       Src_Wd : Width_Type)
   is
      pragma Assert (Dest_Wd >= Src_Wd);
   begin
      if Dest_Off = 0 and then Src_Wd = Dest_Wd then
         --  Normal case: no overflow
         declare
            Bit_Shift : constant Natural := Natural (Src_Off mod Digit_Width);
            Words_Shift : constant Digit_Index :=
              Digit_Index (Src_Off / Digit_Width);
            Last : constant Digit_Index := To_Last (Dest_Wd);
         begin
            if Bit_Shift = 0 then
               for I in 0 .. Last loop
                  Dest (I) := Src (I + Words_Shift);
               end loop;
            else
               for I in 0 .. Last loop
                  Dest (I) := (Shift_Right (Src (I + Words_Shift), Bit_Shift)
                                or Shift_Left (Src (I + Words_Shift + 1),
                                               Digit_Width - Bit_Shift));
               end loop;
            end if;
         end;
      else
         --  Some overflow or truncation.
         declare
            Last : constant Digit_Index := To_Last (Dest_Wd);
            L : Uns32;
            M : Uns32;
            Di : Digit_Index;
            Db : Natural;
         begin
            for I in 0 .. Last loop
               Dest (I) := 0;
            end loop;
            if Src_Wd = 0 then
               return;
            end if;

            for I in 0 .. Bit_Offset (Src_Wd - 1) loop
               --  Extract bit #I
               L := Shift_Right (Src (Digit_Index (Src_Off + I) / Digit_Width),
                                 Natural (Src_Off + I) mod Digit_Width)
                 and 1;
               --  Insert it
               Di := Digit_Index (Dest_Off + I) / Digit_Width;
               Db := Natural (Dest_Off + I) mod Digit_Width;
               M := Shift_Left (1, Db);
               Dest (Di) := (Dest (Di) and (not M)) or Shift_Left (L, Db);
            end loop;
         end;
      end if;
   end Compute_Bit_Part_Extract;

   procedure Compute_Part_Select
     (Res : Logvec_Ptr; Val : Logvec_Ptr; Off : Bit_Offset; Width : Width_Type)
   is
      Bit_Shift : constant Natural := Natural (Off mod Digit_Width);
      Words_Shift : constant Digit_Index := Digit_Index (Off / Digit_Width);
      Last : constant Digit_Index := To_Last (Width);
   begin
      if Bit_Shift = 0 then
         for I in 0 .. Last loop
            Res (I) := Val (I + Words_Shift);
         end loop;
      else
         for I in 0 .. Last loop
            Res (I) := (Shift_Right (Val (I + Words_Shift), Bit_Shift)
                          or Shift_Left (Val (I + Words_Shift + 1),
                                         Digit_Width - Bit_Shift));
         end loop;
      end if;
   end Compute_Part_Select;

   procedure Compute_Part_Insert (Dst : Logvec_Ptr;
                                  Dst_Off : Bit_Offset;
                                  Src : Logvec_Ptr;
                                  Src_Off : Bit_Offset;
                                  Width : Width_Type;
                                  Change : out Boolean)
   is
      Bit_Shift : constant Natural := Natural (Dst_Off mod Digit_Width);
      Words_Shift : constant Digit_Index :=
        Digit_Index (Dst_Off / Digit_Width);
      Last_Width : constant Natural := Natural (Width mod Digit_Width);
      Last : constant Digit_Index := To_Last (Width);
   begin
      Change := False;
      if Width = 0 then
         return;
      end if;

      if Src_Off = 0 then
         if Bit_Shift = 0 then
            --  Simple: no shift.
            if Last_Width = 0 then
               --  Even simpler: only words.
               for I in 0 .. Last loop
                  Change := Change or (Dst (I + Words_Shift) /= Src (I));
                  Dst (I + Words_Shift) := Src (I);
               end loop;
            else
               for I in 0 .. Last - 1 loop
                  Change := Change or (Dst (I + Words_Shift) /= Src (I));
                  Dst (I + Words_Shift) := Src (I);
               end loop;
               declare
                  Mask : constant Uns32 := Shift_Left (1, Last_Width) - 1;
                  Old : constant Logic_32 := Dst (Last + Words_Shift);
               begin
                  Change := Change
                    or ((Old and Mask) /= (Src (Last) and Mask));
                  Dst (Last + Words_Shift) :=
                    (Old and not Mask) or (Src (Last) and Mask);
               end;
            end if;
         else
            --  General: shift.
            declare
               Mask : Uns32;
               W : Width_Type;
               Idx : Digit_Index;
               Old : Logic_32;
               Ins : Logic_32;
            begin
               W := Width;
               Idx := 0;
               Mask := Shift_Left (1, Bit_Shift) - 1;
               while W > Digit_Width loop
                  --  Insert low part of VAL(Idx) to RES.
                  Old := Dst (Idx + Words_Shift);
                  Ins := Shift_Left (Src (Idx), Bit_Shift);
                  Change := Change or ((Old and not Mask) /= Ins);
                  Dst (Idx + Words_Shift) := (Old and Mask) or Ins;
                  Idx := Idx + 1;

                  --  Insert high part of VAL(Idx)
                  Old := Dst (Idx + Words_Shift);
                  Ins := Shift_Right (Src (Idx), Bit_Shift);
                  Change := Change or ((Old and Mask) /= Ins);
                  Dst (Idx + Words_Shift) := (Old and (not Mask)) or Ins;
                  W := W - Digit_Width;
               end loop;
               if W > 0 then
                  --  The remaining.
                  if W + Width_Type (Bit_Shift) > Digit_Width then
                     --  Needs to be written on 2 words.
                     Old := Dst (Idx + Words_Shift);
                     Ins := Shift_Left (Src (Idx), Bit_Shift);
                     Change := Change or ((Old and not Mask) /= Ins);
                     Dst (Idx + Words_Shift) := (Old and Mask) or Ins;
                     W := W - (Digit_Width - Width_Type (Bit_Shift));

                     Mask := Shift_Left (1, Natural (W)) - 1;
                     Old := Dst (Idx + Words_Shift + 1);
                     Ins := Shift_Right (Src (Idx), Bit_Shift) and Mask;
                     Change := Change or ((Old and Mask) /= Ins);
                     Dst (Idx + Words_Shift + 1) :=
                       (Old and (not Mask)) or Ins;
                  else
                     Mask := Shift_Left (Shift_Left (1, Natural (W)) - 1,
                                         Bit_Shift);
                     Old := Dst (Idx + Words_Shift);
                     Ins := Shift_Left (Src (Idx), Bit_Shift) and Mask;
                     Change := Change or ((Old and Mask) /= Ins);
                     Dst (Idx + Words_Shift) := (Old and (not Mask)) or Ins;
                  end if;
               end if;
            end;
         end if;
      else
         --  TODO: optimize
         --  Bit per bit.
         declare
            L : Logic_32;
            M : Uns32;
            Di : Digit_Index;
            Db : Natural;
         begin
            for I in 0 .. Bit_Offset (Width - 1) loop
               --  Extract bit #I
               L := Shift_Right (Src (Digit_Index (Src_Off + I) / Digit_Width),
                                 Natural (Src_Off + I) mod Digit_Width)
                 and (1, 1);
               --  Insert it
               Di := Digit_Index (Dst_Off + I) / Digit_Width;
               Db := Natural (Dst_Off + I) mod Digit_Width;
               L := Shift_Left (L, Db);
               M := Shift_Left (1, Db);
               Change := Change or ((Dst (Di) and (M, M)) /= L);
               Dst (Di) := (Dst (Di) and (not M, not M)) or L;
            end loop;
         end;
      end if;
   end Compute_Part_Insert;

   procedure Compute_Log_Bit_Part_Insert (Dst : Logvec_Ptr;
                                          Dst_Off : Bit_Offset;
                                          Src : Bitvec_Ptr;
                                          Width : Width_Type)
   is
      Bit_Shift : constant Natural := Natural (Dst_Off mod Digit_Width);
      Words_Shift : constant Digit_Index :=
        Digit_Index (Dst_Off / Digit_Width);
      Last_Width : constant Natural := Natural (Width mod Digit_Width);
      Last : constant Digit_Index := To_Last (Width);
   begin
      if Width = 0 then
         return;
      end if;

      if Bit_Shift = 0 then
         --  Simple: no shift.
         if Last_Width = 0 then
            --  Even simpler: only words.
            for I in 0 .. Last loop
               Dst (I + Words_Shift) := (Src (I), 0);
            end loop;
         else
            for I in 0 .. Last - 1 loop
               Dst (I + Words_Shift) := (Src (I), 0);
            end loop;
            declare
               Mask : constant Uns32 := Shift_Left (1, Last_Width) - 1;
               Old : constant Logic_32 := Dst (Last + Words_Shift);
            begin
               Dst (Last + Words_Shift) :=
                 ((Old.Val and not Mask) or (Src (Last) and Mask),
                  Old.Zx and not Mask);
            end;
         end if;
      else
         --  General: shift.
         declare
            Mask : Uns32;
            W : Width_Type;
            Idx : Digit_Index;
            Old : Logic_32;
            Ins : Bitvec_Digit;
         begin
            W := Width;
            Idx := 0;
            Mask := Shift_Left (1, Bit_Shift) - 1;
            while W > Digit_Width loop
               --  Insert low part of VAL(Idx) to RES.
               Old := Dst (Idx + Words_Shift);
               Ins := Shift_Left (Src (Idx), Bit_Shift);
               Dst (Idx + Words_Shift) := (Old and Mask) or (Ins, 0);
               Idx := Idx + 1;

               --  Insert high part of VAL(Idx)
               Old := Dst (Idx + Words_Shift);
               Ins := Shift_Right (Src (Idx), Bit_Shift);
               Dst (Idx + Words_Shift) := (Old and (not Mask)) or (Ins, 0);
               W := W - Digit_Width;
            end loop;
            if W > 0 then
               --  The remaining.
               if W + Width_Type (Bit_Shift) > Digit_Width then
                  --  Needs to be written on 2 words.
                  Old := Dst (Idx + Words_Shift);
                  Ins := Shift_Left (Src (Idx), Bit_Shift);
                  Dst (Idx + Words_Shift) := (Old and Mask) or (Ins, 0);
                  W := W - (Digit_Width - Width_Type (Bit_Shift));

                  Mask := Shift_Left (1, Natural (W)) - 1;
                  Old := Dst (Idx + Words_Shift + 1);
                  Ins := Shift_Right (Src (Idx), Bit_Shift) and Mask;
                  Dst (Idx + Words_Shift + 1) :=
                    (Old and (not Mask)) or (Ins, 0);
               else
                  Mask := Shift_Left (Shift_Left (1, Natural (W)) - 1,
                                      Bit_Shift);
                  Old := Dst (Idx + Words_Shift);
                  Ins := Shift_Left (Src (Idx), Bit_Shift) and Mask;
                  Dst (Idx + Words_Shift) := (Old and (not Mask)) or (Ins, 0);
               end if;
            end if;
         end;
      end if;
   end Compute_Log_Bit_Part_Insert;

   function Compute_Bit_Select (Val : Logvec_Ptr; Off : Bit_Offset)
                               return Logic_Type
   is
      Off_Bit : constant Natural := Natural (Off mod Digit_Width);
      Off_Dig : constant Digit_Index := Digit_Index (Off / Digit_Width);
      D : constant Logic_32 := Val (Off_Dig);
   begin
      return To_Logic (Shift_Right (D.Val, Off_Bit),
                       Shift_Right (D.Zx, Off_Bit));
   end Compute_Bit_Select;

   function Compute_Bit_Select (Val : Bitvec_Ptr; Off : Bit_Offset)
                               return Bit_Type
   is
      Off_Bit : constant Natural := Natural (Off mod Digit_Width);
      Off_Dig : constant Digit_Index := Digit_Index (Off / Digit_Width);
      D : constant Uns32 := Val (Off_Dig);
   begin
      return Bit_Type'Val (Shift_Right (D, Off_Bit) and 1);
   end Compute_Bit_Select;

   procedure Compute_Log_Insert (Res : Logvec_Ptr;
                                 Off : Bit_Offset;
                                 Val : Logic_Type;
                                 Change : out Boolean)
   is
      Bit_Shift : constant Natural := Natural (Off mod Digit_Width);
      Idx : constant Digit_Index := Digit_Index (Off / Digit_Width);
      Mask : constant Uns32 := Shift_Left (1, Bit_Shift);
      V : constant Uns32 := Logic_Type'Pos (Val);
      Bval : Uns32;
   begin
      Bval := Shift_Left ((V and 1), Bit_Shift);
      Change := (Res (Idx).Val and Mask) /= Bval;
      Res (Idx).Val := (Res (Idx).Val and not Mask) or Bval;
      Bval := Shift_Left (Shift_Right (V, 1), Bit_Shift);
      Change := Change or ((Res (Idx).Zx and Mask) /= Bval);
      Res (Idx).Zx := (Res (Idx).Zx and not Mask) or Bval;
   end Compute_Log_Insert;

   procedure Compute_Bit_Insert (Res : Bitvec_Ptr;
                                 Off : Bit_Offset;
                                 Val : Bit_Type;
                                 Change : out Boolean)
   is
      Bit_Shift : constant Natural := Natural (Off mod Digit_Width);
      Idx : constant Digit_Index := Digit_Index (Off / Digit_Width);
      Mask : constant Uns32 := Shift_Left (1, Bit_Shift);
      V : constant Uns32 := Bit_Type'Pos (Val);
      Bval : Uns32;
   begin
      Bval := Shift_Left (V, Bit_Shift);
      Change := (Res (Idx) and Mask) /= Bval;
      Res (Idx) := (Res (Idx) and not Mask) or Bval;
   end Compute_Bit_Insert;

   function Is_Eq (Left, Right : Logvec_Ptr; Width : Width_Type) return Boolean
   is
      Sz : Width_Type;
      Mask : Uns32;
      P : Digit_Index;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
         if (Left (P).Val and Mask) /= (Right (P).Val and Mask)
           or (Left (P).Zx and Mask) /= (Right (P).Zx and Mask)
         then
            return False;
         end if;
         if P = 0 then
            return True;
         end if;
         P := P - 1;
      end if;

      loop
         if Left (P).Val /= Right (P).Val or Left (P).Zx /= Right (P).Zx then
            return False;
         end if;
         exit when P = 0;
         P := P - 1;
      end loop;
      return True;
   end Is_Eq;

   function Is_Eq (Left, Right : Bitvec_Ptr; Width : Width_Type) return Boolean
   is
      Sz : Width_Type;
      Mask : Uns32;
      P : Digit_Index;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
         if (Left (P) and Mask) /= (Right (P) and Mask) then
            return False;
         end if;
         if P = 0 then
            return True;
         end if;
         P := P - 1;
      end if;

      loop
         if Left (P) /= Right (P) then
            return False;
         end if;
         exit when P = 0;
         P := P - 1;
      end loop;
      return True;
   end Is_Eq;

   function Is_Eqx (Left, Right : Logvec_Ptr; Width : Width_Type)
                   return Boolean
   is
      Sz : Width_Type;
      Mask : Uns32;
      P : Digit_Index;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
      else
         Mask := not 0;
      end if;

      loop
         Mask := Mask and not (Left (P).Zx and Left (P).Val);
         Mask := Mask and not (Right (P).Zx and Right (P).Val);
         if (Left (P).Val and Mask) /= (Right (P).Val and Mask)
           or (Left (P).Zx and Mask) /= (Right (P).Zx and Mask)
         then
            return False;
         end if;
         exit when P = 0;
         P := P - 1;
         Mask := not 0;
      end loop;
      return True;
   end Is_Eqx;

   function Is_Eqz (Left, Right : Logvec_Ptr; Width : Width_Type)
                   return Boolean
   is
      Sz : Width_Type;
      Mask : Uns32;
      P : Digit_Index;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
      else
         Mask := not 0;
      end if;

      loop
         Mask := Mask and not (Left (P).Zx or Right (P).Zx);
         if (Left (P).Val and Mask) /= (Right (P).Val and Mask)
           or (Left (P).Zx and Mask) /= (Right (P).Zx and Mask)
         then
            return False;
         end if;
         exit when P = 0;
         P := P - 1;
         Mask := not 0;
      end loop;
      return True;
   end Is_Eqz;

   function Compute_Case
     (Left, Right : Logvec_Ptr; Width : Width_Type; Eq : Boolean)
     return Logic_Type
   is
      Res : Boolean;
   begin
      Res := Is_Eq (Left, Right, Width);
      return Logic_Type'Val (Boolean'Pos (Res xor not Eq));
   end Compute_Case;

   function Compute_Log_Eq
     (Left, Right : Logvec_Ptr; Width : Width_Type; Eq : Boolean)
     return Logic_Type
   is
      Res : Boolean;
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         return V_X;
      end if;
      Res := Is_Eq (Left, Right, Width);
      return Logic_Type'Val (Boolean'Pos (Res xor not Eq));
   end Compute_Log_Eq;

   function Ucomp (L, R : Logvec_Ptr; Width : Width_Type) return Order_Type
   is
      Sz : Width_Type;
      Mask : Uns32;
      P : Digit_Index;
      Rd, Ld : Uns32;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);
      Ld := L (P).Val;
      Rd := R (P).Val;

      if Sz /= 0 then
         Mask := Shift_Right (not 0, Natural (Digit_Width - Sz));
         Ld := Rd and Mask;
         Rd := Ld and Mask;
      end if;

      loop
         if Ld /= Rd then
            if Ld < Rd then
               return Less;
            else
               return Greater;
            end if;
         end if;
         exit when P = 0;
         P := P - 1;
         Ld := L (P).Val;
         Rd := R (P).Val;
      end loop;
      return Equal;
   end Ucomp;

   function Scomp (L, R : Logvec_Ptr; Width : Width_Type) return Order_Type
   is
      Sz : Width_Type;
      P : Digit_Index;
      Rd, Ld : Uns32;
   begin
      Sz := Width mod Digit_Width;
      P := To_Last (Width);
      Ld := L (P).Val;
      Rd := R (P).Val;

      --  Use signed comparison for the first digit.
      if Sz /= 0 then
         Ld := Sext (Ld, Sz);
         Rd := Sext (Rd, Sz);
      end if;
      if Ld /= Rd then
         if To_Int32 (Ld) < To_Int32 (Rd) then
            return Less;
         else
            return Greater;
         end if;
      end if;

      loop
         exit when P = 0;
         P := P - 1;
         Ld := L (P).Val;
         Rd := R (P).Val;
         if Ld /= Rd then
            if Ld < Rd then
               return Less;
            else
               return Greater;
            end if;
         end if;
      end loop;
      return Equal;
   end Scomp;

   generic
      with function Val_Cmp (L, R : Logvec_Ptr; Width : Width_Type)
                            return Order_Type;
      with function Ord_Cmp (L, R : Order_Type) return Boolean;
   function Gen_Compare
     (Left, Right : Logvec_Ptr; Width : Width_Type) return Logic_Type;

   function Gen_Compare
     (Left, Right : Logvec_Ptr; Width : Width_Type) return Logic_Type
   is
      Res : Boolean;
   begin
      if Has_Unknowns (Left, Width) or else Has_Unknowns (Right, Width) then
         return V_X;
      end if;
      Res := Ord_Cmp (Val_Cmp (Left, Right, Width), Equal);
      return Logic_Type'Val (Boolean'Pos (Res));
   end Gen_Compare;

   function Compute_Ult1 is new Gen_Compare (Ucomp, "<");
   function Compute_Ult (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type
     renames Compute_Ult1;

   function Compute_Ule1 is new Gen_Compare (Ucomp, "<=");
   function Compute_Ule (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type
     renames Compute_Ule1;

   function Compute_Slt1 is new Gen_Compare (Scomp, "<");
   function Compute_Slt (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type
     renames Compute_Slt1;

   function Compute_Sle1 is new Gen_Compare (Scomp, "<=");
   function Compute_Sle (Left, Right : Logvec_Ptr; Width : Width_Type)
                        return Logic_Type
     renames Compute_Sle1;

   procedure Compute_Int32
     (Dest : Logvec_Ptr; Width : Width_Type; Val : Int32)
   is
      L : constant Digit_Index := To_Last (Width);
      S : Uns32;
   begin
      Dest (0) := (Val => To_Uns32 (Val), Zx => 0);
      if L > 0 then
         S := Shift_Right_Arithmetic (To_Uns32 (Val), Digit_Width - 1);
         for I in 1 .. L loop
            Dest (I) := (Val => S, Zx => 0);
         end loop;
      end if;
   end Compute_Int32;

end Verilog.Bignums;
