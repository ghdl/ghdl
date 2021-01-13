--  GHDL Run Time (GRT) - Floating point conversions.
--  Copyright (C) 2017 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Ada.Unchecked_Conversion;

--  Double float (aka binary64 representation):
--  exponent bias is 1023
--
--  |-----------------------------------------------------------
--  | 63   | 62-52    | 51-0     |
--  | sign | exponent | fraction | Value
--  |-----------------------------------------------------------
--  | s    | 0        | f        | (-1)**s * 0.f * 2**(1 - 1023)
--  |-----------------------------------------------------------
--  | s    | 1 - 2046 | f        | (-1)**s * 1.f * 2**(e - 1023)
--  |-----------------------------------------------------------
--  | s    | 2047     | 0        | (-1)**s * inf
--  |-----------------------------------------------------------
--  | s    | 2047     | /= 0     | NaN
--  |-----------------------------------------------------------

--  Implement 'dragon4' algorithm described in:
--    Printing Floating-Point Numbers Quickly and Accurately
--    Robert G. Burger  and  R. Kent Dybvig
--  (http://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf)
--
--  Notes:
--  - Input radix is 2 (so b = 2)

package body Grt.Fcvt is
   function F64_To_U64 is new Ada.Unchecked_Conversion
     (IEEE_Float_64, Unsigned_64);

   type Fcvt_Context is record
      --  Inputs
      --  v = f * 2**e
      F : Bignum;
      E : Integer;
      B : Natural;

      --  Deduced from the input (for table1)
      Is_Pow2 : Boolean;
      Is_Emin : Boolean;

      --  If true, Mp = Mm (often the case).  In that case Mm is not set.
      Equal_M : Boolean;

      --  Log2 (v).  Used to estimate k.
      Log2v : Integer;

      --  Internal variables
      --  v = r / s
      K : Integer;
      R : Bignum;
      S : Bignum;
      Mp : Bignum;
      Mm : Bignum;
   end record;

   procedure Bignum_Normalize (Bn : in out Bignum) is
   begin
      while Bn.N > 0 loop
         exit when Bn.V (Bn.N) /= 0;
         Bn.N := Bn.N - 1;
      end loop;
   end Bignum_Normalize;

   --  Check invariant within a bignum.
   function Bignum_Is_Valid (Bn : Bignum) return Boolean is
   begin
      return Bn.N <= Bn.V'Last
        and then (Bn.N = 0 or else Bn.V (Bn.N) /= 0);
   end Bignum_Is_Valid;

   --  Create a bignum from a natural.
   procedure Bignum_Int (Res : out Bignum; N : Natural) is
   begin
      if N = 0 then
         Res.N := 0;
      else
         Res.N := 1;
         Res.V (1) := Unsigned_32 (N);
      end if;
   end Bignum_Int;

   procedure Bignum_To_Int
     (N : Bignum; Res : out Unsigned_64; OK : out Boolean) is
   begin
      OK := True;
      case N.N is
         when 0 =>
            Res := 0;
         when 1 =>
            Res := Unsigned_64 (N.V (1));
         when 2 =>
            Res := Shift_Left (Unsigned_64 (N.V (2)), 32)
              or Unsigned_64 (N.V (1));
         when others =>
            Res := 0;
            OK := False;
      end case;
   end Bignum_To_Int;

   --  Add two bignums, assuming A > B.
   function Bignum_Add2 (A, B : Bignum) return Bignum
   is
      pragma Assert (A.N >= B.N);
      Res : Bignum;
      Tmp : Unsigned_64;
   begin
      Tmp := 0;
      for I in 1 .. A.N loop
         Tmp := Tmp + Unsigned_64 (A.V (I));
         if I <= B.N then
            Tmp := Tmp + Unsigned_64 (B.V (I));
         end if;
         Res.V (I) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         Tmp := Shift_Right (Tmp, 32);
      end loop;
      if Tmp /= 0 then
         Res.V (A.N + 1) := Unsigned_32 (Tmp);
         Res.N := A.N + 1;
      else
         Res.N := A.N;
      end if;

      return Res;
   end Bignum_Add2;

   --  Add two bignums.
   function Bignum_Add (A, B : Bignum) return Bignum is
   begin
      if A.N >= B.N then
         return Bignum_Add2 (A, B);
      else
         return Bignum_Add2 (A => B, B => A);
      end if;
   end Bignum_Add;

   type Compare_Type is (Lt, Eq, Gt);

   --  Compare two bignums.
   function Bignum_Compare (L, R : Bignum) return Compare_Type is
   begin
      if L.N /= R.N then
         if L.N > R.N then
            return Gt;
         else
            return Lt;
         end if;
      end if;

      --  Same number of digits.
      for I in reverse 1 .. L.N loop
         if L.V (I) /= R.V (I) then
            if L.V (I) > R.V (I) then
               return Gt;
            else
               return Lt;
            end if;
         end if;
      end loop;

      return Eq;
   end Bignum_Compare;

   --  Multiply two bignums.
   function Bignum_Mul (L, R : Bignum) return Bignum
   is
      Res : Bignum;

      Tmp : Unsigned_64;
   begin
      --  Upper bound.
      Res.N := L.N + R.N;

      for I in 1 .. Res.N loop
         Res.V (I) := 0;
      end loop;

      for I in 1 .. R.N loop
         Tmp := 0;
         for J in 1 .. L.N loop
            Tmp := Tmp + Unsigned_64 (R.V (I)) * Unsigned_64 (L.V (J))
              + Unsigned_64 (Res.V (I + J - 1));
            Res.V (I + J - 1) := Unsigned_32 (Tmp and 16#ffff_ffff#);
            Tmp := Shift_Right (Tmp, 32);
         end loop;
         if Tmp /= 0 then
            Res.V (I + L.N + 1 - 1) := Unsigned_32 (Tmp);
         end if;
      end loop;

      Bignum_Normalize (Res);
      return Res;
   end Bignum_Mul;

   function Bignum_Mul_Int (L : Bignum; R : Positive; Carry_In : Natural := 0)
                           return Bignum
   is
      Res : Bignum;

      Tmp : Unsigned_64;
   begin
      Tmp := Unsigned_64 (Carry_In);

      for I in 1 .. L.N loop
         Tmp := Tmp + Unsigned_64 (L.V (I)) * Unsigned_64 (R);
         Res.V (I) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         Tmp := Shift_Right (Tmp, 32);
      end loop;

      if Tmp = 0 then
         Res.N := L.N;
      else
         Res.N := L.N + 1;
         Res.V (Res.N) := Unsigned_32 (Tmp);
      end if;

      pragma Assert (Bignum_Is_Valid (Res));
      return Res;
   end Bignum_Mul_Int;

   --  In place multiplication.
   procedure Bignum_Mul_Int
     (L : in out Bignum; R : Positive; Carry_In : Natural := 0)
   is
      Tmp : Unsigned_64;
   begin
      Tmp := Unsigned_64 (Carry_In);

      for I in 1 .. L.N loop
         Tmp := Tmp + Unsigned_64 (L.V (I)) * Unsigned_64 (R);
         L.V (I) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         Tmp := Shift_Right (Tmp, 32);
      end loop;

      if Tmp > 0 then
         L.N := L.N + 1;
         L.V (L.N) := Unsigned_32 (Tmp);
      end if;

      pragma Assert (Bignum_Is_Valid (L));
   end Bignum_Mul_Int;

   --  Compute 2**N
   function Bignum_Pow2 (N : Natural) return Bignum
   is
      Res : Bignum;
   begin
      Res.N := 1 + (N / 32);
      for I in 1 .. Res.N loop
         Res.V (I) := 0;
      end loop;
      Res.V (Res.N) := Shift_Left (1, N mod 32);
      return Res;
   end Bignum_Pow2;

   --  Compute L**N
   function Bignum_Pow (L : Natural; N : Natural) return Bignum
   is
      Res : Bignum;
      N1 : Natural;
      T : Bignum;
   begin
      Bignum_Int (Res, 1);

      Bignum_Int (T, L);
      N1 := N;
      loop
         if N1 mod 2 = 1 then
            Res := Bignum_Mul (Res, T);
         end if;
         N1 := N1 / 2;
         exit when N1 = 0;
         T := Bignum_Mul (T, T);
      end loop;

      return Res;
   end Bignum_Pow;

   --  TODO: non-restoring division
   procedure Bignum_Divstep (N : in out Bignum;
                             Div : Bignum;
                             D : out Boolean)
   is
      Tmp : Unsigned_64;
   begin
      if N.N < Div.N then
         D := False;
         return;
      end if;

      Tmp := 0;
      for I in 1 .. Div.N loop
         Tmp := Tmp + (Unsigned_64 (N.V (I)) - Unsigned_64 (Div.V (I)));
         N.V (I) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         Tmp := Shift_Right_Arithmetic (Tmp, 32);
      end loop;
      if N.N > Div.N then
         Tmp := Tmp + Unsigned_64 (N.V (N.N));
         N.V (N.N) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         Tmp := Shift_Right_Arithmetic (Tmp, 32);
      end if;
      if Tmp = 0 then
         Bignum_Normalize (N);
         D := True;
      else
         --  Restore
         Tmp := 0;
         for I in 1 .. Div.N loop
            Tmp := Tmp + (Unsigned_64 (N.V (I)) + Unsigned_64 (Div.V (I)));
            N.V (I) := Unsigned_32 (Tmp and 16#ffff_ffff#);
            Tmp := Shift_Right_Arithmetic (Tmp, 32);
         end loop;
         if N.N > Div.N then
            Tmp := Tmp + Unsigned_64 (N.V (N.N));
            N.V (N.N) := Unsigned_32 (Tmp and 16#ffff_ffff#);
         end if;
         D := False;
      end if;
   end Bignum_Divstep;

   --  N := N * 2
   procedure Bignum_Mul2 (N : in out Bignum)
   is
      Carry, Carry1 : Unsigned_32;
      V : Unsigned_32;
   begin
      if N.N = 0 then
         return;
      end if;

      Carry := 0;
      for I in 1 .. N.N loop
         V := N.V (I);
         Carry1 := Shift_Right (V, 31);
         V := Shift_Left (V, 1) or Carry;
         N.V (I) := V;
         Carry := Carry1;
      end loop;
      if Carry /= 0 then
         N.N := N.N + 1;
         N.V (N.N) := Carry;
      end if;
   end Bignum_Mul2;

   function Ffs (V : Unsigned_32) return Natural
   is
      T : Unsigned_32;
      Res : Natural;
   begin
      if V = 0 then
         return 0;
      end if;

      --  Compute clz (Count Leading Zero).
      T := V;
      Res := 0;
      if (T and 16#ffff_0000#) = 0 then
         T := Shift_Left (T, 16);
         Res := Res + 16;
      end if;
      if (T and 16#ff00_0000#) = 0 then
         T := Shift_Left (T, 8);
         Res := Res + 8;
      end if;
      if (T and 16#f000_0000#) = 0 then
         T := Shift_Left (T, 4);
         Res := Res + 4;
      end if;
      if (T and 16#c000_0000#) = 0 then
         T := Shift_Left (T, 2);
         Res := Res + 2;
      end if;
      if (T and 16#8000_0000#) = 0 then
         Res := Res + 1;
      end if;
      return 32 - Res;
   end Ffs;

   --  Convert F to M*2**E, M having P bits of precision (2**P > M >= 2**(P-1))
   --  P < 64
   procedure Bignum_To_Fp (F : Bignum;
                           P : Natural;
                           M : out Unsigned_64;
                           E : out Integer)
   is
      Nbits : Natural;
      P1 : Natural;
      MSW : Unsigned_32;
      MSW_Pos : Natural;
      R : Unsigned_32;
      Carry : Boolean;
   begin
      if F.N = 0 then
         M := 0;
         E := 0;
         return;
      end if;

      --  MSW_Pos is the position of the word from which R is extracted.
      MSW_Pos := F.N;
      MSW := F.V (MSW_Pos);
      pragma Assert (MSW /= 0);
      Nbits := Ffs (MSW);
      P1 := P;
      M := 0;

      E := Nbits + (F.N - 1) * 32 - P;

      if Nbits > P1 then
         M := Unsigned_64 (Shift_Right (MSW, Nbits - P1));
         R := Shift_Left (MSW, 32 - (Nbits - P1));
      else
         M := Shift_Left (Unsigned_64 (MSW), P1 - Nbits);
         P1 := P1 - Nbits;
         loop
            MSW_Pos := MSW_Pos - 1;
            if MSW_Pos = 0 then
               --  No more input bits.
               R := 0;
               exit;
            end if;
            MSW:= F.V (MSW_Pos);
            if P1 = 0 then
               --  No more bits to shift in.
               R := MSW;
               exit;
            end if;
            if P1 < 32 then
               M := M or Shift_Right (Unsigned_64 (MSW), 32 - P1);
               R := Shift_Left (MSW, P1);
               P1 := 0;
               exit;
            else
               M := M or Shift_Left (Unsigned_64 (MSW), P1 - 32);
               P1 := P1 - 32;
            end if;
         end loop;
      end if;

      --  Round.
      if R > 16#8000_0000# then
         Carry := True;
      elsif R < 16#8000_0000# then
         Carry := False;
      else
         --  Tie.
         loop
            --  MSW_Pos = 0 means R was 0.
            pragma Assert (MSW_Pos /= 0);

            if MSW_Pos = 1 then
               --  R was extracted from the first word of F.  No more input
               --  bits.

               --  When exactely half in the middle, truncate.
               Carry := False;
               exit;
            end if;

            MSW_Pos := MSW_Pos - 1;
            if F.V (MSW_Pos) /= 0 then
               Carry := True;
               exit;
            end if;
            MSW_Pos := MSW_Pos - 1;
         end loop;
      end if;

      if Carry then
         M := M + 1;
         if M >= Shift_Left (1, P) then
            E := E + 1;
            M := Shift_Right (M, 1);
         end if;
      end if;
   end Bignum_To_Fp;

   --  Multiply N by 2**(32 * Count)
   procedure Bignum_Shift32_Left (N : in out Bignum; Count : Natural) is
   begin
      for I in reverse 1 .. N.N loop
         N.V (I + Count) := N.V (I);
      end loop;
      for I in 1 .. Count loop
         N.V (I) := 0;
      end loop;
      N.N := N.N + Count;
   end Bignum_Shift32_Left;

   --  Compute F / Div = M * 2**E, with 2**Precision > M >= 2**(Precision-1)
   procedure Bignum_Divide_To_Fp (F : in out Bignum;
                                  Div : in out Bignum;
                                  Precision : Natural;
                                  M : out Unsigned_64;
                                  E : out Integer)
   is
      Ediff : constant Integer := Div.N - (F.N + 1);
      Dig : Boolean;
   begin
      --  Adjust exponents so that Div.N = F.N + 1
      E := -Precision + 1;
      if Ediff > 0 then
         --  Divider is larger
         E := E - (32 * Ediff);
         Bignum_Shift32_Left (F, Ediff);
      elsif Ediff < 0 then
         E := E - (32 * Ediff);
         Bignum_Shift32_Left (Div, -Ediff);
      end if;
      pragma Assert (Div.N > F.N);

      --  Divide until the first 1.
      loop
         Bignum_Divstep (F, Div, Dig);
         Bignum_Mul2 (F);
         exit when Dig;
         E := E - 1;
      end loop;

      M := 1;

      --  Do precision steps
      for I in 1 .. Precision - 1 loop
         Bignum_Divstep (F, Div, Dig);
         Bignum_Mul2 (F);
         M := 2*M + Boolean'Pos (Dig);
      end loop;

      --  Round.
      Bignum_Divstep (F, Div, Dig);
      if Dig then
         M := M + 1;
         if M = Shift_Left (1, Precision) then
            M := M / 2;
            E := E + 1;
         end if;
      end if;
   end Bignum_Divide_To_Fp;

   procedure Append (Str : in out String;
                     Len : in out Natural;
                     C : Character)
   is
      P : constant Positive := Str'First + Len;
   begin
      if P <= Str'Last then
         Str (P) := C;
      end if;
      Len := Len + 1;
   end Append;

   procedure Append_Digit (Str : in out String;
                           Len : in out Natural;
                           D : Natural) is
   begin
      if D < 10 then
         Append (Str, Len, Character'Val (Character'Pos ('0') + D));
      else
         Append (Str, Len, Character'Val (Character'Pos ('a') + D - 10));
      end if;
   end Append_Digit;

   --  Implement Table 1
   procedure Dragon4_Prepare (Ctxt : in out Fcvt_Context)
   is
      Log2_S0 : Natural;
   begin
      if Ctxt.E >= 0 then
         --  Case e >= 0
         if not Ctxt.Is_Pow2 then
            --  Case f /= b**(p-1):
            --  r = f * b**e * 2
            --  s = 2
            --  m+ = b**e
            --  m- = b**e
            Ctxt.R := Bignum_Mul (Ctxt.F, Bignum_Pow2 (Ctxt.E + 1));
            Bignum_Int (Ctxt.S, 2);
            Log2_S0 := 1;
            Ctxt.Mp := Bignum_Pow2 (Ctxt.E);
            Ctxt.Equal_M := True;
         else
            --  Case f = b**(p-1)
            --  r = f * b**(e+1) * 2
            --  s = b * 2
            --  m+ = b**(e+1)
            --  m- = b**e
            Ctxt.R := Bignum_Mul (Ctxt.F, Bignum_Pow2 (Ctxt.E + 1 + 1));
            Bignum_Int (Ctxt.S, 2 * 2);
            Log2_S0 := 2;
            Ctxt.Mp := Bignum_Pow2 (Ctxt.E + 1);
            Ctxt.Mm := Bignum_Pow2 (Ctxt.E);
            Ctxt.Equal_M := False;
         end if;
      else
         --  Case e < 0
         if Ctxt.Is_Emin or not Ctxt.Is_Pow2 then
            --  Case e = min exp  or  f /= b**(p-1)
            --  r = f * 2
            --  s = b**(-e) * 2  = b**(-e + 1)
            --  m+ = 1
            --  m- = 1
            Ctxt.R := Bignum_Mul_Int (Ctxt.F, 2);
            Log2_S0 := -Ctxt.E + 1;
            Bignum_Int (Ctxt.Mp, 1);
            Ctxt.Equal_M := True;
         else
            --  Case e > min exp  and  f = b**(p-1)
            --  r = f * b * 2
            --  s = b**(-e+1) * 2  = b**(-e+1+1)
            --  m+ = b
            --  m- = 1
            Ctxt.R := Bignum_Mul_Int (Ctxt.F, 2 * 2);
            Log2_S0 := -Ctxt.E + 1 + 1;
            Bignum_Int (Ctxt.Mp, 2);
            Bignum_Int (Ctxt.Mm, 1);
            Ctxt.Equal_M := False;
         end if;
         Ctxt.S := Bignum_Pow2 (Log2_S0);
      end if;
   end Dragon4_Prepare;

   procedure Dragon4_Fixup (Ctxt : in out Fcvt_Context)
   is
   begin
      if Bignum_Compare (Bignum_Add (Ctxt.R, Ctxt.Mp), Ctxt.S) = Gt then
         Ctxt.K := Ctxt.K + 1;
      else
         Bignum_Mul_Int (Ctxt.R, 10);
         Bignum_Mul_Int (Ctxt.Mp, 10);
         if not Ctxt.Equal_M then
            Bignum_Mul_Int (Ctxt.Mm, 10);
         end if;
      end if;
   end Dragon4_Fixup;

   --  2. Find the smallest integer k such that (r + m+) / s <= B**k; ie:
   --     k = ceil (logB ((r+m+)/s))
   --  3. If k >= 0, let r0 = r, s0 = s * B**k,  m0+=m+      and m0-=m-
   --     If k < 0,  let r0 = r * B**-k, s0=s, m0+=m+ * B**-k, m0-=m- * B**-k
   --
   --  Note:
   --   with high = (r+m+)/s:
   --     k = ceil (logB (high))
   procedure Dragon4_Scale (Ctxt : in out Fcvt_Context)
   is
      L2 : Integer_64;
      T1 : Bignum;
   begin
      --  Estimate k.
      --  log10(2) ~= 0.301029995664
      --  log10(2) * 2**32 ~= 1292913986
      L2 := Integer_64 (Ctxt.Log2v) * 1292913986;
      Ctxt.K := Integer (L2 / 2**32);

      --  Ceiling.
      --  If L2 < 0, L2 rem 2**32 <= 0
      if L2 rem 2**32 > 0 then
         Ctxt.K := Ctxt.K + 1;
      end if;

      --  Need to compute B**k
      if Ctxt.K >= 0 then
         T1 := Bignum_Pow (10, Ctxt.K);
         Ctxt.S := Bignum_Mul (Ctxt.S, T1);
      else
         T1 := Bignum_Pow (10, -Ctxt.K);
         Ctxt.R := Bignum_Mul (Ctxt.R, T1);
         Ctxt.Mp := Bignum_Mul (Ctxt.Mp, T1);
         if not Ctxt.Equal_M then
            Ctxt.Mm := Bignum_Mul (Ctxt.Mm, T1);
         end if;
      end if;
      Dragon4_Fixup (Ctxt);
   end Dragon4_Scale;

   procedure Dragon4_Generate (Str : in out String;
                               Len : in out Natural;
                               Ctxt : in out Fcvt_Context)
   is
      S8 : constant Bignum := Bignum_Mul_Int (Ctxt.S, 8);
      S4 : constant Bignum := Bignum_Mul_Int (Ctxt.S, 4);
      S2 : constant Bignum := Bignum_Mul_Int (Ctxt.S, 2);
      S1 : Bignum renames Ctxt.S;
      D : Natural;
      Step : Boolean;
      Cond1, Cond2 : Boolean;
   begin
      loop
         Bignum_Divstep (Ctxt.R, S8, Step);
         D := Boolean'Pos (Step) * 8;
         Bignum_Divstep (Ctxt.R, S4, Step);
         D := D + Boolean'Pos (Step) * 4;
         Bignum_Divstep (Ctxt.R, S2, Step);
         D := D + Boolean'Pos (Step) * 2;
         Bignum_Divstep (Ctxt.R, S1, Step);
         D := D + Boolean'Pos (Step);

         --  Stop conditions.
         --  Note: there is a typo for condition (2) in the original paper
         --  (was fixed in 2006 - check the publish note at the bottom of the
         --  first page).
         if not Ctxt.Equal_M then
            Cond1 := Bignum_Compare (Ctxt.R, Ctxt.Mm) = Lt;
         else
            Cond1 := Bignum_Compare (Ctxt.R, Ctxt.Mp) = Lt;
         end if;
         Cond2 := Bignum_Compare (Bignum_Add (Ctxt.R, Ctxt.Mp), Ctxt.S) = Gt;
         exit when Cond1 or Cond2;

         Append_Digit (Str, Len, D);

         Bignum_Mul_Int (Ctxt.R, 10);
         Bignum_Mul_Int (Ctxt.Mp, 10);
         if not Ctxt.Equal_M then
            Bignum_Mul_Int (Ctxt.Mm, 10);
         end if;
      end loop;

      if Cond1 and not Cond2 then
         null;
      elsif not Cond1 and Cond2 then
         D := D + 1;
      else
         if Bignum_Compare (Bignum_Mul_Int (Ctxt.R, 2), Ctxt.S) = Gt then
            D := D + 1;
         end if;
      end if;
      Append_Digit (Str, Len, D);
   end Dragon4_Generate;

   procedure Dragon4 (Str : in out String;
                      Len : in out Natural;
                      Ctxt : in out Fcvt_Context)
   is
   begin
      Dragon4_Prepare (Ctxt);
      Dragon4_Scale (Ctxt);
      Dragon4_Generate (Str, Len, Ctxt);
   end Dragon4;

   procedure Output_Nan_Inf (Str : out String;
                             Len : in out Natural;
                             Is_Inf : Boolean) is
   begin
      if Is_Inf then
         --  Infinite
         Append (Str, Len, 'i');
         Append (Str, Len, 'n');
         Append (Str, Len, 'f');
      else
         Append (Str, Len, 'n');
         Append (Str, Len, 'a');
         Append (Str, Len, 'n');
      end if;
   end Output_Nan_Inf;

   procedure To_String (Str : out String;
                        Len : out Natural;
                        Is_Num : out Boolean;
                        Is_Neg : out Boolean;
                        Exp : out Integer;
                        V : IEEE_Float_64)
   is
      pragma Assert (Str'First = 1);

      --  Decompose V (assuming same endianness).
      V_Bits : constant Unsigned_64 := F64_To_U64 (V);
      S : constant Boolean := Shift_Right (V_Bits, 63) = 1;
      M : constant Unsigned_64 := V_Bits and 16#f_ff_ff_ff_ff_ff_ff#;
      E : constant Integer := Integer (Shift_Right (V_Bits, 52) and 16#7_ff#);

      Ctxt : Fcvt_Context;
   begin
      Is_Neg := S;
      Len := 0;

      --  Handle NaN & Inf
      if E = 2047 then
         Output_Nan_Inf (Str, Len, M = 0);
         Is_Num := False;
         return;
      end if;

      --  Normal or denormal float.
      Is_Num := True;

      Ctxt.F.N := 2;
      Ctxt.F.V (1) := Unsigned_32 (M and 16#ffff_ffff#);
      Ctxt.F.V (2) := Unsigned_32 (Shift_Right (M, 32) and 16#ffff_ffff#);
      if E = 0 then
         --  Denormal.
         Ctxt.E := -1022 - 52;

         --  Bignum digits may be 0.
         Bignum_Normalize (Ctxt.F);

         Ctxt.Is_Emin := True;
         Ctxt.Is_Pow2 := False; --  Not needed.

         --  Compute len(M).  Don't use a dichotomy as the distribution is
         --  not uniform but exponential.
         Ctxt.Log2v := -1022 - 53;
         for I in reverse 0 .. 51 loop
            if M >= Shift_Left (1, I) then
               Ctxt.Log2v := -1022 - 53 + I + 1;
               exit;
            end if;
         end loop;
      else
         --  Normal.
         Ctxt.E := E - 1023 - 52;

         --  Implicit leading 1.
         Ctxt.F.V (2) := Ctxt.F.V (2) or 16#10_00_00#;

         Ctxt.Is_Emin := False;
         Ctxt.Is_Pow2 := M = 0;
         Ctxt.Log2v := E - 1023;
      end if;

      pragma Assert (Bignum_Is_Valid (Ctxt.F));

      --  At this point, the number is represented as:
      --  F * 2**K
      if Ctxt.F.N = 0 then
         --  Zero is special, handle it directly.
         Append (Str, Len, '0');
         Ctxt.K := 1;
      else
         Dragon4 (Str, Len, Ctxt);
      end if;

      Exp := Ctxt.K;
   end To_String;

   --  Input is: (-1)**S * M * 2**E
   function Pack (M : Unsigned_64;
                  E : Integer;
                  S : Boolean) return IEEE_Float_64
   is
      function To_IEEE_Float_64 is new Ada.Unchecked_Conversion
        (Unsigned_64, IEEE_Float_64);
      T : Unsigned_64;
   begin
      pragma Assert (M < 16#20_00_00_00_00_00_00#);
      if M = 0 then
         T := 0;
      else
         pragma Assert (M >= 16#10_00_00_00_00_00_00#);
         --  Note: input is (-1)**S * 1.FFFFF... * 2**(E + 52)
         if E + 52 + 1023 >= 2047 then
            --  Above greatest IEEE number, use Inf.
            T := 16#7ff_0000000000000#;
         elsif E + 52 + 1023 < 1 then
            --  Denormal
            if E + 52 + 1023 < -52 then
               --  Below small IEEE number, use 0
               T := 0;
            else
               --  Denormal
               T := Shift_Right (M, 52 + E + 52 + 1023);
            end if;
         else
            --  Normal
            T := M and 16#f_ff_ff_ff_ff_ff_ff#;
            T := T or Shift_Left (Unsigned_64 (E + 52 + 1023), 52);
         end if;
      end if;

      if S then
         T := T or Shift_Left (1, 63);
      end if;

      return To_IEEE_Float_64 (T);
   end Pack;

   --  Return (-1)**Neg * F * BASE**EXP to a float.
   function To_Float_64
     (Neg : Boolean; F : Bignum; Base : Positive; Exp : Integer)
     return IEEE_Float_64
   is
      M : Unsigned_64;
      T : Bignum;
      Frac : Bignum;
      E : Integer;
   begin
      if F.N = 0 then
         --  0 is always special...
         M := 0;
         E := 0;
      elsif Exp >= 0 then
         --  TODO: Multiply by 2**EXP * 5**EXP
         Frac := Bignum_Mul (F, Bignum_Pow (Base, Exp));
         Bignum_To_Fp (Frac, 53, M, E);
      else
         T := Bignum_Pow (Base, -Exp);
         --  M = F / 10**-Exp
         --    = F / T
         Frac := F;
         Bignum_Divide_To_Fp (Frac, T, 53, M, E);
      end if;

      return Pack (M, E, Neg);
   end To_Float_64;

   function From_String (Str : String) return IEEE_Float_64
   is
      P : Positive;
      C : Character;

      Neg : Boolean;
      Nbr_Digits : Natural;
      Point_Position : Integer;

      F : Bignum;
      Exp : Integer;
      Exp_Neg : Boolean;
   begin
      Neg := False;

      P := Str'First;

      --  A correctly formatted number has at least one character.

      --  Leading (and optional) sign.
      C := Str (P);
      if C = '-' then
         Neg := True;
         P := P + 1;
         C := Str (P);
      elsif C = '+' then
         P := P + 1;
         C := Str (P);
      end if;

      Nbr_Digits := 0;
      Point_Position := -1;
      F.N := 0;
      loop
         case C is
            when '0' .. '9' =>
               F := Bignum_Mul_Int
                 (F, 10, Character'Pos (C) - Character'Pos ('0'));
               Nbr_Digits := Nbr_Digits + 1;
            when '.' =>
               Point_Position := Nbr_Digits;
            when '_' =>
               null;
            when 'e' | 'E' =>
               Exp := 0;
               exit;
            when others =>
               raise Constraint_Error;
         end case;
         P := P + 1;
         if P > Str'Last then
            Exp := -1;
            exit;
         end if;
         C := Str (P);
      end loop;

      if Exp = 0 then
         P := P + 1;
         C := Str (P);

         --  Sign of the exponent.
         Exp_Neg := False;
         if C = '-' then
            Exp_Neg := True;
            P := P + 1;
            C := Str (P);
         elsif C = '+' then
            P := P + 1;
            C := Str (P);
         end if;

         --  Exponent.
         loop
            case C is
               when '0' .. '9' =>
                  Exp := Exp * 10 + Character'Pos (C) - Character'Pos ('0');
               when '_' =>
                  null;
               when others =>
                  raise Constraint_Error;
            end case;
            P := P + 1;
            exit when P > Str'Last;
            C := Str (P);
         end loop;

         if Exp_Neg then
            Exp := -Exp;
         end if;
      else
         Exp := 0;
      end if;

      if Point_Position /= -1 then
         Exp := Exp - (Nbr_Digits - Point_Position);
      end if;

      --  The internal representation of the number is:
      --  F * 10**EXP
      return To_Float_64 (Neg, F, 10, Exp);
   end From_String;

   procedure Format_Image
     (Str : out String; Last : out Natural; N : IEEE_Float_64)
   is
      P : Natural;
      S : String (1 .. 20);
      Len : Natural;
      Is_Num : Boolean;
      Is_Neg : Boolean;
      Exp : Integer;
   begin
      To_String (S, Len, Is_Num, Is_Neg, Exp, N);

      --  The sign.
      P := Str'First;
      if Is_Neg then
         Str (P) := '-';
         P := P + 1;
      end if;

      --  Non numbers
      if not Is_Num then
         Str (P .. P + Len - 1) := S (1 .. Len);
         Last := P + Len - 1;
         return;
      end if;

      --  Mantissa N.NNNNN
      Str (P) := S (1);
      Str (P + 1) := '.';
      Exp := Exp - 1;
      if Len = 1 then
         Str (P + 2) := '0';
         P := P + 3;
      else
         Str (P + 2 .. P + 2 + Len - 2) := S (2 .. Len);
         P := P + 2 + Len - 2 + 1;
      end if;

      --  Exponent
      if Exp /= 0 then
         --  LRM93 14.3
         --  if the exponent is present, the `e' is written as a lower case
         --  character.
         Str (P) := 'e';
         P := P + 1;

         if Exp < 0 then
            Str (P) := '-';
            P := P + 1;
            Exp := -Exp;
         end if;
         declare
            B : Boolean;
            D : Natural;
         begin
            B := False;
            for I in 0 .. 4 loop
               D := (Exp / 10000) mod 10;
               if D /= 0 or B or I = 4 then
                  Str (P) := Character'Val (48 + D);
                  P := P + 1;
                  B := True;
               end if;
               Exp := (Exp - D * 10000) * 10;
            end loop;
         end;
      end if;

      Last := P - 1;
   end Format_Image;

   procedure Format_Precision (Str : in out String;
                               Len : in out Natural;
                               Exp : in out Integer;
                               Prec : Positive)
   is
      pragma Assert (Str'First = 1);

      --  LEN is the number of digits, so there are LEN - EXP digits after the
      --  point.
      Ndigits : constant Integer := Len - Exp;
      Nlen : Integer;
      Inc : Boolean;
   begin
      if Ndigits <= Prec then
         --  Already precise enough.
         return;
      end if;

      Nlen := Prec + Exp;
      if Nlen < 0 then
         --  Number is too small
         Str (1) := '0';
         Len := 1;
         Exp := 0;
         return;
      end if;

      if Nlen < Len then
         --  Round.
         if Str (Nlen + 1) < '5' then
            Inc := False;
         elsif Str (Nlen + 1) > '5' then
            Inc := True;
         else
            Inc := False;
            for I in Nlen + 2 .. Len loop
               if Str (I) /= '0' then
                  Inc := True;
                  exit;
               end if;
            end loop;
         end if;
         if Inc then
            --  Increment the last digit and handle carray propagation if any.
            Inc := True;
            for I in reverse 1 .. Nlen loop
               if Str (I) < '9' then
                  Str (I) := Character'Val (Character'Pos (Str (I)) + 1);
                  Inc := False;
                  exit;
               else
                  Str (I) := '0';
               end if;
            end loop;
            if Inc then
               --  The digits were 9999...  so becomes 10000...
               --  Change the exponent so recompute the length.
               Exp := Exp + 1;
               Nlen := Prec + Exp;

               Str (1) := '1';
               Str (2 .. Nlen) := (others => '0');
            end if;
         end if;
         Len := Nlen;
      end if;
   end Format_Precision;

   procedure Format_Digits (Str : out String;
                            Last : out Natural;
                            N : IEEE_Float_64;
                            Ndigits : Natural)
   is
      procedure Append (C : Character) is
      begin
         Last := Last + 1;
         if Last <= Str'Last then
            Str (Last) := C;
         end if;
      end Append;

      S : String (1 .. 20);
      Len : Natural;
      Exp : Integer;
      Is_Num, Is_Neg : Boolean;
   begin
      --  LRM08 5.2.6 Predefined operations on scalar types
      --  If DIGITS is 0, then the string representation is the same as that
      --  produced by the TO_STRING operation without the DIGITS or FORMAT
      --  parameter.
      if Ndigits = 0 then
         Format_Image (Str, Last, N);
         return;
      end if;

      --  Radix conversion.
      Grt.Fcvt.To_String (S, Len, Is_Num, Is_Neg, Exp, N);

      --  Sign.
      Last := Str'First - 1;
      if Is_Neg then
         Append ('-');
      end if;

      --  Non finite numbers.  That shouldn't appear in VHDL, but let's handle
      --  them.
      if not Is_Num then
         for I in 1 .. Len loop
            Append (S (I));
         end loop;
         return;
      end if;

      --  Finite numbers.  Set precision.
      Grt.Fcvt.Format_Precision (S, Len, Exp, Ndigits);

      if Exp <= 0 then
         --  Integer part is 0.
         Append ('0');
         Append ('.');
         if Len - Exp <= Ndigits then
            for I in 1 .. -Exp loop
               Append ('0');
            end loop;
            for I in 1 .. Len loop
               Append (S (I));
            end loop;
            for I in Len - Exp + 1 .. Ndigits loop
               Append ('0');
            end loop;
         else
            for I in 1 .. Ndigits loop
               Append ('0');
            end loop;
         end if;
      elsif Exp >= Len then
         --  No fractional part.
         for I in 1 .. Len loop
            Append (S (I));
         end loop;
         for I in Len + 1 .. Exp loop
            Append ('0');
         end loop;
         Append ('.');
         for I in 1 .. Ndigits loop
            Append ('0');
         end loop;
      else
         for I in 1 .. Exp loop
            Append (S (I));
         end loop;
         Append ('.');
         for I in Exp + 1 .. Len loop
            Append (S (I));
         end loop;
         --  Len - (Exp + 1) + 1 digits after the point have been added.
         --  Complete with '0'.
         for I in Len - (Exp + 1) + 1 + 1 .. Ndigits loop
            Append ('0');
         end loop;
      end if;
   end Format_Digits;

end Grt.Fcvt;
