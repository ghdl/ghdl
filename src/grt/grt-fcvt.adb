--  GHDL Run Time (GRT) - Floating point conversions.
--  Copyright (C) 2017 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Ada.Unchecked_Conversion;

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

   type Unsigned_32_Array is array (Natural range <>) of Unsigned_32;

   type Bignum is record
      --  Number of digits.  Must be 0 for number 0.
      N : Natural;
      --  Digits.  The leading digit V(N + 1) must not be 0.
      V : Unsigned_32_Array (1 .. 37);
   end record;

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

      --  If true, the number is >= 1.0
      --  Used by the fast estimator.
      Ge_One : Boolean;

      --  Internal variables
      --  v = r / s
      Log2_S0 : Natural;
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
   procedure Bignum_Int (Res : out Bignum; N : Natural)
   is
   begin
      if N = 0 then
         Res.N := 0;
      else
         Res.N := 1;
         Res.V (1) := Unsigned_32 (N);
      end if;
   end Bignum_Int;

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

   function Bignum_Mul_Int (L : Bignum; R : Natural) return Bignum
   is
      Res : Bignum;

      Tmp : Unsigned_64;
   begin
      if R = 0 then
         Res.N := 0;
         return Res;
      end if;

      Tmp := 0;

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
   procedure Bignum_Mul_Int (L : in out Bignum; R : Positive)
   is
      Tmp : Unsigned_64;
   begin
      Tmp := 0;
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
   function Bignum_Pow (L : Bignum; N : Natural) return Bignum
   is
      Res : Bignum;
      N1 : Natural;
      T : Bignum;
   begin
      Bignum_Int (Res, 1);

      T := L;
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
   procedure Dragon4_Prepare (Ctxt : in out Fcvt_Context) is
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
            Ctxt.Log2_S0 := 1;
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
            Ctxt.Log2_S0 := 2;
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
            Ctxt.Log2_S0 := -Ctxt.E + 1;
            Bignum_Int (Ctxt.Mp, 1);
            Ctxt.Equal_M := True;
         else
            --  Case e > min exp  and  f = b**(p-1)
            --  r = f * b * 2
            --  s = b**(-e+1) * 2  = b**(-e+1+1)
            --  m+ = b
            --  m- = 1
            Ctxt.R := Bignum_Mul_Int (Ctxt.F, 2 * 2);
            Ctxt.Log2_S0 := -Ctxt.E + 1 + 1;
            Bignum_Int (Ctxt.Mp, 2);
            Bignum_Int (Ctxt.Mm, 1);
            Ctxt.Equal_M := False;
         end if;
         Ctxt.S := Bignum_Pow2 (Ctxt.Log2_S0);
      end if;
   end Dragon4_Prepare;

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
      T1 : Bignum;
   begin
      if False and Ctxt.B = 10 then
         --  TODO.
         declare
            E : Integer;
         begin
            if Ctxt.F.N = 2 and then Ctxt.F.V (2) >= 16#10_00_00# then
               --  Normal binary64 number
               E := 52 + Ctxt.E;
            else
               --  Denormal or non binary64.
               E := Ctxt.E;
            end if;

            --  Estimate k.
            Ctxt.K := Integer (Float (E) * 0.30103);

            --  Need to compute B**k
            Bignum_Int (T1, Ctxt.B);

            if Ctxt.K >= 0 then
               T1 := Bignum_Pow (T1, Ctxt.K);
               Ctxt.S := Bignum_Mul (Ctxt.S, T1);
            else
               T1 := Bignum_Pow (T1, -Ctxt.K);
               Ctxt.R := Bignum_Mul (Ctxt.R, T1);
               Ctxt.Mp := Bignum_Mul (Ctxt.Mp, T1);
               if not Ctxt.Equal_M then
                  Ctxt.Mm := Bignum_Mul (Ctxt.Mm, T1);
               end if;
            end if;
         end;
      else
         Ctxt.K := 0;
      end if;

      T1 := Bignum_Add (Ctxt.R, Ctxt.Mp);

      --  Adjust s0 so that r + m+ <= s0 * B**k
      while Bignum_Compare (T1, Ctxt.S) >= Eq loop
         Ctxt.K := Ctxt.K + 1;
         Bignum_Mul_Int (Ctxt.S, Ctxt.B);
      end loop;

      if Ctxt.K > 0 then
         return;
      end if;

      loop
         Bignum_Mul_Int (T1, Ctxt.B);
         exit when not (Bignum_Compare (T1, Ctxt.S) <= Eq);
         Ctxt.K := Ctxt.K - 1;
         Bignum_Mul_Int (Ctxt.R, Ctxt.B);
         Bignum_Mul_Int (Ctxt.Mp, Ctxt.B);
         if not Ctxt.Equal_M then
            Bignum_Mul_Int (Ctxt.Mm, Ctxt.B);
         end if;
         T1 := Bignum_Add (Ctxt.R, Ctxt.Mp);
      end loop;

      --  Note: high = (v + v+) / 2
      --             = (2*v + b**e) / 2
      --             = ((2*f + 1) * b**e) / 2
      --  Proof:
      --
      --  Case e >= 0
      --    Case f /= b**(p-1):
      --      high = ((2*f + 1) * b**e) / 2
      --    Case f = b**(p-1)
      --      high = ((2*f + 1) * b**(e+1)) / (b * 2)
      --           = ((2*f + 1) * b**e) / 2
      --  Case e < 0
      --    Case e = min exp  or f /= b**(p-1)
      --      high = (2*f + 1) / (2*b**(-e))
      --    Case e > min exp  and f = b**(p-1)
      --      high = (2*f*b + b) / (2*b**(-e+1))
      --           = (2*f + 1) / (2*b**(-e))
      --  In all cases:
      --  high = ((2*f + 1) * b**e) / 2

      --  if Ctxt.B = 10 then
      --  log10(2) ~= 0.30103
      --  k = ceil(log10((2*f+1) / 2) + log10(b**e))
      --    = ceil(log10((2*f+1)/2) + e*log10(2))
      --  If the input number was normalized, then:
      --    2**53 <= (2*f + 1)/2 <= 2**54
      --    15.95 <= log10((2*f+1)/2) <= 16.255

      --  so k = ceil (log10(r+m+) - log2(s)*log10(2))
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
         Bignum_Mul_Int (Ctxt.R, Ctxt.B);

         Bignum_Divstep (Ctxt.R, S8, Step);
         D := Boolean'Pos (Step) * 8;
         Bignum_Divstep (Ctxt.R, S4, Step);
         D := D + Boolean'Pos (Step) * 4;
         Bignum_Divstep (Ctxt.R, S2, Step);
         D := D + Boolean'Pos (Step) * 2;
         Bignum_Divstep (Ctxt.R, S1, Step);
         D := D + Boolean'Pos (Step);

         Bignum_Mul_Int (Ctxt.Mp, Ctxt.B);
         if not Ctxt.Equal_M then
            Bignum_Mul_Int (Ctxt.Mm, Ctxt.B);
         end if;

         --  Stop conditions.
         --  Note: there is a typo for condition (2) in the original paper.
         if not Ctxt.Equal_M then
            Cond1 := Bignum_Compare (Ctxt.R, Ctxt.Mm) = Lt;
         else
            Cond1 := Bignum_Compare (Ctxt.R, Ctxt.Mp) = Lt;
         end if;
         Cond2 := Bignum_Compare (Bignum_Add (Ctxt.R, Ctxt.Mp), Ctxt.S) = Gt;
         exit when Cond1 or Cond2;

         Append_Digit (Str, Len, D);
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
                             Len : out Natural;
                             Is_Inf : Boolean;
                             S : Boolean) is
   begin
      Len := 0;

      if Is_Inf then
         --  Infinite
         if S then
            Append (Str, Len, '-');
         else
            Append (Str, Len, '+');
         end if;
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
                        V : IEEE_Float_64;
                        Radix : Positive := 10)
   is
      pragma Assert (Str'First = 1);

      --  Decompose V (assuming same endianness).
      V_Bits : constant Unsigned_64 := F64_To_U64 (V);
      S : constant Boolean := Shift_Right (V_Bits, 63) = 1;
      M : constant Unsigned_64 := V_Bits and 16#f_ff_ff_ff_ff_ff_ff#;
      E : constant Integer := Integer (Shift_Right (V_Bits, 52) and 16#7_ff#);

      Ctxt : Fcvt_Context;
      First : Natural;
   begin
      --  Handle NaN & Inf
      if E = 2047 then
         Output_Nan_Inf (Str, Len, M = 0, S);
         return;
      end if;

      --  Normal or denormal float.
      Len := 0;

      Ctxt.B := Radix;
      Ctxt.F.N := 2;
      Ctxt.F.V (1) := Unsigned_32 (M and 16#ffff_ffff#);
      Ctxt.F.V (2) := Unsigned_32 (Shift_Right (M, 32) and 16#ffff_ffff#);
      if E = 0 then
         --  Denormal.
         Ctxt.E := -1022 - 52;

         --  Bignum digits may be 0.
         Bignum_Normalize (Ctxt.F);

         --  Display sign.
         if S then
            Append (Str, Len, '-');
         end if;

         Ctxt.Is_Emin := True;
         Ctxt.Is_Pow2 := False; --  Not needed.
         Ctxt.Ge_One := False;
      else
         --  Normal.
         Ctxt.E := E - 1023 - 52;

         --  Implicit leading 1.
         Ctxt.F.V (2) := Ctxt.F.V (2) or 16#10_00_00#;

         --  Display sign.
         if S then
            Append (Str, Len, '-');
         end if;

         Ctxt.Is_Emin := False;
         Ctxt.Is_Pow2 := M = 0;
         Ctxt.Ge_One := E = 1023;
      end if;

      pragma Assert (Bignum_Is_Valid (Ctxt.F));

      First := Len;

      if Ctxt.F.N = 0 then
         --  Zero is special
         Append (Str, Len, '0');
         Ctxt.K := 1;
      else
         Dragon4 (Str, Len, Ctxt);
      end if;

      --  Formatting.
      --  Insert the dot.
      declare
         C, Prev_C : Character;
      begin
         if Len > First + 1 then
            C := '.';
            for I in First + 2 .. Len loop
               Prev_C := Str (I);
               Str (I) := C;
               C := Prev_C;
            end loop;
            Len := Len + 1;
            Str (Len) := C;
         end if;
         Ctxt.K := Ctxt.K - 1;
      end;

      Append (Str, Len, 'e');
      declare
         K : Integer;
         T : Integer;
         Den : Natural;
      begin
         K := Ctxt.K;
         if K < 0 then
            Append (Str, Len, '-');
            K := -K;
         end if;
         if K >= 100 then
            Den := 100;
         elsif K >= 10 then
            Den := 10;
         else
            Den := 1;
         end if;
         loop
            T := K / Den;
            Append_Digit (Str, Len, T);
            K := K - T * Den;
            exit when Den = 1;
            Den := Den / 10;
         end loop;
      end;
   end To_String;
end Grt.Fcvt;
