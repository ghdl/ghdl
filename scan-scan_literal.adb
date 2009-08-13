--  Lexical analysis for numbers.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Ada.Unchecked_Conversion;

separate (Scan)

-- scan a decimal literal or a based literal.
--
-- LRM93 13.4.1
-- DECIMAL_LITERAL ::= INTEGER [ . INTEGER ] [ EXPONENT ]
-- EXPONENT ::= E [ + ] INTEGER | E - INTEGER
--
-- LRM93 13.4.2
-- BASED_LITERAL ::= BASE # BASED_INTEGER [ . BASED_INTEGER ] # EXPONENT
-- BASE ::= INTEGER
procedure Scan_Literal is
   --  The base of an E_NUM is 2**16.
   --  Type Uint16 is the type of a digit.
   type Uint16 is mod 2 ** 16;

   type Uint32 is mod 2 ** 32;

   --  Type of the exponent.
   type Sint16 is range -2 ** 15 .. 2 ** 15 - 1;

   --  Number of digits in a E_NUM.
   --  We want at least 64bits of precision, so at least 5 digits of 16 bits
   --  are required.
   Nbr_Digits : constant Sint16 := 5;
   subtype Digit_Range is Sint16 range 0 .. Nbr_Digits - 1;

   type Uint16_Array is array (Sint16 range <>) of Uint16;

   --  The value of an E_NUM is (S(N-1)|S(N-2) .. |S(0))* 2**(16*E)
   --  where '|' is concatenation.
   type E_Num is record
      S : Uint16_Array (Digit_Range);
      E : Sint16;
   end record;

   E_Zero : constant E_Num := (S => (others => 0), E => 0);
   E_One  : constant E_Num := (S => (0 => 1, others => 0), E => 0);

   --  Compute RES = E * B + V.
   --  RES and E can be the same object.
   procedure Bmul (Res : out E_Num; E : E_Num; V : Uint16; B : Uint16);

   --  Convert to integer.
   procedure Fix (Res : out Iir_Int64; Ok : out Boolean; E : E_Num);

   --  RES := A * B
   --  RES can be A or B.
   procedure Mul (Res : out E_Num; A, B : E_Num);

   --  RES := A / B.
   --  RES can be A.
   --  May raise constraint error.
   procedure Div (Res : out E_Num; A, B: E_Num);

   --  Convert V to an E_Num.
   function To_E_Num (V : Uint16) return E_Num;

   --  Convert E to RES.
   procedure To_Float (Res : out Iir_Fp64; Ok : out Boolean; E : E_Num);

   procedure Bmul (Res : out E_Num; E : E_Num; V : Uint16; B : Uint16)
   is
      --  The carry.
      C : Uint32;
   begin
      --  Only consider V if E is not scaled (otherwise V is not significant).
      if E.E = 0 then
         C := Uint32 (V);
      else
         C := 0;
      end if;

      --  Multiply and propagate the carry.
      for I in Digit_Range loop
         C := Uint32 (E.S (I)) * Uint32 (B) + C;
         Res.S (I) := Uint16 (C mod Uint16'Modulus);
         C := C / Uint16'Modulus;
      end loop;

      --  There is a carry, shift.
      if C /= 0 then
         --  ERR: Possible overflow.
         Res.E := E.E + 1;
         for I in 0 .. Nbr_Digits - 2 loop
            Res.S (I) := Res.S (I + 1);
         end loop;
         Res.S (Nbr_Digits - 1) := Uint16 (C);
      else
         Res.E := E.E;
      end if;
   end Bmul;

   type Uint64 is mod 2 ** 64;
   function Shift_Left (Value : Uint64; Amount: Natural) return Uint64;
   function Shift_Left (Value : Uint16; Amount: Natural) return Uint16;
   pragma Import (Intrinsic, Shift_Left);

   function Shift_Right (Value : Uint16; Amount: Natural) return Uint16;
   pragma Import (Intrinsic, Shift_Right);

   function Unchecked_Conversion is new Ada.Unchecked_Conversion
     (Source => Uint64, Target => Iir_Int64);

   procedure Fix (Res : out Iir_Int64; Ok : out Boolean; E : E_Num)
   is
      R : Uint64;
      M : Sint16;
   begin
      --  Find the most significant digit.
      M := -1;
      for I in reverse Digit_Range loop
         if E.S (I) /= 0 then
            M := I;
            exit;
         end if;
      end loop;

      --  Handle the easy 0 case.
      --  The case M = -1 is handled below, in the normal flow.
      if M + E.E < 0 then
         Res := 0;
         Ok := True;
         return;
      end if;

      --  Handle overflow.
      --  4 is the number of uint16 in a uint64.
      if M + E.E >= 4 then
         Ok := False;
         return;
      end if;

      --  Convert
      R := 0;
      for I in 0 .. M loop
         R := R or Shift_Left (Uint64 (E.S (I)), 16 * Natural (E.E + I));
      end loop;
      --  Check the sign bit is 0.
      if (R and Shift_Left (1, 63)) /= 0 then
         Ok := False;
      else
         Ok := True;
         Res := Unchecked_Conversion (R);
      end if;
   end Fix;

   --  Return the position of the most non-null digit, -1 if V is 0.
   function First_Digit (V : E_Num) return Sint16 is
   begin
      for I in reverse Digit_Range loop
         if V.S (I) /= 0 then
            return I;
         end if;
      end loop;
      return -1;
   end First_Digit;

   procedure Mul (Res : out E_Num; A, B : E_Num)
   is
      T : Uint16_Array (0 .. 2 * Nbr_Digits - 1);
      V : Uint32;
      Max : Sint16;
   begin
      V := 0;
      for I in 0 .. Nbr_Digits - 1 loop
         for J in 0 .. I loop
            V := V + Uint32 (A.S (J)) * Uint32 (B.S (I - J));
         end loop;
         T (I) := Uint16 (V mod Uint16'Modulus);
         V := V / Uint16'Modulus;
      end loop;
      for I in Nbr_Digits .. 2 * Nbr_Digits - 2 loop
         for J in I - Nbr_Digits + 1 .. Nbr_Digits - 1 loop
            V := V + Uint32 (A.S (J)) * Uint32 (B.S (I - J));
         end loop;
         T (I) := Uint16 (V mod Uint16'Modulus);
         V := V / Uint16'Modulus;
      end loop;
      T (T'Last) := Uint16 (V);
      --  Search the leading non-nul.
      Max := -1;
      for I in reverse T'Range loop
         if T (I) /= 0 then
            Max := I;
            exit;
         end if;
      end loop;
      if Max > Nbr_Digits - 1 then
         --  Loss of precision.
         --  Round.
         if T (Max - Nbr_Digits) >= Uint16 (Uint16'Modulus / 2) then
            V := 1;
            for I in Max - (Nbr_Digits - 1) .. Max loop
               V := V + Uint32 (T (I));
               T (I) := Uint16 (V mod Uint16'Modulus);
               V := V / Uint16'Modulus;
               exit when V = 0;
            end loop;
            if V /= 0 then
               Max := Max + 1;
               T (Max) := Uint16 (V);
            end if;
         end if;
         Res.S := T (Max - (Nbr_Digits - 1) .. Max);
         --  This may overflow.
         Res.E := A.E + B.E + Max - (Nbr_Digits - 1);
      else
         Res.S (0 .. Max) := T (0 .. Max);
         Res.S (Max + 1 .. Nbr_Digits - 1) := (others => 0);
         --  This may overflow.
         Res.E := A.E + B.E;
      end if;
   end Mul;

   procedure Div (Res : out E_Num; A, B: E_Num)
   is
      Dividend : Uint16_Array (0 .. Nbr_Digits);
      A_F : constant Sint16 := First_Digit (A);
      B_F : constant Sint16 := First_Digit (B);

      --  Digit corresponding to the first digit of B.
      Doff : constant Sint16 := Dividend'Last - B_F;
      Q : Uint16;
      C, N_C : Uint16;
   begin
      --  Check for division by 0.
      if B_F < 0 then
         raise Constraint_Error;
      end if;

      --  Copy and shift dividend.
      --  Bit 15 of the most significant digit of A becomes bit 0 of the
      --  most significant digit of DIVIDEND.  Therefore we are sure
      --  DIVIDEND < B (after realignment).
      C := 0;
      for K in 0 .. A_F loop
         N_C := Shift_Right (A.S (K), 15);
         Dividend (Dividend'Last - A_F - 1 + K)
           := Shift_Left (A.S (K), 1) or C;
         C := N_C;
      end loop;
      Dividend (Nbr_Digits) := C;
      Dividend (0 .. Dividend'last - 2 - A_F) := (others => 0);

      --  Algorithm is the same as division by hand.
      C := 0;
      for I in reverse Digit_Range loop
         Q := 0;
         for J in 0 .. 15 loop
            declare
               Borrow : Uint32;
               Tmp : Uint16_Array (0 .. B_F);
               V : Uint32;
               V16 : Uint16;
            begin
               --  Compute TMP := dividend - B;
               Borrow := 0;
               for K in 0 .. B_F loop
                  V := Uint32 (B.S (K)) + Borrow;
                  V16 := Uint16 (V mod Uint16'Modulus);
                  if V16 > Dividend (Doff + K) then
                     Borrow := 1;
                  else
                     Borrow := 0;
                  end if;
                  Tmp (K) := Dividend (Doff + K) - V16;
               end loop;

               --  If the last shift creates a carry, we are sure Dividend > B
               if C /= 0 then
                  Borrow := 0;
               end if;

               Q := Q * 2;
               --  Begin of : Dividend = Dividend * 2
               C := 0;
               for K in 0 .. Doff - 1 loop
                  N_C := Shift_Right (Dividend (K), 15);
                  Dividend (K) := Shift_Left (Dividend (K), 1) or C;
                  C := N_C;
               end loop;

               if Borrow = 0 then
                  --  Dividend > B
                  Q := Q + 1;
                  --  Dividend = Tmp * 2
                  --           = (Dividend - B) * 2
                  for K in Doff .. Nbr_Digits loop
                     N_C := Shift_Right (Tmp (K - Doff), 15);
                     Dividend (K) := Shift_Left (Tmp (K - Doff), 1) or C;
                     C := N_C;
                  end loop;
               else
                  --  Dividend = Dividend * 2
                  for K in Doff .. Nbr_Digits loop
                     N_C := Shift_Right (Dividend (K), 15);
                     Dividend (K) := Shift_Left (Dividend (K), 1) or C;
                     C := N_C;
                  end loop;
               end if;
            end;
         end loop;
         Res.S (I) := Q;
      end loop;
      Res.E := A.E - B.E + (A_F - B_F) - (Nbr_Digits - 1);
   end Div;

   procedure To_Float (Res : out Iir_Fp64; Ok : out Boolean; E : E_Num)
   is
      V : Iir_Fp64;
      P : Iir_Fp64;
   begin
      Res := 0.0;
      P := Iir_Fp64'Scaling (1.0, 16 * E.E);
      for I in Digit_Range loop
         V := Iir_Fp64 (E.S (I)) * P;
         P := Iir_Fp64'Scaling (P, 16);
         Res := Res + V;
      end loop;
      Ok := True;
   end To_Float;

   function To_E_Num (V : Uint16) return E_Num
   is
      Res : E_Num;
   begin
      Res.E := 0;
      Res.S := (0 => V, others => 0);
      return Res;
   end To_E_Num;

   --  Numbers of digits.
   Scale : Integer;
   Res : E_Num;

   --  LRM 13.4.1
   --  INTEGER ::= DIGIT { [ UNDERLINE ] DIGIT }
   --
   --  Update SCALE, RES.
   --  The first character must be a digit.
   procedure Scan_Integer
   is
      C : Character;
   begin
      C := Source (Pos);
      loop
         --  C is a digit.
         Bmul (Res, Res, Character'Pos (C) - Character'Pos ('0'), 10);
         Scale := Scale + 1;

         Pos := Pos + 1;
         C := Source (Pos);
         if C = '_' then
            loop
               Pos := Pos + 1;
               C := Source (Pos);
               exit when C /= '_';
               Error_Msg_Scan ("double underscore in number");
            end loop;
            if C not in '0' .. '9' then
               Error_Msg_Scan ("underscore must be followed by a digit");
            end if;
         end if;
         exit when C not in '0' .. '9';
      end loop;
   end Scan_Integer;

   C : Character;
   D : Uint16;
   Ok : Boolean;
   Has_Dot : Boolean;
   Exp : Integer;
   Exp_Neg : Boolean;
   Base : Uint16;
begin
   --  Start with a simple and fast conversion.
   C := Source (Pos);
   D := 0;
   loop
      D := D * 10 + Character'Pos (C) - Character'Pos ('0');

      Pos := Pos + 1;
      C := Source (Pos);
      if C = '_' then
         loop
            Pos := Pos + 1;
            C := Source (Pos);
            exit when C /= '_';
            Error_Msg_Scan ("double underscore in number");
         end loop;
         if C not in '0' .. '9' then
            Error_Msg_Scan ("underscore must be followed by a digit");
         end if;
      end if;
      if C not in '0' .. '9' then
         if C = '.' or else C = '#' or else (C = 'e' or C = 'E' or C = ':')
         then
            --  Continue scanning.
            Res := To_E_Num (D);
            exit;
         end if;

         --  Finished.
         --  a universal integer.
         Current_Token := Tok_Integer;
         --  No possible overflow.
         Current_Context.Int64 := Iir_Int64 (D);
         return;
      elsif D >= 6552 then
         --  Number may be greather than the uint16 limit.
         Scale := 0;
         Res := To_E_Num (D);
         Scan_Integer;
         exit;
      end if;
   end loop;

   Has_Dot := False;
   Base := 10;

   C := Source (Pos);
   if C = '.' then
      --  Decimal integer.
      Has_Dot := True;
      Scale := 0;
      Pos := Pos + 1;
      C := Source (Pos);
      if C not in '0' .. '9' then
         Error_Msg_Scan ("a dot must be followed by a digit");
         return;
      end if;
      Scan_Integer;
   elsif C = '#'
     or else (C = ':' and then (Source (Pos + 1) in '0' .. '9'
                                or else Source (Pos + 1) in 'a' .. 'f'
                                or else Source (Pos + 1) in 'A' .. 'F'))
   then
      --  LRM 13.10
      --  The number sign (#) of a based literal can be replaced by colon (:),
      --  provided that the replacement is done for both occurrences.
      -- GHDL: correctly handle 'variable v : integer range 0 to 7:= 3'.
      --   Is there any other places where a digit can be followed
      --   by a colon ? (See IR 1093).

      --  Based integer.
      declare
         Number_Sign : constant Character := C;
         Res_Int : Iir_Int64;
      begin
         Fix (Res_Int, Ok, Res);
         if not Ok or else Res_Int > 16 then
            --  LRM 13.4.2
            --  The base must be [...] at most sixteen.
            Error_Msg_Scan ("base must be at most 16");
            --  Fallback.
            Base := 16;
         elsif Res_Int < 2 then
            --  LRM 13.4.2
            --  The base must be at least two [...].
            Error_Msg_Scan ("base must be at least 2");
            --  Fallback.
            Base := 2;
         else
            Base := Uint16 (Res_Int);
         end if;

         Pos := Pos + 1;
         Res := E_Zero;
         C := Source (Pos);
         loop
            if C >= '0' and C <= '9' then
               D := Character'Pos (C) - Character'Pos ('0');
            elsif C >= 'A' and C <= 'F' then
               D := Character'Pos (C) - Character'Pos ('A') + 10;
            elsif C >= 'a' and C <= 'f' then
               D := Character'Pos (C) - Character'Pos ('a') + 10;
            else
               Error_Msg_Scan ("bad extended digit");
               exit;
            end if;

            if D >= Base then
               --  LRM 13.4.2
               --  The conventional meaning of base notation is
               --  assumed; in particular the value of each extended
               --  digit of a based literal must be less then the base.
               Error_Msg_Scan ("digit beyond base");
               D := 1;
            end if;
            Pos := Pos + 1;
            Bmul (Res, Res, D, Base);
            Scale := Scale + 1;

            C := Source (Pos);
            if C = '_' then
               loop
                  Pos := Pos + 1;
                  C := Source (Pos);
                  exit when C /= '_';
                  Error_Msg_Scan ("double underscore in based integer");
               end loop;
            elsif C = '.' then
               if Has_Dot then
                  Error_Msg_Scan ("double dot ignored");
               else
                  Has_Dot := True;
                  Scale := 0;
               end if;
               Pos := Pos + 1;
               C := Source (Pos);
            elsif C = Number_Sign then
               Pos := Pos + 1;
               exit;
            elsif C = '#' or C = ':' then
               Error_Msg_Scan ("bad number sign replacement character");
               exit;
            end if;
         end loop;
      end;
   end if;
   C := Source (Pos);
   Exp := 0;
   if C = 'E' or else C = 'e' then
      Pos := Pos + 1;
      C := Source (Pos);
      Exp_Neg := False;
      if C = '+' then
         Pos := Pos + 1;
         C := Source (Pos);
      elsif C = '-' then
         if Has_Dot then
            Exp_Neg := True;
         else
            --  LRM 13.4.1
            --  An exponent for an integer literal must not have a minus sign.
            --
            --  LRM 13.4.2
            --  An exponent for a based integer literal must not have a minus
            --  sign.
            Error_Msg_Scan
              ("negative exponent not allowed for integer literal");
         end if;
         Pos := Pos + 1;
         C := Source (Pos);
      end if;
      if C not in '0' .. '9' then
         Error_Msg_Scan ("digit expected after exponent");
      else
         loop
            --  C is a digit.
            Exp := Exp * 10 + (Character'Pos (C) - Character'Pos ('0'));

            Pos := Pos + 1;
            C := Source (Pos);
            if C = '_' then
               loop
                  Pos := Pos + 1;
                  C := Source (Pos);
                  exit when C /= '_';
                  Error_Msg_Scan ("double underscore not allowed in integer");
               end loop;
               if C not in '0' .. '9' then
                  Error_Msg_Scan ("digit expected after underscore");
                  exit;
               end if;
            elsif C not in '0' .. '9' then
               exit;
            end if;
         end loop;
      end if;
      if Exp_Neg then
         Exp := -Exp;
      end if;
   end if;

   if Has_Dot then
      Scale := Scale - Exp;
   else
      Scale := -Exp;
   end if;
   if Scale /= 0 then
      declare
         Scale_Neg : Boolean;
         Val_Exp : E_Num;
         Val_Pow : E_Num;
      begin
         if Scale > 0 then
            Scale_Neg := True;
         else
            Scale_Neg := False;
            Scale := -Scale;
         end if;

         Val_Pow := To_E_Num (Base);
         Val_Exp := E_One;
         while Scale /= 0 loop
            if Scale mod 2 = 1 then
               Mul (Val_Exp, Val_Exp, Val_Pow);
            end if;
            Scale := Scale / 2;
            Mul (Val_Pow, Val_Pow, Val_Pow);
         end loop;
         if Scale_Neg then
            Div (Res, Res, Val_Exp);
         else
            Mul (Res, Res, Val_Exp);
         end if;
      end;
   end if;

   if Has_Dot then
      -- a universal real.
      Current_Token := Tok_Real;
      -- Set to a valid literal, in case of constraint error.
      To_Float (Current_Context.Fp64, Ok, Res);
      if not Ok then
         Error_Msg_Scan ("literal beyond real bounds");
      end if;
   else
      -- a universal integer.
      Current_Token := Tok_Integer;
      -- Set to a valid literal, in case of constraint error.
      Fix (Current_Context.Int64, Ok, Res);
      if not Ok then
         Error_Msg_Scan ("literal beyond integer bounds");
      end if;
   end if;
exception
   when Constraint_Error =>
      Error_Msg_Scan ("literal overflow");
end Scan_Literal;
