--  Lexical analysis for numbers.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with Interfaces; use Interfaces;
with Grt.Fcvt; use Grt.Fcvt;

separate (Vhdl.Scanner)

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
   --  Numbers of digits.
   Scale : Integer;
   Res : Bignum;

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
         Bignum_Mul_Int (Res, 10, Character'Pos (C) - Character'Pos ('0'));
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
   D : Natural;
   Ok : Boolean;
   Has_Dot : Boolean;
   Exp : Integer;
   Exp_Neg : Boolean;
   Base : Positive;
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
            Bignum_Int (Res, D);
            exit;
         end if;

         --  Finished.
         --  a universal integer.
         Current_Token := Tok_Integer;
         --  No possible overflow.
         Current_Context.Lit_Int64 := Int64 (D);
         return;
      elsif D >= (Natural'Last / 10) - 1 then
         --  Number may be greather than the natural limit.
         Scale := 0;
         Bignum_Int (Res, D);
         Scan_Integer;
         exit;
      end if;
   end loop;

   Has_Dot := False;
   Base := 10;
   Scale := 0;

   C := Source (Pos);
   if C = '.' then
      --  Decimal integer.
      Has_Dot := True;
      Pos := Pos + 1;
      C := Source (Pos);
      if C not in '0' .. '9' then
         Error_Msg_Scan ("a dot must be followed by a digit");
         Current_Token := Tok_Real;
         Current_Context.Lit_Fp64 := Fp64 (To_Float_64 (False, Res, Base, 0));
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
         Res_Int : Interfaces.Unsigned_64;
      begin
         Bignum_To_Int (Res, Res_Int, Ok);
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
            Base := Natural (Res_Int);
         end if;

         Pos := Pos + 1;
         Bignum_Int (Res, 0);
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
            Bignum_Mul_Int (Res, Base, D);
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

   --  Exponent.
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
      -- a universal real.
      Current_Token := Tok_Real;

      Current_Context.Lit_Fp64 :=
        Fp64 (To_Float_64 (False, Res, Base, Exp - Scale));
   else
      -- a universal integer.
      Current_Token := Tok_Integer;

      -- Set to a valid literal, in case of constraint error.
      if Exp /= 0 then
         Res := Bignum_Mul (Res, Bignum_Pow (Base, Exp));
      end if;

      declare
         U : Unsigned_64;
      begin
         Bignum_To_Int (Res, U, Ok);
         if U > Unsigned_64 (Int64'Last) then
            Ok := False;
         else
            Current_Context.Lit_Int64 := Int64 (U);
         end if;
      end;
      if not Ok then
         Error_Msg_Scan ("literal beyond integer bounds");
      end if;
   end if;
exception
   when Constraint_Error =>
      Error_Msg_Scan ("literal overflow");

      Current_Token := Tok_Integer;
      Current_Context.Lit_Int64 := 0;
end Scan_Literal;
