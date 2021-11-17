--  GHDL Run Time (GRT) -  'image subprograms.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

with Interfaces;
with Grt.Fcvt;

package body Grt.To_Strings is
   generic
      type Ntype is range <>;
      --Max_Len : Natural;
   procedure Gen_To_String (Str : out String; First : out Natural; N : Ntype);

   procedure Gen_To_String (Str : out String; First : out Natural; N : Ntype)
   is
      subtype R_Type is String (1 .. Str'Length);
      S : R_Type renames Str;
      P : Natural := S'Last;
      V : Ntype;
   begin
      if N > 0 then
         V := -N;
      else
         V := N;
      end if;
      loop
         S (P) := Character'Val (48 - (V rem 10));
         V := V / 10;
         exit when V = 0;
         P := P - 1;
      end loop;
      if N < 0 then
         P := P - 1;
         S (P) := '-';
      end if;
      First := P;
   end Gen_To_String;

   procedure To_String_I32 is new Gen_To_String (Ntype => Ghdl_I32);

   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I32)
     renames To_String_I32;

   procedure To_String_I64 is new Gen_To_String (Ntype => Ghdl_I64);

   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I64)
     renames To_String_I64;

   procedure To_String (Str : out String; Last : out Natural; N : Ghdl_F64) is
   begin
      Grt.Fcvt.Format_Image (Str, Last, Interfaces.IEEE_Float_64 (N));
   end To_String;

   procedure To_String (Str : out String;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Nbr_Digits : Ghdl_I32) is
   begin
      Grt.Fcvt.Format_Digits
        (Str, Last, Interfaces.IEEE_Float_64 (N), Natural (Nbr_Digits));
   end To_String;

   procedure To_String (Str : out String_Real_Format;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Format : Ghdl_C_String)
   is
      procedure Snprintf_Fmtf (Str : in out String;
                               Len : Natural;
                               Format : Ghdl_C_String;
                               V : Ghdl_F64);
      pragma Import (C, Snprintf_Fmtf, "__ghdl_snprintf_fmtf");
   begin
      --  FIXME: check format ('%', f/g/e/a)
      Snprintf_Fmtf (Str, Str'Length, Format, N);
      Last := strlen (To_Ghdl_C_String (Str'Address));
   end To_String;

   procedure To_String (Str : out String_Time_Unit;
                        First : out Natural;
                        Value : Ghdl_I64;
                        Unit : Ghdl_I64)
   is
      V, U : Ghdl_I64;
      D : Natural;
      P : Natural := Str'Last;
      Has_Digits : Boolean;
   begin
      --  Always work on negative values.
      if Value > 0 then
         V := -Value;
      else
         V := Value;
      end if;

      Has_Digits := False;
      U := Unit;
      loop
         if U = 1 then
            if Has_Digits then
               Str (P) := '.';
               P := P - 1;
            else
               Has_Digits := True;
            end if;
         end if;

         D := Natural (-(V rem 10));
         if D /= 0 or else Has_Digits then
            Str (P) := Character'Val (48 + D);
            P := P - 1;
            Has_Digits := True;
         end if;
         U := U / 10;
         V := V / 10;
         exit when V = 0 and then U = 0;
      end loop;
      if not Has_Digits then
         Str (P) := '0';
      else
         P := P + 1;
      end if;
      if Value < 0 then
         P := P - 1;
         Str (P) := '-';
      end if;
      First := P;
   end To_String;

   NBSP : constant Character := Character'Val (160);
   HT : constant Character := Character'Val (9);

   --  Convert S (INIT_POS .. LEN) to a signed integer.
   function Value_I64 (S : Std_String_Basep;
                       Len : Ghdl_Index_Type;
                       Init_Pos : Ghdl_Index_Type) return Value_I64_Result
   is
      Pos : Ghdl_Index_Type := Init_Pos;
      C : Character;
      Sep : Character;
      Val, D, Base : Ghdl_I64;
      Exp : Integer;
      Is_Neg : Boolean;
   begin
      C := S (Pos);
      Val := 0;

      --  LRM02 14.1 Predefined attributes
      --  Restrictions: It is an error is the parameter is not a valid string
      --  representation of a literal ot type T.
      --
      --  Apparently there is no definition of 'string representation', the
      --  closest is:
      --
      --  LRM02 14.3 Package TEXTIO
      --  The representation of both INTEGER and REAL values [...]
      Is_Neg := False;
      if C = '+' or C = '-' then
         if Pos = Len then
            return (Status => Value_Err_No_Digit, Pos => Pos);
         end if;
         Pos := Pos + 1;
         Is_Neg := C = '-';
         C := S (Pos);
      end if;

      loop
         if C in '0' .. '9' then
            Val := Val * 10 - (Character'Pos (C) - Character'Pos ('0'));
            Pos := Pos + 1;
            exit when Pos >= Len;
            C := S (Pos);
         else
            return (Status => Value_Err_No_Digit, Pos => Pos);
         end if;
         case C is
            when '_' =>
               Pos := Pos + 1;
               if Pos >= Len then
                  return (Status => Value_Err_Underscore, Pos => Pos);
               end if;
               C := S (Pos);
            when '#'
              | ':'
              | 'E'
              | 'e' =>
               exit;
            when ' '
              | NBSP
              | HT =>
               Pos := Pos + 1;
               exit;
            when others =>
               null;
         end case;
      end loop;

      if Pos >= Len then
         if not Is_Neg then
            Val := -Val;
         end if;
         return (Status => Value_Ok, Val => Val);
      end if;

      if C = '#' or C = ':' then
         Base := -Val;
         Val := 0;
         Sep := C;
         Pos := Pos + 1;
         if Base < 2 or Base > 16 then
            return (Status => Value_Err_Bad_Base, Pos => Pos);
         end if;
         if Pos >= Len then
            return (Status => Value_Err_No_Digit, Pos => Pos);
         end if;
         C := S (Pos);
         loop
            case C is
               when '0' .. '9' =>
                  D := Character'Pos (C) - Character'Pos ('0');
               when 'a' .. 'f' =>
                  D := Character'Pos (C) - Character'Pos ('a') + 10;
               when 'A' .. 'F' =>
                  D := Character'Pos (C) - Character'Pos ('A') + 10;
               when others =>
                  return (Status => Value_Err_Bad_Digit, Pos => Pos);
            end case;
            if D >= Base then
               return (Status => Value_Err_Bad_Digit, Pos => Pos);
            end if;
            Val := Val * Base - D;
            Pos := Pos + 1;
            if Pos >= Len then
               return (Status => Value_Err_Bad_End_Sign, Pos => Pos);
            end if;
            C := S (Pos);
            if C = '#' or C = ':' then
               if C /= Sep then
                  return (Status => Value_Err_Bad_End_Sign, Pos => Pos);
               end if;
               Pos := Pos + 1;
               exit;
            elsif C = '_' then
               Pos := Pos + 1;
               if Pos >= Len then
                  return (Status => Value_Err_Underscore, Pos => Pos);
               end if;
               C := S (Pos);
            end if;
         end loop;
      else
         Base := 10;
      end if;

      -- Handle exponent.
      if C = 'e' or C = 'E' then
         Pos := Pos + 1;
         if Pos >= Len then
            return (Status => Value_Err_No_Digit, Pos => Pos);
         end if;
         C := S (Pos);
         if C = '+' then
            Pos := Pos + 1;
            if Pos >= Len then
               return (Status => Value_Err_No_Digit, Pos => Pos);
            end if;
            C := S (Pos);
         elsif C = '-' then
            return (Status => Value_Err_Bad_Exponent, Pos => Pos);
         end if;
         Exp := 0;
         loop
            if C in '0' .. '9' then
               Exp := Exp * 10 + Character'Pos (C) - Character'Pos ('0');
               Pos := Pos + 1;
               exit when Pos >= Len;
               C := S (Pos);
            else
               return (Status => Value_Err_Bad_Digit, Pos => Pos);
            end if;
            case C is
               when '_' =>
                  Pos := Pos + 1;
                  if Pos >= Len then
                     return (Status => Value_Err_Underscore, Pos => Pos);
                  end if;
                  C := S (Pos);
               when ' '
                 | NBSP
                 | HT =>
                  Pos := Pos + 1;
                  exit;
               when others =>
                  null;
            end case;
         end loop;
         while Exp > 0 loop
            if Exp mod 2 = 1 then
               Val := Val * Base;
            end if;
            Exp := Exp / 2;
            Base := Base * Base;
         end loop;
      end if;

      if Pos /= Len then
         return (Status => Value_Err_Trailing_Chars, Pos => Pos);
      end if;

      if not Is_Neg then
         Val := -Val;
      end if;

      return (Status => Value_Ok, Val => Val);
   end Value_I64;

   -- From patch attached to https://gna.org/bugs/index.php?18352
   -- thanks to Christophe Curis https://gna.org/users/lobotomy
   function Value_F64 (S : Std_String_Basep;
                       Len : Ghdl_Index_Type;
                       Init_Pos : Ghdl_Index_Type) return Value_F64_Result
   is
      Pos     : Ghdl_Index_Type := Init_Pos;
      C       : Character;
      Is_Negative, Is_Neg_Exp : Boolean := False;
      Base    : Ghdl_F64;
      Intg    : Ghdl_I32;
      Val, Df : Ghdl_F64;
      Sep     : Character;
      FrcExp  : Ghdl_F64;
   begin
      C := S (Pos);
      if C = '-' then
         Is_Negative := True;
         Pos := Pos + 1;
      elsif C = '+' then
         Pos := Pos + 1;
      end if;

      if Pos >= Len then
         return (Status => Value_Err_No_Digit, Pos => Pos);
      end if;

      -- Read Integer-or-Base part (may be optional)
      Intg := 0;
      while Pos < Len loop
         C := S (Pos);
         if C in '0' .. '9' then
            Intg := Intg * 10 + Character'Pos (C) - Character'Pos ('0');
         elsif C /= '_' then
            exit;
         end if;
         Pos := Pos + 1;
      end loop;

      if Pos = Len then
         return (Status => Value_Ok, Val => Ghdl_F64 (Intg));
      end if;

      -- Special case: base was specified
      if C = '#' or C = ':' then
         if Intg < 2 or Intg > 16 then
            return (Status => Value_Err_Bad_Base, Pos => Pos);
         end if;
         Base := Ghdl_F64 (Intg);
         Val  := 0.0;
         Sep  := C;
         Pos  := Pos + 1;
         if Pos >= Len then
            return (Status => Value_Err_No_Digit, Pos => Pos);
         end if;

         -- Get the Integer part of the Value
         while Pos < Len loop
            C := S (Pos);
            case C is
               when '0' .. '9' =>
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('0') );
               when 'A' .. 'F' =>
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('A') + 10);
               when 'a' .. 'f' =>
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('a') + 10);
               when others =>
                  exit;
            end case;
            if C /= '_' then
               if Df >= Base then
                  return (Status => Value_Err_Bad_Digit, Pos => Pos);
               end if;
               Val := Val * Base + Df;
            end if;
            Pos := Pos + 1;
         end loop;
         if Pos >= Len then
            return (Status => Value_Err_Bad_End_Sign, Pos => Pos);
         end if;
      else
         Base := 10.0;
         Sep  := ' ';
         Val  := Ghdl_F64 (Intg);
      end if;

      -- Handle the Fractional part
      if C = '.' then
         Pos := Pos + 1;
         FrcExp := 1.0;
         while Pos < Len loop
            C := S (Pos);
            case C is
               when '0' .. '9' =>
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('0'));
               when 'A' .. 'F' =>
                  exit when Sep = ' ';
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('A') + 10);
               when 'a' .. 'f' =>
                  exit when Sep = ' ';
                  Df := Ghdl_F64 (Character'Pos (C) - Character'Pos('a') + 10);
               when others =>
                  exit;
            end case;
            if C /= '_' then
               FrcExp := FrcExp / Base;
               if Df > Base then
                  return (Status => Value_Err_Bad_Digit, Pos => Pos);
               end if;
               Val := Val + Df * FrcExp;
            end if;
            Pos := Pos + 1;
         end loop;
      end if;

      -- If base was specified, we must find here the end marker
      if Sep /= ' ' then
         if Pos >= Len or else C /= Sep then
            return (Status => Value_Err_Bad_End_Sign, Pos => Pos);
         end if;
         Pos := Pos + 1;
      end if;

      -- Handle exponent
      if Pos < Len then
         C := S (Pos);
         if C = 'e' or C = 'E' then
            Pos := Pos + 1;
            if Pos >= Len then
               return (Status => Value_Err_No_Digit, Pos => Pos);
            end if;
            C := S (Pos);
            if C = '-' then
               Is_Neg_Exp := True;
               Pos := Pos + 1;
            elsif C = '+' then
               Pos := Pos + 1;
            end if;
            Intg := 0;
            while Pos < Len loop
               C := S (Pos);
               if C in '0' .. '9' then
                  Intg := Intg * 10 + Character'Pos (C) - Character'Pos ('0');
               else
                  exit;
               end if;
               Pos := Pos + 1;
            end loop;
            -- This Exponentiation method is sub-optimal,
            -- but it does not depend on any library
            FrcExp := 1.0;
            if Is_Neg_Exp then
               while Intg > 0 loop
                  FrcExp := FrcExp / 10.0;
                  Intg := Intg - 1;
               end loop;
            else
               while Intg > 0 loop
                  FrcExp := FrcExp * 10.0;
                  Intg := Intg - 1;
               end loop;
            end if;
            Val := Val * FrcExp;
         end if;
      end if;

      if Pos /= Len then
         return (Status => Value_Err_Trailing_Chars, Pos => Pos);
      end if;

      if Is_Negative then
         Val := -Val;
      end if;

      return (Status => Value_Ok, Val => Val);
   end Value_F64;
end Grt.To_Strings;
