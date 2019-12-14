--  GHDL Run Time (GRT) - 'value subprograms.
--  Copyright (C) 2002 - 2016 Tristan Gingold
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
with Grt.Errors; use Grt.Errors;
with Grt.Rtis_Utils;
with Grt.Strings; use Grt.Strings;

package body Grt.Values is

   NBSP : constant Character := Character'Val (160);
   HT : constant Character := Character'Val (9);

   --  Increase POS to skip leading whitespace characters, decrease LEN to
   --  skip trailing whitespaces in string S.
   procedure Remove_Whitespaces (S     : Std_String_Basep;
                                 Len   : in out Ghdl_Index_Type;
                                 Pos   : in out Ghdl_Index_Type) is
   begin
      --  GHDL: allow several leading whitespace.
      while Pos < Len loop
         exit when not Is_Whitespace (S (Pos));
         Pos := Pos + 1;
      end loop;

      --  GHDL: allow several leading whitespace.
      while Len > Pos loop
         exit when not Is_Whitespace (S (Len - 1));
         Len := Len - 1;
      end loop;
      if Pos = Len then
         Error_E ("'value: empty string");
      end if;
   end Remove_Whitespaces;

   --  Return TRUE iff user string S (POS .. LEN - 1) is equal to REF.
   --  Comparaison is case insensitive, but REF must be lowercase (REF is
   --  supposed to come from an RTI).
   function String_Match (S : Std_String_Basep;
                          Pos : Ghdl_Index_Type;
                          Len : Ghdl_Index_Type;
                          Ref : Ghdl_C_String) return Boolean
   is
      Is_Char : constant Boolean := S (Pos) = ''';
      P : Ghdl_Index_Type;
      C_Ref, C_S : Character;
   begin
      P := 0;
      loop
         C_Ref := Ref (Natural (P + 1));
         if Pos + P = Len then
            --  End of string.
            return C_Ref = ASCII.NUL;
         end if;
         C_S := S (Pos + P);
         if not Is_Char then
            C_S := To_Lower (C_S);
         end if;
         if C_S /= C_Ref or else C_Ref = ASCII.NUL then
            return False;
         end if;
         P := P + 1;
      end loop;
   end String_Match;

   --  Return the value of STR for enumerated type RTI.
   function Value_Enum
     (S : Std_String_Basep; Len : Ghdl_Index_Type; Rti : Ghdl_Rti_Access)
      return Ghdl_Index_Type
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Pos : Ghdl_Index_Type;
      L : Ghdl_Index_Type;
   begin
      Pos := 0;
      L := Len;
      Remove_Whitespaces (S, L, Pos);

      for I in 0 .. Enum_Rti.Nbr - 1 loop
         if String_Match (S, Pos, L, Enum_Rti.Names (I)) then
            return I;
         end if;
      end loop;
      Error_S ("'value: '");
      Diag_C_Std (S (Pos .. L - 1));
      Diag_C ("' not in enumeration '");
      Diag_C (Enum_Rti.Name);
      Error_E ("'");
   end Value_Enum;

   function Ghdl_Value_Enum (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_Index_Type is
   begin
      return Value_Enum (Str.Base, Str.Bounds.Dim_1.Length, Rti);
   end Ghdl_Value_Enum;

   function Ghdl_Value_B1 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_B1
   is
   begin
      return Ghdl_B1'Val (Ghdl_Value_Enum (Str, Rti));
   end Ghdl_Value_B1;

   function Ghdl_Value_E8 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_E8
   is
   begin
      return Ghdl_E8'Val (Ghdl_Value_Enum (Str, Rti));
   end Ghdl_Value_E8;

   function Ghdl_Value_E32 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_E32
   is
   begin
      return Ghdl_E32'Val (Ghdl_Value_Enum (Str, Rti));
   end Ghdl_Value_E32;

   --  Convert S (INIT_POS .. LEN) to a signed integer.
   function Value_I64
     (S : Std_String_Basep; Len : Ghdl_Index_Type; Init_Pos : Ghdl_Index_Type)
     return Ghdl_I64
   is
      Pos : Ghdl_Index_Type := Init_Pos;
      C : Character;
      Sep : Character;
      Val, D, Base : Ghdl_I64;
      Exp : Integer;
      Is_Neg : Boolean;
   begin
      C := S (Pos);

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
            Error_E ("'value: missing digit after +/-");
         end if;
         Pos := Pos + 1;
         Is_Neg := C = '-';
         C := S (Pos);
      end if;

      Val := 0;
      loop
         if C in '0' .. '9' then
            Val := Val * 10 - (Character'Pos (C) - Character'Pos ('0'));
            Pos := Pos + 1;
            exit when Pos >= Len;
            C := S (Pos);
         else
            Error_E ("'value: decimal digit expected");
         end if;
         case C is
            when '_' =>
               Pos := Pos + 1;
               if Pos >= Len then
                  Error_E ("'value: trailing underscore");
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
         return Val;
      end if;

      if C = '#' or C = ':' then
         Base := -Val;
         Val := 0;
         Sep := C;
         Pos := Pos + 1;
         if Base < 2 or Base > 16 then
            Error_E ("'value: bad base");
         end if;
         if Pos >= Len then
            Error_E ("'value: missing based integer");
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
                  Error_E ("'value: digit expected");
            end case;
            if D >= Base then
               Error_E ("'value: digit >= base");
            end if;
            Val := Val * Base - D;
            Pos := Pos + 1;
            if Pos >= Len then
               Error_E ("'value: missing end sign number");
            end if;
            C := S (Pos);
            if C = '#' or C = ':' then
               if C /= Sep then
                  Error_E ("'value: sign number mismatch");
               end if;
               Pos := Pos + 1;
               exit;
            elsif C = '_' then
               Pos := Pos + 1;
               if Pos >= Len then
                  Error_E ("'value: no character after underscore");
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
            Error_E ("'value: no character after exponent");
         end if;
         C := S (Pos);
         if C = '+' then
            Pos := Pos + 1;
            if Pos >= Len then
               Error_E ("'value: no character after sign");
            end if;
            C := S (Pos);
         elsif C = '-' then
            Error_E ("'value: negativ exponent not allowed");
         end if;
         Exp := 0;
         loop
            if C in '0' .. '9' then
               Exp := Exp * 10 + Character'Pos (C) - Character'Pos ('0');
               Pos := Pos + 1;
               exit when Pos >= Len;
               C := S (Pos);
            else
               Error_E ("'value: decimal digit expected");
            end if;
            case C is
               when '_' =>
                  Pos := Pos + 1;
                  if Pos >= Len then
                     Error_E ("'value: trailing underscore");
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
         Error_E ("'value: trailing characters after blank");
      end if;

      if not Is_Neg then
         Val := -Val;
      end if;

      return Val;
   end Value_I64;

   function Ghdl_Value_I64 (Str : Std_String_Ptr) return Ghdl_I64
   is
      S : constant Std_String_Basep := Str.Base;
      Len : Ghdl_Index_Type := Str.Bounds.Dim_1.Length;
      Pos : Ghdl_Index_Type := 0;
   begin
      --  LRM 14.1
      --  Leading [and trailing] whitespace is allowed and ignored.
      --
      --  GHDL: allow several leading whitespace.
      Remove_Whitespaces (S, Len, Pos);

      return Value_I64 (S, Len, Pos);
   end Ghdl_Value_I64;

   function Ghdl_Value_I32 (Str : Std_String_Ptr) return Ghdl_I32
   is
   begin
      return Ghdl_I32 (Ghdl_Value_I64 (Str));
   end Ghdl_Value_I32;

   -- From patch attached to https://gna.org/bugs/index.php?18352
   -- thanks to Christophe Curis https://gna.org/users/lobotomy
   function Ghdl_Value_F64 (S : Std_String_Basep;
                            Len : Ghdl_Index_Type;
                            Init_Pos : Ghdl_Index_Type)
                           return Ghdl_F64
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
         Error_E ("'value: decimal digit expected");
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
         return Ghdl_F64 (Intg);
      end if;

      -- Special case: base was specified
      if C = '#' or C = ':' then
         if Intg < 2 or Intg > 16 then
            Error_E ("'value: bad base");
         end if;
         Base := Ghdl_F64 (Intg);
         Val  := 0.0;
         Sep  := C;
         Pos  := Pos + 1;
         if Pos >= Len then
            Error_E ("'value: missing based decimal");
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
                  Error_E ("'value: digit greater than base");
               end if;
               Val := Val * Base + Df;
            end if;
            Pos := Pos + 1;
         end loop;
         if Pos >= Len then
            Error_E ("'value: missing end sign number");
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
                  Error_E ("'value: digit greater than base");
               end if;
               Val := Val + Df * FrcExp;
            end if;
            Pos := Pos + 1;
         end loop;
      end if;

      -- If base was specified, we must find here the end marker
      if Sep /= ' ' then
         if Pos >= Len then
            Error_E ("'value: missing end sign number");
         end if;
         if C /= Sep then
            Error_E ("'value: sign number mismatch");
         end if;
         Pos := Pos + 1;
      end if;

      -- Handle exponent
      if Pos < Len then
         C := S (Pos);
         if C = 'e' or C = 'E' then
            Pos := Pos + 1;
            if Pos >= Len then
               Error_E ("'value: no character after exponent");
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
         Error_E ("'value: trailing characters after blank");
      end if;

      if Is_Negative then
         Val := -Val;
      end if;

      return Val;
   end Ghdl_Value_F64;

   -- From patch attached to https://gna.org/bugs/index.php?18352
   -- thanks to Christophe Curis https://gna.org/users/lobotomy
   function Ghdl_Value_F64 (Str : Std_String_Ptr) return Ghdl_F64
   is
      S       : constant Std_String_Basep := Str.Base;
      Len     : Ghdl_Index_Type  := Str.Bounds.Dim_1.Length;
      Pos     : Ghdl_Index_Type := 0;
   begin
      --  LRM 14.1
      --  Leading and trailing whitespace is allowed and ignored.
      --
      --  GHDL: allow several leading whitespace.
      Remove_Whitespaces (S, Len, Pos);

      return Ghdl_Value_F64 (S, Len, Pos);
   end Ghdl_Value_F64;

   procedure Ghdl_Value_Physical_Split (Str : Std_String_Ptr;
                                        Is_Real : out Boolean;
                                        Lit_Pos : out Ghdl_Index_Type;
                                        Lit_End : out Ghdl_Index_Type;
                                        Unit_Pos : out Ghdl_Index_Type)
   is
      S        : constant Std_String_Basep := Str.Base;
      Len      : Ghdl_Index_Type  := Str.Bounds.Dim_1.Length;
   begin
      --  LRM 14.1
      --  Leading and trailing whitespace is allowed and ignored.
      Lit_Pos := 0;
      Remove_Whitespaces (S, Len, Lit_Pos);

      --  Split between abstract literal (optionnal) and unit name.
      Lit_End := Lit_Pos;
      Is_Real := False;
      while Lit_End < Len loop
         exit when Is_Whitespace (S (Lit_End));
         if S (Lit_End) = '.' then
            Is_Real := True;
         end if;
         Lit_End := Lit_End + 1;
      end loop;
      if Lit_End = Len then
         --  No literal
         Unit_Pos := Lit_Pos;
         Lit_End := 0;
      else
         Unit_Pos := Lit_End + 1;
         while Unit_Pos < Len loop
            exit when not Is_Whitespace (S (Unit_Pos));
            Unit_Pos := Unit_Pos + 1;
         end loop;
      end if;
   end Ghdl_Value_Physical_Split;

   function Ghdl_Value_Physical_Type (Str : Std_String_Ptr;
                                      Rti : Ghdl_Rti_Access)
                                     return Ghdl_I64
   is
      S        : constant Std_String_Basep := Str.Base;
      Len      : Ghdl_Index_Type := Str.Bounds.Dim_1.Length;
      Unit_Pos : Ghdl_Index_Type;
      Lit_Pos  : Ghdl_Index_Type;
      Lit_End  : Ghdl_Index_Type;

      Found_Real : Boolean;

      Phys_Rti : constant Ghdl_Rtin_Type_Physical_Acc :=
        To_Ghdl_Rtin_Type_Physical_Acc (Rti);
      Unit_Name : Ghdl_C_String;
      Multiple : Ghdl_Rti_Access;
      Mult     : Ghdl_I64;
   begin
      --  Remove trailing whitespaces.  FIXME: also called in physical_split.
      Lit_Pos := 0;
      Remove_Whitespaces (S, Len, Lit_Pos);

      --  Extract literal and unit
      Ghdl_Value_Physical_Split (Str, Found_Real, Lit_Pos, Lit_End, Unit_Pos);

      --  Find unit value
      Multiple := null;
      for i in 0 .. Phys_Rti.Nbr - 1 loop
         Unit_Name :=
           Rtis_Utils.Get_Physical_Unit_Name (Phys_Rti.Units (i));
         if String_Match (S, Unit_Pos, Len, Unit_Name) then
            Multiple := Phys_Rti.Units (i);
            exit;
         end if;
      end loop;
      if Multiple = null then
         Error_S ("'value: unit '");
         Diag_C_Std (S (Unit_Pos .. Len - 1));
         Diag_C ("' not in physical type '");
         Diag_C (Phys_Rti.Name);
         Error_E ("'");
      end if;

      Mult := Grt.Rtis_Utils.Get_Physical_Unit_Value (Multiple, Rti);

      if Lit_End = 0 then
         return Mult;
      else
         if Found_Real then
            return Ghdl_I64
              (Ghdl_Value_F64 (S, Lit_End, Lit_Pos) * Ghdl_F64 (Mult));
         else
            return Value_I64 (S, Lit_End, Lit_Pos) * Mult;
         end if;
      end if;
   end Ghdl_Value_Physical_Type;

   function Ghdl_Value_P64 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I64
   is
   begin
      if Rti.Kind /= Ghdl_Rtik_Type_P64 then
         Error_E ("Physical_Type_64'value: incorrect RTI");
      end if;
      return Ghdl_Value_Physical_Type (Str, Rti);
   end Ghdl_Value_P64;

   function Ghdl_Value_P32 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I32
   is
   begin
      if Rti.Kind /= Ghdl_Rtik_Type_P32 then
         Error_E ("Physical_Type_32'value: incorrect RTI");
      end if;
      return Ghdl_I32 (Ghdl_Value_Physical_Type (Str, Rti));
   end Ghdl_Value_P32;

end Grt.Values;
