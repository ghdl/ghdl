--  GHDL Run Time (GRT) - 'value subprograms.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Grt.Errors; use Grt.Errors;
with System;
with Ada.Unchecked_Conversion;

package body Grt.Values is

   NBSP : constant Character := Character'Val (160);
   HT : constant Character := Character'Val (9);

   function White (C : in Character) return Boolean is
   begin
      return C = ' ' or C = NBSP or C = HT;
   end White;

   procedure Remove_Whitespace(S     : in Std_String_Basep;
                               Pos   : in out Ghdl_Index_Type;
                               Len   : in Ghdl_Index_Type;
                               Chars : out Ghdl_B2) is
   begin
      Chars := False;
      --  GHDL: allow several leading whitespace.
      while Pos < Len loop
         if White (S (Pos)) then
            Pos := Pos + 1;
         else
            Chars := True;
            exit;
         end if;
      end loop;
   end Remove_Whitespace;

   function LC(C : in Character) return Character is
   begin
      if C >= 'A' and then C <= 'Z' then
         return Character'val(Character'pos(C) + Character'pos('a')
                                               - Character'pos('A'));
      else
         return C;
      end if;
   end LC;

   procedure Make_LC_String(S : Std_String_Basep;
                            Pos : in out Ghdl_Index_Type;
                            Str : out String) is
   begin
      for i in Str'range loop
         Str(i) := LC(S(Pos));
         Pos := Pos + 1;
      end loop;
   end Make_LC_String;

   function StringMatch(Str : String; EnumStr : Ghdl_C_String) return boolean
   is
      EnumLen : constant Natural := strlen(EnumStr);
   begin
      for j in Str'range loop
         if j > EnumLen or else Str(j) /= EnumStr(j) then
            return false;
         end if;
      end loop;
      if Str'last = EnumLen then
         return true;
      else
         return false;
      end if;
   end StringMatch;

   function Ghdl_Value_Enum (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_Index_Type
   is
      Val   : Ghdl_Index_Type := 0;
      S     : constant Std_String_Basep := Str.Base;
      Len   : constant Ghdl_Index_Type  := Str.Bounds.Dim_1.Length;
      Pos   : Ghdl_Index_Type := 0;
      Chars : Ghdl_B2;
      Enum_Rti : Ghdl_Rtin_Type_Enum_Acc;

   begin
      Remove_Whitespace(S, Pos, Len, Chars);
      if Pos = Len then
         Error_E ("'value: empty string");
      end if;

      Enum_Rti := To_Ghdl_Rtin_Type_Enum_Acc (Rti);

      declare
         Str     : String(1..Natural(Len - Pos));
         Found   : Boolean := False;
      begin
         Make_LC_String(S, Pos, Str);
         for i in 0 .. Enum_Rti.Nbr - 1 loop
            if StringMatch(Str,  Enum_Rti.Names.all(i)) then
               Found := True;
               Val := i;
               exit;
            end if;
         end loop;
         if not Found then
            Error_E ("'value: " & Str & " not in enumeration " &
                     Enum_Rti.Name.all(1..strlen(Enum_Rti.Name)));
         end if;
      end;

      Remove_Whitespace(S, Pos, Len, Chars);
      if Chars then
         Error_E ("'value: trailing characters after blank");
      end if;
      return Val;
   end Ghdl_Value_Enum;

   function Ghdl_Value_B2 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_B2
   is
   begin
      return Ghdl_B2'Val(Ghdl_Value_Enum (Str , Rti ));
   end Ghdl_Value_B2;

   function Ghdl_Value_E8 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_E8
   is
   begin
      return Ghdl_E8'Val(Ghdl_Value_Enum (Str , Rti ));
   end Ghdl_Value_E8;

   function Ghdl_Value_E32 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_E32
   is
   begin
      return Ghdl_E32'Val(Ghdl_Value_Enum (Str , Rti ));
   end Ghdl_Value_E32;

   function Ghdl_Value_I64 (Str : Std_String_Ptr) return Ghdl_I64
   is
      S : constant Std_String_Basep := Str.Base;
      Len : constant Ghdl_Index_Type := Str.Bounds.Dim_1.Length;
      Pos : Ghdl_Index_Type := 0;
      C : Character;
      Sep : Character;
      Val, D, Base : Ghdl_I64;
      Exp : Integer;
      Chars : Ghdl_B2;
   begin
      --  LRM 14.1
      --  Leading [and trailing] whitespace is allowed and ignored.
      --
      --  GHDL: allow several leading whitespace.
      Remove_Whitespace(S, Pos, Len, Chars);
      if Pos = Len then
         Error_E ("'value: empty string");
      end if;
      C := S (Pos);

      --  Be user friendly.
      if C = '-' or C = '+' then
         Error_E ("'value: leading sign +/- not allowed");
      end if;

      Val := 0;
      loop
         if C in '0' .. '9' then
            Val := Val * 10 + Character'Pos (C) - Character'Pos ('0');
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
         return Val;
      end if;

      if C = '#' or C = ':' then
         Base := Val;
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
            if D > Base then
               Error_E ("'value: digit greather than base");
            end if;
            Val := Val * Base + D;
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

      --  LRM 14.1
      --  [Leading] and trailing whitespace is allowed and ignored.
      --
      --  GHDL: allow several trailing whitespace.
      Remove_Whitespace(S, Pos, Len, Chars);
      if Chars then
         Error_E ("integer'value: trailing characters after blank");
      end if;

      return Val;
   end Ghdl_Value_I64;

   function Ghdl_Value_I32 (Str : Std_String_Ptr) return Ghdl_I32
   is
   begin
      return Ghdl_I32 (Ghdl_Value_I64 (Str));
   end Ghdl_Value_I32;

   function Ghdl_Value_Physical_Type (Str : Std_String_Ptr;
                                      Rti : Ghdl_Rti_Access)
      return Ghdl_I64
   is
      function To_Std_String_Ptr is new Ada.Unchecked_Conversion
         (Source => System.Address, Target => Std_String_Ptr);
      function To_Std_String_Boundp is new Ada.Unchecked_Conversion
         (Source => System.Address, Target => Std_String_Boundp);

      S     : aliased Std_String := Str.all;
      Bound : aliased Std_String_Bound := Str.Bounds.all;
      Start, Finish : Ghdl_Index_Type;
      Found_Real    : Boolean := false;

      Phys_Rti : Ghdl_Rtin_Type_Physical_Acc;
      Unit     : Ghdl_Rtin_Unit_Acc;
      Multiple : Ghdl_Rti_Unit_Val;
      Mult     : Ghdl_I64;
   begin
      Phys_Rti := To_Ghdl_Rtin_Type_Physical_Acc (Rti);

      S.Bounds := To_Std_String_Boundp(Bound'Address);
      -- find characters at the end...
      Finish := Ghdl_Index_Type(Bound.Dim_1.Length)-1;
      while White(S.Base.all(Finish)) loop
         Finish := Finish - 1;
      end loop;
      Start := Finish;
      while not White(S.Base.all(Start - 1)) loop
         Start := Start - 1;
      end loop;
      -- shorten Bounds to exclude non-numeric part
      Bound.Dim_1.Right := Bound.Dim_1.Right
                          - Std_Integer(Bound.Dim_1.Length - Start);
      Bound.Dim_1.Length := Start;
      -- does the string represent a Real?
      for i in 0 .. Start loop
         if S.Base.all(i) = '.' then
            Found_Real := true;
         end if;
      end loop;

      declare
         Unit_Str : String(1 .. Natural(1 + Finish - Start));
         Found    : Boolean := False;
      begin
         Make_LC_String(Str.Base, Start, Unit_Str);
         for i in 0 .. Phys_Rti.Nbr - 1 loop
            Unit := To_Ghdl_Rtin_Unit_Acc(Phys_Rti.Units(i));
            if StringMatch(Unit_Str, Unit.Name) then
               Found := True;
               Multiple := To_Ghdl_Rtin_Unit_Acc (Phys_Rti.Units (i)).Value;
               exit;
            end if;
         end loop;
         if not Found then
            Error_E ("'value: Unit " & Unit_Str & " not in physical type" &
                     Phys_Rti.Name.all(1..strlen(Phys_Rti.Name)));
         end if;
      end;

      if Rti.Kind = Ghdl_Rtik_Type_P64 then
         Mult := Ghdl_I64(Multiple.Unit_64);
      else
         Mult := Ghdl_I64(Multiple.Unit_32);
      end if;

      if Found_Real then
         return Ghdl_I64 (Ghdl_Value_F64 (To_Std_String_Ptr(S'Address))
                                * Ghdl_F64 (Mult));
      else
         return Ghdl_Value_I64 (To_Std_String_Ptr(S'Address)) * Mult;
      end if;
   end Ghdl_Value_Physical_Type;

   function Ghdl_Value_P64 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I64
   is
   begin
      if Rti.Kind /= Ghdl_Rtik_Type_P64 then
         Error_E ("Physical_Type_64'value: incorrect RTI");
      end if;
      return Ghdl_Value_Physical_Type(Str, Rti);
   end Ghdl_Value_P64;

   function Ghdl_Value_P32 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I32
   is
   begin
      if Rti.Kind /= Ghdl_Rtik_Type_P32 then
         Error_E ("Physical_Type_32'value: incorrect RTI");
      end if;
      return Ghdl_I32(Ghdl_Value_Physical_Type(Str, Rti));
   end Ghdl_Value_P32;

   -- From patch attached to https://gna.org/bugs/index.php?18352
   -- thanks to Christophe Curis https://gna.org/users/lobotomy
   function Ghdl_Value_F64 (Str : Std_String_Ptr) return Ghdl_F64
   is
      S       : constant Std_String_Basep := Str.Base;
      Len     : constant Ghdl_Index_Type  := Str.Bounds.Dim_1.Length;
      Pos     : Ghdl_Index_Type := 0;
      C       : Character;
      Chars   : Ghdl_B2;
      Is_Negative, Is_Neg_Exp : Boolean := False;
      Base    : Ghdl_F64;
      Intg    : Ghdl_I32;
      Val, Df : Ghdl_F64;
      Sep     : Character;
      FrcExp  : Ghdl_F64;
   begin
      --  LRM 14.1
      --  Leading [and trailing] whitespace is allowed and ignored.
      --
      --  GHDL: allow several leading whitespace.
      Remove_Whitespace(S, Pos, Len, Chars);

      if Pos = Len then
         Error_E ("'value: empty string");
      end if;

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

      --  LRM 14.1
      --  [Leading] and trailing whitespace is allowed and ignored.
      --
      --  GHDL: allow several leading whitespace.
      Remove_Whitespace(S, Pos, Len, Chars);
      if Chars then
         Error_E ("'value: trailing characters after blank");
      end if;

      if Is_Negative then
         Val := -Val;
      end if;

      return Val;
   end Ghdl_Value_F64;

end Grt.Values;
