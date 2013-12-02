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

package body Grt.Values is

   NBSP : constant Character := Character'Val (160);
   HT : constant Character := Character'Val (9);

   procedure Remove_Whitespace(S     : in Std_String_Basep;
                               Pos   : in out Ghdl_Index_Type;
                               Len   : in Ghdl_Index_Type;
                               Chars : out Ghdl_B2) is
   begin
      Chars := False;
      --  GHDL: allow several leading whitespace.
      while Pos < Len loop
         case S (Pos) is
            when ' '
              | NBSP
              | HT =>
               Pos := Pos + 1;
            when others =>
               Chars := True;
               exit;
         end case;
      end loop;
   end Remove_Whitespace;

   procedure Stub_Error(S : String) is
   begin
      Error_E ("'value: function Ghdl_Value_" & S & " is a stub!"
               & "Please report as missing to http://gna.org/projects/ghdl");
   end Stub_Error;

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
                            Len : Ghdl_Index_Type;
                            Str : out String) is
   pragma unreferenced(Len);
   begin
      for i in Str'range loop
         Str(i) := LC(S(Pos));  -- LC it later
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
         Make_LC_String(S, Pos, Len, Str);
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
      -- Stub_Error("E8");
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

   function Ghdl_Value_I32 (Str : Std_String_Ptr) return Ghdl_I32
   is
      S : constant Std_String_Basep := Str.Base;
      Len : constant Ghdl_Index_Type := Str.Bounds.Dim_1.Length;
      Pos : Ghdl_Index_Type := 0;
      C : Character;
      Sep : Character;
      Val, D, Base : Ghdl_I32;
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
         Error_E ("'value: trailing characters after blank");
      end if;

      return Val;
   end Ghdl_Value_I32;

   function Ghdl_Value_F64 (Str : Std_String_Ptr) return Ghdl_F64 is
      pragma unreferenced(Str);
      Val : constant Ghdl_F64 := 0.0;
   begin
      Stub_Error("F64");
      return Val;
   end Ghdl_Value_F64;

   function Ghdl_Value_P64 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I64
   is
      pragma unreferenced(Str);
      pragma unreferenced(Rti);
      Val : constant Ghdl_I64 := 0;
   begin
      Stub_Error("P64");
      return Val;
   end Ghdl_Value_P64;

   function Ghdl_Value_P32 (Str : Std_String_Ptr; Rti : Ghdl_Rti_Access)
      return Ghdl_I32
   is
      pragma unreferenced(Str);
      pragma unreferenced(Rti);
      Val : constant Ghdl_I32 := 0;
   begin
      Stub_Error("P32");
      return Val;
   end Ghdl_Value_P32;

end Grt.Values;
