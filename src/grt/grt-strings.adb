--  GHDL Run Time (GRT) - Misc subprograms for characters and strings
--  Copyright (C) 2016 Tristan Gingold
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

package body Grt.Strings is
   function Is_Whitespace (C : in Character) return Boolean is
      use ASCII;
   begin
      return C = ' ' or C = NBSP or C = HT;
   end Is_Whitespace;

   function First_Non_Whitespace_Pos (Str : String) return Integer is
   begin
      for I in Str'Range loop
         if not Is_Whitespace (Str (I)) then
            return I;
         end if;
      end loop;
      return -1;
   end First_Non_Whitespace_Pos;

   function Last_Non_Whitespace_Pos (Str : String) return Integer is
   begin
      for Index in reverse Str'Range loop
         if not Is_Whitespace (Str (Index)) then
            return Index;
         end if;
      end loop;
      return -1;
   end Last_Non_Whitespace_Pos;

   function New_Line_Pos (Line : String) return Integer is
   begin
      return Find (Line, ASCII.LF);
   end New_Line_Pos;

   function Find (Str : String; Char : Character) return Integer is
   begin
      for Index in Str'Range loop
         if Str (Index) = Char then
            return Index;
         end if;
      end loop;
      return -1;
   end Find;
   function Find (Str : String; Char : Character; Start : Positive)
                 return Integer is
   begin
      return Find (Str (Start .. Str'Last), Char);
   end Find;

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

   function Value (Str : String) return Integer
   is
      Decimal : Positive;
      Value_Tmp : Natural;
      Digit : Integer;
   begin
      Decimal := 1;
      Value_Tmp := 0;

      for Index in reverse Str'Range loop
         Digit := Value (Str (Index));
         if Digit = -1 then
            return -1;
         end if;
         Value_Tmp := Value_Tmp + Digit * Decimal;
         Decimal := Decimal * 10;
      end loop;
      return Value_Tmp;
   end Value;

   function Value (Char : Character) return Integer is
   begin
      case Char is
         when '0' .. '9' =>
            return Character'Pos (Char) - Character'Pos ('0');
         when others =>
            return -1;
      end case;
   end Value;
end Grt.Strings;
