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

package Grt.Strings is
   pragma Pure;

   NBSP : constant Character := Character'Val (160);

   --  Return True IFF C is a whitespace character (as defined in LRM93 14.3)
   function Is_Whitespace (C : in Character) return Boolean;

   -- The following functions return -1 in case there is no match in string ---

   -- Return the index of the first non whitespace character in string
   function First_Non_Whitespace_Pos (Str : String) return Integer;

   -- Return the index of the last non whitespace character in string
   function Last_Non_Whitespace_Pos (Str : String) return Integer;

   -- Return the index of the new line character (ASCII.LF) in string
   function New_Line_Pos (Line : String) return Integer;

   -- Return the index of the first character that matches Char in string
   function Find (Str : String; Char : Character) return Integer;
   function Find (Str : String; Char : Character; Start : Positive)
                 return Integer;

   ----------------------------------------------------------------------------

   --  Convert C/S to lowercase.
   function To_Lower (C : Character) return Character;

   -- Str/Char : image of a natural number/digit
   function Value (Str : String) return Integer;
   function Value (Char : Character) return Integer;
end Grt.Strings;
