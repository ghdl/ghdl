--  GHDL Run Time (GRT) -  to_string subprograms.
--  Copyright (C) 2002 - 2019 Tristan Gingold
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
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

package Grt.To_Strings is
   --  Write the image of N into STR padded to the right.  FIRST is the index
   --  of the first character, so the result is in STR (FIRST .. STR'last).
   --  Requires at least 11 characters.
   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I32);

   --  Write the image of N into STR padded to the right.  FIRST is the index
   --  of the first character, so the result is in STR (FIRST .. STR'last).
   --  Requires at least 21 characters.
   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I64);

   --  Write the image of N into STR.  LAST is the index of the last character,
   --  so the result is in STR (STR'first .. LAST).
   --  Requires at least 24 characters.
   --  Sign (1) + digit (1) + dot (1) + digits (15) + exp (1) + sign (1)
   --  + exp_digits (4) -> 24.
   procedure To_String (Str : out String; Last : out Natural; N : Ghdl_F64);

   --  Write the image of N into STR using NBR_DIGITS digits after the decimal
   --  point.
   procedure To_String (Str : out String;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Nbr_Digits : Ghdl_I32);

   subtype String_Real_Format is String (1 .. 128);

   --  Write the image of N into STR using NBR_DIGITS digits after the decimal
   --  point.
   procedure To_String (Str : out String_Real_Format;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Format : Ghdl_C_String);

   --  Write the image of VALUE to STR using UNIT as unit.  The output is in
   --  STR (FIRST .. STR'last).
   subtype String_Time_Unit is String (1 .. 22);
   procedure To_String (Str : out String_Time_Unit;
                        First : out Natural;
                        Value : Ghdl_I64;
                        Unit : Ghdl_I64);

   type Value_Status is
     (
      Value_Ok,
      Value_Err_No_Digit,  -- After sign, at start, after exponent...
      Value_Err_Bad_Digit,
      Value_Err_Underscore,
      Value_Err_Bad_Base,
      Value_Err_Bad_End_Sign,  --  Missing or mismatch
      Value_Err_Bad_Exponent,
      Value_Err_Trailing_Chars
     );
   subtype Value_Status_Error is Value_Status range
     Value_Status'Succ (Value_Ok) .. Value_Status'Last;

   type Value_I64_Result (Status : Value_Status := Value_Ok) is record
      case Status is
         when Value_Ok =>
            Val : Ghdl_I64;
         when others =>
            Pos : Ghdl_Index_Type;
      end case;
   end record;

   --  Convert S (INIT_POS .. LEN) to a signed integer.
   function Value_I64 (S : Std_String_Basep;
                       Len : Ghdl_Index_Type;
                       Init_Pos : Ghdl_Index_Type) return Value_I64_Result;

   type Value_F64_Result (Status : Value_Status := Value_Ok) is record
      case Status is
         when Value_Ok =>
            Val : Ghdl_F64;
         when others =>
            Pos : Ghdl_Index_Type;
      end case;
   end record;

   --  Convert S (INIT_POS .. LEN) to a floating point number.
   function Value_F64 (S : Std_String_Basep;
                       Len : Ghdl_Index_Type;
                       Init_Pos : Ghdl_Index_Type) return Value_F64_Result;
end Grt.To_Strings;
