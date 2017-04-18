--  GHDL Run Time (GRT) - variable strings.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
with Grt.Stdio; use Grt.Stdio;
with Grt.Types; use Grt.Types;
with System; use System;

package Grt.Vstrings is
   pragma Preelaborate;
   --  A Vstring (Variable string) is an object which contains an unbounded
   --  string.
   type Vstring is limited private;

   --  Deallocate all storage internally allocated.
   procedure Free (Vstr : in out Vstring);

   --  Reset VSTR to an empty string.
   procedure Reset (Vstr : in out Vstring);

   --  Append a character.
   procedure Append (Vstr : in out Vstring; C : Character);

   --  Append a string.
   procedure Append (Vstr : in out Vstring; Str : String);

   --  Append a C string.
   procedure Append (Vstr : in out Vstring; Str : Ghdl_C_String);

   --  Get length of VSTR.
   function Length (Vstr : Vstring) return Natural;

   --  Truncate VSTR to LEN.
   --  It is an error if LEN is greater than the current length.
   procedure Truncate (Vstr : in out Vstring; Len : Natural);

   --  Display VSTR.
   procedure Put (Stream : FILEs; Vstr : Vstring);

   --  Get VSTR as a C String.  The NUL character must have been added.
   function Get_C_String (Vstr : Vstring) return Ghdl_C_String;

   --  A Rstring is link a Vstring but characters can only be prepended.
   type Rstring is limited private;

   --  Deallocate storage associated with Rstr.
   procedure Free (Rstr : in out Rstring);

   --  Prepend characters or strings.
   procedure Prepend (Rstr : in out Rstring; C : Character);
   procedure Prepend (Rstr : in out Rstring; Str : String);
   procedure Prepend (Rstr : in out Rstring; Str : Ghdl_C_String);

   --  Get the length of RSTR.
   function Length (Rstr : Rstring) return Natural;

   --  Return the address of the first character of RSTR.
   function Get_Address (Rstr : Rstring) return Address;

   --  Display RSTR.
   procedure Put (Stream : FILEs; Rstr : Rstring);

   --  Copy RSTR to STR, and return length of the string to LEN.
   procedure Copy (Rstr : Rstring; Str : in out String; Len : out Natural);

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

private
   subtype Fat_String is String (Positive);
   type Fat_String_Acc is access Fat_String;

   type Vstring is record
      Str : Fat_String_Acc := null;
      Max : Natural := 0;
      Len : Natural := 0;
   end record;

   type Rstring is record
      --  String whose bounds is (1 .. Max).
      Str : Fat_String_Acc := null;

      --  Last index in STR.
      Max : Natural := 0;

      --  Index of the first character.
      First : Natural := 1;
   end record;
end Grt.Vstrings;
