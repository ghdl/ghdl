--  GHDL Run Time (GRT) - variable strings.
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

with Grt.Types; use Grt.Types;
with System; use System;

package Grt.Rstrings is
   pragma Preelaborate;
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

   --  Copy RSTR to STR, and return length of the string to LEN.
   procedure Copy (Rstr : Rstring; Str : in out String; Len : out Natural);

private
   type Rstring is record
      --  String whose bounds is (1 .. Max).
      Str : Ghdl_C_String := null;

      --  Last index in STR.
      Max : Natural := 0;

      --  Index of the first character.
      First : Natural := 1;
   end record;
end Grt.Rstrings;
