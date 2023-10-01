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

package Grt.Vstrings is
   pragma Preelaborate;
   --  A Vstring (Variable string) is an object which contains an unbounded
   --  string.
   type Vstring (Threshold : Natural) is limited private;

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

   --  Return the address of the first character of VSTR.
   function Get_Address (Vstr : Vstring) return Address;

   --  Get VSTR as a C String.  The NUL character must have been added.
   function Get_C_String (Vstr : Vstring) return Ghdl_C_String;

private
   type Vstring (Threshold : Natural) is limited record
      Str : Ghdl_C_String := null;
      Max : Natural := 0;
      Len : Natural := 0;

      --  The fixed buffer, used when strings len is less than threshold.
      Fixed : String (1 .. Threshold);
   end record;
end Grt.Vstrings;
