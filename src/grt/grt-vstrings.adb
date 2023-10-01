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

with Grt.C; use Grt.C;

package body Grt.Vstrings is
   procedure Free (Vstr : in out Vstring) is
   begin
      if Vstr.Max > Vstr.Threshold then
         Free (To_Address (Vstr.Str));
      end if;
      Vstr.Max := 0;
      Vstr.Len := 0;
      Vstr.Str := null;
   end Free;

   procedure Reset (Vstr : in out Vstring) is
   begin
      Vstr.Len := 0;
   end Reset;

   procedure Grow (Vstr : in out Vstring; Sum : Natural)
   is
      Nlen : constant Natural := Vstr.Len + Sum;
      Nmax : Natural;
   begin
      if Nlen <= Vstr.Max then
         Vstr.Len := Nlen;
         return;
      end if;

      if Vstr.Max = 0 then
         --  Initialization.
         pragma Assert (Vstr.Str = null);
         Vstr.Max := Vstr.Threshold;

         if Nlen <= Vstr.Threshold then
            --  Use the fixed part.
            Vstr.Str := To_Ghdl_C_String (Vstr.Fixed'Address);
            Vstr.Len := Nlen;
            return;
         end if;

         if Vstr.Max /= 0 then
            Nmax := Vstr.Max;
         else
            Nmax := 32;
         end if;
      else
         Nmax := Vstr.Max;
      end if;

      --  Compute new maximum.  Must be large enough to contain the new
      --  strings.
      while Nmax < Nlen loop
         Nmax := Nmax * 2;
      end loop;

      --  Realloc buffer.
      if Vstr.Max <= Vstr.Threshold then
         --  Move from static buffer to allocaed buffer.
         Vstr.Str := To_Ghdl_C_String (Malloc (size_t (Nmax)));
         Vstr.Str (1 .. Vstr.Len) := Vstr.Fixed (1 .. Vstr.Len);
      else
         Vstr.Str := To_Ghdl_C_String
           (Realloc (To_Address (Vstr.Str), size_t (Nmax)));
      end if;
      if Vstr.Str = null then
         --  Memory exhausted.
         raise Storage_Error;
      end if;
      Vstr.Max := Nmax;
      Vstr.Len := Nlen;
   end Grow;

   procedure Append (Vstr : in out Vstring; C : Character)
   is
   begin
      Grow (Vstr, 1);
      Vstr.Str (Vstr.Len) := C;
   end Append;

   procedure Append (Vstr : in out Vstring; Str : String)
   is
      S : constant Natural := Vstr.Len;
   begin
      Grow (Vstr, Str'Length);
      Vstr.Str (S + 1 .. S + Str'Length) := Str;
   end Append;

   procedure Append (Vstr : in out Vstring; Str : Ghdl_C_String)
   is
      S : constant Natural := Vstr.Len;
      L : constant Natural := strlen (Str);
   begin
      Grow (Vstr, L);
      Vstr.Str (S + 1 .. S + L) := Str (1 .. L);
   end Append;

   function Length (Vstr : Vstring) return Natural is
   begin
      return Vstr.Len;
   end Length;

   procedure Truncate (Vstr : in out Vstring; Len : Natural) is
   begin
      if Len > Vstr.Len then
         --  Incorrect length.
         raise Constraint_Error;
      end if;
      Vstr.Len := Len;
   end Truncate;

   function Get_Address (Vstr : Vstring) return Address is
   begin
      return To_Address (Vstr.Str);
   end Get_Address;

   function Get_C_String (Vstr : Vstring) return Ghdl_C_String is
   begin
      return Vstr.Str;
   end Get_C_String;
end Grt.Vstrings;
