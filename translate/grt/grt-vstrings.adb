--  GHDL Run Time (GRT) - variable strings.
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
with System.Storage_Elements; --  Work around GNAT bug.
pragma Unreferenced (System.Storage_Elements);
with Grt.Errors; use Grt.Errors;
with Grt.C; use Grt.C;

package body Grt.Vstrings is
   procedure Free (Fs : Fat_String_Acc);
   pragma Import (C, Free);

   function Malloc (Len : Natural) return Fat_String_Acc;
   pragma Import (C, Malloc);

   function Realloc (Ptr : Fat_String_Acc; Len : Natural)
                    return Fat_String_Acc;
   pragma Import (C, Realloc);


   procedure Free (Vstr : in out Vstring) is
   begin
      Free (Vstr.Str);
      Vstr := (Str => null,
               Max => 0,
               Len => 0);
   end Free;

   procedure Grow (Vstr : in out Vstring; Sum : Natural)
   is
      Nlen : constant Natural := Vstr.Len + Sum;
      Nmax : Natural;
   begin
      Vstr.Len := Nlen;
      if Nlen <= Vstr.Max then
         return;
      end if;
      if Vstr.Max = 0 then
         Nmax := 32;
      else
         Nmax := Vstr.Max;
      end if;
      while Nmax < Nlen loop
         Nmax := Nmax * 2;
      end loop;
      Vstr.Str := Realloc (Vstr.Str, Nmax);
      if Vstr.Str = null then
         Internal_Error ("grt.vstrings.grow: memory exhausted");
      end if;
      Vstr.Max := Nmax;
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
         Internal_Error ("grt.vstrings.truncate: bad len");
      end if;
      Vstr.Len := Len;
   end Truncate;

   procedure Put (Stream : FILEs; Vstr : Vstring)
   is
      S : size_t;
   begin
      S := size_t (Vstr.Len);
      if S > 0 then
         S := fwrite (Vstr.Str (1)'Address, S, 1, Stream);
      end if;
   end Put;

   procedure Free (Rstr : in out Rstring) is
   begin
      Free (Rstr.Str);
      Rstr := (Str => null,
               Max => 0,
               First => 0);
   end Free;

   function Length (Rstr : Rstring) return Natural is
   begin
      return Rstr.Max + 1 - Rstr.First;
   end Length;

   procedure Grow (Rstr : in out Rstring; Min : Natural)
   is
      Len : constant Natural := Length (Rstr);
      Nlen : constant Natural := Len + Min;
      Nstr : Fat_String_Acc;
      Nfirst : Natural;
      Nmax : Natural;
   begin
      if Nlen <= Rstr.Max then
         return;
      end if;
      if Rstr.Max = 0 then
         Nmax := 32;
      else
         Nmax := Rstr.Max;
      end if;
      while Nmax < Nlen loop
         Nmax := Nmax * 2;
      end loop;
      Nstr := Malloc (Nmax);
      Nfirst := Nmax + 1 - Len;
      if Rstr.Str /= null then
         Nstr (Nfirst .. Nmax) := Rstr.Str (Rstr.First .. Rstr.Max);
         Free (Rstr.Str);
      end if;
      Rstr := (Str => Nstr,
               Max => Nmax,
               First => Nfirst);
   end Grow;

   procedure Prepend (Rstr : in out Rstring; C : Character)
   is
   begin
      Grow (Rstr, 1);
      Rstr.First := Rstr.First - 1;
      Rstr.Str (Rstr.First) := C;
   end Prepend;

   procedure Prepend (Rstr : in out Rstring; Str : String)
   is
   begin
      Grow (Rstr, Str'Length);
      Rstr.First := Rstr.First - Str'Length;
      Rstr.Str (Rstr.First .. Rstr.First + Str'Length - 1) := Str;
   end Prepend;

   procedure Prepend (Rstr : in out Rstring; Str : Ghdl_C_String)
   is
      L : constant Natural := strlen (Str);
   begin
      Grow (Rstr, L);
      Rstr.First := Rstr.First - L;
      Rstr.Str (Rstr.First .. Rstr.First + L - 1) := Str (1 .. L);
   end Prepend;

   function Get_Address (Rstr : Rstring) return Address
   is
   begin
      return Rstr.Str (Rstr.First)'Address;
   end Get_Address;

   procedure Copy (Rstr : Rstring; Str : in out String; Len : out Natural)
   is
   begin
      Len := Length (Rstr);
      if Len > Str'Length then
         Str := Rstr.Str (Rstr.First .. Rstr.First + Str'Length - 1);
      else
         Str (Str'First .. Str'First + Len - 1) :=
           Rstr.Str (Rstr.First .. Rstr.First + Len - 1);
      end if;
   end Copy;

   procedure Put (Stream : FILEs; Rstr : Rstring)
   is
      S : size_t;
      pragma Unreferenced (S);
   begin
      S := fwrite (Get_Address (Rstr), size_t (Length (Rstr)), 1, Stream);
   end Put;

   generic
      type Ntype is range <>;
      --Max_Len : Natural;
   procedure Gen_To_String (Str : out String; First : out Natural; N : Ntype);

   procedure Gen_To_String (Str : out String; First : out Natural; N : Ntype)
   is
      subtype R_Type is String (1 .. Str'Length);
      S : R_Type renames Str;
      P : Natural := S'Last;
      V : Ntype;
   begin
      if N > 0 then
         V := -N;
      else
         V := N;
      end if;
      loop
         S (P) := Character'Val (48 - (V rem 10));
         V := V / 10;
         exit when V = 0;
         P := P - 1;
      end loop;
      if N < 0 then
         P := P - 1;
         S (P) := '-';
      end if;
      First := P;
   end Gen_To_String;

   procedure To_String_I32 is new Gen_To_String (Ntype => Ghdl_I32);

   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I32)
     renames To_String_I32;

   procedure To_String_I64 is new Gen_To_String (Ntype => Ghdl_I64);

   procedure To_String (Str : out String; First : out Natural; N : Ghdl_I64)
     renames To_String_I64;
end Grt.Vstrings;
