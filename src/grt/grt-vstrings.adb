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

with Interfaces;
with Grt.Errors; use Grt.Errors;
with Grt.C; use Grt.C;
with Grt.Fcvt;

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

   procedure Reset (Vstr : in out Vstring) is
   begin
      Vstr.Len := 0;
   end Reset;

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

   function Get_C_String (Vstr : Vstring) return Ghdl_C_String is
   begin
      return To_Ghdl_C_String (Vstr.Str.all'Address);
   end Get_C_String;

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

   procedure To_String (Str : out String; Last : out Natural; N : Ghdl_F64) is
   begin
      Grt.Fcvt.Format_Image (Str, Last, Interfaces.IEEE_Float_64 (N));
   end To_String;

   procedure To_String (Str : out String;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Nbr_Digits : Ghdl_I32) is
   begin
      Grt.Fcvt.Format_Digits
        (Str, Last, Interfaces.IEEE_Float_64 (N), Natural (Nbr_Digits));
   end To_String;

   procedure To_String (Str : out String_Real_Format;
                        Last : out Natural;
                        N : Ghdl_F64;
                        Format : Ghdl_C_String)
   is
      procedure Snprintf_Fmtf (Str : in out String;
                               Len : Natural;
                               Format : Ghdl_C_String;
                               V : Ghdl_F64);
      pragma Import (C, Snprintf_Fmtf, "__ghdl_snprintf_fmtf");
   begin
      --  FIXME: check format ('%', f/g/e/a)
      Snprintf_Fmtf (Str, Str'Length, Format, N);
      Last := strlen (To_Ghdl_C_String (Str'Address));
   end To_String;

   procedure To_String (Str : out String_Time_Unit;
                        First : out Natural;
                        Value : Ghdl_I64;
                        Unit : Ghdl_I64)
   is
      V, U : Ghdl_I64;
      D : Natural;
      P : Natural := Str'Last;
      Has_Digits : Boolean;
   begin
      --  Always work on negative values.
      if Value > 0 then
         V := -Value;
      else
         V := Value;
      end if;

      Has_Digits := False;
      U := Unit;
      loop
         if U = 1 then
            if Has_Digits then
               Str (P) := '.';
               P := P - 1;
            else
               Has_Digits := True;
            end if;
         end if;

         D := Natural (-(V rem 10));
         if D /= 0 or else Has_Digits then
            Str (P) := Character'Val (48 + D);
            P := P - 1;
            Has_Digits := True;
         end if;
         U := U / 10;
         V := V / 10;
         exit when V = 0 and then U = 0;
      end loop;
      if not Has_Digits then
         Str (P) := '0';
      else
         P := P + 1;
      end if;
      if Value < 0 then
         P := P - 1;
         Str (P) := '-';
      end if;
      First := P;
   end To_String;
end Grt.Vstrings;
