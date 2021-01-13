--  GHDL Run Time (GRT) stdio subprograms for GRT types.
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

package body Grt.Astdio is
   procedure Put (Stream : FILEs; Str : String)
   is
      S : size_t;
      pragma Unreferenced (S);
   begin
      S := fwrite (Str'Address, Str'Length, 1, Stream);
   end Put;

   procedure Put (Stream : FILEs; C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := fputc (Character'Pos (C), Stream);
   end Put;

   procedure Put (Stream : FILEs; Str : Ghdl_C_String)
   is
      Len : Natural;
      S : size_t;
      pragma Unreferenced (S);
   begin
      Len := strlen (Str);
      S := fwrite (Str (1)'Address, size_t (Len), 1, Stream);
   end Put;

   procedure New_Line (Stream : FILEs) is
   begin
      Put (Stream, Nl);
   end New_Line;

   procedure Put_Line (Stream : FILEs; Str : String) is
   begin
      Put (Stream, Str);
      New_Line (Stream);
   end Put_Line;

   procedure Put (Str : String)
   is
      S : size_t;
      pragma Unreferenced (S);
   begin
      S := fwrite (Str'Address, Str'Length, 1, stdout);
   end Put;

   procedure Put (C : Character)
   is
      R : int;
      pragma Unreferenced (R);
   begin
      R := fputc (Character'Pos (C), stdout);
   end Put;

   procedure Put (Str : Ghdl_C_String)
   is
      Len : Natural;
      S : size_t;
      pragma Unreferenced (S);
   begin
      Len := strlen (Str);
      S := fwrite (Str (1)'Address, size_t (Len), 1, stdout);
   end Put;

   procedure New_Line is
   begin
      Put (Nl);
   end New_Line;

   procedure Put_Line (Str : String)
   is
   begin
      Put (Str);
      New_Line;
   end Put_Line;

   generic
      type Ntype is range <>;
      Max_Len : Natural;
   procedure Put_Ntype (Stream : FILEs; N : Ntype);

   procedure Put_Ntype (Stream : FILEs; N : Ntype)
   is
      Str : String (1 .. Max_Len);
      P : Natural := Str'Last;
      V : Ntype;
   begin
      --  V is negativ.
      if N > 0 then
         V := -N;
      else
         V := N;
      end if;
      loop
         Str (P) := Character'Val (48 - (V rem 10)); -- V is <= 0.
         V := V / 10;
         exit when V = 0;
         P := P - 1;
      end loop;
      if N < 0 then
         P := P - 1;
         Str (P) := '-';
      end if;
      Put (Stream, Str (P .. Max_Len));
   end Put_Ntype;

   generic
      type Utype is mod <>;
      Max_Len : Natural;
   procedure Put_Utype (Stream : FILEs; N : Utype);

   procedure Put_Utype (Stream : FILEs; N : Utype)
   is
      Str : String (1 .. Max_Len);
      P : Natural := Str'Last;
      V : Utype := N;
   begin
      loop
         Str (P) := Character'Val (48 + (V rem 10));
         V := V / 10;
         exit when V = 0;
         P := P - 1;
      end loop;
      Put (Stream, Str (P .. Max_Len));
   end Put_Utype;

   procedure Put_I32_1 is new Put_Ntype (Ntype => Ghdl_I32, Max_Len => 11);
   procedure Put_I32 (Stream : FILEs; I32 : Ghdl_I32) renames Put_I32_1;

   procedure Put_U32_1 is new Put_Utype (Utype => Ghdl_U32, Max_Len => 11);
   procedure Put_U32 (Stream : FILEs; U32 : Ghdl_U32) renames Put_U32_1;

   procedure Put_I64_1 is new Put_Ntype (Ntype => Ghdl_I64, Max_Len => 20);
   procedure Put_I64 (Stream : FILEs; I64 : Ghdl_I64) renames Put_I64_1;

   procedure Put_U64_1 is new Put_Utype (Utype => Ghdl_U64, Max_Len => 20);
   procedure Put_U64 (Stream : FILEs; U64 : Ghdl_U64) renames Put_U64_1;

   procedure Put_F64 (Stream : FILEs; F64 : Ghdl_F64)
   is
      procedure Fprintf_G (Stream : FILEs;
                           Arg : Ghdl_F64);
      pragma Import (C, Fprintf_G, "__ghdl_fprintf_g");
   begin
      Fprintf_G (Stream, F64);
   end Put_F64;

   Hex_Map : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   procedure Put (Stream : FILEs; Addr : System.Address)
   is
      Res : String (1 .. System.Word_Size / 4);
      Val : Integer_Address := To_Integer (Addr);
   begin
      for I in reverse Res'Range loop
         Res (I) := Hex_Map (Natural (Val and 15));
         Val := Val / 16;
      end loop;
      Put (Stream, Res);
   end Put;

end Grt.Astdio;
