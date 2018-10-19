--  GHDL Run Time (GRT) stdio subprograms for GRT types.
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
with Grt.C; use Grt.C;
with Grt.Options;

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

   procedure Put_Str_Len (Stream : FILEs; Str : Ghdl_Str_Len_Type)
   is
      S : String (1 .. 3);
   begin
      if Str.Str = null then
         S (1) := ''';
         S (2) := Character'Val (Str.Len);
         S (3) := ''';
         Put (Stream, S);
      else
         Put (Stream, Str.Str (1 .. Str.Len));
      end if;
   end Put_Str_Len;

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

   procedure Put_Dir (Stream : FILEs; Dir : Ghdl_Dir_Type) is
   begin
      case Dir is
         when Dir_To =>
            Put (Stream, " to ");
         when Dir_Downto =>
            Put (Stream, " downto ");
      end case;
   end Put_Dir;

   procedure Put_Time (Stream : FILEs; Time : Std_Time)
   is
      use Grt.Options;
      Unit : Natural_Time_Scale;
      T : Std_Time;
   begin
      if Time = Std_Time'First then
         Put (Stream, "-Inf");
      else
         --  Do not bother with sec, min, and hr.
         Unit := Time_Resolution_Scale;
         T := Time;
         while Unit > 1 and then (T mod 1_000) = 0 loop
            T := T / 1000;
            Unit := Unit - 1;
         end loop;
         Put_I64 (Stream, Ghdl_I64 (T));
         case Unit is
            when 0 =>
               Put (Stream, "sec");
            when 1 =>
               Put (Stream, "ms");
            when 2 =>
               Put (Stream, "us");
            when 3 =>
               Put (Stream, "ns");
            when 4 =>
               Put (Stream, "ps");
            when 5 =>
               Put (Stream, "fs");
         end case;
      end if;
   end Put_Time;

end Grt.Astdio;
