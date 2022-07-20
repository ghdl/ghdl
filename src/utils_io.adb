--  Common IO utilities.
--  Copyright (C) 2019 Tristan Gingold
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

with Ada.Unchecked_Conversion;

with Simple_IO; use Simple_IO;

package body Utils_IO is
   procedure Put_Indent (Indent : Natural) is
   begin
      Put (String'(1 .. Indent * 2 => ' '));
   end Put_Indent;

   --  Like Put, but without the leading space (if any).
   procedure Put_Trim (S : String) is
   begin
      if S'First <= S'Last and then S (S'First) = ' ' then
         Put (S (S'First + 1 .. S'Last));
      else
         Put (S);
      end if;
   end Put_Trim;

   procedure Put_Uns32 (V : Uns32) is
   begin
      Put_Trim (Uns32'Image (V));
   end Put_Uns32;

   procedure Put_Int32 (V : Int32) is
   begin
      Put_Trim (Int32'Image (V));
   end Put_Int32;

   procedure Put_Int64 (V : Int64) is
   begin
      Put_Trim (Int64'Image (V));
   end Put_Int64;

   procedure Put_Fp64 (V : Fp64) is
   begin
      Put_Trim (Fp64'Image (V));
   end Put_Fp64;

   Hex_Map : constant array (0 .. 15) of Character := "0123456789ABCDEF";

   procedure Put_Addr (V : System.Address)
   is
      type Integer_Address is mod System.Memory_Size;
      function To_Integer is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => Integer_Address);
      Res : String (1 .. System.Word_Size / 4);
      Val : Integer_Address := To_Integer (V);
   begin
      for I in reverse Res'Range loop
         Res (I) := Hex_Map (Natural (Val and 15));
         Val := Val / 16;
      end loop;
      Put (Res);
   end Put_Addr;

end Utils_IO;
