--  String table.
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with System;
with Ada.Unchecked_Conversion;
with GNAT.Table;

package body Str_Table is
   package String_Table is new GNAT.Table
     (Table_Index_Type => String_Id,
      Table_Component_Type => Character,
      Table_Low_Bound => Null_String + 1,
      Table_Initial => 4096,
      Table_Increment => 100);

   Nul : constant Character := Character'Val (0);

   In_String : Boolean := False;
   function Start return String_Id
   is
   begin
      pragma Assert (In_String = False);
      In_String := True;
      return String_Table.Last + 1;
   end Start;

   procedure Append (C : Character) is
   begin
      pragma Assert (In_String);
      String_Table.Append (C);
   end Append;

   procedure Finish is
   begin
      pragma Assert (In_String);
      String_Table.Append (Nul);
      In_String := False;
   end Finish;

   function Get_String_Fat_Acc (Id : String_Id) return String_Fat_Acc
   is
      function To_String_Fat_Acc is new Ada.Unchecked_Conversion
        (Source => System.Address, Target => String_Fat_Acc);
   begin
      return To_String_Fat_Acc (String_Table.Table (Id)'Address);
   end Get_String_Fat_Acc;

   function Get_Length (Id : String_Id) return Natural
   is
      Ptr : String_Fat_Acc;
      Len : Nat32;
   begin
      Ptr := Get_String_Fat_Acc (Id);
      Len := 1;
      loop
         if Ptr (Len) = Nul then
            return Natural (Len - 1);
         end if;
         Len := Len + 1;
      end loop;
   end Get_Length;

   function Image (Id : String_Id) return String
   is
      Ptr : String_Fat_Acc;
      Len : Nat32;
   begin
      Len := Nat32 (Get_Length (Id));
      Ptr := Get_String_Fat_Acc (Id);
      return String (Ptr (1 .. Len));
   end Image;

   procedure Initialize is
   begin
      String_Table.Free;
      String_Table.Init;
   end Initialize;
end Str_Table;
