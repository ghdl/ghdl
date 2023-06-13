--  Verilog strings
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with System;
with Ada.Unchecked_Conversion;
with Types; use Types;
with Grt.Types; use Grt.Types;

package Verilog.Sv_Strings is
   type Sv_String is private;

   type Sv_String_Arr is array (Natural range <>) of Sv_String;

   function Is_Eq (L, R : Sv_String) return Boolean;

   --  Return the empty sv_string (with ref incremenented).
   function Empty_Sv_String return Sv_String;

   function New_Sv_String (Len : Natural) return Sv_String;
   function New_Sv_String (Id : Name_Id) return Sv_String;
   function New_Sv_String (Id : String8_Id; Len : Natural) return Sv_String;
   function New_Sv_String (S : Ghdl_C_String) return Sv_String;

   procedure Ref (S : Sv_String);
   procedure Unref (S : Sv_String);

   --  Ensure there is only one reference to S; so copy the string if it is
   --  shared.
   procedure Make_Unique (S : in out Sv_String);

   procedure Set_String_El (S : in out Sv_String; I : Positive; C : Character);
   function Get_String_El (S : Sv_String; I : Positive) return Character;

   type Sv_String_Ptr is access all Sv_String;

   function To_Sv_String_Ptr is
      new Ada.Unchecked_Conversion (System.Address, Sv_String_Ptr);

   function Get_Length (S : Sv_String) return Natural;
   function Get_String (S : Sv_String) return String;
   function Compare (L, R : Sv_String) return Order_Type;
private
   type Sv_String_Type (Len : Natural) is record
      Ref : Natural;
      S : String (1 .. Len);
   end record;

   type Sv_String is access all Sv_String_Type;

end Verilog.Sv_Strings;
