--  GHDL driver - Foreign functions known by JIT.
--  Copyright (C) 2002 - 2015 Tristan Gingold
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
with Interfaces.C; use Interfaces.C;

package body Foreigns is
   function Ceil (Arg : double) return double;
   pragma Import (C, Ceil);

   function Floor (Arg : double) return double;
   pragma Import (C, Floor);

   function Round (Arg : double) return double;
   pragma Import (C, Round);

   function Trunc (Arg : double) return double;
   pragma Import (C, Trunc);

   function Sin (Arg : double) return double;
   pragma Import (C, Sin);

   function Cos (Arg : double) return double;
   pragma Import (C, Cos);

   function Log (Arg : double) return double;
   pragma Import (C, Log);

   function Exp (Arg : double) return double;
   pragma Import (C, Exp);

   function Sqrt (Arg : double) return double;
   pragma Import (C, Sqrt);

   function Asin (Arg : double) return double;
   pragma Import (C, Asin);

   function Acos (Arg : double) return double;
   pragma Import (C, Acos);

   function Asinh (Arg : double) return double;
   pragma Import (C, Asinh);

   function Acosh (Arg : double) return double;
   pragma Import (C, Acosh);

   function Atanh (X : double) return double;
   pragma Import (C, Atanh);

   function Atan2 (X, Y : double) return double;
   pragma Import (C, Atan2);

   type String_Cacc is access constant String;
   type Foreign_Record is record
      Name : String_Cacc;
      Addr : Address;
   end record;

   Foreign_Arr : constant array (Natural range <>) of Foreign_Record :=
     (
      (new String'("ceil"), Ceil'Address),
      (new String'("floor"), Floor'Address),
      (new String'("round"), Round'Address),
      (new String'("trunc"), Trunc'Address),
      (new String'("sin"), Sin'Address),
      (new String'("cos"), Cos'Address),
      (new String'("log"), Log'Address),
      (new String'("exp"), Exp'Address),
      (new String'("sqrt"), Sqrt'Address),
      (new String'("asin"), Asin'Address),
      (new String'("acos"), Acos'Address),
      (new String'("asinh"), Asinh'Address),
      (new String'("acosh"), Acosh'Address),
      (new String'("atanh"), Atanh'Address),
      (new String'("atan2"), Atan2'Address)
     );

   function Find_Foreign (Name : String) return Address is
   begin
      for I in Foreign_Arr'Range loop
         if Foreign_Arr(I).Name.all = Name then
            return Foreign_Arr(I).Addr;
         end if;
      end loop;
      return Null_Address;
   end Find_Foreign;
end Foreigns;
