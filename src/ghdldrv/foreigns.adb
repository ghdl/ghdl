--  GHDL driver - Foreign functions known by JIT.
--  Copyright (C) 2002 - 2015 Tristan Gingold
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

   function Fmod (X, Y : double) return double;
   pragma Import (C, Fmod);

   function Fmin (X, Y : double) return double;
   pragma Import (C, Fmin);

   function Fmax (X, Y : double) return double;
   pragma Import (C, Fmax);

   function Sin (Arg : double) return double;
   pragma Import (C, Sin);

   function Cos (Arg : double) return double;
   pragma Import (C, Cos);

   function Tan (Arg : double) return double;
   pragma Import (C, Tan);

   function Atan (Y : double) return double;
   pragma Import (C, Atan);

   function Atan2 (X, Y : double) return double;
   pragma Import (C, Atan2);

   function Log (Arg : double) return double;
   pragma Import (C, Log);

   function Log2 (Arg : double) return double;
   pragma Import (C, Log2);

   function Log10 (Arg : double) return double;
   pragma Import (C, Log10);

   function Exp (Arg : double) return double;
   pragma Import (C, Exp);

   function Pow (X, Y : double) return double;
   pragma Import (C, Pow);

   function Sqrt (Arg : double) return double;
   pragma Import (C, Sqrt);

   function Cbrt (Arg : double) return double;
   pragma Import (C, Cbrt);

   function Asin (Arg : double) return double;
   pragma Import (C, Asin);

   function Acos (Arg : double) return double;
   pragma Import (C, Acos);

   function Sinh (Arg : double) return double;
   pragma Import (C, Sinh);

   function Cosh (Arg : double) return double;
   pragma Import (C, Cosh);

   function Tanh (Arg : double) return double;
   pragma Import (C, Tanh);

   function Asinh (Arg : double) return double;
   pragma Import (C, Asinh);

   function Acosh (Arg : double) return double;
   pragma Import (C, Acosh);

   function Atanh (X : double) return double;
   pragma Import (C, Atanh);

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
      (new String'("fmod"), Fmod'Address),
      (new String'("fmin"), Fmin'Address),
      (new String'("fmax"), Fmax'Address),
      (new String'("log"), Log'Address),
      (new String'("log2"), Log2'Address),
      (new String'("log10"), Log10'Address),
      (new String'("exp"), Exp'Address),
      (new String'("sqrt"), Sqrt'Address),
      (new String'("cbrt"), Cbrt'Address),
      (new String'("pow"), Pow'Address),
      (new String'("sin"), Sin'Address),
      (new String'("cos"), Cos'Address),
      (new String'("tan"), Tan'Address),
      (new String'("asin"), Asin'Address),
      (new String'("acos"), Acos'Address),
      (new String'("atan"), Atan'Address),
      (new String'("atan2"), Atan2'Address),
      (new String'("sinh"), Sinh'Address),
      (new String'("cosh"), Cosh'Address),
      (new String'("tanh"), Tanh'Address),
      (new String'("asinh"), Asinh'Address),
      (new String'("acosh"), Acosh'Address),
      (new String'("atanh"), Atanh'Address)
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
