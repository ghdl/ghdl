--  GHDL Run Time (GRT) - wave dumper (GHW) declarations.
--  Copyright (C) 2016 Tristan Gingold
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

with Interfaces; use Interfaces;

package Grt.Ghw is
   --  Hierarhcy elements.
   Ghw_Hie_Design       : constant Unsigned_8 := 1;
   Ghw_Hie_Block        : constant Unsigned_8 := 3;
   Ghw_Hie_Generate_If  : constant Unsigned_8 := 4;
   Ghw_Hie_Generate_For : constant Unsigned_8 := 5;
   Ghw_Hie_Instance     : constant Unsigned_8 := 6;
   Ghw_Hie_Package      : constant Unsigned_8 := 7;
   Ghw_Hie_Process      : constant Unsigned_8 := 13;
   Ghw_Hie_Generic      : constant Unsigned_8 := 14;
   Ghw_Hie_Eos          : constant Unsigned_8 := 15; --  End of scope.
   Ghw_Hie_Signal       : constant Unsigned_8 := 16; --  Signal.
   Ghw_Hie_Port_In      : constant Unsigned_8 := 17; --  Port
   Ghw_Hie_Port_Out     : constant Unsigned_8 := 18; --  Port
   Ghw_Hie_Port_Inout   : constant Unsigned_8 := 19; --  Port
   Ghw_Hie_Port_Buffer  : constant Unsigned_8 := 20; --  Port
   Ghw_Hie_Port_Linkage : constant Unsigned_8 := 21; --  Port

   --  Type kind was initially ghdl_rtik, but to avoid coupling, we are now
   --  using Ghw_Rtik (with old values).
   type Ghw_Rtik is new Unsigned_8;
   Ghw_Rtik_Error : constant Ghw_Rtik := 0;
   Ghw_Rtik_Eos      : constant Ghw_Rtik := 15; --  End of scope.

   Ghw_Rtik_Signal       : constant Ghw_Rtik := 16; --  Signal.
   Ghw_Rtik_Port_In      : constant Ghw_Rtik := 17; --  Port
   Ghw_Rtik_Port_Out     : constant Ghw_Rtik := 18; --  Port
   Ghw_Rtik_Port_Inout   : constant Ghw_Rtik := 19; --  Port
   Ghw_Rtik_Port_Buffer  : constant Ghw_Rtik := 20; --  Port
   Ghw_Rtik_Port_Linkage : constant Ghw_Rtik := 21; --  Port

   Ghw_Rtik_Type_B2  : constant Ghw_Rtik := 22;
   Ghw_Rtik_Type_E8  : constant Ghw_Rtik := 23;
   Ghw_Rtik_Type_E32 : constant Ghw_Rtik := 24;  --  Not used in waves
   Ghw_Rtik_Type_I32 : constant Ghw_Rtik := 25;
   Ghw_Rtik_Type_I64 : constant Ghw_Rtik := 26;
   Ghw_Rtik_Type_F64 : constant Ghw_Rtik := 27;
   Ghw_Rtik_Type_P32 : constant Ghw_Rtik := 28;
   Ghw_Rtik_Type_P64 : constant Ghw_Rtik := 29;
   Ghw_Rtik_Type_Array     : constant Ghw_Rtik := 31;
   Ghw_Rtik_Type_Record    : constant Ghw_Rtik := 32;
   Ghw_Rtik_Subtype_Scalar : constant Ghw_Rtik := 34;
   Ghw_Rtik_Subtype_Array  : constant Ghw_Rtik := 35;
   Ghw_Rtik_Subtype_Unbounded_Array  : constant Ghw_Rtik := 37;
   Ghw_Rtik_Subtype_Record : constant Ghw_Rtik := 38;
   Ghw_Rtik_Subtype_Unbounded_Record : constant Ghw_Rtik := 39;

   --  Not used in waves
   Ghw_Rtik_Subtype_B1  : constant Ghw_Rtik := 41;
   Ghw_Rtik_Subtype_E8  : constant Ghw_Rtik := 42;
   Ghw_Rtik_Subtype_E32 : constant Ghw_Rtik := 43;
   Ghw_Rtik_Subtype_I32 : constant Ghw_Rtik := 44;
   Ghw_Rtik_Subtype_I64 : constant Ghw_Rtik := 45;
   Ghw_Rtik_Subtype_F64 : constant Ghw_Rtik := 46;
   Ghw_Rtik_Subtype_P32 : constant Ghw_Rtik := 47;
   Ghw_Rtik_Subtype_P64 : constant Ghw_Rtik := 48;

end Grt.Ghw;
