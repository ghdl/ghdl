--  GHDL Run Time (GRT) - wave dumper (GHW) declarations.
--  Copyright (C) 2016 Tristan Gingold
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
   Ghw_Rtik_Type_B2  : constant Ghw_Rtik := 22;
   Ghw_Rtik_Type_E8  : constant Ghw_Rtik := 23;
   --  Ghw_Rtik_Type_E32 : constant Ghw_Rtik := 24;  --  Not used
   Ghw_Rtik_Type_I32 : constant Ghw_Rtik := 25;
   Ghw_Rtik_Type_I64 : constant Ghw_Rtik := 26;
   Ghw_Rtik_Type_F64 : constant Ghw_Rtik := 27;
   Ghw_Rtik_Type_P32 : constant Ghw_Rtik := 28;
   Ghw_Rtik_Type_P64 : constant Ghw_Rtik := 29;
   Ghw_Rtik_Type_Array     : constant Ghw_Rtik := 31;
   Ghw_Rtik_Type_Record    : constant Ghw_Rtik := 32;
   Ghw_Rtik_Subtype_Scalar : constant Ghw_Rtik := 34;
   Ghw_Rtik_Subtype_Array  : constant Ghw_Rtik := 35;
   Ghw_Rtik_Subtype_Record : constant Ghw_Rtik := 38;
end Grt.Ghw;
