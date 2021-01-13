--  GHDL driver - shared variables with grt.
--  Copyright (C) 2011 Tristan Gingold
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
with System; use System;

package Grtlink is

   Flag_String : String (1 .. 5);
   pragma Export (C, Flag_String, "__ghdl_flag_string");

   Std_Standard_Bit_RTI_Ptr : Address := Null_Address;

   Std_Standard_Boolean_RTI_Ptr : Address := Null_Address;

   pragma Export (C, Std_Standard_Bit_RTI_Ptr,
                  "std__standard__bit__RTI_ptr");

   pragma Export (C, Std_Standard_Boolean_RTI_Ptr,
                  "std__standard__boolean__RTI_ptr");

   Ieee_Std_Logic_1164_Resolved_Resolv_Ptr : Address := Null_Address;
   pragma Export (C, Ieee_Std_Logic_1164_Resolved_Resolv_Ptr,
                  "ieee__std_logic_1164__resolved_RESOLV_ptr");

end Grtlink;
