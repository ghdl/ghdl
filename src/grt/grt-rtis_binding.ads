--  GHDL Run Time (GRT) -  Well known RTIs.
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
with System; use System;
with Grt.Rtis; use Grt.Rtis;

--  Set RTI_ptr defined in grt.rtis_types.

package Grt.Rtis_Binding is
   pragma Preelaborate (Grt.Rtis_Binding);

   --  Define and set bit and boolean RTIs.
   Std_Standard_Bit_RTI : aliased Ghdl_Rti_Common;

   Std_Standard_Boolean_RTI : aliased Ghdl_Rti_Common;

   pragma Import (C, Std_Standard_Bit_RTI,
                  "std__standard__bit__RTI");

   pragma Import (C, Std_Standard_Boolean_RTI,
                  "std__standard__boolean__RTI");

   Std_Standard_Bit_RTI_Ptr : Ghdl_Rti_Access
     := Std_Standard_Bit_RTI'Access;

   Std_Standard_Boolean_RTI_Ptr : Ghdl_Rti_Access
     := Std_Standard_Boolean_RTI'Access;

   pragma Export (C, Std_Standard_Bit_RTI_Ptr,
                  "std__standard__bit__RTI_ptr");

   pragma Export (C, Std_Standard_Boolean_RTI_Ptr,
                  "std__standard__boolean__RTI_ptr");


   --  Define and set Resolved_Resolv_Ptr.
   procedure Ieee_Std_Logic_1164_Resolved_RESOLV;
   pragma Import (C, Ieee_Std_Logic_1164_Resolved_RESOLV,
                  "ieee__std_logic_1164__resolved_RESOLV");

   Ieee_Std_Logic_1164_Resolved_Resolv_Ptr : Address :=
     Ieee_Std_Logic_1164_Resolved_RESOLV'Address;
   pragma Export (C, Ieee_Std_Logic_1164_Resolved_Resolv_Ptr,
                  "ieee__std_logic_1164__resolved_RESOLV_ptr");

end Grt.Rtis_Binding;
