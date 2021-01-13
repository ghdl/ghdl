--  GHDL Run Time (GRT) -  Well known RTI types.
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
with Grt.Rtis; use Grt.Rtis;

--  This package allow access to RTIs of some types.
--  This is used to recognize some VHDL logic types.
--  This is also used by grt.signals to set types of some implicit signals
--   (such as 'stable or 'transation).

package Grt.Rtis_Types is
   --  RTIs for some logic types.
   Std_Standard_Bit_RTI_Ptr : Ghdl_Rti_Access;

   Std_Standard_Boolean_RTI_Ptr : Ghdl_Rti_Access;

   --  std_ulogic.
   --  A VHDL may not contain ieee.std_logic_1164 package.  So, this RTI
   --  must be dynamicaly searched.
   Ieee_Std_Logic_1164_Std_Ulogic_RTI_Ptr : Ghdl_Rti_Access := null;

   --  Search RTI for types.
   --  If a type is not found, its RTI is set to null.
   --  If this procedure has already been called, then this is a noop.
   procedure Search_Types_RTI;
private
   --  These are set either by grt.rtis_binding or by ghdlrun.
   --  This is not very clean...
   pragma Import (C, Std_Standard_Bit_RTI_Ptr,
                  "std__standard__bit__RTI_ptr");

   pragma Import (C, Std_Standard_Boolean_RTI_Ptr,
                  "std__standard__boolean__RTI_ptr");
end Grt.Rtis_Types;
