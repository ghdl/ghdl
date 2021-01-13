--  GHDL Run Time (GRT) - RTI dumper.
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
with Grt.Types; use Grt.Types;
with Grt.Stdio; use Grt.Stdio;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;

package Grt.Disp_Rti is
   --  Disp NAME.  If NAME is null, then disp <anonymous>.
   procedure Disp_Name (Name : Ghdl_C_String);

   --  Disp a value.
   procedure Disp_Value (Stream : FILEs;
                         Rti : Ghdl_Rti_Access;
                         Ctxt : Rti_Context;
                         Obj : in out Address;
                         Bounds : in out Address;
                         Is_Sig : Boolean);

   procedure Register;
end Grt.Disp_Rti;
