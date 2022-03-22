--  GHDL Run Time (GRT) -  'name* subprograms.
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Rtis; use Grt.Rtis;

package Grt.Names is
   procedure Ghdl_Get_Path_Name (Res : Std_String_Ptr;
                                 Ctxt : Ghdl_Rti_Access;
                                 Base : Address;
                                 Name : Ghdl_Str_Len_Ptr);

   procedure Ghdl_Get_Instance_Name (Res : Std_String_Ptr;
                                     Ctxt : Ghdl_Rti_Access;
                                     Base : Address;
                                     Name : Ghdl_Str_Len_Ptr);
private
   pragma Export (C, Ghdl_Get_Path_Name, "__ghdl_get_path_name");
   pragma Export (C, Ghdl_Get_Instance_Name, "__ghdl_get_instance_name");
end Grt.Names;
