--  GHDL Run Time (GRT) - Utility functions for AVHPI.
--  Copyright (C) 2015 Tristan Gingold
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

with Grt.Errors; use Grt.Errors;

package body Grt.Avhpi_Utils is
   function Get_Root_Entity (Root : VhpiHandleT) return VhpiHandleT
   is
      Hdl   : VhpiHandleT;
      Unit  : VhpiHandleT;
      Error : AvhpiErrorT;
   begin
      Vhpi_Handle (VhpiDesignUnit, Root, Unit, Error);
      if Error /= AvhpiErrorOk then
         Internal_Error ("VhpiDesignUnit");
      end if;

      case Vhpi_Get_Kind (Unit) is
         when VhpiArchBodyK =>
            Vhpi_Handle (VhpiPrimaryUnit, Unit, Hdl, Error);
            if Error /= AvhpiErrorOk then
               Internal_Error ("VhpiPrimaryUnit");
            end if;
         when others =>
            Internal_Error ("get_root_entity");
      end case;
      return Hdl;
   end Get_Root_Entity;

   function Name_Compare (Handle : VhpiHandleT;
                          Name : String;
                          Property : VhpiStrPropertyT := VhpiNameP)
                         return Boolean
   is
      Obj_Name : String (1 .. Name'Length);
      Len : Natural;
   begin
      Vhpi_Get_Str (Property, Handle, Obj_Name, Len);
      return Len = Name'Length and then Obj_Name = Name;
   end Name_Compare;

end Grt.Avhpi_Utils;
