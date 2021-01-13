--  GHDL Run Time (GRT) -  Run Time Informations.
--  Copyright (C) 2013 - 2014 Tristan Gingold
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

package body Grt.Rtis is
   Max_Top_Pkg : Ghdl_Index_Type := 0;

   procedure Ghdl_Rti_Add_Package (Pkg : Ghdl_Rti_Access) is
   begin
      pragma Assert (Ghdl_Rti_Top.Nbr_Child < Max_Top_Pkg);
      Ghdl_Rti_Top.Children (Ghdl_Rti_Top.Nbr_Child) := Pkg;
      Ghdl_Rti_Top.Nbr_Child := Ghdl_Rti_Top.Nbr_Child + 1;
   end Ghdl_Rti_Add_Package;

   procedure Ghdl_Rti_Add_Top (Max_Pkg : Ghdl_Index_Type;
                               Pkgs : Ghdl_Rti_Arr_Acc;
                               Top : Ghdl_Rti_Access;
                               Instance : Address) is
   begin
      Max_Top_Pkg := Max_Pkg;

      Ghdl_Rti_Top.Parent := Top;
      Ghdl_Rti_Top.Children := Pkgs;
      Ghdl_Rti_Top_Instance := Instance;
   end Ghdl_Rti_Add_Top;

end Grt.Rtis;
