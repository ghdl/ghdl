--  GHDL Run Time (GRT) -  Run Time Informations.
--  Copyright (C) 2013 - 2014 Tristan Gingold
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

package body Grt.Rtis is
   procedure Ghdl_Rti_Add_Package (Pkg : Ghdl_Rti_Access) is
   begin
      Ghdl_Rti_Top.Children (Ghdl_Rti_Top.Nbr_Child) := Pkg;
      Ghdl_Rti_Top.Nbr_Child := Ghdl_Rti_Top.Nbr_Child + 1;
   end Ghdl_Rti_Add_Package;

   procedure Ghdl_Rti_Add_Top (Max_Pkg : Ghdl_Index_Type;
                               Pkgs : Ghdl_Rti_Arr_Acc;
                               Top : Ghdl_Rti_Access;
                               Instance : Address)
   is
      pragma Unreferenced (Max_Pkg);
   begin
      Ghdl_Rti_Top.Parent := Top;
      Ghdl_Rti_Top.Children := Pkgs;
      Ghdl_Rti_Top_Instance := Instance;
   end Ghdl_Rti_Add_Top;

end Grt.Rtis;
