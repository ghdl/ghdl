--  Locations for instances.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
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

package Netlists.Locations is
   --  If True, locations are enabled.
   Flag_Locations : Boolean := True;

   --  Save location LOC for INST.  Noop if locations are not enabled.
   procedure Set_Location (Inst : Instance; Loc : Location_Type);

   --  Get the previously saved location for INST.
   --  Return Null_Location if no location set or locations are disabled.
   function Get_Location (Inst : Instance) return Location_Type;

   --  Utilities.
   procedure Set_Location (Dest : Net; Loc : Location_Type);
   procedure Copy_Location (Dest : Net; Src : Net);
   procedure Copy_Location (Dest : Net; Src : Instance);
   pragma Inline (Copy_Location);
end Netlists.Locations;
