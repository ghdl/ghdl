--  Disp a netlist in vhdl.
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

package Netlists.Disp_Vhdl is
   procedure Disp_Vhdl (M : Module);

   procedure Disp_Architecture_Declarations (M : Module);
   procedure Disp_Architecture_Statements (M : Module);

   procedure Put_Type (W : Width);

   --  Disp name N (as a normal identifier or as an extended identifier).
   procedure Put_Name (N : Sname);

   --  Return True IFF N requires an extended identifier for vhdl.
   function Is_Extended_Sname (N : Sname) return Boolean;

   --  Disp name N without extended character.
   --  Useful when a suffix or a prefix is needed.
   --  Use Is_Extended_Sname if N must be displayed as an extended identifier.
   procedure Put_Name_Inner (N : Sname);

   procedure Disp_Vhdl (M : Module; Is_Top : Boolean);
end Netlists.Disp_Vhdl;
