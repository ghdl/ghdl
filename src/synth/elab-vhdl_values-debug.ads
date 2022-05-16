--  Values in synthesis.
--  Copyright (C) 2017 Tristan Gingold
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

package Elab.Vhdl_Values.Debug is
   procedure Debug_Valtyp (V : Valtyp);
   procedure Debug_Memtyp (M : Memtyp);
   procedure Debug_Typ (T : Type_Acc);

   --  Short description, no newline.
   procedure Debug_Type_Short (T : Type_Acc);

   --  Display 'to' or 'downto'.
   procedure Put_Dir (Dir : Direction_Type);
end Elab.Vhdl_Values.Debug;
