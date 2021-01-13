--  Mcode back-end for ortho - Internal tree dumper.
--  Copyright (C) 2006 Tristan Gingold
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
with Ortho_Code.Exprs; use Ortho_Code.Exprs;

package Ortho_Code.Disps is
   procedure Disp_Subprg (Subprg : Subprogram_Data_Acc);
   procedure Disp_Type (Atype : O_Tnode; Force : Boolean := False);
   procedure Init;
   procedure Finish;
end Ortho_Code.Disps;
