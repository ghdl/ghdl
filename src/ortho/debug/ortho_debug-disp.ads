--  Display the ortho codes from a tree.
--  Copyright (C) 2005 Tristan Gingold
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
with Interfaces.C_Streams;

package Ortho_Debug.Disp is
   --  Initialize the current context.
   --  Must be called before any use of the DISP_* subprograms.
   procedure Init_Context (File : Interfaces.C_Streams.FILEs);

   --  Disp nodes in a pseudo-language.
   procedure Disp_Ortho (Decls : O_Snode);

private
end Ortho_Debug.Disp;
