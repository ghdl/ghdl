--  Display the ortho codes from a tree.
--  Copyright (C) 2005 Tristan Gingold
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
with Interfaces.C_Streams;

package Ortho_Debug.Disp is
   --  Initialize the current context.
   --  Must be called before any use of the DISP_* subprograms.
   procedure Init_Context (File : Interfaces.C_Streams.FILEs);

   --  Disp nodes in a pseudo-language.
   procedure Disp_Ortho (Decls : O_Snode);

private
end Ortho_Debug.Disp;
