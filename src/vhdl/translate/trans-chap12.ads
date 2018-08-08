--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap12 is
   --  Generate ortho declarations for elaboration.
   procedure Gen_Elab_Decls;

   --  Generate ortho code to elaborate declaration of the top unit.
   procedure Call_Elab_Decls (Arch : Iir; Arch_Instance : O_Enode);

   --  Write to file FILELIST all the files that are needed to link the design.
   procedure Write_File_List (Filelist : String);

   --  Generate elaboration code for CONFIG.
   procedure Elaborate (Config : Iir_Design_Unit;
                        Filelist : String;
                        Whole : Boolean);
end Trans.Chap12;
