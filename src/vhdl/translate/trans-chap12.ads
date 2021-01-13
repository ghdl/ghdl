--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap12 is
   --  Generate ortho declarations for elaboration.
   procedure Gen_Elab_Decls;

   --  Generate ortho code to elaborate declaration of the top unit.
   procedure Call_Elab_Decls (Arch : Iir; Arch_Instance : O_Enode);

   --  Generate elaboration code for CONFIG.
   procedure Elaborate (Config : Iir_Design_Unit; Whole : Boolean);
end Trans.Chap12;
