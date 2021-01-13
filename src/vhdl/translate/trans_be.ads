--  Back-end for translation.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;
with Translation;

package Trans_Be is
   type Sem_Foreign_Hook_Type is access
     procedure (Decl : Iir; Info : Translation.Foreign_Info_Type);

   --  Hook called by Sem_Foreign.
   Sem_Foreign_Hook : Sem_Foreign_Hook_Type := null;

   procedure Register_Translation_Back_End;
end Trans_Be;
