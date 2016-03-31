--  Semantic analysis pass for PSL.
--  Copyright (C) 2009 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Iirs; use Iirs;

package Sem_Psl is
   procedure Sem_Psl_Declaration (Stmt : Iir);
   procedure Sem_Psl_Endpoint_Declaration (Stmt : Iir);

   --  May return a non-psl concurrent assertion statement.
   function Sem_Psl_Assert_Statement (Stmt : Iir) return Iir;

   procedure Sem_Psl_Cover_Statement (Stmt : Iir);
   procedure Sem_Psl_Default_Clock (Stmt : Iir);
   function Sem_Psl_Name (Name : Iir) return Iir;
end Sem_Psl;
