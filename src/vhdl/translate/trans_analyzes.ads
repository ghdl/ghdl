--  Analysis for translation.
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
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

with Iirs; use Iirs;

package Trans_Analyzes is
   --  Extract a list of drivers from PROC.
   function Extract_Drivers (Proc : Iir) return Iir_List;

   --  Free the list.
   procedure Free_Drivers_List (List : in out Iir_List);

   --  Dump list of drivers (LIST) for process PROC.
   procedure Dump_Drivers (Proc : Iir; List : Iir_List);

end Trans_Analyzes;
