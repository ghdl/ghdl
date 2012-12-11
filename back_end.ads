--  Back-end specialization
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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

package Back_End is
   --  Return the name of the library file for LIBRARY.
   --  The library file describe the contents of LIBRARY.
   function Default_Library_To_File_Name (Library : Iir_Library_Declaration)
                                         return String;

   type Library_To_File_Name_Acc is
     access function (Library : Iir_Library_Declaration) return String;

   Library_To_File_Name : Library_To_File_Name_Acc :=
     Default_Library_To_File_Name'Access;

   --  Back-end options.
   type Parse_Option_Acc is access function (Opt : String) return Boolean;
   Parse_Option : Parse_Option_Acc := null;

   --  Disp back-end option help.
   type Disp_Option_Acc is access procedure;
   Disp_Option : Disp_Option_Acc := null;

   --  UNIT is a design unit from parse.
   --  According to the current back-end, do what is necessary.
   --
   --  If MAIN is true, then UNIT is a wanted to be analysed design unit, and
   --  dump/list options can applied.
   --  This avoid to dump/list units fetched (through a selected name or a
   --  use clause) indirectly by the main unit.
   type Finish_Compilation_Acc is access
     procedure (Unit : Iir_Design_Unit; Main : Boolean := False);

   Finish_Compilation : Finish_Compilation_Acc := null;

   --  DECL is an architecture (library unit) or a subprogram (specification)
   --  decorated with a FOREIGN attribute.  Do back-end checks.
   --  May be NULL for no additionnal checks.
   type Sem_Foreign_Acc is access procedure (Decl : Iir);
   Sem_Foreign : Sem_Foreign_Acc := null;
end Back_End;
