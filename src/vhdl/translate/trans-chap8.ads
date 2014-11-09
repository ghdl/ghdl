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

package Trans.Chap8 is
   procedure Translate_Statements_Chain (First : Iir);

   --  Return true if there is a return statement in the chain.
   function Translate_Statements_Chain_Has_Return (First : Iir)
                                                      return Boolean;

   --  Create a case branch for CHOICE.
   --  Used by case statement and aggregates.
   procedure Translate_Case_Choice
     (Choice : Iir; Choice_Type : Iir; Blk : in out O_Case_Block);

   --  Inc or dec by VAL ITERATOR according to DIR.
   --  Used for loop statements.
   procedure Gen_Update_Iterator (Iterator : O_Dnode;
                                  Dir      : Iir_Direction;
                                  Val      : Unsigned_64;
                                  Itype    : Iir);

   procedure Translate_Report (Stmt : Iir; Subprg : O_Dnode; Level : Iir);
end Trans.Chap8;

