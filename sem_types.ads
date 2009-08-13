--  Semantic analysis.
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

package Sem_Types is
   --  Semantization of types (LRM chapter 3)

   -- Semantize subtype indication DEF.
   -- If INCOMPLETE is TRUE, then DEF may designate an incomplete type
   -- definition.
   -- This is used by sem_expr for qualified expression and allocators.
   function Sem_Subtype_Indication (Def: Iir; Incomplete : Boolean := False)
     return Iir;

   -- Return FALSE if A_TYPE is an unconstrained array type or subtype.
   --function Sem_Is_Constrained (A_Type: Iir) return Boolean;

   procedure Sem_Protected_Type_Body (Bod : Iir);

   function Sem_Type_Definition (Def: Iir; Decl: Iir) return Iir;

   --  Convert a range expression to a subtype definition whose constraint is
   --  A_RANGE.
   --  This function extract the type of the range expression.
   function Range_To_Subtype_Definition (A_Range: Iir) return Iir;

   --  ATYPE is used to declare a signal.
   --  Set (recursively) the Has_Signal_Flag on ATYPE and all types used by
   --   ATYPE (basetype, elements...)
   --  If ATYPE can have signal (eg: access or file type), then this procedure
   --   returns silently.
   procedure Set_Type_Has_Signal (Atype : Iir);

   --  Return TRUE iff FUNC is a resolution function.
   --  If ATYPE is not NULL_IIR, type must match.
   function Is_A_Resolution_Function (Func: Iir; Atype: Iir) return Boolean;

   --  Return a subtype definition copy of DEF.
   --  This is used when an alias of DEF is required (eg: subtype a is b).
   function Copy_Subtype_Indication (Def : Iir) return Iir;

end Sem_Types;
