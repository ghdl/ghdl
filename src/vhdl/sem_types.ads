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
   --  Analyze of types (LRM93 3 / LRM08 5)

   --  Analyze subtype indication DEF.
   --  If INCOMPLETE is TRUE, then DEF may designate an incomplete type
   --  definition.  Return either a name (denoting a type), an anonymous
   --  subtype definition or a name whose type is an error node.
   function Sem_Subtype_Indication (Def: Iir; Incomplete : Boolean := False)
     return Iir;

   procedure Sem_Protected_Type_Body (Bod : Iir);

   function Sem_Type_Definition (Def: Iir; Decl: Iir) return Iir;

   --  If A_RANGE is a range (range expression or range attribute), convert it
   --  to a subtype definition.  Otherwise return A_RANGE.
   --  The result is a subtype indication: either a type name or a subtype
   --  definition.
   function Range_To_Subtype_Indication (A_Range: Iir) return Iir;

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

   --  Return a copy of the resolution_indication in SUBDEF, or null_iir if
   --  none.
   function Copy_Resolution_Indication (Subdef : Iir) return Iir;

   --  Although a nature is not a type, it is patterned like a type.
   function Sem_Subnature_Indication (Def: Iir) return Iir;
end Sem_Types;
