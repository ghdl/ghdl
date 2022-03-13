--  Semantic analysis.
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

package Vhdl.Sem_Types is
   --  Analyze of types (LRM93 3 / LRM08 5)

   --  Analyze subtype indication DEF.
   --  If INCOMPLETE is TRUE, then DEF may designate an incomplete type
   --  definition.  Return either a name (denoting a type), an anonymous
   --  subtype definition or a name whose type is an error node.
   function Sem_Subtype_Indication (Def: Iir; Incomplete : Boolean := False)
     return Iir;

   procedure Sem_Protected_Type_Body (Bod : Iir);

   function Sem_Type_Definition (Def: Iir; Decl: Iir) return Iir;

   --  Check restrictions on access type DEF.
   procedure Check_Access_Type_Restrictions (Def : Iir; D_Type : Iir);

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

   --  If ATYPE is not fully constrained, build a fully constrained subtype.
   --  This is for 'subtype attribute.
   function Build_Constrained_Subtype (Atype : Iir; Loc : Iir) return Iir;

   --  Adjust the constraint state CONSTRAINT given new element EL_TYPE.
   --  Initially CONSTRAINT must be Fully_Constrained and COMPOSITE_FOUND
   --  must be false.
   procedure Update_Record_Constraint (Constraint : in out Iir_Constraint;
                                       Composite_Found : in out Boolean;
                                       El_Type : Iir);

   --  Although a nature is not a type, it is patterned like a type.
   function Sem_Subnature_Indication (Def: Iir) return Iir;

   function Sem_Nature_Definition (Def : Iir; Decl : Iir) return Iir;

   --  AMS-LRM17 6.4.2.7 Quantity declarations
   --  A nature type is a floating-point type of a composite type whose
   --  elements are of a nature type.
   --
   --  Return true iff DTYPE is a nature type.
   function Is_Nature_Type (Dtype : Iir) return Boolean;

   --  Return the simple nature of NAT.
   --
   --  AMS-LRM17 5.8.2 Scalar natures
   --  The simple nature of a scalar nature is the nature itself.
   --
   --  AMS-LRM17 5.8.3 Composite natures
   --  The scalar subelements of a composite nature shall have the same simple
   --  nature, which is also the simple nature of the composite nature.
   function Get_Nature_Simple_Nature (Nat : Iir) return Iir;

   --  Return TRUE iff nature NAT is a composite nature.
   function Is_Composite_Nature (Nat : Iir) return Boolean;
end Vhdl.Sem_Types;
