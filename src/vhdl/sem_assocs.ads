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
with Sem_Expr; use Sem_Expr;

package Sem_Assocs is
   --  Rewrite the association chain by changing the kind of assocation
   --  corresponding to non-object interfaces.  Such an association mustn't be
   --  handled an like association for object as the actual is not an
   --  expression.
   function Extract_Non_Object_Association
     (Assoc_Chain : Iir; Inter_Chain : Iir) return Iir;

   --  Analyze actuals of ASSOC_CHAIN.
   --  Check all named associations are after positionnal one.
   --  Return TRUE if no error.
   function Sem_Actual_Of_Association_Chain (Assoc_Chain : Iir) return Boolean;

   --  Analyze association chain ASSOC_CHAIN with interfaces from
   --  INTERFACE_CHAIN.
   --  Return the level of compatibility between the two chains in LEVEL.
   --  If FINISH is true, then ASSOC_CHAIN may be modifies (individual assoc
   --  added), and error messages (if any) are displayed.
   --  MISSING control unassociated interfaces.
   -- LOC is the association.
   -- Sem_Actual_Of_Association_Chain must have been called before.
   type Missing_Type is (Missing_Parameter, Missing_Port, Missing_Generic,
                         Missing_Allowed);
   procedure Sem_Association_Chain
     (Interface_Chain : Iir;
      Assoc_Chain: in out Iir;
      Finish: Boolean;
      Missing : Missing_Type;
      Loc : Iir;
      Match : out Compatibility_Level);

   --  Do port Sem_Association_Chain checks for subprograms.
   procedure Check_Subprogram_Associations
     (Inter_Chain : Iir; Assoc_Chain : Iir);

   --  Check for restrictions in LRM93 1.1.1.2
   --  Return FALSE in case of error.
   function Check_Port_Association_Mode_Restrictions
     (Formal : Iir_Interface_Signal_Declaration;
      Actual : Iir_Interface_Signal_Declaration;
      Assoc : Iir)
     return Boolean;

   --  Check restrictions of LRM02 12.2.4
   procedure Check_Port_Association_Bounds_Restrictions
     (Formal : Iir; Actual : Iir; Assoc : Iir);

end Sem_Assocs;
