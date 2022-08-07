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
with Vhdl.Sem_Expr; use Vhdl.Sem_Expr;

package Vhdl.Sem_Assocs is
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
   --  If FINISH is true, then ASSOC_CHAIN may be modified (individual assoc
   --  added), and error messages (if any) are displayed.
   --  MISSING control unassociated interfaces.
   -- LOC is the association.
   -- Sem_Actual_Of_Association_Chain must have been called before.
   type Missing_Type is (Missing_Parameter, Missing_Port, Missing_Generic,
                         Missing_Allowed);
   procedure Sem_Association_Chain (Interface_Chain : Iir;
                                    Assoc_Chain: in out Iir;
                                    Finish: Boolean;
                                    Missing : Missing_Type;
                                    Loc : Iir;
                                    Match : out Compatibility_Level);

   --  Check association for expression ACTUAL to interface FORMAL.
   --  ASSOC may be null for operator.
   procedure Check_Subprogram_Association_Expression
     (Formal : Iir; Actual : Iir; Assoc : Iir; Loc : Iir);

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

   --  LRM93 8.6 Procedure Call Statement
   --  For each formal parameter of a procedure, a procedure call must
   --  specify exactly one corresponding actual parameter.
   --  This actual parameter is specified either explicitly, by an
   --  association element (other than the actual OPEN) in the association
   --  list, or in the absence of such an association element, by a default
   --  expression (see Section 4.3.3.2).
   --
   --  LRM93 7.3.3 Function Calls
   --  For each formal parameter of a function, a function call must
   --  specify exactly one corresponding actual parameter.
   --  This actual parameter is specified either explicitly, by an
   --  association element (other than the actual OPEN) in the association
   --  list, or in the absence of such an association element, by a default
   --  expression (see Section 4.3.3.2).
   --
   --  LRM93 1.1.1.2 / LRM08 6.5.6.3 Port clauses
   --  A port of mode IN may be unconnected or unassociated only if its
   --  declaration includes a default expression.
   --  A port of any mode other than IN may be unconnected or unassociated
   --  as long as its type is not an unconstrained array type.
   --
   --  LRM08 6.5.6.2 Generic clauses
   --  It is an error if no such actual [instantiated package] is specified
   --  for a given formal generic package (either because the formal generic
   --  is unassociated or because the actual is OPEN).
   --
   --  INTER is an interface that is known not to be associated.
   --  Report an error according to MISSING iff FINISH is true.
   --  Return True iff not associating INTER is an error.
   function Sem_Check_Missing_Association (Inter : Iir;
                                           Missing : Missing_Type;
                                           Finish : Boolean;
                                           Is_Open : Boolean;
                                           Loc : Iir) return Boolean;
end Vhdl.Sem_Assocs;
