--  Canonicalization pass
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

package Canon is
   --  If true, a label will be added for statements which do not have a
   --  label.
   Canon_Flag_Add_Labels : Boolean := False;

   --  If true, canon sequentials statements (processes and subprograms).
   Canon_Flag_Sequentials_Stmts : Boolean := False;

   --  If true, canon concurrent statements.
   Canon_Flag_Concurrent_Stmts : Boolean := True;

   --  If true, canon configuration.
   Canon_Flag_Configurations : Boolean := True;

   --  If true, canon lists in specifications.
   Canon_Flag_Specification_Lists : Boolean := True;

   --  If true, canon expressions.
   Canon_Flag_Expressions : Boolean := False;

   --  If true, replace 'all' sensitivity list by the explicit list
   --  (If true, Canon_Flag_Sequentials_Stmts must be true)
   Canon_Flag_All_Sensitivity : Boolean := False;

   --  If true, operands of type array element of a concatenation operator
   --  are converted (by an aggregate) into array.
   Canon_Concatenation : Boolean := False;

   -- Do canonicalization:
   --  Transforms concurrent statements into sensitized process statements
   --   (all but component instanciation and block).
   --  This computes sensivity list.
   --
   --  Association list are completed:
   --  * Formal are added.
   --  * association are created for formal not associated (actual is open).
   --  * an association is created (for block header only).
   procedure Canonicalize (Unit: Iir_Design_Unit);

   --  Create a default configuration declaration for architecture ARCH.
   function Create_Default_Configuration_Declaration
     (Arch : Iir_Architecture_Body)
     return Iir_Design_Unit;

   --  Macro-expand package bodies for instantiations in DECL.  Return the
   --  chain of bodies (the parent of each body is set to PARENT).
   function Create_Instantiation_Bodies
     (Decl : Iir_Package_Declaration; Parent : Iir) return Iir;

   --  Canonicalize a subprogram call.
   procedure Canon_Subprogram_Call (Call : Iir);

   --  Canon on expressions, mainly for function calls.
   procedure Canon_Expression (Expr: Iir);

   --  Canon a conditional variable assignment into a conditional statement.
   function Canon_Conditional_Variable_Assignment_Statement (Stmt : Iir)
                                                            return Iir;

   --  Canon a conditional signal assignment into a conditional statement.
   function Canon_Conditional_Signal_Assignment_Statement (Stmt : Iir)
                                                          return Iir;

   -- Compute the sensivity list of EXPR and add it to SENSIVITY_LIST.
   -- If IS_TARGET is true, the longuest static prefix of the signal name
   -- is not added to the sensitivity list, but other static prefix (such
   -- as indexes of an indexed name) are added.
   procedure Canon_Extract_Sensitivity
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False);

   --  Compute the sensitivity list of all-sensitized process PROC.
   --  Used for vhdl 08.
   function Canon_Extract_Process_Sensitivity
     (Proc : Iir_Sensitized_Process_Statement)
     return Iir_List;
end Canon;
