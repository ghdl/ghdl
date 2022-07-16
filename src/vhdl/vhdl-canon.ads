--  Canonicalization pass
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

package Vhdl.Canon is
   --  If true, a label will be added to unlabelled concurrent statements.
   Canon_Flag_Add_Labels : Boolean := False;

   --  If true, canon sequentials statements (processes and subprograms).
   Canon_Flag_Sequentials_Stmts : Boolean := False;

   --  If true, canon concurrent statements: transform them into processes.
   Canon_Flag_Concurrent_Stmts : Boolean := True;

   --  If true, canon configuration.
   Canon_Flag_Configurations : Boolean := True;

   --  If true, canon associations (reorder, add open associations, signal
   --  association with a non globally expression).
   Canon_Flag_Associations : Boolean := True;

   --  If true, canon lists in specifications.
   Canon_Flag_Specification_Lists : Boolean := True;

   --  If true, canon expressions.
   Canon_Flag_Expressions : Boolean := False;

   --  If true, replace 'all' sensitivity list by the explicit list
   --  (If true, Canon_Flag_Sequentials_Stmts must be true)
   Canon_Flag_All_Sensitivity : Boolean := False;

   --  Add suspend state variables and statements.
   Canon_Flag_Add_Suspend_State : Boolean := False;

   --  Do canonicalization:
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

   --  Compute the sensivity list of EXPR and add it to SENSIVITY_LIST.
   --  If IS_TARGET is true, the longest static prefix of the signal name
   --  is not added to the sensitivity list, but other static prefix (such
   --  as indexes of an indexed name) are added.
   procedure Canon_Extract_Sensitivity_Expression
     (Expr: Iir; Sensitivity_List: Iir_List; Is_Target: Boolean := False);

   --  Extract sensitivity of WAVEFORM.
   procedure Extract_Waveform_Sensitivity
     (Waveform : Iir; Sensitivity_List: Iir_List);

   --  Likewise, but for all expressions appearing in statements CHAIN.
   procedure Canon_Extract_Sensitivity_Sequential_Statement_Chain
     (Chain : Iir; List : Iir_List);

   --  Compute the sensitivity list of all-sensitized process PROC.
   --  Used for vhdl 08.
   function Canon_Extract_Sensitivity_Process
     (Proc : Iir_Sensitized_Process_Statement) return Iir_List;

   --  For a concurrent or sequential conditional signal assignment.
   procedure Canon_Extract_Sensitivity_Conditional_Signal_Assignment
     (Stmt : Iir; List : Iir_List);

   --  For a concurrent or sequential simple signal assignment.
   procedure Canon_Extract_Sensitivity_Simple_Signal_Assignment
     (Stmt : Iir; List : Iir_List);

   --  For a concurrent selected signal statement.
   procedure Canon_Extract_Sensitivity_Selected_Signal_Assignment
     (Stmt : Iir; List : Iir_List);

   --  For a concurrent or sequential simple assertion statement.
   procedure Canon_Extract_Sensitivity_Assertion_Statement
     (Stmt : Iir; List : Iir_List);

   --  For a concurrent break statement (AMS).
   procedure Canon_Extract_Sensitivity_Break_Statement
     (Stmt : Iir; Sensitivity_List : Iir_List);

   --  For a procedure call.
   procedure Canon_Extract_Sensitivity_Procedure_Call
     (Call : Iir; Sensitivity_List : Iir_List);

end Vhdl.Canon;
