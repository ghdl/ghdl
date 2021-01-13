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

package Vhdl.Sem_Stmts is
   --  Analyze declarations and concurrent statements of BLK, which is
   --  either an architecture_declaration, and entity_declaration,
   --  a block_statement or a generate_statement_body.
   procedure Sem_Block (Blk: Iir);

   --  Analyze concurrent statement STMT.  Used only by PSL.
   procedure Sem_Concurrent_Statement
     (Stmt : in out Iir; Is_Passive : Boolean);

   --  Analyze the concurrent statements of PARENT.
   procedure Sem_Concurrent_Statement_Chain (Parent : Iir);

   --  Analyze declaration chain and sequential statement chain
   --  of BODY_PARENT.
   --  DECL is the declaration for these chains (DECL is the declaration, which
   --   is different from the bodies).
   --  This is used by processes and subprograms analyze.
   procedure Sem_Sequential_Statements (Decl : Iir; Body_Parent : Iir);

   --  Analyze report expression of a PSL statement.
   procedure Sem_Report_Expression (Stmt : Iir);

   --  Sem for concurrent and sequential assertion statements.
   procedure Sem_Report_Statement (Stmt : Iir);

   -- Get the current subprogram or process.
   function Get_Current_Subprogram return Iir;
   pragma Inline (Get_Current_Subprogram);

   --  Get the current concurrent statement, or NULL_IIR if none.
   function Get_Current_Concurrent_Statement return Iir;
   pragma Inline (Get_Current_Concurrent_Statement);

   --  Current PSL default_clock declaration.
   --  Automatically saved and restore while analyzing concurrent statements.
   Current_Psl_Default_Clock : Iir;

   --  Add a driver for SIG.
   --  STMT is used in case of error (it is the statement that creates the
   --   driver).
   --  Do nothing if:
   --    The current statement list does not belong to a process,
   --    SIG is a formal signal interface.
   procedure Sem_Add_Driver (Sig : Iir; Stmt : Iir);
end Vhdl.Sem_Stmts;
