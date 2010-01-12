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

package Sem_Stmts is
   --  Semantize declarations and concurrent statements of BLK, which is
   --  either an architecture_declaration or a block_statement.
   --  If SEM_DECLS is true, then semantize the declarations of BLK.
   procedure Sem_Block (Blk: Iir; Sem_Decls : Boolean);

   procedure Sem_Concurrent_Statement_Chain
     (Parent : Iir; Is_Passive : Boolean);

   --  Some signals are implicitly declared.  This is the case for signals
   --  declared by an attribute ('stable, 'quiet and 'transaction).
   --  Note: guard signals are also implicitly declared, but with a guard
   --   expression, which is located.
   --  Since these signals need resources and are not easily located (can be
   --  nearly in every expression), it is useful to add a node into a
   --  declaration list to declare them.
   --  However, only a few declaration_list can declare signals.  These
   --  declarations lists must register and unregister themselves with
   --  push_declarative_region_with_signals and
   --  pop_declarative_region_with_signals.
   type Implicit_Signal_Declaration_Type is private;

   procedure Push_Signals_Declarative_Part
     (Cell: out Implicit_Signal_Declaration_Type; Decls_Parent : Iir);

   procedure Pop_Signals_Declarative_Part
     (Cell: in Implicit_Signal_Declaration_Type);

   -- Declare an implicit signal.
   procedure Add_Declaration_For_Implicit_Signal (Sig : Iir);

   --  Semantize declaration chain and sequential statement chain
   --  of BODY_PARENT.
   --  DECL is the declaration for these chains (DECL is the declaration, which
   --   is different from the bodies).
   --  This is used by processes and subprograms semantization.
   procedure Sem_Sequential_Statements (Decl : Iir; Body_Parent : Iir);

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
private
   type Implicit_Signal_Declaration_Type is record
      Decls_Parent : Iir;
      Last_Decl : Iir;
   end record;

end Sem_Stmts;
