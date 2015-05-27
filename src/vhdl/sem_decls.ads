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

package Sem_Decls is
   --  Analyze an interface chain.
   procedure Sem_Interface_Chain (Interface_Chain: Iir;
                                  Interface_Kind : Interface_Kind_Type);

   --  Create predefined operations for DECL.
   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False);

   --  Semantize declarations of PARENT.
   procedure Sem_Declaration_Chain (Parent : Iir);

   --  Check all declarations of DECLS_PARENT are complete
   --  This checks subprograms, deferred constants, incomplete types and
   --  protected types.
   --
   --  DECL is the declaration that contains the declaration_list DECLS_PARENT.
   --  (location of errors).
   --  DECL is different from DECLS_PARENT for package bodies and protected
   --  type bodies.
   --
   --  Also, report unused declarations if DECL = DECLS_PARENT.
   --  As a consequence, Check_Full_Declaration must be called after sem
   --  of statements, if any.
   procedure Check_Full_Declaration (Decls_Parent : Iir; Decl: Iir);

   procedure Sem_Iterator (Iterator : Iir_Iterator_Declaration;
                           Staticness : Iir_Staticness);

   --  Extract from NAME the named entity whose profile matches SIG.  If NAME
   --  is an overload list, it is destroyed.
   function Sem_Signature (Name : Iir; Sig : Iir_Signature) return Iir;

   --  The attribute signals ('stable, 'quiet and 'transaction) are
   --  implicitely declared.
   --  Note: guard signals are also implicitly declared but with a guard
   --   expression, which is at a known location.
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

   --  Declare an implicit signal.
   procedure Add_Declaration_For_Implicit_Signal (Sig : Iir);

private
   type Implicit_Signal_Declaration_Type is record
      --  Declaration or statement than will contain implicit declarations.
      Decls_Parent : Iir;

      --  If True, declarations of DECLS_PARENT have already been analyzed.
      --  So implicit declarations are appended to the parent, and the last
      --  declaration is LAST_IMPLICIT_DECL.
      --  If False, declarations are being analyzed.  Implicit declarations
      --  are saved in FIRST_IMPLICIT_DECL / LAST_IMPLICIT_DECL and will be
      --  inserted before the current declaration.
      Decls_Analyzed : Boolean;

      --  If DECLS_ANALYZED is False, this is the chain of implicit
      --  declarations.  If True, LAST_IMPLICIT_DECL contains the last
      --  declaration.
      First_Implicit_Decl : Iir;
      Last_Implicit_Decl : Iir;
   end record;
end Sem_Decls;
