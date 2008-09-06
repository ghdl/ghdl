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
   --  The kind of an inteface list.
   type Interface_Kind_Type is (Interface_Generic, Interface_Port,
                                Interface_Procedure, Interface_Function);
   subtype Parameter_Kind_Subtype is
     Interface_Kind_Type range Interface_Procedure .. Interface_Function;

   procedure Sem_Interface_Chain (Interface_Chain: Iir;
                                  Interface_Kind : Interface_Kind_Type);

   --  Create predefined operations for DECL.
   procedure Create_Implicit_Operations
     (Decl : Iir; Is_Std_Standard : Boolean := False);

   --  Semantize declarations of PARENT.
   --  If IS_GLOBAL is set, then declarations may be seen outside of the units.
   --  This must be set for entities and packages (except when
   --   Flags.Flag_Whole_Analyze is set).
   procedure Sem_Declaration_Chain (Parent : Iir; Is_Global : Boolean);

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

   --  Extract from NAME the named entity whose profile matches SIG.
   function Sem_Signature (Name : Iir; Sig : Iir_Signature) return Iir;

end Sem_Decls;
