--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

package Trans.Chap4 is
   --  Translate of a type declaration corresponds to the translation of
   --  its definition.
   procedure Translate_Type_Declaration (Decl : Iir);
   procedure Translate_Anonymous_Type_Declaration (Decl : Iir);
   procedure Translate_Subtype_Declaration (Decl : Iir_Subtype_Declaration);
   procedure Translate_Bool_Type_Declaration (Decl : Iir_Type_Declaration);

   --  Translation of declarations occurs in three phases:
   --  1) instance is built by putting objects in a record.
   --  2) subprograms are declared (using instance as the first parameter).
   --  3) subprogram bodies are translated.
   --
   --  In some cases (non nested packages), no instance is needed, thus the
   --  first phase directly creates objects.
   --  Phase 1 and phase 2 could theorically be merged, but the instance
   --  pointer is generally created at the end of the phase 1.
   --
   --  Phase 2 and phase 3 were merged, but there is at least one case where it
   --  is not possible: instantiation of a package INST in package
   --  declaration PKG.  Subprograms declared in INST must have access to the
   --  INST body but also (indirectly) to the PKG body if they call a
   --  subprogram declared in PKG (and passed as a subprogram interface).  So
   --  INST instance is the instance for PKG body.  Therefore they must be
   --  translated during PKG body translation (that's because INST body appears
   --  at the instantiation point instead of the end of PKG body as described
   --  in the LRM).

   --  Translate declaration DECL, which must not be a subprogram
   --  specification.
   procedure Translate_Declaration (Decl : Iir);

   --  Translate declarations, except subprograms spec and bodies.
   procedure Translate_Declaration_Chain (Parent : Iir);

   --  Create declarations for statements STMTS to support resume.
   procedure Translate_Statements_Chain_State_Declaration
     (Stmts : Iir; State_Scope : Var_Scope_Acc);

   --  Translate subprograms in declaration chain of PARENT.
   procedure Translate_Declaration_Chain_Subprograms
     (Parent : Iir; What : Subprg_Translate_Kind);

   --  Create subprograms for type/function conversion of signal
   --  associations.
   --  ENTITY is the entity instantiated, which can be either
   --   an entity_declaration (for component configuration or direct
   --   component instantiation), a component declaration (for a component
   --   instantiation) or Null_Iir (for a block header).
   --  BLOCK is the block/architecture containing the instantiation stmt.
   --  STMT is either the instantiation stmt or the block header.
   procedure Translate_Association_Subprograms
     (Stmt : Iir; Block : Iir; Base_Block : Iir; Entity : Iir);

   --  Elaborate In/Out_Conversion for ASSOC (signals only).
   --  NDEST is the data structure to be registered.
   procedure Elab_In_Conversion
     (Assoc : Iir; Formal : Iir; Ndest : out Mnode);
   procedure Elab_Out_Conversion
     (Assoc : Iir; Formal : Iir; Ndest : out Mnode);

   --  Create code to elaborate declarations.
   --  NEED_FINAL is set when at least one declaration needs to be
   --  finalized (eg: file declaration, protected objects).
   procedure Elab_Declaration_Chain
     (Parent : Iir; Need_Final : out Boolean);

   --  Finalize declarations.
   procedure Final_Declaration_Chain (Parent : Iir; Deallocate : Boolean);

   --  Translate port or generic declarations of PARENT.
   procedure Translate_Port_Chain (Parent : Iir);
   procedure Translate_Generic_Chain (Parent : Iir);

   --  Elaborate signal subtypes and allocate the storage for the object.
   --  If HAS_COPY is true, do not allocate storage for values, as the values
   --  will be directly referenced from the association.
   procedure Elab_Signal_Declaration_Storage
     (Decl : Iir; Has_Copy : Boolean);

   --  Create signal object.
   --  Note: SIG can be a signal sub-element (used when signals are
   --   collapsed).
   --  If CHECK_NULL is TRUE, create the signal only if it was not yet
   --  created.
   --  PARENT is used to link the signal to its parent by rti.
   procedure Elab_Signal_Declaration_Object
     (Sig : Iir; Parent : Iir; Check_Null : Boolean);

   --  True of SIG has a direct driver.
   function Has_Direct_Driver (Sig : Iir) return Boolean;

   --  Allocate memory for direct driver if necessary.
   procedure Elab_Direct_Driver_Declaration_Storage (Decl : Iir);

   --  Generate code to create object OBJ and initialize it with value VAL.
   procedure Elab_Object_Value (Obj : Iir; Value : Iir);

   --  Allocate the storage for OBJ, if necessary.
   procedure Elab_Object_Storage (Obj : Iir);

   --  For a scalar or access type ATYPE, return the default initial value.
   function Get_Scalar_Initial_Value (Atype : Iir) return O_Enode;

   --  Initialize NAME/OBJ with VALUE.
   procedure Elab_Object_Init
     (Name : Mnode; Obj : Iir; Value : Iir; Alloc_Kind : Allocation_Kind);

   --  Get the ortho type for an object of type TINFO.
   function Get_Object_Type (Tinfo : Type_Info_Acc; Kind : Object_Kind_Type)
                                return O_Tnode;

   --  Allocate (and build) a complex object of type OBJ_TYPE.
   --  VAR is the object to be allocated.
   procedure Allocate_Complex_Object (Obj_Type   : Iir;
                                      Alloc_Kind : Allocation_Kind;
                                      Var        : in out Mnode);

   --function Translate_Interface_Declaration
   --  (Decl : Iir; Subprg : Iir) return Tree;

   --  Create a record that describe thes location of an IIR node and
   --  returns the address of it.
   function Get_Location (N : Iir) return O_Dnode;

   --  Set default value to OBJ.
   procedure Init_Object (Obj : Mnode; Obj_Type : Iir);
end Trans.Chap4;
