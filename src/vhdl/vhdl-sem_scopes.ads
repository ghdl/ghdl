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
with Types; use Types;

package Vhdl.Sem_Scopes is

   --  The purpose of SEM_NAME package is to handle association between
   --  identifiers and declarations.
   --  Roughly speacking, it implements ch10 of LRM: scope and visibility.
   --
   --  Basic elements are: declarations and declarative region.
   --  Declaration should be understood in the large meaning: any textual
   --   construction declaring an identifier, which can be a label.
   --  A declarative region contains declarations and possibly other
   --   declarative regions.
   --
   --  Rules are scope, visibility and overloading.
   --

   -- Create and close a declarative region.
   -- By closing a declarative region, all declarations made in this region
   --  are discarded.
   procedure Open_Declarative_Region;
   procedure Close_Declarative_Region;

   -- Add meaning DECL for its identifier to the current declarative region.
   procedure Add_Name (Decl: Iir);
   pragma Inline (Add_Name);

   -- Add meaning DECL to the identifier IDENT.
   -- POTENTIALLY is true if the identifier comes from a use clause.
   procedure Add_Name (Decl: Iir; Ident : Name_Id; Potentially: Boolean);

   --  Set the visible_flag of DECL to true.
   procedure Name_Visible (Decl : Iir);

   --  Replace the interpretation OLD of ID by DECL.
   --  ID must have a uniq interpretation OLD (ie, it must not be overloaded).
   --  The interpretation must have been done in the current scope.
   --
   --  This procedure is used when the meaning of a name is changed due to its
   --  analysis, eg: when a concurrent_procedure_call_statement becomes
   --  a component_instantiation_statement.
   procedure Replace_Name (Id: Name_Id; Old : Iir; Decl: Iir);

   --  Interpretation is a simply linked list of what an identifier means.
   --  In LRM08 12.3 Visibility, the sentence is 'the declaration defines a
   --  possible meaning of this occurrence'.
   --  FIXME: replace Interpretation by Meaning.
   type Name_Interpretation_Type is private;

   --  Return true if INTER is a valid interpretation, ie has a corresponding
   --  declaration.  There are only two invalids interpretations, which
   --  are declared just below as constants.
   function Valid_Interpretation (Inter : Name_Interpretation_Type)
                                 return Boolean;
   pragma Inline (Valid_Interpretation);

   -- Get the first interpretation of identifier ID.
   function Get_Interpretation (Id: Name_Id) return Name_Interpretation_Type;

   -- Get the next interpretation from an interpretation.
   function Get_Next_Interpretation (Ni: Name_Interpretation_Type)
                                     return Name_Interpretation_Type;

   --  Get a declaration associated with an interpretation.
   function Get_Declaration (Ni: Name_Interpretation_Type) return Iir;
   pragma Inline (Get_Declaration);

   --  Same as Get_Declaration, but get the name of non-object alias.
   --  (ie, can never returns an object alias).
   function Get_Non_Alias_Declaration (Ni: Name_Interpretation_Type)
                                      return Iir;

   --  Get the previous interpretation of identifier ID, ie the interpretation
   --  for ID before the current interpretation of ID.
   function Get_Under_Interpretation (Id : Name_Id)
     return Name_Interpretation_Type;

   -- Return TRUE if INTER was made directly visible via a use clause.
   function Is_Potentially_Visible (Inter: Name_Interpretation_Type)
     return Boolean;
   pragma Inline (Is_Potentially_Visible);

   --  Return TRUE if INTER was made direclty visible in the current
   --  declarative region.  Note this is different from being declared in the
   --  current declarative region because of use clauses.
   function Is_In_Current_Declarative_Region (Inter: Name_Interpretation_Type)
     return Boolean;
   pragma Inline (Is_In_Current_Declarative_Region);

   --  Return the raw interpretation of ID.  To be used only in case of
   --  invalid interpretation to clarify the issue: this may be due to
   --  conflicting declarations.
   function Get_Interpretation_Raw (Id : Name_Id)
                                   return Name_Interpretation_Type;

   --  Return True iff NI is a conflicting declaration.
   function Is_Conflict_Declaration (Ni : Name_Interpretation_Type)
                                    return Boolean;

   -- Push and pop all interpretations.
   -- This can be used to suspend name interpretation, in case of recursive
   -- semantics.
   -- After a push, all names have no_name_interpretation.
   -- Pop restore the previous state.
   procedure Pop_Interpretations;
   procedure Push_Interpretations;

   -- Execute a use clause on NAME.
   -- Make potentially directly visible declarations of NAMES.
   --procedure Use_Selected_Name (Name : Iir);
   procedure Use_All_Names (Name: Iir);

   --  Achieves visibility of the selected_name of use clause CLAUSE.
   procedure Add_Use_Clause (Clause : Iir_Use_Clause);

   --  Add declarations for context clause in REF.
   procedure Add_Context_Reference (Ref : Iir);

   --  Add declarations for a context clause into the current declarative
   --  regions.
   procedure Add_Context_Clauses (Unit : Iir_Design_Unit);

   --  Handle PSL inherit spec.
   procedure Add_Inherit_Spec (Spec : Iir);

   -- Add declarations from an entity into the current declarative region.
   -- This is needed when an architecture is analysed.
   procedure Add_Entity_Declarations (Entity : Iir_Entity_Declaration);

   -- Add declarations from a package into the current declarative region.
   -- This is needed when a package body is analysed.
   -- FIXME:  this must be done as if the declarative region was extended.
   procedure Add_Package_Declarations (Decl: Iir_Package_Declaration);

   --  Add interfaces declaration of a component into the current declarative
   --  region.
   procedure Add_Component_Declarations
     (Component : Iir_Component_Declaration);

   --  Add declarations from a protected type declaration into the current
   --  declaration region (which is expected to be the region of the protected
   --  type body).
   procedure Add_Protected_Type_Declarations
     (Decl : Iir_Protected_Type_Declaration);

   --  Add declarations of interface chain CHAIN into the current
   --  declarative region.
   procedure Add_Declarations_From_Interface_Chain
     (Chain : Iir; Potentially : Boolean);

   --  Add all declarations for concurrent statements declared in PARENT.
   procedure Add_Declarations_Of_Concurrent_Statement (Parent : Iir);

   --  Add declarations of a declaration chain CHAIN.
   procedure Add_Declarations (Chain : Iir; Potentially : Boolean := False);

   --  Scope extension area contains declarations from another declarative
   --  region.  These area are abstract and only used to be able to add
   --  and remove declarations.
   procedure Open_Scope_Extension;
   procedure Close_Scope_Extension;

   -- Add any declarations that include the end of the declarative part of
   --  the given block BLOCK.  This follow rules of LRM93 10.2
   -- FIXME: BLOCK must be an architecture at first, then blocks declared
   --  inside this architecture, then a block declared inside this block...
   -- This procedure must be called after an Open_Scope_Extension and
   --  declarations added can be removed with Close_Scope_Extension.
   procedure Extend_Scope_Of_Block_Declarations (Decl : Iir);

   --  Call HANDLE_DECL for each declaration found in DECL.
   --  This will generally call HANDLE_DECL with DECL.
   --  For types, HANDLE_DECL is first called with the type declaration, then
   --  with implicit functions, with element literals for enumeration type,
   --  and units for physical type.
   generic
      type Arg_Type is private;
      with procedure Handle_Decl (Decl : Iir; Arg : Arg_Type);
   procedure Iterator_Decl (Decl : Iir; Arg : Arg_Type);

   --  Call HANDLE_DECL for each declaration found in DECL_LIST.
   --  Generally, HANDLE_DECL must be an ITERATOR_DECL; this is not
   --  automatically done, since the user might be interested in using the
   --  ITERATOR_DECL.
   generic
      type Arg_Type is private;
      with procedure Handle_Decl (Decl : Iir; Arg : Arg_Type);
   procedure Iterator_Decl_List (Decl_List : Iir_List; Arg : Arg_Type);

   generic
      type Arg_Type is private;
      with procedure Handle_Decl (Decl : Iir; Arg : Arg_Type);
   procedure Iterator_Decl_Chain (Chain_First : Iir; Arg : Arg_Type);

private
   type Name_Interpretation_Type is new Int32 range 0 .. (2 ** 30) - 1;

   --  This pseudo interpretation marks the end of the interpretation chain,
   --  and means there is no (more) interpretations for the name.
   --  Unless you need to discriminate between an absence of declaration and
   --  a conflict between potential declarations, you should use the
   --  VALID_INTERPRETATION function.
   No_Name_Interpretation : constant Name_Interpretation_Type := 0;

   First_Valid_Interpretation : constant Name_Interpretation_Type := 1;
end Vhdl.Sem_Scopes;
