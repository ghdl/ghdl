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

package Trans.Chap2 is
   --  Subprogram specification being currently translated.  This is used
   --  for the return statement.
   Current_Subprogram : Iir := Null_Iir;

   procedure Translate_Subprogram_Interfaces (Spec : Iir);
   procedure Elab_Subprogram_Interfaces (Spec : Iir);

   procedure Translate_Subprogram_Declaration (Spec : Iir);
   procedure Translate_Subprogram_Body (Subprg : Iir);

   --  Set the identifier prefix with the subprogram identifier and
   --  overload number if any.
   procedure Push_Subprg_Identifier (Spec : Iir; Mark : out Id_Mark_Type);

   --  Package declaration, body and instantiation when they are design units.
   procedure Translate_Package_Declaration_Unit
     (Decl : Iir_Package_Declaration);
   procedure Translate_Package_Body_Unit (Bod : Iir_Package_Body);
   procedure Translate_Package_Instantiation_Declaration_Unit (Inst : Iir);
   procedure Elab_Package_Unit_Without_Body (Spec : Iir);

   --  For nested packages.
   procedure Translate_Package_Declaration (Decl : Iir_Package_Declaration);
   procedure Translate_Package_Body (Bod : Iir_Package_Body);
   procedure Translate_Package_Instantiation_Declaration (Inst : Iir);
   procedure Translate_Package_Declaration_Subprograms
     (Decl : Iir_Package_Declaration; What : Subprg_Translate_Kind);
   procedure Translate_Package_Body_Subprograms
     (Bod : Iir_Package_Body; What : Subprg_Translate_Kind);
   procedure Translate_Package_Instantiation_Declaration_Subprograms
     (Inst : Iir; What : Subprg_Translate_Kind);
   procedure Elab_Package_Declaration (Spec : Iir);
   procedure Elab_Package_Body (Spec : Iir_Package_Declaration; Bod : Iir);
   procedure Elab_Package_Instantiation_Declaration (Inst : Iir);

   --  Add info for an interface_package_declaration or a
   --  package_instantiation_declaration
   procedure Instantiate_Info_Package (Inst : Iir);

   procedure Instantiate_Info_Entity (Inst : Iir);

   --  Elaborate packages that DESIGN_UNIT depends on (except std.standard).
   procedure Elab_Dependence (Design_Unit: Iir_Design_Unit);

   --  Declare an incomplete record type DECL_TYPE and access PTR_TYPE to
   --  it.  The names are respectively INSTTYPE and INSTPTR.
   procedure Declare_Inst_Type_And_Ptr (Scope    : Var_Scope_Acc;
                                        Ptr_Type : out O_Tnode);
end Trans.Chap2;
