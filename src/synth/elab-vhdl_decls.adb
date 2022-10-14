--  Create declarations for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils; use Vhdl.Utils;

with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Files;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;
with Elab.Vhdl_Insts;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Stmts; use Synth.Vhdl_Stmts;
with Synth.Vhdl_Decls;

package body Elab.Vhdl_Decls is
   procedure Elab_Subprogram_Declaration
     (Syn_Inst : Synth_Instance_Acc; Subprg : Node)
   is
      Inter : Node;
      Typ : Type_Acc;
   begin
      if Is_Second_Subprogram_Specification (Subprg) then
         --  Already handled.
         return;
      end if;

      Inter := Get_Interface_Declaration_Chain (Subprg);
      while Inter /= Null_Node loop
         Typ := Elab_Declaration_Type (Syn_Inst, Inter);
         Inter := Get_Chain (Inter);
      end loop;
      pragma Unreferenced (Typ);
   end Elab_Subprogram_Declaration;

   procedure Create_Signal (Syn_Inst : Synth_Instance_Acc;
                            Decl : Node;
                            Typ : Type_Acc)
   is
      Def : constant Iir := Get_Default_Value (Decl);
      Expr_Mark : Mark_Type;
      Init : Valtyp;
   begin
      pragma Assert (Typ.Is_Global);

      if Is_Valid (Def) then
         Mark_Expr_Pool (Expr_Mark);
         Init := Synth_Expression_With_Type (Syn_Inst, Def, Typ);
         Init := Exec_Subtype_Conversion (Init, Typ, False, Decl);
         Init := Unshare (Init, Instance_Pool);
         Release_Expr_Pool (Expr_Mark);
      else
         Init := No_Valtyp;
      end if;
      Create_Signal (Syn_Inst, Decl, Typ, Init.Val);
   end Create_Signal;

   procedure Elab_Signal_Declaration (Syn_Inst : Synth_Instance_Acc;
                                      Decl : Node)
   is
      Obj_Typ : Type_Acc;
   begin
      Obj_Typ := Elab_Declaration_Type (Syn_Inst, Decl);

      Create_Signal (Syn_Inst, Decl, Obj_Typ);
   end Elab_Signal_Declaration;

   procedure Elab_Variable_Declaration (Syn_Inst : Synth_Instance_Acc;
                                        Decl : Node;
                                        Force_Init : Boolean)
   is
      Def : constant Node := Get_Default_Value (Decl);
      Marker : Mark_Type;
      Init : Valtyp;
      Obj_Typ : Type_Acc;
   begin
      Obj_Typ := Elab_Declaration_Type (Syn_Inst, Decl);

      Mark_Expr_Pool (Marker);
      if Is_Valid (Def) then
         Init := Synth_Expression_With_Type (Syn_Inst, Def, Obj_Typ);
         Init := Exec_Subtype_Conversion (Init, Obj_Typ, False, Decl);
         Init := Unshare (Init, Instance_Pool);
         --  Note: Obj_Typ is bounded.
         Init.Typ := Obj_Typ;
      else
         if Force_Init then
            Current_Pool := Instance_Pool;
            Init := Create_Value_Default (Obj_Typ);
            Current_Pool := Expr_Pool'Access;
         else
            --  For synthesis, no need to set a value for a shared variable
            --  (they will certainly become a memory).
            Init := (Typ => Obj_Typ, Val => null);
         end if;
      end if;
      Release_Expr_Pool (Marker);

      Create_Object (Syn_Inst, Decl, Init);
   end Elab_Variable_Declaration;

   procedure Elab_File_Declaration (Syn_Inst : Synth_Instance_Acc;
                                    Decl : Node)
   is
      F : File_Index;
      Res : Valtyp;
      Obj_Typ : Type_Acc;
   begin
      F := Elab.Vhdl_Files.Elaborate_File_Declaration (Syn_Inst, Decl);
      Obj_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Decl));
      Res := Create_Value_File (Obj_Typ, F, Instance_Pool);
      Create_Object (Syn_Inst, Decl, Res);
   end Elab_File_Declaration;

   procedure Elab_Free_Quantity_Declaration (Syn_Inst : Synth_Instance_Acc;
                                             Decl : Node)
   is
      Obj_Typ : Type_Acc;
      Res : Valtyp;
   begin
      Obj_Typ := Elab_Declaration_Type (Syn_Inst, Decl);
      Res := Create_Value_Quantity (Obj_Typ, No_Quantity_Index);
      Create_Object (Syn_Inst, Decl, Res);
   end Elab_Free_Quantity_Declaration;

   procedure Elab_Implicit_Signal_Declaration (Syn_Inst : Synth_Instance_Acc;
                                               Decl : Node)
   is
      Obj_Typ : Type_Acc;
   begin
      Obj_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Decl));
      Create_Signal (Syn_Inst, Decl, Obj_Typ, null);
   end Elab_Implicit_Signal_Declaration;

   procedure Elab_Implicit_Quantity_Declaration (Syn_Inst : Synth_Instance_Acc;
                                                 Decl : Node)
   is
      Obj_Typ : Type_Acc;
      Res : Valtyp;
   begin
      Obj_Typ := Get_Subtype_Object (Syn_Inst, Get_Type (Decl));
      Res := Create_Value_Quantity (Obj_Typ, No_Quantity_Index);
      Create_Object (Syn_Inst, Decl, Res);
   end Elab_Implicit_Quantity_Declaration;

   procedure Elab_Terminal_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Res : Valtyp;
   begin
      Res := Create_Value_Terminal (null, No_Terminal_Index);
      Create_Object (Syn_Inst, Decl, Res);
   end Elab_Terminal_Declaration;

   procedure Elab_Nature_Definition
     (Syn_Inst : Synth_Instance_Acc; Def : Node)
   is
      pragma Unreferenced (Syn_Inst);
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            null;
         when others =>
            Error_Kind ("elab_nature_definition", Def);
      end case;
   end Elab_Nature_Definition;

   procedure Elab_Attribute_Specification
     (Syn_Inst : Synth_Instance_Acc; Spec : Node)
   is
      Attr_Decl : constant Node :=
        Get_Named_Entity (Get_Attribute_Designator (Spec));
      Marker : Mark_Type;
      Value : Node;
      Val : Valtyp;
      Val_Type : Type_Acc;
   begin
      Mark_Expr_Pool (Marker);

      Val_Type := Get_Subtype_Object (Syn_Inst, Get_Type (Attr_Decl));
      Value := Get_Attribute_Value_Spec_Chain (Spec);
      while Value /= Null_Iir loop
         --  2. The expression is evaluated to determine the value
         --     of the attribute.
         --     It is an error if the value of the expression does not
         --     belong to the subtype of the attribute; if the
         --     attribute is of an array type, then an implicit
         --     subtype conversion is first performed on the value,
         --     unless the attribute's subtype indication denotes an
         --     unconstrained array type.
         Val := Synth_Expression_With_Type
           (Syn_Inst, Get_Expression (Spec), Val_Type);
         --  Check_Constraints (Instance, Val, Attr_Type, Decl);

         --  3. A new instance of the designated attribute is created
         --     and associated with each of the affected items.
         --
         --  4. Each new attribute instance is assigned the value of
         --     the expression.
         Val := Unshare (Val, Instance_Pool);
         Val.Typ := Unshare (Val.Typ, Instance_Pool);
         Create_Object (Syn_Inst, Value, Val);
         Release_Expr_Pool (Marker);

         Value := Get_Spec_Chain (Value);
      end loop;
   end Elab_Attribute_Specification;

   procedure Elab_Object_Alias_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node)
   is
      Marker : Mark_Type;
      Off : Value_Offsets;
      Res : Valtyp;
      Obj_Typ : Type_Acc;
      Base : Valtyp;
      Typ : Type_Acc;
   begin
      Mark_Expr_Pool (Marker);

      if Get_Subtype_Indication (Decl) /= Null_Node then
         Obj_Typ := Elab_Declaration_Type (Syn_Inst, Decl);
      else
         Obj_Typ := null;
      end if;

      Synth_Assignment_Prefix (Syn_Inst, Get_Name (Decl), Base, Typ, Off);
      Typ := Unshare (Typ, Instance_Pool);
      Res := Create_Value_Alias (Base, Off, Typ, Expr_Pool'Access);
      if Obj_Typ /= null and then Obj_Typ.Kind not in Type_Scalars then
         --  Reshape bounds.
         Res := Exec_Subtype_Conversion (Res, Obj_Typ, True, Decl);
      end if;
      Res := Unshare (Res, Instance_Pool);
      Create_Object (Syn_Inst, Decl, Res);
      Release_Expr_Pool (Marker);
   end Elab_Object_Alias_Declaration;

   procedure Elab_Declaration (Syn_Inst : Synth_Instance_Acc;
                               Decl : Node;
                               Force_Init : Boolean;
                               Last_Type : in out Node)
   is
      use Synth.Vhdl_Decls;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration =>
            Elab_Variable_Declaration (Syn_Inst, Decl, Force_Init);
         --  when Iir_Kind_Interface_Variable_Declaration =>
         --     --  Ignore default value.
         --     Create_Wire_Object (Syn_Inst, Wire_Variable, Decl);
         --     Create_Var_Wire (Syn_Inst, Decl, No_Valtyp);
         when Iir_Kind_Constant_Declaration =>
            Synth_Constant_Declaration (Syn_Inst, Decl, False, Last_Type);
         when Iir_Kind_Signal_Declaration =>
            Elab_Signal_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Object_Alias_Declaration =>
            Elab_Object_Alias_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Procedure_Declaration
            | Iir_Kind_Function_Declaration =>
            Elab_Subprogram_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Procedure_Body
            | Iir_Kind_Function_Body =>
            null;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Attribute_Declaration =>
            --  Nothing to do: the type is a type_mark, not a subtype
            --  indication.
            null;
         when Iir_Kind_Attribute_Specification =>
            Elab_Attribute_Specification (Syn_Inst, Decl);
         when Iir_Kind_Type_Declaration =>
            declare
               Incomp : constant Node :=
                 Get_Incomplete_Type_Declaration (Decl);
               Def : constant Node := Get_Type_Definition (Decl);
            begin
               Elab_Type_Definition (Syn_Inst, Def);
               if Incomp /= Null_Node then
                  Elab_Incomplete_Type_Finish
                    (Syn_Inst, Get_Type_Definition (Incomp), Def);
               end if;
            end;
         when Iir_Kind_Anonymous_Type_Declaration =>
            Elab_Anonymous_Type_Definition
              (Syn_Inst, Get_Type_Definition (Decl),
               Get_Subtype_Definition (Decl));
         when Iir_Kind_Subtype_Declaration =>
            declare
               T : Type_Acc;
            begin
               T := Elab_Declaration_Type (Syn_Inst, Decl);
               pragma Unreferenced (T);
            end;
         when Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_File_Declaration =>
            Elab_File_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Package_Instantiation_Declaration =>
            Vhdl_Insts.Elab_Package_Instantiation (Syn_Inst, Decl);
         when Iir_Kind_Protected_Type_Body =>
            null;
         when Iir_Kind_Psl_Default_Clock =>
            --  Ignored; directly used by PSL directives.
            null;
         when Iir_Kind_Use_Clause =>
            null;
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Attribute_Implicit_Declaration =>
            declare
               El : Node;
            begin
               El := Get_Attribute_Implicit_Chain (Decl);
               while El /= Null_Node loop
                  Elab_Declaration (Syn_Inst, El, Force_Init, Last_Type);
                  El := Get_Attr_Chain (El);
               end loop;
            end;
         when Iir_Kind_Nature_Declaration =>
            Elab_Nature_Definition (Syn_Inst, Get_Nature (Decl));
         when Iir_Kind_Free_Quantity_Declaration =>
            Elab_Free_Quantity_Declaration (Syn_Inst, Decl);
         when Iir_Kinds_Branch_Quantity_Declaration =>
            Elab_Implicit_Quantity_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Above_Attribute =>
            Elab_Implicit_Signal_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Dot_Attribute =>
            Elab_Implicit_Quantity_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Terminal_Declaration =>
            Elab_Terminal_Declaration (Syn_Inst, Decl);
         when Iir_Kinds_Signal_Attribute =>
            Elab_Implicit_Signal_Declaration (Syn_Inst, Decl);
         when Iir_Kind_Disconnection_Specification =>
            null;

         when Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration =>
            null;

         when others =>
            Vhdl.Errors.Error_Kind ("elab_declaration", Decl);
      end case;

      pragma Assert (Is_Expr_Pool_Empty);
   end Elab_Declaration;

   procedure Elab_Declarations (Syn_Inst : Synth_Instance_Acc;
                                Decls : Iir;
                                Force_Init : Boolean := False)
   is
      Decl : Node;
      Last_Type : Node;
   begin
      Last_Type := Null_Node;
      Decl := Decls;
      while Is_Valid (Decl) loop
         Elab_Declaration (Syn_Inst, Decl, Force_Init, Last_Type);

         exit when Is_Error (Syn_Inst);

         Decl := Get_Chain (Decl);
      end loop;
   end Elab_Declarations;

   procedure Finalize_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node; Is_Subprg : Boolean)
   is
      pragma Unreferenced (Syn_Inst);
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Variable_Declaration
           | Iir_Kind_Interface_Variable_Declaration =>
            null;
         when Iir_Kind_Constant_Declaration =>
            null;
         when Iir_Kind_Signal_Declaration
            | Iir_Kind_Interface_Signal_Declaration =>
            pragma Assert (not Is_Subprg);
            null;
         when Iir_Kind_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Procedure_Declaration
           | Iir_Kind_Function_Declaration =>
            null;
         when Iir_Kind_Procedure_Body
           | Iir_Kind_Function_Body =>
            null;
         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Attribute_Declaration =>
            null;
         when Iir_Kind_Attribute_Specification =>
            null;
         when Iir_Kind_Type_Declaration =>
            null;
         when Iir_Kind_Anonymous_Type_Declaration =>
            null;
         when  Iir_Kind_Subtype_Declaration =>
            null;
         when Iir_Kind_Component_Declaration =>
            null;
         when Iir_Kind_File_Declaration =>
            null;
         when Iir_Kind_Configuration_Specification =>
            null;
         when Iir_Kind_Psl_Default_Clock =>
            --  Ignored; directly used by PSL directives.
            null;
         when Iir_Kind_Attribute_Implicit_Declaration =>
            --  Not supported by synthesis.
            null;
         when others =>
            Vhdl.Errors.Error_Kind ("finalize_declaration", Decl);
      end case;
   end Finalize_Declaration;

   procedure Finalize_Declarations (Syn_Inst : Synth_Instance_Acc;
                                    Decls : Iir;
                                    Is_Subprg : Boolean := False)
   is
      Decl : Iir;
   begin
      Decl := Decls;
      while Is_Valid (Decl) loop
         Finalize_Declaration (Syn_Inst, Decl, Is_Subprg);

         Decl := Get_Chain (Decl);
      end loop;
   end Finalize_Declarations;
end Elab.Vhdl_Decls;
