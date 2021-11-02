--  Design elaboration
--  Copyright (C) 2021 Tristan Gingold
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

with Types; use Types;
with Libraries;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Annotations;
with Vhdl.Configuration; use Vhdl.Configuration;
with Vhdl.Errors; use Vhdl.Errors;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Decls; use Elab.Vhdl_Decls;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Stmts; use Elab.Vhdl_Stmts;
with Elab.Vhdl_Files;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;

package body Elab.Vhdl_Insts is
   procedure Elab_Convertible_Declarations (Syn_Inst : Synth_Instance_Acc)
   is
      use Vhdl.Std_Package;
   begin
      Create_Subtype_Object
        (Syn_Inst, Convertible_Integer_Type_Definition,
         Get_Subtype_Object (Syn_Inst, Universal_Integer_Type_Definition));
      Create_Subtype_Object
        (Syn_Inst, Convertible_Real_Type_Definition,
         Get_Subtype_Object (Syn_Inst, Universal_Real_Type_Definition));
   end Elab_Convertible_Declarations;

   procedure Elab_Generics_Association (Sub_Inst : Synth_Instance_Acc;
                                        Syn_Inst : Synth_Instance_Acc;
                                        Inter_Chain : Node;
                                        Assoc_Chain : Node)
   is
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Assoc_Inter : Node;
      Actual : Node;
      Val : Valtyp;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               Elab_Declaration_Type (Sub_Inst, Inter);
               Inter_Type := Get_Subtype_Object (Sub_Inst, Get_Type (Inter));

               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_Open =>
                     Actual := Get_Default_Value (Inter);
                     Val := Exec_Expression_With_Type
                       (Sub_Inst, Actual, Inter_Type);
                  when Iir_Kind_Association_Element_By_Expression =>
                     Actual := Get_Actual (Assoc);
                     Val := Exec_Expression_With_Type
                       (Syn_Inst, Actual, Inter_Type);
                  when others =>
                     raise Internal_Error;
               end case;

               Val := Exec_Subtype_Conversion (Val, Inter_Type, True, Assoc);

               if Val = No_Valtyp then
                  Set_Error (Sub_Inst);
               elsif not Is_Static (Val.Val) then
                  Error_Msg_Elab
                    (+Assoc, "value of generic %i must be static", +Inter);
                  Val := No_Valtyp;
                  Set_Error (Sub_Inst);
               end if;

               Create_Object (Sub_Inst, Inter, Val);

            when Iir_Kind_Interface_Package_Declaration =>
               declare
                  Actual : constant Iir :=
                    Strip_Denoting_Name (Get_Actual (Assoc));
                  Pkg_Inst : Synth_Instance_Acc;
               begin
                  Pkg_Inst := Get_Package_Object (Sub_Inst, Actual);
                  Create_Package_Interface (Sub_Inst, Inter, Pkg_Inst);
               end;

            when Iir_Kind_Interface_Variable_Declaration
               | Iir_Kind_Interface_File_Declaration
               | Iir_Kind_Interface_Signal_Declaration
               | Iir_Kind_Interface_Quantity_Declaration
               | Iir_Kind_Interface_Terminal_Declaration =>
               raise Internal_Error;

            when Iir_Kinds_Interface_Subprogram_Declaration
               | Iir_Kind_Interface_Type_Declaration =>
               raise Internal_Error;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Elab_Generics_Association;

   procedure Elab_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      Syn_Inst : Synth_Instance_Acc;
   begin
      if Is_Uninstantiated_Package (Pkg) then
         --  Nothing to do (yet) for uninstantiated packages.
         return;
      end if;

      Syn_Inst := Create_Package_Instance (Parent_Inst, Pkg);

      Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Pkg));
      if Pkg = Vhdl.Std_Package.Standard_Package then
         Elab_Convertible_Declarations (Syn_Inst);
      end if;
   end Elab_Package_Declaration;

   procedure Elab_Package_Body
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node; Bod : Node)
   is
      Pkg_Inst : Synth_Instance_Acc;
   begin
      if Is_Uninstantiated_Package (Pkg) then
         --  Nothing to do (yet) for uninstantiated packages.
         return;
      end if;

      Pkg_Inst := Get_Package_Object (Parent_Inst, Pkg);

      Elab_Declarations (Pkg_Inst, Get_Declaration_Chain (Bod));
   end Elab_Package_Body;

   procedure Elab_Package_Instantiation
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      Bod : constant Node := Get_Instance_Package_Body (Pkg);
      Sub_Inst : Synth_Instance_Acc;
   begin
      Sub_Inst := Create_Package_Instance (Parent_Inst, Pkg);

      Elab_Generics_Association
        (Sub_Inst, Parent_Inst,
         Get_Generic_Chain (Pkg), Get_Generic_Map_Aspect_Chain (Pkg));

      Elab_Declarations (Sub_Inst, Get_Declaration_Chain (Pkg));

      if Bod /= Null_Node then
         --  Macro expanded package instantiation.
         raise Internal_Error;
      else
         --  Shared body
         declare
            Uninst : constant Node := Get_Uninstantiated_Package_Decl (Pkg);
            Uninst_Bod : constant Node := Get_Package_Body (Uninst);
         begin
            Set_Uninstantiated_Scope (Sub_Inst, Uninst);
            --  Synth declarations of (optional) body.
            if Uninst_Bod /= Null_Node then
               Elab_Declarations
                 (Sub_Inst, Get_Declaration_Chain (Uninst_Bod));
            end if;
         end;
      end if;
   end Elab_Package_Instantiation;

   procedure Elab_Dependencies (Parent_Inst : Synth_Instance_Acc; Unit : Node)
   is
      Dep_List : constant Node_List := Get_Dependence_List (Unit);
      Dep_It : List_Iterator;
      Dep : Node;
      Dep_Unit : Node;
   begin
      Dep_It := List_Iterate (Dep_List);
      while Is_Valid (Dep_It) loop
         Dep := Get_Element (Dep_It);
         if Get_Kind (Dep) = Iir_Kind_Design_Unit
           and then not Get_Elab_Flag (Dep)
         then
            Set_Elab_Flag (Dep, True);
            Elab_Dependencies (Parent_Inst, Dep);
            Dep_Unit := Get_Library_Unit (Dep);
            case Iir_Kinds_Library_Unit (Get_Kind (Dep_Unit)) is
               when Iir_Kind_Entity_Declaration =>
                  null;
               when Iir_Kind_Configuration_Declaration =>
                  null;
               when Iir_Kind_Context_Declaration =>
                  null;
               when Iir_Kind_Package_Declaration =>
                  declare
                     Bod : constant Node := Get_Package_Body (Dep_Unit);
                     Bod_Unit : Node;
                  begin
                     Elab_Package_Declaration (Parent_Inst, Dep_Unit);
                     --  Do not try to elaborate math_real body: there are
                     --  functions with loop.  Currently, try create signals,
                     --  which is not possible during package elaboration.
                     if Bod /= Null_Node then
                        Bod_Unit := Get_Design_Unit (Bod);
                        Elab_Dependencies (Parent_Inst, Bod_Unit);
                        Elab_Package_Body (Parent_Inst, Dep_Unit, Bod);
                     end if;
                  end;
               when Iir_Kind_Package_Instantiation_Declaration =>
                  Elab_Package_Instantiation (Parent_Inst, Dep_Unit);
               when Iir_Kind_Package_Body =>
                  null;
               when Iir_Kind_Architecture_Body =>
                  null;
               when Iir_Kinds_Verification_Unit =>
                  null;
            end case;
         end if;
         Next (Dep_It);
      end loop;
   end Elab_Dependencies;

   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node)
   is
      Item : Node;
   begin
      --  Be sure CFG applies to BLK.
      pragma Assert (Get_Block_From_Block_Specification
                       (Get_Block_Specification (Cfg)) = Blk);

      --  Clear_Instantiation_Configuration (Blk);

      Item := Get_Configuration_Item_Chain (Cfg);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Component_Configuration =>
               declare
                  List : constant Iir_Flist :=
                    Get_Instantiation_List (Item);
                  El : Node;
                  Inst : Node;
               begin
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Nth_Element (List, I);
                     Inst := Get_Named_Entity (El);
                     pragma Assert
                       (Get_Kind (Inst)
                          = Iir_Kind_Component_Instantiation_Statement);
                     pragma Assert
                       (Get_Component_Configuration (Inst) = Null_Node);
                     Set_Component_Configuration (Inst, Item);
                  end loop;
               end;
            when Iir_Kind_Block_Configuration =>
               declare
                  Sub_Blk : constant Node := Get_Block_From_Block_Specification
                    (Get_Block_Specification (Item));
               begin
                  case Get_Kind (Sub_Blk) is
                     when Iir_Kind_Generate_Statement_Body =>
                        -- Linked chain.
                        Set_Prev_Block_Configuration
                          (Item, Get_Generate_Block_Configuration (Sub_Blk));
                        Set_Generate_Block_Configuration (Sub_Blk, Item);
                     when Iir_Kind_Block_Statement =>
                        Set_Block_Block_Configuration (Sub_Blk, Item);
                     when others =>
                        Vhdl.Errors.Error_Kind
                          ("apply_block_configuration(blk)", Sub_Blk);
                  end case;
               end;
            when others =>
               Vhdl.Errors.Error_Kind ("apply_block_configuration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Apply_Block_Configuration;

   function Elab_Port_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                        Syn_Inst : Synth_Instance_Acc;
                                        Inter : Node;
                                        Assoc : Node) return Type_Acc is
   begin
      if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
         --  TODO
         --  Find the association for this interface
         --  * if individual assoc: get type
         --  * if whole assoc: get type from object.
         if Assoc = Null_Node then
            raise Internal_Error;
         end if;
         case Get_Kind (Assoc) is
            when Iir_Kinds_Association_Element_By_Actual =>
               return Exec_Type_Of_Object (Syn_Inst, Get_Actual (Assoc));
            when others =>
               raise Internal_Error;
         end case;
      else
         Elab_Declaration_Type (Sub_Inst, Inter);
         return Get_Subtype_Object (Sub_Inst, Get_Type (Inter));
      end if;
   end Elab_Port_Association_Type;

   procedure Elab_Ports_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                          Syn_Inst : Synth_Instance_Acc;
                                          Inter_Chain : Node;
                                          Assoc_Chain : Node)
   is
      Inter : Node;
      Assoc : Node;
      Assoc_Inter : Node;
      Inter_Typ : Type_Acc;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         if Get_Whole_Association_Flag (Assoc) then
            Inter_Typ := Elab_Port_Association_Type
              (Sub_Inst, Syn_Inst, Inter, Assoc);
            Create_Signal (Sub_Inst, Inter, Inter_Typ, null);
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Elab_Ports_Association_Type;

   procedure Elab_Verification_Unit
     (Syn_Inst : Synth_Instance_Acc; Unit : Node)
   is
      Unit_Inst : Synth_Instance_Acc;
      Item : Node;
      Last_Type : Node;
   begin
      Unit_Inst := Make_Elab_Instance (Syn_Inst, Unit, Config => Null_Node);
      Add_Extra_Instance (Syn_Inst, Unit_Inst);

      Apply_Block_Configuration
        (Get_Verification_Block_Configuration (Unit), Unit);

      Last_Type := Null_Node;
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
              | Iir_Kind_Psl_Declaration =>
               null;
            when Iir_Kind_Psl_Assert_Directive
               | Iir_Kind_Psl_Assume_Directive
               | Iir_Kind_Psl_Cover_Directive
               | Iir_Kind_Psl_Restrict_Directive =>
               null;
            when Iir_Kind_Signal_Declaration
              | Iir_Kind_Constant_Declaration
              | Iir_Kind_Function_Declaration
              | Iir_Kind_Procedure_Declaration
              | Iir_Kind_Function_Body
              | Iir_Kind_Procedure_Body
              | Iir_Kind_Attribute_Declaration
              | Iir_Kind_Attribute_Specification =>
               Elab_Declaration (Unit_Inst, Item, Last_Type);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Elab_Concurrent_Statement (Unit_Inst, Item);
            when others =>
               Error_Kind ("elab_vunit_declaration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Elab_Verification_Unit;

   procedure Elab_Verification_Units
     (Syn_Inst : Synth_Instance_Acc; Parent : Node)
   is
      Unit : Node;
   begin
      Unit := Get_Bound_Vunit_Chain (Parent);
      while Unit /= Null_Node loop
         Elab_Verification_Unit (Syn_Inst, Unit);
         Unit := Get_Bound_Vunit_Chain (Unit);
      end loop;
   end Elab_Verification_Units;

   procedure Elab_Instance_Body (Syn_Inst : Synth_Instance_Acc;
                                 Entity : Node;
                                 Arch : Node;
                                 Config : Node) is
   begin
      Apply_Block_Configuration (Config, Arch);

      Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      Elab_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));

      Elab_Verification_Units (Syn_Inst, Entity);

      Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      Elab_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));

      Elab_Verification_Units (Syn_Inst, Arch);
   end Elab_Instance_Body;

   procedure Elab_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Entity : Node;
      Arch : Node;
      Config : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
   begin
      --  Elaborate generic + map aspect
      Sub_Inst := Make_Elab_Instance (Syn_Inst, Arch, Config);

      Create_Sub_Instance (Syn_Inst, Stmt, Sub_Inst);

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Entity));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));


      Elab_Generics_Association (Sub_Inst, Syn_Inst,
                                 Get_Generic_Chain (Entity),
                                 Get_Generic_Map_Aspect_Chain (Stmt));

      --  Elaborate port types.
      Elab_Ports_Association_Type (Sub_Inst, Syn_Inst,
                                   Get_Port_Chain (Entity),
                                   Get_Port_Map_Aspect_Chain (Stmt));

      if Is_Error (Sub_Inst) then
         --  TODO: Free it?
         return;
      end if;

      --  Recurse.
      Elab_Instance_Body (Sub_Inst, Entity, Arch, Config);
   end Elab_Direct_Instantiation_Statement;

   procedure Elab_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Component : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Config : constant Node := Get_Component_Configuration (Stmt);
      Bind : constant Node := Get_Binding_Indication (Config);
      Aspect : Iir;
      Comp_Inst : Synth_Instance_Acc;

      Ent : Node;
      Arch : Node;
      Sub_Config : Node;
      Sub_Inst : Synth_Instance_Acc;
   begin
      --  Create the sub-instance for the component
      --  Elaborate generic + map aspect
      Comp_Inst := Make_Elab_Instance (Syn_Inst, Component, Config);
      Create_Sub_Instance (Syn_Inst, Stmt, Comp_Inst);

      Elab_Generics_Association (Comp_Inst, Syn_Inst,
                                 Get_Generic_Chain (Component),
                                 Get_Generic_Map_Aspect_Chain (Stmt));

      --  Create objects for the inputs and the outputs of the component,
      --  assign inputs (that's nets) and create wires for outputs.
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Inter : Node;
         Inter_Typ : Type_Acc;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Component);
         while Is_Valid (Assoc) loop
            if Get_Whole_Association_Flag (Assoc) then
               Inter := Get_Association_Interface (Assoc, Assoc_Inter);

               Inter_Typ := Elab_Port_Association_Type
                 (Comp_Inst, Syn_Inst, Inter, Assoc);

               Create_Signal (Comp_Inst, Assoc_Inter, Inter_Typ, null);
            end if;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;

      Set_Component_Configuration (Stmt, Null_Node);

      if Bind = Null_Iir then
         --  No association.
         return;
      end if;

      Aspect := Get_Entity_Aspect (Bind);

      --  Extract entity/architecture instantiated by the component.
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Ent := Get_Entity (Aspect);
            Arch := Get_Architecture (Aspect);
         when others =>
            Vhdl.Errors.Error_Kind
              ("Elab_Component_Instantiation_Statement(2)", Aspect);
      end case;

      if Get_Kind (Ent) = Iir_Kind_Foreign_Module then
         --  TODO.
         raise Internal_Error;
      end if;

      if Arch = Null_Node then
         Arch := Libraries.Get_Latest_Architecture (Ent);
      else
         Arch := Get_Named_Entity (Arch);
      end if;
      Sub_Config := Get_Library_Unit
        (Get_Default_Configuration_Declaration (Arch));
      Sub_Config := Get_Block_Configuration (Sub_Config);

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Ent));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      --  Elaborate generic + map aspect for the entity instance.
      Sub_Inst := Make_Elab_Instance (Comp_Inst, Arch, Sub_Config);
      Create_Component_Instance (Comp_Inst, Sub_Inst);

      Elab_Generics_Association (Sub_Inst, Comp_Inst,
                                 Get_Generic_Chain (Ent),
                                 Get_Generic_Map_Aspect_Chain (Bind));

      Elab_Ports_Association_Type (Sub_Inst, Comp_Inst,
                                   Get_Port_Chain (Ent),
                                   Get_Port_Map_Aspect_Chain (Bind));

      --  Recurse.
      --  TODO: factorize with direct instantiation
      Elab_Instance_Body (Sub_Inst, Ent, Arch, Sub_Config);
   end Elab_Component_Instantiation_Statement;

   procedure Elab_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Aspect : constant Iir := Get_Instantiated_Unit (Stmt);
      Arch : Node;
      Ent : Node;
      Config : Node;
   begin
      --  Load configured entity + architecture
      case Iir_Kinds_Entity_Aspect (Get_Kind (Aspect)) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Arch := Get_Architecture (Aspect);
            if Arch = Null_Node then
               Arch := Libraries.Get_Latest_Architecture (Get_Entity (Aspect));
            else
               Arch := Strip_Denoting_Name (Arch);
            end if;
            Config := Get_Library_Unit
              (Get_Default_Configuration_Declaration (Arch));
         when Iir_Kind_Entity_Aspect_Configuration =>
            Config := Get_Configuration (Aspect);
            Arch := Get_Block_Specification (Get_Block_Configuration (Config));
         when Iir_Kind_Entity_Aspect_Open =>
            return;
      end case;
      Config := Get_Block_Configuration (Config);
      Ent := Get_Entity (Arch);

      Elab_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Ent, Arch, Config);
   end Elab_Design_Instantiation_Statement;

   function Elab_Top_Unit (Config : Node) return Synth_Instance_Acc
   is
      Arch : Node;
      Entity : Node;
      Inter : Node;
      Top_Inst : Synth_Instance_Acc;
   begin
      Arch := Get_Named_Entity
        (Get_Block_Specification (Get_Block_Configuration (Config)));
      Entity := Get_Entity (Arch);

      --  Annotate units.
      Vhdl.Annotations.Flag_Synthesis := True;
      Vhdl.Annotations.Initialize_Annotate;
      Vhdl.Annotations.Annotate (Vhdl.Std_Package.Std_Standard_Unit);
      for I in Design_Units.First .. Design_Units.Last loop
         Vhdl.Annotations.Annotate (Design_Units.Table (I));
      end loop;

      Elab.Vhdl_Objtypes.Init;

      --  Start elaboration.
      Make_Root_Instance;

      Top_Inst := Make_Elab_Instance (Root_Instance, Arch, Null_Node);

      --  Save the current architecture, so that files can be open using a
      --  path relative to the architecture filename.
      Elab.Vhdl_Files.Set_Design_Unit (Arch);

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Entity));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      --  Compute generics.
      Inter := Get_Generic_Chain (Entity);
      while Is_Valid (Inter) loop
         Elab_Declaration_Type (Top_Inst, Inter);
         declare
            Val : Valtyp;
            Inter_Typ : Type_Acc;
         begin
            Inter_Typ := Get_Subtype_Object (Top_Inst, Get_Type (Inter));
            Val := Exec_Expression_With_Type
              (Top_Inst, Get_Default_Value (Inter), Inter_Typ);
            pragma Assert (Is_Static (Val.Val));
            Create_Object (Top_Inst, Inter, Val);
         end;
         Inter := Get_Chain (Inter);
      end loop;

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Entity);
      while Is_Valid (Inter) loop
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            --  TODO
            raise Internal_Error;
         end if;
         declare
            Inter_Typ : Type_Acc;
         begin
            Elab_Declaration_Type (Top_Inst, Inter);
            Inter_Typ := Get_Subtype_Object (Top_Inst, Get_Type (Inter));
            Create_Signal (Top_Inst, Inter, Inter_Typ, null);
         end;
         Inter := Get_Chain (Inter);
      end loop;

      Elab_Instance_Body
        (Top_Inst, Entity, Arch, Get_Block_Configuration (Config));

      --  Clear elab_flag
      for I in Design_Units.First .. Design_Units.Last loop
         Set_Elab_Flag (Design_Units.Table (I), False);
      end loop;

      return Top_Inst;
   end Elab_Top_Unit;

end Elab.Vhdl_Insts;
