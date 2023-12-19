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
with Areapools;
with Name_Table;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Std_Package;
with Vhdl.Configuration; use Vhdl.Configuration;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Sem_Inst;

with Elab.Memtype;
with Elab.Vhdl_Annotations;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Decls; use Elab.Vhdl_Decls;
with Elab.Vhdl_Types; use Elab.Vhdl_Types;
with Elab.Vhdl_Stmts; use Elab.Vhdl_Stmts;
with Elab.Vhdl_Files;
with Elab.Vhdl_Errors; use Elab.Vhdl_Errors;
with Elab.Vhdl_Expr; use Elab.Vhdl_Expr;

with Synth.Vhdl_Expr; use Synth.Vhdl_Expr;
with Synth.Vhdl_Stmts;

package body Elab.Vhdl_Insts is
   --  Immediately elaborate sub instances.
   --  Needed if external names are present as they can refer to declarations
   --  in sub-instances.
   --  Otherwise, sub-instances are elaborated at the end of each unit.
   Flag_Elab_Sub_Instances : constant Boolean := True;

   procedure Elab_Instance_Body (Syn_Inst : Synth_Instance_Acc);
   procedure Elab_Recurse_Instantiations
     (Syn_Inst : Synth_Instance_Acc; Head : Node);
   procedure Elab_Recurse_Instantiations_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node);

   Inst_Num : Positive := 1;

   procedure Add_To_Elab_Units (Lib_Unit : Node)
   is
      Design_Unit : constant Node := Get_Design_Unit (Lib_Unit);
   begin
      if Design_Unit = Null_Iir
        or else
        Get_Kind (Design_Unit) = Iir_Kind_Component_Instantiation_Statement
      then
         --  Library units without a parent are the instantiated ones.
         declare
            use Name_Table;
            Orig : constant Node := Vhdl.Sem_Inst.Get_Origin (Lib_Unit);
            Orig_Id : constant Name_Id := Get_Identifier (Orig);
            Id_Len : constant Natural := Get_Name_Length (Orig_Id);
            Suffix : constant String := Natural'Image (Inst_Num);
            New_Name : String (1 .. Id_Len + 8);
            New_Id : Name_Id;
         begin
            New_Name (1 .. Id_Len) := Image (Orig_Id);
            New_Name (Id_Len + 1 .. Id_Len + Suffix'Length) := Suffix;
            New_Name (Id_Len + 1) := 'O';  -- Replace the space.
            New_Id := Get_Identifier (New_Name (1 .. Id_Len + Suffix'Length));
            Set_Identifier (Lib_Unit, New_Id);
            Inst_Num := Inst_Num + 1;
         end;

         Elab_Units.Append (Lib_Unit);
      else
         if not Get_Elab_Flag (Design_Unit) then
            Elab_Units.Append (Lib_Unit);
            Set_Elab_Flag (Design_Unit, True);
         end if;
      end if;
   end Add_To_Elab_Units;

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
      use Elab.Memtype;
      Marker : Mark_Type;
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Assoc_Inter : Node;
      Actual : Node;
      Formal_Typ : Type_Acc;
      Formal_Base : Valtyp;
      Formal_Offs : Value_Offsets;
      Val : Valtyp;
   begin
      Mark_Expr_Pool (Marker);

      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);
         case Iir_Kinds_Interface_Declaration (Get_Kind (Inter)) is
            when Iir_Kind_Interface_Constant_Declaration =>
               Inter_Type := Elab_Declaration_Type (Sub_Inst, Inter);
               Formal_Base := No_Valtyp;

               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_Open =>
                     Actual := Get_Default_Value (Inter);
                     Val := Synth_Expression_With_Type
                       (Sub_Inst, Actual, Inter_Type);
                  when Iir_Kind_Association_Element_By_Expression =>
                     Actual := Get_Actual (Assoc);
                     if Get_Whole_Association_Flag (Assoc) then
                        Formal_Typ := Inter_Type;
                     else
                        declare
                           use Synth.Vhdl_Stmts;
                           Formal : constant Node := Get_Formal (Assoc);
                           Dyn : Dyn_Name;
                        begin
                           Synth_Assignment_Prefix
                             (Syn_Inst, Sub_Inst, Formal,
                              Formal_Base, Formal_Typ, Formal_Offs, Dyn);
                           pragma Assert (Dyn = No_Dyn_Name);
                        end;
                     end if;
                     Val := Synth_Expression_With_Type
                       (Syn_Inst, Actual, Formal_Typ);
                  when Iir_Kind_Association_Element_By_Individual =>
                     Val.Typ := Synth_Subtype_Indication
                       (Syn_Inst, Get_Actual_Type (Assoc));
                     Val := Create_Value_Memory (Val.Typ, Expr_Pool'Access);
                  when others =>
                     raise Internal_Error;
               end case;

               if Get_Whole_Association_Flag (Assoc) then
                  Val := Exec_Subtype_Conversion
                    (Val, Inter_Type, True, Assoc);
               end if;

               if Val = No_Valtyp then
                  Set_Error (Sub_Inst);
               elsif not Is_Static (Val.Val) then
                  Error_Msg_Elab
                    (Syn_Inst, Assoc,
                     "value of generic %i must be static", +Inter);
                  Val := No_Valtyp;
                  Set_Error (Sub_Inst);
               end if;

               if Get_Whole_Association_Flag (Assoc) then
                  if Val /= No_Valtyp then
                     Val := Unshare (Val, Global_Pool'Access);
                     Val.Typ := Unshare (Val.Typ, Global_Pool'Access);
                  end if;
                  Create_Object (Sub_Inst, Inter, Val);
               else
                  --  Modify the generic.
                  Copy_Memory (Formal_Base.Val.Mem + Formal_Offs.Mem_Off,
                               Get_Memory (Val), Formal_Typ.Sz);
               end if;

               Release_Expr_Pool (Marker);

            when Iir_Kind_Interface_Package_Declaration =>
               declare
                  Actual : Node;
                  Pkg_Inst : Synth_Instance_Acc;
               begin
                  if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open
                  then
                     Elab_Package_Instantiation (Sub_Inst, Inter);
                  else
                     Actual := Strip_Denoting_Name (Get_Actual (Assoc));
                     Pkg_Inst := Get_Package_Object (Sub_Inst, Actual);
                     Create_Package_Interface (Sub_Inst, Inter, Pkg_Inst);
                  end if;
               end;

            when Iir_Kind_Interface_Type_Declaration =>
               declare
                  Act : Node;
                  Act_Typ : Type_Acc;
               begin
                  Act := Get_Actual (Assoc);
                  if Get_Kind (Act) in Iir_Kinds_Denoting_Name then
                     Act := Get_Type (Act);
                  end if;
                  if Get_Kind (Act) in Iir_Kinds_Subtype_Definition then
                     --  Need to elaborate the subtype.
                     Act_Typ := Synth_Subtype_Indication (Syn_Inst, Act);
                  else
                     --  An existing type.
                     Act_Typ := Get_Subtype_Object (Syn_Inst, Act);
                  end if;
                  Act_Typ := Unshare (Act_Typ, Instance_Pool);
                  Create_Interface_Type
                    (Sub_Inst, Get_Interface_Type_Definition (Inter),
                     Act_Typ, Act);
                  Release_Expr_Pool (Marker);
               end;

            when Iir_Kind_Interface_Variable_Declaration
               | Iir_Kind_Interface_File_Declaration
               | Iir_Kind_Interface_Signal_Declaration
               | Iir_Kind_Interface_View_Declaration
               | Iir_Kind_Interface_Quantity_Declaration
               | Iir_Kind_Interface_Terminal_Declaration =>
               raise Internal_Error;

            when Iir_Kinds_Interface_Subprogram_Declaration =>
               null;
         end case;

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Elab_Generics_Association;

   procedure Elab_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      Header : constant Node := Get_Package_Header (Pkg);
      Syn_Inst : Synth_Instance_Acc;
   begin
      if Is_Uninstantiated_Package (Pkg) then
         --  Nothing to do (yet) for uninstantiated packages.
         return;
      end if;

      Syn_Inst := Create_Package_Instance (Parent_Inst, Pkg);

      if Header /= Null_Node then
         Elab_Generics_Association
           (Syn_Inst, Parent_Inst,
            Get_Generic_Chain (Header), Get_Generic_Map_Aspect_Chain (Header));
      end if;

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
      Uninst : constant Node := Get_Uninstantiated_Package_Decl (Pkg);
      Bod : Node;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Sub_Inst := Create_Package_Instance (Parent_Inst, Pkg);

      if Get_Kind (Pkg) = Iir_Kind_Interface_Package_Declaration then
         --  Not yet implemented: macro-expanded body for mapped package.
         Bod := Null_Node;
      else
         Bod := Get_Instance_Package_Body (Pkg);
      end if;

      --  Set uninstantiated scope.
      --  Technically this can be done later as it is needed only when
      --  the body is used.  However some designs do access before elaboration,
      --  and we need to detect that.
      if Bod = Null_Node then
         --  Shared body
         Set_Uninstantiated_Scope (Sub_Inst, Uninst);
      end if;

      Elab_Generics_Association
        (Sub_Inst, Parent_Inst,
         Get_Generic_Chain (Pkg), Get_Generic_Map_Aspect_Chain (Pkg));

      Elab_Declarations (Sub_Inst, Get_Declaration_Chain (Pkg));

      if Bod /= Null_Node then
         --  Macro expanded package instantiation.
         if Get_Immediate_Body_Flag (Pkg) then
            Elab_Declarations
              (Sub_Inst, Get_Declaration_Chain (Bod));
         end if;
      else
         --  Shared body
         declare
            Uninst_Bod : constant Node := Get_Package_Body (Uninst);
         begin
            --  Synth declarations of (optional) body.
            if Uninst_Bod /= Null_Node then
               Elab_Declarations
                 (Sub_Inst, Get_Declaration_Chain (Uninst_Bod));
            end if;
         end;
      end if;
   end Elab_Package_Instantiation;

   procedure Elab_Package_Instantiation_Body
     (Parent_Inst : Synth_Instance_Acc; Bod : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
   begin
      Sub_Inst := Get_Package_Object (Parent_Inst, Bod);
      Elab_Declarations
        (Sub_Inst, Get_Declaration_Chain (Bod));
   end Elab_Package_Instantiation_Body;

   procedure Elab_Dependencies (Parent_Inst : Synth_Instance_Acc; Unit : Node);

   procedure Elab_Configuration_Declaration (Parent_Inst : Synth_Instance_Acc;
                                             Conf : Node)
   is
      Syn_Inst : Synth_Instance_Acc;
   begin
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Conf));

      Syn_Inst := Create_Package_Instance (Parent_Inst, Conf);
      Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Conf));
   end Elab_Configuration_Declaration;

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
            Dep_Unit := Get_Library_Unit (Dep);
            case Iir_Kinds_Library_Unit (Get_Kind (Dep_Unit)) is
               when Iir_Kind_Entity_Declaration =>
                  null;
               when Iir_Kind_Configuration_Declaration =>
                  Set_Elab_Flag (Dep, True);
                  Elab_Dependencies (Parent_Inst, Dep);
                  Elab_Units.Append (Dep_Unit);
                  Elab_Configuration_Declaration (Parent_Inst, Dep_Unit);
               when Iir_Kind_Context_Declaration =>
                  Set_Elab_Flag (Dep, True);
                  Elab_Dependencies (Parent_Inst, Dep);
               when Iir_Kind_Package_Declaration =>
                  Set_Elab_Flag (Dep, True);
                  declare
                     Bod : constant Node := Get_Package_Body (Dep_Unit);
                     Bod_Unit : Node;
                  begin
                     --  Dependencies of the package.
                     Elab_Dependencies (Parent_Inst, Dep);
                     Elab_Units.Append (Dep_Unit);
                     Elab_Package_Declaration (Parent_Inst, Dep_Unit);
                     --  Do not try to elaborate math_real body: there are
                     --  functions with loop.  Currently, try create signals,
                     --  which is not possible during package elaboration.
                     if Bod /= Null_Node then
                        Bod_Unit := Get_Design_Unit (Bod);
                        Elab_Dependencies (Parent_Inst, Bod_Unit);
                        Elab_Units.Append (Bod);
                        Set_Elab_Flag (Bod_Unit, True);
                        Elab_Package_Body (Parent_Inst, Dep_Unit, Bod);
                     end if;
                  end;
               when Iir_Kind_Package_Instantiation_Declaration =>
                  Set_Elab_Flag (Dep, True);
                  Elab_Dependencies (Parent_Inst, Dep);
                  Elab_Units.Append (Dep_Unit);
                  Elab_Package_Instantiation (Parent_Inst, Dep_Unit);
               when Iir_Kind_Package_Body =>
                  null;
               when Iir_Kind_Architecture_Body =>
                  null;
               when Iir_Kinds_Verification_Unit =>
                  null;
               when Iir_Kind_Foreign_Module =>
                  raise Internal_Error;
            end case;
         end if;
         Next (Dep_It);
      end loop;
   end Elab_Dependencies;

   function Apply_Block_Configuration_With_Stmts
     (Cfg : Node; Stmts : Node) return Configs_Rec
   is
      Item : Node;
      Count : Natural;
      Res : Iir_Array_Acc;
   begin
      --  Be sure CFG applies to BLK.
      --  There was an assertion, but it doesn't work with instantiated
      --  architectures.

      Count := 0;

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

                     Count := Count + 1;
                  end loop;
               end;
            when Iir_Kind_Block_Configuration =>
               declare
                  Sub_Blk : constant Node := Get_Block_From_Block_Specification
                    (Get_Block_Specification (Item));
                  Prev : Node;
               begin
                  case Get_Kind (Sub_Blk) is
                     when Iir_Kind_Generate_Statement_Body =>
                        -- Linked chain.
                        Prev := Get_Generate_Block_Configuration (Sub_Blk);
                        Set_Prev_Block_Configuration (Item, Prev);
                        Set_Generate_Block_Configuration (Sub_Blk, Item);
                        if Prev = Null_Node then
                           Count := Count + 1;
                        end if;
                     when Iir_Kind_Block_Statement =>
                        Set_Block_Block_Configuration (Sub_Blk, Item);
                        Count := Count + 1;
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

      --  There are COUNT sub-blocks and instances.
      Res := new Iir_Array (1 .. Count);

      Count := 0;
      Item := Stmts;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Component_Instantiation_Statement =>
               if Is_Component_Instantiation (Item) then
                  Count := Count + 1;
                  Res (Count) := Get_Component_Configuration (Item);
                  Set_Component_Configuration (Item, Null_Node);
               end if;
            when Iir_Kind_Block_Statement =>
               Count := Count + 1;
               Res (Count) := Get_Block_Block_Configuration (Item);
               Set_Block_Block_Configuration (Item, Null_Node);
            when Iir_Kind_If_Generate_Statement =>
               declare
                  Item2 : Node;
                  Bod : Node;
               begin
                  Item2 := Item;
                  while Item2 /= Null_Node loop
                     Bod := Get_Generate_Statement_Body (Item2);
                     Count := Count + 1;
                     Res (Count) := Get_Generate_Block_Configuration (Bod);
                     Set_Generate_Block_Configuration (Bod, Null_Node);
                     Item2 := Get_Generate_Else_Clause (Item2);
                  end loop;
               end;
            when Iir_Kind_For_Generate_Statement =>
               declare
                  Bod : Node;
               begin
                  Bod := Get_Generate_Statement_Body (Item);
                  Count := Count + 1;
                  Res (Count) := Get_Generate_Block_Configuration (Bod);
                  Set_Generate_Block_Configuration (Bod, Null_Node);
               end;
            when Iir_Kind_Case_Generate_Statement =>
               declare
                  Bod : Node;
                  Alt : Node;
               begin
                  Alt := Get_Case_Statement_Alternative_Chain (Item);
                  while Alt /= Null_Node loop
                     if not Get_Same_Alternative_Flag (Alt) then
                        Bod := Get_Associated_Expr (Alt);
                        Count := Count + 1;
                        Res (Count) := Get_Generate_Block_Configuration (Bod);
                        Set_Generate_Block_Configuration (Bod, Null_Node);
                     end if;
                     Alt := Get_Chain (Alt);
                  end loop;
               end;
            when others =>
               null;
         end case;
         Item := Get_Chain (Item);
      end loop;

      pragma Assert (Count = Res'Last);
      return (Res, Res'First - 1);
   end Apply_Block_Configuration_With_Stmts;

   function Apply_Block_Configuration
     (Cfg : Node; Blk : Node) return Configs_Rec is
   begin
      return Apply_Block_Configuration_With_Stmts
        (Cfg, Get_Concurrent_Statement_Chain (Blk));
   end Apply_Block_Configuration;

   procedure Free_Configs_Rec (Cfg : in out Configs_Rec) is
   begin
      Free (Cfg.Cfg);
   end Free_Configs_Rec;

   procedure Get_Next_Block_Configuration (Cfg : in out Configs_Rec;
                                           Res : out Node) is
   begin
      Cfg.Idx := Cfg.Idx + 1;
      Res := Cfg.Cfg (Cfg.Idx);
      pragma Assert (Get_Kind (Res) = Iir_Kind_Block_Configuration);
   end Get_Next_Block_Configuration;

   procedure Get_Next_Component_Configuration (Cfg : in out Configs_Rec;
                                               Res : out Node) is
   begin
      Cfg.Idx := Cfg.Idx + 1;
      Res := Cfg.Cfg (Cfg.Idx);
      pragma Assert (Get_Kind (Res) = Iir_Kind_Component_Configuration);
   end Get_Next_Component_Configuration;

   function Elab_Port_Association_Type (Sub_Inst : Synth_Instance_Acc;
                                        Syn_Inst : Synth_Instance_Acc;
                                        Inter : Node;
                                        Assoc : Node) return Type_Acc
   is
      Inter_Type : constant Node := Get_Type (Inter);
      Ind : Node;
      Marker : Mark_Type;
      Inter_Typ : Type_Acc;
      Val : Valtyp;
      Res : Type_Acc;
   begin
      if not Is_Fully_Constrained_Type (Inter_Type) then
         --  TODO
         --  Find the association for this interface
         --  * if individual assoc: get type
         --  * if whole assoc: get type from object.
         if Assoc = Null_Node then
            raise Internal_Error;
         end if;

         Mark_Expr_Pool (Marker);

         if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Expression
           and then not Get_Inertial_Flag (Assoc)
         then
            --  For expression: just compute the expression and associate.
            Inter_Typ := Elab_Declaration_Type (Sub_Inst, Inter);
            Val := Synth_Expression_With_Type
              (Syn_Inst, Get_Actual (Assoc), Inter_Typ);
            Res := Val.Typ;
            if Res /= null then
               Res := Unshare (Res, Global_Pool'Access);
            end if;
         else
            case Iir_Kinds_Association_Element_Parameters (Get_Kind (Assoc)) is
               when Iir_Kinds_Association_Element_By_Actual =>
                  Res := Exec_Name_Subtype (Syn_Inst, Get_Actual (Assoc));
               when Iir_Kind_Association_Element_By_Individual =>
                  Res := Synth_Subtype_Indication
                    (Syn_Inst, Get_Actual_Type (Assoc));
               when Iir_Kind_Association_Element_Open =>
                  Res := Exec_Name_Subtype
                    (Syn_Inst, Get_Default_Value (Inter));
            end case;

            if Res /= null then
               Res := Unshare (Res, Global_Pool'Access);
            end if;

            Ind := Get_Subtype_Indication (Inter);
            if Res /= null
              and then Ind /= Null_Iir
              and then Get_Kind (Ind) in Iir_Kinds_Subtype_Definition
              and then not Get_Is_Ref (Inter)
            then
               Create_Subtype_Object (Sub_Inst, Inter_Type, Res);
            end if;
         end if;

         Release_Expr_Pool (Marker);
         return Res;
      else
         Res := Elab_Declaration_Type (Sub_Inst, Inter);
         return Res;
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
            if Inter_Typ /= null then
               --  Check matching bounds.
               if Get_Kind (Assoc) = Iir_Kind_Association_Element_By_Name
                 and then Get_Formal_Conversion (Assoc) = Null_Node
                 and then Get_Actual_Conversion (Assoc) = Null_Node
               then
                  declare
                     use Synth.Vhdl_Stmts;
                     Marker : Mark_Type;
                     Actual : constant Node := Get_Actual (Assoc);
                     Actual_Base : Valtyp;
                     Actual_Typ : Type_Acc;
                     Actual_Offs : Value_Offsets;
                     Same : Boolean;
                  begin
                     Mark_Expr_Pool (Marker);

                     Synth_Assignment_Prefix
                       (Syn_Inst, Actual,
                        Actual_Base, Actual_Typ, Actual_Offs);
                     case Inter_Typ.Kind is
                        when Type_All_Discrete =>
                           Same := Inter_Typ.Drange = Actual_Typ.Drange;
                        when Type_Float =>
                           Same := Inter_Typ.Frange = Actual_Typ.Frange;
                        when Type_Composite =>
                           if not Check_Matching_Bounds (Syn_Inst, Inter_Typ,
                                                         Actual_Typ, Assoc)
                           then
                              null;
                           end if;
                           Same := True;
                        when Type_Slice
                          | Type_Protected
                          | Type_Access
                          | Type_File =>
                           raise Internal_Error;
                     end case;
                     if not Same then
                        Error_Msg_Elab
                          (Syn_Inst, Assoc,
                           "range of formal %i is different from formal range",
                           +Inter);
                     end if;
                     Release_Expr_Pool (Marker);
                  end;
               elsif (Get_Kind (Assoc)
                        = Iir_Kind_Association_Element_By_Individual)
               then
                  --  Check matching bounds.
                  declare
                     Marker : Mark_Type;
                     Actual_Typ : Type_Acc;
                  begin
                     Mark_Expr_Pool (Marker);

                     --  Although actual type is defined in Syn_Inst, its
                     --  parent is defined in Sub_Inst.
                     Actual_Typ := Synth_Subtype_Indication
                       (Sub_Inst, Get_Actual_Type (Assoc));

                     if not Check_Matching_Bounds (Syn_Inst, Actual_Typ,
                                                   Inter_Typ, Assoc)
                     then
                        --  Error message already emitted.
                        null;
                     end if;
                     Release_Expr_Pool (Marker);
                  end;
               end if;
               Create_Signal (Sub_Inst, Inter, Inter_Typ);
            end if;
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
      Cfgs : Configs_Rec;
   begin
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Unit));

      Unit_Inst := Make_Elab_Instance
        (Syn_Inst, Null_Node, Unit, Config => Null_Node);
      Add_Extra_Instance (Syn_Inst, Unit_Inst);

      Cfgs := Apply_Block_Configuration_With_Stmts
        (Get_Verification_Block_Configuration (Unit),
         Get_Vunit_Item_Chain (Unit));

      Last_Type := Null_Node;
      Item := Get_Vunit_Item_Chain (Unit);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when Iir_Kind_Psl_Default_Clock
               | Iir_Kind_Psl_Declaration
               | Iir_Kind_PSL_Inherit_Spec =>
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
               | Iir_Kind_Attribute_Specification
               | Iir_Kind_Object_Alias_Declaration
               | Iir_Kind_Non_Object_Alias_Declaration
               | Iir_Kind_Subtype_Declaration
               | Iir_Kind_Type_Declaration
               | Iir_Kind_Anonymous_Type_Declaration =>
               Elab_Declaration (Unit_Inst, Item, False, Last_Type);
            when Iir_Kinds_Concurrent_Signal_Assignment
               | Iir_Kinds_Process_Statement
               | Iir_Kinds_Generate_Statement
               | Iir_Kind_Block_Statement
               | Iir_Kind_Concurrent_Procedure_Call_Statement
               | Iir_Kind_Component_Instantiation_Statement =>
               Elab_Concurrent_Statement (Unit_Inst, Item, Cfgs);
            when others =>
               Error_Kind ("elab_verification_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      Free_Configs_Rec (Cfgs);

      if not Flag_Elab_Sub_Instances then
         --  Recurse now.
         Item := Get_Vunit_Item_Chain (Unit);
         while Item /= Null_Node loop
            if Get_Kind (Item) in Iir_Kinds_Concurrent_Statement then
               Elab_Recurse_Instantiations_Statement (Unit_Inst, Item);
            end if;
            Item := Get_Chain (Item);
         end loop;
      end if;
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

   procedure Elab_Recurse_Instantiations_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node) is
   begin
      case Get_Kind (Stmt) is
         when Iir_Kind_Concurrent_Simple_Signal_Assignment
           | Iir_Kind_Concurrent_Conditional_Signal_Assignment
           | Iir_Kind_Concurrent_Selected_Signal_Assignment
           | Iir_Kind_Concurrent_Procedure_Call_Statement
           | Iir_Kind_Concurrent_Break_Statement
           | Iir_Kind_Simple_Simultaneous_Statement
           | Iir_Kind_Simultaneous_If_Statement
           | Iir_Kind_Simultaneous_Case_Statement
           | Iir_Kinds_Process_Statement =>
            null;
         when Iir_Kind_If_Generate_Statement
            | Iir_Kind_Case_Generate_Statement =>
            declare
               Sub_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Syn_Inst, Stmt);
            begin
               if Sub_Inst /= null then
                  Elab_Recurse_Instantiations
                    (Sub_Inst, Get_Source_Scope (Sub_Inst));
               end if;
            end;
         when Iir_Kind_For_Generate_Statement =>
            declare
               Iterator : constant Node :=
                 Get_Parameter_Specification (Stmt);
               Bod : constant Node :=
                 Get_Generate_Statement_Body (Stmt);
               It_Rng : constant Type_Acc :=
                 Get_Subtype_Object (Syn_Inst, Get_Type (Iterator));
               Gen_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Syn_Inst, Stmt);
               Sub_Inst : Synth_Instance_Acc;
            begin
               for I in 1 .. Get_Range_Length (It_Rng.Drange) loop
                  Sub_Inst := Get_Generate_Sub_Instance
                    (Gen_Inst, Positive (I));
                  Elab_Recurse_Instantiations (Sub_Inst, Bod);
               end loop;
            end;
         when Iir_Kind_Component_Instantiation_Statement =>
            if Is_Component_Instantiation (Stmt) then
               declare
                  Comp_Inst : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Syn_Inst, Stmt);
                  Sub_Inst : constant Synth_Instance_Acc :=
                    Get_Component_Instance (Comp_Inst);
               begin
                  if Sub_Inst /= null then
                     --  Nothing to do if the component is not bound.
                     Elab_Instance_Body (Sub_Inst);
                  end if;
               end;
            else
               declare
                  Sub_Inst : constant Synth_Instance_Acc :=
                    Get_Sub_Instance (Syn_Inst, Stmt);
               begin
                  Elab_Instance_Body (Sub_Inst);
               end;
            end if;
         when Iir_Kind_Block_Statement =>
            declare
               Blk_Inst : constant Synth_Instance_Acc :=
                 Get_Sub_Instance (Syn_Inst, Stmt);
            begin
               Elab_Recurse_Instantiations (Blk_Inst, Stmt);
            end;
         when Iir_Kind_Psl_Default_Clock
           | Iir_Kind_Psl_Declaration
           | Iir_Kind_Psl_Restrict_Directive
           | Iir_Kind_Psl_Assume_Directive
           | Iir_Kind_Psl_Cover_Directive
           | Iir_Kind_Psl_Assert_Directive
           | Iir_Kind_Psl_Endpoint_Declaration
           | Iir_Kind_Concurrent_Assertion_Statement =>
            null;
         when others =>
            Error_Kind ("elab_recurse_instantiations_statement", Stmt);
      end case;
   end Elab_Recurse_Instantiations_Statement;

   --  Elaborate instantiations.
   --  This cannot be done immediately like the other statements due to a
   --  possible conflict with configurations.
   --  Configurations are applied by Apply_Block_Configuration to the vhdl
   --  nodes.  If instantiations are handled immediately, in case of recursion,
   --  the configuration may have already been applied to an instantiation and
   --  therefore cannot be applied again.
   --  To avoid this issue, statements are first elaborated and instances for
   --  instantiations are created.  The configuration is saved in the
   --  instances.  Then, instances are elaborated using the configuration
   --  saved.
   procedure Elab_Recurse_Instantiations
     (Syn_Inst : Synth_Instance_Acc; Head : Node)
   is
      Stmt : Node;
   begin
      Stmt := Get_Concurrent_Statement_Chain (Head);
      while Stmt /= Null_Node loop
         Elab_Recurse_Instantiations_Statement (Syn_Inst, Stmt);
         Stmt := Get_Chain (Stmt);
      end loop;
   end Elab_Recurse_Instantiations;

   procedure Elab_Instance_Body (Syn_Inst : Synth_Instance_Acc)
   is
      Arch : constant Node := Get_Source_Scope (Syn_Inst);
      Config : constant Node := Get_Instance_Config (Syn_Inst);
      Arch_Orig : Node;
      Entity : Node;
      Cfgs : Configs_Rec;
   begin
      if Get_Kind (Arch) = Iir_Kind_Foreign_Module then
         return;
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Configurations are not instantiated so they apply to the original
      --  architecture.
      Arch_Orig := Vhdl.Sem_Inst.Get_Origin (Arch);
      if Arch_Orig = Null_Node then
         Arch_Orig := Arch;
      end if;

      Cfgs := Apply_Block_Configuration (Config, Arch_Orig);

      --  File names are relative to the source.
      Elab.Vhdl_Files.Set_Design_Unit (Arch);

      Entity := Get_Entity (Arch);

      Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      pragma Assert (Is_Expr_Pool_Empty);

      if not Is_Error (Syn_Inst) then
         --  CFGS won't be used.
         Elab_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Entity), Cfgs);
         pragma Assert (Is_Expr_Pool_Empty);
      end if;

      if not Is_Error (Syn_Inst) then
         Elab_Verification_Units (Syn_Inst, Entity);
         pragma Assert (Is_Expr_Pool_Empty);
      end if;

      if not Is_Error (Syn_Inst) then
         Elab_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
         pragma Assert (Is_Expr_Pool_Empty);
      end if;

      if not Is_Error (Syn_Inst) then
         Elab_Concurrent_Statements
           (Syn_Inst, Get_Concurrent_Statement_Chain (Arch), Cfgs);
         pragma Assert (Is_Expr_Pool_Empty);
      end if;

      Free_Configs_Rec (Cfgs);

      if not Flag_Elab_Sub_Instances then
         if not Is_Error (Syn_Inst) then
            Elab_Recurse_Instantiations (Syn_Inst, Arch);
            pragma Assert (Areapools.Is_Empty (Expr_Pool));
         end if;
      end if;

      if not Is_Error (Syn_Inst) then
         Elab_Verification_Units (Syn_Inst, Arch);
         pragma Assert (Areapools.Is_Empty (Expr_Pool));
      end if;
   end Elab_Instance_Body;

   procedure Elab_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Entity : Node;
      Arch : Node;
      Config : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
      E_Ent : Node;
      E_Arch : Node;
   begin
      if Flag_Macro_Expand_Instance
        and then Get_Macro_Expand_Flag (Entity)
      then
         E_Ent := Vhdl.Sem_Inst.Instantiate_Entity_Declaration (Entity, Stmt);
         E_Arch := Vhdl.Sem_Inst.Instantiate_Architecture
           (Arch, E_Ent, Stmt, Stmt);
         Elab.Vhdl_Annotations.Instantiate_Annotate (E_Ent);
         Elab.Vhdl_Annotations.Instantiate_Annotate (E_Arch);

         --  TODO: remove previous Instantiated_Header.
         Set_Instantiated_Header (Stmt, E_Ent);

         pragma Assert (Get_Parent (E_Ent) = Null_Iir);
         Set_Parent (E_Ent, Stmt);
      else
         E_Ent := Entity;
         E_Arch := Arch;
      end if;

      --  Elaborate generic + map aspect
      Sub_Inst := Make_Elab_Instance (Syn_Inst, Stmt, E_Arch, Config);

      Create_Sub_Instance (Syn_Inst, Stmt, Sub_Inst);

      pragma Assert (Is_Expr_Pool_Empty);

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Entity));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      pragma Assert (Is_Expr_Pool_Empty);

      Add_To_Elab_Units (E_Ent);

      Elab_Generics_Association (Sub_Inst, Syn_Inst,
                                 Get_Generic_Chain (E_Ent),
                                 Get_Generic_Map_Aspect_Chain (Stmt));

      pragma Assert (Is_Expr_Pool_Empty);

      --  Elaborate port types.
      Elab_Ports_Association_Type (Sub_Inst, Syn_Inst,
                                   Get_Port_Chain (E_Ent),
                                   Get_Port_Map_Aspect_Chain (Stmt));

      pragma Assert (Is_Expr_Pool_Empty);

      if Is_Error (Sub_Inst) then
         --  TODO: Free it?
         return;
      end if;

      if Flag_Elab_Sub_Instances then
         Elab_Instance_Body (Sub_Inst);
      end if;

      Add_To_Elab_Units (E_Arch);
   end Elab_Direct_Instantiation_Statement;

   procedure Elab_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Cfgs : in out Configs_Rec)
   is
      Component : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Config : Node;
      Bind : Node;
      Aspect : Iir;
      Comp_Inst : Synth_Instance_Acc;

      Ent : Node;
      Arch : Node;
      Sub_Config : Node;
      Sub_Inst : Synth_Instance_Acc;
   begin
      Get_Next_Component_Configuration (Cfgs, Config);
      Bind := Get_Binding_Indication (Config);

      pragma Assert (Is_Expr_Pool_Empty);

      --  Create the sub-instance for the component
      --  Elaborate generic + map aspect
      Comp_Inst := Make_Elab_Instance (Syn_Inst, Stmt, Component, Config);
      Create_Sub_Instance (Syn_Inst, Stmt, Comp_Inst);

      pragma Assert (Is_Expr_Pool_Empty);

      Elab_Generics_Association (Comp_Inst, Syn_Inst,
                                 Get_Generic_Chain (Component),
                                 Get_Generic_Map_Aspect_Chain (Stmt));

      pragma Assert (Is_Expr_Pool_Empty);

      --  Create objects for the inputs and the outputs of the component,
      --  assign inputs (that's nets) and create wires for outputs.
      Elab_Ports_Association_Type (Comp_Inst, Syn_Inst,
                                   Get_Port_Chain (Component),
                                   Get_Port_Map_Aspect_Chain (Stmt));

      Set_Component_Configuration (Stmt, Null_Node);

      pragma Assert (Is_Expr_Pool_Empty);

      if Bind = Null_Iir then
         --  No association.
         Create_Component_Instance (Comp_Inst, null);
         return;
      end if;

      Aspect := Get_Entity_Aspect (Bind);

      --  Extract entity/architecture instantiated by the component.
      case Iir_Kinds_Entity_Aspect (Get_Kind (Aspect)) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Ent := Get_Entity (Aspect);
            Arch := Get_Architecture (Aspect);
            Sub_Config := Get_Block_Configuration (Config);
         when Iir_Kind_Entity_Aspect_Configuration =>
            Sub_Config := Get_Block_Configuration (Get_Configuration (Aspect));
            Arch := Get_Block_Specification (Sub_Config);
            Ent := Get_Entity (Get_Named_Entity (Arch));
         when Iir_Kind_Entity_Aspect_Open =>
            Create_Component_Instance (Comp_Inst, null);
            return;
      end case;

      if Get_Kind (Ent) = Iir_Kind_Foreign_Module then
         Sub_Inst := Make_Elab_Instance (Comp_Inst, Stmt, Ent, Null_Node);
         Create_Component_Instance (Comp_Inst, Sub_Inst);

         Elab_Foreign_Instance (Sub_Inst, Comp_Inst, Bind, Ent);
         return;
      end if;

      if Arch = Null_Node then
         Arch := Libraries.Get_Latest_Architecture (Ent);
      else
         Arch := Get_Named_Entity (Arch);
      end if;
      if Sub_Config = Null_Node then
         Sub_Config := Get_Library_Unit
           (Get_Default_Configuration_Declaration (Arch));
         Sub_Config := Get_Block_Configuration (Sub_Config);
      end if;

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Ent));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      Add_To_Elab_Units (Ent);

      --  Elaborate generic + map aspect for the entity instance.
      Sub_Inst := Make_Elab_Instance (Comp_Inst, Stmt, Arch, Sub_Config);
      Create_Component_Instance (Comp_Inst, Sub_Inst);

      Elab_Generics_Association (Sub_Inst, Comp_Inst,
                                 Get_Generic_Chain (Ent),
                                 Get_Generic_Map_Aspect_Chain (Bind));

      Elab_Ports_Association_Type (Sub_Inst, Comp_Inst,
                                   Get_Port_Chain (Ent),
                                   Get_Port_Map_Aspect_Chain (Bind));
      pragma Assert (Is_Expr_Pool_Empty);

      if Flag_Elab_Sub_Instances then
         Elab_Instance_Body (Sub_Inst);
      end if;

      Add_To_Elab_Units (Arch);
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
            Arch := Get_Named_Entity (Arch);
         when Iir_Kind_Entity_Aspect_Open =>
            return;
      end case;
      Config := Get_Block_Configuration (Config);
      Ent := Get_Entity (Arch);

      pragma Assert (Is_Expr_Pool_Empty);

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

      Elab_Units.Init;

      --  Annotate units.
      Elab.Vhdl_Annotations.Initialize_Annotate;
      Elab.Vhdl_Annotations.Annotate (Vhdl.Std_Package.Std_Standard_Unit);
      for I in Design_Units.First .. Design_Units.Last loop
         Elab.Vhdl_Annotations.Annotate (Design_Units.Table (I));
      end loop;

      --  Use global memory.
      Instance_Pool := Global_Pool'Access;
      pragma Assert (Is_Expr_Pool_Empty);

      --  Start elaboration.
      Make_Root_Instance;

      Top_Inst := Make_Elab_Instance
        (Root_Instance, Null_Node, Arch, Get_Block_Configuration (Config));

      --  Save the current architecture, so that files can be open using a
      --  path relative to the architecture filename.
      Elab.Vhdl_Files.Set_Design_Unit (Arch);

      Elab_Dependencies (Root_Instance, Get_Design_Unit (Entity));
      Elab_Dependencies (Root_Instance, Get_Design_Unit (Arch));
      Elab_Configuration_Declaration (Root_Instance, Config);

      pragma Assert (Is_Expr_Pool_Empty);

      --  Compute generics.
      Inter := Get_Generic_Chain (Entity);
      while Is_Valid (Inter) loop
         declare
            Em : Mark_Type;
            Val : Valtyp;
            Inter_Typ : Type_Acc;
            Defval : Node;
         begin
            Mark_Expr_Pool (Em);
            Inter_Typ := Elab_Declaration_Type (Top_Inst, Inter);
            Defval := Get_Default_Value (Inter);
            if Defval /= Null_Node then
               Val := Synth_Expression_With_Type (Top_Inst, Defval, Inter_Typ);
            else
               --  Only for simulation, expect override.
               Val := Create_Value_Default (Inter_Typ);
            end if;
            pragma Assert (Is_Static (Val.Val));
            Val := Unshare (Val, Instance_Pool);
            Val.Typ := Unshare_Type_Instance (Val.Typ, Inter_Typ);
            Create_Object (Top_Inst, Inter, Val);
            Release_Expr_Pool (Em);
         end;
         Inter := Get_Chain (Inter);
      end loop;

      pragma Assert (Is_Expr_Pool_Empty);

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Entity);
      while Is_Valid (Inter) loop
         if Is_Fully_Constrained_Type (Get_Type (Inter)) then
            declare
               Inter_Typ : Type_Acc;
            begin
               Inter_Typ := Elab_Declaration_Type (Top_Inst, Inter);
               Create_Signal (Top_Inst, Inter, Inter_Typ);
            end;
         else
            declare
               Def : constant Node := Get_Default_Value (Inter);
               Marker : Mark_Type;
               Inter_Typ : Type_Acc;
               Val : Valtyp;
            begin
               Mark_Expr_Pool (Marker);
               pragma Assert (Def /= Null_Node);
               Inter_Typ := Elab_Declaration_Type (Top_Inst, Inter);
               Val := Synth_Expression_With_Type (Top_Inst, Def, Inter_Typ);
               Val := Unshare (Val, Instance_Pool);
               Val.Typ := Unshare_Type_Instance (Val.Typ, Inter_Typ);
               Release_Expr_Pool (Marker);
               Create_Signal (Top_Inst, Inter, Val.Typ);
            end;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      pragma Assert (Is_Expr_Pool_Empty);

      Top_Instance := Top_Inst;

      Elab_Instance_Body (Top_Inst);

      pragma Assert (Areapools.Is_Empty (Expr_Pool));

      Instance_Pool := null;

      Add_To_Elab_Units (Entity);
      Add_To_Elab_Units (Arch);
      Add_To_Elab_Units (Config);

      --  Clear instance for unused packages.
      for I in Design_Units.First .. Design_Units.Last loop
         declare
            Unit : constant Node := Design_Units.Table (I);
            Lib_Unit : Node;
         begin
            if not Get_Elab_Flag (Unit) then
               --  This unit is not used.
               Lib_Unit := Get_Library_Unit (Unit);
               if Get_Kind (Lib_Unit) = Iir_Kind_Package_Declaration
                 and then not Is_Uninstantiated_Package (Lib_Unit)
               then
                  Clear_Package_Object (Root_Instance, Lib_Unit);
               end if;
            end if;
         end;
      end loop;

      --  Clear elab flag.
      for I in Elab_Units.First .. Elab_Units.Last loop
         declare
            Lib_Unit : constant Node := Elab_Units.Table (I);
            Unit : constant Node := Get_Design_Unit (Lib_Unit);
         begin
            if Unit /= Null_Node
              and then
              Get_Kind (Unit) /= Iir_Kind_Component_Instantiation_Statement
            then
               pragma Assert (Get_Elab_Flag (Unit));
               Set_Elab_Flag (Unit, False);
            end if;
         end;
      end loop;

      return Top_Inst;

   exception
      when Elaboration_Error =>
         return null;
   end Elab_Top_Unit;

end Elab.Vhdl_Insts;
