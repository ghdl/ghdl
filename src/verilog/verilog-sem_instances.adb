--  Verilog semantic analyzer (instances)
--  Copyright (C) 2023 Tristan Gingold
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
--  along with this program.  If not, see <gnu.org/licenses>.

with Types; use Types;
with Tables;

with Verilog.Nutils;
with Verilog.Nodes_Meta; use Verilog.Nodes_Meta;

package body Verilog.Sem_Instances is
   --  When a node is cloned, it is referenced in this table using the index
   --  of the original node, so that it won't be cloned twice in case of
   --  multiple references.
   package Clonet is new Tables
     (Table_Component_Type => Node,
      Table_Index_Type => Node,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   procedure Expand_Clone_Table
   is
      Last : constant Node := Nodes.Get_Last_Node;
      First : constant Node := Clonet.Last + 1;
   begin
      Clonet.Set_Last (Last);
      for I in First .. Last loop
         Clonet.Table (I) := Null_Node;
      end loop;
   end Expand_Clone_Table;

   procedure Set_Clone_Table_Size is
   begin
      Clonet.Init;
      Expand_Clone_Table;
   end Set_Clone_Table_Size;

   procedure Clear_Clone_Table is
   begin
      Clonet.Table (Clonet.First .. Clonet.Last) := (others => Null_Node);
   end Clear_Clone_Table;

   procedure Free_Clone_Table is
   begin
      Clonet.Free;
   end Free_Clone_Table;

   function Clone_Node (N : Node) return Node;

   function Clone_Chain (Chain: Node) return Node
   is
      First, Last : Node;
      El : Node;
      New_El : Node;
   begin
      Nutils.Init_Chain (First, Last);
      El := Chain;
      while El /= Null_Node loop
         New_El := Clone_Node (El);
         Nutils.Append_Chain (First, Last, New_El);
         El := Get_Chain (El);
      end loop;
      return First;
   end Clone_Chain;

   procedure Clone_Field (Res : Node; N : Node; F : Fields_Enum)
   is
      Child : Node;
   begin
      case Get_Field_Type (F) is
         when Type_Node =>
            Child := Get_Node (N, F);
            case Get_Field_Actual_Attribute (N, F) is
               when Attr_None =>
                  Set_Node (Res, F, Clone_Node (Child));
               when Attr_Ref | Attr_Forward_Ref =>
                  --  Will be set by Fix_Ref_Node, as the might be a
                  --  forward reference.
                  Set_Node (Res, F, Child);
               when Attr_Chain =>
                  Set_Node (Res, F, Clone_Chain (Child));
               when Attr_Chain_Next =>
                  null;
            end case;
         when Type_String8_Id =>
            Set_String8_Id (Res, F, Get_String8_Id (N, F));
         when Type_Base_Type =>
            Set_Base_Type (Res, F, Get_Base_Type (N, F));
         when Type_Boolean =>
            Set_Boolean (Res, F, Get_Boolean (N, F));
         when Type_Lifetime_Type =>
            Set_Lifetime_Type (Res, F, Get_Lifetime_Type (N, F));
         when Type_Int32 =>
            Set_Int32 (Res, F, Get_Int32 (N, F));
         when Type_Uns32 =>
            Set_Uns32 (Res, F, Get_Uns32 (N, F));
         when Type_Fp64 =>
            Set_Fp64 (Res, F, Get_Fp64 (N, F));
         when Type_Width_Type =>
            Set_Width_Type (Res, F, Get_Width_Type (N, F));
         when Type_Bn_Index =>
            Set_Bn_Index (Res, F, Get_Bn_Index (N, F));
         when Type_Tsize_Type =>
            Set_Tsize_Type (Res, F, Get_Tsize_Type (N, F));
         when Type_Name_Id =>
            Set_Name_Id (Res, F, Get_Name_Id (N, F));
         when Type_Unary_Ops =>
            Set_Unary_Ops (Res, F, Get_Unary_Ops (N, F));
         when Type_Binary_Ops =>
            Set_Binary_Ops (Res, F, Get_Binary_Ops (N, F));
         when Type_Conv_Ops =>
            Set_Conv_Ops (Res, F, Get_Conv_Ops (N, F));
         when Type_Edge_Type =>
            Set_Edge_Type (Res, F, Get_Edge_Type (N, F));
         when Type_Visibility_Type =>
            Set_Visibility_Type (Res, F, Get_Visibility_Type (N, F));
         when Type_DPI_Spec_Type =>
            Set_DPI_Spec_Type (Res, F, Get_DPI_Spec_Type (N, F));
         when Type_Violation_Type =>
            Set_Violation_Type (Res, F, Get_Violation_Type (N, F));
         when Type_Polarity_Type =>
            Set_Polarity_Type (Res, F, Get_Polarity_Type (N, F));
         when Type_Obj_Id =>
            Set_Obj_Id (Res, F, Get_Obj_Id (N, F));
         when Type_Scope_Id =>
            Set_Scope_Id (Res, F, Get_Scope_Id (N, F));
         when Type_Lit_Id =>
            Set_Lit_Id (Res, F, Get_Lit_Id (N, F));
         when Type_Proc_Id =>
            Set_Proc_Id (Res, F, Get_Proc_Id (N, F));
         when Type_Sys_Tf_Id =>
            Set_Sys_Tf_Id (Res, F, Get_Sys_Tf_Id (N, F));
         when Type_Join_Type =>
            Set_Join_Type (Res, F, Get_Join_Type (N, F));
         when Type_Udp_Symbol =>
            Set_Udp_Symbol (Res, F, Get_Udp_Symbol (N, F));
         when Type_Udp_Kind =>
            Set_Udp_Kind (Res, F, Get_Udp_Kind (N, F));
      end case;
   end Clone_Field;

   function Clone_Node (N : Node) return Node
   is
      Kind : Nkind;
      Res : Node;
   begin
      if N = Null_Node then
         return Null_Node;
      end if;

      --  Create the cloned node.
      Kind := Get_Kind (N);
      Res := Create_Node (Kind);
      Set_Location (Res, Get_Location (N));

      --  Insert it in the clonet table.
      pragma Assert (Clonet.Table (N) = Null_Node);
      Clonet.Table (N) := Res;

      --  Recurse.
      declare
         Fields : constant Fields_Array := Get_Fields (Kind);
      begin
         for I in Fields'Range loop
            Clone_Field (Res, N, Fields (I));
         end loop;
      end;

      return Res;
   end Clone_Node;

   procedure Fix_Ref_Node (N : Node);

   procedure Fix_Ref_Chain (Chain: Node)
   is
      El : Node;
   begin
      El := Chain;
      while El /= Null_Node loop
         Fix_Ref_Node (El);
         El := Get_Chain (El);
      end loop;
   end Fix_Ref_Chain;

   procedure Fix_Ref_Node (N : Node) is
   begin
      if N = Null_Node then
         return;
      end if;

      --  Recurse.
      declare
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
         Child : Node;

         function Get_Clone_Ref (C : Node) return Node
         is
            Ref : Node;
         begin
            if C = Null_Node then
               --  For field like 'parent'.
               return Null_Node;
            end if;

            pragma Assert (C <= Clonet.Last);
            Ref := Clonet.Table (C);
            if Ref /= Null_Node then
               --  Was cloned.
               return Ref;
            else
               --  Was not cloned: reference a node not in the instance.
               return C;
            end if;
         end Get_Clone_Ref;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            if Get_Field_Type (F) = Type_Node then
               Child := Get_Node (N, F);
               case Get_Field_Actual_Attribute (N, F) is
                  when Attr_None =>
                     Fix_Ref_Node (Child);
                  when Attr_Ref | Attr_Forward_Ref =>
                     Set_Node (N, F, Get_Clone_Ref (Child));
                  when Attr_Chain =>
                     Fix_Ref_Chain (Child);
                  when Attr_Chain_Next =>
                     null;
               end case;
            end if;
         end loop;
      end;
   end Fix_Ref_Node;

   procedure Instantiate_Instance (Inst : Node);

   --  Instantiate all instances in MODULE.
   procedure Instantiate_Instances (Chain : Node)
   is
      El : Node;
   begin
      El := Chain;
      while El /= Null_Node loop
         if Get_Kind (El) = N_Module_Instance then
            Instantiate_Instance (El);
         end if;
         El := Get_Chain (El);
      end loop;
   end Instantiate_Instances;

   --  Instantiate instance INST.
   procedure Instantiate_Instance (Inst : Node)
   is
      Module, New_Module : Node;
   begin
      Module := Get_Declaration (Get_Module (Inst));

      if Get_Kind (Module) = N_Foreign_Module
        and then Get_Ports_Chain (Module) = Null_Node
        and then Get_Parameter_Port_Chain (Module) = Null_Node
      then
         Complete_Foreign_Module (Module);

         --  New nodes may have been created.
         Expand_Clone_Table;
      end if;

      pragma Debug (Clear_Clone_Table);
      New_Module := Clone_Node (Module);
      Fix_Ref_Node (New_Module);
      Set_Instance (Inst, New_Module);

      case Get_Kind (Module) is
         when N_Module =>
            --  Recurse.
            Instantiate_Instances (Get_Items_Chain (New_Module));
         when N_Foreign_Module =>
            null;
         when others =>
            raise Internal_Error;
      end case;
   end Instantiate_Instance;

   procedure Instantiate_Design (Chain : Node) is
   begin
      Set_Clone_Table_Size;

      Instantiate_Instances (Chain);

      Free_Clone_Table;
   end Instantiate_Design;

   function Instantiate_Parameters (Params : Node) return Node
   is
      Res : Node;
   begin
      Set_Clone_Table_Size;

      Res := Clone_Chain (Params);
      Fix_Ref_Chain (Res);

      Free_Clone_Table;
      return Res;
   end Instantiate_Parameters;

   procedure Instantiate_Class (Klass : Node; Gen_Class : Node)
   is
      Param, Gen_Param : Node;
   begin
      Set_Clone_Table_Size;

      --  References to GEN_CLASS are cloned as KLASS.
      Clonet.Table (Gen_Class) := Klass;

      --  Handle references to parameters.
      Param := Get_Parameter_Port_Chain (Klass);
      Gen_Param := Get_Parameter_Port_Chain (Gen_Class);
      while Param /= Null_Node loop
         pragma Assert (Gen_Param /= Null_Node);
         Set_Parent (Param, Klass);
         Clonet.Table (Gen_Param) := Param;
         Param := Get_Chain (Param);
         Gen_Param := Get_Chain (Gen_Param);
      end loop;
      pragma Assert (Gen_Param = Null_Node);

      pragma Assert (Get_Kind (Klass) = N_Instantiated_Class);
      declare
         Fields : constant Fields_Array := Get_Fields (N_Class);
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            if F /= Field_Parameter_Port_Chain then
               Clone_Field (Klass, Gen_Class, F);
            end if;
         end loop;
      end;
      Fix_Ref_Node (Klass);

      Free_Clone_Table;
   end Instantiate_Class;

   function Instantiate_Generate_Block
     (Items : Node; Old_Parent : Node; New_Parent : Node) return Node
   is
      Res : Node;
   begin
      Set_Clone_Table_Size;

      Clonet.Table (Old_Parent) := New_Parent;

      Res := Clone_Chain (Items);
      Fix_Ref_Chain (Res);

      Free_Clone_Table;
      return Res;
   end Instantiate_Generate_Block;

end Verilog.Sem_Instances;
