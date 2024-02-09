--  Verilog elaboration for synthesis
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
--  along with GHDL; see the file COPYING.  If not, see:
--  <http://www.gnu.org/licenses>.

with Types; use Types;
with Tables;

with Verilog.Types; use Verilog.Types;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Sem_Utils;
with Verilog.Allocates;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Storages; use Verilog.Storages;

with Netlists; use Netlists;

with Elab.Memtype;

with Synth.Verilog_Values;
with Synth.Verilog_Exprs;

package body Synth.Verilog_Elaboration is

   Global_Scope : Scope_Acc;

   package Scopes is new Tables
     (Table_Component_Type => Scope_Acc,
      Table_Index_Type => Scope_Id,
      Table_Low_Bound => No_Scope_Id + 1,
      Table_Initial => 8);

   procedure Elaborate_Data (Scope : Scope_Acc; N : Node);
   procedure Elaborate_Data_Chain (Scope : Scope_Acc; First : Node);

   procedure Align_Scope_Size (Scope : Scope_Acc; Align : Storage_Index) is
   begin
      Scope.Size := Verilog.Allocates.Align_Storage_Size (Scope.Size, Align);
   end Align_Scope_Size;

--     procedure Allocate_Var (Frame_Scope : Scope_Acc; Var : Node)
--     is
--        Id : constant Obj_Id := Get_Obj_Id (Var);
--  --      Vtype : constant Node := Get_Type (Var);
--     begin
--        Frame_Scope.Frame.Objs (Id) := (Kind => Obj_Value,
--                                        Scope => Frame_Scope,
--                                        Decl => Var,
--                                        Wire => No_Wire_Id);
--     end Allocate_Var;

   procedure Elaborate_Global is
   begin
      pragma Assert (Global_Scope = null);
      Global_Scope := new Scope_Type'(Kind => Scope_Global,
                                      Decl => Null_Node,
                                      Last_Obj => No_Obj_Id,
                                      Frame => null,
                                      Size => 0,
                                      Align => 1);
      Scopes.Append (Global_Scope);
   end Elaborate_Global;

   procedure Elaborate_Type (N : Node) is
   begin
      if Get_Size_Flag (N) then
         return;
      end if;

      case Get_Kind (N) is
         when N_Logic_Type
           | N_Bit_Type =>
            Set_Size_Flag (N, True);
         when N_Log_Packed_Array_Cst
           | N_Bit_Packed_Array_Cst =>
            Set_Size_Flag (N, True);
         when N_Enum_Type
           | N_Event_Type
           | N_Real_Type
           | N_Shortreal_Type
           | N_String_Type =>
            Set_Size_Flag (N, True);
         when N_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (N);
               El_Size : Storage_Index;
               Index_Length : Int32;
            begin
               Elaborate_Type (El_Type);
               El_Size := Verilog.Allocates.Get_Storage_Size (El_Type);
               Set_Stride_Size (N, Tsize_Type (El_Size));
               Index_Length := Verilog.Sem_Utils.Compute_Length (N);
               Set_Type_Size
                 (N, Tsize_Type (El_Size) * Tsize_Type (Index_Length));
               Set_Size_Flag (N, True);
            end;
         when N_Dynamic_Array_Cst =>
            declare
               El_Type : constant Node := Get_Type_Element_Type (N);
               El_Size : Storage_Index;
            begin
               Elaborate_Type (El_Type);
               El_Size := Verilog.Allocates.Get_Storage_Size (El_Type);
               Set_Stride_Size (N, Tsize_Type (El_Size));
               Set_Size_Flag (N, True);
            end;
         when N_Struct_Type =>
            if Get_Scope_Id (N) /= No_Scope_Id then
               --  Already allocated.
               return;
            end if;
            declare
               Sscope : Scope_Acc;
            begin
               Sscope := new Scope_Type'(Kind => Scope_Struct,
                                         Decl => N,
                                         Last_Obj => No_Obj_Id,
                                         Frame => null,
                                         Size => 0,
                                         Align => 1);
               Scopes.Append (Sscope);
               Set_Scope_Id (N, Scopes.Last);
               Elaborate_Data_Chain (Sscope, Get_Members (N));
               --  Align the size of the structure.
               Align_Scope_Size (Sscope, Sscope.Align);
               Set_Type_Size (N, Tsize_Type (Sscope.Size));
            end;
         when N_Class =>
            pragma Assert (Get_Scope_Id (N) /= No_Scope_Id);
            return;
         when N_Queue_Cst =>
            Elaborate_Type (Get_Type_Element_Type (N));
            Set_Size_Flag (N, True);
         when others =>
            Error_Kind ("allocate_type", N);
      end case;
   end Elaborate_Type;

   procedure Elaborate_Object_Type (N : Node)
   is
      Atype : constant Node := Get_Type_Data_Type (N);
   begin
      Elaborate_Type (Atype);
   end Elaborate_Object_Type;

   procedure Allocate_Obj_Id (Frame_Scope : Scope_Acc; Var : Node)
   is
      pragma Assert (Get_Obj_Id (Var) = No_Obj_Id);
   begin
      Frame_Scope.Last_Obj := Frame_Scope.Last_Obj + 1;
      Set_Obj_Id (Var, Frame_Scope.Last_Obj);
   end Allocate_Obj_Id;

   procedure Allocate_Param_Chain (Scope : Scope_Acc; Chain : Node);

   procedure Allocate_Param_Node (Scope : Scope_Acc; N : Node) is
   begin
      case Get_Kind (N) is
         when N_Parameter
            | N_Localparam =>
            Elaborate_Type (Get_Param_Type (N));
            --  Was already set during elaboration.
            pragma Assert (Get_Obj_Id (N) /= No_Obj_Id);
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Allocate_Param_Chain (Scope, Get_Generate_Item_Chain (N));
         when N_Loop_Generate
            | N_If_Generate =>
            null;
         when Nkinds_Net_Port
            | Nkinds_Nets
            | N_Var
            | N_Assign
            | Nkinds_Process
            | Nkinds_Gate
            | N_Genvar
            | N_Module_Instance
            | N_Task
            | N_Function
            | N_Specify =>
            null;
         when others =>
            Error_Kind ("allocate_param_node", N);
      end case;
   end Allocate_Param_Node;

   procedure Allocate_Param_Chain (Scope : Scope_Acc; Chain : Node)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         Allocate_Param_Node (Scope, N);
         N := Get_Chain (N);
      end loop;
   end Allocate_Param_Chain;

   function Allocate_Module_Param (Module : Node) return Scope_Acc
   is
      Scope : Scope_Acc;
   begin
      pragma Assert (Get_Kind (Module) in Nkinds_Module);
      Scope := new Scope_Type'(Kind => Scope_Instance,
                               Decl => Module,
                               Size => 0,
                               Align => 1,
                               Frame => null,
                               Last_Obj => No_Obj_Id,
                               Nbr_Inputs => 0,
                               Nbr_Outputs => 0);

      Scopes.Append (Scope);
      pragma Assert (Get_Scope_Id (Module) = No_Scope_Id);
      Set_Scope_Id (Module, Scopes.Last);

      --  First parameters, so that they could be hashed.
      Allocate_Param_Chain (Scope, Get_Parameter_Port_Chain (Module));
      Allocate_Param_Chain (Scope, Get_Items_Chain (Module));

      return Scope;
   end Allocate_Module_Param;

   package Param_Table is new Tables
     (Table_Component_Type => Verilog_Values.Valtyp,
      Table_Index_Type => Obj_Id,
      Table_Low_Bound => Obj_Id'Succ (No_Obj_Id),
      Table_Initial => 64);

   procedure Elaborate_Param (Inst : Synth_Instance_Acc; Param : Node)
   is
      use Synth.Verilog_Values;
      Param_Type : constant Node := Get_Param_Type (Param);
      Data : constant Data_Ptr := Verilog.Allocates.Get_Parameter_Data (Param);
      N : Net;
      Vt : Valtyp;
   begin
      N := Verilog_Exprs.Memory2net
        (Get_Build (Inst), Elab.Memtype.To_Memory_Ptr (Data), Param_Type);
      Vt := Create_Value_Net (N, Param_Type);
      Param_Table.Append (Vt);
      pragma Assert (Param_Table.Last = Get_Obj_Id (Param));
   end Elaborate_Param;

   pragma Unreferenced (Elaborate_Param);

   procedure Elaborate_Param_Chain
     (Parent : Synth_Instance_Acc; Inst : Synth_Instance_Acc; Chain : Node)
   is
      Item : Node;
   begin
      Item := Chain;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Localparam
              | N_Parameter =>
               null;
               --  Elaborate_Param (Inst, Item);
            when N_Generate_Region
               | N_Array_Generate_Block
               | N_Indexed_Generate_Block
               | N_Generate_Block =>
               Elaborate_Param_Chain
                 (Parent, Inst, Get_Generate_Item_Chain (Item));
            when N_Loop_Generate
               | N_If_Generate =>
               null;
            when Nkinds_Net_Port
               | Nkinds_Nets
               | N_Var
               | N_Assign
               | Nkinds_Process
               | Nkinds_Gate
               | N_Genvar
               | N_Module_Instance
               | N_Task
               | N_Function
               | N_Specify =>
               null;
            when others =>
               Error_Kind ("elaborate_param_chain", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Elaborate_Param_Chain;

   function Elaborate_Sub_Instance_Params
     (Parent_Inst : Synth_Instance_Acc; Module : Node)
     return Synth_Instance_Acc
   is
      Scope : Scope_Acc;
      Inst : Synth_Instance_Acc;
   begin
      Scope := Allocate_Module_Param (Module);
      Allocate_Frame_For_Scope (Scope);

      Inst := Make_Sub_Instance (Parent_Inst, Scope);

      Elaborate_Param_Chain
        (Parent_Inst, Inst, Get_Parameter_Port_Chain (Module));
      Elaborate_Param_Chain
        (Parent_Inst, Inst, Get_Items_Chain (Module));

      return Inst;
   end Elaborate_Sub_Instance_Params;

   procedure Allocate_Chain (Scope : Scope_Acc; Chain : Node);

   procedure Allocate_Node (Scope : Scope_Acc; N : Node) is
   begin
      --  For empty statements.
      if N = Null_Node then
         return;
      end if;

      case Get_Kind (N) is
         when N_Input =>
            if Scope.Kind = Scope_Instance then
               Scope.Nbr_Inputs := Scope.Nbr_Inputs + 1;
            end if;
            Allocate_Obj_Id (Scope, N);
            Set_Obj_Id (Get_Redeclaration (N), Get_Obj_Id (N));
         when N_Output =>
            if Scope.Kind = Scope_Instance then
               Scope.Nbr_Outputs := Scope.Nbr_Outputs + 1;
            end if;
            Allocate_Obj_Id (Scope, N);
            Set_Obj_Id (Get_Redeclaration (N), Get_Obj_Id (N));
         when Nkinds_Nets
            | N_Var =>
            Elaborate_Object_Type (N);
            if not Get_Redeclaration_Flag (N) then
               Allocate_Obj_Id (Scope, N);
            end if;
         when N_Parameter
            | N_Localparam =>
            null;
         when N_Port =>
            null;

         when N_Task
            | N_Function =>
            --  TODO
            null;

         when N_Specify =>
            null;

         when N_Assign
            | N_Noblk_Assign
            | N_Blocking_Assign
            | N_Subroutine_Call_Stmt
            | Nkinds_Gate =>
            null;
         when Nkinds_Process =>
            Allocate_Node (Scope, Get_Statement (N));
         when N_Event_Control =>
            Allocate_Node (Scope, Get_Statement (N));
         when N_If =>
            Allocate_Node (Scope, Get_True_Stmt (N));
            Allocate_Node (Scope, Get_False_Stmt (N));
         when N_For =>
            Allocate_Node (Scope, Get_For_Initialization (N));
            Allocate_Node (Scope, Get_Statement (N));
         when N_Seq_Block =>
            --  TODO: Keep same scope id.
            Allocate_Chain (Scope, Get_Block_Item_Declaration_Chain (N));
            Allocate_Chain (Scope, Get_Statements_Chain (N));
         when Nkinds_Case =>
            Allocate_Chain (Scope, Get_Case_Items (N));
         when N_Case_Item
           | N_Default_Case_Item =>
            Allocate_Node (Scope, Get_Statement (N));
         when N_Genvar =>
            --  Replicated as a localparam
            null;
         when N_Loop_Generate
            | N_If_Generate =>
            --  Replicated
            null;
         when N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            --  The replication
            Allocate_Chain (Scope, Get_Generate_Item_Chain (N));
         when N_Generate_Region =>
            Allocate_Chain (Scope, Get_Generate_Item_Chain (N));

         when N_Module_Instance =>
            null;
         when others =>
            Error_Kind ("allocate_node", N);
      end case;
   end Allocate_Node;

   procedure Allocate_Chain (Scope : Scope_Acc; Chain : Node)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         Allocate_Node (Scope, N);
         N := Get_Chain (N);
      end loop;
   end Allocate_Chain;

   procedure Elaborate_Data_Chain (Scope : Scope_Acc; First : Node)
   is
      Item : Node;
   begin
      Item := First;
      while Item /= Null_Node loop
         Elaborate_Data (Scope, Item);
         Item := Get_Chain (Item);
      end loop;
   end Elaborate_Data_Chain;

   procedure Elaborate_Data (Scope : Scope_Acc; N : Node) is
   begin
      if N = Null_Node then
         --  For null statement.
         return;
      end if;

      case Get_Kind (N) is
         when N_Compilation_Unit =>
            Elaborate_Data_Chain (Scope, Get_Descriptions (N));
         when N_Package =>
            Elaborate_Data_Chain (Scope, Get_Package_Item_Chain (N));
         when N_Module =>
            null;
         when N_Module_Instance =>
            declare
               Inst : constant Node := Get_Instance (N);
            begin
               Elaborate_Data_Chain (Scope, Get_Parameter_Port_Chain (Inst));
               Elaborate_Data_Chain (Scope, Get_Ports_Chain (Inst));
               Elaborate_Data_Chain (Scope, Get_Items_Chain (Inst));
            end;
         when Nkinds_Nets =>
            null;
         when N_Port =>
            null;
         when N_Var
           | N_Return_Var
           | Nkinds_Tf_Port =>
            -- Allocate_Var (Scope, N);
            null;
--         when N_Class =>
--            Allocate_Class (N);
--         when N_Interface_Declaration =>
--            if Get_Parameter_Port_Chain (N) = Null_Node then
--               Allocate_Interface (N);
--            end if;
--         when N_Interface_Instance =>
--            Allocate_Interface_Instance (Global_Scope, N);
         when N_Typedef_Struct =>
            null;
         when N_Parameter
           | N_Localparam =>
            --  Already allocated
            null;
         when Nkinds_Net_Port =>
            --  Allocated by the corresponding net/var.
            null;
         when Nkinds_Gate =>
            null;
         when N_Assign
           | N_Post_Increment =>
            null;
         when N_Initial
           | N_Always =>
            --  No nested processes.
            Elaborate_Data (Scope, Get_Statement (N));
         when N_Seq_Block =>
            Elaborate_Data_Chain (Scope, Get_Block_Item_Declaration_Chain (N));
            Elaborate_Data_Chain (Scope, Get_Statements_Chain (N));
         when N_Delay_Control =>
            Elaborate_Data (Scope, Get_Statement (N));
         when N_Event_Control =>
            Elaborate_Data (Scope, Get_Statement (N));
         when N_Blocking_Assign
           | N_Noblk_Assign
           | N_Subroutine_Call_Stmt
           | N_Return_Stmt
           | N_Trigger =>
            null;
         when N_Simple_Immediate_Assert =>
            Elaborate_Data (Scope, Get_Pass_Stmt (N));
            Elaborate_Data (Scope, Get_Else_Stmt (N));
         when N_If =>
            Elaborate_Data (Scope, Get_True_Stmt (N));
            Elaborate_Data (Scope, Get_False_Stmt (N));
         when N_For =>
            Elaborate_Data (Scope, Get_For_Initialization (N));
            Elaborate_Data (Scope, Get_Step_Assign (N));
            Elaborate_Data (Scope, Get_Statement (N));
         when N_Repeat =>
            --  Allocate_Var (Scope, N);
            Elaborate_Data (Scope, Get_Statement (N));
         when N_While
           | N_Forever =>
            Elaborate_Data (Scope, Get_Statement (N));
         when N_Repeat_Control =>
            Elaborate_Data (Scope, Get_Statement (N));
         when Nkinds_Case =>
            declare
               Item : Node;
               Stmt : Node;
            begin
               Item := Get_Case_Items (N);
               while Item /= Null_Node loop
                  Stmt := Get_Statement (Item);
                  Elaborate_Data (Scope, Stmt);
                  Item := Get_Chain (Item);
               end loop;
            end;
         when N_Typedef =>
            null;
         when N_Function
           | N_Task =>
            declare
               Tf_Scope : Scope_Acc;
--               Offset : Storage_Index;
            begin
               pragma Assert (Get_Scope_Id (N) = No_Scope_Id);
               Tf_Scope := new Scope_Type'(Kind => Scope_Tf,
                                           Decl => N,
                                           Last_Obj => No_Obj_Id,
                                           Frame => null,
                                           Size => 0,
                                           Align => 1);
               Scopes.Append (Tf_Scope);
               Set_Scope_Id (N, Scopes.Last);
               --  Allocate saved frame.
--               Elaborate_Data_Raw (Tf_Scope,
--                                  Offset,
--                                  Frame_Link_Size,
--                                  Frame_Link_Align);
--               pragma Assert (Offset = Frame_Link_Offset);
               if Get_Kind (N) = N_Function then
                  Elaborate_Data (Tf_Scope, Get_Return_Variable (N));
               end if;
               Elaborate_Data_Chain (Tf_Scope, Get_Tf_Ports_Chain (N));
               Elaborate_Data_Chain
                 (Tf_Scope, Get_Tf_Item_Declaration_Chain (N));
               Elaborate_Data_Chain (Tf_Scope, Get_Statements_Chain (N));
            end;
--         when N_Member =>
--            Elaborate_Object_Type (N);
--            Allocate_Member (Scope, N);
         when N_Genvar =>
            --  1800-2017 27.4 Loop generate constructs
            --  The genvar [...], but it does not exist at simulation time.
            null;
         when N_Generate_Region
           | N_Generate_Block
           | N_Array_Generate_Block
           | N_Indexed_Generate_Block =>
            Elaborate_Data_Chain (Scope, Get_Generate_Item_Chain (N));
         when N_Loop_Generate
           | N_If_Generate =>
            --  Original code, that has been instantiated.
            null;
         when N_Package_Import =>
            null;
         when others =>
            Error_Kind ("allocate_data", N);
      end case;
   end Elaborate_Data;

   procedure Elaborate_Sub_Instance_Complete
     (Module : Node; Inst : Synth_Instance_Acc)
   is
      Scope : constant Scope_Acc := Get_Scope (Inst);
   begin
      if Get_Kind (Module) = N_Foreign_Module then
         return;
      end if;

      Allocate_Chain (Scope, Get_Ports_Chain (Module));
      Allocate_Chain (Scope, Get_Items_Chain (Module));

      --  TODO: do not discard the previous frame: reuse old parameters
      --  value.
      Allocate_Frame_For_Scope (Scope);

      --  Data.
      Elaborate_Data (Scope, Module);
   end Elaborate_Sub_Instance_Complete;
end Synth.Verilog_Elaboration;
