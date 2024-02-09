--  Verilog semantic analyzer (upward references)
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

with Hash; use Hash;
with Tables;
with Dyn_Maps;
with Verilog.Errors; use Verilog.Errors;

package body Verilog.Sem_Upwards is
   type Cell_Index is new Nat32;

   type Name_Cell is record
      Prev : Cell_Index;
      Decl : Node;
   end record;

   package Cells is new Tables
     (Table_Component_Type => Name_Cell,
      Table_Index_Type => Cell_Index,
      Table_Low_Bound => 1,
      Table_Initial => 128);

   Null_Index : constant Cell_Index := 0;

   Hierarchy_Started : Boolean := False;

   --  Create a map from Name_Id to Name_Cell.  Could have used a direct
   --  table instead (indexed by Name_Id), but there shouldn't be many entries
   --  in this map (and also not many requests), so a more compact approach is
   --  preferred.

   --  TODO: lazy build ? (build the map only on request).

   function Map_Hash (Param : Name_Id) return Hash_Value_Type is
   begin
      return Hash_Value_Type (Param);
   end Map_Hash;

   function Map_Build (Param : Name_Id) return Name_Id is
   begin
      return Param;
   end Map_Build;

   function Map_Build_Value (Obj : Name_Id) return Cell_Index
   is
      pragma Unreferenced (Obj);
   begin
      return Null_Index;
   end Map_Build_Value;

   package Name_Maps is new Dyn_Maps
     (Params_Type => Name_Id,
      Object_Type => Name_Id,
      Value_Type  => Cell_Index,
      Hash => Map_Hash,
      Build => Map_Build,
      Build_Value => Map_Build_Value,
      Equal => "=");

   Name_Map : Name_Maps.Instance;

   Current_Scope : Cell_Index;

   procedure Add2 (Id : Name_Id; N : Node)
   is
      Idx : Name_Maps.Index_Type;
      Prev : Cell_Index;
   begin
      Name_Maps.Get_Index (Name_Map, Id, Idx);
      Prev := Name_Maps.Get_Value (Name_Map, Idx);
      Cells.Append ((Prev => Prev, Decl => N));
      Name_Maps.Set_Value (Name_Map, Idx, Cells.Last);
   end Add2;

   procedure Add (N : Node) is
   begin
      Add2 (Get_Identifier (N), N);
   end Add;

   procedure Init (Root : Node)
   is
      N : Node;
   begin
      Name_Maps.Init (Name_Map);
      Current_Scope := Null_Index;
      Add (Root);

      --  Add instances in $root
      N := Get_Items_Chain (Root);
      while N /= Null_Node loop
         Add (N);
         N := Get_Chain (N);
      end loop;

      Hierarchy_Started := True;
   end Init;

   procedure Enter_Scope (N : Node)
   is
      Name : Node;
      Item : Node;
   begin
      --  Save and update current_scope.
      Cells.Append ((Prev => Current_Scope, Decl => N));
      Current_Scope := Cells.Last;

      case Get_Kind (N) is
         when N_Module_Instance
           | N_Program_Instance =>
            Name := Get_Module (N);
            Add2 (Get_Identifier (Name), N);
            Item := Get_Items_Chain (Get_Instance (N));
         when N_Interface_Instance =>
            Name := Get_Interface_Name (N);
            Add2 (Get_Identifier (Name), N);
            Item := Get_Items_Chain (Get_Instance_Ref (N));
         when others =>
            Error_Kind ("enter_scope", N);
      end case;
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Module_Instance
              | N_Interface_Instance =>
               Add (Item);
            when N_Generate_Region =>
               --  TODO: it is ok ?  Is it a scope ?
               null;
            when N_Var
              | Nkinds_Process
              | Nkinds_Nets
              | Nkinds_Net_Port
              | Nkinds_Gate
              | Nkinds_Forward_Typedef
              | Nkinds_Tf
              | N_Typedef
              | N_Assign
              | N_Parameter
              | N_Localparam
              | N_Genvar
              | N_Loop_Generate
              | N_If_Generate
              | N_Assert_Property
              | N_Class
              | N_Package_Import
              | N_Analog
              | N_Specify =>
               null;
            when others =>
               Error_Kind ("enter_scope(2)", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Enter_Scope;

   procedure Revert_Until_Last (First : Cell_Index) is
   begin
      for I in reverse First .. Cells.Last loop
         declare
            C : constant Name_Cell := Cells.Table (I);
            Idx : Name_Maps.Index_Type;
         begin
            Name_Maps.Get_Index (Name_Map, Get_Identifier (C.Decl), Idx);
            Name_Maps.Set_Value (Name_Map, Idx, C.Prev);
            Cells.Decrement_Last;
         end;
      end loop;
   end Revert_Until_Last;

   procedure Revert_By_Name (N : Node)
   is
      C : constant Name_Cell := Cells.Table (Cells.Last);
      Idx : Name_Maps.Index_Type;
   begin
      Name_Maps.Get_Index (Name_Map, Get_Identifier (N), Idx);
      Name_Maps.Set_Value (Name_Map, Idx, C.Prev);
      Cells.Decrement_Last;
   end Revert_By_Name;

   procedure Leave_Scope
   is
      Prev_Scope : constant Name_Cell := Cells.Table (Current_Scope);
   begin
      case Get_Kind (Prev_Scope.Decl) is
         when N_Module_Instance
           | N_Program_Instance =>
            Revert_Until_Last (Current_Scope + 2);
            --  Revert module name.
            Revert_By_Name (Prev_Scope.Decl);
         when N_Interface_Instance =>
            Revert_Until_Last (Current_Scope + 2);
            --  Revert interface name.
            Revert_By_Name (Prev_Scope.Decl);
         when others =>
            Error_Kind ("leave_scope", Prev_Scope.Decl);
      end case;
      pragma Assert (Cells.Last = Current_Scope);
      Cells.Decrement_Last;
      Current_Scope := Prev_Scope.Prev;
   end Leave_Scope;

   function Find_Scope (Id : Name_Id) return Node
   is
      use Name_Maps;
      Idx : Index_Type;
      Cidx : Cell_Index;
   begin
      if not Hierarchy_Started then
         return Null_Node;
      end if;

      Idx := Get_Index_Soft (Name_Map, Id);
      if Idx = No_Index then
         return Null_Node;
      end if;
      Cidx := Get_Value (Name_Map, Idx);
      return Cells.Table (Cidx).Decl;
   end Find_Scope;
end Verilog.Sem_Upwards;
