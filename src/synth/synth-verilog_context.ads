--  Synthesis context.
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

with Areapools; use Areapools;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Verilog.Types; use Verilog.Types;
with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Storages; use Verilog.Storages;

with Elab.Memtype; use Elab.Memtype;

with Synth.Context; use Synth.Context;
with Synth.Verilog_Environment; use Synth.Verilog_Environment.Env;
with Synth.Verilog_Values; use Synth.Verilog_Values;

package Synth.Verilog_Context is
   --  Values are stored into Synth_Instance, which is parallel to simulation
   --  Block_Instance_Type.

   type Synth_Instance_Type (<>) is limited private;
   type Synth_Instance_Acc is access Synth_Instance_Type;

   type Scope_Kind is (Scope_Global, Scope_Instance, Scope_Tf,
                       Scope_Struct, Scope_Class, Scope_Interface);

   pragma Unreferenced (Scope_Class, Scope_Interface);

   type Frame_Type (<>) is private;

   type Frame_Acc is access Frame_Type;

   type Scope_Type (Kind : Scope_Kind := Scope_Global) is record
      --  Declaration for the scope (Null_Node for compilation_unit, Module,
      --  task, function).
      Decl : Node;

      Size : Storage_Index;
      Align : Storage_Index;
      Last_Obj : Obj_Id;

      --  Current frame for this scope (if any).
      Frame : Frame_Acc;

      case Kind is
         when Scope_Instance =>
            Nbr_Inputs : Port_Nbr;
            Nbr_Outputs : Port_Nbr;
         when others =>
            null;
      end case;
   end record;

   type Scope_Acc is access Scope_Type;

   type Obj_Type is private;

   --  Create the first instance.
   function Make_Root_Instance (Base : Base_Instance_Acc)
                               return Synth_Instance_Acc;

   --  Create and free the corresponding synth instance.
   function Make_Sub_Instance (Parent : Synth_Instance_Acc;
                               Scope : Scope_Acc) return Synth_Instance_Acc;

   procedure Free_Instance (Synth_Inst : in out Synth_Instance_Acc);

   function Get_Sname (Inst : Synth_Instance_Acc) return Sname;
   pragma Inline (Get_Sname);

   --  Modify the current name of the instance for hiearchy changes.
   procedure Push_Sname
     (Inst : Synth_Instance_Acc; Name : Sname; Prev : out Sname);
   procedure Pop_Sname
     (Inst : Synth_Instance_Acc; Prev : Sname);

   function Get_Build (Inst : Synth_Instance_Acc) return Context_Acc;
   pragma Inline (Get_Build);

   function Get_Scope (Inst : Synth_Instance_Acc) return Scope_Acc;
   pragma Inline (Get_Scope);

   procedure Set_Module (Inst : Synth_Instance_Acc; M : Module);

   --  Declare to the builder the current module.  This is needed to
   --  put new gates into the right module.
   procedure Set_Current_Module (Inst : Synth_Instance_Acc; M : Module);

   function Get_Module (Inst : Synth_Instance_Acc) return Module;
   pragma Inline (Get_Module);

   function Get_Top_Module (Inst : Synth_Instance_Acc) return Module;
   pragma Inline (Get_Top_Module);

   function Get_Self_Instance (Inst : Synth_Instance_Acc) return Instance;

   procedure Allocate_Frame_For_Scope (Scope : Scope_Acc);

   procedure Set_Obj_Wire (Inst : Synth_Instance_Acc; N : Node; Wid : Wire_Id);
   procedure Set_Obj_Net (Inst : Synth_Instance_Acc; Obj : Node; N : Net);

   function Get_Obj_Value (Scope : Scope_Acc; N : Node) return Valtyp;
   procedure Set_Obj_Value (Inst : Synth_Instance_Acc; N : Node; Val : Valtyp);
   function Get_Obj_Value (Inst : Synth_Instance_Acc; N : Node) return Valtyp;

   procedure Set_Obj_Port (Scope : Scope_Acc; N : Node; Port : Port_Idx);
   function Get_Obj_Port (Inst : Synth_Instance_Acc; N : Node) return Port_Idx;

   function Allocate_Memory (Inst : Synth_Instance_Acc; Typ : Node)
                            return Memory_Ptr;

   --  Get the value of OBJ.
--   function Get_Value (Syn_Inst : Synth_Instance_Acc; Obj : Node)
--                      return Valtyp;

--   --  Get a net from a scalar/vector value.  This will automatically create
--   --  a net for literals.
--   function Get_Net (Ctxt : Context_Acc; Val : Valtyp) return Net;
private
   type Obj_Kind is (Obj_None,
                     Obj_Port, Obj_Net, Obj_Wire, Obj_Memory,
                     Obj_Static_Var, Obj_Frame_Var, Obj_Member,
                     Obj_Interface_Instance);

   type Obj_Type (Kind : Obj_Kind := Obj_None) is record
      Decl : Node;
      case Kind is
         when Obj_None =>
            null;
         when Obj_Port =>
            Port : Port_Idx;
         when Obj_Net =>
            N : Net;
         when Obj_Memory =>
            Mem : Memory_Ptr;
         when Obj_Wire =>
            Wire : Wire_Id;
         when Obj_Frame_Var
           | Obj_Static_Var =>
            Obj_Off : Storage_Index;
         when Obj_Member =>
            null;
         when Obj_Interface_Instance =>
            Decl_Scope : Scope_Acc;
      end case;
   end record;

   type Obj_Array is array (Obj_Id range <>) of Obj_Type;

   type Frame_Type (Last_Obj : Obj_Id; Size : Storage_Index) is record
      Data : Frame_Ptr;
      Objs : Obj_Array (No_Obj_Id + 1 .. Last_Obj);
   end record;

   type Synth_Instance_Type is limited record
      Base : Base_Instance_Acc;

      --  Name prefix for declarations.
      Name : Sname;

      Scope : Scope_Acc;

      --  Memory pool for temporary expressions.
      Expr_Pool : Areapools.Areapool_Acc;
      Mark : Areapools.Mark_Type;
   end record;
end Synth.Verilog_Context;
