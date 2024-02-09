--  Instantiation synthesis for verilog
--  Copyright (C) 2023 Tristan Gingold
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

with Types_Utils;
with Hash; use Hash;
with Name_Table;
with Std_Names;
with Interning;
with Errorout; use Errorout;
with Libraries;

with Netlists; use Netlists;
with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;
with Netlists.Builders; use Netlists.Builders;
with Netlists.Locations; use Netlists.Locations;

with Verilog.Nodes; use Verilog.Nodes;
with Verilog.Nutils; use Verilog.Nutils;
with Verilog.Elaborate; use Verilog.Elaborate;
with Verilog.Sem;
with Verilog.Errors; use Verilog.Errors;
with Verilog.Disp_Verilog;
with Verilog.Vpi;
with Verilog.Sem_Instances;
with Verilog.Sem_Names;
with Verilog.Bignums;
with Verilog.Storages;
with Verilog.Allocates;

with Vhdl.Nodes;
with Vhdl.Errors;
with Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values;
with Elab.Vhdl_Errors;

with Synthesis;
with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Expr;

with Synth.Verilog_Sources; use Synth.Verilog_Sources;
with Synth.Verilog_Environment; use Synth.Verilog_Environment.Env;
with Synth.Verilog_Context; use Synth.Verilog_Context;
with Synth.Verilog_Values; use Synth.Verilog_Values;
with Synth.Verilog_Stmts; use Synth.Verilog_Stmts;
with Synth.Verilog_Exprs; use Synth.Verilog_Exprs;
with Synth.Verilog_Elaboration; use Synth.Verilog_Elaboration;

package body Synth.Verilog_Insts is
   function Build_Module (M : Node; Inst : Synth_Instance_Acc)
                         return Module;

   type Inst_Params is record
      --  Module declaration (well, the instantiated one).
      M : Node;
      --  Corresponding instance (containing the scope)
      Inst : Synth_Instance_Acc;
   end record;

   type Inst_Object is record
      --  The module and its full scope.
      Decl : Node;
      Inst : Synth_Instance_Acc;
      --  The module in the netlist.
      M : Module;
   end record;

   function Hash (Params : Inst_Params) return Hash_Value_Type
   is
      Res : Hash_Value_Type;
   begin
      Res := Hash_Value_Type (Get_Identifier (Params.M));
      --  TODO: parameters
      return Res;
   end Hash;

   function Equal (Ln : Node; Ls : Scope_Acc; Rn : Node; Rs : Scope_Acc)
                  return Boolean
   is
      pragma Unreferenced (Ls, Rs);
      use Verilog.Bignums;
      use Verilog.Storages;
      use Verilog.Allocates;
      Lt : constant Node := Get_Param_Type (Ln);
      Rt : constant Node := Get_Param_Type (Rn);
      Lv, Rv : Data_Ptr;
   begin
      --  First: type
      if Lt /= Rt then
         return False;
      end if;

      --  Second: value
      Lv := Get_Parameter_Data (Ln);
      Rv := Get_Parameter_Data (Rn);
      case Get_Kind (Lt) is
         when N_Log_Packed_Array_Cst =>
            return Compute_Log_Eq
              (To_Logvec_Ptr (Lv), To_Logvec_Ptr (Rv),
               Get_Type_Width (Lt), True) = V_1;
         when others =>
            raise Internal_Error;
      end case;
   end Equal;

   function Equal_Chain (Lc : Node; Ls : Scope_Acc; Rc : Node; Rs : Scope_Acc)
                        return Boolean
   is
      Ln, Rn : Node;
   begin
      Ln := Lc;
      Rn := Rc;
      while Ln /= Null_Node loop
         pragma Assert (Rn /= Null_Node);
         pragma assert (Get_Kind (Ln) = Get_Kind (Rn));
         case Get_Kind (Ln) is
            when N_Parameter
              | N_Localparam =>
               if not Equal (Ln, Ls, Rn, Rs) then
                  return False;
               end if;
            when others =>
               null;
         end case;
         Ln := Get_Chain (Ln);
         Rn := Get_Chain (Rn);
      end loop;
      pragma Assert (Rn = Null_Node);
      return True;
   end Equal_Chain;

   function Equal (Obj : Inst_Object; Params : Inst_Params) return Boolean is
   begin
      if Get_Identifier (Obj.Decl) /= Get_Identifier (Params.M) then
         return False;
      end if;
      if not Equal_Chain (Get_Parameter_Port_Chain (Obj.Decl),
                          Get_Scope (Obj.Inst),
                          Get_Parameter_Port_Chain (Params.M),
                          Get_Scope (Params.Inst))
      then
         return False;
      end if;
      if not Equal_Chain (Get_Items_Chain (Obj.Decl), Get_Scope (Obj.Inst),
                          Get_Items_Chain (Params.M), Get_Scope (Params.Inst))
      then
         return False;
      end if;

      return True;
   end Equal;

   function Build (Params : Inst_Params) return Inst_Object
   is
      Res : Module;
   begin
      --  FIXME: is it needed ?
      Elaborate_Sub_Instance_Complete (Params.M, Params.Inst);

      Res := Build_Module (Params.M, Params.Inst);

      return Inst_Object'(Decl => Params.M,
                          Inst => Params.Inst,
                          M => Res);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   function Synth_Foreign_Module_Instance
     (Sub_Inst : Synth_Instance_Acc; Foreign_Module : Node) return Module
   is
      pragma Unreferenced (Sub_Inst);
      use Vhdl.Nodes;
      Vhd_Unit : constant Vhdl_Node :=
        Vhdl_Node (Get_Foreign_Node (Foreign_Module));
      Vhd_Ent : constant Vhdl_Node := Get_Library_Unit (Vhd_Unit);
      Arch, Config : Vhdl_Node;
      Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
   begin
      if Get_Generic_Chain (Vhd_Ent) /= Null_Vhdl_Node then
         raise Internal_Error;
      end if;

      --  Create VHDL instance (Cf elab.vhdl_insts.elab_top_unit).
      Arch := Libraries.Get_Latest_Architecture (Vhd_Ent);
      if Arch = Null_Vhdl_Node then
         declare
            use Vhdl.Errors;
         begin
            Elab.Vhdl_Errors.Error_Msg_Elab
              (Vhd_Inst, Vhd_Ent, "no architecture for %n", +Vhd_Ent);
         end;
         return No_Module;
      end if;
      Config := Get_Default_Configuration_Declaration (Arch);
      Config := Get_Library_Unit (Config);

      Vhd_Inst := Elab.Vhdl_Context.Make_Elab_Instance
        (Elab.Vhdl_Context.Root_Instance,
         Null_Vhdl_Node, Arch, Get_Block_Configuration (Config));

      --  Populate it (elab dependencies, elab generic and port types)
      pragma Unreferenced (Vhd_Inst);

      --  Intern it.
      raise Internal_Error;
      return No_Module;
   end Synth_Foreign_Module_Instance;

   procedure Synth_Module_Instance (Parent_Inst : Synth_Instance_Acc; N : Node)
   is
      Ctxt : constant Context_Acc := Get_Build (Parent_Inst);
      Inst_Module : constant Node := Get_Instance (N);
      Obj : Inst_Object;
      M : Module;
      Inst : Instance;
      Sub_Inst : Synth_Instance_Acc;
      Conn : Node;
      Port : Node;
      Expr : Node;
      V : Valtyp;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      --  Allocate obj_id for parameters/localparam and compute their
      --   expressions.
      Sub_Inst := Elaborate_Sub_Instance_Params (Parent_Inst, Inst_Module);

      if Get_Kind (Inst_Module) = N_Foreign_Module then
         --  Create the vhdl instance,
         --  with the vhdl value of the generics,
         --  and vhdl types of generics and ports.
         M := Synth_Foreign_Module_Instance (Sub_Inst, Inst_Module);
      else
         --  Check for existing module with same name and same parameters
         --    or create a new module and put it in a list.
         Obj := Insts_Interning.Get (Inst_Params'(M => Inst_Module,
                                                  Inst => Sub_Inst));
         M := Obj.M;
      end if;

      --  Create new instance, connect.
      Inst := New_Instance
        (Get_Module (Parent_Inst), M,
         New_Sname_User (Get_Identifier (N), Get_Sname (Parent_Inst)));
      Set_Location (Inst, Get_Location (N));

      Push_Phi;

      Conn := Get_Connections (N);

      Nbr_Inputs := 0;
      Nbr_Outputs := 0;

      --  TODO: assume same order
      --  TODO: non-ANSI style
      while Conn /= Null_Node loop
         pragma Assert (Get_Kind (Conn) = N_Port_Connection);
         Port := Get_Port (Conn);
         if Get_Kind (Port) = N_Port then
            Port := Get_Expression (Port);
            pragma Assert (Get_Kind (Port) = N_Name);
            Port := Get_Declaration (Port);
         end if;

         Expr := Get_Expression (Conn);

         case Get_Kind (Port) is
            when N_Input =>
               V := Synth_Expression (Parent_Inst, Expr);
               Connect (Get_Input (Inst, Nbr_Inputs), Get_Net (Ctxt, V));
               Nbr_Inputs := Nbr_Inputs + 1;
            when N_Output =>
               if Expr /= Null_Node then
                  --  Discard unconnected output.
                  declare
                     Targ : Valtyp;
                     Off : Name_Offsets;
                     O : Net;
                     Doff : Net;
                  begin
                     O := Get_Output (Inst, Nbr_Outputs);
                     Synth_Name (Parent_Inst, Expr, Targ, Doff, Off);
                     if Doff /= No_Net then
                        raise Internal_Error;
                     end if;
                     Phi_Assign_Net (Ctxt, Targ.W, O, Off.Net_Off);
                  end;
               end if;
               Nbr_Outputs := Nbr_Outputs + 1;
            when others =>
               Error_Kind ("synth_module_instance", Port);
         end case;
         Conn := Get_Chain (Conn);
      end loop;

      Pop_And_Merge_Phi (Get_Build (Sub_Inst), Get_Location (N));
   end Synth_Module_Instance;


   function Get_Module_Ports (M : Node) return Node is
   begin
      if Get_Ansi_Port_Flag (M) then
         return Get_Ports_Chain (M);
      else
         return Get_Items_Chain (M);
      end if;
   end Get_Module_Ports;

   function Is_Black_Box (M : Node) return Boolean
   is
      Attr : Node;
   begin
      Attr := Get_Attributes_Chain (M);
      while Attr /= Null_Node loop
         if Get_Identifier (Attr) = Std_Names.Name_Syn_Black_Box
           and then Get_Attribute_Item (Attr) = M
         then
            --  TODO: check expression ?
            return True;
         end if;
         Attr := Get_Chain (Attr);
      end loop;
      return False;
   end Is_Black_Box;

   procedure Convert_Attribute (Attr : Node;
                                Ptype : out Param_Type;
                                Pv : out Pval)
   is
      Expr : constant Node := Get_Expression (Attr);
   begin
      if Expr = Null_Node then
         --  Default value is 1.
         Ptype := Param_Uns32;
         --  TODO: create the value only once ?
         Pv := Create_Pval2 (1);
         Write_Pval (Pv, 0, (Val => 1, Zx => 0));
      else
         raise Internal_Error;
      end if;
   end Convert_Attribute;

   function Build_Module (M : Node; Inst : Synth_Instance_Acc)
                         return Module
   is
      Ports : constant Node := Get_Module_Ports (M);
      Scope : constant Scope_Acc := Get_Scope (Inst);
      Name : Sname;
      Res : Module;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Self : Instance;
   begin
      Nbr_Inputs := Scope.Nbr_Inputs;
      Nbr_Outputs := Scope.Nbr_Outputs;

      --  Create the module.
      Name := New_Sname_User (Get_Identifier (M), No_Sname);
      Res := New_User_Module (Get_Top_Module (Inst), Name, Id_User_None,
                              Nbr_Inputs, Nbr_Outputs, 0);

      --  Create the ports.
      declare
         Inports : Port_Desc_Array (1 .. Nbr_Inputs);
         Outports : Port_Desc_Array (1 .. Nbr_Outputs);
         Port : Node;
         Decl : Node;
      begin
         Nbr_Inputs := 0;
         Nbr_Outputs := 0;

         Port := Ports;
         while Port /= Null_Node loop
            Decl := Port;
            case Get_Kind (Decl) is
               when N_Input =>
                  Set_Obj_Port (Scope, Decl, Nbr_Inputs);
                  Nbr_Inputs := Nbr_Inputs + 1;
                  Inports (Nbr_Inputs) :=
                    (Name => New_Sname_User (Get_Identifier (Decl), No_Sname),
                     Dir => Port_In,
                     W => Get_Type_Bitwidth (Get_Type_Data_Type (Decl)));
               when N_Output =>
                  Set_Obj_Port (Scope, Decl, Nbr_Outputs);
                  Nbr_Outputs := Nbr_Outputs + 1;
                  Outports (Nbr_Outputs) :=
                    (Name => New_Sname_User (Get_Identifier (Decl), No_Sname),
                    Dir => Port_Out,
                     W => Get_Type_Bitwidth (Get_Type_Data_Type (Decl)));
               when N_Wire
                  | N_Wire_Direct
                  | N_Var
                  | N_Assign
                  | N_Module_Instance
                  | N_Generate_Region
                  | N_Always
                  | N_Parameter =>
                  --  Skip non-port items.
                  null;
               when others =>
                  Error_Kind ("build_module", Port);
            end case;
            Port := Get_Chain (Port);
         end loop;

         pragma Assert (Nbr_Inputs = Inports'Last);
         pragma Assert (Nbr_Outputs = Outports'Last);
         Set_Ports_Desc (Res, Inports, Outports);
      end;

      --  Apply attributes.
      declare
         Attr : Node;
         Attr_Item : Node;
         K : Nkind;
         Ptype : Param_Type;
         Pv : Pval;
         Idx : Port_Idx;
         Id : Name_Id;
      begin
         Attr := Get_Attributes_Chain (M);
         while Attr /= Null_Node loop
            Attr_Item := Get_Attribute_Item (Attr);
            K := Get_Kind (Attr_Item);
            if K in Nkinds_Net_Port then
               Convert_Attribute (Attr, Ptype, Pv);
               Idx := Get_Obj_Port (Inst, Attr_Item);
               Id := Get_Identifier (Attr);
               if K = N_Input then
                  Set_Input_Port_Attribute (Res, Idx, Id, Ptype, Pv);
               else
                  Set_Output_Port_Attribute (Res, Idx, Id, Ptype, Pv);
               end if;
            end if;
            Attr := Get_Chain (Attr);
         end loop;
      end;

      if not Is_Black_Box (M) then
         Self := Create_Self_Instance (Res);
         pragma Unreferenced (Self);
      end if;

      return Res;
   end Build_Module;

   generic
      with procedure Call_Item (Inst : Synth_Instance_Acc; N : Node);
   procedure Generic_Call_Items_Chain (Inst : Synth_Instance_Acc;
                                       Chain : Node);

   procedure Generic_Call_Items_Chain (Inst : Synth_Instance_Acc; Chain : Node)
   is
      N : Node;
   begin
      N := Chain;
      while N /= Null_Node loop
         Call_Item (Inst, N);
         N := Get_Chain (N);
      end loop;
   end Generic_Call_Items_Chain;

   procedure Synth_Decl_Item (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Initial_Item (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Always_Item (Inst : Synth_Instance_Acc; N : Node);
   procedure Synth_Finalize_Item (Inst : Synth_Instance_Acc; N : Node);

   procedure Synth_Decl_Items_Chain is
      new Generic_Call_Items_Chain (Synth_Decl_Item);

   procedure Synth_Initial_Items_Chain is
      new Generic_Call_Items_Chain (Synth_Initial_Item);

   procedure Synth_Always_Items_Chain is
      new Generic_Call_Items_Chain (Synth_Always_Item);

   procedure Synth_Finalize_Items_Chain is
      new Generic_Call_Items_Chain (Synth_Finalize_Item);

   procedure Synth_Decl_Item (Inst : Synth_Instance_Acc; N : Node)
   is
      Kind : constant Nkind := Get_Kind (N);
   begin
      case Kind is
         when N_Input =>
            --  The net already exists.
            declare
               Self : constant Instance := Get_Self_Instance (Inst);
               Port : constant Port_Idx := Get_Obj_Port (Inst, N);
               Inp : Net;
            begin
               Inp := Get_Output (Self, Port);
               Set_Obj_Net (Inst, N, Inp);
            end;
         when N_Output =>
            --  Add an output gate/wire
            declare
               Self : constant Instance := Get_Self_Instance (Inst);
               Port : constant Port_Idx := Get_Obj_Port (Inst, N);
               Wid : Wire_Id;
               O : Net;
            begin
               O := Builders.Build_Output
                 (Get_Build (Inst),
                  Get_Type_Bitwidth (Get_Type_Data_Type (N)));
               Set_Location (O, N);
               Connect (Get_Input (Self, Port), O);
               Wid := Alloc_Wire (Wire_Unset, N);
               Set_Wire_Gate (Wid, O);
               Set_Obj_Wire (Inst, N, Wid);
            end;
         when N_Wire
            | N_Wire_Direct
            | N_Var =>
            if not Get_Redeclaration_Flag (N) then
               --  Create the wire.
               declare
                  Name : Sname;
                  Wid : Wire_Id;
                  Nt : Net;
               begin
                  Name := New_Sname_User (Get_Identifier (N),
                                          Get_Sname (Inst));
                  Nt := Builders.Build_Signal
                    (Get_Build (Inst), Name,
                     Get_Type_Bitwidth (Get_Type_Data_Type (N)));
                  Set_Location (Nt, N);
                  if Kind = N_Var then
                     Wid := Alloc_Wire (Wire_Unset, N);
                  else
                     Wid := Alloc_Wire (Wire_Variable, N);
                  end if;
                  Set_Wire_Gate (Wid, Nt);
                  Set_Obj_Wire (Inst, N, Wid);
               end;
            end if;
            if Get_Expression (N) /= Null_Node then
               if Kind = N_Var then
                  --  TODO: var initial value
                  raise Internal_Error;
               else
                  Synth_Net_Init (Inst, N);
               end if;
            end if;
         when N_Parameter
            | N_Localparam =>
            null;
         when N_Port =>
            null;
         when N_Assign
            | N_Initial
            | N_Always
            | N_Always_Ff
            | N_Always_Comb
            | Nkinds_Gate =>
            null;
         when N_Genvar
            | N_Loop_Generate
            | N_If_Generate =>
            --  Replicated
            null;
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Synth_Decl_Items_Chain (Inst, Get_Generate_Item_Chain (N));
         when N_Module_Instance =>
            null;

         when N_Task
            | N_Function =>
            null;

         when N_Specify =>
            null;

         when others =>
            Error_Kind ("synth_decl_item", N);
      end case;
   end Synth_Decl_Item;

   procedure Synth_Initial_Item (Inst : Synth_Instance_Acc; N : Node)
   is
      Kind : constant Nkind := Get_Kind (N);
   begin
      case Kind is
         when N_Input
            | N_Output
            | N_Wire
            | N_Wire_Direct
            | N_Var =>
            null;
         when N_Parameter
            | N_Localparam =>
            null;
         when N_Port =>
            null;
         when N_Assign
            | Nkinds_Gate =>
            null;
         when N_Initial =>
            Synth_Initial (Inst, N);
         when N_Always
           | N_Always_Ff =>
            null;
         when N_Always_Comb =>
            null;
         when N_Genvar
            | N_Loop_Generate
            | N_If_Generate =>
            --  Replicated
            null;
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Synth_Initial_Items_Chain (Inst, Get_Generate_Item_Chain (N));
         when N_Module_Instance =>
            null;

         when N_Task
            | N_Function =>
            null;

         when N_Specify =>
            null;
         when others =>
            Error_Kind ("synth_initial_item", N);
      end case;
   end Synth_Initial_Item;

   procedure Synth_Always_Item (Inst : Synth_Instance_Acc; N : Node)
   is
      Kind : constant Nkind := Get_Kind (N);
   begin
      case Kind is
         when N_Input
            | N_Output
            | N_Wire
            | N_Wire_Direct
            | N_Var =>
            null;
         when N_Parameter
            | N_Localparam =>
            null;
         when N_Port =>
            null;
         when N_Assign =>
            Synth_Continuous_Assign (Inst, N);
         when Nkinds_Gate =>
            Synth_Gate (Inst, N);
         when N_Initial =>
            null;
         when N_Always
            | N_Always_Ff =>
            --  TODO: check FF and comb.
            Synth_Always (Inst, N);
         when N_Always_Comb =>
            Synth_Always_Comb (Inst, N);
         when N_Genvar
            | N_Loop_Generate
            | N_If_Generate =>
            --  Replicated
            null;
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Synth_Always_Items_Chain (Inst, Get_Generate_Item_Chain (N));
         when N_Module_Instance =>
            Synth_Module_Instance (Inst, N);

         when N_Task
            | N_Function =>
            null;

         when N_Specify =>
            null;

         when others =>
            Error_Kind ("synth_always_item", N);
      end case;
   end Synth_Always_Item;

   procedure Synth_Finalize_Wire (Inst : Synth_Instance_Acc; N : Node)
   is
      use Netlists.Gates;
      Obj : constant Valtyp := Get_Obj_Value (Inst, N);
      Gate_Net : Net;
      Gate : Instance;
      Drv : Net;
      Def_Val : Net;
   begin
      if Obj.Kind /= Value_Wire then
         --  Inputs are nets.
         return;
      end if;

      Finalize_Assignment (Get_Build (Inst), Obj.W);

      --  FIXME: share code with synth.vhdl_decls.finalize_signal.
      Gate_Net := Get_Wire_Gate (Obj.W);
      Gate := Get_Net_Parent (Gate_Net);
      case Get_Id (Gate) is
         when Id_Signal
            | Id_Output
            | Id_Inout =>
            Drv := Get_Input_Net (Gate, 0);
            Def_Val := No_Net;
         when Id_Isignal
            | Id_Ioutput
            | Id_Iinout =>
            Drv := Get_Input_Net (Gate, 0);
            Def_Val := Get_Input_Net (Gate, 1);
         when others =>
            --  Todo: output ?
            raise Internal_Error;
      end case;
      if Drv = No_Net then
         --  Undriven signals.
         if Is_Connected (Get_Output (Gate, 0)) then
            --  No warning if the signal is not used.
            --  TODO: maybe simply remove it.
            if Def_Val = No_Net then
               Warning_Msg_Synth (+N, "%n is never assigned", (1 => +N));
            end if;
         end if;
         if Def_Val = No_Net then
            Def_Val := Build_Const_X (Get_Build (Inst), Get_Width (Gate_Net));
         end if;
         Connect (Get_Input (Gate, 0), Def_Val);
      end if;

      Free_Wire (Obj.W);
   end Synth_Finalize_Wire;

   procedure Synth_Finalize_Item (Inst : Synth_Instance_Acc; N : Node) is
   begin
      if N = Null_Node then
         return;
      end if;

      case Get_Kind (N) is
         when N_Input =>
            null;
         when N_Output =>
            null;
         when N_Wire
           | N_Wire_Direct
           | N_Var =>
            Synth_Finalize_Wire (Inst, N);
         when N_Parameter
            | N_Localparam =>
            --  TODO: free mem ?
            null;
         when N_Assign
            | N_Noblk_Assign
            | N_Blocking_Assign =>
            null;
         when N_Always
            | N_Always_Comb
            | N_Always_Ff
            | N_Event_Control =>
            Synth_Finalize_Item (Inst, Get_Statement (N));
         when N_If =>
            Synth_Finalize_Item (Inst, Get_True_Stmt (N));
            Synth_Finalize_Item (Inst, Get_False_Stmt (N));
         when N_Seq_Block =>
            Synth_Finalize_Items_Chain (Inst, Get_Statements_Chain (N));
         when N_Case =>
            Synth_Finalize_Items_Chain (Inst, Get_Case_Items (N));
         when N_Case_Item
            | N_Default_Case_Item =>
            Synth_Finalize_Items_Chain (Inst, Get_Statement (N));
         when N_Genvar
            | N_Loop_Generate
            | N_If_Generate =>
            null;
         when N_Generate_Region
            | N_Array_Generate_Block
            | N_Indexed_Generate_Block
            | N_Generate_Block =>
            Synth_Finalize_Items_Chain (Inst, Get_Generate_Item_Chain (N));

         when N_Module_Instance =>
            null;

         when N_Initial =>
            null;
         when N_Task =>
            null;
         when N_Subroutine_Call_Stmt =>
            null;
         when Nkinds_Gate =>
            null;
         when N_Specify =>
            null;
         when others =>
            Error_Kind ("synth_finalize_item", N);
      end case;
   end Synth_Finalize_Item;

   procedure Synth_Module (Inst : Synth_Instance_Acc; N : Node) is
   begin
      Synth_Decl_Items_Chain (Inst, Get_Parameter_Port_Chain (N));
      Synth_Decl_Items_Chain (Inst, Get_Ports_Chain (N));

      --  First declarations.
      Synth_Decl_Items_Chain (Inst, Get_Items_Chain (N));

      --  Then initial processes.
      Synth_Initial_Items_Chain (Inst, Get_Items_Chain (N));

      --  And always processes.
      Synth_Always_Items_Chain (Inst, Get_Items_Chain (N));

      Synth_Finalize_Items_Chain (Inst, Get_Items_Chain (N));
   end Synth_Module;

   procedure Initialize is
   begin
      Insts_Interning.Init;
      Verilog.Vpi.Blocking_Assign := Synth_Blocking_Assign_Vpi'Access;
   end Initialize;

   --  Create the declaration of the top entity.
   procedure Synth_Top_Module (Base : Base_Instance_Acc;
                               Unit : Int32;
                               Encoding : Name_Encoding)
   is
      pragma Unreferenced (Encoding);
      Module_Decl : constant Node := Node (Unit);
      Module : Node;
      Inst : Synth_Instance_Acc;
      Root_Inst : Synth_Instance_Acc;
      Root : Node;
      Module_Inst : Node;
      Obj : Inst_Object;
   begin
      pragma Assert (Module_Decl /= Null_Node);

      --  Elaborate module (create root module).
      Root := Elab_Design (Module_Decl);

      if Errorout.Nbr_Errors > 0 then
         return;
      end if;

      Module_Inst := Get_Items_Chain (Root);
      Module := Get_Instance (Module_Inst);

      if Flag_Debug_Elaborate then
         Verilog.Disp_Verilog.Disp_Module (Root);
      end if;

      --  Create global scope, and scope for the top module.
      Synth.Verilog_Elaboration.Elaborate_Global;

      Root_Inst := Make_Root_Instance (Base);

      Inst := Elaborate_Sub_Instance_Params (Root_Inst, Module);

      Obj := Insts_Interning.Get (Inst_Params'(M => Module,
                                               Inst => Inst));
      pragma Unreferenced (Obj);
   end Synth_Top_Module;

   procedure Synth_Instance (Syn_Inst : Synth_Instance_Acc;
                             Decl : Node;
                             M : Module)
   is
      Self_Inst : constant Instance := Get_Self_Instance (M);
   begin
      if Flag_Verbose then
         Errors.Info_Msg_Synth
           (+Decl, "synthesizing %i", (1 => +Get_Identifier (Decl)));
      end if;

      if Self_Inst = No_Instance then
         --  A black box.
         return;
      end if;

      Set_Location (Self_Inst, Get_Location (Decl));

      Set_Current_Module (Syn_Inst, M);

      Synth_Module (Syn_Inst, Decl);

      Finalize_Wires;

      Synthesis.Instance_Passes (Get_Build (Syn_Inst), M);
   end Synth_Instance;

   procedure Synth_All_Instances
   is
      use Insts_Interning;
      Idx : Index_Type;
      Obj : Inst_Object;
   begin
      Idx := First_Index;
      while Idx <= Last_Index loop
         Obj := Get_By_Index (Idx);

         Synth_Instance (Obj.Inst, Obj.Decl, Obj.M);

         Idx := Idx + 1;
      end loop;
   end Synth_All_Instances;

   procedure Elab_Foreign_Parameter
     (Param : Node;
      Syn_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Act_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Assoc : Vhdl_Node)
   is
      pragma Unreferenced (Syn_Inst);
      use Vhdl.Nodes;
      use Elab.Vhdl_Objtypes;
      use Types_Utils;
      Val : Elab.Vhdl_Values.Valtyp;
   begin
      --  If no assoc or open association: check the parameter has a
      --   default value.
      if Assoc = Null_Vhdl_Node
        or else Get_Kind (Assoc) = Iir_Kind_Association_Element_Open
      then
         if Get_Expression (Param) = Null_Vlg_Node then
            raise Internal_Error;
         end if;

         return;
      end if;

      --  If assoc (in whole, skip individual), get the valtyp.
      --   check the parameter has no type, create a const from the valtyp.
      pragma Assert (Get_Whole_Association_Flag (Assoc));
      if Get_Param_Type (Param) /= Null_Vlg_Node then
         raise Internal_Error;
      end if;
      Val := Synth.Vhdl_Expr.Synth_Expression (Act_Inst, Get_Actual (Assoc));
      case Val.Typ.Kind is
         when Type_Discrete =>
            declare
               V : constant Uns64 :=
                 To_Uns64 (Elab.Vhdl_Values.Read_Discrete (Val));
               Res : Vlg_Node;
               Ve : Vlg_Node;
            begin
               Res := Create_Node (N_Number);
               Set_Location (Res, Get_Location (Assoc));
               Set_Number_Lo_Val (Res, Uns32 (V and 16#ffff_ffff#));
               Set_Number_Hi_Val (Res, Uns32 (Shift_Right (V, 32)));

               Ve := Create_Node (N_Parameter_Value_Expr);
               Set_Location (Ve, Get_Location (Res));
               Set_Identifier (Ve, Get_Identifier (Param));
               Set_Expression (Ve, Res);
               Set_Parameter (Ve, Param);
               Set_Override_Stmt (Param, Ve);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Elab_Foreign_Parameter;

   procedure Elab_Foreign_Parameters
     (Item_Chain : Node;
      Syn_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Act_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Assocs : in out Vhdl_Node)
   is
      use Vhdl.Nodes;
      Item : Vlg_Node;
   begin
      Item := Item_Chain;
      while Item /= Null_Vlg_Node loop
         case Get_Kind (Item) is
            when N_Parameter =>
               Elab_Foreign_Parameter (Item, Syn_Inst, Act_Inst, Assocs);
               if Assocs /= Null_Iir then
                  Assocs := Get_Chain (Assocs);
               end if;
            when others =>
               null;
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Elab_Foreign_Parameters;

   procedure Elab_Foreign_Instance
     (Syn_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Comp_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Bind : Vhdl_Node;
      N : Vhdl_Node)
   is
      use Vhdl.Nodes;
      Module : constant Vlg_Node := Vlg_Node (Get_Foreign_Node (N));
      Gen_Assoc : Vhdl_Node;

      Root_Module : Vlg_Node;
      Inst : Vlg_Node;
      Mod_Inst : Vlg_Node;
   begin
      Root_Module := Create_Root_Module;

      --  Create a module instance.
      Inst := Create_Root_Instance (Module, Get_Location (Bind));

      --  Instantiate (deep copy).
      Verilog.Sem_Instances.Instantiate_Design (Inst);
      Set_Items_Chain (Root_Module, Inst);
      Mod_Inst := Get_Instance (Inst);

      --  Override parameters.
      Gen_Assoc := Get_Generic_Map_Aspect_Chain (Bind);
      Elab_Foreign_Parameters (Get_Parameter_Port_Chain (Mod_Inst),
                               Syn_Inst, Comp_Inst, Gen_Assoc);
      Elab_Foreign_Parameters (Get_Items_Chain (Mod_Inst),
                               Syn_Inst, Comp_Inst, Gen_Assoc);
      pragma Assert (Gen_Assoc = Null_Vhdl_Node);

      --  Analyze.
      Verilog.Sem.Sem_Design (Root_Module);

      Elab.Vhdl_Context.Set_Instance_Foreign (Syn_Inst, Int32 (Mod_Inst));
   end Elab_Foreign_Instance;

   package Back_Elab_Pkg is
      procedure Back_Elab_Foreign_Module
        (Vlg_Module : Node;
         Vlg_Inst : Synth_Instance_Acc;
         Vhd_Module : Vhdl_Node;
         Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc);
   end Back_Elab_Pkg;

   package body Back_Elab_Pkg is
      use Elab.Vhdl_Objtypes;

      function Back_Elab_Vector_Type (T : Node; El : Type_Acc) return Type_Acc
      is
         Rng : Bound_Type;
         Lsb, Msb : Int32;
      begin
         Lsb := Get_Lsb_Cst (T);
         Msb := Get_Msb_Cst (T);
         if Msb > Lsb then
            Rng := (Dir => Dir_Downto,
                    Left => Msb,
                    Right => Lsb,
                    Len => Uns32 (Msb - Lsb + 1));
         else
            Rng := (Dir => Dir_To,
                    Left => Msb,
                    Right => Lsb,
                    Len => Uns32 (Lsb - Msb + 1));
         end if;
         return Elab.Vhdl_Objtypes.Create_Vector_Type (Rng, True, El);
      end Back_Elab_Vector_Type;

      function Back_Elab_Type (T : Node) return Type_Acc is
      begin
         case Get_Kind (T) is
            when N_Logic_Type =>
               return Logic_Type;
            when N_Bit_Type =>
               return Bit_Type;
            when N_Log_Packed_Array_Cst =>
               return Back_Elab_Vector_Type (T, Logic_Type);
            when N_Bit_Packed_Array_Cst =>
               return Back_Elab_Vector_Type (T, Bit_Type);
            when others =>
               Error_Kind ("back_elab_type", T);
         end case;
      end Back_Elab_Type;

      procedure Back_Elab_Decl
        (Vlg_Typ : Node;
         Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
         Decl : Vhdl_Node)
      is
         use Vhdl.Nodes;
         Vhd_Type : constant Vhdl_Node := Get_Type (Decl);
         Typ : Type_Acc;

         use Elab.Vhdl_Context;
      begin
         pragma Assert (Get_Kind (Vhd_Type)
                          = Iir_Kind_Foreign_Vector_Type_Definition);
         Typ := Back_Elab_Type (Vlg_Typ);
         Create_Subtype_Object (Vhd_Inst, Vhd_Type, Typ);

         Create_Object (Vhd_Inst, Decl, (Typ => Typ, Val => null));
      end Back_Elab_Decl;

      procedure Back_Elab_Parameter_Chain
        (Param_Chain : Node;
         Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
         Generics : in out Vhdl_Node)
      is
         Item : Node;
      begin
         Item := Param_Chain;
         while Item /= Null_Node loop
            if Get_Kind (Item) = N_Parameter then
               Back_Elab_Decl (Get_Param_Type (Item), Vhd_Inst, Generics);
               Generics := Vhdl.Nodes.Get_Chain (Generics);
            end if;
            Item := Get_Chain (Item);
         end loop;
      end Back_Elab_Parameter_Chain;

      procedure Back_Elab_Port_Chain
        (Port_Chain : Node;
         Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
         Ports : in out Vhdl_Node)
      is
         Item : Node;
      begin
         Item := Port_Chain;
         while Item /= Null_Node loop
            case Get_Kind (Item) is
               when N_Input | N_Output | N_Inout =>
                  Back_Elab_Decl (Get_Type_Data_Type (Item), Vhd_Inst, Ports);
                  Ports := Vhdl.Nodes.Get_Chain (Ports);
               when others =>
                  null;
            end case;
            Item := Get_Chain (Item);
         end loop;
      end Back_Elab_Port_Chain;

      procedure Back_Elab_Foreign_Module
        (Vlg_Module : Node;
         Vlg_Inst : Synth_Instance_Acc;
         Vhd_Module : Vhdl_Node;
         Vhd_Inst : Elab.Vhdl_Context.Synth_Instance_Acc)
      is
         pragma Unreferenced (Vlg_Inst);
         use Vhdl.Nodes;
         Generics : Vhdl_Node;
         Ports : Vhdl_Node;
      begin
         Generics := Get_Generic_Chain (Vhd_Module);
         Ports := Get_Port_Chain (Vhd_Module);

         Back_Elab_Parameter_Chain (Get_Parameter_Port_Chain (Vlg_Module),
                                    Vhd_Inst, Generics);
         Back_Elab_Parameter_Chain (Get_Items_Chain (Vlg_Module),
                                    Vhd_Inst, Generics);
         Back_Elab_Port_Chain (Get_Ports_Chain (Vlg_Module),
                               Vhd_Inst, Ports);
         Back_Elab_Port_Chain (Get_Items_Chain (Vlg_Module),
                               Vhd_Inst, Ports);
      end Back_Elab_Foreign_Module;
   end Back_Elab_Pkg;

   function Synth_Foreign_Module
     (Base : Base_Instance_Acc;
      M : Int32;
      Vhdl_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Vhdl_Decl : Vhdl_Node) return Netlists.Module
   is
      Vlg_Module : constant Node := Node (M);
      Root_Inst : Synth_Instance_Acc;
      Inst : Synth_Instance_Acc;
      Obj : Inst_Object;
   begin
      Root_Inst := Make_Root_Instance (Base);

      Inst := Elaborate_Sub_Instance_Params (Root_Inst, Vlg_Module);

      Obj := Insts_Interning.Get (Inst_Params'(M => Vlg_Module,
                                               Inst => Inst));

      --  Back-propagate verilog nets and types to vhdl.
      --  For each parameter (either in parameter port chain or items chain),
      --   for each ports (either in ports chain or in items chain):
      --  (check matching vhdl declaration - name)
      --  create the vhdl type (bounds)
      Back_Elab_Pkg.Back_Elab_Foreign_Module
        (Vlg_Module, Inst, Vhdl_Decl, Vhdl_Inst);

      return Obj.M;
   end Synth_Foreign_Module;

   function Value_To_Number (Value : String) return Node
   is
      V : Uns32;
      Res : Node;
   begin
      --  Quick and dirty.
      V := Uns32'Value (Value);

      Res := Create_Node (N_Number);
      Set_Number_Lo_Val (Res, V);
      Set_Number_Lo_Zx (Res, 0);
      Set_Number_Hi_Val (Res, 0);
      Set_Number_Hi_Zx (Res, 0);
      return Res;
   end Value_To_Number;

   procedure Verilog_Override_Generic (Top : Int32;
                                       Gen : String;
                                       Value : String)
   is
      use Verilog.Sem_Names;
      Vlg_Module : constant Node := Node (Top);
      Id : Name_Id;
      Param : Node;
      Val : Node;
   begin
      Id := Name_Table.Get_Identifier (Gen);

      --  Find generic.
      Param := Find_Id_In_Chain (Get_Parameter_Port_Chain (Vlg_Module), Id);
      if Param = Null_Node then
         Param := Find_Id_In_Chain (Get_Items_Chain (Vlg_Module), Id);
      end if;

      if Param = Null_Node then
         Error_Msg_Elab ("cannot find parameter %i to override", +Id);
         return;
      end if;
      if Get_Kind (Param) /= N_Parameter then
         Error_Msg_Elab ("%i does not designate a parameter", +Id);
         return;
      end if;

      --  Simply override the expression.
      Val := Value_To_Number (Value);
      Set_Location (Val, Get_Location (Param));
      Set_Expression (Param, Val);
   end Verilog_Override_Generic;
end Synth.Verilog_Insts;
