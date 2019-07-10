--  Instantiation synthesis.
--  Copyright (C) 2019 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Libraries;
with Hash; use Hash;
with Interning;
with Synthesis; use Synthesis;

with Netlists.Builders;
with Netlists.Utils;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Annotations; use Vhdl.Annotations;

with Synth.Environment; use Synth.Environment;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls; use Synth.Decls;
with Synth.Types; use Synth.Types;
with Synth.Expr; use Synth.Expr;

package body Synth.Insts is
   procedure Make_Port_Desc (Val : Value_Acc;
                             Name : Sname;
                             Wd : Width;
                             Ports : in out Port_Desc_Array;
                             Idx : in out Port_Nbr;
                             Dir : Port_Kind)
   is
   begin
      case Val.Kind is
         when Value_Wire =>
            Idx := Idx + 1;
            Ports (Idx) := (Name => Name,
                            W => Wd,
                            Dir => Dir,
                            Left | Right => 0);
         when others =>
            raise Internal_Error; --  TODO
      end case;
   end Make_Port_Desc;

   procedure Make_Port_Desc (Syn_Inst : Synth_Instance_Acc;
                             Inter : Node;
                             Ports : in out Port_Desc_Array;
                             Idx : in out Port_Nbr;
                             Dir : Port_Kind)
   is
      Val : constant Value_Acc := Get_Value (Syn_Inst, Inter);
      Wd : constant Width := Get_Width (Syn_Inst, Get_Type (Inter));
      Name : Sname;
   begin
      Name :=  New_Sname_User (Get_Identifier (Inter));
      Make_Port_Desc (Val, Name, Wd, Ports, Idx, Dir);
   end Make_Port_Desc;

   type Inst_Params is record
      Arch : Node;
      Config : Node;
      Syn_Inst : Synth_Instance_Acc;
   end record;

   type Inst_Object is record
      Arch : Node;
      Config : Node;
      Syn_Inst : Synth_Instance_Acc;
   end record;

   function Hash (Params : Inst_Params) return Hash_Value_Type
   is
      Res : Hash_Value_Type;
   begin
      Res := Hash_Value_Type (Params.Arch);
      Res := Res xor Hash_Value_Type (Params.Config);
      --  TODO: hash generics
      return Res;
   end Hash;

   function Equal (Obj : Inst_Object; Params : Inst_Params) return Boolean
   is
      Ent : Node;
      Inter : Node;
   begin
      if Obj.Arch /= Params.Arch
        or else Obj.Config /= Params.Config
      then
         return False;
      end if;
      Ent := Get_Entity (Obj.Arch);
      Inter := Get_Generic_Chain (Ent);
      while Inter /= Null_Node loop
         if not Is_Equal (Get_Value (Obj.Syn_Inst, Inter),
                          Get_Value (Params.Syn_Inst, Inter))
         then
            return False;
         end if;
         Inter := Get_Chain (Inter);
      end loop;

      --  TODO: ports size ?
      return True;
   end Equal;

   function Build (Params : Inst_Params) return Inst_Object
   is
      Arch : constant Node := Params.Arch;
      Entity : constant Node := Get_Entity (Arch);
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Num : Uns32;
   begin
      --  Create the instance.
      Syn_Inst := Make_Instance (Global_Instance, Get_Info (Arch));
      Syn_Inst.Block_Scope := Get_Info (Entity);
      Syn_Inst.Name := New_Sname_User (Get_Identifier (Entity));

      --  Copy values for generics.
      Inter := Get_Generic_Chain (Entity);
      while Inter /= Null_Node loop
         Create_Object (Syn_Inst, Inter, Get_Value (Params.Syn_Inst, Inter));
         Inter := Get_Chain (Inter);
      end loop;

      --  Allocate values and count inputs and outputs
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         Synth_Declaration_Type (Syn_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Make_Object (Syn_Inst, Wire_None, Inter);
               Num := Get_Nbr_Wire (Get_Value (Syn_Inst, Inter));
               Nbr_Inputs := Nbr_Inputs + Port_Nbr (Num);
            when Port_Out
              | Port_Inout =>
               Make_Object (Syn_Inst, Wire_None, Inter);
               Num := Get_Nbr_Wire (Get_Value (Syn_Inst, Inter));
               Nbr_Outputs := Nbr_Outputs + Port_Nbr (Num);
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Declare module.
      Syn_Inst.M := New_User_Module
        (Global_Module, New_Sname_User (Get_Identifier (Entity)),
         Id_User_None, Nbr_Inputs, Nbr_Outputs, 0);

      --  Add ports to module.
      declare
         Inports : Port_Desc_Array (1 .. Nbr_Inputs);
         Outports : Port_Desc_Array (1 .. Nbr_Outputs);
      begin
         Inter := Get_Port_Chain (Entity);
         Nbr_Inputs := 0;
         Nbr_Outputs := 0;
         while Is_Valid (Inter) loop
            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  Make_Port_Desc
                    (Syn_Inst, Inter, Inports, Nbr_Inputs, Port_In);
               when Port_Out
                 | Port_Inout =>
                  Make_Port_Desc
                    (Syn_Inst, Inter, Outports, Nbr_Outputs, Port_Out);
            end case;
            Inter := Get_Chain (Inter);
         end loop;
         pragma Assert (Nbr_Inputs = Inports'Last);
         pragma Assert (Nbr_Outputs = Outports'Last);
         Set_Port_Desc (Syn_Inst.M, Inports, Outports);
      end;

      return Inst_Object'(Arch => Arch,
                          Config => Params.Config,
                          Syn_Inst => Syn_Inst);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   function Mode_To_Port_Kind (Mode : Iir_Mode) return Port_Kind is
   begin
      case Mode is
         when Iir_In_Mode =>
            return Port_In;
         when Iir_Buffer_Mode
           | Iir_Out_Mode
           | Iir_Inout_Mode =>
            return Port_Out;
         when Iir_Linkage_Mode
           | Iir_Unknown_Mode =>
            raise Synth_Error;
      end case;
   end Mode_To_Port_Kind;

   function Get_Nbr_Wire (Val : Value_Acc) return Uns32 is
   begin
      case Val.Kind is
         when Value_Wire =>
            return 1;
         when others =>
            raise Internal_Error;  --  TODO
      end case;
   end Get_Nbr_Wire;

   procedure Synth_Design_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Aspect : constant Iir := Get_Instantiated_Unit (Stmt);
      Arch : Node;
      Ent : Node;
      Config : Node;
      Sub_Inst : Synth_Instance_Acc;
      Inter : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Num : Uns32;
      Inst_Obj : Inst_Object;
      Inst : Instance;
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

      --  Elaborate generic + map aspect
      Sub_Inst := Make_Instance (Syn_Inst, Get_Info (Ent));
      Sub_Inst.Name := New_Sname_User (Get_Identifier (Ent));
      Synth_Subprogram_Association (Sub_Inst, Syn_Inst,
                                    Get_Generic_Chain (Ent),
                                    Get_Generic_Map_Aspect_Chain (Stmt));

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Ent);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            raise Internal_Error;
         end if;
         Synth_Declaration_Type (Sub_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Make_Object (Sub_Inst, Wire_None, Inter);
               Num := Get_Nbr_Wire (Get_Value (Sub_Inst, Inter));
               Nbr_Inputs := Nbr_Inputs + Port_Nbr (Num);
            when Port_Out
              | Port_Inout =>
               Make_Object (Sub_Inst, Wire_None, Inter);
               Num := Get_Nbr_Wire (Get_Value (Sub_Inst, Inter));
               Nbr_Outputs := Nbr_Outputs + Port_Nbr (Num);
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get ((Arch => Arch,
                                        Config => Config,
                                        Syn_Inst => Sub_Inst));

      --  TODO: free sub_inst.

      Inst := New_Instance (Syn_Inst.M, Inst_Obj.Syn_Inst.M,
                            New_Sname_User (Get_Identifier (Stmt)));

      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)

      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Actual : Node;
         O : Value_Acc;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Ent);
         Nbr_Inputs := 0;
         Nbr_Outputs := 0;
         while Is_Valid (Assoc) loop
            Inter := Get_Association_Interface (Assoc, Assoc_Inter);

            case Get_Kind (Assoc) is
               when Iir_Kind_Association_Element_Open =>
                  Actual := Get_Default_Value (Inter);
               when Iir_Kind_Association_Element_By_Expression =>
                  Actual := Get_Actual (Assoc);
               when others =>
                  raise Internal_Error;
            end case;

            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  Connect
                    (Get_Input (Inst, Nbr_Inputs),
                     Get_Net (Synth_Expression_With_Type
                                (Syn_Inst, Actual, Get_Type (Assoc_Inter)),
                              Get_Type (Assoc_Inter)));
                  Nbr_Inputs := Nbr_Inputs + 1;
               when Port_Out
                 | Port_Inout =>
                  O := Create_Value_Net (Get_Output (Inst, Nbr_Outputs),
                                           null);
                  Synth_Assignment (Syn_Inst, Actual, O);
                  Nbr_Outputs := Nbr_Outputs + 1;
            end case;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;
   end Synth_Design_Instantiation_Statement;

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
   begin
      --  Create the sub-instance for the component
      --  Elaborate generic + map aspect

      --  Load configured entity + architecture
      --  Elaborate generic + map aspect

      --  Search if corresponding module has already been used.
      --   * compare with generics value, ports size, configuration.
      --  If not create a new module
      --   * create a name from the generics, the library, the configuration
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.

      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)
      raise Internal_Error;
   end Synth_Component_Instantiation_Statement;

   procedure Create_Input_Wire (Self_Inst : Instance;
                                Inter : Node;
                                Idx : in out Port_Idx;
                                Val : Value_Acc) is
   begin
      case Val.Kind is
         when Value_Wire =>
            Val.W := Alloc_Wire (Wire_Input, Inter);
            Wire_Id_Table.Table (Val.W).Gate := Get_Output (Self_Inst, Idx);
            Idx := Idx + 1;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Input_Wire;

   procedure Create_Output_Wire (Self_Inst : Instance;
                                 Inter : Node;
                                 Idx : in out Port_Idx;
                                 Val : Value_Acc)
   is
      Value : Net;
      Inp : Input;
      W : Width;
   begin
      case Val.Kind is
         when Value_Wire =>
            --  Create a gate for the output, so that it could be read.
            Val.W := Alloc_Wire (Wire_Output, Inter);
            W := Get_Output_Desc (Get_Module (Self_Inst), Idx).W;
            Value := Builders.Build_Output (Build_Context, W);
            Inp := Get_Input (Self_Inst, Idx);
            Connect (Inp, Value);
            Wire_Id_Table.Table (Val.W).Gate := Value;
            Idx := Idx + 1;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Output_Wire;

   procedure Synth_Instance (Inst : Inst_Object)
   is
      Syn_Inst : constant Synth_Instance_Acc := Inst.Syn_Inst;
      Entity : constant Node := Get_Entity (Inst.Arch);
      Arch : constant Node := Inst.Arch;
      Self_Inst : Instance;
      Inter : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      Self_Inst := Create_Self_Instance (Syn_Inst.M);
      Builders.Set_Parent (Build_Context, Syn_Inst.M);

      --  Create wires for inputs and outputs.
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Create_Input_Wire
                 (Self_Inst, Inter, Nbr_Inputs, Get_Value (Syn_Inst, Inter));
            when Port_Out
              | Port_Inout =>
               Create_Output_Wire
                 (Self_Inst, Inter, Nbr_Outputs, Get_Value (Syn_Inst, Inter));
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      Synth_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      Synth_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      Netlists.Utils.Remove_Unused_Instances (Syn_Inst.M);
   end Synth_Instance;

   procedure Synth_All_Instances
   is
      use Insts_Interning;
      Idx : Index_Type;
   begin
      Idx := First_Index;
      while Idx <= Last_Index loop
         Synth_Instance (Get_By_Index (Idx));
         Idx := Idx + 1;
      end loop;
   end Synth_All_Instances;

   procedure Init is
   begin
      Insts_Interning.Init;
   end Init;
end Synth.Insts;
