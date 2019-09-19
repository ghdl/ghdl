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

with Types; use Types;
with Libraries;
with Hash; use Hash;
with Interning;
with Synthesis; use Synthesis;

with Netlists; use Netlists;
with Netlists.Builders;
with Netlists.Utils;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Annotations; use Vhdl.Annotations;
with Vhdl.Errors;

with Synth.Flags;
with Synth.Values; use Synth.Values;
with Synth.Environment; use Synth.Environment;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls; use Synth.Decls;
with Synth.Expr; use Synth.Expr;

package body Synth.Insts is
   Root_Instance : Synth_Instance_Acc;

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

   procedure Make_Port_Desc (Val : Value_Acc;
                             Name : Sname;
                             Ports : in out Port_Desc_Array;
                             Idx : in out Port_Nbr;
                             Dir : Port_Kind) is
   begin
      case Val.Kind is
         when Value_Wire =>
            Idx := Idx + 1;
            Ports (Idx) := (Name => Name,
                            W => Get_Type_Width (Val.Typ),
                            Dir => Dir);
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
      Name : Sname;
   begin
      Name :=  New_Sname_User (Get_Identifier (Inter));
      Make_Port_Desc (Val, Name, Ports, Idx, Dir);
   end Make_Port_Desc;

   --  Parameters that define an instance.
   type Inst_Params is record
      --  Declaration: either the entity or the component.
      Decl : Node;
      --  Implementation: the architecture or Null_Node for black boxes.
      Arch : Node;
      --  Configuration (Null_Node for black boxes).
      Config : Node;
      --  Values of generics.
      Syn_Inst : Synth_Instance_Acc;
   end record;

   type Inst_Object is record
      Decl : Node;
      Arch : Node;
      Config : Node;
      Syn_Inst : Synth_Instance_Acc;
   end record;

   function Hash (Params : Inst_Params) return Hash_Value_Type
   is
      Res : Hash_Value_Type;
   begin
      Res := Hash_Value_Type (Params.Decl);
      Res := Res xor Hash_Value_Type (Params.Arch);
      Res := Res xor Hash_Value_Type (Params.Config);
      --  TODO: hash generics
      return Res;
   end Hash;

   function Equal (Obj : Inst_Object; Params : Inst_Params) return Boolean
   is
      Inter : Node;
   begin
      if Obj.Decl /= Params.Decl
        or else Obj.Arch /= Params.Arch
        or else Obj.Config /= Params.Config
      then
         return False;
      end if;
      Inter := Get_Generic_Chain (Params.Decl);
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
      Decl : constant Node := Params.Decl;
      Arch : constant Node := Params.Arch;
      Imp : Node;
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Type : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Num : Uns32;
   begin
      if Get_Kind (Params.Decl) = Iir_Kind_Component_Declaration then
         pragma Assert (Params.Arch = Null_Node);
         pragma Assert (Params.Config = Null_Node);
         Imp := Params.Decl;
      else
         pragma Assert
           (Get_Kind (Params.Config) = Iir_Kind_Block_Configuration);
         Imp := Params.Arch;
      end if;

      --  Create the instance.
      Syn_Inst := Make_Instance (Root_Instance, Get_Info (Imp), No_Sname);
      --  Make the entity reachable.
      Set_Block_Scope (Syn_Inst, Get_Info (Decl));

      --  Copy values for generics.
      Inter := Get_Generic_Chain (Decl);
      while Inter /= Null_Node loop
         --  Bounds or range of the type.
         Inter_Type := Get_Subtype_Indication (Inter);
         if Inter_Type /= Null_Node then
            case Get_Kind (Inter_Type) is
               when Iir_Kind_Array_Subtype_Definition
                 | Iir_Kind_Integer_Subtype_Definition =>
                  Create_Object (Syn_Inst, Inter_Type,
                                 Get_Value (Params.Syn_Inst, Inter_Type));
               when others =>
                  null;
            end case;
         end if;

         --  Object.
         Create_Object (Syn_Inst, Inter, Get_Value (Params.Syn_Inst, Inter));
         Inter := Get_Chain (Inter);
      end loop;

      --  Allocate values and count inputs and outputs
      Inter := Get_Port_Chain (Decl);
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
      Set_Module (Syn_Inst,
                  New_User_Module
                    (Get_Module (Root_Instance),
                     New_Sname_User (Get_Identifier (Decl)),
                     Id_User_None, Nbr_Inputs, Nbr_Outputs, 0));

      --  Add ports to module.
      declare
         Inports : Port_Desc_Array (1 .. Nbr_Inputs);
         Outports : Port_Desc_Array (1 .. Nbr_Outputs);
      begin
         Inter := Get_Port_Chain (Decl);
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
         Set_Port_Desc (Get_Module (Syn_Inst), Inports, Outports);
      end;

      return Inst_Object'(Decl => Decl,
                          Arch => Arch,
                          Config => Params.Config,
                          Syn_Inst => Syn_Inst);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   --  Subprogram used for instantiation (direct or by component).
   --  PORTS_ASSOC belong to SYN_INST.
   procedure Synth_Instantiate_Module (Syn_Inst : Synth_Instance_Acc;
                                       Inst : Instance;
                                       Inst_Obj : Inst_Object;
                                       Ports_Assoc : Node)
   is
      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)

      Assoc : Node;
      Assoc_Inter : Node;
      Inter : Node;
      Actual : Node;
      Port : Net;
      O : Value_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      Assoc := Ports_Assoc;
      Assoc_Inter := Get_Port_Chain (Inst_Obj.Decl);
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
                  Get_Net (Synth_Expression (Syn_Inst, Actual)));
               Nbr_Inputs := Nbr_Inputs + 1;
            when Port_Out
              | Port_Inout =>
               if Actual /= Null_Iir then
                  Port := Get_Output (Inst, Nbr_Outputs);
                  Port := Builders.Build_Port (Build_Context, Port);
                  O := Create_Value_Net
                    (Port, Get_Value_Type (Inst_Obj.Syn_Inst,
                                           Get_Type (Inter)));
                  Synth_Assignment (Syn_Inst, Actual, O, Assoc);
               end if;
               Nbr_Outputs := Nbr_Outputs + 1;
         end case;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Instantiate_Module;

   procedure Synth_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Ent : Node;
      Arch : Node;
      Config : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inst_Obj : Inst_Object;
      Inst : Instance;
   begin
      --  Elaborate generic + map aspect
      Sub_Inst := Make_Instance
        (Syn_Inst, Get_Info (Ent), New_Sname_User (Get_Identifier (Ent)));
      Synth_Subprogram_Association (Sub_Inst, Syn_Inst,
                                    Get_Generic_Chain (Ent),
                                    Get_Generic_Map_Aspect_Chain (Stmt));

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Ent);
      while Is_Valid (Inter) loop
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            --  TODO
            raise Internal_Error;
         end if;
         Synth_Declaration_Type (Sub_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Make_Object (Sub_Inst, Wire_None, Inter);
            when Port_Out
              | Port_Inout =>
               Make_Object (Sub_Inst, Wire_None, Inter);
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get ((Decl => Ent,
                                        Arch => Arch,
                                        Config => Config,
                                        Syn_Inst => Sub_Inst));

      --  TODO: free sub_inst.

      Inst := New_Instance
        (Get_Module (Syn_Inst), Get_Module (Inst_Obj.Syn_Inst),
         New_Sname_User (Get_Identifier (Stmt)));

      Synth_Instantiate_Module
        (Syn_Inst, Inst, Inst_Obj, Get_Port_Map_Aspect_Chain (Stmt));
   end Synth_Direct_Instantiation_Statement;

   procedure Synth_Design_Instantiation_Statement
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

      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Ent, Arch, Config);
   end Synth_Design_Instantiation_Statement;

   procedure Synth_Blackbox_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Comp : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
   begin
      Synth_Direct_Instantiation_Statement
        (Syn_Inst, Stmt, Comp, Null_Node, Null_Node);
   end Synth_Blackbox_Instantiation_Statement;

   procedure Create_Component_Wire (Inter : Node; Val : Value_Acc)
   is
      Value : Net;
      W : Width;
   begin
      case Val.Kind is
         when Value_Wire =>
            --  Create a gate for the output, so that it could be read.
            Val.W := Alloc_Wire (Wire_Output, Inter);
            W := Get_Type_Width (Val.Typ);
            Value := Builders.Build_Signal
              (Build_Context, New_Sname (No_Sname, Get_Identifier (Inter)), W);
            Set_Wire_Gate (Val.W, Value);
         when others =>
            raise Internal_Error;
      end case;
   end Create_Component_Wire;

   procedure Synth_Component_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc; Stmt : Node)
   is
      Component : constant Node :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Config : constant Node := Get_Component_Configuration (Stmt);
      Bind : constant Node := Get_Binding_Indication (Config);
      Aspect : constant Node := Get_Entity_Aspect (Bind);
      Comp_Inst : Synth_Instance_Acc;

      Ent : Node;
      Arch : Node;
      Sub_Config : Node;
      Sub_Inst : Synth_Instance_Acc;
      Inst_Obj : Inst_Object;
      Inst : Instance;
   begin
      pragma Assert (Get_Component_Configuration (Stmt) /= Null_Node);
      pragma Assert (Get_Kind (Aspect) = Iir_Kind_Entity_Aspect_Entity);

      --  Create the sub-instance for the component
      --  Elaborate generic + map aspect
      Comp_Inst := Make_Instance (Syn_Inst, Get_Info (Component),
                                  New_Sname_User (Get_Identifier (Component)));
      Synth_Subprogram_Association (Comp_Inst, Syn_Inst,
                                    Get_Generic_Chain (Component),
                                    Get_Generic_Map_Aspect_Chain (Stmt));

      --  Create objects for inputs and outputs, assign inputs.
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Actual : Node;
         Inter : Node;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Component);
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

            Synth_Declaration_Type (Comp_Inst, Inter);
            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  Create_Object
                    (Comp_Inst, Assoc_Inter,
                     Synth_Expression_With_Type
                       (Syn_Inst, Actual, Get_Type (Assoc_Inter)));
               when Port_Out
                 | Port_Inout =>
                  Make_Object (Comp_Inst, Wire_None, Assoc_Inter);
                  Create_Component_Wire
                    (Assoc_Inter, Get_Value (Comp_Inst, Assoc_Inter));
            end case;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;

      --  Extract entity/architecture instantiated by the component.
      case Get_Kind (Aspect) is
         when Iir_Kind_Entity_Aspect_Entity =>
            Ent := Get_Entity (Aspect);
            Arch := Get_Architecture (Aspect);
         when others =>
            Vhdl.Errors.Error_Kind
              ("Synth_Component_Instantiation_Statement(2)", Aspect);
      end case;

      if Arch = Null_Node then
         Arch := Libraries.Get_Latest_Architecture (Ent);
         Sub_Config := Get_Library_Unit
           (Get_Default_Configuration_Declaration (Arch));
         Sub_Config := Get_Block_Configuration (Sub_Config);
      else
         raise Internal_Error;
      end if;

      --  Elaborate generic + map aspect
      Sub_Inst := Make_Instance
        (Comp_Inst, Get_Info (Ent), New_Sname_User (Get_Identifier (Ent)));
      Synth_Subprogram_Association (Sub_Inst, Comp_Inst,
                                    Get_Generic_Chain (Ent),
                                    Get_Generic_Map_Aspect_Chain (Bind));

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get ((Decl => Ent,
                                        Arch => Arch,
                                        Config => Sub_Config,
                                        Syn_Inst => Sub_Inst));

      --  TODO: free sub_inst.

      Inst := New_Instance
        (Get_Module (Syn_Inst), Get_Module (Inst_Obj.Syn_Inst),
         New_Sname_User (Get_Identifier (Stmt)));

      Synth_Instantiate_Module
        (Comp_Inst, Inst, Inst_Obj, Get_Port_Map_Aspect_Chain (Bind));

      --  Connect out from component to instance.
      --  Instantiate the module
      --  Elaborate ports + map aspect for the inputs (component then entity)
      --  Elaborate ports + map aspect for the outputs (entity then component)
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Inter : Node;
         Actual : Node;
         Port : Net;
         O : Value_Acc;
         Nbr_Outputs : Port_Nbr;
      begin
         Assoc := Get_Port_Map_Aspect_Chain (Stmt);
         Assoc_Inter := Get_Port_Chain (Component);
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
                  null;
               when Port_Out
                 | Port_Inout =>
                  Port := Get_Output (Inst, Nbr_Outputs);
                  Port := Builders.Build_Port (Build_Context, Port);
                  O := Create_Value_Net
                    (Port, Get_Value_Type (Syn_Inst, Get_Type (Inter)));
                  Synth_Assignment (Syn_Inst, Actual, O, Assoc);
                  Nbr_Outputs := Nbr_Outputs + 1;
            end case;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;
   end Synth_Component_Instantiation_Statement;

   procedure Synth_Top_Entity (Global_Instance : Synth_Instance_Acc;
                               Arch : Node;
                               Config : Node;
                               Inst : out Synth_Instance_Acc)
   is
      Entity : constant Node := Get_Entity (Arch);
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inst_Obj : Inst_Object;
   begin
      Root_Instance := Global_Instance;

      Syn_Inst := Make_Instance (Global_Instance, Get_Info (Arch),
                                 New_Sname_User (Get_Identifier (Entity)));
      --  Make the entity visible.
      Set_Block_Scope (Syn_Inst, Get_Info (Entity));

      --  Compute generics.
      Inter := Get_Generic_Chain (Entity);
      while Is_Valid (Inter) loop
         Synth_Declaration_Type (Syn_Inst, Inter);
         declare
            Val : Value_Acc;
         begin
            Val := Synth_Expression_With_Type
              (Syn_Inst, Get_Default_Value (Inter), Get_Type (Inter));
            Create_Object (Syn_Inst, Inter, Val);
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
         Synth_Declaration_Type (Syn_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Make_Object (Syn_Inst, Wire_None, Inter);
            when Port_Out
              | Port_Inout =>
               Make_Object (Syn_Inst, Wire_None, Inter);
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Search if corresponding module has already been used.
      --  If not create a new module
      --   * create a name from the generics and the library
      --   * create inputs/outputs
      --   * add it to the list of module to be synthesized.
      Inst_Obj := Insts_Interning.Get
        ((Decl => Entity,
          Arch => Arch,
          Config => Get_Block_Configuration (Config),
          Syn_Inst => Syn_Inst));
      Inst := Inst_Obj.Syn_Inst;
   end Synth_Top_Entity;

   procedure Create_Input_Wire (Self_Inst : Instance;
                                Inter : Node;
                                Idx : in out Port_Idx;
                                Val : Value_Acc) is
   begin
      case Val.Kind is
         when Value_Wire =>
            Val.W := Alloc_Wire (Wire_Input, Inter);
            Set_Wire_Gate (Val.W, Get_Output (Self_Inst, Idx));
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
            pragma Assert (W = Get_Type_Width (Val.Typ));
            Value := Builders.Build_Output (Build_Context, W);
            Set_Location (Value, Inter);
            Inp := Get_Input (Self_Inst, Idx);
            Connect (Inp, Value);
            Set_Wire_Gate (Val.W, Value);
            Idx := Idx + 1;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Output_Wire;

   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node)
   is
      Item : Node;
   begin
      pragma Assert (Get_Block_From_Block_Specification
                       (Get_Block_Specification (Cfg)) = Blk);
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

   procedure Synth_Verification_Units
     (Syn_Inst : Synth_Instance_Acc; Parent : Node)
   is
      Unit : Node;
   begin
      Unit := Get_Bound_Vunit_Chain (Parent);
      while Unit /= Null_Node loop
         Synth_Verification_Unit (Syn_Inst, Unit);
         Unit := Get_Bound_Vunit_Chain (Unit);
      end loop;
   end Synth_Verification_Units;

   procedure Synth_Instance (Inst : Inst_Object)
   is
      Entity : constant Node := Inst.Decl;
      Arch : constant Node := Inst.Arch;
      Syn_Inst : constant Synth_Instance_Acc := Inst.Syn_Inst;
      Self_Inst : Instance;
      Inter : Node;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      if Arch = Null_Node then
         --  Black box.
         return;
      end if;

      Self_Inst := Create_Self_Instance (Get_Module (Syn_Inst));
      Builders.Set_Parent (Build_Context, Get_Module (Syn_Inst));

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

      --  Apply configuration.
      --  FIXME: what about inner block configuration ?
      pragma Assert (Get_Kind (Inst.Config) = Iir_Kind_Block_Configuration);
      Apply_Block_Configuration (Inst.Config, Arch);

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      Synth_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      Synth_Concurrent_Statements
        (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));

      Synth_Verification_Units (Syn_Inst, Entity);
      Synth_Verification_Units (Syn_Inst, Arch);

      Finalize_Assignments (Build_Context);

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      if not Flags.Flag_Debug_Nocleanup then
         Netlists.Utils.Remove_Unused_Instances (Get_Module (Syn_Inst));
      end if;
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
