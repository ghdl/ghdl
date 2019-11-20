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
with Dyn_Tables;
with Interning;
with Synthesis; use Synthesis;

with Grt.Algos;

with Netlists; use Netlists;
with Netlists.Builders;
with Netlists.Cleanup;
with Netlists.Memories;
with Netlists.Expands;
with Netlists.Concats;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Errors;
with Vhdl.Ieee.Math_Real;

with Synth.Flags;
with Synth.Values; use Synth.Values;
with Synth.Environment; use Synth.Environment;
with Synth.Stmts; use Synth.Stmts;
with Synth.Decls; use Synth.Decls;
with Synth.Expr; use Synth.Expr;
with Synth.Source; use Synth.Source;
with Synth.Debugger;

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

   function Make_Port_Desc (Syn_Inst : Synth_Instance_Acc;
                            Inter : Node;
                            Dir : Port_Kind) return Port_Desc
   is
      Val : constant Value_Acc := Get_Value (Syn_Inst, Inter);
      Name : Sname;
   begin
      Name :=  New_Sname_User (Get_Identifier (Inter));
      return (Name => Name,
              W => Get_Type_Width (Val.Typ),
              Dir => Dir);
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
      M : Module;
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
      Inter_Typ : Type_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Cur_Module : Module;
      Val : Value_Acc;
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
      Syn_Inst := Make_Instance (Root_Instance, Imp, No_Sname);

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
         --  Elaborate the type...
         Synth_Declaration_Type (Syn_Inst, Inter);
         Inter_Typ := Get_Value_Type (Syn_Inst, Get_Type (Inter));
         if not Is_Bounded_Type (Inter_Typ) then
            --  ... but get it from the template (so that unbounded types
            --  are bounded).
            Inter_Typ := Get_Value (Params.Syn_Inst, Inter).Typ;
         end if;
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
               Nbr_Inputs := Nbr_Inputs + 1;
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
               Nbr_Outputs := Nbr_Outputs + 1;
         end case;
         Create_Object (Syn_Inst, Inter, Val);
         Inter := Get_Chain (Inter);
      end loop;

      --  Declare module.
      --  Build it now because it may be referenced for instantiations before
      --  being synthetized.
      Cur_Module := New_User_Module (Get_Top_Module (Root_Instance),
                                     New_Sname_User (Get_Identifier (Decl)),
                                     Id_User_None, Nbr_Inputs, Nbr_Outputs, 0);

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
                  Nbr_Inputs := Nbr_Inputs + 1;
                  Inports (Nbr_Inputs) :=
                    Make_Port_Desc (Syn_Inst, Inter, Port_In);
               when Port_Out
                 | Port_Inout =>
                  Nbr_Outputs := Nbr_Outputs + 1;
                  Outports (Nbr_Outputs) :=
                    Make_Port_Desc (Syn_Inst, Inter, Port_Out);
            end case;
            Inter := Get_Chain (Inter);
         end loop;
         pragma Assert (Nbr_Inputs = Inports'Last);
         pragma Assert (Nbr_Outputs = Outports'Last);
         Set_Port_Desc (Cur_Module, Inports, Outports);
      end;

      return Inst_Object'(Decl => Decl,
                          Arch => Arch,
                          Config => Params.Config,
                          Syn_Inst => Syn_Inst,
                          M => Cur_Module);
   end Build;

   package Insts_Interning is new Interning
     (Params_Type => Inst_Params,
      Object_Type => Inst_Object,
      Hash => Hash,
      Build => Build,
      Equal => Equal);

   procedure Synth_Individual_Prefix (Syn_Inst : Synth_Instance_Acc;
                                      Inter_Inst : Synth_Instance_Acc;
                                      Formal : Node;
                                      Off : out Uns32;
                                      Typ : out Type_Acc) is
   begin
      case Get_Kind (Formal) is
         when Iir_Kind_Interface_Signal_Declaration =>
            Off := 0;
            Typ := Get_Value_Type (Inter_Inst, Get_Type (Formal));
         when Iir_Kind_Simple_Name =>
            Synth_Individual_Prefix
              (Syn_Inst, Inter_Inst, Get_Named_Entity (Formal), Off, Typ);
         when Iir_Kind_Selected_Element =>
            declare
               Idx : constant Iir_Index32 :=
                 Get_Element_Position (Get_Named_Entity (Formal));
            begin
               Synth_Individual_Prefix
                 (Syn_Inst, Inter_Inst, Get_Prefix (Formal), Off, Typ);
               Off := Off + Typ.Rec.E (Idx + 1).Off;
               Typ := Typ.Rec.E (Idx + 1).Typ;
            end;
         when Iir_Kind_Indexed_Name =>
            declare
               Voff : Net;
               Arr_Off : Uns32;
               W : Width;
            begin
               Synth_Individual_Prefix
                 (Syn_Inst, Inter_Inst, Get_Prefix (Formal), Off, Typ);
               Synth_Indexed_Name
                 (Syn_Inst, Formal, Typ, Voff, Arr_Off, W);
               if Voff /= No_Net then
                  raise Internal_Error;
               end if;
               Off := Off + Arr_Off;
               Typ := Get_Array_Element (Typ);
            end;
         when others =>
            Vhdl.Errors.Error_Kind ("synth_individual_prefix", Formal);
      end case;
   end Synth_Individual_Prefix;

   type Value_Offset_Record is record
      Off : Uns32;
      Val : Value_Acc;
   end record;

   package Value_Offset_Tables is new Dyn_Tables
     (Table_Component_Type => Value_Offset_Record,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1);

   procedure Sort_Value_Offset (Els : Value_Offset_Tables.Instance)
   is
      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Els.Table (Op1).Off < Els.Table (Op2).Off;
      end Lt;

      procedure Swap (From : Natural; To : Natural)
      is
         T : constant Value_Offset_Record := Els.Table (From);
      begin
         Els.Table (From) := Els.Table (To);
         Els.Table (To) := T;
      end Swap;

      procedure Heap_Sort is new Grt.Algos.Heap_Sort (Lt => Lt, Swap => Swap);
   begin
      Heap_Sort (Value_Offset_Tables.Last (Els));
   end Sort_Value_Offset;

   procedure Synth_Individual_Input_Assoc (Inp : Input;
                                           Syn_Inst : Synth_Instance_Acc;
                                           Assoc : Node;
                                           Inter_Inst : Synth_Instance_Acc)
   is
      use Netlists.Concats;
      Iassoc : Node;
      V : Value_Acc;
      Off : Uns32;
      Typ : Type_Acc;
      Els : Value_Offset_Tables.Instance;
      Concat : Concat_Type;
      N_Off : Uns32;
      N : Net;
   begin
      Value_Offset_Tables.Init (Els, 16);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Individual_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Off, Typ);

         --   2. synth expression
         V := Synth_Expression_With_Type (Syn_Inst, Get_Actual (Iassoc), Typ);

         --   3. save in a table
         Value_Offset_Tables.Append (Els, (Off, V));

         Iassoc := Get_Chain (Iassoc);
      end loop;

      --  Then:
      --   1. sort table by offset
      Sort_Value_Offset (Els);

      --   2. concat
      N_Off := 0;
      for I in Value_Offset_Tables.First .. Value_Offset_Tables.Last (Els)
      loop
         pragma Assert (N_Off = Els.Table (I).Off);
         V := Els.Table (I).Val;
         N_Off := N_Off + V.Typ.W;
         Append (Concat, Get_Net (V));
      end loop;
      Value_Offset_Tables.Free (Els);

      --   3. connect
      Build (Get_Build (Syn_Inst), Concat, N);
      Connect (Inp, N);
   end Synth_Individual_Input_Assoc;

   procedure Synth_Input_Assoc (Inp : Input;
                                Syn_Inst : Synth_Instance_Acc;
                                Assoc : Node;
                                Inter_Inst : Synth_Instance_Acc;
                                Inter : Node)
   is
      Actual : Node;
      Formal_Typ : Type_Acc;
   begin
      case Iir_Kinds_Association_Element (Get_Kind (Assoc)) is
         when Iir_Kind_Association_Element_Open =>
            Actual := Get_Default_Value (Inter);
         when Iir_Kind_Association_Element_By_Expression =>
            Actual := Get_Actual (Assoc);
         when Iir_Kind_Association_Element_By_Individual =>
            Synth_Individual_Input_Assoc (Inp, Syn_Inst, Assoc, Inter_Inst);
            return;
      end case;

      Formal_Typ := Get_Value_Type (Inter_Inst, Get_Type (Inter));

      Connect (Inp,
               Get_Net (Synth_Expression_With_Type
                          (Syn_Inst, Actual, Formal_Typ)));
   end Synth_Input_Assoc;

   procedure Synth_Individual_Output_Assoc (Outp : Net;
                                            Syn_Inst : Synth_Instance_Acc;
                                            Assoc : Node;
                                            Inter_Inst : Synth_Instance_Acc)
   is
      use Netlists.Builders;
      Iassoc : Node;
      V : Value_Acc;
      Off : Uns32;
      Typ : Type_Acc;
      O : Net;
      Port : Net;
   begin
      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);

      Iassoc := Get_Chain (Assoc);
      while Iassoc /= Null_Node
        and then not Get_Whole_Association_Flag (Iassoc)
      loop
         --  For each individual assoc:
         --   1. compute type and offset
         Synth_Individual_Prefix
           (Syn_Inst, Inter_Inst, Get_Formal (Iassoc), Off, Typ);

         --   2. Extract the value.
         O := Build_Extract (Get_Build (Syn_Inst), Port, Off, Typ.W);
         V := Create_Value_Net (O, Typ);

         --   3. Assign.
         Synth_Assignment (Syn_Inst, Get_Actual (Iassoc), V, Iassoc);

         Iassoc := Get_Chain (Iassoc);
      end loop;
   end Synth_Individual_Output_Assoc;

   procedure Synth_Output_Assoc (Outp : Net;
                                 Syn_Inst : Synth_Instance_Acc;
                                 Assoc : Node;
                                 Inter_Inst : Synth_Instance_Acc;
                                 Inter : Node)
   is
      Actual : Node;
      Formal_Typ : Type_Acc;
      Port : Net;
      O : Value_Acc;
   begin
      case Get_Kind (Assoc) is
         when Iir_Kind_Association_Element_Open =>
            --  Not connected.
            return;
         when Iir_Kind_Association_Element_By_Expression =>
            Actual := Get_Actual (Assoc);
         when others =>
            Synth_Individual_Output_Assoc
              (Outp, Syn_Inst, Assoc, Inter_Inst);
            return;
      end case;

      Formal_Typ := Get_Value (Inter_Inst, Inter).Typ;

      --  Create a port gate (so that is has a name).
      Port := Builders.Build_Port (Get_Build (Syn_Inst), Outp);
      O := Create_Value_Net (Port, Formal_Typ);
      --  Assign the port output to the actual (a net).
      Synth_Assignment (Syn_Inst, Actual, O, Assoc);
   end Synth_Output_Assoc;

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
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      Assoc := Ports_Assoc;
      Assoc_Inter := Get_Port_Chain (Inst_Obj.Decl);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Assoc) loop
         if Get_Whole_Association_Flag (Assoc) then
            Inter := Get_Association_Interface (Assoc, Assoc_Inter);

            case Mode_To_Port_Kind (Get_Mode (Inter)) is
               when Port_In =>
                  --  Connect the net to the input.
                  Synth_Input_Assoc
                    (Get_Input (Inst, Nbr_Inputs),
                     Syn_Inst, Assoc, Inst_Obj.Syn_Inst, Inter);
                  Nbr_Inputs := Nbr_Inputs + 1;
               when Port_Out
                 | Port_Inout =>
                  Synth_Output_Assoc
                    (Get_Output (Inst, Nbr_Outputs),
                     Syn_Inst, Assoc, Inst_Obj.Syn_Inst, Inter);
                  Nbr_Outputs := Nbr_Outputs + 1;
            end case;
         end if;
         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Instantiate_Module;

   procedure Synth_Generics_Association (Sub_Inst : Synth_Instance_Acc;
                                         Syn_Inst : Synth_Instance_Acc;
                                         Inter_Chain : Node;
                                         Assoc_Chain : Node)
   is
      Inter : Node;
      Inter_Type : Type_Acc;
      Assoc : Node;
      Assoc_Inter : Node;
      Actual : Node;
      Val : Value_Acc;
   begin
      Assoc := Assoc_Chain;
      Assoc_Inter := Inter_Chain;
      while Is_Valid (Assoc) loop
         Inter := Get_Association_Interface (Assoc, Assoc_Inter);

         Synth_Declaration_Type (Sub_Inst, Inter);
         Inter_Type := Get_Value_Type (Sub_Inst, Get_Type (Inter));

         pragma Assert (Iir_Parameter_Modes (Get_Mode (Inter)) = Iir_In_Mode);
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               Actual := Get_Default_Value (Inter);
               Val := Synth_Expression_With_Type
                 (Sub_Inst, Actual, Inter_Type);
            when Iir_Kind_Association_Element_By_Expression =>
               Actual := Get_Actual (Assoc);
               Val := Synth_Expression_With_Type
                 (Syn_Inst, Actual, Inter_Type);
            when others =>
               raise Internal_Error;
         end case;

         Val := Synth_Subtype_Conversion (Val, Inter_Type, True, Assoc);

         pragma Assert (Is_Static (Val));

         Create_Object (Sub_Inst, Inter, Val);

         Next_Association_Interface (Assoc, Assoc_Inter);
      end loop;
   end Synth_Generics_Association;

   --  Return the type of EXPR without evaluating it.
   --  FIXME: how dubious is it ?
   function Synth_Type_Of_Object (Syn_Inst : Synth_Instance_Acc; Expr : Node)
                                 return Type_Acc is
   begin
      case Get_Kind (Expr) is
         when Iir_Kind_Signal_Declaration
           | Iir_Kind_Interface_Signal_Declaration =>
            declare
               Val : constant Value_Acc := Get_Value (Syn_Inst, Expr);
            begin
               return Val.Typ;
            end;
         when Iir_Kind_Simple_Name =>
            return Synth_Type_Of_Object (Syn_Inst, Get_Named_Entity (Expr));
         when others =>
            Vhdl.Errors.Error_Kind ("synth_type_of_object", Expr);
      end case;
      return null;
   end Synth_Type_Of_Object;

   procedure Synth_Direct_Instantiation_Statement
     (Syn_Inst : Synth_Instance_Acc;
      Stmt : Node;
      Ent : Node;
      Arch : Node;
      Config : Node)
   is
      Sub_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Typ : Type_Acc;
      Inst_Obj : Inst_Object;
      Inst : Instance;
      Val : Value_Acc;
   begin
      --  Elaborate generic + map aspect
      Sub_Inst := Make_Instance
        (Syn_Inst, Ent, New_Sname_User (Get_Identifier (Ent)));

      Synth_Generics_Association (Sub_Inst, Syn_Inst,
                                  Get_Generic_Chain (Ent),
                                  Get_Generic_Map_Aspect_Chain (Stmt));

      --  Elaborate port types.
      --  FIXME: what about unconstrained ports ?  Get the type from the
      --    association.
      Inter := Get_Port_Chain (Ent);
      while Is_Valid (Inter) loop
         if not Is_Fully_Constrained_Type (Get_Type (Inter)) then
            --  TODO
            --  Find the association for this interface
            --  * if individual assoc: get type
            --  * if whole assoc: get type from object.
            declare
               Assoc : Node;
            begin
               Assoc := Find_First_Association_For_Interface
                 (Get_Port_Map_Aspect_Chain (Stmt), Get_Port_Chain (Ent),
                  Inter);
               if Assoc = Null_Node then
                  raise Internal_Error;
               end if;
               case Get_Kind (Assoc) is
                  when Iir_Kind_Association_Element_By_Expression =>
                     Inter_Typ := Synth_Type_Of_Object
                       (Syn_Inst, Get_Actual (Assoc));
                  when others =>
                     raise Internal_Error;
               end case;
            end;
         else
            Synth_Declaration_Type (Sub_Inst, Inter);
            Inter_Typ := Get_Value_Type (Sub_Inst, Get_Type (Inter));
         end if;
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
         end case;
         Create_Object (Sub_Inst, Inter, Val);
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

      Inst := New_Instance (Get_Instance_Module (Syn_Inst),
                            Inst_Obj.M,
                            New_Sname (Get_Sname (Syn_Inst),
                                       Get_Identifier (Stmt)));
      Set_Location (Inst, Stmt);

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
      Comp_Inst := Make_Instance (Syn_Inst, Component,
                                  New_Sname_User (Get_Identifier (Component)));

      Synth_Generics_Association (Comp_Inst, Syn_Inst,
                                  Get_Generic_Chain (Component),
                                  Get_Generic_Map_Aspect_Chain (Stmt));

      --  Create objects for inputs and outputs, assign inputs.
      declare
         Assoc : Node;
         Assoc_Inter : Node;
         Actual : Node;
         Inter : Node;
         Inter_Type : Type_Acc;
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
                  Inter_Type :=
                    Get_Value_Type (Comp_Inst, Get_Type (Assoc_Inter));
                  Create_Object (Comp_Inst, Assoc_Inter,
                                 Synth_Expression_With_Type
                                   (Syn_Inst, Actual, Inter_Type));
               when Port_Out
                 | Port_Inout =>
                  Create_Wire_Object (Comp_Inst, Wire_None, Assoc_Inter);
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
        (Comp_Inst, Ent, New_Sname_User (Get_Identifier (Ent)));
      Synth_Generics_Association (Sub_Inst, Comp_Inst,
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

      Inst := New_Instance (Get_Instance_Module (Syn_Inst),
                            Inst_Obj.M,
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
                  if Actual /= Null_Node then
                     Port := Get_Output (Inst, Nbr_Outputs);
                     Port := Builders.Build_Port (Get_Build (Syn_Inst), Port);
                     O := Create_Value_Net
                       (Port, Get_Value_Type (Comp_Inst, Get_Type (Inter)));
                     Synth_Assignment (Syn_Inst, Actual, O, Assoc);
                  end if;
                  Nbr_Outputs := Nbr_Outputs + 1;
            end case;
            Next_Association_Interface (Assoc, Assoc_Inter);
         end loop;
      end;
   end Synth_Component_Instantiation_Statement;

   procedure Synth_Dependencies (Parent_Inst : Synth_Instance_Acc; Unit : Node)
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
            Synth_Dependencies (Parent_Inst, Dep);
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
                     Synth_Package_Declaration (Parent_Inst, Dep_Unit);
                     --  Do not try to elaborate math_real body: there are
                     --  functions with loop.  Currently, try create signals,
                     --  which is not possible during package elaboration.
                     if Bod /= Null_Node
                       and then Dep_Unit /= Vhdl.Ieee.Math_Real.Math_Real_Pkg
                     then
                        Bod_Unit := Get_Design_Unit (Bod);
                        Synth_Dependencies (Parent_Inst, Bod_Unit);
                        Synth_Package_Body (Parent_Inst, Dep_Unit, Bod);
                     end if;
                  end;
               when Iir_Kind_Package_Instantiation_Declaration =>
                  null;
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
   end Synth_Dependencies;

   procedure Synth_Top_Entity (Global_Instance : Synth_Instance_Acc;
                               Arch : Node;
                               Config : Node;
                               Inst : out Synth_Instance_Acc)
   is
      Entity : constant Node := Get_Entity (Arch);
      Syn_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Typ : Type_Acc;
      Inst_Obj : Inst_Object;
      Val : Value_Acc;
   begin
      Root_Instance := Global_Instance;

      if Flags.Flag_Debug_Init then
         Synth.Debugger.Debug_Init;
      end if;

      --  Dependencies first.
      Synth_Dependencies (Global_Instance, Get_Design_Unit (Entity));
      Synth_Dependencies (Global_Instance, Get_Design_Unit (Arch));

      Syn_Inst := Make_Instance (Global_Instance, Arch,
                                 New_Sname_User (Get_Identifier (Entity)));

      --  Compute generics.
      Inter := Get_Generic_Chain (Entity);
      while Is_Valid (Inter) loop
         Synth_Declaration_Type (Syn_Inst, Inter);
         declare
            Val : Value_Acc;
            Inter_Type : Type_Acc;
         begin
            Inter_Type := Get_Value_Type (Syn_Inst, Get_Type (Inter));
            Val := Synth_Expression_With_Type
              (Syn_Inst, Get_Default_Value (Inter), Inter_Type);
            pragma Assert (Is_Static (Val));
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
         Inter_Typ := Get_Value_Type (Syn_Inst, Get_Type (Inter));
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Val := Create_Value_Net (No_Net, Inter_Typ);
            when Port_Out
              | Port_Inout =>
               Val := Create_Value_Wire (No_Wire_Id, Inter_Typ);
         end case;
         Create_Object (Syn_Inst, Inter, Val);
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
                                Idx : Port_Idx;
                                Val : Value_Acc) is
   begin
      pragma Assert (Val.Kind = Value_Net);
      Val.N := Get_Output (Self_Inst, Idx);
   end Create_Input_Wire;

   procedure Create_Output_Wire (Self_Inst : Instance;
                                 Inter : Node;
                                 Idx : Port_Idx;
                                 Val : Value_Acc)
   is
      Value : Net;
      Inp : Input;
      W : Width;
   begin
      pragma Assert (Val.Kind = Value_Wire);

      --  Create a gate for the output, so that it could be read.
      Val.W := Alloc_Wire (Wire_Output, Inter);
      W := Get_Output_Desc (Get_Module (Self_Inst), Idx).W;
      pragma Assert (W = Get_Type_Width (Val.Typ));
      Value := Builders.Build_Output (Build_Context, W);
      Set_Location (Value, Inter);
      Inp := Get_Input (Self_Inst, Idx);
      Connect (Inp, Value);
      Set_Wire_Gate (Val.W, Value);
   end Create_Output_Wire;

   procedure Apply_Block_Configuration (Cfg : Node; Blk : Node)
   is
      Item : Node;
   begin
      --  Be sure CFG applies to BLK.
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
      Val : Value_Acc;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
   begin
      if Arch = Null_Node then
         --  Black box.
         return;
      end if;

      Synth_Dependencies (Root_Instance, Get_Design_Unit (Arch));

      Set_Instance_Module (Syn_Inst, Inst.M);
      Self_Inst := Get_Self_Instance (Inst.M);
      Set_Location (Self_Inst, Entity);

      --  Create wires for inputs and outputs.
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         Val := Get_Value (Syn_Inst, Inter);
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Create_Input_Wire (Self_Inst, Nbr_Inputs, Val);
               Nbr_Inputs := Nbr_Inputs + 1;
            when Port_Out
              | Port_Inout =>
               Create_Output_Wire (Self_Inst, Inter, Nbr_Outputs, Val);
               Nbr_Outputs := Nbr_Outputs + 1;
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

      Finalize_Assignments (Get_Build (Syn_Inst));

      Finalize_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      if not Synth.Flags.Flag_Debug_Nocleanup then
         Netlists.Cleanup.Remove_Unconnected_Instances (Inst.M);
      end if;

      if not Synth.Flags.Flag_Debug_Nomemory then
         Netlists.Memories.Extract_Memories (Get_Build (Syn_Inst), Inst.M);
      end if;

      if not Synth.Flags.Flag_Debug_Noexpand then
         Netlists.Expands.Expand_Gates (Get_Build (Syn_Inst), Inst.M);
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
