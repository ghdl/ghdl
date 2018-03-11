--  Synthesis.
--  Copyright (C) 2017 Tristan Gingold
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
with Name_Table; use Name_Table;

with Netlists.Builders; use Netlists.Builders;
with Netlists.Utils;

with Iirs_Utils; use Iirs_Utils;
with Simul.Environments; use Simul.Environments;
with Simul.Elaboration; use Simul.Elaboration;

with Synth.Environment; use Synth.Environment;
with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Synth.Types; use Synth.Types;
with Synth.Decls; use Synth.Decls;
with Synth.Stmts; use Synth.Stmts;

with Synth.Environment.Debug;
pragma Unreferenced (Synth.Environment.Debug);

with Errorout; use Errorout;

package body Synthesis is
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
                             Inter : Iir;
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

   procedure Create_Input_Wire
     (Self_Inst : Instance; Idx : in out Port_Idx; Val : Value_Acc) is
   begin
      case Val.Kind is
         when Value_Wire =>
            Wire_Id_Table.Table (Val.W).Gate := Get_Output (Self_Inst, Idx);
            Idx := Idx + 1;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Input_Wire;

   procedure Create_Output_Wire
     (Self_Inst : Instance; Idx : in out Port_Idx; Val : Value_Acc)
   is
      Value : Net;
      Inp : Input;
      W : Width;
   begin
      case Val.Kind is
         when Value_Wire =>
            --  Create a gate for the output, so that it could be read.
            W := Get_Output_Desc (Get_Module (Self_Inst), Idx).W;
            Value := Build_Output (Build_Context, W);
            Inp := Get_Input (Self_Inst, Idx);
            Connect (Inp, Value);
            Wire_Id_Table.Table (Val.W).Gate := Value;
            Idx := Idx + 1;
         when others =>
            raise Internal_Error;
      end case;
   end Create_Output_Wire;

   function Synth_Entity
     (Parent : Module; Arch : Iir; Sim_Inst : Block_Instance_Acc)
     return Synth_Instance_Acc
   is
      Entity : constant Iir := Get_Entity (Arch);
      Syn_Inst : Synth_Instance_Acc;
      Self_Inst : Instance;
      Inter : Iir;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      Num : Uns32;
   begin
      Syn_Inst := Make_Instance (Sim_Inst);
      Syn_Inst.Name := New_Sname_User (Get_Identifier (Entity));

      --  Allocate values and count inputs and outputs
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Make_Object (Syn_Inst, Wire_Input, Inter);
               Num := Get_Nbr_Wire (Get_Value (Syn_Inst, Inter));
               Nbr_Inputs := Nbr_Inputs + Port_Nbr (Num);
            when Port_Out
              | Port_Inout =>
               Make_Object (Syn_Inst, Wire_Output, Inter);
               Num := Get_Nbr_Wire (Get_Value (Syn_Inst, Inter));
               Nbr_Outputs := Nbr_Outputs + Port_Nbr (Num);
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      --  Declare module.
      Syn_Inst.M :=
        New_User_Module (Parent, New_Sname_User (Get_Identifier (Entity)),
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

      Self_Inst := Create_Self_Instance (Syn_Inst.M);
      Set_Parent (Build_Context, Syn_Inst.M);

      --  Create wires for inputs and outputs.
      Inter := Get_Port_Chain (Entity);
      Nbr_Inputs := 0;
      Nbr_Outputs := 0;
      while Is_Valid (Inter) loop
         case Mode_To_Port_Kind (Get_Mode (Inter)) is
            when Port_In =>
               Create_Input_Wire
                 (Self_Inst, Nbr_Inputs, Get_Value (Syn_Inst, Inter));
            when Port_Out
              | Port_Inout =>
               Create_Output_Wire
                 (Self_Inst, Nbr_Outputs, Get_Value (Syn_Inst, Inter));
         end case;
         Inter := Get_Chain (Inter);
      end loop;

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Entity));
      Synth_Statements (Syn_Inst, Get_Concurrent_Statement_Chain (Entity));

      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Arch));
      Synth_Statements (Syn_Inst, Get_Concurrent_Statement_Chain (Arch));

      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      Netlists.Utils.Remove_Unused_Instances (Syn_Inst.M);

      return Syn_Inst;
   end Synth_Entity;

   function Synth_Design (Design : Iir) return Module
   is
      Unit : constant Iir := Get_Library_Unit (Design);
      Arch : Iir;

      Des : Module;
      Syn_Inst : Synth_Instance_Acc;
   begin
      --  Extract architecture from design.
      case Get_Kind (Unit) is
         when Iir_Kind_Architecture_Body =>
            Arch := Unit;
         when Iir_Kind_Configuration_Declaration =>
            Arch := Get_Named_Entity
              (Get_Block_Specification (Get_Block_Configuration (Unit)));
         when others =>
            Error_Kind ("synth_design", Unit);
      end case;

      Instance_Map := new Instance_Map_Array (0 .. Nbr_Block_Instances);

      Des := New_Design (New_Sname_Artificial (Get_Identifier ("top")));
      Build_Context := Build_Builders (Des);
      Syn_Inst := Synth_Entity (Des, Arch, Top_Instance);

      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      pragma Unreferenced (Syn_Inst);
      return Des;
   end Synth_Design;
end Synthesis;
