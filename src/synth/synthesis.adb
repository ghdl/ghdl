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

with Name_Table; use Name_Table;

with Netlists.Builders; use Netlists.Builders;

with Vhdl.Utils; use Vhdl.Utils;
with Vhdl.Annotations; use Vhdl.Annotations;

with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;
with Synth.Decls; use Synth.Decls;
with Synth.Insts; use Synth.Insts;

with Synth.Environment.Debug;
pragma Unreferenced (Synth.Environment.Debug);

with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Std_Package;

package body Synthesis is
   procedure Synth_Convertible_Declarations (Syn_Inst : Synth_Instance_Acc)
   is
      use Vhdl.Std_Package;
   begin
      Create_Object
        (Syn_Inst, Convertible_Integer_Type_Definition,
         Get_Value (Syn_Inst, Universal_Integer_Type_Definition));
      Create_Object
        (Syn_Inst, Convertible_Real_Type_Definition,
         Get_Value (Syn_Inst, Universal_Real_Type_Definition));
   end Synth_Convertible_Declarations;

   procedure Synth_Package_Declaration
     (Parent_Inst : Synth_Instance_Acc; Pkg : Node)
   is
      use Vhdl.Std_Package;
      pragma Assert (not Is_Uninstantiated_Package (Pkg));
      Info : constant Sim_Info_Acc := Get_Info (Pkg);
      Syn_Inst : Synth_Instance_Acc;
      Val : Value_Acc;
   begin
      Syn_Inst := Make_Instance (Parent_Inst, Info);
      Val := Create_Value_Instance (Syn_Inst);
      if Parent_Inst /= Global_Instance then
         Create_Object (Parent_Inst, Pkg, Val);
      else
         Parent_Inst.Objects (Info.Pkg_Slot) := Val;
      end if;
      Synth_Declarations (Syn_Inst, Get_Declaration_Chain (Pkg));
      if Pkg = Vhdl.Std_Package.Standard_Package then
         Synth_Convertible_Declarations (Syn_Inst);
      end if;
   end Synth_Package_Declaration;

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
         pragma Assert (Get_Kind (Dep) = Iir_Kind_Design_Unit);
         if not Get_Elab_Flag (Dep) then
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
                  Synth_Package_Declaration (Parent_Inst, Dep_Unit);
               when Iir_Kind_Package_Instantiation_Declaration =>
                  null;
               when Iir_Kind_Package_Body =>
                  null;
               when Iir_Kind_Architecture_Body =>
                  null;
            end case;
         end if;
         Next (Dep_It);
      end loop;
   end Synth_Dependencies;

   function Synth_Design (Design : Node) return Module
   is
      Unit : constant Node := Get_Library_Unit (Design);
      Arch : Node;
      Config : Node;

      Syn_Inst : Synth_Instance_Acc;
   begin
      --  Extract architecture from design.
      case Get_Kind (Unit) is
         when Iir_Kind_Architecture_Body =>
            Arch := Unit;
            Config := Get_Library_Unit
              (Get_Default_Configuration_Declaration (Arch));
         when Iir_Kind_Configuration_Declaration =>
            Config := Unit;
            Arch := Get_Named_Entity
              (Get_Block_Specification (Get_Block_Configuration (Unit)));
         when others =>
            Error_Kind ("synth_design", Unit);
      end case;

      Global_Module :=
        New_Design (New_Sname_Artificial (Get_Identifier ("top")));
      Build_Context := Build_Builders (Global_Module);
      Synth.Values.Init;
      Global_Instance := Make_Instance (null, Global_Info);
      Synth.Insts.Init;

      --  Dependencies first.
      Synth_Dependencies
        (Global_Instance, Get_Design_Unit (Get_Entity (Arch)));
      Synth_Dependencies
        (Global_Instance, Get_Design_Unit (Arch));

      Synth_Top_Entity (Arch, Config);
      Synth_All_Instances;
      if Errorout.Nbr_Errors > 0 then
         raise Compilation_Error;
      end if;

      pragma Unreferenced (Syn_Inst);
      return Global_Module;
   end Synth_Design;
end Synthesis;
