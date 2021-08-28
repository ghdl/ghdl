--  Synthesis.
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

with Types; use Types;
with Errorout; use Errorout;

with Netlists.Cleanup;
with Netlists.Memories;
with Netlists.Expands;

with Synth.Objtypes;
with Synth.Vhdl_Insts; use Synth.Vhdl_Insts;

with Synth.Values.Debug;
pragma Unreferenced (Synth.Values.Debug);

package body Synthesis is
   procedure Synth_Design (Design : Node;
                           Encoding : Name_Encoding;
                           M : out Module;
                           Inst : out Synth_Instance_Acc)
   is
      Unit : constant Node := Get_Library_Unit (Design);
      Arch : Node;
      Config : Node;
      Global_Instance : Synth_Instance_Acc;
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
            raise Internal_Error;
      end case;

      Global_Instance := Make_Base_Instance;

      Synth.Objtypes.Init;

      Synth_Top_Entity (Global_Instance, Arch, Config, Encoding, Inst);
      Synth_All_Instances;
      if Errorout.Nbr_Errors > 0 then
         M := No_Module;
         return;
      end if;

      M := Get_Top_Module (Global_Instance);
   end Synth_Design;

   procedure Instance_Passes (Ctxt : Context_Acc; M : Module) is
   begin
      --  Remove unused gates.  This is not only an optimization but also
      --  a correctness point: there might be some unsynthesizable gates, like
      --  the one created for 'rising_egde (clk) and not rst'.
      if not Synth.Flags.Flag_Debug_Nocleanup then
         --  Netlists.Cleanup.Remove_Unconnected_Instances (Inst.M);
         Netlists.Cleanup.Mark_And_Sweep (M);
         Netlists.Cleanup.Remove_Output_Gates (M);
      end if;

      if not Synth.Flags.Flag_Debug_Nomemory2 then
         Netlists.Memories.Extract_Memories (Ctxt, M);
         --  Remove remaining clock edge gates.
         Netlists.Cleanup.Mark_And_Sweep (M);
      end if;

      if not Synth.Flags.Flag_Debug_Noexpand then
         Netlists.Expands.Expand_Gates (Ctxt, M);
      end if;
   end Instance_Passes;

end Synthesis;
