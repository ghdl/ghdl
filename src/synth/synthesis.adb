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

with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;

with Synth.Objtypes;
with Synth.Insts; use Synth.Insts;

with Synth.Environment.Debug;
pragma Unreferenced (Synth.Environment.Debug);
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
            Error_Kind ("synth_design", Unit);
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
end Synthesis;
