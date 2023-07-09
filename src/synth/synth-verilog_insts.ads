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

with Types; use Types;

with Elab.Vhdl_Context;
with Vhdl.Types; use Vhdl.Types;

with Netlists;

with Synth.Context; use Synth.Context;
with Synth.Flags; use Synth.Flags;

package Synth.Verilog_Insts is
   procedure Initialize;

   --  Create the declaration of the top entity.
   procedure Synth_Top_Module (Base : Base_Instance_Acc;
                               Unit : Int32;
                               Encoding : Name_Encoding);

   procedure Elab_Foreign_Instance
     (Syn_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Comp_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Bind : Vhdl_Node;
      N : Vhdl_Node);

   function Synth_Foreign_Module
     (Base : Base_Instance_Acc;
      M : Int32;
      Vhdl_Inst : Elab.Vhdl_Context.Synth_Instance_Acc;
      Vhdl_Decl : Vhdl_Node) return Netlists.Module;

   procedure Verilog_Override_Generic (Top : Int32;
                                       Gen : String;
                                       Value : String);

   procedure Synth_All_Instances;
end Synth.Verilog_Insts;
