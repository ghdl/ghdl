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
with Vhdl.Nodes; use Vhdl.Nodes;

with Netlists; use Netlists;
with Netlists.Builders; use Netlists.Builders;

with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Synth.Context; use Synth.Context;
with Synth.Flags; use Synth.Flags;

package Synthesis is
   function Synth_Design (Design : Iir;
                          Inst : Synth_Instance_Acc;
                          Encoding : Name_Encoding) return Module;

   --  Run cleanup/memory extraction/expand passes on M.
   procedure Instance_Passes (Ctxt : Context_Acc; M : Module);

   --  Function to be called for a foreign top-level module.
   type Synth_Top_Acc is access procedure
     (Base : Base_Instance_Acc; Unit : Int32; Encoding : Name_Encoding);
   Synth_Top_Foreign : Synth_Top_Acc;

   type Synth_Initialize_Acc is access procedure;
   Synth_Initialize_Foreign : Synth_Initialize_Acc;

   Synth_Error : exception;
end Synthesis;
