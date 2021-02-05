--  Source/origin of synthesis.
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
with Errorout;

with Netlists;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Errors;

package Synth.Source is
   subtype Syn_Src is Node;
   No_Syn_Src : constant Syn_Src := Null_Node;

   function "+" (N : Node) return Location_Type renames Vhdl.Errors."+";
   function "+" (N : Node) return Errorout.Earg_Type renames Vhdl.Errors."+";

   procedure Set_Location (N : Netlists.Net; Src : Syn_Src);
   pragma Inline (Set_Location);

   procedure Set_Location (Inst : Netlists.Instance; Src : Syn_Src);
   pragma Inline (Set_Location);

   --  Set only if not yet set.
   procedure Set_Location_Maybe (Inst : Netlists.Instance; Src : Syn_Src);
   procedure Set_Location_Maybe (N : Netlists.Net; Src : Syn_Src);
end Synth.Source;
