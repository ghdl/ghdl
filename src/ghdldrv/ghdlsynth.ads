--  GHDL driver for synthesis
--  Copyright (C) 2016 Tristan Gingold
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
with Grt.Types; use Grt.Types;
with Netlists; use Netlists;

package Ghdlsynth is
   procedure Register_Commands;

   --  When used from a library, this init procedure must be called before
   --  ghdl_synth.
   procedure Init_For_Ghdl_Synth;

   type C_String_Array is array (Natural) of Ghdl_C_String;
   type C_String_Array_Acc is access C_String_Array;

   function Ghdl_Synth
     (Init : Natural; Argc : Natural; Argv : C_String_Array_Acc)
     return Module;

   type Foreign_Resolve_Instances_Acc is access procedure;

   Foreign_Resolve_Instances : Foreign_Resolve_Instances_Acc;
end Ghdlsynth;
