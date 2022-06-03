--  Flags for synthesis.
--  Copyright (C) 2019 Tristan Gingold
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

with Grt.Severity;

package Synth.Flags is
   --  Control name generation.  The same entity can be synthesized in very
   --  different designs because of the generics.  We need to give unique names
   --  to these designs.
   type Name_Encoding is
     (
      --  Use the entity name as is for the design name.  Possible for the
      --  top entity (and also for entities without generics and one config).
      Name_Asis,

      --  Add generic values or/and an hash.  Results in unique but long names.
      --  This allows partial synthesis: black-boxes can be synthesized later.
      Name_Hash,

      --  Just append a unique index.  Create shorter names than Name_Hash,
      --  but the names depend on the whole design.  So it won't be possible
      --  to do partial synthesis (ie synthesizing a sub-module, and then its
      --  parent considering the sub-module as a black-box).
      Name_Index,

      --  Use the entity name but also add parameters to the module.
      Name_Parameters
     );

   Flag_Debug_Noinference : Boolean := False;

   Flag_Debug_Nocleanup : Boolean := False;

   --  Do not reduce muxes in dyn extract/insert chains.
   Flag_Debug_Nomemory1 : Boolean := False;

   Flag_Debug_Nomemory2 : Boolean := False;

   Flag_Debug_Noexpand : Boolean := False;

   Flag_Trace_Statements : Boolean := False;

   --  Display source of elaborated design.
   Flag_Debug_Elaborate : Boolean := False;

   --  True to start debugger at elaboration.
   Flag_Debug_Init : Boolean := False;

   --  Maximum number of iterations for (while)/loop.  0 means unlimited.
   Flag_Max_Loop : Natural := 1000;

   --  Level at which an assert stop the simulation.
   Severity_Level : Integer := Grt.Severity.Error_Severity;

   --  Synthesize PSL and assertions.
   Flag_Formal : Boolean := True;

   --  If true, automatically add a cover on PSL asserts to know if the
   --  asserted has been started.
   Flag_Assert_Cover : Boolean := True;

   --  If true, treat all PSL assert directives like assume directives
   Flag_Assert_As_Assume : Boolean := False;

   --  If true, treat all PSL assume directives like assert directives
   Flag_Assume_As_Assert : Boolean := False;

   Flag_Verbose : Boolean := False;
end Synth.Flags;
