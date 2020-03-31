--  Flags for synthesis.
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

   Flag_Debug_Nomemory : Boolean := False;

   Flag_Debug_Noexpand : Boolean := False;

   Flag_Trace_Statements : Boolean := False;

   --  True to start debugger at elaboration.
   Flag_Debug_Init : Boolean := False;

   --  True to start debugger on error.
   Flag_Debug_Enable : Boolean := True;
end Synth.Flags;
