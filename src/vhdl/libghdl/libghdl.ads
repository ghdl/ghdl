--  Library interface for the analyzer.
--  Copyright (C) 2017 Tristan Gingold
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
with Ghdlsynth_Maybe;
pragma Unreferenced (Ghdlsynth_Maybe);

package Libghdl is
   --  Perform early initializations, and set hooks.
   procedure Set_Hooks_For_Analysis;

   --  To be called before Analyze_Init to set command line options.
   --  This decodes a driver option (so handle all analyzer options, and
   --  --ieee). Return 0 for success.
   function Set_Option (Opt : Thin_String_Ptr; Len : Natural) return Integer;

   --  Set the prefix (used to locate libraries).
   procedure Set_Exec_Prefix (Prefix : Thin_String_Ptr; Len : Natural);

   --  To be called before Analyze_File to initialize analysis.
   function Analyze_Init_Status return Integer;

   --  Deprecated, use Analyze_Init_Status instead.
   --  Raise an exception in case of error.
   procedure Analyze_Init;

   --  Analyze one file.
   function Analyze_File (File : Thin_String_Ptr; Len : Natural) return Iir;
end Libghdl;
