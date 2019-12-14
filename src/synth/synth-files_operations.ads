--  Create declarations for synthesis.
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

with Vhdl.Nodes; use Vhdl.Nodes;

with Synth.Source; use Synth.Source;
with Synth.Values; use Synth.Values;
with Synth.Context; use Synth.Context;

package Synth.Files_Operations is
   --  Raised in case of un-recoverable error.
   File_Execution_Error : exception;

   function Elaborate_File_Declaration
     (Syn_Inst : Synth_Instance_Acc; Decl : Node) return File_Index;

   function Endfile (F : File_Index; Loc : Syn_Src) return Boolean;

   procedure Synth_Untruncated_Text_Read (Syn_Inst : Synth_Instance_Acc;
                                          Imp : Node;
                                          Loc : Node);
end Synth.Files_Operations;
